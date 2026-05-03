{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Infrastructure.Zeit
  ( ZeitError(..)
  , ZeitSession(..)
  , absoluteZeitUrl
  , extractArticleContent
  , extractArticleList
  , fetchArticleContentZeit
  , fetchArticleListZeit
  , looksLikeArticleUrl
  ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Network.HTTP.Simple
import Text.HTML.TagSoup
import ZeitLingq.Domain.Section (allSections, lookupSection)
import ZeitLingq.Domain.Types

data ZeitSession = ZeitSession
  { zeitCookies :: Text
  } deriving (Eq, Show)

data ZeitError
  = ZeitHttpError Int Text
  | ZeitParseError Text
  deriving (Eq, Show)

baseUrl :: Text
baseUrl = "https://www.zeit.de"

fetchArticleListZeit :: ZeitSession -> Text -> Int -> IO (Either ZeitError [ArticleSummary])
fetchArticleListZeit session sectionIdent page = do
  let section = fromMaybe (head allSections) (lookupSection sectionIdent)
      pageSuffix = if page > 1 then "?p=" <> T.pack (show page) else ""
      url = baseUrl <> sectionPath section <> pageSuffix
  result <- fetchHtml session url
  pure (extractArticleList (sectionLabel section) <$> result)

fetchArticleContentZeit :: ZeitSession -> Text -> IO (Either ZeitError Article)
fetchArticleContentZeit session url = do
  result <- fetchHtml session (completeViewUrl url)
  case result of
    Right html -> pure (extractArticleContent url html)
    Left _ -> do
      fallback <- fetchHtml session url
      pure (fallback >>= extractArticleContent url)

fetchHtml :: ZeitSession -> Text -> IO (Either ZeitError Text)
fetchHtml session url = do
  request <- addZeitHeaders session =<< parseRequest (T.unpack url)
  response <- httpLBS request
  let status = getResponseStatusCode response
  pure $
    if status >= 200 && status < 300
      then Right (decodeUtf8Lazy (getResponseBody response))
      else Left (ZeitHttpError status "Zeit request failed.")

extractArticleList :: Text -> Text -> [ArticleSummary]
extractArticleList sectionLabelValue html =
  dedupeByUrl
    [ ArticleSummary
        { summaryId = Nothing
        , summaryUrl = url
        , summaryTitle = title
        , summarySection = sectionLabelValue
        , summaryWordCount = 0
        , summaryIgnored = False
        , summaryUploaded = False
        , summaryKnownPct = Nothing
        }
    | Anchor href title <- collectAnchors (parseTagsText html)
    , let url = absoluteZeitUrl href
    , looksLikeArticleUrl url
    , T.length title > 10
    ]

extractArticleContent :: Text -> Text -> Either ZeitError Article
extractArticleContent url html
  | T.null title = Left (ZeitParseError "Could not extract article title.")
  | null paragraphs = Left (ZeitParseError "Could not extract article paragraphs.")
  | otherwise =
      Right
        Article
          { articleId = Nothing
          , articleUrl = url
          , articleTitle = title
          , articleSubtitle = fromMaybe "" (metaContent "description" tags)
          , articleAuthor = fromMaybe "" (metaContent "author" tags)
          , articleDate = metaProperty "article:published_time" tags <|> firstAttr "time" "datetime" tags
          , articleSection = fromMaybe (sectionFromUrl url) (metaProperty "article:section" tags)
          , articleParagraphs = paragraphs
          , articleFetchedAt = Nothing
          , articleUploadedLesson = Nothing
          , articleIgnored = False
          , articleAudioUrl = firstAttr "audio" "src" tags <|> metaProperty "og:audio" tags
          , articleAudioPath = Nothing
          , articleKnownPct = Nothing
          }
  where
    tags = parseTagsText html
    title = cleanTitle (fromMaybe "" (firstBlockText "h1" tags <|> firstBlockText "title" tags))
    paragraphs = extractBodyBlocks tags

looksLikeArticleUrl :: Text -> Bool
looksLikeArticleUrl url =
  "zeit.de" `T.isInfixOf` url
    && "/20" `T.isInfixOf` url
    && not ("#comments" `T.isSuffixOf` url)

absoluteZeitUrl :: Text -> Text
absoluteZeitUrl href
  | "https://" `T.isPrefixOf` href || "http://" `T.isPrefixOf` href = stripQueryNoise href
  | "/" `T.isPrefixOf` href = stripQueryNoise (baseUrl <> href)
  | otherwise = stripQueryNoise (baseUrl <> "/" <> href)

completeViewUrl :: Text -> Text
completeViewUrl url
  | "/komplettansicht" `T.isSuffixOf` clean = clean
  | "/" `T.isSuffixOf` clean = clean <> "komplettansicht"
  | otherwise = clean <> "/komplettansicht"
  where
    clean = stripQueryNoise url

data Anchor = Anchor Text Text
  deriving (Eq, Show)

collectAnchors :: [Tag String] -> [Anchor]
collectAnchors [] = []
collectAnchors (TagOpen "a" attrs : rest) =
  let (inside, after) = break (tagCloseName "a") rest
      title = cleanText (T.pack (innerText inside))
      href = T.pack <$> lookup "href" attrs
      anchor = Anchor <$> href <*> nonEmpty title
   in maybe id (:) anchor (collectAnchors (drop 1 after))
collectAnchors (_ : rest) = collectAnchors rest

extractBodyBlocks :: [Tag String] -> [Text]
extractBodyBlocks [] = []
extractBodyBlocks (TagOpen name _ : rest)
  | name `elem` ["h2", "h3", "p"] =
      let (inside, after) = break (tagCloseName name) rest
          text = cleanText (T.pack (innerText inside))
          block = blockText name text
       in maybe id (:) block (extractBodyBlocks (drop 1 after))
extractBodyBlocks (_ : rest) = extractBodyBlocks rest

blockText :: String -> Text -> Maybe Text
blockText name text
  | T.null text = Nothing
  | name == "h2" || name == "h3" = Just ("## " <> text)
  | T.length text > 20 = Just text
  | otherwise = Nothing

firstBlockText :: String -> [Tag String] -> Maybe Text
firstBlockText name tags =
  case dropWhile (not . tagOpenName name) tags of
    TagOpen _ _ : rest ->
      let (inside, _) = break (tagCloseName name) rest
       in nonEmpty (cleanText (T.pack (innerText inside)))
    _ -> Nothing

metaContent :: String -> [Tag String] -> Maybe Text
metaContent name =
  attrFromFirst "content" . filter (tagHasAttr "name" name)

metaProperty :: String -> [Tag String] -> Maybe Text
metaProperty property =
  attrFromFirst "content" . filter (tagHasAttr "property" property)

firstAttr :: String -> String -> [Tag String] -> Maybe Text
firstAttr tagName attrName =
  attrFromFirst attrName . filter (tagOpenName tagName)

attrFromFirst :: String -> [Tag String] -> Maybe Text
attrFromFirst attrName tags = do
  TagOpen _ attrs <- find tagOpen tags
  nonEmpty . T.pack =<< lookup attrName attrs

tagHasAttr :: String -> String -> Tag String -> Bool
tagHasAttr attrName expected (TagOpen _ attrs) =
  lookup attrName attrs == Just expected
tagHasAttr _ _ _ = False

tagOpen :: Tag String -> Bool
tagOpen TagOpen {} = True
tagOpen _ = False

tagOpenName :: String -> Tag String -> Bool
tagOpenName expected (TagOpen name _) = name == expected
tagOpenName _ _ = False

tagCloseName :: String -> Tag String -> Bool
tagCloseName expected (TagClose name) = name == expected
tagCloseName _ _ = False

dedupeByUrl :: [ArticleSummary] -> [ArticleSummary]
dedupeByUrl = go Set.empty
  where
    go _ [] = []
    go seen (article:rest)
      | summaryUrl article `Set.member` seen = go seen rest
      | otherwise = article : go (Set.insert (summaryUrl article) seen) rest

sectionFromUrl :: Text -> Text
sectionFromUrl url =
  fromMaybe "" $ do
    path <- T.stripPrefix "https://www.zeit.de/" (stripQueryNoise url)
    nonEmpty (head (T.splitOn "/" path))

cleanTitle :: Text -> Text
cleanTitle =
  cleanText
    . head
    . T.splitOn " | "

cleanText :: Text -> Text
cleanText = T.unwords . T.words

nonEmpty :: Text -> Maybe Text
nonEmpty value
  | T.null value = Nothing
  | otherwise = Just value

stripQueryNoise :: Text -> Text
stripQueryNoise = head . T.splitOn "?" . head . T.splitOn "#"

parseTagsText :: Text -> [Tag String]
parseTagsText = parseTags . T.unpack

decodeUtf8Lazy :: BL.ByteString -> Text
decodeUtf8Lazy = LT.toStrict . LTE.decodeUtf8

addZeitHeaders :: ZeitSession -> Request -> IO Request
addZeitHeaders session request =
  pure $
    maybe id (\cookie -> setRequestHeader "Cookie" [cookie]) (cookieHeader session)
      . setRequestHeader "User-Agent" [zeitUserAgent]
      $ request

cookieHeader :: ZeitSession -> Maybe ByteString
cookieHeader session
  | T.null (zeitCookies session) = Nothing
  | otherwise = Just (encodeUtf8 (zeitCookies session))

zeitUserAgent :: ByteString
zeitUserAgent = "Mozilla/5.0 ZeitToolHaskell/0.1"
