{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Infrastructure.Zeit (
  ZeitError (..),
  ZeitSession (..),
  absoluteZeitUrl,
  defaultZeitUserAgent,
  extractArticleContent,
  extractAdditionalArticlePageUrls,
  extractArticleList,
  fetchArticleContentZeit,
  fetchArticleListZeit,
  looksLikeArticleUrl,
  normalizeZeitUserAgent,
) where

import Control.Applicative (asum, (<|>))
import Control.Concurrent (threadDelay)
import Data.Aeson (Value (..), eitherDecodeStrict')
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Either (rights)
import Data.Foldable (toList)
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
  , zeitUserAgent :: Text
  }
  deriving (Eq, Show)

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
    Right html ->
      case extractArticleContent url html of
        Right article -> pure (Right article)
        Left _ -> fetchArticleContentWithPages session url
    Left _ ->
      fetchArticleContentWithPages session url

fetchArticleContentWithPages :: ZeitSession -> Text -> IO (Either ZeitError Article)
fetchArticleContentWithPages session url = do
  fallback <- fetchHtml session url
  case fallback of
    Left err -> pure (Left err)
    Right html ->
      case extractArticleContent url html of
        Left err -> pure (Left err)
        Right article -> do
          let pageUrls = extractAdditionalArticlePageUrls url html
          pageResults <- traverse fetchAdditionalPage pageUrls
          pure (Right (mergeArticlePages article (rights pageResults)))
 where
  fetchAdditionalPage pageUrl = do
    threadDelay 500000
    pageHtml <- fetchHtml session pageUrl
    pure (pageHtml >>= extractArticleContent pageUrl)

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
  | looksPaywalled html && length paragraphs < 2 =
      Left (ZeitParseError "Article appears to be behind a paywall or the Zeit session expired.")
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
          , articleAudioUrl = extractAudioUrl tags
          , articleAudioPath = Nothing
          , articleKnownPct = Nothing
          }
 where
  tags = parseTagsText html
  title = cleanTitle (fromMaybe "" (firstBlockText "h1" tags <|> firstBlockText "title" tags))
  paragraphs = extractBodyBlocks tags

extractAdditionalArticlePageUrls :: Text -> Text -> [Text]
extractAdditionalArticlePageUrls articleUrl html =
  dedupeTexts
    [ pageUrl
    | Anchor href title <- collectAnchors (parseTagsText html)
    , let pageUrl = absoluteZeitUrl href
    , pageUrl /= cleanArticleUrl
    , not ("/komplettansicht" `T.isSuffixOf` pageUrl)
    , sameArticlePage cleanArticleUrl pageUrl
    , looksLikePagination title pageUrl
    ]
 where
  cleanArticleUrl = normalizeArticleBase articleUrl

mergeArticlePages :: Article -> [Article] -> Article
mergeArticlePages article pages =
  article
    { articleParagraphs = dedupeTexts (concatMap articleParagraphs (article : pages))
    , articleAudioUrl = articleAudioUrl article <|> asum (map articleAudioUrl pages)
    }

looksLikePagination :: Text -> Text -> Bool
looksLikePagination title url =
  any (`T.isInfixOf` lowerTitle) ["seite", "weiter", "nächste", "naechste", "auf einer seite"]
    || "/seite-" `T.isInfixOf` lowerUrl
    || maybe False (T.all isDigitText) (lastMaybe (T.splitOn "/" lowerUrl))
 where
  lowerTitle = T.toLower title
  lowerUrl = T.toLower url

sameArticlePage :: Text -> Text -> Bool
sameArticlePage cleanArticleUrl candidate =
  candidateBase == cleanArticleUrl
    || (cleanArticleUrl <> "/") `T.isPrefixOf` candidateBase
 where
  candidateBase = normalizeArticleBase candidate

normalizeArticleBase :: Text -> Text
normalizeArticleBase =
  T.dropWhileEnd (== '/') . stripQueryNoise

dedupeTexts :: [Text] -> [Text]
dedupeTexts = go Set.empty
 where
  go _ [] = []
  go seen (value : rest)
    | key `Set.member` seen = go seen rest
    | otherwise = value : go (Set.insert key seen) rest
   where
    key = T.take 100 value

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe values = Just (last values)

isDigitText :: Char -> Bool
isDigitText ch = ch >= '0' && ch <= '9'

extractAudioUrl :: [Tag String] -> Maybe Text
extractAudioUrl tags =
  absoluteMediaUrl
    <$> ( firstAttr "audio" "src" tags
            <|> firstAttr "source" "src" tags
            <|> metaProperty "og:audio" tags
            <|> metaProperty "og:audio:secure_url" tags
            <|> jsonLdAudioUrl tags
        )

absoluteMediaUrl :: Text -> Text
absoluteMediaUrl value
  | "https://" `T.isPrefixOf` value || "http://" `T.isPrefixOf` value = value
  | "/" `T.isPrefixOf` value = baseUrl <> value
  | otherwise = value

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

jsonLdAudioUrl :: [Tag String] -> Maybe Text
jsonLdAudioUrl =
  asum . map audioFromJsonText . jsonLdScripts

jsonLdScripts :: [Tag String] -> [Text]
jsonLdScripts [] = []
jsonLdScripts (TagOpen "script" attrs : rest)
  | lookup "type" attrs == Just "application/ld+json" =
      let (inside, after) = break (tagCloseName "script") rest
       in T.pack (innerText inside) : jsonLdScripts (drop 1 after)
jsonLdScripts (_ : rest) = jsonLdScripts rest

audioFromJsonText :: Text -> Maybe Text
audioFromJsonText raw =
  case eitherDecodeStrict' (encodeUtf8 raw) of
    Right value -> audioFromValue value
    Left _ -> Nothing

audioFromValue :: Value -> Maybe Text
audioFromValue (Object obj) =
  asum
    [ textField "contentUrl"
    , textField "url"
    , KeyMap.lookup "audio" obj >>= audioFromValue
    , asum (map audioFromValue (KeyMap.elems obj))
    ]
 where
  textField key = do
    String value <- KeyMap.lookup key obj
    if looksLikeAudioUrl value then Just value else Nothing
audioFromValue (Array values) =
  asum (map audioFromValue (toList values))
audioFromValue (String value)
  | looksLikeAudioUrl value = Just value
audioFromValue _ = Nothing

looksLikeAudioUrl :: Text -> Bool
looksLikeAudioUrl value =
  any (`T.isInfixOf` lower) [".mp3", ".m4a", ".ogg", ".aac", "audio"]
 where
  lower = T.toLower value

attrFromFirst :: String -> [Tag String] -> Maybe Text
attrFromFirst attrName tags = do
  TagOpen _ attrs <- find tagOpen tags
  nonEmpty . T.pack =<< lookup attrName attrs

tagHasAttr :: String -> String -> Tag String -> Bool
tagHasAttr attrName expected (TagOpen _ attrs) =
  lookup attrName attrs == Just expected
tagHasAttr _ _ _ = False

tagOpen :: Tag String -> Bool
tagOpen TagOpen{} = True
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
  go seen (article : rest)
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

looksPaywalled :: Text -> Bool
looksPaywalled html =
  any (`T.isInfixOf` lower) markers
 where
  lower = T.toLower html
  markers =
    [ "z+"
    , "diesen artikel"
    , "abonnenten"
    , "einloggen"
    , "session"
    ]

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
  pure
    $ maybe id (\cookie -> setRequestHeader "Cookie" [cookie]) (cookieHeader session)
      . setRequestHeader "User-Agent" [encodeUtf8 (normalizeZeitUserAgent (zeitUserAgent session))]
      . setRequestHeader
        "Accept"
        ["text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8"]
      . setRequestHeader "Accept-Language" ["de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7"]
      . setRequestHeader "Upgrade-Insecure-Requests" ["1"]
      . setRequestHeader "Sec-Fetch-Dest" ["document"]
      . setRequestHeader "Sec-Fetch-Mode" ["navigate"]
      . setRequestHeader "Sec-Fetch-Site" ["none"]
    $ request

cookieHeader :: ZeitSession -> Maybe ByteString
cookieHeader session
  | T.null (zeitCookies session) = Nothing
  | otherwise = Just (encodeUtf8 (zeitCookies session))

defaultZeitUserAgent :: Text
defaultZeitUserAgent =
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"

normalizeZeitUserAgent :: Text -> Text
normalizeZeitUserAgent value
  | T.null stripped = defaultZeitUserAgent
  | otherwise = stripped
 where
  stripped = T.strip value
