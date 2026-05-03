{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Infrastructure.Lingq
  ( LingqError(..)
  , LingqToken(..)
  , fetchCollectionsLingq
  , fetchKnownWordsLingq
  , loginWithApiKeyLingq
  , loginWithPasswordLingq
  , normalizeLessonText
  , parseCollectionsValue
  , parseKnownWordTerms
  , uploadLessonLingq
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
import ZeitLingq.Domain.Article (articleBodyText)
import ZeitLingq.Domain.Types

newtype LingqToken = LingqToken {unLingqToken :: Text}
  deriving (Eq, Show)

data LingqError
  = LingqHttpError Int Text
  | LingqJsonError Text
  | LingqAuthError Text
  deriving (Eq, Show)

lingqBase :: String
lingqBase = "https://www.lingq.com/api/v3"

lingqAuth :: String
lingqAuth = "https://www.lingq.com/api/v2/api-token-auth/"

loginWithPasswordLingq :: Text -> Text -> IO (Either LingqError LingqToken)
loginWithPasswordLingq username password = do
  request <- parseRequest lingqAuth
  let authedRequest =
        setRequestMethod "POST"
          . setRequestBodyURLEncoded
            [ ("username", encodeUtf8 username)
            , ("password", encodeUtf8 password)
            ]
          $ request
  fmap authTokenFromResponse (httpJson authedRequest)

loginWithApiKeyLingq :: Text -> IO (Either LingqError LingqToken)
loginWithApiKeyLingq apiKey = do
  request <- authorized (LingqToken apiKey) =<< parseRequest (lingqBase <> "/languages/")
  response <- httpNoBody request
  let status = getResponseStatusCode response
  pure $
    if status >= 200 && status < 300
      then Right (LingqToken apiKey)
      else Left (LingqAuthError ("Invalid API key, HTTP " <> T.pack (show status)))

fetchCollectionsLingq :: LingqToken -> Text -> IO (Either LingqError [LingqCollection])
fetchCollectionsLingq token languageCode = do
  request <- authorized token =<< parseRequest (lingqBase <> "/" <> T.unpack languageCode <> "/collections/my/")
  fmap (>>= parseCollectionsValue) (httpJsonValue request)

uploadLessonLingq :: LingqToken -> Text -> Maybe Text -> Article -> IO (Either LingqError LingqLesson)
uploadLessonLingq token languageCode maybeCollection article = do
  request <- authorized token =<< parseRequest (lingqBase <> "/" <> T.unpack languageCode <> "/lessons/")
  let uploadRequest =
        setRequestMethod "POST"
          . setRequestBodyJSON (lessonPayload maybeCollection article)
          $ request
  fmap (>>= lessonFromResponse languageCode) (httpJsonValue uploadRequest)

fetchKnownWordsLingq :: LingqToken -> Text -> IO (Either LingqError [Text])
fetchKnownWordsLingq token languageCode =
  tryStrategies knownWordUrls
  where
    lang = T.unpack languageCode
    knownWordUrls =
      [ lingqBase <> "/" <> lang <> "/known-words/?page_size=1000"
      , lingqBase <> "/" <> lang <> "/cards/?status=3&page_size=1000"
      , lingqBase <> "/" <> lang <> "/cards/?status=4&page_size=1000"
      , "https://www.lingq.com/api/v2/" <> lang <> "/words/?page_size=1000"
      , "https://www.lingq.com/api/" <> lang <> "/known-words/?page_size=1000"
      ]

    tryStrategies [] = pure (Left (LingqJsonError "Could not fetch known words from any LingQ endpoint."))
    tryStrategies (url:rest) = do
      result <- fetchKnownPages token url
      case result of
        Right terms | not (null terms) -> pure (Right terms)
        _ -> tryStrategies rest

fetchKnownPages :: LingqToken -> String -> IO (Either LingqError [Text])
fetchKnownPages token startUrl = go [] (Just startUrl) (0 :: Int)
  where
    go terms Nothing _ = pure (Right (reverse terms))
    go terms (Just url) page
      | page >= 100 = pure (Right (reverse terms))
      | otherwise = do
          request <- authorized token =<< parseRequest url
          result <- httpJsonValue request
          case result of
            Left err -> pure (Left err)
            Right value ->
              let pageTerms = parseKnownWordTerms value
                  nextUrl = fmap T.unpack (parseNextUrl value)
               in go (reverse pageTerms <> terms) nextUrl (page + 1)

normalizeLessonText :: Text -> Text
normalizeLessonText =
  T.intercalate "\n\n"
    . filter (not . T.null)
    . map (T.unwords . T.words)
    . T.splitOn "\n\n"

lessonPayload :: Maybe Text -> Article -> Value
lessonPayload maybeCollection article =
  object $
    [ "title" .= articleTitle article
    , "text" .= normalizeLessonText (articleBodyText article)
    , "status" .= ("private" :: Text)
    , "original_url" .= articleUrl article
    ]
      <> maybe [] (\collection -> ["collection" .= collection]) maybeCollection

parseCollectionsValue :: Value -> Either LingqError [LingqCollection]
parseCollectionsValue =
  either (Left . LingqJsonError . T.pack) Right
    . parseEither collectionsParser

collectionsParser :: Value -> Parser [LingqCollection]
collectionsParser value =
  parseArray value <|> parseObjectResults value
  where
    parseArray = withArray "collections" (traverse parseCollection . toList)
    parseObjectResults = withObject "collections response" $ \obj -> do
      results <- obj .: "results"
      parseArray results

parseCollection :: Value -> Parser LingqCollection
parseCollection = withObject "LingqCollection" $ \obj -> do
  lessonsCount <- obj .:? "lessonsCount"
  lessonsCountSnake <- obj .:? "lessons_count"
  LingqCollection
    <$> parseCollectionId obj
    <*> obj .: "title"
    <*> pure (fromMaybe 0 (lessonsCount <|> lessonsCountSnake))

parseCollectionId :: Object -> Parser Text
parseCollectionId obj =
  parseTextOrInt obj "id" <|> parseTextOrInt obj "pk"

parseKnownWordTerms :: Value -> [Text]
parseKnownWordTerms value =
  case parseMaybe knownWordsParser value of
    Just terms -> terms
    Nothing -> []

knownWordsParser :: Value -> Parser [Text]
knownWordsParser value =
  parseArray value <|> parseObjectResults value
  where
    parseArray = withArray "known words" (fmap concat . traverse parseTerm . toList)
    parseObjectResults = withObject "known words response" $ \obj -> do
      results <- obj .: "results"
      parseArray results

parseTerm :: Value -> Parser [Text]
parseTerm =
  withObject "known word" $ \obj -> do
    termValue <- obj .:? "term"
    wordValue <- obj .:? "word"
    textValue <- obj .:? "text"
    let term = termValue <|> wordValue <|> textValue
    pure (maybe [] (\value -> [T.toLower (T.strip value) | not (T.null (T.strip value))]) term)

parseNextUrl :: Value -> Maybe Text
parseNextUrl value =
  case parseMaybe parser value of
    Just (Just nextUrl) -> Just nextUrl
    _ -> Nothing
  where
    parser = withObject "pagination" $ \obj -> obj .:? "next"

lessonFromResponse :: Text -> Value -> Either LingqError LingqLesson
lessonFromResponse languageCode =
  either (Left . LingqJsonError . T.pack) Right
    . parseEither parser
  where
    parser = withObject "lesson" $ \obj -> do
      ident <- parseTextOrInt obj "id" <|> parseTextOrInt obj "pk"
      pure
        LingqLesson
          { lessonId = ident
          , lessonUrl = "https://www.lingq.com/" <> languageCode <> "/learn/lesson/" <> ident <> "/"
          }

authTokenFromResponse :: Either LingqError AuthTokenResponse -> Either LingqError LingqToken
authTokenFromResponse = fmap (\(AuthTokenResponse token) -> LingqToken token)

newtype AuthTokenResponse = AuthTokenResponse Text

instance FromJSON AuthTokenResponse where
  parseJSON = withObject "auth token" $ \obj -> AuthTokenResponse <$> obj .: "token"

httpJson :: FromJSON a => Request -> IO (Either LingqError a)
httpJson request = do
  response <- httpJSONEither request
  pure $
    let status = getResponseStatusCode response
     in if status >= 200 && status < 300
          then either (Left . LingqJsonError . T.pack . show) Right (getResponseBody response)
          else Left (LingqHttpError status "LingQ request failed.")

httpJsonValue :: Request -> IO (Either LingqError Value)
httpJsonValue = httpJson

authorized :: LingqToken -> Request -> IO Request
authorized (LingqToken token) request =
  pure $
    setRequestHeader "Authorization" [authHeader token]
      . setRequestHeader "User-Agent" ["ZeitToolHaskell/0.1"]
      $ request

authHeader :: Text -> ByteString
authHeader token = "Token " <> encodeUtf8 token

parseTextOrInt :: Object -> Key -> Parser Text
parseTextOrInt obj key =
  (obj .: key :: Parser Text)
    <|> (T.pack . show <$> (obj .: key :: Parser Int))
