{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Gui.BrowserSession (
  BrowserZeitSession (..),
  parseBrowserZeitSession,
) where

import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.:))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import ZeitLingq.Infrastructure.Zeit (defaultZeitUserAgent, normalizeZeitUserAgent)

data BrowserZeitSession = BrowserZeitSession
  { browserCookieHeader :: Text
  , browserUserAgent :: Text
  }
  deriving (Eq, Show)

instance FromJSON BrowserZeitSession where
  parseJSON =
    withObject "BrowserZeitSession" $ \obj ->
      BrowserZeitSession
        <$> obj .: "cookieHeader"
        <*> obj .: "userAgent"

parseBrowserZeitSession :: Text -> BrowserZeitSession
parseBrowserZeitSession raw =
  case eitherDecodeStrict' (encodeUtf8 raw) of
    Right session -> normalizeBrowserZeitSession session
    Left _ ->
      BrowserZeitSession
        { browserCookieHeader = T.strip raw
        , browserUserAgent = defaultZeitUserAgent
        }

normalizeBrowserZeitSession :: BrowserZeitSession -> BrowserZeitSession
normalizeBrowserZeitSession session =
  session
    { browserCookieHeader = T.strip (browserCookieHeader session)
    , browserUserAgent = normalizeZeitUserAgent (browserUserAgent session)
    }
