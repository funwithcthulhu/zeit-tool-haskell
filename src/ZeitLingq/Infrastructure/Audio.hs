{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Infrastructure.Audio
  ( AudioDownloadError(..)
  , audioFilename
  , downloadArticleAudio
  ) where

import Data.ByteString.Lazy qualified as BL
import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Simple
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import ZeitLingq.Domain.Types

data AudioDownloadError
  = ArticleHasNoAudioUrl
  | AudioHttpError Int Text
  deriving (Eq, Show)

downloadArticleAudio :: FilePath -> Article -> IO (Either AudioDownloadError FilePath)
downloadArticleAudio audioDir article =
  case articleAudioPath article of
    Just path -> do
      exists <- doesFileExist path
      if exists
        then pure (Right path)
        else downloadFresh
    Nothing -> downloadFresh
  where
    downloadFresh =
      case articleAudioUrl article of
        Nothing -> pure (Left ArticleHasNoAudioUrl)
        Just url -> do
          createDirectoryIfMissing True audioDir
          request <- parseRequest (T.unpack url)
          response <- httpLBS (setRequestHeader "User-Agent" ["ZeitToolHaskell/0.1"] request)
          let status = getResponseStatusCode response
          if status >= 200 && status < 300
            then do
              let path = audioDir </> audioFilename article
              BL.writeFile path (getResponseBody response)
              pure (Right path)
            else pure (Left (AudioHttpError status "Audio download failed."))

audioFilename :: Article -> FilePath
audioFilename article =
  idPart <> "-" <> slugPart <> "." <> T.unpack (audioExtension (articleAudioUrl article))
  where
    idPart = maybe "article" (show . unArticleId) (articleId article)
    slugPart =
      case safeSlug (articleTitle article) of
        "" -> "untitled"
        value -> value

safeSlug :: Text -> FilePath
safeSlug =
  trimHyphens
    . collapseHyphens
    . map safeChar
    . T.unpack
    . T.toLower
    . T.take 80

safeChar :: Char -> Char
safeChar ch
  | isAsciiLower ch || isDigit ch = ch
  | otherwise = '-'

collapseHyphens :: String -> String
collapseHyphens [] = []
collapseHyphens ('-' : '-' : rest) = collapseHyphens ('-' : rest)
collapseHyphens (ch : rest) = ch : collapseHyphens rest

trimHyphens :: String -> String
trimHyphens = reverse . dropWhile (== '-') . reverse . dropWhile (== '-')

audioExtension :: Maybe Text -> Text
audioExtension maybeUrl =
  case maybeUrl >>= findExtension . stripQuery of
    Just ext -> ext
    Nothing -> "mp3"
  where
    stripQuery = head . T.splitOn "?" . head . T.splitOn "#"

findExtension :: Text -> Maybe Text
findExtension url
  | ".mp3" `T.isSuffixOf` lower = Just "mp3"
  | ".m4a" `T.isSuffixOf` lower = Just "m4a"
  | ".ogg" `T.isSuffixOf` lower = Just "ogg"
  | ".aac" `T.isSuffixOf` lower = Just "aac"
  | otherwise = Nothing
  where
    lower = T.toLower url
