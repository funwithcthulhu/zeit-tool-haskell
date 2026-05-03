{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Infrastructure.Sqlite
  ( LibraryDb
  , closeLibrary
  , deleteArticleSqlite
  , getArticleSqlite
  , getArticlesSqlite
  , getStatsSqlite
  , markUploadedSqlite
  , openLibrary
  , saveArticleSqlite
  , setIgnoredSqlite
  , sqliteLibraryPort
  , withLibrary
  ) where

import Control.Exception (bracket)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (toField)
import ZeitLingq.Domain.Article
  ( articleBodyText
  , composeCleanText
  , wordCount
  )
import ZeitLingq.Domain.Types
import ZeitLingq.Ports (LibraryPort(..))

newtype LibraryDb = LibraryDb Connection

openLibrary :: FilePath -> IO LibraryDb
openLibrary path = do
  conn <- open path
  execute_ conn "PRAGMA foreign_keys = ON"
  migrate conn
  pure (LibraryDb conn)

closeLibrary :: LibraryDb -> IO ()
closeLibrary (LibraryDb conn) = close conn

withLibrary :: FilePath -> (LibraryDb -> IO a) -> IO a
withLibrary path = bracket (openLibrary path) closeLibrary

sqliteLibraryPort :: LibraryDb -> LibraryPort IO
sqliteLibraryPort db =
  LibraryPort
    { loadLibrary = getArticlesSqlite db
    , loadArticle = getArticleSqlite db
    , saveArticle = saveArticleSqlite db
    , deleteArticle = deleteArticleSqlite db
    , loadStats = getStatsSqlite db
    }

migrate :: Connection -> IO ()
migrate conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS articles\
    \ (id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \  url TEXT UNIQUE NOT NULL,\
    \  title TEXT NOT NULL,\
    \  subtitle TEXT NOT NULL DEFAULT '',\
    \  author TEXT NOT NULL DEFAULT '',\
    \  date TEXT,\
    \  section TEXT NOT NULL DEFAULT '',\
    \  body_text TEXT NOT NULL,\
    \  clean_text TEXT NOT NULL,\
    \  word_count INTEGER NOT NULL DEFAULT 0,\
    \  fetched_at TIMESTAMP NOT NULL,\
    \  uploaded_to_lingq INTEGER NOT NULL DEFAULT 0,\
    \  lingq_lesson_id TEXT NOT NULL DEFAULT '',\
    \  lingq_lesson_url TEXT NOT NULL DEFAULT '',\
    \  ignored INTEGER NOT NULL DEFAULT 0,\
    \  audio_url TEXT,\
    \  audio_path TEXT,\
    \  known_pct INTEGER)"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_articles_section ON articles(section)"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_articles_word_count ON articles(word_count)"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_articles_date ON articles(date)"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_articles_ignored ON articles(ignored)"

saveArticleSqlite :: LibraryDb -> Article -> IO ArticleId
saveArticleSqlite (LibraryDb conn) article = do
  now <- getCurrentTime
  existing <- query conn "SELECT id, uploaded_to_lingq, lingq_lesson_id, lingq_lesson_url, ignored, audio_path, known_pct FROM articles WHERE url = ?" (Only (articleUrl article))
  let preserved = case existing of
        row : _ -> Just (row :: PreservedFields)
        [] -> Nothing
      storedId = maybe (articleId article) (Just . preservedId) preserved
      fetchedAt = fromMaybe now (articleFetchedAt article)
      uploadedLesson = articleUploadedLesson article <|> (preservedLesson =<< preserved)
      ignored = maybe (articleIgnored article) preservedIgnored preserved
      audioPath = articleAudioPath article <|> (preservedAudioPath =<< preserved)
      knownPct = articleKnownPct article <|> (preservedKnownPct =<< preserved)
  execute
    conn
    "INSERT INTO articles\
    \ (id, url, title, subtitle, author, date, section, body_text, clean_text, word_count, fetched_at,\
    \  uploaded_to_lingq, lingq_lesson_id, lingq_lesson_url, ignored, audio_url, audio_path, known_pct)\
    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\
    \ ON CONFLICT(url) DO UPDATE SET\
    \  title = excluded.title,\
    \  subtitle = excluded.subtitle,\
    \  author = excluded.author,\
    \  date = excluded.date,\
    \  section = excluded.section,\
    \  body_text = excluded.body_text,\
    \  clean_text = excluded.clean_text,\
    \  word_count = excluded.word_count,\
    \  fetched_at = excluded.fetched_at,\
    \  audio_url = excluded.audio_url"
    ArticleWrite
      { writeId = fmap unArticleId storedId
      , writeUrl = articleUrl article
      , writeTitle = articleTitle article
      , writeSubtitle = articleSubtitle article
      , writeAuthor = articleAuthor article
      , writeDate = articleDate article
      , writeSection = articleSection article
      , writeBodyText = articleBodyText article
      , writeCleanText = composeCleanText article
      , writeWordCount = wordCount article
      , writeFetchedAt = fetchedAt
      , writeUploaded = maybe (0 :: Int) (const 1) uploadedLesson
      , writeLessonId = maybe "" lessonId uploadedLesson
      , writeLessonUrl = maybe "" lessonUrl uploadedLesson
      , writeIgnored = boolToInt ignored
      , writeAudioUrl = articleAudioUrl article
      , writeAudioPath = audioPath
      , writeKnownPct = knownPct
      }
  [Only savedId] <- query conn "SELECT id FROM articles WHERE url = ?" (Only (articleUrl article))
  pure (ArticleId savedId)

getArticleSqlite :: LibraryDb -> ArticleId -> IO (Maybe Article)
getArticleSqlite (LibraryDb conn) (ArticleId ident) = do
  rows <- query conn "SELECT id, url, title, subtitle, author, date, section, body_text, fetched_at, uploaded_to_lingq, lingq_lesson_id, lingq_lesson_url, ignored, audio_url, audio_path, known_pct FROM articles WHERE id = ?" (Only ident)
  pure $ case rows of
    row : _ -> Just (articleFromRow row)
    [] -> Nothing

getArticlesSqlite :: LibraryDb -> WordFilter -> IO [ArticleSummary]
getArticlesSqlite (LibraryDb conn) filters = do
  rows <- query conn queryText params
  pure (map summaryFromRow rows)
  where
    queryText =
      "SELECT id, url, title, section, word_count, ignored, uploaded_to_lingq, known_pct\
      \ FROM articles\
      \ WHERE word_count >= ? AND word_count <= ? AND ignored = 0\
      \ ORDER BY fetched_at DESC"
    params = (fromMaybe 0 (minWords filters), fromMaybe maxBound (maxWords filters) :: Int)

deleteArticleSqlite :: LibraryDb -> ArticleId -> IO ()
deleteArticleSqlite (LibraryDb conn) (ArticleId ident) =
  execute conn "DELETE FROM articles WHERE id = ?" (Only ident)

setIgnoredSqlite :: LibraryDb -> ArticleId -> Bool -> IO ()
setIgnoredSqlite (LibraryDb conn) (ArticleId ident) ignored =
  execute conn "UPDATE articles SET ignored = ? WHERE id = ?" (boolToInt ignored, ident)

markUploadedSqlite :: LibraryDb -> ArticleId -> LingqLesson -> IO ()
markUploadedSqlite (LibraryDb conn) (ArticleId ident) LingqLesson {lessonId, lessonUrl} =
  execute
    conn
    "UPDATE articles SET uploaded_to_lingq = 1, lingq_lesson_id = ?, lingq_lesson_url = ? WHERE id = ?"
    (lessonId, lessonUrl, ident)

getStatsSqlite :: LibraryDb -> IO LibraryStats
getStatsSqlite (LibraryDb conn) = do
  [Only totalArticles] <- query_ conn "SELECT COUNT(*) FROM articles"
  [Only uploadedArticles] <- query_ conn "SELECT COUNT(*) FROM articles WHERE uploaded_to_lingq = 1"
  [Only avgWords] <- query_ conn "SELECT CAST(COALESCE(ROUND(AVG(word_count)), 0) AS INTEGER) FROM articles"
  sectionRows <- query_ conn "SELECT section, COUNT(*) FROM articles GROUP BY section"
  pure
    LibraryStats
      { totalArticles
      , uploadedArticles
      , averageWordCount = avgWords
      , sectionCounts = Map.fromList sectionRows
      }

data PreservedFields = PreservedFields
  { preservedId :: ArticleId
  , preservedLesson :: Maybe LingqLesson
  , preservedIgnored :: Bool
  , preservedAudioPath :: Maybe FilePath
  , preservedKnownPct :: Maybe Int
  }

data ArticleWrite = ArticleWrite
  { writeId :: Maybe Int
  , writeUrl :: Text
  , writeTitle :: Text
  , writeSubtitle :: Text
  , writeAuthor :: Text
  , writeDate :: Maybe Text
  , writeSection :: Text
  , writeBodyText :: Text
  , writeCleanText :: Text
  , writeWordCount :: Int
  , writeFetchedAt :: UTCTime
  , writeUploaded :: Int
  , writeLessonId :: Text
  , writeLessonUrl :: Text
  , writeIgnored :: Int
  , writeAudioUrl :: Maybe Text
  , writeAudioPath :: Maybe FilePath
  , writeKnownPct :: Maybe Int
  }

instance ToRow ArticleWrite where
  toRow ArticleWrite {writeId, writeUrl, writeTitle, writeSubtitle, writeAuthor, writeDate, writeSection, writeBodyText, writeCleanText, writeWordCount, writeFetchedAt, writeUploaded, writeLessonId, writeLessonUrl, writeIgnored, writeAudioUrl, writeAudioPath, writeKnownPct} =
    [ toField writeId
    , toField writeUrl
    , toField writeTitle
    , toField writeSubtitle
    , toField writeAuthor
    , toField writeDate
    , toField writeSection
    , toField writeBodyText
    , toField writeCleanText
    , toField writeWordCount
    , toField writeFetchedAt
    , toField writeUploaded
    , toField writeLessonId
    , toField writeLessonUrl
    , toField writeIgnored
    , toField writeAudioUrl
    , toField writeAudioPath
    , toField writeKnownPct
    ]

instance FromRow PreservedFields where
  fromRow = do
    ident <- field
    uploaded <- field
    remoteLessonId <- field
    remoteLessonUrl <- field
    ignored <- field
    audioPath <- field
    knownPct <- field
    pure
      PreservedFields
        { preservedId = ArticleId ident
        , preservedLesson =
            if uploaded == (1 :: Int) && not (T.null remoteLessonId)
              then Just (LingqLesson remoteLessonId remoteLessonUrl)
              else Nothing
        , preservedIgnored = uploadedIntToBool ignored
        , preservedAudioPath = nonEmptyTextPath audioPath
        , preservedKnownPct = knownPct
        }

data ArticleRow = ArticleRow
  Int
  Text
  Text
  Text
  Text
  (Maybe Text)
  Text
  Text
  UTCTime
  Int
  Text
  Text
  Int
  (Maybe Text)
  (Maybe Text)
  (Maybe Int)

instance FromRow ArticleRow where
  fromRow =
    ArticleRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

articleFromRow :: ArticleRow -> Article
articleFromRow (ArticleRow ident url title subtitle author publishedAt section body fetchedAt uploaded remoteLessonId remoteLessonUrl ignored audioUrl audioPath knownPct) =
  Article
    { articleId = Just (ArticleId ident)
    , articleUrl = url
    , articleTitle = title
    , articleSubtitle = subtitle
    , articleAuthor = author
    , articleDate = publishedAt
    , articleSection = section
    , articleParagraphs = splitParagraphs body
    , articleFetchedAt = Just fetchedAt
    , articleUploadedLesson =
        if uploaded == 1 && not (T.null remoteLessonId)
          then Just (LingqLesson remoteLessonId remoteLessonUrl)
          else Nothing
    , articleIgnored = uploadedIntToBool ignored
    , articleAudioUrl = nonEmptyText audioUrl
    , articleAudioPath = nonEmptyTextPath audioPath
    , articleKnownPct = knownPct
    }

data SummaryRow = SummaryRow
  Int
  Text
  Text
  Text
  Int
  Int
  Int
  (Maybe Int)

instance FromRow SummaryRow where
  fromRow =
    SummaryRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

summaryFromRow :: SummaryRow -> ArticleSummary
summaryFromRow (SummaryRow ident url title section totalWords ignored uploaded knownPct) =
  ArticleSummary
    { summaryId = Just (ArticleId ident)
    , summaryUrl = url
    , summaryTitle = title
    , summarySection = section
    , summaryWordCount = totalWords
    , summaryIgnored = uploadedIntToBool ignored
    , summaryUploaded = uploadedIntToBool uploaded
    , summaryKnownPct = knownPct
    }

splitParagraphs :: Text -> [Text]
splitParagraphs =
  filter (not . T.null)
    . map T.strip
    . T.splitOn "\n\n"

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText value = do
  text <- value
  if T.null text then Nothing else Just text

nonEmptyTextPath :: Maybe Text -> Maybe FilePath
nonEmptyTextPath = fmap T.unpack . nonEmptyText

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

uploadedIntToBool :: Int -> Bool
uploadedIntToBool = (/= 0)

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just value <|> _ = Just value
Nothing <|> fallback = fallback
