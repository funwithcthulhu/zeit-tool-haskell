{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Infrastructure.Sqlite
  ( LibraryDb
  , addKnownWordsSqlite
  , closeLibrary
  , deleteArticleSqlite
  , deleteIgnoredSqlite
  , deleteOlderThanSqlite
  , getArticleSqlite
  , getArticlesByQuerySqlite
  , getArticlesSqlite
  , getKnownStemCountSqlite
  , getKnownStemsSqlite
  , getKnownWordsSyncedAtSqlite
  , getIgnoredUrlsSqlite
  , getStatsSqlite
  , clearKnownWordsSqlite
  , clearAllKnownPctSqlite
  , computeKnownPctSqlite
  , saveKnownWordsSqlite
  , ignoreUrlSqlite
  , markUploadedSqlite
  , openLibrary
  , saveArticleSqlite
  , setAudioPathSqlite
  , setAudioUrlSqlite
  , setIgnoredSqlite
  , sqliteLibraryPort
  , unignoreUrlSqlite
  , updateKnownPctSqlite
  , withLibrary
  ) where

import Control.Exception (bracket)
import Data.Int (Int64)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
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
import ZeitLingq.Core.KnownWords (estimateKnownPct)
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
    , setArticleIgnored = setIgnoredSqlite db
    , markArticleUploaded = markUploadedSqlite db
    , setArticleAudioPath = setAudioPathSqlite db
    , loadIgnoredUrls = getIgnoredUrlsSqlite db
    , ignoreArticleUrl = ignoreUrlSqlite db
    , unignoreArticleUrl = unignoreUrlSqlite db
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
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS known_words\
    \ (lang TEXT NOT NULL,\
    \  stem TEXT NOT NULL,\
    \  synced_at TIMESTAMP NOT NULL,\
    \  PRIMARY KEY (lang, stem))"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_known_words_lang ON known_words(lang)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS ignored_urls\
    \ (url TEXT PRIMARY KEY,\
    \  ignored_at TIMESTAMP NOT NULL)"

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
getArticlesSqlite db filters =
  libraryPageArticles <$> getArticlesByQuerySqlite db defaultLibraryQuery
    { libraryWordFilter = filters
    , libraryLimit = 10000
    }

getArticlesByQuerySqlite :: LibraryDb -> LibraryQuery -> IO LibraryPage
getArticlesByQuerySqlite (LibraryDb conn) libraryQuery = do
  [Only total] <- query conn countQuery whereParams
  rows <- query conn pageQuery pageParams
  pure
    LibraryPage
      { libraryPageArticles = map summaryFromRow rows
      , libraryPageTotal = total
      }
  where
    (whereClause, whereParams) = libraryWhere libraryQuery
    countQuery =
      Query ("SELECT COUNT(*) FROM articles WHERE " <> whereClause)
    pageQuery =
      Query
        ( "SELECT id, url, title, section, word_count, ignored, uploaded_to_lingq, known_pct\
          \ FROM articles\
          \ WHERE "
            <> whereClause
            <> " ORDER BY fetched_at DESC LIMIT ? OFFSET ?"
        )
    pageParams =
      whereParams
        <> [ SQLInteger (safeLimit (libraryLimit libraryQuery))
           , SQLInteger (safeOffset (libraryOffset libraryQuery))
           ]

defaultLibraryQuery :: LibraryQuery
defaultLibraryQuery =
  LibraryQuery
    { librarySearch = Nothing
    , librarySection = Nothing
    , libraryWordFilter = WordFilter Nothing Nothing
    , libraryIncludeIgnored = False
    , libraryOnlyIgnored = False
    , libraryOnlyNotUploaded = False
    , libraryLimit = 50
    , libraryOffset = 0
    }

libraryWhere :: LibraryQuery -> (Text, [SQLData])
libraryWhere libraryQuery =
  (T.intercalate " AND " (map fst fragments), concatMap snd fragments)
  where
    filters = libraryWordFilter libraryQuery
    fragments =
      [ ("1=1", [])
      ]
        <> maybeFragment (librarySearch libraryQuery) searchFragment
        <> maybeFragment (librarySection libraryQuery) sectionFragment
        <> maybeFragment (minWords filters) minWordsFragment
        <> maybeFragment (maxWords filters) maxWordsFragment
        <> ignoredFragments
        <> notUploadedFragments

    searchFragment raw =
      let term = "%" <> T.strip raw <> "%"
       in [("(title LIKE ? OR body_text LIKE ?)", [SQLText term, SQLText term]) | not (T.null (T.strip raw))]
    sectionFragment raw =
      [("section = ?", [SQLText raw]) | not (T.null (T.strip raw))]
    minWordsFragment value =
      [("word_count >= ?", [SQLInteger (fromIntegral value)])]
    maxWordsFragment value =
      [("word_count <= ?", [SQLInteger (fromIntegral value)])]
    ignoredFragments
      | libraryOnlyIgnored libraryQuery = [("ignored = 1", [])]
      | libraryIncludeIgnored libraryQuery = []
      | otherwise = [("ignored = 0", [])]
    notUploadedFragments
      | libraryOnlyNotUploaded libraryQuery = [("uploaded_to_lingq = 0", [])]
      | otherwise = []

maybeFragment :: Maybe a -> (a -> [(Text, [SQLData])]) -> [(Text, [SQLData])]
maybeFragment Nothing _ = []
maybeFragment (Just value) build = build value

safeLimit :: Int -> Int64
safeLimit value
  | value <= 0 = 50
  | otherwise = fromIntegral value

safeOffset :: Int -> Int64
safeOffset value
  | value <= 0 = 0
  | otherwise = fromIntegral value

deleteArticleSqlite :: LibraryDb -> ArticleId -> IO ()
deleteArticleSqlite (LibraryDb conn) (ArticleId ident) =
  execute conn "DELETE FROM articles WHERE id = ?" (Only ident)

deleteIgnoredSqlite :: LibraryDb -> IO Int
deleteIgnoredSqlite (LibraryDb conn) = do
  [Only deleted] <- query_ conn "SELECT COUNT(*) FROM articles WHERE ignored = 1"
  execute_ conn "DELETE FROM articles WHERE ignored = 1"
  pure deleted

deleteOlderThanSqlite :: LibraryDb -> UTCTime -> Bool -> Bool -> IO Int
deleteOlderThanSqlite (LibraryDb conn) cutoff onlyUploaded onlyUnuploaded = do
  [Only deleted] <- query conn countQuery params
  execute conn deleteQuery params
  pure deleted
  where
    (whereClause, params) =
      deleteOlderWhere cutoff onlyUploaded onlyUnuploaded
    countQuery =
      Query ("SELECT COUNT(*) FROM articles WHERE " <> whereClause)
    deleteQuery =
      Query ("DELETE FROM articles WHERE " <> whereClause)

deleteOlderWhere :: UTCTime -> Bool -> Bool -> (Text, [SQLData])
deleteOlderWhere cutoff onlyUploaded onlyUnuploaded =
  (T.intercalate " AND " (map fst fragments), concatMap snd fragments)
  where
    fragments =
      [ ("fetched_at < ?", [SQLText (T.pack (show cutoff))])
      ]
        <> [("uploaded_to_lingq = 1", []) | onlyUploaded]
        <> [("uploaded_to_lingq = 0", []) | onlyUnuploaded]

setIgnoredSqlite :: LibraryDb -> ArticleId -> Bool -> IO ()
setIgnoredSqlite (LibraryDb conn) (ArticleId ident) ignored =
  execute conn "UPDATE articles SET ignored = ? WHERE id = ?" (boolToInt ignored, ident)

ignoreUrlSqlite :: LibraryDb -> Text -> IO ()
ignoreUrlSqlite (LibraryDb conn) url = do
  now <- getCurrentTime
  execute conn "INSERT OR REPLACE INTO ignored_urls (url, ignored_at) VALUES (?, ?)" (url, now)

unignoreUrlSqlite :: LibraryDb -> Text -> IO ()
unignoreUrlSqlite (LibraryDb conn) url =
  execute conn "DELETE FROM ignored_urls WHERE url = ?" (Only url)

getIgnoredUrlsSqlite :: LibraryDb -> IO [Text]
getIgnoredUrlsSqlite (LibraryDb conn) = do
  rows <- query_ conn "SELECT url FROM ignored_urls ORDER BY url ASC"
  pure [url | Only url <- rows]

markUploadedSqlite :: LibraryDb -> ArticleId -> LingqLesson -> IO ()
markUploadedSqlite (LibraryDb conn) (ArticleId ident) LingqLesson {lessonId, lessonUrl} =
  execute
    conn
    "UPDATE articles SET uploaded_to_lingq = 1, lingq_lesson_id = ?, lingq_lesson_url = ? WHERE id = ?"
    (lessonId, lessonUrl, ident)

setAudioUrlSqlite :: LibraryDb -> ArticleId -> Maybe Text -> IO ()
setAudioUrlSqlite (LibraryDb conn) (ArticleId ident) audioUrl =
  execute conn "UPDATE articles SET audio_url = ? WHERE id = ?" (audioUrl, ident)

setAudioPathSqlite :: LibraryDb -> ArticleId -> Maybe FilePath -> IO ()
setAudioPathSqlite (LibraryDb conn) (ArticleId ident) audioPath =
  execute conn "UPDATE articles SET audio_path = ? WHERE id = ?" (audioPath, ident)

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

saveKnownWordsSqlite :: LibraryDb -> Text -> Set Text -> IO Int
saveKnownWordsSqlite (LibraryDb conn) languageCode stems = do
  now <- getCurrentTime
  withTransaction conn $ do
    execute conn "DELETE FROM known_words WHERE lang = ?" (Only languageCode)
    executeMany conn "INSERT OR IGNORE INTO known_words (lang, stem, synced_at) VALUES (?, ?, ?)" (knownWordRows languageCode now stems)
  pure (Set.size stems)

addKnownWordsSqlite :: LibraryDb -> Text -> Set Text -> IO Int
addKnownWordsSqlite (LibraryDb conn) languageCode stems = do
  now <- getCurrentTime
  executeMany conn "INSERT OR IGNORE INTO known_words (lang, stem, synced_at) VALUES (?, ?, ?)" (knownWordRows languageCode now stems)
  pure (Set.size stems)

clearKnownWordsSqlite :: LibraryDb -> Text -> IO ()
clearKnownWordsSqlite (LibraryDb conn) languageCode =
  execute conn "DELETE FROM known_words WHERE lang = ?" (Only languageCode)

getKnownStemCountSqlite :: LibraryDb -> Text -> IO Int
getKnownStemCountSqlite (LibraryDb conn) languageCode = do
  [Only total] <- query conn "SELECT COUNT(*) FROM known_words WHERE lang = ?" (Only languageCode)
  pure total

getKnownStemsSqlite :: LibraryDb -> Text -> IO (Set Text)
getKnownStemsSqlite (LibraryDb conn) languageCode = do
  rows <- query conn "SELECT stem FROM known_words WHERE lang = ?" (Only languageCode)
  pure (Set.fromList [stem | Only stem <- rows])

getKnownWordsSyncedAtSqlite :: LibraryDb -> Text -> IO (Maybe UTCTime)
getKnownWordsSyncedAtSqlite (LibraryDb conn) languageCode = do
  [Only syncedAt] <- query conn "SELECT MAX(synced_at) FROM known_words WHERE lang = ?" (Only languageCode)
  pure syncedAt

updateKnownPctSqlite :: LibraryDb -> ArticleId -> Maybe Int -> IO ()
updateKnownPctSqlite (LibraryDb conn) (ArticleId ident) pct =
  execute conn "UPDATE articles SET known_pct = ? WHERE id = ?" (pct, ident)

clearAllKnownPctSqlite :: LibraryDb -> IO ()
clearAllKnownPctSqlite (LibraryDb conn) =
  execute_ conn "UPDATE articles SET known_pct = NULL"

computeKnownPctSqlite :: LibraryDb -> Text -> IO (Either Text Int)
computeKnownPctSqlite db@(LibraryDb conn) languageCode = do
  stems <- getKnownStemsSqlite db languageCode
  if Set.null stems
    then pure (Left "No known words in database. Sync or import first.")
    else do
      rows <- query_ conn "SELECT id, body_text FROM articles"
      let computeOne (ArticleTextRow ident body) =
            updateKnownPctSqlite db (ArticleId ident) (estimateKnownPct stems body)
      traverse_ computeOne rows
      pure (Right (length rows))

data PreservedFields = PreservedFields
  { preservedId :: ArticleId
  , preservedLesson :: Maybe LingqLesson
  , preservedIgnored :: Bool
  , preservedAudioPath :: Maybe FilePath
  , preservedKnownPct :: Maybe Int
  }

knownWordRows :: Text -> UTCTime -> Set Text -> [(Text, Text, UTCTime)]
knownWordRows languageCode now =
  map (\stem -> (languageCode, stem, now))
    . filter (not . T.null)
    . Set.toList

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

data ArticleTextRow = ArticleTextRow Int Text

instance FromRow ArticleTextRow where
  fromRow = ArticleTextRow <$> field <*> field

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
