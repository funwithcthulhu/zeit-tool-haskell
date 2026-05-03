{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.App.Runtime
  ( runCommand
  ) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (catMaybes)
import Data.Map.Strict qualified as Map
import Data.Foldable (traverse_)
import ZeitLingq.App.Update (Command(..), Event(..))
import ZeitLingq.App.UploadConfig (uploadConfigFromPreferences)
import ZeitLingq.Core.Batch (BatchFetchResult(..), batchFetchArticles)
import ZeitLingq.Core.Browse (hideIgnoredSummaries, markIgnoredSummaries)
import ZeitLingq.Core.KnownWords (importKnownWordStems)
import ZeitLingq.Core.Upload (BatchUploadResult(..), batchUploadArticles)
import ZeitLingq.Domain.Article (wordCount)
import ZeitLingq.Domain.Types
import ZeitLingq.Ports

runCommand :: Monad m => AppPorts m -> Command -> m [Event]
runCommand ports command =
  case command of
    PersistCurrentView view ->
      [] <$ saveCurrentView settings view
    LoginZeitWithCookie cookie -> do
      status <- loginToZeitWithCookie zeit cookie
      pure [ZeitStatusChanged status, Notify SuccessNotice "Saved Zeit cookie session."]
    LogoutZeit -> do
      logoutFromZeit zeit
      pure [ZeitStatusChanged (AuthStatus False (Just "disconnected")), ZeitCookieChanged "", Notify SuccessNotice "Disconnected Zeit session."]
    LoginLingqWithApiKey apiKey -> do
      status <- loginToLingqWithApiKey lingq apiKey
      pure [LingqStatusChanged status, Notify SuccessNotice "Connected LingQ API key.", RefreshCurrentView]
    LoginLingqWithPassword username password -> do
      status <- loginToLingq lingq username password
      pure [LingqStatusChanged status, Notify SuccessNotice "Connected LingQ account.", RefreshCurrentView]
    LogoutLingq -> do
      logoutFromLingq lingq
      pure [ LingqStatusChanged (AuthStatus False (Just "disconnected"))
           , LingqApiKeyChanged ""
           , LingqPasswordChanged ""
           , Notify SuccessNotice "Disconnected LingQ."
           ]
    PersistBrowseSection sectionId ->
      [] <$ saveBrowseSection settings sectionId
    PersistBrowseFilter filters ->
      [] <$ saveBrowseFilter settings filters
    PersistBrowseOnlyNew enabled ->
      [] <$ saveBrowseOnlyNew settings enabled
    PersistLingqLanguage languageCode ->
      [] <$ saveLingqLanguage settings languageCode
    PersistLingqFilter filters ->
      [] <$ saveLingqFilter settings filters
    PersistLingqOnlyNotUploaded enabled ->
      [] <$ saveLingqOnlyNotUploaded settings enabled
    PersistDatePrefix enabled ->
      [] <$ saveDatePrefixEnabled settings enabled
    PersistLingqFallbackCollection collectionId ->
      [] <$ saveLingqFallbackCollection settings collectionId
    PersistSectionCollections mappings ->
      [] <$ saveSectionCollections settings mappings
    PersistRowDensity density ->
      [] <$ saveRowDensity settings density
    PersistUiTheme theme ->
      [] <$ saveUiTheme settings theme
    RefreshBrowse sectionId page showHidden -> do
      articles <- fetchArticleList zeit sectionId page
      ignoredUrls <- Set.fromList <$> loadIgnoredUrls library
      savedArticles <- loadLibrary library (WordFilter Nothing Nothing)
      let savedByUrl = Map.fromList [(summaryUrl article, article) | article <- savedArticles]
          annotated = map (mergeSavedSummary savedByUrl) (markIgnoredSummaries ignoredUrls articles)
          visible =
            if showHidden
              then annotated
              else hideIgnoredSummaries ignoredUrls annotated
      pure [BrowseArticlesLoaded visible]
    RefreshLibrary filters -> do
      articles <- loadLibrary library filters
      pure [LibraryArticlesLoaded articles]
    RefreshLibraryPage query -> do
      page <- loadLibraryPage library query
      pure [LibraryPageLoaded page]
    LoadLibraryStats -> do
      stats <- loadStats library
      pure [LibraryStatsLoaded stats]
    RefreshLingqLibrary filters onlyNotUploaded -> do
      let query =
            defaultLibraryQuery
              { libraryWordFilter = filters
              , libraryOnlyNotUploaded = onlyNotUploaded
              , libraryLimit = 10000
              }
      page <- loadLibraryPage library query
      pure [LingqArticlesLoaded (libraryPageArticles page)]
    LoadArticle ident -> do
      article <- loadArticle library ident
      pure
        [ maybe
            (Notify ErrorNotice "Article not found.")
            ArticleContentLoaded
            article
        ]
    PreviewArticle url -> do
      article <- fetchArticleContent zeit url
      pure [ArticleContentLoaded article]
    FetchAndSaveArticle summary -> do
      article <- fetchArticleContent zeit (summaryUrl summary)
      savedId <- saveArticle library article
      pure
        [ Notify SuccessNotice ("Saved article " <> summaryTitle summary <> ".")
        , LibraryArticlesLoaded
            [ summary
                { summaryId = Just savedId
                , summarySection = articleSection article
                , summaryWordCount = wordCount article
                }
            ]
        , RefreshCurrentView
        ]
    DeleteSavedArticle ident -> do
      deleteArticle library ident
      pure
        [ Notify SuccessNotice "Deleted article."
        , ArticleClosed
        ]
    SetArticleIgnored ident ignored -> do
      setArticleIgnored library ident ignored
      pure
        [ Notify SuccessNotice (if ignored then "Article ignored." else "Article unignored.")
        , RefreshCurrentView
        ]
    UploadSavedArticle day languageCode fallbackCollection sectionCollections datePrefix ident -> do
      maybeArticle <- loadArticle library ident
      case maybeArticle of
        Nothing ->
          pure [Notify ErrorNotice "Article not found."]
        Just article -> do
          results <-
            batchUploadArticles
              (\uploadLanguageCode collectionId titledArticle ->
                Right <$> uploadLessonToLingq lingq uploadLanguageCode collectionId titledArticle)
              (\uploadLanguageCode existingLesson titledArticle ->
                Right <$> updateLessonOnLingq lingq uploadLanguageCode existingLesson titledArticle)
              (markArticleUploaded library)
              (uploadConfigFromPreferences day languageCode fallbackCollection datePrefix sectionCollections)
              [article]
          pure (concatMap uploadResultEvents results <> [RefreshCurrentView])
    UploadSavedArticles day languageCode fallbackCollection sectionCollections datePrefix idents -> do
      articles <- catMaybes <$> traverse (loadArticle library) idents
      if null articles
        then pure [Notify ErrorNotice "No uploadable articles selected."]
        else do
          results <-
            batchUploadArticles
              (\uploadLanguageCode collectionId titledArticle ->
                Right <$> uploadLessonToLingq lingq uploadLanguageCode collectionId titledArticle)
              (\uploadLanguageCode existingLesson titledArticle ->
                Right <$> updateLessonOnLingq lingq uploadLanguageCode existingLesson titledArticle)
              (markArticleUploaded library)
              (uploadConfigFromPreferences day languageCode fallbackCollection datePrefix sectionCollections)
              articles
          pure (BatchUploadFinished (batchUploadFailures results) : batchUploadResultEvents results <> [RefreshCurrentView])
    SyncLingqStatus languageCode collectionId -> do
      remoteLessons <- fetchCollectionLessons lingq languageCode collectionId
      page <-
        loadLibraryPage
          library
          defaultLibraryQuery
            { libraryIncludeIgnored = True
            , libraryLimit = 100000
            }
      let syncResult = matchLingqLessons (libraryPageArticles page) remoteLessons
      traverse_ (uncurry (markArticleUploaded library)) (syncMatches syncResult)
      pure
        [ Notify SuccessNotice (syncLingqStatusMessage syncResult)
        , RefreshCurrentView
        ]
    SetBrowseUrlIgnored url -> do
      ignoreArticleUrl library url
      pure
        [ Notify SuccessNotice "Article hidden from browse."
        , RefreshCurrentView
        ]
    SetBrowseUrlUnignored url -> do
      unignoreArticleUrl library url
      pure
        [ Notify SuccessNotice "Article unhidden from browse."
        , RefreshCurrentView
        ]
    FetchAndSaveArticles filters summaries -> do
      results <-
        batchFetchArticles
          (\url -> Right <$> fetchArticleContent zeit url)
          (saveArticle library)
          filters
          (map summaryUrl summaries)
      pure
        [ BatchFetchFinished (batchFetchFailures results)
        , Notify SuccessNotice (batchFetchSummary results)
        , RefreshCurrentView
        ]
    DownloadArticleAudio audioDir ident -> do
      maybeArticle <- loadArticle library ident
      case maybeArticle of
        Nothing ->
          pure [Notify ErrorNotice "Article not found."]
        Just article -> do
          path <- downloadArticleAudioFile audio audioDir article
          setArticleAudioPath library ident (Just path)
          pure
            [ Notify SuccessNotice ("Saved audio: " <> T.pack path)
            , RefreshCurrentView
            ]
    OpenArticleAudio ident -> do
      maybeArticle <- loadArticle library ident
      case maybeArticle >>= articleAudioPath of
        Nothing ->
          pure [Notify ErrorNotice "No downloaded audio file for this article."]
        Just path -> do
          openAudioFile audio path
          pure [Notify SuccessNotice "Opened audio file."]
    SyncKnownWordsFromLingq languageCode -> do
      terms <- fetchKnownWords lingq languageCode
      added <- replaceKnownWords library languageCode (importKnownWordStems (T.unlines terms))
      computeResult <- computeKnownPercentages library languageCode
      pure
        [ Notify SuccessNotice (knownWordsSyncMessage added computeResult)
        , KnownWordsInfoLoaded added
        , RefreshCurrentView
        ]
    ImportKnownWordText languageCode rawWords replaceExisting -> do
      let stems = importKnownWordStems rawWords
      added <-
        if replaceExisting
          then replaceKnownWords library languageCode stems
          else addKnownWords library languageCode stems
      total <- knownStemCount library languageCode
      computeResult <- computeKnownPercentages library languageCode
      pure
        [ Notify SuccessNotice (knownWordsImportMessage added total computeResult)
        , KnownWordsInfoLoaded total
        , KnownWordsImportTextChanged ""
        , RefreshCurrentView
        ]
    ComputeKnownPercentagesFor languageCode -> do
      computeResult <- computeKnownPercentages library languageCode
      pure
        [ Notify (knownWordsComputeLevel computeResult) (knownWordsComputeMessage computeResult)
        , RefreshCurrentView
        ]
    ClearKnownWords languageCode -> do
      clearKnownWords library languageCode
      clearKnownPercentages library
      pure
        [ Notify SuccessNotice "Cleared known words and cached known-word percentages."
        , KnownWordsInfoLoaded 0
        , RefreshCurrentView
        ]
    LoadKnownWordsInfo languageCode -> do
      total <- knownStemCount library languageCode
      pure [KnownWordsInfoLoaded total]
    RefreshLingqLanguages -> do
      languages <- fetchLanguages lingq
      pure [LingqLanguagesLoaded languages]
    RefreshLingqCollections languageCode -> do
      collections <- fetchCollections lingq languageCode
      pure [LingqCollectionsLoaded collections]
    DeleteIgnoredArticles -> do
      deleted <- deleteIgnoredArticles library
      pure
        [ Notify SuccessNotice ("Deleted " <> tshow deleted <> " ignored article(s).")
        , RefreshCurrentView
        ]
    DeleteOlderArticles cutoff onlyUploaded onlyUnuploaded -> do
      deleted <- deleteOlderArticles library cutoff onlyUploaded onlyUnuploaded
      pure
        [ Notify SuccessNotice ("Deleted " <> tshow deleted <> " old article(s).")
        , RefreshCurrentView
        ]
  where
    zeit = zeitPort ports
    lingq = lingqPort ports
    audio = audioPort ports
    library = libraryPort ports
    settings = settingsPort ports

uploadResultEvents :: BatchUploadResult -> [Event]
uploadResultEvents result =
  case result of
    UploadSucceeded _ title _ ->
      [Notify SuccessNotice ("Uploaded " <> title <> " to LingQ.")]
    UploadSucceededUntracked title _ ->
      [Notify SuccessNotice ("Uploaded " <> title <> " to LingQ.")]
    UploadFailed _ title err ->
      [Notify ErrorNotice ("Could not upload " <> title <> ": " <> err)]

batchUploadResultEvents :: [BatchUploadResult] -> [Event]
batchUploadResultEvents results =
  [ Notify level
      ( "Batch upload: uploaded "
          <> tshow uploaded
          <> ", failed "
          <> tshow failed
          <> "."
      )
  ]
  where
    uploaded = length [() | UploadSucceeded {} <- results] + length [() | UploadSucceededUntracked {} <- results]
    failed = length [() | UploadFailed {} <- results]
    level
      | failed > 0 = ErrorNotice
      | otherwise = SuccessNotice

batchUploadFailures :: [BatchUploadResult] -> [(ArticleId, Text)]
batchUploadFailures results =
  [ (ident, title <> ": " <> err)
  | UploadFailed (Just ident) title err <- results
  ]

data LingqStatusSyncResult = LingqStatusSyncResult
  { syncScanned :: Int
  , syncMatches :: [(ArticleId, LingqLesson)]
  , syncAmbiguous :: Int
  }

matchLingqLessons :: [ArticleSummary] -> [LingqRemoteLesson] -> LingqStatusSyncResult
matchLingqLessons articles lessons =
  LingqStatusSyncResult
    { syncScanned = length lessons
    , syncMatches = Map.elems matchedByArticle
    , syncAmbiguous = ambiguous
    }
  where
    articleRows =
      [ article
      | article <- articles
      , Just _ <- [summaryId article]
      ]
    byUrl =
      Map.fromListWith
        (<>)
        [ (normalizeUrl (summaryUrl article), [article])
        | article <- articleRows
        , not (T.null (normalizeUrl (summaryUrl article)))
        ]
    byTitle =
      Map.fromListWith
        (<>)
        [ (normalizeTitle (summaryTitle article), [article])
        | article <- articleRows
        , not (T.null (normalizeTitle (summaryTitle article)))
        ]
    (matchedByArticle, ambiguous) =
      foldl collectMatch (Map.empty, 0) lessons
    collectMatch (matched, ambiguousCount) lesson =
      case lessonCandidates lesson of
        [article] ->
          case summaryId article of
            Just ident ->
              ( Map.insert
                  ident
                  ( ident
                  , LingqLesson (remoteLessonId lesson) (remoteLessonUrl lesson)
                  )
                  matched
              , ambiguousCount
              )
            Nothing -> (matched, ambiguousCount)
        [] -> (matched, ambiguousCount)
        _ -> (matched, ambiguousCount + 1)
    lessonCandidates lesson =
      case remoteLessonOriginalUrl lesson >>= (`Map.lookup` byUrl) . normalizeUrl of
        Just candidates -> candidates
        Nothing ->
          Map.findWithDefault [] (normalizeTitle (remoteLessonTitle lesson)) byTitle

normalizeUrl :: Text -> Text
normalizeUrl =
  T.toLower . T.dropWhileEnd (== '/') . fst . T.breakOn "?" . T.strip

normalizeTitle :: Text -> Text
normalizeTitle =
  T.toLower . T.unwords . T.words . stripDatePrefix

stripDatePrefix :: Text -> Text
stripDatePrefix title
  | hasDatePrefix stripped =
      T.strip (T.dropWhile isDateSeparator (T.drop 10 stripped))
  | otherwise = stripped
  where
    stripped = T.strip title
    hasDatePrefix value =
      T.length value >= 10
        && T.all isDigitText (T.take 4 value)
        && T.index value 4 == '-'
        && T.all isDigitText (T.take 2 (T.drop 5 value))
        && T.index value 7 == '-'
        && T.all isDigitText (T.take 2 (T.drop 8 value))
    isDigitText char = char >= '0' && char <= '9'
    isDateSeparator char = char == ' ' || char == '-'

syncLingqStatusMessage :: LingqStatusSyncResult -> Text
syncLingqStatusMessage result =
  "Synced LingQ status: scanned "
    <> tshow (syncScanned result)
    <> " lesson(s), matched "
    <> tshow (length (syncMatches result))
    <> " article(s), ambiguous "
    <> tshow (syncAmbiguous result)
    <> "."

batchFetchSummary :: [BatchFetchResult] -> Text
batchFetchSummary results =
  "Batch fetch: saved "
    <> tshow saved
    <> ", skipped "
    <> tshow skipped
    <> ", failed "
    <> tshow failed
    <> "."
  where
    saved = length [() | BatchSaved {} <- results]
    skipped = length [() | BatchSkipped {} <- results]
    failed = length [() | BatchFailed {} <- results]

batchFetchFailures :: [BatchFetchResult] -> [(Text, Text)]
batchFetchFailures results =
  [ (url, err)
  | BatchFailed url err <- results
  ]

tshow :: Show a => a -> Text
tshow = T.pack . show

mergeSavedSummary :: Map.Map Text ArticleSummary -> ArticleSummary -> ArticleSummary
mergeSavedSummary savedByUrl summary =
  case Map.lookup (summaryUrl summary) savedByUrl of
    Nothing -> summary
    Just saved ->
      summary
        { summaryId = summaryId saved
        , summaryWordCount = preferPositive (summaryWordCount saved) (summaryWordCount summary)
        , summaryIgnored = summaryIgnored summary || summaryIgnored saved
        , summaryUploaded = summaryUploaded saved
        , summaryKnownPct = summaryKnownPct saved
        }
  where
    preferPositive value fallback
      | value > 0 = value
      | otherwise = fallback

knownWordsSyncMessage :: Int -> Either Text Int -> Text
knownWordsSyncMessage added computeResult =
  case computeResult of
    Right articleCount ->
      "Synced "
        <> tshow added
        <> " known stems and updated "
        <> tshow articleCount
        <> " articles."
    Left err ->
      "Synced " <> tshow added <> " known stems. " <> err

knownWordsImportMessage :: Int -> Int -> Either Text Int -> Text
knownWordsImportMessage added total computeResult =
  case computeResult of
    Right articleCount ->
      "Imported "
        <> tshow added
        <> " stems ("
        <> tshow total
        <> " total) and updated "
        <> tshow articleCount
        <> " articles."
    Left err ->
      "Imported " <> tshow added <> " stems (" <> tshow total <> " total). " <> err

knownWordsComputeLevel :: Either Text Int -> NotificationLevel
knownWordsComputeLevel (Right _) = SuccessNotice
knownWordsComputeLevel (Left _) = ErrorNotice

knownWordsComputeMessage :: Either Text Int -> Text
knownWordsComputeMessage computeResult =
  case computeResult of
    Right articleCount ->
      "Updated known-word estimates for " <> tshow articleCount <> " articles."
    Left err ->
      err
