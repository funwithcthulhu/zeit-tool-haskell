{-# LANGUAGE OverloadedStrings #-}

module ZeitLingq.Cli (
  CliCommand (..),
  defaultDbPath,
  defaultSettingsPath,
  parseArgs,
  usageText,
) where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import ZeitLingq.Domain.Types (View (..), WordFilter (..))

data CliCommand
  = ShowHelp
  | ListSections
  | BrowseZeit Text Int FilePath
  | FetchArticle Text FilePath
  | BatchFetch FilePath FilePath WordFilter
  | ShowLibrary FilePath
  | ShowStats FilePath
  | DeleteArticle Int FilePath
  | DeleteOlderThan Int Bool Bool FilePath
  | DeleteIgnored FilePath
  | IgnoreArticle Int FilePath
  | UnignoreArticle Int FilePath
  | SyncKnownWords FilePath
  | ImportKnownWords FilePath FilePath
  | KnownWordsInfo FilePath
  | ComputeKnownPct FilePath
  | UploadLingq Int FilePath FilePath
  | DownloadAudio Int FilePath FilePath
  | IgnoreUrl Text FilePath
  | UnignoreUrl Text FilePath
  | ListIgnored FilePath
  | ShowSettings FilePath
  | SetSettingsView View FilePath
  | SetSettingsBrowseSection Text FilePath
  | SetSettingsDatePrefix Bool FilePath
  | SetSettingsSectionCollection Text Text FilePath
  | ClearSettingsSectionCollection Text FilePath
  deriving (Eq, Show)

defaultDbPath :: FilePath
defaultDbPath = "zeit-tool.db"

defaultSettingsPath :: FilePath
defaultSettingsPath = "settings.json"

parseArgs :: [String] -> Either String CliCommand
parseArgs rawArgs
  | any isHelpArg args = Right ShowHelp
  | otherwise =
      case args of
        [] -> Right ShowHelp
        [command] | lower command `elem` ["topics", "sections", "t"] -> Right ListSections
        command : rest ->
          case lower command of
            "topics" -> parseNoArgs "topics" rest ListSections
            "sections" -> parseNoArgs "sections" rest ListSections
            "t" -> parseNoArgs "t" rest ListSections
            "browse" -> parseBrowse rest
            "b" -> parseBrowse rest
            "read" -> parseFetch "read" rest
            "fetch" -> parseFetch "fetch" rest
            "save" -> parseFetch "save" rest
            "r" -> parseFetch "r" rest
            "fetch-list" -> parseBatchFetch "fetch-list" rest
            "f" -> parseBatchFetch "f" rest
            "batch" -> parseBatchFetch "batch" rest
            "batch-fetch" -> parseBatchFetch "batch-fetch" rest
            "library" -> parseDbOnly "library" ShowLibrary rest
            "list" -> parseDbOnly "list" ShowLibrary rest
            "l" -> parseDbOnly "l" ShowLibrary rest
            "stats" -> parseDbOnly "stats" ShowStats rest
            "s" -> parseDbOnly "s" ShowStats rest
            "delete" -> parseDelete rest
            "del" -> parseDelete rest
            "rm" -> parseDelete rest
            "delete-article" -> parseArticleIdWithDb "delete-article" DeleteArticle rest
            "delete-older-than" -> parseDeleteOldLegacy "delete-older-than" False False rest
            "delete-older-than-uploaded" -> parseDeleteOldLegacy "delete-older-than-uploaded" True False rest
            "delete-older-than-unuploaded" -> parseDeleteOldLegacy "delete-older-than-unuploaded" False True rest
            "delete-ignored" -> parseDbOnly "delete-ignored" DeleteIgnored rest
            "hide" -> parseHide True rest
            "show" -> parseHide False rest
            "ignore-article" -> parseArticleIdWithDb "ignore-article" IgnoreArticle rest
            "unignore-article" -> parseArticleIdWithDb "unignore-article" UnignoreArticle rest
            "ignore-url" -> parseUrlWithDb "ignore-url" IgnoreUrl rest
            "unignore-url" -> parseUrlWithDb "unignore-url" UnignoreUrl rest
            "hidden" -> parseDbOnly "hidden" ListIgnored rest
            "ignored" -> parseDbOnly "ignored" ListIgnored rest
            "known" -> parseKnown rest
            "k" -> parseKnown rest
            "known-sync" -> parseDbOnly "known-sync" SyncKnownWords rest
            "known-import" -> parseImportKnown "known-import" rest
            "known-info" -> parseDbOnly "known-info" KnownWordsInfo rest
            "known-compute" -> parseDbOnly "known-compute" ComputeKnownPct rest
            "lingq" -> parseLingq rest
            "u" -> parseLingqUpload "u" rest
            "upload" -> parseLingqUpload "upload" rest
            "lingq-upload" -> parseLingqUpload "lingq-upload" rest
            "audio" -> parseAudio rest
            "a" -> parseAudio rest
            "audio-download" -> parseAudioDownload "audio-download" rest
            "settings" -> parseSettings rest
            "cfg" -> parseSettings rest
            "config" -> parseSettings rest
            "set" -> parseSettings rest
            _ | isUrl command -> parseFetch "url" (command : rest)
            _ -> invalidCommand command
 where
  args = concatMap expandEquals rawArgs

usageText :: String
usageText =
  unlines
    [ "Zeit Tool Haskell CLI"
    , ""
    , "Use the desktop app for daily work. Use `zt` only for quick checks or scripts."
    , ""
    , "Short commands:"
    , "  zt h                         Help"
    , "  zt t                         Topics"
    , "  zt b [topic] [-p N]          Browse Zeit"
    , "  zt r <url>                   Read and save one article"
    , "  zt <url>                     Same as `zt r <url>`"
    , "  zt f <urls.txt> [-m N] [-x N] Fetch URL list"
    , "  zt l                         Library"
    , "  zt s                         Stats"
    , ""
    , "Library cleanup:"
    , "  zt hide <id|url>"
    , "  zt show <id|url>"
    , "  zt hidden"
    , "  zt rm <id>"
    , "  zt rm old <days> [-u|-n]"
    , "  zt rm ignored"
    , ""
    , "Known words, LingQ, and audio:"
    , "  zt k                         Known-word status"
    , "  zt k sync"
    , "  zt k import <words.txt>"
    , "  zt k re"
    , "  zt u <id>                    Upload article to LingQ"
    , "  zt a <id> [-o DIR]           Download article audio"
    , ""
    , "Settings:"
    , "  zt cfg"
    , "  zt cfg view <browse|library|lingq|zeit-login|diagnostics|article>"
    , "  zt cfg topic <section-id>"
    , "  zt cfg date <on|off>"
    , "  zt cfg map <section-name> <collection-id>"
    , "  zt cfg unmap <section-name>"
    , ""
    , "Flags:"
    , "  -d, --db FILE        SQLite library path."
    , "  -s, --settings FILE  Settings file path."
    , "  -p, --page N         Browse page."
    , "  -m, --min N          Minimum words for URL-list fetch."
    , "  -x, --max N          Maximum words for URL-list fetch."
    , "  -o, --to DIR         Audio folder."
    , "  -u, --uploaded       Delete only uploaded old articles."
    , "  -n, --unuploaded     Delete only unuploaded old articles."
    , ""
    , "Set ZEIT_COOKIE to pass an authenticated zeit.de cookie header for paid articles."
    , "Set ZEIT_USER_AGENT too when scripting with cookies imported from a browser."
    , "Set LINGQ_API_KEY, and optionally LINGQ_COLLECTION_ID, before uploading to LingQ."
    , ""
    , "Long commands such as `zeit-lingq-tool browse` still work for existing scripts."
    ]

parseNoArgs :: String -> [String] -> CliCommand -> Either String CliCommand
parseNoArgs _ [] command = Right command
parseNoArgs commandName _ _ = invalidCommand commandName

parseBrowse :: [String] -> Either String CliCommand
parseBrowse args = do
  (withoutDb, dbOpt) <- takeValueOption dbOptionLabels args
  (remaining, pageOpt) <- takeValueOption ["--page", "-p"] withoutDb
  case (remaining, pageOpt, dbOpt) of
    ([], _, _) ->
      BrowseZeit "index" <$> parseMaybePage pageOpt <*> pure (fromMaybe defaultDbPath dbOpt)
    ([sectionId], _, _) ->
      BrowseZeit (T.pack sectionId) <$> parseMaybePage pageOpt <*> pure (fromMaybe defaultDbPath dbOpt)
    ([sectionId, pageValue], Nothing, _) ->
      BrowseZeit (T.pack sectionId)
        <$> parsePositiveInt "page" pageValue
        <*> pure (fromMaybe defaultDbPath dbOpt)
    ([sectionId, pageValue, legacyDb], Nothing, Nothing) ->
      BrowseZeit (T.pack sectionId)
        <$> parsePositiveInt "page" pageValue
        <*> pure legacyDb
    _ -> invalidCommand "browse"

parseFetch :: String -> [String] -> Either String CliCommand
parseFetch commandName args =
  parseUrlWithDb commandName FetchArticle args

parseBatchFetch :: String -> [String] -> Either String CliCommand
parseBatchFetch commandName args = do
  (withoutDb, dbOpt) <- takeValueOption dbOptionLabels args
  (withoutMin, minOpt) <- takeValueOption ["--min", "--min-words", "-m"] withoutDb
  (remaining, maxOpt) <- takeValueOption ["--max", "--max-words", "-x"] withoutMin
  minWords <- parseMaybePositive "min-words" minOpt
  maxWords <- parseMaybePositive "max-words" maxOpt
  let flaggedFilter = WordFilter minWords maxWords
  case (remaining, minOpt, maxOpt, dbOpt) of
    ([sourcePath], _, _, _) ->
      Right (BatchFetch sourcePath (fromMaybe defaultDbPath dbOpt) flaggedFilter)
    ([sourcePath, legacyDb], _, _, Nothing) ->
      Right (BatchFetch sourcePath legacyDb flaggedFilter)
    ([sourcePath, legacyDb, minValue, maxValue], Nothing, Nothing, Nothing) ->
      BatchFetch sourcePath legacyDb
        <$> ( WordFilter
                <$> fmap Just (parsePositiveInt "min-words" minValue)
                <*> fmap Just (parsePositiveInt "max-words" maxValue)
            )
    _ -> invalidCommand commandName

parseDbOnly :: String -> (FilePath -> CliCommand) -> [String] -> Either String CliCommand
parseDbOnly commandName constructor args = do
  (remaining, dbOpt) <- takeValueOption dbOptionLabels args
  case (remaining, dbOpt) of
    ([], _) -> Right (constructor (fromMaybe defaultDbPath dbOpt))
    ([legacyDb], Nothing) -> Right (constructor legacyDb)
    _ -> invalidCommand commandName

parseArticleIdWithDb ::
  String -> (Int -> FilePath -> CliCommand) -> [String] -> Either String CliCommand
parseArticleIdWithDb commandName constructor args = do
  (remaining, dbOpt) <- takeValueOption dbOptionLabels args
  case (remaining, dbOpt) of
    ([articleId], _) ->
      constructor <$> parsePositiveInt "article-id" articleId <*> pure (fromMaybe defaultDbPath dbOpt)
    ([articleId, legacyDb], Nothing) ->
      constructor <$> parsePositiveInt "article-id" articleId <*> pure legacyDb
    _ -> invalidCommand commandName

parseUrlWithDb :: String -> (Text -> FilePath -> CliCommand) -> [String] -> Either String CliCommand
parseUrlWithDb commandName constructor args = do
  (remaining, dbOpt) <- takeValueOption dbOptionLabels args
  case (remaining, dbOpt) of
    ([url], _) -> Right (constructor (T.pack url) (fromMaybe defaultDbPath dbOpt))
    ([url, legacyDb], Nothing) -> Right (constructor (T.pack url) legacyDb)
    _ -> invalidCommand commandName

parseDelete :: [String] -> Either String CliCommand
parseDelete [] = invalidCommand "delete"
parseDelete (subcommand : rest) =
  case lower subcommand of
    "article" -> parseArticleIdWithDb "delete article" DeleteArticle rest
    "old" -> parseDeleteOld rest
    "older" -> parseDeleteOld rest
    "ignored" -> parseDbOnly "delete ignored" DeleteIgnored rest
    _ | looksPositiveInt subcommand -> parseArticleIdWithDb "delete" DeleteArticle (subcommand : rest)
    _ -> invalidCommand "delete"

parseDeleteOld :: [String] -> Either String CliCommand
parseDeleteOld args = do
  (withoutDb, dbOpt) <- takeValueOption dbOptionLabels args
  let (withoutUploaded, onlyUploaded) = takeSwitch ["--uploaded", "-u"] withoutDb
      (remaining, onlyUnuploaded) = takeSwitch ["--unuploaded", "-n"] withoutUploaded
  if onlyUploaded && onlyUnuploaded
    then Left ("Use either --uploaded or --unuploaded, not both.\n\n" <> usageText)
    else case remaining of
      [days] ->
        DeleteOlderThan
          <$> parsePositiveInt "days" days
          <*> pure onlyUploaded
          <*> pure onlyUnuploaded
          <*> pure (fromMaybe defaultDbPath dbOpt)
      _ -> invalidCommand "delete old"

parseDeleteOldLegacy :: String -> Bool -> Bool -> [String] -> Either String CliCommand
parseDeleteOldLegacy commandName onlyUploaded onlyUnuploaded args = do
  (remaining, dbOpt) <- takeValueOption dbOptionLabels args
  case (remaining, dbOpt) of
    ([days], _) ->
      DeleteOlderThan
        <$> parsePositiveInt "days" days
        <*> pure onlyUploaded
        <*> pure onlyUnuploaded
        <*> pure (fromMaybe defaultDbPath dbOpt)
    ([days, legacyDb], Nothing) ->
      DeleteOlderThan
        <$> parsePositiveInt "days" days
        <*> pure onlyUploaded
        <*> pure onlyUnuploaded
        <*> pure legacyDb
    _ -> invalidCommand commandName

parseHide :: Bool -> [String] -> Either String CliCommand
parseHide hidden args =
  case args of
    [] -> invalidCommand (if hidden then "hide" else "show")
    subcommand : rest ->
      case lower subcommand of
        "article" -> parseArticleIdWithDb commandName articleConstructor rest
        "url" -> parseUrlWithDb commandName urlConstructor rest
        _ | isUrl subcommand -> parseUrlWithDb commandName urlConstructor args
        _ -> parseArticleIdWithDb commandName articleConstructor args
 where
  commandName = if hidden then "hide" else "show"
  articleConstructor = if hidden then IgnoreArticle else UnignoreArticle
  urlConstructor = if hidden then IgnoreUrl else UnignoreUrl

parseKnown :: [String] -> Either String CliCommand
parseKnown [] = Right (KnownWordsInfo defaultDbPath)
parseKnown (subcommand : rest) =
  case lower subcommand of
    "--db" -> parseDbOnly "known" KnownWordsInfo (subcommand : rest)
    "-d" -> parseDbOnly "known" KnownWordsInfo (subcommand : rest)
    "s" -> parseDbOnly "known sync" SyncKnownWords rest
    "sync" -> parseDbOnly "known sync" SyncKnownWords rest
    "update" -> parseDbOnly "known update" SyncKnownWords rest
    "i" -> parseImportKnown "known import" rest
    "import" -> parseImportKnown "known import" rest
    "info" -> parseDbOnly "known info" KnownWordsInfo rest
    "r" -> parseDbOnly "known recompute" ComputeKnownPct rest
    "re" -> parseDbOnly "known recompute" ComputeKnownPct rest
    "pct" -> parseDbOnly "known recompute" ComputeKnownPct rest
    "compute" -> parseDbOnly "known compute" ComputeKnownPct rest
    "recompute" -> parseDbOnly "known recompute" ComputeKnownPct rest
    _ -> invalidCommand "known"

parseImportKnown :: String -> [String] -> Either String CliCommand
parseImportKnown commandName args = do
  (remaining, dbOpt) <- takeValueOption dbOptionLabels args
  case (remaining, dbOpt) of
    ([sourcePath], _) -> Right (ImportKnownWords sourcePath (fromMaybe defaultDbPath dbOpt))
    ([sourcePath, legacyDb], Nothing) -> Right (ImportKnownWords sourcePath legacyDb)
    _ -> invalidCommand commandName

parseLingq :: [String] -> Either String CliCommand
parseLingq [] = invalidCommand "lingq"
parseLingq (subcommand : rest) =
  case lower subcommand of
    "upload" -> parseLingqUpload "lingq upload" rest
    _ | looksPositiveInt subcommand -> parseLingqUpload "lingq" (subcommand : rest)
    _ -> invalidCommand "lingq"

parseLingqUpload :: String -> [String] -> Either String CliCommand
parseLingqUpload commandName args = do
  (withoutDb, dbOpt) <- takeValueOption dbOptionLabels args
  (remaining, settingsOpt) <- takeValueOption settingsOptionLabels withoutDb
  case (remaining, dbOpt, settingsOpt) of
    ([articleId], _, _) ->
      UploadLingq
        <$> parsePositiveInt "article-id" articleId
        <*> pure (fromMaybe defaultDbPath dbOpt)
        <*> pure (fromMaybe defaultSettingsPath settingsOpt)
    ([articleId, legacyDb], Nothing, _) ->
      UploadLingq
        <$> parsePositiveInt "article-id" articleId
        <*> pure legacyDb
        <*> pure (fromMaybe defaultSettingsPath settingsOpt)
    ([articleId, legacyDb, legacySettings], Nothing, Nothing) ->
      UploadLingq
        <$> parsePositiveInt "article-id" articleId
        <*> pure legacyDb
        <*> pure legacySettings
    _ -> invalidCommand commandName

parseAudio :: [String] -> Either String CliCommand
parseAudio [] = invalidCommand "audio"
parseAudio [articleId] = parseAudioDownload "audio" [articleId]
parseAudio (subcommand : rest) =
  case lower subcommand of
    "download" -> parseAudioDownload "audio download" rest
    "get" -> parseAudioDownload "audio get" rest
    _ -> parseAudioDownload "audio" (subcommand : rest)

parseAudioDownload :: String -> [String] -> Either String CliCommand
parseAudioDownload commandName args = do
  (withoutDb, dbOpt) <- takeValueOption dbOptionLabels args
  (remaining, audioDirOpt) <- takeValueOption ["--to", "--audio-dir", "-o"] withoutDb
  case (remaining, audioDirOpt, dbOpt) of
    ([articleId], _, _) ->
      DownloadAudio
        <$> parsePositiveInt "article-id" articleId
        <*> pure (fromMaybe "audio" audioDirOpt)
        <*> pure (fromMaybe defaultDbPath dbOpt)
    ([articleId, legacyAudioDir], Nothing, _) ->
      DownloadAudio
        <$> parsePositiveInt "article-id" articleId
        <*> pure legacyAudioDir
        <*> pure (fromMaybe defaultDbPath dbOpt)
    ([articleId, legacyAudioDir, legacyDb], Nothing, Nothing) ->
      DownloadAudio
        <$> parsePositiveInt "article-id" articleId
        <*> pure legacyAudioDir
        <*> pure legacyDb
    _ -> invalidCommand commandName

parseSettings :: [String] -> Either String CliCommand
parseSettings args = do
  (remaining, settingsOpt) <- takeValueOption settingsOptionLabels args
  case remaining of
    [] -> Right (ShowSettings (fromMaybe defaultSettingsPath settingsOpt))
    [legacySettings]
      | settingsOpt == Nothing && not (isSettingsSubcommand legacySettings) ->
          Right (ShowSettings legacySettings)
    subcommand : values ->
      case lower subcommand of
        "set-view" -> parseSettingsView "settings set-view" settingsOpt values
        "v" -> parseSettingsView "settings view" settingsOpt values
        "view" -> parseSettingsView "settings view" settingsOpt values
        "set-browse-section" -> parseSettingsBrowseSection "settings set-browse-section" settingsOpt values
        "t" -> parseSettingsBrowseSection "settings topic" settingsOpt values
        "topic" -> parseSettingsBrowseSection "settings topic" settingsOpt values
        "browse-section" -> parseSettingsBrowseSection "settings browse-section" settingsOpt values
        "set-date-prefix" -> parseSettingsDatePrefix "settings set-date-prefix" settingsOpt values
        "date" -> parseSettingsDatePrefix "settings date-prefix" settingsOpt values
        "dp" -> parseSettingsDatePrefix "settings date-prefix" settingsOpt values
        "date-prefix" -> parseSettingsDatePrefix "settings date-prefix" settingsOpt values
        "set-collection" -> parseSettingsCollection "settings set-collection" settingsOpt values
        "map" -> parseSettingsCollection "settings collection" settingsOpt values
        "collection" -> parseSettingsCollection "settings collection" settingsOpt values
        "clear-collection" -> parseSettingsClearCollection "settings clear-collection" settingsOpt values
        "unmap" -> parseSettingsClearCollection "settings forget-collection" settingsOpt values
        "forget-collection" -> parseSettingsClearCollection "settings forget-collection" settingsOpt values
        _ -> invalidCommand "settings"

parseSettingsView :: String -> Maybe FilePath -> [String] -> Either String CliCommand
parseSettingsView commandName settingsOpt values =
  case values of
    viewValue : rest ->
      SetSettingsView <$> parseView "view" viewValue <*> parseSettingsPath commandName settingsOpt rest
    _ -> invalidCommand commandName

parseSettingsBrowseSection :: String -> Maybe FilePath -> [String] -> Either String CliCommand
parseSettingsBrowseSection commandName settingsOpt values =
  case values of
    sectionId : rest ->
      SetSettingsBrowseSection (T.pack sectionId) <$> parseSettingsPath commandName settingsOpt rest
    _ -> invalidCommand commandName

parseSettingsDatePrefix :: String -> Maybe FilePath -> [String] -> Either String CliCommand
parseSettingsDatePrefix commandName settingsOpt values =
  case values of
    enabledValue : rest ->
      SetSettingsDatePrefix
        <$> parseToggle "date-prefix" enabledValue
        <*> parseSettingsPath commandName settingsOpt rest
    _ -> invalidCommand commandName

parseSettingsCollection :: String -> Maybe FilePath -> [String] -> Either String CliCommand
parseSettingsCollection commandName settingsOpt values =
  case values of
    sectionName : collectionId : rest ->
      SetSettingsSectionCollection (T.pack sectionName) (T.pack collectionId)
        <$> parseSettingsPath commandName settingsOpt rest
    _ -> invalidCommand commandName

parseSettingsClearCollection :: String -> Maybe FilePath -> [String] -> Either String CliCommand
parseSettingsClearCollection commandName settingsOpt values =
  case values of
    sectionName : rest ->
      ClearSettingsSectionCollection (T.pack sectionName)
        <$> parseSettingsPath commandName settingsOpt rest
    _ -> invalidCommand commandName

parseSettingsPath :: String -> Maybe FilePath -> [String] -> Either String FilePath
parseSettingsPath commandName settingsOpt remaining =
  case (remaining, settingsOpt) of
    ([], _) -> Right (fromMaybe defaultSettingsPath settingsOpt)
    ([legacySettings], Nothing) -> Right legacySettings
    _ -> invalidCommand commandName

parseMaybePage :: Maybe String -> Either String Int
parseMaybePage Nothing = Right 1
parseMaybePage (Just raw) = parsePositiveInt "page" raw

parseMaybePositive :: String -> Maybe String -> Either String (Maybe Int)
parseMaybePositive _ Nothing = Right Nothing
parseMaybePositive label (Just raw) = Just <$> parsePositiveInt label raw

takeValueOption :: [String] -> [String] -> Either String ([String], Maybe String)
takeValueOption labels = go [] Nothing
 where
  go kept found [] = Right (reverse kept, found)
  go _ _ [arg]
    | lower arg `elem` labels =
        Left ("Missing value for " <> arg <> ".\n\n" <> usageText)
  go kept _ (arg : value : rest)
    | lower arg `elem` labels =
        go kept (Just value) rest
  go kept found (arg : rest) =
    go (arg : kept) found rest

takeSwitch :: [String] -> [String] -> ([String], Bool)
takeSwitch labels args =
  (filter (not . (`elem` labels) . lower) args, any ((`elem` labels) . lower) args)

expandEquals :: String -> [String]
expandEquals raw =
  case break (== '=') raw of
    (flag, '=' : value)
      | "--" `isPrefixOf` flag && not (null value) -> [flag, value]
    _ -> [raw]

dbOptionLabels :: [String]
dbOptionLabels = ["--db", "-d"]

settingsOptionLabels :: [String]
settingsOptionLabels = ["--settings", "-s"]

isSettingsSubcommand :: String -> Bool
isSettingsSubcommand raw =
  lower raw
    `elem` [ "set-view"
           , "v"
           , "view"
           , "set-browse-section"
           , "t"
           , "topic"
           , "browse-section"
           , "set-date-prefix"
           , "date"
           , "dp"
           , "date-prefix"
           , "set-collection"
           , "map"
           , "collection"
           , "clear-collection"
           , "unmap"
           , "forget-collection"
           ]

isHelpArg :: String -> Bool
isHelpArg raw =
  lower raw `elem` ["help", "h", "?", "--help", "-h", "/?"]

isUrl :: String -> Bool
isUrl raw =
  "https://" `isPrefixOf` lower raw || "http://" `isPrefixOf` lower raw

looksPositiveInt :: String -> Bool
looksPositiveInt raw =
  case reads raw :: [(Int, String)] of
    [(value, "")] -> value > 0
    _ -> False

invalidCommand :: String -> Either String a
invalidCommand commandName =
  Left ("I could not understand `" <> commandName <> "`.\n\n" <> usageText)

lower :: String -> String
lower = T.unpack . T.toLower . T.pack

parsePositiveInt :: String -> String -> Either String Int
parsePositiveInt label raw =
  case reads raw of
    [(value, "")] | value > 0 -> Right value
    _ -> Left ("Invalid " <> label <> ": " <> raw <> "\n\n" <> usageText)

parseToggle :: String -> String -> Either String Bool
parseToggle label raw =
  case T.toLower (T.pack raw) of
    "on" -> Right True
    "true" -> Right True
    "yes" -> Right True
    "1" -> Right True
    "enabled" -> Right True
    "off" -> Right False
    "false" -> Right False
    "no" -> Right False
    "0" -> Right False
    "disabled" -> Right False
    _ -> Left ("Invalid " <> label <> ": " <> raw <> "\n\n" <> usageText)

parseView :: String -> String -> Either String View
parseView label raw =
  case T.toLower (T.pack raw) of
    "browse" -> Right BrowseView
    "library" -> Right LibraryView
    "lingq" -> Right LingqView
    "zeit-login" -> Right ZeitLoginView
    "diagnostics" -> Right DiagnosticsView
    "article" -> Right ArticleView
    _ -> Left ("Invalid " <> label <> ": " <> raw <> "\n\n" <> usageText)
