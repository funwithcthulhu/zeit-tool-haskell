# Command Reference

Zeit Tool Haskell is primarily a desktop app. The CLI is for quick checks, scripted fetches, known-word maintenance, and library cleanup.

From source, prefix commands with:

```powershell
cabal run zeit-lingq-tool --
```

From an installed build, run:

```powershell
zeit-lingq-tool.exe
```

On Windows from the project folder, the launcher also accepts a friendly `cli` command:

```powershell
.\run-zeit-tool.ps1 cli help
```

The older hyphenated commands still work for existing scripts, but new docs use the grouped commands below.

## Help

```powershell
zeit-lingq-tool help
zeit-lingq-tool --help
```

Running `zeit-lingq-tool` with no arguments shows the same help text.

## Common Flags

```powershell
--db FILE
--settings FILE
--page N
--min N
--max N
--to DIR
```

`--db FILE` changes the SQLite library path. The default is `zeit-tool.db`.

`--settings FILE` changes the JSON settings path. The default is `settings.json`.

`--page N` chooses the Zeit browse page.

`--min N` and `--max N` filter articles by word count when fetching a list of URLs.

`--to DIR` chooses the audio download folder.

Flags may also use equals syntax:

```powershell
zeit-lingq-tool browse wissen --page=2 --db=custom.db
```

## Browse And Fetch

List available Zeit topics:

```powershell
zeit-lingq-tool topics
```

Browse the front page or a topic:

```powershell
zeit-lingq-tool browse
zeit-lingq-tool browse wissen
zeit-lingq-tool browse wissen --page 2
```

Read and save a single article:

```powershell
zeit-lingq-tool read https://www.zeit.de/wissen/2026-05/example
```

Fetch a text file of article URLs:

```powershell
zeit-lingq-tool fetch-list urls.txt
zeit-lingq-tool fetch-list urls.txt --min 500 --max 2000
```

If an article needs an authenticated Zeit session, set `ZEIT_COOKIE`. When the cookie came from a real browser, also set `ZEIT_USER_AGENT` to the same browser user-agent.

## Library

Show saved articles:

```powershell
zeit-lingq-tool library
```

Show library stats:

```powershell
zeit-lingq-tool stats
```

Hide or restore saved articles:

```powershell
zeit-lingq-tool hide article 42
zeit-lingq-tool show article 42
```

Hide or restore browse URLs before they are fetched:

```powershell
zeit-lingq-tool hide url https://www.zeit.de/wissen/2026-05/example
zeit-lingq-tool show url https://www.zeit.de/wissen/2026-05/example
```

List hidden browse URLs:

```powershell
zeit-lingq-tool hidden
```

Delete one article:

```powershell
zeit-lingq-tool delete article 42
```

Delete old articles:

```powershell
zeit-lingq-tool delete old 30
zeit-lingq-tool delete old 30 --uploaded
zeit-lingq-tool delete old 30 --unuploaded
```

Delete ignored articles:

```powershell
zeit-lingq-tool delete ignored
```

## Known Words

Sync known German words from LingQ:

```powershell
zeit-lingq-tool known sync
```

Import known words from a text file:

```powershell
zeit-lingq-tool known import known-words.txt
```

Recompute known-word percentages for saved articles:

```powershell
zeit-lingq-tool known recompute
```

Show known-word status:

```powershell
zeit-lingq-tool known info
```

`known sync` requires `LINGQ_API_KEY`.

## LingQ

Upload one saved article:

```powershell
zeit-lingq-tool lingq upload 42
```

Use a different database or settings file:

```powershell
zeit-lingq-tool lingq upload 42 --db custom.db --settings settings.dev.json
```

`lingq upload` requires `LINGQ_API_KEY`. You can set `LINGQ_COLLECTION_ID` as a fallback course.

## Audio

Download article audio if the saved article has an audio source:

```powershell
zeit-lingq-tool audio download 42
zeit-lingq-tool audio download 42 --to audio-cache
```

## Settings

Show settings:

```powershell
zeit-lingq-tool settings
```

Set the startup view:

```powershell
zeit-lingq-tool settings view library
```

Set the default browse topic:

```powershell
zeit-lingq-tool settings topic wissen
```

Turn date prefixes on or off for LingQ lesson titles:

```powershell
zeit-lingq-tool settings date-prefix on
zeit-lingq-tool settings date-prefix off
```

Map a Zeit section to a LingQ collection:

```powershell
zeit-lingq-tool settings collection Wissen 12345
```

Clear a section mapping:

```powershell
zeit-lingq-tool settings forget-collection Wissen
```

Use a separate settings file:

```powershell
zeit-lingq-tool settings --settings settings.dev.json
zeit-lingq-tool settings topic wissen --settings settings.dev.json
```

## Legacy Command Mapping

The old commands remain available:

```powershell
sections
browse <section-id> [page] [db-path]
fetch <article-url> [db-path]
batch-fetch <url-list.txt> [db-path] [min-words max-words]
library [db-path]
stats [db-path]
delete-article <article-id> [db-path]
delete-older-than <days> [db-path]
delete-older-than-uploaded <days> [db-path]
delete-older-than-unuploaded <days> [db-path]
delete-ignored [db-path]
ignore-article <article-id> [db-path]
unignore-article <article-id> [db-path]
known-sync [db-path]
known-import <word-list.txt> [db-path]
known-info [db-path]
known-compute [db-path]
lingq-upload <article-id> [db-path] [settings-path]
audio-download <article-id> [audio-dir] [db-path]
ignore-url <url> [db-path]
unignore-url <url> [db-path]
ignored [db-path]
settings [settings-path]
settings set-view <view> [settings-path]
settings set-browse-section <section-id> [settings-path]
settings set-date-prefix <on|off> [settings-path]
settings set-collection <section-name> <collection-id> [settings-path]
settings clear-collection <section-name> [settings-path]
```
