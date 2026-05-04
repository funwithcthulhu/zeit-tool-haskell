# Command Reference

Zeit Tool Haskell is primarily a personal desktop utility. The CLI is for quick checks, scripts, and maintenance.

From the project folder on Windows:

```powershell
.\zt h
```

From an installed build, use:

```powershell
zt.exe h
```

The same parser is also available through Cabal:

```powershell
cabal run zt -- h
cabal run zeit-lingq-tool -- h
```

## Core Commands

```powershell
zt h                         # help
zt t                         # topics
zt b                         # browse front page
zt b wissen -p 2             # browse a topic/page
zt r <url>                   # read and save one article
zt <url>                     # same as zt r <url>
zt f urls.txt -m 500 -x 2000 # fetch a URL list
zt l                         # library
zt s                         # stats
```

## Cleanup

```powershell
zt hide 42
zt show 42
zt hide <url>
zt show <url>
zt hidden
zt rm 42
zt rm old 30
zt rm old 30 -u              # only uploaded
zt rm old 30 -n              # only unuploaded
zt rm ignored
```

## Known Words

```powershell
zt k                         # known-word status
zt k sync
zt k import known-words.txt
zt k re                      # recompute percentages
```

`zt k sync` requires `LINGQ_API_KEY`.

## LingQ And Audio

```powershell
zt u 42
zt a 42
zt a 42 -o audio-cache
```

`zt u` requires `LINGQ_API_KEY`. You can set `LINGQ_COLLECTION_ID` as a fallback course.

## Settings

```powershell
zt cfg
zt cfg view library
zt cfg topic wissen
zt cfg date on
zt cfg date off
zt cfg map Wissen 12345
zt cfg unmap Wissen
```

## Flags

```powershell
-d, --db FILE
-s, --settings FILE
-p, --page N
-m, --min N
-x, --max N
-o, --to DIR
-u, --uploaded
-n, --unuploaded
```

Examples:

```powershell
zt b wissen -p 3 -d custom.db
zt f urls.txt -m 500 -x 2000 -d custom.db
zt u 42 -d custom.db -s settings.dev.json
zt cfg topic wissen -s settings.dev.json
```

## Auth Environment

Set `ZEIT_COOKIE` before `zt r` or `zt f` if an article needs an authenticated Zeit session. If the cookie came from a real browser, also set `ZEIT_USER_AGENT` to that browser's user-agent.

Set `LINGQ_API_KEY` before `zt u` or `zt k sync`. Optionally set `LINGQ_COLLECTION_ID` as an upload fallback.

## Compatibility

The longer commands still work for old scripts, including:

```powershell
zeit-lingq-tool browse wissen --page 2
zeit-lingq-tool read <url>
zeit-lingq-tool fetch-list urls.txt --min 500 --max 2000
zeit-lingq-tool known sync
zeit-lingq-tool lingq upload 42
zeit-lingq-tool audio download 42 --to audio-cache
zeit-lingq-tool settings topic wissen
```
