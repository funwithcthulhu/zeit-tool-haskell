# Zeit Tool Haskell

[![Haskell CI](https://github.com/funwithcthulhu/zeit-tool-haskell/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/funwithcthulhu/zeit-tool-haskell/actions/workflows/haskell-ci.yml)

Personal Haskell/Monomer desktop app for saving Die Zeit articles and turning some of them into LingQ lessons.

This is not a general Zeit or LingQ client. It assumes a Windows desktop, a local SQLite library, browser-based Zeit login, and a LingQ workflow for German articles.

## What It Does

- Browses Die Zeit sections and saves article text locally.
- Stores article metadata, ignored state, upload state, audio paths, and known-word percentages in SQLite.
- Imports or syncs LingQ known words and estimates rough coverage for saved articles.
- Creates or updates LingQ lessons from saved articles.
- Maps Zeit section names to LingQ collection ids for lesson grouping.
- Imports a Zeit browser session from Edge/Chrome on Windows, or accepts a pasted Cookie header.

## Build And Run

From the repo root:

```powershell
cd C:\projects\zeit-tool-haskell
cabal test
cabal run zt -- h
```

Launch the GUI:

```powershell
.\run-zeit-tool.ps1
```

Run the CLI wrapper:

```powershell
.\zt h
.\zt t
.\zt b wissen -p 2
```

`.\run-zeit-tool.ps1 cli <args>` and the older `-Cli` switch still work for existing scripts.

## GUI

The GUI is for day-to-day use.

- Browse Zeit sections, preview articles, and fetch selected or visible rows.
- Search and filter the saved library, including ignored articles and duplicate-title review.
- Upload selected saved articles to LingQ.
- Refresh LingQ languages, collections, known words, and upload status.
- Paste a Zeit Cookie header or import one from Edge/Chrome.
- Retry failed fetch/upload batches and create a local support bundle.

## CLI

The `zt` executable is for quick checks and maintenance:

```powershell
.\zt h
.\zt r https://www.zeit.de/wissen/2026-05/example
.\zt f urls.txt -m 500 -x 2000
.\zt l
.\zt hide 1
.\zt rm old 30 -u
.\zt k sync
.\zt u 1
.\zt a 1 -o audio
.\zt cfg topic wissen
.\zt cfg map Wissen 12345
```

Use `-d FILE` for another SQLite database and `-s FILE` for another settings file. See [COMMANDS.md](COMMANDS.md) for the command list.

Environment variables:

- `ZEIT_COOKIE`: Cookie header for authenticated Zeit fetches outside the GUI.
- `ZEIT_USER_AGENT`: browser user-agent matching `ZEIT_COOKIE`.
- `LINGQ_API_KEY`: required for LingQ upload and known-word sync.
- `LINGQ_COLLECTION_ID`: optional fallback LingQ collection id.

## Installer

The installer is for this Windows setup. It installs per user under `%LOCALAPPDATA%\Zeit Tool Haskell`, so the app can write `settings.json`, `zeit-tool.db`, logs, audio, and support bundles without administrator rights.

```powershell
.\scripts\build-installer.ps1
```

Use `.\scripts\build-installer.ps1 -SkipBuild` when the release binaries already exist. The installer is written to `dist\ZeitToolHaskellSetup-<version>.exe`.

## Code Map

- `src/ZeitLingq/Domain`: article, section, library, LingQ, job, and settings types.
- `src/ZeitLingq/Text`: German tokenization and stemming.
- `src/ZeitLingq/Core`: fetch filtering, upload decisions, and known-word scoring.
- `src/ZeitLingq/App`: model, update loop, runtime commands, startup loading, upload preferences, and view-model projection.
- `src/ZeitLingq/Infrastructure`: Zeit HTTP, LingQ HTTP, SQLite, JSON settings, and audio adapters.
- `src/ZeitLingq/Ports.hs`: effect records used by the runtime and tests.
- `gui/Main.hs`: Monomer screens, GUI events, queued batch producers, and desktop actions.
- `gui/ZeitLingq/Gui`: GUI theme, browser-session parsing, and auth error wording.
- `app/Main.hs`: CLI entry point for both `zeit-lingq-tool` and `zt`.
- `scripts`: installer and browser-login helpers.

## Rough Edges

- Zeit authentication depends on a browser session I control. If the session expires, fetches fail until I import or paste fresh cookies.
- Browser-assisted Zeit login is Windows Edge/Chrome only.
- The job queue is in memory. Queued work and completed-job history do not survive restart.
- The installer is unsigned, so Windows SmartScreen may warn about the publisher.
- The GUI is shaped around my reading/import workflow, not every Zeit or LingQ feature.
