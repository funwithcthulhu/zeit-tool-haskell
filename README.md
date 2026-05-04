# Zeit Tool Haskell

[![Haskell CI](https://github.com/funwithcthulhu/zeit-tool-haskell/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/funwithcthulhu/zeit-tool-haskell/actions/workflows/haskell-ci.yml)

Zeit Tool Haskell is a Windows desktop app for saving Die Zeit articles, managing a local SQLite reading library, and creating LingQ lessons from cleaned article text.

## What It Does

- Browses Zeit sections and saves articles into a local library.
- Keeps article text, metadata, ignored state, LingQ upload state, audio paths, and known-word percentages in SQLite.
- Imports or syncs LingQ known words and estimates article coverage.
- Uploads saved articles to LingQ, with optional date prefixes and per-section collection mappings.
- Uses a real Edge/Chrome session for Zeit subscriber access instead of headless login automation.
- Provides a short CLI (`zt`) for checks, scripts, and maintenance.

## Build From Source

```powershell
cd C:\projects\zeit-tool-haskell
cabal test
cabal run zt -- h
```

Launch the GUI from the project folder:

```powershell
.\run-zeit-tool.ps1
```

Run the CLI wrapper:

```powershell
.\zt h
.\zt t
.\zt b wissen -p 2
```

`.\run-zeit-tool.ps1 cli <args>` and the old `-Cli` switch remain available for existing scripts.

## Windows Installer

The installer is built with Inno Setup and installs per user under `%LOCALAPPDATA%\Zeit Tool Haskell`. The app writes `settings.json`, `zeit-tool.db`, logs, audio, and support bundles beside the installed executable, so administrator rights are not needed.

Build from existing optimized binaries:

```powershell
.\scripts\build-installer.ps1 -SkipBuild
```

Build fresh binaries and then package them:

```powershell
.\scripts\build-installer.ps1
```

The installer is written to `dist\ZeitToolHaskellSetup-<version>.exe`. If Inno Setup 6 is missing, the script still creates `dist\installer-staging` and prints the install instructions for the compiler.

## GUI Features

- Zeit browsing with section dropdown, search, hidden URL filtering, only-new filtering, paging, preview, original-link opening, selected fetch, visible fetch, retryable failures, and multi-page article fallback.
- Library management with workflow presets, duplicate-title review, light/dark theme, compact or comfortable rows, search, section and word filters, ignored/not-uploaded filters, collapsible grouping, sorting, paging, article actions, audio state, and guarded cleanup for destructive actions.
- LingQ upload with API key or password login, connected-account status, language and collection refresh, fallback collection, per-section collection mapping, date-prefixed titles, existing-lesson updates, course-status sync, selected/visible upload, and retryable failed uploads.
- Known-word sync/import/clear/recompute, cached article coverage, and library filtering by estimated coverage.
- Zeit login through cookie paste or browser-assisted Edge/Chrome session import. Browser import stores the cookie header and matching user-agent from the real browser session, then uses those values for article fetches.
- Diagnostics for live jobs, queued work, cooperative cancellation, queue pause/resume/clear, completed job history, retry lists, recent logs, data folder access, and support bundles.

## CLI

The GUI is the main workflow. Use `zt` when a terminal command is faster:

```powershell
.\zt h
.\zt t
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

Use `-d FILE` for another SQLite database and `-s FILE` for another settings file. See [COMMANDS.md](COMMANDS.md) for the full command list.

Environment variables:

- `ZEIT_COOKIE`: cookie header for authenticated Zeit fetches outside the GUI.
- `ZEIT_USER_AGENT`: browser user-agent matching `ZEIT_COOKIE`.
- `LINGQ_API_KEY`: required for LingQ upload and known-word sync.
- `LINGQ_COLLECTION_ID`: optional fallback LingQ collection.

## Repository Layout

- `src/ZeitLingq/Domain`: business types and article rules.
- `src/ZeitLingq/Text`: German tokenization and stemming.
- `src/ZeitLingq/Core`: batch fetch, upload, and known-word logic.
- `src/ZeitLingq/App`: pure model, update loop, command runtime, startup hydration, and view models.
- `src/ZeitLingq/Infrastructure`: Zeit HTTP, LingQ HTTP, SQLite, JSON settings, and audio adapters.
- `src/ZeitLingq/Ports.hs`: effect boundaries used by the app runtime and tests.
- `gui/Main.hs`: Monomer event wiring and desktop screens.
- `gui/ZeitLingq/Gui`: GUI-specific theme, browser-session parsing, and error wording.
- `app/Main.hs`: CLI entry point for both `zeit-lingq-tool` and `zt`.
- `scripts`: installer and browser-login helpers.

## Data And Reliability

- SQLite schema setup is idempotent and records a schema version in `schema_migrations`.
- The SQLite adapter enables foreign keys, a busy timeout, WAL journaling, and indexes for common browse/library queries.
- Settings are stored in JSON so UI preferences and account/session fields survive restarts.
- Batch fetch and upload failures remain retryable from the GUI.
- Support bundles collect logs and local metadata needed for troubleshooting.

## Release Status

The desktop path covers browsing, fetching, local library work, known-word workflows, LingQ upload/status sync, audio, diagnostics, browser-session Zeit login, CLI maintenance commands, and Windows installer packaging.

Known limits:

- Zeit authentication depends on a valid user-controlled browser session. On Windows the GUI can import cookies and user-agent from Edge/Chrome; on other platforms paste a Cookie header manually.
- The job queue is in memory. Queued work and completed-job history do not survive an app restart.
