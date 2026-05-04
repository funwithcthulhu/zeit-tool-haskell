# Zeit Tool Haskell

[![Haskell CI](https://github.com/funwithcthulhu/zeit-tool-haskell/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/funwithcthulhu/zeit-tool-haskell/actions/workflows/haskell-ci.yml)

Personal Haskell/Monomer desktop tool for saving Die Zeit articles locally and preparing LingQ imports.

This is personal tooling, not a general-purpose app. It assumes my Windows setup: local SQLite storage, browser-based Zeit login, and a LingQ import path for saved articles. Expect rough edges.

## Current Scope

- Zeit section browsing and article save/fetch.
- Local SQLite storage for article text, metadata, ignored state, LingQ upload state, audio paths, and known-word percentages.
- LingQ known-word import/sync and rough article coverage estimates.
- LingQ lesson creation from saved articles, with optional date prefixes and per-section collection mappings.
- Edge/Chrome session import for Zeit subscriber access.
- A short CLI (`zt`) for maintenance tasks and quick checks.

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

The installer is mainly for this Windows setup. It installs per user under `%LOCALAPPDATA%\Zeit Tool Haskell`, where the app can write `settings.json`, `zeit-tool.db`, logs, audio, and troubleshooting bundles without administrator rights.

Build from existing optimized binaries:

```powershell
.\scripts\build-installer.ps1 -SkipBuild
```

Build fresh binaries and then package them:

```powershell
.\scripts\build-installer.ps1
```

The installer is written to `dist\ZeitToolHaskellSetup-<version>.exe`. If Inno Setup 6 is missing, the script still creates `dist\installer-staging` and prints the install instructions for the compiler.

## GUI

- Browse Zeit sections, preview/open articles, fetch individual articles, and batch-fetch selected or visible rows.
- Work through a local reading library with search, filters, grouping, sorting, audio/original-link actions, duplicate review, and guarded cleanup.
- Send saved articles to LingQ, refresh languages/collections, map Zeit sections to LingQ collections, and update existing lessons when found.
- Import or sync known words and keep cached article estimates.
- Manage Zeit session details through cookie paste or browser-assisted Edge/Chrome session import.
- Check queued work, retry failures, copy logs, and create a troubleshooting bundle from the Diagnostics view.

## CLI

Use the GUI for day-to-day use. Use `zt` when a terminal command is quicker:

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

## Repository Map

- `src/ZeitLingq/Domain`: domain types and article rules.
- `src/ZeitLingq/Text`: German tokenization and stemming.
- `src/ZeitLingq/Core`: batch fetch, upload, and known-word logic.
- `src/ZeitLingq/App`: model, update loop, command runtime, startup hydration, and view models.
- `src/ZeitLingq/Infrastructure`: Zeit HTTP, LingQ HTTP, SQLite, JSON settings, and audio adapters.
- `src/ZeitLingq/Ports.hs`: effect boundaries used by the app runtime and tests.
- `gui/Main.hs`: Monomer event wiring and desktop screens.
- `gui/ZeitLingq/Gui`: GUI-specific theme, browser-session parsing, and error wording.
- `app/Main.hs`: CLI entry point for both `zeit-lingq-tool` and `zt`.
- `scripts`: installer and browser-login helpers.

## Storage Notes

- SQLite schema setup is idempotent and records a schema version in `schema_migrations`.
- The SQLite adapter enables foreign keys, a busy timeout, WAL journaling, and indexes for common browse/library queries.
- Settings are stored in JSON so UI preferences and account/session fields survive restarts.
- Batch fetch and upload failures remain retryable from the GUI.
- Troubleshooting bundles collect logs and local metadata.

## Rough Edges

- Zeit authentication depends on a valid user-controlled browser session. On Windows the GUI can import cookies and user-agent from Edge/Chrome; on other platforms paste a Cookie header manually.
- The job queue is in memory. Queued work and completed-job history do not survive an app restart.
- The installer is not code-signed yet, so Windows SmartScreen may show an unknown-publisher warning.
- The GUI is aimed at this reading/import setup; it is not a general Zeit or LingQ client.
