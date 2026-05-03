# Zeit Tool Haskell

[![Haskell CI](https://github.com/funwithcthulhu/zeit-tool-haskell/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/funwithcthulhu/zeit-tool-haskell/actions/workflows/haskell-ci.yml)

Zeit Tool Haskell is a Haskell-native desktop workflow for reading Die Zeit articles and turning them into LingQ lessons.

## Goals

- Browse Die Zeit sections, fetch and store articles, estimate known-word coverage, and upload lessons to LingQ.
- Keep business logic in pure modules with explicit effect boundaries.
- Keep the GUI swappable so the core stays independent from the front-end adapter.

## Current Structure

- `src/ZeitLingq/Domain`: stable business types and article rules.
- `src/ZeitLingq/Core/Batch.hs`: batch article fetch/save workflow.
- `src/ZeitLingq/Core/Upload.hs`: batch LingQ upload workflow.
- `src/ZeitLingq/Text/German.hs`: German tokenization and stemming.
- `src/ZeitLingq/Core/KnownWords.hs`: pure known-word import and percentage estimation.
- `src/ZeitLingq/App`: GUI-agnostic application model, update loop, and command runtime.
- `src/ZeitLingq/App/Driver.hs`: event dispatch helper that runs commands and folds follow-up events back into the model.
- `src/ZeitLingq/App/Startup.hs`: settings-to-model startup hydration for GUI startup.
- `src/ZeitLingq/App/ViewModel.hs`: pure GUI-facing presentation state for navigation, badges, filters, and article rows.
- `src/ZeitLingq/Infrastructure/Audio.hs`: article audio filename and download helpers.
- `src/ZeitLingq/Infrastructure/Lingq.hs`: LingQ HTTP adapter and response parsing helpers.
- `src/ZeitLingq/Infrastructure/Settings.hs`: JSON-backed user settings adapter.
- `src/ZeitLingq/Infrastructure/Sqlite.hs`: SQLite-backed article library adapter.
- `src/ZeitLingq/Infrastructure/Zeit.hs`: Zeit HTTP adapter and HTML extraction helpers.
- `src/ZeitLingq/Ports.hs`: effect boundaries for Zeit scraping, LingQ, settings, and persistence.

## Build

```powershell
cd C:\projects\zeit-tool-haskell
cabal test
cabal run zeit-lingq-tool -- help
```

On Windows, `run-zeit-tool.ps1` launches the Monomer GUI from the project directory:

```powershell
.\run-zeit-tool.ps1
```

Use `cli` when you want the terminal harness instead:

```powershell
.\run-zeit-tool.ps1 cli topics
```

The old `-Cli` switch still works for existing scripts.

The desktop shortcut uses `launch-zeit-tool-gui.vbs` so the GUI opens without leaving a PowerShell window on screen. The launcher prepares the MSYS2 UCRT `pkg-config` paths needed by Monomer, GLEW, FreeType, and SDL2.

## Windows Installer

The repository includes an Inno Setup packaging script for a per-user Windows install. It installs the GUI, CLI, runtime DLLs, browser-login helper, and docs under `%LOCALAPPDATA%\Zeit Tool Haskell`, so the app can write `settings.json`, `zeit-tool.db`, logs, audio, and support bundles without administrator rights.

To rebuild the installer from existing optimized GUI/CLI binaries:

```powershell
.\scripts\build-installer.ps1 -SkipBuild
```

For a fresh optimized release build plus installer:

```powershell
.\scripts\build-installer.ps1
```

The script writes `dist\ZeitToolHaskellSetup-<version>.exe` and prints its SHA256 hash. If Inno Setup 6 is not installed, the script still creates a portable staging folder at `dist\installer-staging` and tells you how to install the compiler.

## CLI

The CLI is meant for quick checks, automation, and library maintenance. Commands now use plain grouped verbs and named flags; the older hyphenated commands still work.

If you are developing from source, prefix commands with `cabal run zeit-lingq-tool --`. If you installed the app, run `zeit-lingq-tool.exe` from the install folder or Start Menu shortcut.

Common examples:

```powershell
cabal run zeit-lingq-tool -- help
cabal run zeit-lingq-tool -- topics
cabal run zeit-lingq-tool -- browse wissen --page 2
cabal run zeit-lingq-tool -- read https://www.zeit.de/wissen/2026-05/example
cabal run zeit-lingq-tool -- fetch-list urls.txt --min 500 --max 2000
cabal run zeit-lingq-tool -- library
cabal run zeit-lingq-tool -- hide article 1
cabal run zeit-lingq-tool -- show article 1
cabal run zeit-lingq-tool -- delete old 30 --uploaded
cabal run zeit-lingq-tool -- known sync
cabal run zeit-lingq-tool -- known import known-words.txt
cabal run zeit-lingq-tool -- lingq upload 1
cabal run zeit-lingq-tool -- audio download 1 --to audio
cabal run zeit-lingq-tool -- settings topic wissen
cabal run zeit-lingq-tool -- settings date-prefix off
cabal run zeit-lingq-tool -- settings collection Wissen 12345
```

Use `--db FILE` for a different SQLite library and `--settings FILE` for a different settings file. Full command reference: [COMMANDS.md](COMMANDS.md).

Set `ZEIT_COOKIE` before running `read` if an article needs an authenticated Zeit session. If you are scripting authenticated fetches outside the GUI, you can also set `ZEIT_USER_AGENT` to the browser user-agent that produced the cookie.
Set `LINGQ_API_KEY` before running `lingq upload` or `known sync`; optionally set `LINGQ_COLLECTION_ID` as an upload fallback. Section-specific LingQ collection mappings and the date-prefix toggle are read from `settings.json`.

## GUI

The Monomer GUI is the primary desktop entry point. It stays thin: user actions become pure app events, the command runtime talks to ports, and the GUI renders the updated model. This keeps scraping, SQLite, settings, audio, and LingQ code reusable outside the desktop adapter.

The GUI currently supports:

- Browsing Zeit sections with topic dropdown, search, hidden-url filtering, only-new filtering, paging, preview, original-link opening, single fetch, selected fetch, visible fetch, retryable failed fetches, and multi-page article fallback when a complete view is unavailable.
- Managing the local library with presets for common reading/upload workflows and duplicate review, compact or comfortable row density, persisted light/dark theme selection, search, section and word filters, ignored/not-uploaded filters, collapsible grouping, sorting, paging, article open/copy/original/audio actions with article metadata and audio availability, and configurable cleanup of ignored, old, uploaded, or unuploaded articles with two-click confirmation for destructive actions.
- Uploading saved articles to LingQ with API key or password login, a compact connected-account state, language selection, collection refresh, fallback collection selection, per-section collection mapping, date-prefixed lesson titles, existing-lesson updates, upload-status sync from an existing LingQ course, known-word sync/import/clear/recompute, selected/visible upload, and retryable failed uploads.
- Zeit authentication through manual cookie paste or browser-assisted Edge/Chrome session import. The browser import opens a real installed browser for interactive login, stores the matching browser user-agent with the cookies, and reuses browser-like request headers for article fetches. The GUI warns when the session is missing or likely expired, so subscriber/paywalled fetch failures point back to browser-session import instead of failing silently. This avoids brittle headless login automation while keeping requests aligned with the browser session you created. The GUI also opens the project data folder, the GUI log file, timestamped support bundles, and the Zeit login page.
- Diagnostics for live batch jobs, queued fetch/upload work, cooperative cancellation, queue pause/resume/clear controls, completed job history, retry lists, support bundles, and copying recent GUI log lines.

## Current Status

- The Haskell core covers Zeit browsing/fetching, SQLite persistence, library maintenance, known-word import/sync/estimation, article audio download/open, LingQ upload, and LingQ course status reconciliation.
- The Monomer GUI is the feature-complete desktop path for daily use and is launched by `run-zeit-tool.ps1` or the Windows desktop shortcut.
- The Windows installer packages the GUI, CLI, runtime DLLs, docs, and browser-session helper into a per-user install under LocalAppData.
- Batch fetch and batch upload show live progress, queue overlapping work instead of rejecting it, and keep per-item failures retryable from the sidebar and Diagnostics view.
- JSON settings persist the current view, row density, UI theme, Zeit cookie, Zeit browser user-agent, LingQ API key, LingQ language, browse filters, LingQ filters, date-prefix preference, fallback collection, and section collection mappings.
- The CLI remains available for quick verification and scripting, with friendly grouped commands and named flags.

Known limits:

- Zeit authentication is cookie-session based. On Windows, the GUI can launch a real Edge/Chrome window for interactive login and import the resulting cookies plus browser user-agent; this is intentionally closer to a normal user-controlled browser session than an embedded/headless login flow. On other platforms, paste a Cookie header manually.
- Long-running batch operations now have an in-memory queue, cooperative cancellation, and completed-job history. The queue is not yet persisted across app restarts.
