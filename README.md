# Zeit Tool Haskell

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
cabal run
```

On Windows, `run-zeit-tool.ps1` launches the Monomer GUI from the project directory:

```powershell
.\run-zeit-tool.ps1
```

Use `-Cli` when you want the terminal harness instead:

```powershell
.\run-zeit-tool.ps1 -Cli sections
```

The desktop shortcut uses `launch-zeit-tool-gui.vbs` so the GUI opens without leaving a PowerShell window on screen. The launcher prepares the MSYS2 UCRT `pkg-config` paths needed by Monomer, GLEW, FreeType, and SDL2.

## CLI Harness

```powershell
cabal run zeit-lingq-tool -- sections
cabal run zeit-lingq-tool -- browse wissen 1 zeit-tool.db
cabal run zeit-lingq-tool -- fetch https://www.zeit.de/wissen/2026-05/example
cabal run zeit-lingq-tool -- batch-fetch urls.txt zeit-tool.db 500 2000
cabal run zeit-lingq-tool -- library
cabal run zeit-lingq-tool -- stats
cabal run zeit-lingq-tool -- ignore-article 1
cabal run zeit-lingq-tool -- unignore-article 1
cabal run zeit-lingq-tool -- delete-article 1
cabal run zeit-lingq-tool -- known-sync
cabal run zeit-lingq-tool -- known-import known-words.txt
cabal run zeit-lingq-tool -- known-compute
cabal run zeit-lingq-tool -- known-info
cabal run zeit-lingq-tool -- lingq-upload 1 zeit-tool.db settings.json
cabal run zeit-lingq-tool -- audio-download 1 audio
cabal run zeit-lingq-tool -- ignore-url https://www.zeit.de/wissen/2026-05/example
cabal run zeit-lingq-tool -- ignored
cabal run zeit-lingq-tool -- settings
cabal run zeit-lingq-tool -- settings set-browse-section wissen
cabal run zeit-lingq-tool -- settings set-date-prefix off
cabal run zeit-lingq-tool -- settings set-collection Wissen 12345
```

Set `ZEIT_COOKIE` before running `fetch` if an article needs an authenticated Zeit session.
Set `LINGQ_API_KEY` before running `lingq-upload` or `known-sync`; optionally set `LINGQ_COLLECTION_ID` as an upload fallback. Section-specific LingQ collection mappings and the date-prefix toggle are read from `settings.json`.

## GUI

The Monomer GUI is the primary desktop entry point. It stays thin: user actions become pure app events, the command runtime talks to ports, and the GUI renders the updated model. This keeps scraping, SQLite, settings, audio, and LingQ code reusable outside the desktop adapter.

The GUI currently supports:

- Browsing Zeit sections with topic dropdown, search, hidden-url filtering, only-new filtering, paging, preview, original-link opening, single fetch, selected fetch, visible fetch, and retryable failed fetches.
- Managing the local library with presets for common reading/upload workflows and duplicate review, compact or comfortable row density, search, section and word filters, ignored/not-uploaded filters, grouping, sorting, paging, article open/copy/original/audio actions, and configurable cleanup of ignored, old, uploaded, or unuploaded articles.
- Uploading saved articles to LingQ with API key or password login, language selection, collection refresh, fallback collection selection, per-section collection mapping, date-prefixed lesson titles, existing-lesson updates, upload-status sync from an existing LingQ course, known-word sync/import/clear/recompute, selected/visible upload, and retryable failed uploads.
- Opening the project data folder, the GUI log file, timestamped support bundles, and the Zeit login page from the GUI.

## Current Status

- The Haskell core covers Zeit browsing/fetching, SQLite persistence, library maintenance, known-word import/sync/estimation, article audio download/open, LingQ upload, and LingQ course status reconciliation.
- The Monomer GUI is functional and launched by `run-zeit-tool.ps1` or the Windows desktop shortcut.
- Batch fetch and batch upload show live progress and keep per-item failures retryable from the sidebar.
- JSON settings persist the current view, row density, Zeit cookie, LingQ API key, LingQ language, browse filters, LingQ filters, date-prefix preference, fallback collection, and section collection mappings.
- The CLI harness remains available for quick verification and scripting.

Known limits:

- Zeit authentication is cookie-session based. The GUI can open the Zeit login page, but it does not embed a browser or automatically capture browser cookies.
- Long-running operations show pending status, live batch progress, and final retry lists, but they do not yet expose a cancellable job queue.
