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
- `src/ZeitLingq/App`: GUI-agnostic application model, update loop, and command runtime. This is shaped to fit Monomer's Elm-style architecture later.
- `src/ZeitLingq/App/Startup.hs`: settings-to-model startup hydration for a future GUI adapter.
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

The GUI shell is optional so native Monomer dependencies do not affect the default CLI and CI path:

```powershell
cabal run -fgui zeit-lingq-tool-gui
```

Monomer requires native graphics dependencies. On Windows, make sure GLEW is visible through `pkg-config`; otherwise Cabal will report that `glew-any` is missing.

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

## GUI Direction

The current recommendation is:

1. Keep the core library pure and adapter-free.
2. Add a `Monomer` executable once we are happy with the state model.
3. Treat scraping, SQLite, logging, and LingQ as infrastructure ports that the GUI drives.

That gives us a Haskell-native application without forcing the whole codebase to depend on the first GUI choice we try.

## Current Status

- Pure article, known-word, and app-update logic is in place.
- The app model stores browse, library, and LingQ article rows for GUI rendering.
- Batch fetch/save behavior is available as a pure use case over effectful callbacks.
- App refresh commands can load browse, library, and LingQ rows through ports and emit pure loaded events.
- Batch LingQ upload behavior is available as a pure use case over effectful callbacks.
- LingQ upload now derives date-prefix and section collection behavior from persisted settings.
- SQLite article persistence is available through `LibraryPort`.
- SQLite stats, delete, and saved-article ignore/unignore controls are exposed through the CLI.
- SQLite ignored browse URLs are available for pre-fetch hiding.
- Browse output hides URLs recorded in the SQLite ignored URL list.
- SQLite known-word storage and article `known_pct` recomputation are available.
- LingQ known-word sync is available from the CLI and recomputes cached coverage.
- Article audio metadata persistence and download helpers are available.
- JSON user settings are available through `SettingsPort`.
- JSON user settings are exposed through the CLI for GUI-ready configuration.
- App startup can hydrate the pure model from `SettingsPort`, ready for a Monomer shell.
- Pure view-model projection is available for a Monomer shell, including current-screen article rows.
- An optional Monomer executable shell is available behind the `gui` Cabal flag.
- LingQ login, collection fetch, lesson upload, and known-word fetch helpers are scaffolded in Haskell.
- Zeit article-list and article-content extraction is scaffolded in Haskell.
- The executable includes a CLI harness for sections, browse, fetch, library maintenance, settings, known words, audio, and LingQ uploads.
- Next target is the first GUI shell.
