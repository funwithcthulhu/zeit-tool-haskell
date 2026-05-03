# Zeit Tool Haskell

This repository is the Haskell-first rewrite of the original Electron Zeit/LingQ tool.

## Goals

- Keep the current user-facing behavior: browse Die Zeit, fetch and store articles, estimate known-word coverage, and upload lessons to LingQ.
- Move the business logic into pure modules first.
- Keep the GUI swappable so we can start with a functional core and decide on the front-end adapter with less risk.

## Current Structure

- `src/ZeitLingq/Domain`: stable business types and article rules.
- `src/ZeitLingq/Core/Batch.hs`: batch article fetch/save workflow.
- `src/ZeitLingq/Text/German.hs`: German tokenization and stemming ported from the JavaScript app.
- `src/ZeitLingq/Core/KnownWords.hs`: pure known-word import and percentage estimation.
- `src/ZeitLingq/App`: GUI-agnostic application model and update loop. This is shaped to fit Monomer's Elm-style architecture later.
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

## CLI Harness

```powershell
cabal run zeit-lingq-tool -- sections
cabal run zeit-lingq-tool -- browse wissen 1
cabal run zeit-lingq-tool -- fetch https://www.zeit.de/wissen/2026-05/example
cabal run zeit-lingq-tool -- library
```

Set `ZEIT_COOKIE` before running `fetch` if an article needs an authenticated Zeit session.

## GUI Direction

The current recommendation is:

1. Keep the core library pure and adapter-free.
2. Add a `Monomer` executable once we are happy with the state model.
3. Treat scraping, SQLite, logging, and LingQ as infrastructure ports that the GUI drives.

That gives us a Haskell-native application without forcing the whole codebase to depend on the first GUI choice we try.

## Current Migration Slice

- Pure article, known-word, and app-update logic is in place.
- Batch fetch/save behavior is available as a pure use case over effectful callbacks.
- SQLite article persistence is available through `LibraryPort`.
- SQLite known-word storage and article `known_pct` recomputation are available.
- JSON user settings are available through `SettingsPort`.
- LingQ login, collection fetch, lesson upload, and known-word fetch helpers are scaffolded in Haskell.
- Zeit article-list and article-content extraction is scaffolded in Haskell.
- The executable now includes a small CLI harness for sections, browse, fetch, and library commands.
- Next target is the first GUI shell.
