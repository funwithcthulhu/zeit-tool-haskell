# Zeit Tool Haskell

This repository is the Haskell-first rewrite of the original Electron Zeit/LingQ tool.

## Goals

- Keep the current user-facing behavior: browse Die Zeit, fetch and store articles, estimate known-word coverage, and upload lessons to LingQ.
- Move the business logic into pure modules first.
- Keep the GUI swappable so we can start with a functional core and decide on the front-end adapter with less risk.

## Current Structure

- `src/ZeitLingq/Domain`: stable business types and article rules.
- `src/ZeitLingq/Text/German.hs`: German tokenization and stemming ported from the JavaScript app.
- `src/ZeitLingq/Core/KnownWords.hs`: pure known-word import and percentage estimation.
- `src/ZeitLingq/App`: GUI-agnostic application model and update loop. This is shaped to fit Monomer's Elm-style architecture later.
- `src/ZeitLingq/Infrastructure/Settings.hs`: JSON-backed user settings adapter.
- `src/ZeitLingq/Infrastructure/Sqlite.hs`: SQLite-backed article library adapter.
- `src/ZeitLingq/Ports.hs`: effect boundaries for Zeit scraping, LingQ, settings, and persistence.

## Build

```powershell
cd C:\projects\zeit-tool-haskell
cabal test
cabal run
```

## GUI Direction

The current recommendation is:

1. Keep the core library pure and adapter-free.
2. Add a `Monomer` executable once we are happy with the state model.
3. Treat scraping, SQLite, logging, and LingQ as infrastructure ports that the GUI drives.

That gives us a Haskell-native application without forcing the whole codebase to depend on the first GUI choice we try.

## Current Migration Slice

- Pure article, known-word, and app-update logic is in place.
- SQLite article persistence is available through `LibraryPort`.
- JSON user settings are available through `SettingsPort`.
- Next infrastructure targets are the LingQ client and Zeit article fetching adapter.
