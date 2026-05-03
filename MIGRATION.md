# Migration Notes

## Why a parallel Haskell track

The existing Electron app is still the best executable specification of behavior. Rewriting directly over it would make it too easy to lose working features while the new code is still forming. This repository is the new Haskell home; the legacy app is used as a reference until we reach feature parity.

## Functional architecture

The rewrite uses four layers:

1. `Domain`
   - Plain immutable data structures.
   - Pure rules for article formatting, word-count filtering, and LingQ lesson naming.
2. `Text`
   - Language-specific normalization and stemming.
3. `App`
   - A pure model and event/update loop, intentionally compatible with Elm-style GUI libraries such as Monomer.
4. `Ports`
   - Explicit effect boundaries for scraping, persistence, settings, and LingQ.

## Legacy to Haskell mapping

- `electron/services/zeit-scraper.js`
  - Partially ported into `ZeitLingq.Infrastructure.Zeit`.
- `electron/services/lingq-api.js`
  - Partially ported into `ZeitLingq.Infrastructure.Lingq`.
- `electron/services/database.js`
  - Partially ported into `ZeitLingq.Infrastructure.Sqlite` behind `LibraryPort`.
- `electron/services/stemmer.js`
  - Ported into `ZeitLingq.Text.German`.
- `src/App.jsx` and component state
  - Reframed as `ZeitLingq.App.Model` and `ZeitLingq.App.Update`.
- Electron `settings.json`
  - Ported into `ZeitLingq.Infrastructure.Settings` behind `SettingsPort`.

## GUI recommendation

The most sensible first GUI remains Monomer because the app state already fits a pure model/update flow. We should still keep Monomer as an adapter, not as the center of the architecture, so replacing it later would not touch scraping, persistence, or domain rules.

## CLI harness

The executable now provides a small terminal harness around the adapters. It is useful for verifying scraper, persistence, and library behavior before the GUI is ready.
