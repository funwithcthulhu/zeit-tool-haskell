# Architecture Notes

## Functional architecture

The project uses four layers:

1. `Domain`
   - Plain immutable data structures.
   - Pure rules for article formatting, word-count filtering, and LingQ lesson naming.
2. `Text`
   - Language-specific normalization and stemming.
3. `App`
   - A pure model and event/update loop, intentionally compatible with Elm-style GUI libraries such as Monomer.
4. `Ports`
   - Explicit effect boundaries for scraping, persistence, settings, and LingQ.

## Core modules

- `ZeitLingq.Infrastructure.Zeit` handles Zeit HTTP requests and HTML extraction.
- `ZeitLingq.Core.Batch` handles batch article fetch/save workflows over effect callbacks.
- `ZeitLingq.Core.Upload` handles LingQ upload workflows over effect callbacks.
- `ZeitLingq.Infrastructure.Lingq` handles LingQ authentication, collections, known words, and lesson uploads.
- `ZeitLingq.Infrastructure.Sqlite` provides article library persistence behind `LibraryPort`.
- `ZeitLingq.Core.KnownWords` handles known-word import and percentage computation.
- `ZeitLingq.Infrastructure.Audio` handles article audio filenames and downloads.
- `ZeitLingq.Text.German` handles German tokenization and stemming.
- `ZeitLingq.App.Model` and `ZeitLingq.App.Update` define GUI-ready state and events.
- `ZeitLingq.App.ViewModel` projects pure app state into renderer-friendly labels, badges, filters, and current-screen article rows.
- `ZeitLingq.Infrastructure.Settings` provides JSON settings behind `SettingsPort`.

## GUI direction

The most sensible first GUI remains Monomer because the app state already fits a pure model/update flow. Monomer should stay as an adapter, not as the center of the architecture, so replacing it later would not touch scraping, persistence, or domain rules.

The optional `zeit-lingq-tool-gui` executable is guarded by the `gui` Cabal flag. The default build keeps native GUI dependencies out of CI and CLI workflows, while `cabal run -fgui zeit-lingq-tool-gui` enables the Monomer shell. Monomer depends on native graphics packages, including GLEW through `pkg-config`.

## CLI harness

The executable provides a terminal harness around the adapters. It is useful for verifying scraper, persistence, settings, known-word, audio, and LingQ behavior before the GUI is ready.

The CLI also exposes the JSON settings adapter so view preferences, browse section, date-prefix behavior, and section-specific LingQ collection mappings can be exercised before a GUI is attached.

## App runtime

The pure app update loop has a small command interpreter for persisted settings. That keeps Monomer or any other GUI layer focused on rendering and event wiring.

Startup hydration is port-driven: a GUI adapter can load `SettingsPort` into the pure `Model` before rendering its first frame.
