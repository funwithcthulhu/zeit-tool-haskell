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
- Zeit sessions are represented as cookie header plus browser user-agent, so GUI imports can reuse the identity of the real browser session that produced the cookies.
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

The desktop GUI uses Monomer because the app state fits a pure model/update flow. Monomer stays as an adapter, not as the center of the architecture, so replacing it later would not touch scraping, persistence, or domain rules.

The optional `zeit-lingq-tool-gui` executable is guarded by the `gui` Cabal flag. The default build keeps native GUI dependencies out of CI and CLI workflows, while `run-zeit-tool.ps1` prepares the Windows UCRT `pkg-config` paths and launches the Monomer shell. The desktop shortcut invokes `launch-zeit-tool-gui.vbs` so the GUI opens without a visible terminal.

The Windows installer uses the same GUI executable and stages the runtime DLLs discovered from the MSYS2 UCRT dependency graph. It installs per-user under LocalAppData so the app can keep its SQLite database, JSON settings, logs, audio, and support bundles beside the executable without elevation.

## CLI harness

The executable provides a terminal harness around the adapters. It is useful for verifying scraper, persistence, settings, known-word, audio, and LingQ behavior without opening the GUI.

The CLI also exposes the JSON settings adapter so view preferences, browse section, date-prefix behavior, Zeit browser identity, and section-specific LingQ collection mappings can be exercised from scripts or terminal workflows.

## App runtime

The pure app update loop has a command interpreter for persisted settings, refresh effects, and article content loading. Refresh commands load browse, library, and LingQ article rows through ports and return pure loaded events. Opening a saved article emits a content-load command, and the runtime returns either loaded article content or a notification event. That keeps Monomer or any other GUI layer focused on rendering and event wiring.

`ZeitLingq.App.Driver` composes the pure update function with the command runtime: it dispatches an event, runs generated commands through ports, and folds follow-up events back into the model.

Startup hydration is port-driven: a GUI adapter can load `SettingsPort` into the pure `Model` before rendering its first frame.

Batch fetch/upload queue state is also represented in the pure model. The Monomer adapter owns the actual background producers and a small cooperative cancellation flag, but queuing, pausing, clearing, and completed-job history are modeled as regular app events so the behavior remains testable and renderer-independent.

Zeit login is deliberately browser-session based instead of headless. The GUI opens the user's installed Edge or Chrome for interactive login, imports the authenticated cookie header and matching user-agent, then feeds those values through the `ZeitSession` port boundary.
