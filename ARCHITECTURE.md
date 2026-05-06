# Architecture

This repo is a small desktop app with a maintenance CLI. The main workflow is: browse Zeit, save
articles to SQLite, estimate LingQ coverage, and upload selected articles to LingQ.

## Dependency Direction

The usual app flow is:

```text
app/gui entry points
  -> ZeitLingq.Infrastructure wiring
  -> ZeitLingq.App
  -> ZeitLingq.Core
  -> ZeitLingq.Domain / ZeitLingq.Text
```

`app/Main.hs` and `gui/Main.hs` wire the real SQLite, HTTP, JSON settings, and audio adapters.
`ZeitLingq.App.Runtime` reaches those effects through `ZeitLingq.Ports`, which keeps the pure update
tests from needing a database or network.

The domain modules do not import GUI, CLI, runtime, or infrastructure modules.

## Pure And Domain Code

- `ZeitLingq.Domain.Types` defines article, library, LingQ, notification, queued-job, retry-failure,
  and settings-facing data types.
- `ZeitLingq.Domain.Article` holds article rules such as word counts, lesson title date prefixes,
  and word-filter decisions.
- `ZeitLingq.Domain.Section` lists the Zeit sections this tool knows how to browse.
- `ZeitLingq.Text.German` tokenizes and stems German words for rough LingQ coverage estimates.
- `ZeitLingq.Core.Batch`, `Core.Upload`, `Core.Browse`, and `Core.KnownWords` contain fetch
  filtering, retry-failure extraction, upload target choice, and known-word scoring.

## App State And Runtime

- `ZeitLingq.App.Model` is the GUI/runtime state: selected view, loaded rows, filters, auth status,
  retry lists, queue state, and user preferences.
- `ZeitLingq.App.Update` is the pure event reducer. It turns app events into a new model plus
  `Command` values.
- `ZeitLingq.App.Runtime` interprets commands through `AppPorts`, calls adapters, and returns
  follow-up events.
- `ZeitLingq.App.Driver` folds follow-up events back through the reducer.
- `ZeitLingq.App.Startup` loads persisted settings into the initial model.
- `ZeitLingq.App.ViewModel` projects model state into smaller values used by the Monomer view code.
- `ZeitLingq.App.UploadConfig` builds LingQ upload preferences from the current day and saved
  settings.

The app event flow is:

```text
GUI/CLI action
  -> Event
  -> update
  -> [Command]
  -> Runtime through AppPorts
  -> follow-up Event values
```

The job queue lives in `Model`. The GUI starts queued batch jobs and stores only retryable failure
records; the queue is not persisted.

## Infrastructure Adapters

- `ZeitLingq.Infrastructure.Sqlite` owns schema setup, migration, and queries. Startup enables
  foreign keys, sets a busy timeout, tries WAL mode, creates missing columns/indexes, and records
  the schema version.
- `ZeitLingq.Infrastructure.Zeit` fetches Zeit pages and extracts article summaries, article body
  text, pagination links, and audio URLs.
- `ZeitLingq.Infrastructure.Lingq` handles LingQ auth, languages, collections, known words, lesson
  upload/update, and collection lesson reads.
- `ZeitLingq.Infrastructure.Settings` reads and writes `settings.json`.
- `ZeitLingq.Infrastructure.Audio` downloads article audio and builds local filenames.

HTTP-facing functions generally return `Either` at fetch/upload boundaries. SQLite, settings, and
audio functions still use normal `IO` failure.

## GUI

The Monomer executable is optional and behind the Cabal `gui` flag. `gui/Main.hs` currently owns:

- widget layout for Browse, Library, LingQ, Zeit session, Diagnostics, and Article views;
- `GuiEvent` handling and translation into app events;
- queued fetch/upload producers with progress updates and cancellation checks;
- small desktop actions such as opening URLs, opening files, copying text, and writing support
  bundles;
- construction of the real `AppPorts` used by the GUI.

GUI-specific helpers live next to it:

- `ZeitLingq.Gui.Theme`: colors and Monomer theme selection.
- `ZeitLingq.Gui.BrowserSession`: JSON/cookie parsing from the browser-login script.
- `ZeitLingq.Gui.Error`: auth-related error wording.

On Windows, `run-zeit-tool.ps1` sets the MSYS2 UCRT `pkg-config` environment and launches the GUI.
The development shortcut calls that script hidden and logs startup failures to
`logs\shortcut-launch.log`.

## CLI

`app/Main.hs` is the CLI entry point for both `zeit-lingq-tool` and `zt`. It uses the same adapters
as the GUI and keeps commands direct: browse, save, list, cleanup, known words, LingQ upload, audio,
and settings edits.

`ZeitLingq.Cli` contains parsing and help text. Short commands are preferred for day-to-day use;
longer commands remain for old scripts.

## Tests

`test/Spec.hs` covers:

- pure text/domain rules;
- pure app updates and view-model projection;
- runtime command behavior through in-memory ports;
- CLI parsing;
- SQLite adapter behavior against temporary databases;
- JSON settings;
- LingQ response parsers;
- audio filename/download helpers;
- Zeit HTML extraction.

The default `cabal test` path does not build the GUI executable. Use
`cabal build -fgui exe:zeit-lingq-tool-gui` when touching `gui/Main.hs` or `gui/ZeitLingq/Gui`.

## Packaging

`scripts/build-installer.ps1` builds the binaries, stages runtime DLLs from the MSYS2 UCRT
dependency graph, copies docs and helper scripts, and calls Inno Setup when available. The installer
is per-user because the app writes its database, settings, logs, audio, and support bundles next to
the installed files.
