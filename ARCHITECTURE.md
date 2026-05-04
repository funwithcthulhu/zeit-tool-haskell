# Architecture

This project keeps user workflows separate from IO. Article rules, model updates, batch decisions, known-word scoring, and view-model projection are pure. Zeit HTTP, LingQ HTTP, SQLite, settings, audio, and the desktop shell sit behind explicit adapters.

## Main Boundaries

- `ZeitLingq.Domain`: article, library, LingQ, settings, and job types.
- `ZeitLingq.Text`: German tokenization and stemming.
- `ZeitLingq.Core`: pure or callback-driven workflows for fetching, uploading, and known-word scoring.
- `ZeitLingq.App`: model, events, command runtime, startup loading, and view models.
- `ZeitLingq.Ports`: records that describe every effect the app runtime can perform.
- `ZeitLingq.Infrastructure`: production adapters for Zeit, LingQ, SQLite, JSON settings, and audio.
- `gui`: Monomer adapter, theme, browser-session parsing, and user-facing error text.
- `app`: CLI adapter.

## Event Flow

The GUI sends a `ZeitLingq.App.Update.Event`. The pure `update` function returns a new `Model` plus commands. `ZeitLingq.App.Runtime` interprets those commands through `AppPorts`, then returns follow-up events. `ZeitLingq.App.Driver` folds those events back into the model.

That flow keeps the main behavior testable without Monomer, HTTP, or SQLite. The GUI owns only widget layout, user interaction, background task launching, and small platform side effects such as opening files or URLs.

## Storage

`ZeitLingq.Infrastructure.Sqlite` owns schema creation, migration, and query behavior. Database startup:

- enables foreign keys;
- sets a busy timeout for concurrent app/CLI access;
- enables WAL mode when SQLite supports it;
- creates missing columns and indexes idempotently;
- records the current schema version in `schema_migrations`.

The SQLite adapter is accessed through `LibraryPort`, so tests and future storage backends can use the same app/runtime boundary.

## Zeit Authentication

Zeit access is browser-session based. The GUI opens the user's installed Edge or Chrome for login, imports the resulting Cookie header and matching user-agent, and passes those values through `ZeitSession`. This avoids brittle headless login code and better matches Zeit's anti-bot posture.

Manual Cookie-header paste remains available for CLI use and for platforms where browser import is not implemented.

## LingQ

`ZeitLingq.Infrastructure.Lingq` handles login, language and collection reads, known-word sync, lesson upload, lesson update, and existing lesson discovery. Upload decisions stay in `ZeitLingq.Core.Upload`, which keeps collection mapping and title behavior easy to test.

## GUI

The Monomer executable is behind the `gui` Cabal flag. The default CLI/test build does not require native GUI libraries. On Windows, `run-zeit-tool.ps1` prepares the MSYS2 UCRT `pkg-config` environment and launches the GUI; `launch-zeit-tool-gui.vbs` is used by the desktop shortcut to avoid a visible terminal.

GUI-specific helpers are kept out of `gui/Main.hs`:

- `ZeitLingq.Gui.Theme`: colors and Monomer theme selection.
- `ZeitLingq.Gui.BrowserSession`: browser-login JSON/cookie parsing.
- `ZeitLingq.Gui.Error`: auth-related failure guidance.

## CLI

The CLI wraps the same adapters as the GUI. `zt` is the preferred short executable for checks and scripts. Long command names still parse for compatibility, but the public docs focus on the shorter commands in [COMMANDS.md](COMMANDS.md).

## Packaging

`scripts/build-installer.ps1` builds optimized binaries, stages runtime DLLs from the MSYS2 UCRT dependency graph, copies docs and helper scripts, and invokes Inno Setup when available. The installer is per-user so the app can write its database, settings, logs, audio, and support bundles without elevation.
