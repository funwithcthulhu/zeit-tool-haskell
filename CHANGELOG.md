# Revision history for zeit-lingq-tool

## Unreleased

* Replaced raw retry-list tuples with named article fetch/upload failure records.
* Renamed a few app and GUI events so they describe view, browse, and LingQ actions more directly.
* Rewrote README and architecture notes around the actual personal-tool workflow and module boundaries.

## 0.1.0.0 -- 2026-05-03

* Added the GUI, SQLite library, Zeit browsing/fetching, LingQ upload, known-word handling, audio helpers, diagnostics queue, browser-session Zeit login, and Windows installer packaging.
* Added the short `zt` CLI command and kept the longer command names for existing scripts.
* Added schema version tracking, SQLite connection pragmas, and indexes for common library queries.

## 0.1.0.1 -- 2026-05-03

* Linked the GUI as a Windows desktop executable so installer and desktop shortcuts open the app without a console window.
* Changed the development launcher to use the latest built GUI binary first and write shortcut startup failures to `logs\shortcut-launch.log`.
