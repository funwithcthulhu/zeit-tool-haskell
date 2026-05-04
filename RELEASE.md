# Release Checklist

Use this checklist before tagging a build or packaging a shareable installer from this personal project.

## Local Checks

```powershell
cabal test
cabal check
cabal build exe:zt exe:zeit-lingq-tool
cabal build -fgui exe:zeit-lingq-tool-gui
.\zt h
.\zt t
```

## GUI Smoke Test

```powershell
.\run-zeit-tool.ps1
```

Check:

- the window opens without a console launcher staying visible;
- installer and desktop shortcuts open the GUI without a Command Prompt or PowerShell window;
- Browse loads a Zeit section;
- Library opens and filters saved rows;
- LingQ login state displays clearly;
- Zeit browser-session import gives a useful success or failure message;
- Diagnostics can copy recent logs and create a support bundle.

## Installer

```powershell
.\scripts\build-installer.ps1
```

Confirm that the script prints a SHA256 hash and writes `dist\ZeitToolHaskellSetup-<version>.exe`.

## Documentation

- README describes the current GUI honestly without making the project sound broader than it is.
- COMMANDS examples run with `zt`.
- ARCHITECTURE describes any new module boundary.
- CHANGELOG has a concise entry for the release.
