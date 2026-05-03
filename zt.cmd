@echo off
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0zt.ps1" %*
exit /b %ERRORLEVEL%
