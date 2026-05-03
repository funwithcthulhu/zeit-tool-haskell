Set shell = CreateObject("WScript.Shell")
shell.CurrentDirectory = "C:\projects\zeit-tool-haskell"
shell.Run "powershell.exe -NoProfile -ExecutionPolicy Bypass -File ""C:\projects\zeit-tool-haskell\run-zeit-tool.ps1""", 0, False
