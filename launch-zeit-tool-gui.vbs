Function Quote(value)
  Quote = Chr(34) & value & Chr(34)
End Function

Set shell = CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")

scriptDir = fso.GetParentFolderName(WScript.ScriptFullName)
scriptPath = fso.BuildPath(scriptDir, "run-zeit-tool.ps1")
logDir = fso.BuildPath(scriptDir, "logs")
logPath = fso.BuildPath(logDir, "shortcut-launch.log")

shell.CurrentDirectory = scriptDir
exitCode = shell.Run("powershell.exe -NoProfile -ExecutionPolicy Bypass -File " & Quote(scriptPath), 0, True)

If exitCode <> 0 Then
  shell.Popup "Zeit Tool Haskell did not start. Details were written to:" & vbCrLf & logPath, 0, "Zeit Tool Haskell", 16
End If
