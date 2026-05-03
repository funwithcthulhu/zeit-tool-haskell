param(
  [Parameter(ValueFromRemainingArguments = $true)]
  [string[]] $ToolArgs
)

$ErrorActionPreference = 'Stop'

& (Join-Path $PSScriptRoot 'run-zeit-tool.ps1') cli @ToolArgs
exit $LASTEXITCODE
