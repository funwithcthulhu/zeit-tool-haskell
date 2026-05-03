param(
  [switch] $Cli,
  [Parameter(ValueFromRemainingArguments = $true)]
  [string[]] $ToolArgs
)

$ErrorActionPreference = 'Stop'

Set-Location -LiteralPath $PSScriptRoot

if (-not $Cli -and $ToolArgs.Count -gt 0) {
  $firstArg = $ToolArgs[0].ToLowerInvariant()
  if ($firstArg -in @('cli', '--cli', '-cli')) {
    $Cli = $true
    $ToolArgs = @($ToolArgs | Select-Object -Skip 1)
  }
}

$ucrtBin = 'C:\msys64\ucrt64\bin'
$msysBin = 'C:\msys64\usr\bin'
$localPkgConfig = Join-Path $PSScriptRoot 'pkgconfig'
$ucrtPkgConfig = 'C:\msys64\ucrt64\lib\pkgconfig'

$pathParts = @($ucrtBin, $msysBin) + ($env:PATH -split ';')
$env:PATH = ($pathParts | Where-Object { $_ } | Select-Object -Unique) -join ';'

$pkgConfigParts = @($localPkgConfig, $ucrtPkgConfig) + ($env:PKG_CONFIG_PATH -split ';')
$env:PKG_CONFIG_PATH = ($pkgConfigParts | Where-Object { $_ -and (Test-Path $_) } | Select-Object -Unique) -join ';'
$env:PKG_CONFIG_DONT_DEFINE_PREFIX = '1'

if ($Cli) {
  cabal run zeit-lingq-tool -- @ToolArgs
} else {
  $guiExe = (& cabal list-bin -fgui zeit-lingq-tool-gui).Trim()
  if (-not (Test-Path -LiteralPath $guiExe)) {
    cabal build -fgui zeit-lingq-tool-gui
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    $guiExe = (& cabal list-bin -fgui zeit-lingq-tool-gui).Trim()
  }
  if ($ToolArgs.Count -gt 0) {
    Start-Process -FilePath $guiExe -WorkingDirectory $PSScriptRoot -ArgumentList $ToolArgs -WindowStyle Normal
  } else {
    Start-Process -FilePath $guiExe -WorkingDirectory $PSScriptRoot -WindowStyle Normal
  }
  exit 0
}

exit $LASTEXITCODE
