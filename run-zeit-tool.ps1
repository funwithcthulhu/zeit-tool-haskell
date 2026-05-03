param(
  [switch] $Cli,
  [Parameter(ValueFromRemainingArguments = $true)]
  [string[]] $ToolArgs
)

$ErrorActionPreference = 'Stop'

Set-Location -LiteralPath $PSScriptRoot

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
  cabal run -fgui zeit-lingq-tool-gui -- @ToolArgs
}

exit $LASTEXITCODE
