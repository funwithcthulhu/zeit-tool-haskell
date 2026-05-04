param(
  [switch] $Cli,
  [Parameter(ValueFromRemainingArguments = $true)]
  [string[]] $ToolArgs
)

$ErrorActionPreference = 'Stop'

function Write-LaunchFailure {
  param([object] $Failure)

  $logDir = Join-Path $PSScriptRoot 'logs'
  $logPath = Join-Path $logDir 'shortcut-launch.log'
  if (-not (Test-Path -LiteralPath $logDir)) {
    $null = New-Item -ItemType Directory -Path $logDir -Force
  }

  $message = @(
    "[$(Get-Date -Format o)] GUI launcher failed"
    ($Failure | Out-String).Trim()
    ""
  ) -join [Environment]::NewLine
  Add-Content -LiteralPath $logPath -Value $message
}

trap {
  Write-LaunchFailure $_
  throw
}

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

function Resolve-BuiltGuiExe {
  $distDir = Join-Path $PSScriptRoot 'dist-newstyle'
  if (-not (Test-Path -LiteralPath $distDir)) {
    return $null
  }

  $latest =
    Get-ChildItem -LiteralPath $distDir -Filter 'zeit-lingq-tool-gui.exe' -Recurse -ErrorAction SilentlyContinue |
    Sort-Object LastWriteTime -Descending |
    Select-Object -First 1

  if ($latest) {
    return $latest.FullName
  }
  return $null
}

function Resolve-GuiExe {
  $builtExe = Resolve-BuiltGuiExe
  if ($builtExe) {
    return $builtExe
  }

  $listedExe = (& cabal list-bin -fgui exe:zeit-lingq-tool-gui).Trim()
  if ($LASTEXITCODE -eq 0 -and -not [string]::IsNullOrWhiteSpace($listedExe) -and (Test-Path -LiteralPath $listedExe)) {
    return $listedExe
  }

  cabal build -fgui exe:zeit-lingq-tool-gui
  if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
  }

  $rebuiltExe = Resolve-BuiltGuiExe
  if ($rebuiltExe) {
    return $rebuiltExe
  }

  $listedExe = (& cabal list-bin -fgui exe:zeit-lingq-tool-gui).Trim()
  if ($LASTEXITCODE -ne 0 -or [string]::IsNullOrWhiteSpace($listedExe) -or -not (Test-Path -LiteralPath $listedExe)) {
    throw 'Could not find the Zeit Tool Haskell GUI executable after building.'
  }
  return $listedExe
}

if ($Cli) {
  cabal run zeit-lingq-tool -- @ToolArgs
} else {
  $guiExe = Resolve-GuiExe
  if ($ToolArgs.Count -gt 0) {
    Start-Process -FilePath $guiExe -WorkingDirectory $PSScriptRoot -ArgumentList $ToolArgs -WindowStyle Normal
  } else {
    Start-Process -FilePath $guiExe -WorkingDirectory $PSScriptRoot -WindowStyle Normal
  }
  exit 0
}

exit $LASTEXITCODE
