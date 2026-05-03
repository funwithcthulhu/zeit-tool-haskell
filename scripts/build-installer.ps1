param(
  [string] $Version = "",
  [string] $OutputDir = "",
  [string] $IsccPath = "",
  [switch] $SkipBuild,
  [switch] $StageOnly
)

$ErrorActionPreference = 'Stop'

function Resolve-RepoRoot {
  return (Split-Path -Parent $PSScriptRoot)
}

function Resolve-IsccPath {
  param([string] $ExplicitPath)

  if (-not [string]::IsNullOrWhiteSpace($ExplicitPath)) {
    if (Test-Path -LiteralPath $ExplicitPath) {
      return (Resolve-Path -LiteralPath $ExplicitPath).Path
    }
    throw "The specified Inno Setup compiler was not found at $ExplicitPath"
  }

  if ($env:INNO_SETUP_COMPILER -and (Test-Path -LiteralPath $env:INNO_SETUP_COMPILER)) {
    return (Resolve-Path -LiteralPath $env:INNO_SETUP_COMPILER).Path
  }

  $command = Get-Command ISCC.exe -ErrorAction SilentlyContinue
  if ($command) {
    return $command.Source
  }

  $candidates = @(
    (Join-Path $env:LOCALAPPDATA 'Programs\Inno Setup 6\ISCC.exe'),
    'C:\Program Files (x86)\Inno Setup 6\ISCC.exe',
    'C:\Program Files\Inno Setup 6\ISCC.exe'
  )

  foreach ($candidate in $candidates) {
    if (Test-Path -LiteralPath $candidate) {
      return $candidate
    }
  }

  $registryKeys = @(
    'HKCU:\Software\Microsoft\Windows\CurrentVersion\Uninstall\Inno Setup 6_is1',
    'HKLM:\Software\Microsoft\Windows\CurrentVersion\Uninstall\Inno Setup 6_is1',
    'HKLM:\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\Inno Setup 6_is1'
  )

  foreach ($key in $registryKeys) {
    if (Test-Path -LiteralPath $key) {
      $installLocation = (Get-ItemProperty -LiteralPath $key -ErrorAction SilentlyContinue).InstallLocation
      if (-not [string]::IsNullOrWhiteSpace($installLocation)) {
        $candidate = Join-Path $installLocation 'ISCC.exe'
        if (Test-Path -LiteralPath $candidate) {
          return $candidate
        }
      }
    }
  }

  return $null
}

function Get-CabalVersion {
  param([string] $RepoRoot)

  $cabalFile = Join-Path $RepoRoot 'zeit-lingq-tool.cabal'
  $line = Get-Content -LiteralPath $cabalFile | Where-Object { $_ -match '^\s*version\s*:' } | Select-Object -First 1
  if (-not $line) {
    throw "Could not find package version in $cabalFile"
  }
  return (($line -split ':', 2)[1]).Trim()
}

function Initialize-BuildEnvironment {
  param([string] $RepoRoot)

  $ucrtBin = 'C:\msys64\ucrt64\bin'
  $msysBin = 'C:\msys64\usr\bin'
  $localPkgConfig = Join-Path $RepoRoot 'pkgconfig'
  $ucrtPkgConfig = 'C:\msys64\ucrt64\lib\pkgconfig'

  $env:PATH = (@($ucrtBin, $msysBin) + ($env:PATH -split ';') | Where-Object { $_ } | Select-Object -Unique) -join ';'
  $env:PKG_CONFIG_PATH = (@($localPkgConfig, $ucrtPkgConfig) + ($env:PKG_CONFIG_PATH -split ';') | Where-Object { $_ -and (Test-Path -LiteralPath $_) } | Select-Object -Unique) -join ';'
  $env:PKG_CONFIG_DONT_DEFINE_PREFIX = '1'
}

function Invoke-Cabal {
  param(
    [string[]] $Arguments,
    [string] $RepoRoot
  )

  Push-Location $RepoRoot
  try {
    & cabal @Arguments
    if ($LASTEXITCODE -ne 0) {
      throw "cabal $($Arguments -join ' ') failed"
    }
  }
  finally {
    Pop-Location
  }
}

function Get-CabalBin {
  param(
    [string[]] $Arguments,
    [string] $RepoRoot
  )

  Push-Location $RepoRoot
  try {
    $path = (& cabal @Arguments).Trim()
    if ($LASTEXITCODE -ne 0 -or [string]::IsNullOrWhiteSpace($path)) {
      throw "Could not resolve cabal binary with: cabal $($Arguments -join ' ')"
    }
    if (-not (Test-Path -LiteralPath $path)) {
      throw "Resolved cabal binary does not exist: $path"
    }
    return $path
  }
  finally {
    Pop-Location
  }
}

function Copy-IfExists {
  param(
    [string] $Source,
    [string] $Destination
  )

  if (Test-Path -LiteralPath $Source) {
    Copy-Item -LiteralPath $Source -Destination $Destination -Force
  }
}

function Copy-MsysDllDependencies {
  param(
    [string] $ExePath,
    [string] $StageDir
  )

  $ldd = 'C:\msys64\usr\bin\ldd.exe'
  if (-not (Test-Path -LiteralPath $ldd)) {
    throw "MSYS2 ldd.exe not found at $ldd"
  }

  $output = & $ldd $ExePath
  if ($LASTEXITCODE -ne 0) {
    throw "ldd failed for $ExePath"
  }

  foreach ($line in $output) {
    $match = [regex]::Match($line, '=>\s+/ucrt64/bin/([^\s]+\.dll)')
    if ($match.Success) {
      $dll = Join-Path 'C:\msys64\ucrt64\bin' $match.Groups[1].Value
      if (Test-Path -LiteralPath $dll) {
        Copy-Item -LiteralPath $dll -Destination $StageDir -Force
      }
    }
  }
}

$repoRoot = Resolve-RepoRoot
$resolvedOutputDir = if ([string]::IsNullOrWhiteSpace($OutputDir)) {
  Join-Path $repoRoot 'dist'
} else {
  $OutputDir
}
$stageDir = Join-Path $resolvedOutputDir 'installer-staging'
$issFile = Join-Path $repoRoot 'installer\ZeitToolHaskell.iss'

if ([string]::IsNullOrWhiteSpace($Version)) {
  $Version = Get-CabalVersion -RepoRoot $repoRoot
}

Initialize-BuildEnvironment -RepoRoot $repoRoot
$releaseProfileArgs = @('--enable-optimization=2')

if (-not $SkipBuild) {
  Invoke-Cabal -RepoRoot $repoRoot -Arguments (@('build', '-fgui') + $releaseProfileArgs + @('exe:zeit-lingq-tool', 'exe:zeit-lingq-tool-gui'))
}

$guiExe = Get-CabalBin -RepoRoot $repoRoot -Arguments (@('list-bin', '-fgui') + $releaseProfileArgs + @('exe:zeit-lingq-tool-gui'))
$cliExe = Get-CabalBin -RepoRoot $repoRoot -Arguments (@('list-bin') + $releaseProfileArgs + @('exe:zeit-lingq-tool'))

if (Test-Path -LiteralPath $stageDir) {
  Remove-Item -LiteralPath $stageDir -Recurse -Force
}
$null = New-Item -ItemType Directory -Path $stageDir -Force
$null = New-Item -ItemType Directory -Path (Join-Path $stageDir 'scripts') -Force
$null = New-Item -ItemType Directory -Path (Join-Path $stageDir 'docs') -Force

Copy-Item -LiteralPath $guiExe -Destination (Join-Path $stageDir 'Zeit Tool Haskell.exe') -Force
Copy-Item -LiteralPath $cliExe -Destination (Join-Path $stageDir 'zeit-lingq-tool.exe') -Force
Copy-Item -LiteralPath (Join-Path $repoRoot 'scripts\zeit-browser-login.ps1') -Destination (Join-Path $stageDir 'scripts\zeit-browser-login.ps1') -Force
Copy-MsysDllDependencies -ExePath $guiExe -StageDir $stageDir
Copy-MsysDllDependencies -ExePath $cliExe -StageDir $stageDir

Copy-IfExists -Source (Join-Path $repoRoot 'LICENSE') -Destination (Join-Path $stageDir 'LICENSE.txt')
Copy-IfExists -Source (Join-Path $repoRoot 'README.md') -Destination (Join-Path $stageDir 'docs\README.md')
Copy-IfExists -Source (Join-Path $repoRoot 'ARCHITECTURE.md') -Destination (Join-Path $stageDir 'docs\ARCHITECTURE.md')
Copy-IfExists -Source (Join-Path $repoRoot 'CHANGELOG.md') -Destination (Join-Path $stageDir 'docs\CHANGELOG.md')

$installerReadme = @"
Zeit Tool Haskell $Version

Launch:
- Start Menu: Zeit Tool Haskell
- Desktop shortcut, if selected during setup
- CLI: zeit-lingq-tool.exe from the install folder

Data storage:
- settings.json, zeit-tool.db, logs, audio, and support bundles are written beside the installed executable.
- The default installer target is under LocalAppData so the app can write its own data without administrator rights.

Zeit login:
- Use the GUI's Browser login & import action. It opens a real Edge/Chrome window and imports your authenticated zeit.de cookies plus the matching browser user-agent.
- This avoids brittle headless login automation and keeps article requests aligned with the browser session you created interactively.

LingQ:
- Connect an API key or password login from the LingQ view before uploading.
"@
Set-Content -LiteralPath (Join-Path $stageDir 'README.txt') -Value $installerReadme -Encoding ASCII

Write-Host "Staged installer files at: $stageDir" -ForegroundColor Cyan

if ($StageOnly) {
  Write-Host "StageOnly requested; skipping Inno Setup." -ForegroundColor Yellow
  exit 0
}

$resolvedIsccPath = Resolve-IsccPath -ExplicitPath $IsccPath
if (-not $resolvedIsccPath) {
  Write-Host ""
  Write-Host "Inno Setup 6 was not found, so the portable staging folder is ready but no setup.exe was built." -ForegroundColor Yellow
  Write-Host "Install Inno Setup with: winget install JRSoftware.InnoSetup" -ForegroundColor Yellow
  Write-Host "Then rerun: powershell -ExecutionPolicy Bypass -File .\scripts\build-installer.ps1" -ForegroundColor Yellow
  exit 2
}

& $resolvedIsccPath `
  "/DAppVersion=$Version" `
  "/DStageDir=$stageDir" `
  "/DOutputDir=$resolvedOutputDir" `
  $issFile

if ($LASTEXITCODE -ne 0) {
  throw "Inno Setup compilation failed"
}

$installerOutput = Join-Path $resolvedOutputDir "ZeitToolHaskellSetup-$Version.exe"
if (-not (Test-Path -LiteralPath $installerOutput)) {
  throw "Expected installer output not found at $installerOutput"
}

$installer = Get-Item -LiteralPath $installerOutput
$hash = (Get-FileHash -LiteralPath $installer.FullName -Algorithm SHA256).Hash

Write-Host ""
Write-Host "Installer created: $($installer.FullName)" -ForegroundColor Green
Write-Host "Size: $([math]::Round($installer.Length / 1MB, 1)) MB"
Write-Host "SHA256: $hash"
