param(
  [int] $Port = 9227,
  [int] $TimeoutSeconds = 240,
  [string] $ProfileDir = (Join-Path (Split-Path $PSScriptRoot -Parent) 'zeit-browser-profile')
)

$ErrorActionPreference = 'Stop'

$loginUrl = 'https://meine.zeit.de/anmelden?url=https%3A%2F%2Fwww.zeit.de%2Findex&entry_service=sonstige'

function Find-Browser {
  $candidates = @(
    "$env:ProgramFiles\Microsoft\Edge\Application\msedge.exe",
    "${env:ProgramFiles(x86)}\Microsoft\Edge\Application\msedge.exe",
    "$env:LOCALAPPDATA\Microsoft\Edge\Application\msedge.exe",
    "$env:ProgramFiles\Google\Chrome\Application\chrome.exe",
    "${env:ProgramFiles(x86)}\Google\Chrome\Application\chrome.exe",
    "$env:LOCALAPPDATA\Google\Chrome\Application\chrome.exe"
  )

  foreach ($candidate in $candidates) {
    if ($candidate -and (Test-Path -LiteralPath $candidate)) {
      return $candidate
    }
  }

  throw 'Could not find Microsoft Edge or Google Chrome.'
}

function Invoke-Cdp {
  param(
    [Parameter(Mandatory = $true)] [string] $WebSocketUrl,
    [Parameter(Mandatory = $true)] [string] $Method
  )

  $ws = [System.Net.WebSockets.ClientWebSocket]::new()
  try {
    $ws.ConnectAsync([Uri] $WebSocketUrl, [Threading.CancellationToken]::None).GetAwaiter().GetResult()

    $payload = @{ id = 1; method = $Method } | ConvertTo-Json -Compress
    $bytes = [Text.Encoding]::UTF8.GetBytes($payload)
    $ws.SendAsync(
      [ArraySegment[byte]]::new($bytes),
      [System.Net.WebSockets.WebSocketMessageType]::Text,
      $true,
      [Threading.CancellationToken]::None
    ).GetAwaiter().GetResult()

    $buffer = New-Object byte[] 1048576
    $deadline = (Get-Date).AddSeconds(10)
    while ((Get-Date) -lt $deadline) {
      $builder = [System.Text.StringBuilder]::new()
      do {
        $result = $ws.ReceiveAsync(
          [ArraySegment[byte]]::new($buffer),
          [Threading.CancellationToken]::None
        ).GetAwaiter().GetResult()
        if ($result.Count -gt 0) {
          [void] $builder.Append([Text.Encoding]::UTF8.GetString($buffer, 0, $result.Count))
        }
      } while (-not $result.EndOfMessage)

      $message = $builder.ToString() | ConvertFrom-Json
      if ($message.id -eq 1) {
        return $message
      }
    }

    throw "Timed out waiting for Chrome DevTools response: $Method"
  } finally {
    $ws.Dispose()
  }
}

function Get-ZeitBrowserSession {
  $targets = Invoke-RestMethod -Uri "http://127.0.0.1:$Port/json" -TimeoutSec 2
  $target = @($targets | Where-Object {
      $_.type -eq 'page' -and
      $_.webSocketDebuggerUrl -and
      $_.url -match 'zeit\.de'
    } | Select-Object -First 1)[0]

  if (-not $target) {
    return $null
  }

  if ($target.url -match 'login\.zeit\.de|anmelden') {
    return $null
  }

  $cookieResponse = Invoke-Cdp -WebSocketUrl $target.webSocketDebuggerUrl -Method 'Network.getAllCookies'
  $cookies = @($cookieResponse.result.cookies | Where-Object {
      $_.domain -and ($_.domain -match '(^|\.)zeit\.de$')
    })

  if ($cookies.Count -eq 0) {
    return $null
  }

  $versionResponse = Invoke-Cdp -WebSocketUrl $target.webSocketDebuggerUrl -Method 'Browser.getVersion'
  $userAgent = $versionResponse.result.userAgent
  if (-not $userAgent) {
    $userAgent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36'
  }

  return @{
    cookieHeader = (($cookies | ForEach-Object { "$($_.name)=$($_.value)" }) -join '; ')
    userAgent = $userAgent
  }
}

New-Item -ItemType Directory -Force -Path $ProfileDir | Out-Null

$browser = Find-Browser
$args = @(
  "--remote-debugging-port=$Port",
  "--user-data-dir=$ProfileDir",
  '--no-first-run',
  '--new-window',
  $loginUrl
)

Start-Process -FilePath $browser -ArgumentList $args | Out-Null

$deadline = (Get-Date).AddSeconds($TimeoutSeconds)
while ((Get-Date) -lt $deadline) {
  try {
    $session = Get-ZeitBrowserSession
    if ($session) {
      Write-Output ($session | ConvertTo-Json -Compress)
      exit 0
    }
  } catch {
    Start-Sleep -Milliseconds 750
    continue
  }

  Start-Sleep -Seconds 1
}

throw "Timed out waiting for Zeit cookies. Complete the login in the browser window, then try again if needed."
