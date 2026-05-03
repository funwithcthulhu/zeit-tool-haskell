param(
  [Parameter(ValueFromRemainingArguments = $true)]
  [string[]] $ToolArgs
)

Set-Location -LiteralPath $PSScriptRoot

if ($ToolArgs.Count -eq 0) {
  cabal run zeit-lingq-tool --
} else {
  cabal run zeit-lingq-tool -- @ToolArgs
}
