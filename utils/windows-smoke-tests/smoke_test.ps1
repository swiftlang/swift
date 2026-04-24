# Smoke-tests the installed Swift toolchain.
$ErrorActionPreference = 'Stop'

Write-Host "Starting smoke test"
Write-Host ""

swift --version
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Write-Host ""

Write-Host "Smoke test completed successfully!" -ForegroundColor Green
