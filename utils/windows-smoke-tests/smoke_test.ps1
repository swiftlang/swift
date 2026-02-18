# Attempts to build and test `swift-win32`.
$ErrorActionPreference = 'Stop'

Write-Host "Starting smoke test"
Write-Host ""

swift --version
Write-Host ""

$RepoUrl = "https://github.com/compnerd/swift-win32"
$CloneDir = "C:\swift-win32"

Write-Host "Cloning $RepoUrl"
git clone $RepoUrl $CloneDir
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Write-Host ""

Set-Location $CloneDir

Write-Host "Building swift-win32"
swift build
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Write-Host ""

Write-Host "Testing swift-win32"
swift test -Xswiftc -DENABLE_TESTING
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Write-Host ""

Write-Host "Smoke test completed successfully!" -ForegroundColor Green
