<#
.SYNOPSIS
Smoke-tests a Swift installer in a Windows container.

.DESCRIPTION
Builds a container image from the swift-docker smoke-test Dockerfile, installing
the given installer into it, then runs `EntryPoint` inside the resulting
container.

Requires a swift-docker checkout and a Docker installation configured for
Windows containers.

.PARAMETER Installer
Path to the installer to test.

.PARAMETER SourceCache
Path to the directory containing the swift-docker checkout.

.PARAMETER EntryPoint
PowerShell script containing the tests to run inside the container.
#>
[CmdletBinding()]
param
(
  [Parameter(Mandatory)]
  [string] $Installer,
  [Parameter(Mandatory)]
  [string] $SourceCache,
  [Parameter(Mandatory)]
  [string] $EntryPoint
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path $EntryPoint)) {
  throw "Entry point not found at '$EntryPoint'."
}

if (-not (Test-Path $Installer)) {
  throw "Installer not found at '$Installer'."
}

$WindowsBuild = [int](Get-ItemProperty "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion").CurrentBuildNumber
if ($WindowsBuild -ge 20348) {
  $Variant = "ltsc2022-full"
} elseif ($WindowsBuild -ge 17763) {
  $Variant = "1809-full"
} else {
  throw "Smoke tests are not supported on this Windows version (build $WindowsBuild)."
}

$Dockerfile = Join-Path $SourceCache "swift-docker\swift-ci\main\windows\smoketest\$Variant\Dockerfile"
if (-not (Test-Path $Dockerfile)) {
  throw "Smoke tests require a swift-docker checkout at '$(Join-Path $SourceCache "swift-docker")' (expected a Dockerfile at '$Dockerfile')."
}

if (-not (Get-Command docker -ErrorAction Ignore)) {
  throw "Docker was not found on the path."
}
docker version | Out-Null
if ($LASTEXITCODE -ne 0) {
  throw "'docker version' failed; the Docker daemon may not be running."
}
$DockerInfo = docker info
if ($LASTEXITCODE -ne 0) {
  throw "'docker info' failed; the Docker daemon may not be running."
}
if (-not ($DockerInfo -match "OSType:\s*windows")) {
  throw "Docker is not configured for Windows containers."
}

$WorkingDir = Join-Path $env:TEMP "swift-smoke-test-$([Guid]::NewGuid().ToString("n"))"
New-Item -ItemType Directory -Path $WorkingDir | Out-Null
try {
  Copy-Item $Dockerfile (Join-Path $WorkingDir "Dockerfile")

  Copy-Item $Installer (Join-Path $WorkingDir (Split-Path $Installer -Leaf))
  Copy-Item $EntryPoint (Join-Path $WorkingDir (Split-Path $EntryPoint -Leaf))

  Write-Host "[smoke-test] docker build ..."
  $Start = [DateTime]::Now
  docker build `
    --file (Join-Path $WorkingDir "Dockerfile") `
    --build-arg SWIFT_INSTALLER_PATH=$(Split-Path $Installer -Leaf) `
    --build-arg ENTRY_POINT=$(Split-Path $EntryPoint -Leaf) `
    --tag swift-smoke-test `
    $WorkingDir
  if ($LASTEXITCODE -ne 0) { throw "docker build failed with exit code $LASTEXITCODE." }
  Write-Host "[smoke-test] docker build took $(([DateTime]::Now - $Start).ToString("hh\:mm\:ss"))"

  Write-Host "[smoke-test] docker run ..."
  $Start = [DateTime]::Now
  docker run --rm swift-smoke-test
  if ($LASTEXITCODE -ne 0) { throw "Smoke test failed with exit code $LASTEXITCODE." }
  Write-Host "[smoke-test] docker run took $(([DateTime]::Now - $Start).ToString("hh\:mm\:ss"))"

  Write-Host -ForegroundColor Green "[smoke-test] passed"
} finally {
  Remove-Item -Recurse -Force $WorkingDir -ErrorAction Ignore
  if (docker images --quiet swift-smoke-test) {
    docker rmi swift-smoke-test | Out-Null
  }
}

exit 0
