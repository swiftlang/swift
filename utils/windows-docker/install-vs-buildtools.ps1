# Copyright 2026 Apple Inc. and the Swift project authors
#
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

<#
.SYNOPSIS
Installs the Visual Studio Build Tools in the CI image.

.DESCRIPTION
The installer prints nothing in --quiet mode, so this reports the package being
installed every minute, and prints the installer logs if it fails or does not
finish within the timeout. CI only keeps the console output.
#>

param
(
  [Parameter(Mandatory)]
  [string] $URL,
  [Parameter(Mandatory)]
  [string] $InstallPath,
  [Parameter(Mandatory)]
  [string[]] $Components,
  [int] $TimeoutMinutes = 90
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

function Write-InstallerLogs {
  foreach ($Log in Get-ChildItem $env:TEMP -Filter "dd_*.log" | Sort-Object LastWriteTime) {
    if ($Log.Name -match "_errors\.log$|^dd_bootstrapper|^dd_client|^dd_setup_\d+\.log$") {
      Write-Host "===== $($Log.Name) (last 40 lines)"
      Get-Content $Log.FullName -Tail 40 | Write-Host
    }
  }
}

$Bootstrapper = Join-Path $env:TEMP "vs_buildtools.exe"
Invoke-WebRequest $URL -OutFile $Bootstrapper -UseBasicParsing

$Arguments = @("--quiet", "--wait", "--norestart", "--nocache", "--installPath", $InstallPath)
foreach ($Component in $Components) {
  $Arguments += @("--add", $Component)
}

$Stopwatch = [Diagnostics.Stopwatch]::StartNew()
$Process = Start-Process $Bootstrapper -ArgumentList $Arguments -PassThru
# Cache the handle; without it ExitCode is not available after the process exits.
$null = $Process.Handle

while (-not $Process.WaitForExit(60000)) {
  $Latest = Get-ChildItem $env:TEMP -Filter "dd_*.log" | Sort-Object LastWriteTime | Select-Object -Last 1
  $Memory = Get-CimInstance Win32_OperatingSystem
  Write-Host ("[{0:hh\:mm\:ss}] {1}, {2:N1}/{3:N1} GB free memory" -f $Stopwatch.Elapsed,
    $(if ($Latest) { $Latest.Name } else { "no installer log yet" }),
    ($Memory.FreePhysicalMemory / 1MB), ($Memory.TotalVisibleMemorySize / 1MB))

  if ($Stopwatch.Elapsed.TotalMinutes -ge $TimeoutMinutes) {
    Write-InstallerLogs
    Get-Process | Where-Object { $_.Name -match "^vs_|^setup$|^msiexec$" } | Stop-Process -Force -ErrorAction Ignore
    throw "Visual Studio Build Tools installation did not finish within $TimeoutMinutes minutes."
  }
}

# 3010: success, reboot required.
if ($Process.ExitCode -notin 0, 3010) {
  Write-InstallerLogs
  throw "vs_buildtools.exe exited with code $($Process.ExitCode)."
}
Write-Host ("Visual Studio Build Tools installed in {0:hh\:mm\:ss}" -f $Stopwatch.Elapsed)

Remove-Item $Bootstrapper, "$env:ProgramData\Package Cache" -Recurse -Force -ErrorAction Ignore
