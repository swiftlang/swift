# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

<#
.SYNOPSIS
Builds the Swift toolchain like BuildSequential.ps1, with some steps at the same
time.

.DESCRIPTION
This script takes the same parameters as BuildSequential.ps1 and runs the same
scripts, but runs the steps that do not depend on each other at the same time,
in separate processes:

- After the Stage1 toolchain, the Windows SDKs of the other architectures and
  the Android SDK build in the background, while the SDK of the host and then
  the toolchain build. The other Windows SDKs install into their own platform
  roots, which are merged into the image when they are done.
- The package tests run in the background. The test suites of the compilers
  build what they need once, then check-swift runs while the other tests run
  in the background.

The SDKs and the package tests build at a lower priority. The steps after
SetupDependencies.ps1 log to <BinaryCache>\BuildMulti\<name>.log. This script
prints the log of the step that it waits for as the step writes it, and the
log of each background process in one piece once the process ended and that
step is done, so that no log is split by another. When nothing was printed for
a minute, it prints the last line that the processes that it waits for logged.
After a failure, it also prints the logs of the processes that it stops. At the
end, after the summary, it prints the lines of the logs of the processes that
failed that report their failures, then the error.

See BuildSequential.ps1 for the parameters.
#>
[CmdletBinding(PositionalBinding = $false)]
param
(
  # Build Paths
  [System.IO.FileInfo] $SourceCache = "S:\SourceCache",
  [System.IO.FileInfo] $BinaryCache = "S:\BinaryCache",
  [System.IO.FileInfo] $ArtifactCache = "S:\ArtifactCache",
  [Alias("ImageRoot")]
  [System.IO.FileInfo] $BuildRoot = "S:",
  [Alias("Cache")]
  [System.IO.FileInfo] $ObjectStore = "S:\ObjectStore",
  [string] $Stage = "",

  # (Pinned) Bootstrap Toolchain
  [string] $PinnedBuild = "",
  [ValidatePattern("^([A-Fa-f0-9]{64}|)$")]
  [string] $PinnedSHA256 = "",
  [string] $PinnedVersion = "",

  # Build Caching
  [switch] $EnableCaching,

  # SBoM Support
  [switch] $IncludeSBoM = $false,
  [string] $SyftVersion = "1.40.0",

  # CMake
  [string] $CMakeVersion = "4.4.1",

  # Ninja
  [string] $NinjaVersion = "1.13.2",

  # Dependency Download Retries
  [ValidateRange(1, [int]::MaxValue)]
  [int] $DownloadRetryCount = 3,

  # Artifact Lock Timeout
  [ValidateRange(1, [int]::MaxValue)]
  [int] $ArtifactLockTimeoutSeconds = 1800,

  # Dependencies
  [ValidatePattern('^\d+(\.\d+)*$')]
  [string] $PythonVersion = "3.10.1",
  [switch] $DependenciesOnly = $False,

  # Toolchain Version Info
  [string] $ProductVersion = "0.0.0",
  [ValidatePattern("^[A-Fa-f0-9]{16}$")]
  [string] $WindowsSxSAssemblyPublicKeyToken = "0000000000000000",
  [string] $ToolchainIdentifier = $(if ($env:TOOLCHAIN_VERSION) { $env:TOOLCHAIN_VERSION } else { "$env:USERNAME.development" }),

  # Toolchain Cross-compilation
  [ValidateSet("AMD64", "ARM64")]
  [string] $HostArchName = $(if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }),
  [object] $UseHostToolchain = $true,

  # Debug Information
  [switch] $DebugInfo,

  # Android SDK Options
  [switch] $Android = $false,
  [ValidatePattern("^r(?:[1-9]|[1-9][0-9])(?:[a-z])?(-beta[1-9])?$")]
  [string] $AndroidNDKVersion = "r30",
  [ValidateRange(21, 36)]
  [int] $AndroidAPILevel = 23,
  [string[]] $AndroidSDKArchitectures = @("aarch64", "armv7", "i686", "x86_64"),
  [ValidateSet("dynamic", "static")]
  [string[]] $AndroidSDKLinkModes = @("dynamic", "static"),

  # Windows SDK Options
  [switch] $Windows = $false,
  [ValidatePattern("^\d+\.\d+\.\d+(?:-\w+)?")]
  [string] $WinSDKVersion = "",
  [string[]] $WindowsSDKArchitectures = @("X64","X86","Arm64"),
  [ValidateSet("dynamic", "static")]
  [string[]] $WindowsSDKLinkModes = @("dynamic", "static"),

  # Build Phases
  [switch] $Toolchain = $true,
  [switch] $Package = $false,

  # Incremental Build Support
  [switch] $Clean,
  [string[]] $Test = @(),

  [switch] $IncludeDS2 = $false,
  [ValidateSet("none", "full", "thin")]
  [string] $LTO = "none",
  [switch] $IncludeNoAsserts = $false,
  [ValidateSet("debug", "release")]
  [string] $FoundationTestConfiguration = "debug",

  [switch] $Summary,
  [switch] $Trace
)

. "$PSScriptRoot\BuildSupport.ps1"

# Validate that if one is set all are set.
if (($PinnedBuild -or $PinnedSHA256 -or $PinnedVersion) -and -not ($PinnedBuild -and $PinnedSHA256 -and $PinnedVersion)) {
  throw "If any of PinnedBuild, PinnedSHA256, or PinnedVersion is set, all three must be set."
}

# Work around limitations of cmd passing in array arguments via powershell.exe -File
if ($AndroidSDKArchitectures.Length -eq 1) { $AndroidSDKArchitectures = $AndroidSDKArchitectures[0].Split(",") }

if ($WindowsSDKArchitectures.Length -eq 1) { $WindowsSDKArchitectures = $WindowsSDKArchitectures[0].Split(",") }

if ($Test.Length -eq 1) { $Test = $Test[0].Split(",") }

if ($Test -contains "*") {
  # Explicitly don't include llbuild yet since tests are known to fail on Windows
  $Test = @("lld", "lldb", "lldb-swift", "swift", "dispatch", "foundation", "xctest", "swift-format", "sourcekit-lsp")
}

if ($UseHostToolchain -is [string]) {
  $UseHostToolchain = [System.Convert]::ToBoolean($UseHostToolchain)
}

# Resolve the architectures received as argument
$AndroidSDKBuilds = @($AndroidSDKArchitectures | ForEach-Object {
  switch ($_) {
    "aarch64" { $KnownPlatforms["AndroidARM64"] }
    "armv7" { $KnownPlatforms["AndroidARMv7"] }
    "i686" { $KnownPlatforms["AndroidX86"] }
    "x86_64" { $KnownPlatforms["AndroidX64"] }
    default { throw "No Android platform for architecture $_" }
  }
})

$WindowsSDKBuilds = @($WindowsSDKArchitectures | ForEach-Object {
  switch ($_) {
    "X64" { $KnownPlatforms["WindowsX64"] }
    "X86" { $KnownPlatforms["WindowsX86"] }
    "Arm64" { $KnownPlatforms["WindowsArm64"] }
    default { throw "No Windows platform for architecture $_" }
  }
})

# The parameters of this script, which the build scripts take by name.
$Parameters = Get-ScriptParameters $PSCommandPath

# The architecture of the Windows SDK that the toolchain is built against.
$HostSDKArchitecture = if ($HostArchName -eq "ARM64") { "Arm64" } else { "X64" }
$SelectedWindowsSDKArchitectures = @(if ($Windows) { $WindowsSDKArchitectures })
$SelectedAndroidSDKArchitectures = @(if ($Android) { $AndroidSDKArchitectures })

$JobLogs = "$BinaryCache\BuildMulti"
$BuildFailure = $null
$Jobs = New-Object System.Collections.ArrayList
$FailedJobs = New-Object System.Collections.ArrayList
# The logs are printed as the jobs wrote them, without decoding them.
$StandardOutput = [Console]::OpenStandardOutput()

# Quotes an argument for a command line in a batch file.
function ConvertTo-CommandLineArgument([string] $Argument) {
  if ($Argument -and $Argument -notmatch '[\s"&|<>^]') {
    return $Argument.Replace("%", "%%")
  }
  $Escaped = ($Argument -replace '(\\*)"', '$1$1\"') -replace '(\\+)$', '$1$1'
  return "`"$($Escaped.Replace("%", "%%"))`""
}

# Starts one of the build scripts in the background. The log of a job with
# -Followed is printed while this script waits for it, instead of once it ends.
function Start-BuildJob([string] $Name, [string] $Script, [Hashtable] $Overrides = @{}, [switch] $BelowNormal, [switch] $Followed) {
  Write-EndedBuildJobs
  $Arguments = @(Get-BuildScriptArguments $Script $Parameters $Overrides)
  $Now = [DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss")
  if ($Followed) {
    Write-Host -ForegroundColor Cyan "[$Now] $Script $Arguments"
  } else {
    Write-Host -ForegroundColor Cyan "[$Now] Starting $Name in the background: $Script $Arguments"
  }

  # cmd.exe runs the script at the requested priority and logs its output.
  $Log = "$JobLogs\$Name.log"
  # The log of an earlier build would be printed until cmd.exe replaces it.
  Remove-Item $Log -ErrorAction Ignore
  $CommandLine = @($PowerShell, "-NoProfile", "-ExecutionPolicy", "RemoteSigned", "-File", "$PSScriptRoot\$Script") + $Arguments
  $Batch = "$JobLogs\$Name.cmd"
  [IO.File]::WriteAllLines($Batch, @(
    "@echo off",
    "start `"`" /b /wait $(if ($BelowNormal) { "/belownormal" }) $(($CommandLine | ForEach-Object { ConvertTo-CommandLineArgument $_ }) -join " ") > `"$Log`" 2>&1",
    "exit /b %errorlevel%"
  ))
  # The script writes its timings there, as with Invoke-BuildScript.
  $TimingDataPath = "$JobLogs\$Name.json"
  Remove-Item $TimingDataPath -ErrorAction Ignore
  $Process = Invoke-IsolatingEnvVars {
    $env:SWIFT_BUILD_TIMINGS_PATH = $TimingDataPath
    Start-Process -FilePath cmd.exe -ArgumentList "/d /c `"$Batch`"" -NoNewWindow -PassThru
  }
  # Windows PowerShell only reports the exit code after the handle was opened.
  $null = $Process.Handle

  $Job = [PSCustomObject]@{
    Name = $Name
    Process = $Process
    Log = $Log
    TimingDataPath = $TimingDataPath
    # The timings of a followed job go to the step that waits for it.
    Timing = if (-not $RecordTimings) { $null } elseif ($Followed) { $script:CurrentOperation } else { Add-TimingData $BuildPlatform $Name -Variant "Background" }
    Followed = [bool] $Followed
    # The bytes of the log that were printed.
    Offset = [long] 0
    Printed = $false
    Stopped = $false
  }
  [void]$Jobs.Add($Job)
  return $Job
}

# Reads the last bytes of the log of a job, while the job may still write it.
function Read-BuildJobLog([PSCustomObject] $Job, [long] $Last) {
  $Stream = [IO.File]::Open($Job.Log, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::ReadWrite)
  try {
    if ($Stream.Length -gt $Last) {
      $null = $Stream.Seek(-$Last, [IO.SeekOrigin]::End)
    }
    return (New-Object IO.StreamReader($Stream, [Text.Encoding]::UTF8, $true)).ReadToEnd()
  } finally {
    $Stream.Dispose()
  }
}

# The last line that a job logged, which shows what it does.
function Get-BuildJobLastLine([PSCustomObject] $Job) {
  try {
    $Lines = @((Read-BuildJobLog $Job 4096) -split "[\r\n]+" | Where-Object { $_.Trim() })
  } catch {
    return ""
  }
  if (-not $Lines) { return "" }
  $Line = $Lines[-1].Trim()
  if ($Line.Length -gt 200) { $Line = "$($Line.Substring(0, 200))..." }
  return $Line
}

# Prints what a job logged since this script last did, up to its last complete
# line unless -All, and returns whether it printed anything.
function Write-BuildJobOutput([PSCustomObject] $Job, [switch] $All) {
  try {
    $Stream = [IO.File]::Open($Job.Log, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::ReadWrite)
  } catch [IO.FileNotFoundException] {
    # cmd.exe did not create it yet.
    return $false
  }
  try {
    $Count = $Stream.Length - $Job.Offset
    if ($Count -le 0) { return $false }
    $null = $Stream.Seek($Job.Offset, [IO.SeekOrigin]::Begin)
    $Bytes = New-Object byte[] $Count
    $Count = $Stream.Read($Bytes, 0, $Count)
  } finally {
    $Stream.Dispose()
  }
  $Length = if ($All) { $Count } elseif ($Count -gt 0) { [Array]::LastIndexOf($Bytes, [byte]10, $Count - 1) + 1 } else { 0 }
  if ($Length -le 0) { return $false }
  $StandardOutput.Write($Bytes, 0, $Length)
  # What this script prints next starts a line.
  if ($Bytes[$Length - 1] -ne 10) {
    $StandardOutput.Write([byte[]](13, 10), 0, 2)
  }
  $StandardOutput.Flush()
  $Job.Offset += $Length
  return $true
}

# Prints the rest of the log of a job, at once for a job that is not followed so
# that it does not interleave with what this script prints.
function Write-BuildJobLog([PSCustomObject] $Job) {
  $Job.Printed = $true
  $Result = if ($Job.Stopped) { "was stopped" } else { "exited with code $($Job.Process.ExitCode)" }
  if ($Job.Followed) {
    $null = Write-BuildJobOutput $Job -All
    if ($Job.Stopped) {
      Write-Host -ForegroundColor Cyan "===== $($Job.Name) $Result ====="
    }
    return
  }
  $Start = $Job.Process.StartTime
  $End = if ($Job.Process.HasExited) { $Job.Process.ExitTime } else { [DateTime]::Now }
  Write-Host -ForegroundColor Cyan "===== $($Job.Name) ($($Start.ToString("HH:mm:ss")) to $($End.ToString("HH:mm:ss")), $($End - $Start)), from $($Job.Log) ====="
  $null = Write-BuildJobOutput $Job -All
  Write-Host -ForegroundColor Cyan "===== $($Job.Name) $Result ====="
}

# Prints the logs of the jobs that ended since this script last did, in the
# order in which they ended, except the followed jobs.
function Write-EndedBuildJobs {
  $Ended = @($Jobs | Where-Object { -not $_.Printed -and -not $_.Followed -and $_.Process.HasExited } | Sort-Object { $_.Process.ExitTime })
  foreach ($Job in $Ended) {
    Write-BuildJobLog $Job
  }
}

# Records the time that a job took, with the timings of the steps of its script.
function Complete-BuildJob([PSCustomObject] $Job) {
  [void]$Jobs.Remove($Job)
  if (-not $Job.Timing) { return }

  # The job may have ended well before this script waited for it.
  $End = if ($Job.Process.HasExited) { $Job.Process.ExitTime } else { [DateTime]::Now }
  $Job.Timing."Elapsed Time" = $End - $Job.Process.StartTime
  Import-TimingData $Job.Timing $Job.TimingDataPath
  # The use of the processors during the steps of the script, for the same reason.
  $Seconds = 0
  $BusySeconds = 0
  foreach ($Step in $Job.Timing.Children) {
    $Seconds += $Step."Elapsed Time".TotalSeconds
    $BusySeconds += $Step.CPU * $Step."Elapsed Time".TotalSeconds
  }
  $Job.Timing.CPU = if ($Seconds -gt 0) { $BusySeconds / $Seconds } else { 0 }
}

# How often this script reports the jobs that it waits for.
$JobHeartbeat = [TimeSpan]::FromMinutes(1)

# Waits for background jobs, printing the log of $Follow as it is written, and
# the logs of the other jobs that end meanwhile, after it so that they do not
# split it, and throws if any of the jobs that it waited for failed. It reports
# the jobs that it waits for when nothing was printed for a while, so that the
# build does not look stuck.
function Wait-BuildJobs([Object[]] $Wait, [PSCustomObject] $Follow = $null) {
  $Heartbeat = [DateTime]::Now + $JobHeartbeat
  while ($true) {
    $Running = @(foreach ($Job in $Wait) { if (-not $Job.Process.HasExited) { $Job } })
    if ($Follow -and (Write-BuildJobOutput $Follow)) {
      $Heartbeat = [DateTime]::Now + $JobHeartbeat
    }
    if (-not $Follow) {
      Write-EndedBuildJobs
    }
    if (-not $Running) { break }

    if ([DateTime]::Now -ge $Heartbeat) {
      foreach ($Job in $Running) {
        $Line = Get-BuildJobLastLine $Job
        Write-Host "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Waiting for $($Job.Name) ($(([DateTime]::Now - $Job.Process.StartTime).ToString("hh\:mm\:ss")))$(if ($Line) { ": $Line" })"
      }
      $Heartbeat = [DateTime]::Now + $JobHeartbeat
    }
    $null = $Running[0].Process.WaitForExit(1000)
  }
  if ($Follow) {
    Write-BuildJobLog $Follow
    Write-EndedBuildJobs
  }

  $Failed = @()
  foreach ($Job in $Wait) {
    Complete-BuildJob $Job
    if ($Job.Process.ExitCode -ne 0) {
      $Failed += $Job.Name
      [void]$FailedJobs.Add($Job)
    }
  }
  if ($Failed) {
    throw "$($Failed -join ", ") failed"
  }
}

# Stops the jobs that are still running after a failure, and prints the logs
# that were not printed yet, as only the output of this script is kept.
function Stop-BuildJobs {
  foreach ($Job in $Jobs.ToArray()) {
    if (-not $Job.Process.HasExited) {
      Write-Host -ForegroundColor Red "Stopping $($Job.Name), which logs to $($Job.Log)"
      $null = & cmd.exe /d /c "taskkill /T /F /PID $($Job.Process.Id) > nul 2>&1"
      $null = $Job.Process.WaitForExit(10000)
      $Job.Stopped = $true
    }
  }
  foreach ($Job in $Jobs.ToArray()) {
    if (-not $Job.Printed) {
      try {
        Write-BuildJobLog $Job
      } catch {
        Write-Host -ForegroundColor Red "Could not print $($Job.Log): $_"
      }
    }
    if (-not $Job.Stopped -and $Job.Process.HasExited -and $Job.Process.ExitCode -ne 0) {
      [void]$FailedJobs.Add($Job)
    }
    Complete-BuildJob $Job
  }
}

# What the build and test tools print about failures.
$FailurePattern = "recorded an issue|failed after .* with \d+ issues?|^error: |: (fatal )?error: |\berror (LNK|C)\d+:|^FAILED: |^(FAIL|XPASS|UNRESOLVED|TIMEOUT): |^Test Case .* failed|^Error: |CMake Error"
# lit lists the tests that failed at the end of its output, under these lines.
$LitListPattern = "^(Failed|Unexpectedly Passed|Unresolved|Timed Out) Tests \(\d+\):$"
$LitResultPattern = "^(FAIL|XPASS|UNRESOLVED|TIMEOUT): (.+) \(\d+ of \d+(, \d+ of \d+ attempts)?\)$"
# lit prints the output of each of these tests between these lines.
$LitOutputPattern = "^\*{20} TEST '(.+)' FAILED \*{20}$"

# Prints the lists of the tests that failed and the lines that report failures
# in the logs of the jobs that failed, with the lines that follow these, as the
# logs may be far above the error that ends the output. Without such lines, it
# prints the end of the log.
function Write-BuildJobFailures {
  foreach ($Job in $FailedJobs) {
    $Name = "$($Job.Name) ($($Job.Process.StartTime.ToString("HH:mm:ss")) to $($Job.Process.ExitTime.ToString("HH:mm:ss")))"
    try {
      # Some tools end lines with \r\r\n.
      $Lines = [regex]::Split([IO.File]::ReadAllText($Job.Log, [Text.Encoding]::UTF8).TrimEnd("`r", "`n"), "\r*\n")
    } catch {
      Write-Host -ForegroundColor Red "===== $Name exited with code $($Job.Process.ExitCode), and its log could not be read: $_"
      continue
    }

    # The tests that lit lists, which are not shown again.
    $Listed = New-Object System.Collections.Generic.HashSet[string]
    for ($Index = 0; $Index -lt $Lines.Count; $Index++) {
      if ($Lines[$Index] -cmatch $LitListPattern) {
        for ($Line = $Index + 1; $Line -lt $Lines.Count -and $Lines[$Line].StartsWith("  "); $Line++) {
          [void]$Listed.Add($Lines[$Line].Trim())
        }
      }
    }

    $Shown = New-Object System.Collections.Generic.List[string]
    $Next = 0
    $Found = 0
    for ($Index = 0; $Index -lt $Lines.Count; $Index++) {
      if ($Lines[$Index] -cmatch $LitListPattern) {
        # The list, with at most 200 tests.
        $End = $Index + 1
        while ($End -lt $Lines.Count -and $Lines[$End].StartsWith("  ") -and $End -le $Index + 200) { $End++ }
        for ($Line = [Math]::Max($Index, $Next); $Line -lt $End; $Line++) {
          $Shown.Add($Lines[$Line])
        }
        if ($End -lt $Lines.Count -and $Lines[$End].StartsWith("  ")) {
          $Shown.Add("  ...")
        }
        $Next = [Math]::Max($Next, $End)
        $Found++
      } elseif ($Lines[$Index] -cmatch $LitOutputPattern -and $Listed.Contains($Matches[1])) {
        # The output of a listed test, which a search for its name finds.
        while ($Index + 1 -lt $Lines.Count -and $Lines[$Index + 1] -cne "********************") { $Index++ }
      } elseif ($Found -lt 50 -and $Lines[$Index] -cmatch $FailurePattern) {
        if ($Lines[$Index] -cmatch $LitResultPattern -and $Listed.Contains($Matches[2])) { continue }
        $Found++
        for ($Line = [Math]::Max($Index, $Next); $Line -le [Math]::Min($Index + 3, $Lines.Count - 1); $Line++) {
          $Shown.Add($Lines[$Line])
        }
        $Next = [Math]::Max($Next, $Index + 4)
      }
    }
    if ($Found) {
      Write-Host -ForegroundColor Red "===== $Name exited with code $($Job.Process.ExitCode), with these failures in its log above ====="
    } else {
      Write-Host -ForegroundColor Red "===== $Name exited with code $($Job.Process.ExitCode), at the end of its log above ====="
      $Shown.AddRange([string[]]@($Lines | Select-Object -Last 20))
    }
    $Bytes = [Text.Encoding]::UTF8.GetBytes(($Shown -join "`r`n") + "`r`n")
    $StandardOutput.Write($Bytes, 0, $Bytes.Length)
    $StandardOutput.Flush()
  }
}

# Runs one of the build scripts like Invoke-BuildScript, but in the background,
# so that this script prints its log as it runs, then the logs of the jobs that
# ended meanwhile.
function Invoke-FollowedBuildScript([string] $Name, [string] $Script, [Hashtable] $Overrides = @{}) {
  Record-OperationTime $BuildPlatform $Name {
    $Job = Start-BuildJob $Name $Script $Overrides -Followed
    Wait-BuildJobs @($Job) $Job
  }
}

try {

New-Item -ItemType Directory -Path $JobLogs -ErrorAction Ignore | Out-Null

Invoke-BuildScript "SetupDependencies" SetupDependencies.ps1 $Parameters

if ($DependenciesOnly) {
  exit 0
}

if ($Clean) {
  Clear-BuildTree ($WindowsSDKBuilds + $AndroidSDKBuilds)
}

if ($Toolchain) {
  Invoke-FollowedBuildScript "BuildToolchain-Bootstrap" BuildToolchain.ps1 @{
    Mode = "Bootstrap";
    WindowsSDKArchitectures = $SelectedWindowsSDKArchitectures;
    AndroidSDKArchitectures = $SelectedAndroidSDKArchitectures;
  }
  Remove-InstalledSDKs $WindowsSDKBuilds
}

# The projects of the SDK install some files to the same paths for every
# architecture, so the SDKs of the other architectures install into their own
# platform roots, merged into the image when they are done.
$SDKJobs = @()
$StagedPlatformRoots = @()
foreach ($Architecture in $SelectedWindowsSDKArchitectures) {
  if ($Architecture -eq $HostSDKArchitecture) { continue }
  $PlatformRoot = "$BinaryCache\SDKStaging\$Architecture\Windows.platform"
  if ($Toolchain) {
    Remove-Item -Force -Recurse $PlatformRoot -ErrorAction Ignore
  }
  $SDKJobs += Start-BuildJob "BuildSDK-Windows-$Architecture" BuildSDK.ps1 -BelowNormal @{
    Platform = "Windows";
    Architectures = @($Architecture);
    SupportedArchitectures = $SelectedWindowsSDKArchitectures;
    LinkModes = $WindowsSDKLinkModes;
    PlatformRoot = $PlatformRoot;
  }
  $StagedPlatformRoots += $PlatformRoot
}

if ($Android) {
  $SDKJobs += Start-BuildJob "BuildSDK-Android" BuildSDK.ps1 -BelowNormal @{
    Platform = "Android";
    Architectures = $AndroidSDKArchitectures;
    LinkModes = $AndroidSDKLinkModes;
  }
}

if ($SelectedWindowsSDKArchitectures -contains $HostSDKArchitecture) {
  # The toolchain builds against the dynamic SDK of the host.
  Invoke-FollowedBuildScript "BuildSDK-Windows-$HostSDKArchitecture" BuildSDK.ps1 @{
    Platform = "Windows";
    Architectures = @($HostSDKArchitecture);
    SupportedArchitectures = $SelectedWindowsSDKArchitectures;
    LinkModes = @($WindowsSDKLinkModes) + @(if ($Toolchain -and $WindowsSDKLinkModes -notcontains "dynamic") { "dynamic" });
  }
} elseif ($Toolchain) {
  Invoke-FollowedBuildScript "BuildSDK-Windows-$HostSDKArchitecture" BuildSDK.ps1 @{
    Platform = "Windows";
    Architectures = @($HostSDKArchitecture);
    LinkModes = @("dynamic");
  }
}

if ($Toolchain) {
  Invoke-FollowedBuildScript "BuildToolchain" BuildToolchain.ps1 @{
    WindowsSDKArchitectures = $SelectedWindowsSDKArchitectures;
    AndroidSDKArchitectures = $SelectedAndroidSDKArchitectures;
  }
}

Wait-BuildJobs $SDKJobs
if ($StagedPlatformRoots) {
  Record-OperationTime $BuildPlatform "Merge-PlatformRoots" {
    foreach ($PlatformRoot in $StagedPlatformRoots) {
      Write-Host "Merging '$PlatformRoot' into '$(Get-PlatformRoot Windows)'"
      & robocopy.exe $PlatformRoot (Get-PlatformRoot Windows) /E /NFL /NDL /NJH /NJS /NP | Out-Null
      # robocopy reports copied files with exit codes below 8.
      if ($LastExitCode -ge 8) {
        throw "robocopy failed with exit code $LastExitCode"
      }
    }
  }
}

# The tests without -Toolchain use the toolchain in the image, which is bound to
# the runtime again, as the SDK may have been built again since.
if ($Test -and -not $Toolchain -and -not $IsCrossCompiling) {
  Invoke-FollowedBuildScript "BuildToolchain-Bind" BuildToolchain.ps1 @{
    Mode = "Bind";
  }
}

if ($Package) {
  Invoke-FollowedBuildScript "BuildInstaller" BuildInstaller.ps1 @{
    WindowsSDKArchitectures = $SelectedWindowsSDKArchitectures;
    AndroidSDKArchitectures = $SelectedAndroidSDKArchitectures;
  }
} elseif ($IncludeSBoM) {
  Write-Warning "The SBoM is only generated with the installer, with -Package."
}

if ($Test -and -not $IsCrossCompiling) {
  $CompilerTests = @($Test | Where-Object { $_ -in "clang", "lld", "lldb", "lldb-swift", "llvm", "swift" })
  $PackageTests = @($Test | Where-Object { $_ -in "foundation", "swift-format", "sourcekit-lsp", "swiftpm", "llbuild" })
  # The xctest tests use the FileCheck that the compiler tests build, and the
  # libdispatch that the dispatch tests rebuild.
  $OtherTests = @($Test | Where-Object { $_ -notin $CompilerTests -and $_ -notin $PackageTests })

  # The package tests build in their own trees, with the toolchain of the image.
  $TestJobs = @()
  if ($PackageTests) {
    $TestJobs += Start-BuildJob "TestToolchain-Packages" TestToolchain.ps1 -BelowNormal @{
      Test = $PackageTests;
    }
  }

  # The compiler suites build what they need in the same tree, so this happens
  # once before they run.
  if ($CompilerTests) {
    Invoke-FollowedBuildScript "TestToolchain-Build" TestToolchain.ps1 @{
      Test = $CompilerTests;
      Steps = @("Build");
    }
  }

  $Tests = @($CompilerTests | Where-Object { $_ -ne "swift" }) + $OtherTests
  if ($Tests) {
    $TestJobs += Start-BuildJob "TestToolchain-Others" TestToolchain.ps1 @{
      Test = $Tests;
      Steps = @("Run");
    }
  }

  # Run check-swift, the longest suite, here, and report all the failures.
  $Failure = $null
  if ($CompilerTests -contains "swift") {
    try {
      Invoke-FollowedBuildScript "TestToolchain-swift" TestToolchain.ps1 @{
        Test = @("swift");
        Steps = @("Run");
      }
    } catch {
      $Failure = $_
    }
  }
  Wait-BuildJobs $TestJobs
  if ($Failure) {
    throw $Failure
  }

  # TODO: restore Android Swift runtime tests against the new Runtimes/* layout.
  # The previous `Test-Runtime` reconfigured the in-tree stdlib build (built by
  # the now-removed `Build-LegacySDK` -> `Build-Runtime`) with
  # `SWIFT_INCLUDE_TESTS=YES` and ran `check-swift-validation-only_non_executable`.
  # The new SDK build path (Runtimes/*) does not expose that test target, so a
  # straight re-point is not possible.  Wiring this back will likely require
  # either plumbing `SWIFT_INCLUDE_TESTS` into the new runtime CMake or driving
  # the lit suite from a separate test-only build.  Until then, `-Test swift`
  # for Android is silently a no-op here.
  if ($Test -contains "swift" -and $Android) {
    Write-Warning "Android Swift runtime tests are not currently wired up to the new SDK layout; skipping."
  }
}

} catch {
  Stop-BuildJobs
  $BuildFailure = $_
} finally {
  if ($Summary) {
    Write-Summary
  }
  if ($Trace) {
    # Stage the trace with the other CI artifacts so that it can be downloaded.
    $TimingTraceDir = if ($Stage) { $Stage } else { $BinaryCache }
    Write-TimingTrace "$TimingTraceDir\build-timings.json" -TrackPerStep
  }
}

# The failures end the output, where they are easy to find.
if ($BuildFailure) {
  Write-BuildJobFailures
  Write-BuildError $BuildFailure
  exit 1
}
