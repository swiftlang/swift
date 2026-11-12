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
- The installer builds in the background while the tests run.

The background processes run at a lower priority and log to
<BinaryCache>\BuildMulti\<name>.log, which this script prints when they end.

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
$Jobs = New-Object System.Collections.ArrayList

# Quotes an argument for a command line in a batch file.
function ConvertTo-CommandLineArgument([string] $Argument) {
  if ($Argument -and $Argument -notmatch '[\s"&|<>^]') {
    return $Argument.Replace("%", "%%")
  }
  $Escaped = ($Argument -replace '(\\*)"', '$1$1\"') -replace '(\\+)$', '$1$1'
  return "`"$($Escaped.Replace("%", "%%"))`""
}

# Starts one of the build scripts in the background.
function Start-BuildJob([string] $Name, [string] $Script, [Hashtable] $Overrides = @{}, [switch] $BelowNormal) {
  $Arguments = @(Get-BuildScriptArguments $Script $Parameters $Overrides)
  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Starting $Name in the background: $Script $Arguments"

  # cmd.exe runs the script at the requested priority and logs its output.
  $Log = "$JobLogs\$Name.log"
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
    Timing = if ($RecordTimings) { Add-TimingData $BuildPlatform $Name -Variant "Background" } else { $null }
  }
  [void]$Jobs.Add($Job)
  return $Job
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

# Waits for background jobs, prints their logs, and throws if any of them failed.
function Wait-BuildJobs([Object[]] $Wait) {
  $Failed = @()
  foreach ($Job in $Wait) {
    $Job.Process.WaitForExit()
    Complete-BuildJob $Job

    $Elapsed = $Job.Process.ExitTime - $Job.Process.StartTime
    Write-Host -ForegroundColor Cyan "===== $($Job.Name) ($Elapsed), from $($Job.Log) ====="
    [Console]::Out.Write([IO.File]::ReadAllText($Job.Log))
    [Console]::Out.Flush()
    Write-Host -ForegroundColor Cyan "===== $($Job.Name) exited with code $($Job.Process.ExitCode) ====="
    if ($Job.Process.ExitCode -ne 0) {
      $Failed += $Job.Name
    }
  }
  if ($Failed) {
    throw "$($Failed -join ", ") failed"
  }
}

# Stops the jobs that are still running after a failure.
function Stop-BuildJobs {
  foreach ($Job in $Jobs.ToArray()) {
    if (-not $Job.Process.HasExited) {
      Write-Host -ForegroundColor Red "Stopping $($Job.Name), which logs to $($Job.Log)"
      $null = & cmd.exe /d /c "taskkill /T /F /PID $($Job.Process.Id) > nul 2>&1"
    }
    Complete-BuildJob $Job
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
  Invoke-BuildScript "BuildToolchain-Bootstrap" BuildToolchain.ps1 $Parameters @{
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
  Invoke-BuildScript "BuildSDK-Windows-$HostSDKArchitecture" BuildSDK.ps1 $Parameters @{
    Platform = "Windows";
    Architectures = @($HostSDKArchitecture);
    SupportedArchitectures = $SelectedWindowsSDKArchitectures;
    LinkModes = $WindowsSDKLinkModes;
  }
} elseif ($Toolchain) {
  Invoke-BuildScript "BuildSDK-Windows-$HostSDKArchitecture" BuildSDK.ps1 $Parameters @{
    Platform = "Windows";
    Architectures = @($HostSDKArchitecture);
    LinkModes = @("dynamic");
  }
}

if ($Toolchain) {
  Invoke-BuildScript "BuildToolchain" BuildToolchain.ps1 $Parameters @{
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

# The installer does not change the image, so it builds while the tests run.
$InstallerJobs = @()
if ($Package) {
  $InstallerJobs += Start-BuildJob "BuildInstaller" BuildInstaller.ps1 -BelowNormal @{
    WindowsSDKArchitectures = $SelectedWindowsSDKArchitectures;
    AndroidSDKArchitectures = $SelectedAndroidSDKArchitectures;
  }
} elseif ($IncludeSBoM) {
  Write-Warning "The SBoM is only generated with the installer, with -Package."
}

$TestJobs = @()
$Failure = $null
if ($Test -and -not $IsCrossCompiling) {
  $CompilerTests = @($Test | Where-Object { $_ -in "clang", "lld", "lldb", "lldb-swift", "llvm", "swift" })
  $PackageTests = @($Test | Where-Object { $_ -in "foundation", "swift-format", "sourcekit-lsp", "swiftpm", "llbuild" })
  # The xctest tests use the FileCheck that the compiler tests build, and the
  # libdispatch that the dispatch tests rebuild.
  $OtherTests = @($Test | Where-Object { $_ -notin $CompilerTests -and $_ -notin $PackageTests })

  # The package tests build in their own trees, with the toolchain of the image.
  if ($PackageTests) {
    $TestJobs += Start-BuildJob "TestToolchain-Packages" TestToolchain.ps1 -BelowNormal @{
      Test = $PackageTests;
    }
  }

  # The compiler suites build what they need in the same tree, so this happens
  # once before they run.
  if ($CompilerTests) {
    Invoke-BuildScript "TestToolchain-Build" TestToolchain.ps1 $Parameters @{
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
  if ($CompilerTests -contains "swift") {
    try {
      Invoke-BuildScript "TestToolchain-swift" TestToolchain.ps1 $Parameters @{
        Test = @("swift");
        Steps = @("Run");
      }
    } catch {
      $Failure = $_
    }
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

Wait-BuildJobs ($TestJobs + $InstallerJobs)
if ($Failure) {
  throw $Failure
}

} catch {
  Stop-BuildJobs
  Write-BuildError $_
  exit 1
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
