# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

<#
.SYNOPSIS
Builds the Swift toolchain, installers, and optionally runs tests.

.DESCRIPTION
This script builds the Swift toolchain by running these scripts one after the
other:

- SetupDependencies.ps1 downloads the tools that the build uses.
- BuildToolchain.ps1 -Mode Bootstrap builds the Stage1 toolchain.
- BuildSDK.ps1 builds the Windows SDK of the host, which the toolchain is built
  against, and the SDKs selected with -Windows and -Android.
- BuildToolchain.ps1 builds the toolchain, and installs it under
  S:\Program Files for local toolchain use.
- BuildInstaller.ps1 builds the msi's and installer executable, and stages the
  build artifacts for CI.
- TestToolchain.ps1 runs the tests for supported projects.

BuildMulti.ps1 takes the same parameters and runs the same scripts, some of them
at the same time.

.PARAMETER SourceCache
The path to a directory where projects contributing to the Swift toolchain have
been cloned.
Default: 'S:\SourceCache'

.PARAMETER BinaryCache
The path to a directory where to write build system files and outputs.
Default: 'S:\b'

.PARAMETER ArtifactCache
The path to a directory containing downloaded build artifacts that can be
shared by multiple build trees.
Default: 'S:\ArtifactCache'

.PARAMETER BuildRoot
The path to the merged file system image populated by the build. The
"Program Files" subdirectories will be created beneath this directory.
ImageRoot is retained as a compatibility alias.
Default: 'S:\'

.PARAMETER ObjectStore
The path to the content-addressed object store used for build caching.
Cache is retained as a compatibility alias.
Default: 'S:\ObjectStore'

.PARAMETER Stage
The path to a directory where built msi's and the installer executable should be
staged (for CI). Leave empty for local development builds.

.PARAMETER PinnedBuild
The pinned bootstrap Swift toolchain used to build the Swift components with.

.PARAMETER PinnedSHA256
The SHA256 for the pinned toolchain.

.PARAMETER EnableCaching
Enable build caching using LLVM CAS to speed up rebuilds.

.PARAMETER IncludeSBoM
Include Software Bill of Materials generation using syft. Used for compliance
tracking.

.PARAMETER DownloadRetryCount
The number of attempts to make when downloading a dependency before giving up.
Default: 3

.PARAMETER ArtifactLockTimeoutSeconds
The maximum time to wait for another process to release an artifact lock.
Default: 1800

.PARAMETER ProductVersion
The product version to be used when building the installer. Supports semantic
version strings (e.g., "1.0.0"). Default: "0.0.0"

.PARAMETER WindowsSxSAssemblyPublicKeyToken
The public key token to embed in Windows side-by-side assembly manifests.
This is identity metadata only; it does not sign binaries or catalogs.
Default: "0000000000000000"

.PARAMETER ToolchainIdentifier
The toolchain version identifier for the toolchain being built.
Default: Uses TOOLCHAIN_VERSION environment variable or "$USERNAME.development"

.PARAMETER HostArchName
The architecture where the toolchain will execute. Automatically detected from
system.  Valid values: AMD64, ARM64

.PARAMETER DebugInfo
Include debug information in the builds. Useful for debugging the toolchain
itself.
Note: This significantly increases build time and disk usage.

.PARAMETER Android
Build Android SDKs. Requires Android NDK to be available.

.PARAMETER AndroidNDKVersion
The version number of the Android NDK to be used.
Format: r{number}[{letter}][-revision-suffix] (e.g., r28c or r30-beta2)
Default: "r30"

.PARAMETER AndroidAPILevel
The API Level to target when building the Android SDKs. Must be between 21 and 36.
Default: 23

.PARAMETER AndroidSDKArchitectures
An array of architectures for which the Android Swift SDK should be built.
Default: @("aarch64", "armv7", "i686", "x86_64")

.PARAMETER Windows
Build Windows SDKs.

.PARAMETER WinSDKVersion
The version number of the Windows SDK to be used.
Overrides the value resolved by the Visual Studio command prompt.
If no such Windows SDK is installed, it will be downloaded from nuget.

.PARAMETER WindowsSDKArchitectures
An array of architectures for which the Windows Swift SDK should be built.
Default: @("X64","X86","ARM64")

.PARAMETER Clean
Remove selected build outputs before building.

.PARAMETER Toolchain
Build the toolchain.

.PARAMETER Package
Build the MSI installers and packaging.

.PARAMETER Test
An array of names of projects to run tests for. Use '*' to run all tests.
Available tests: lld, lldb, lldb-swift, swift, dispatch, foundation, xctest, swift-format, sourcekit-lsp

.PARAMETER IncludeDS2
Include the ds2 remote debug server in the SDK.
This component is currently only supported in Android builds.

.PARAMETER IncludeNoAsserts
Build and include the no-assert toolchain variant in the output.

.PARAMETER Summary
Display a build time summary at the end of the build. Helpful for performance analysis.

.PARAMETER Trace
Write the build timings to build-timings.json in the Stage directory (or BinaryCache
if Stage is empty) in the Chrome Trace Event format, which can be viewed in
https://ui.perfetto.dev or chrome://tracing.

.EXAMPLE
PS> .\Build.ps1

.EXAMPLE
PS> .\Build.ps1 -WindowsSDKArchitectures x64 -ProductVersion 1.2.3 -Test foundation,xctest
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

try {

if ($Trace) {
  Remove-Item "$BinaryCache\build-timings-*.json" -ErrorAction Ignore
}

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

  if ($SelectedWindowsSDKArchitectures -notcontains $HostSDKArchitecture) {
    Invoke-BuildScript "BuildSDK-Windows-$HostSDKArchitecture" BuildSDK.ps1 $Parameters @{
      Platform = "Windows";
      Architectures = @($HostSDKArchitecture);
      LinkModes = @("dynamic");
    }
  }
}

if ($Windows) {
  Invoke-BuildScript "BuildSDK-Windows" BuildSDK.ps1 $Parameters @{
    Platform = "Windows";
    Architectures = $WindowsSDKArchitectures;
    LinkModes = $WindowsSDKLinkModes;
  }
}

if ($Android) {
  Invoke-BuildScript "BuildSDK-Android" BuildSDK.ps1 $Parameters @{
    Platform = "Android";
    Architectures = $AndroidSDKArchitectures;
    LinkModes = $AndroidSDKLinkModes;
  }
}

if ($Toolchain) {
  Invoke-BuildScript "BuildToolchain" BuildToolchain.ps1 $Parameters @{
    WindowsSDKArchitectures = $SelectedWindowsSDKArchitectures;
    AndroidSDKArchitectures = $SelectedAndroidSDKArchitectures;
  }
}

if ($Package) {
  Invoke-BuildScript "BuildInstaller" BuildInstaller.ps1 $Parameters @{
    WindowsSDKArchitectures = $SelectedWindowsSDKArchitectures;
    AndroidSDKArchitectures = $SelectedAndroidSDKArchitectures;
  }
} elseif ($IncludeSBoM) {
  Write-Warning "The SBoM is only generated with the installer, with -Package."
}

if ($Test -and -not $IsCrossCompiling) {
  Invoke-BuildScript "TestToolchain" TestToolchain.ps1 $Parameters

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
  Write-BuildError $_
  exit 1
} finally {
  Write-BuildTimings "BuildSequential"
  if ($Trace) {
    # Stage the trace with the other CI artifacts so that it can be downloaded.
    $TimingTraceDir = if ($Stage) { $Stage } else { $BinaryCache }
    Merge-TimingTraces "$TimingTraceDir\build-timings.json"
  }
}
