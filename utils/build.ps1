# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

<#
.SYNOPSIS
Builds the Swift toolchain, installers, and optionally runs tests.

.DESCRIPTION
This script performs various steps associated with building the Swift toolchain:

- Builds the redistributable, SDK, devtools and toolchain binaries and files
- Builds the msi's and installer executable
- Creates a mock installation under S:\Program Files and S:\Library for local toolchain use
- Optionally runs tests for supported projects
- Optionally stages build artifacts for CI

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

  # Dependency Download Retries
  [ValidateRange(1, [int]::MaxValue)]
  [int] $DownloadRetryCount = 3,

  # Artifact Lock Timeout
  [ValidateRange(1, [int]::MaxValue)]
  [int] $ArtifactLockTimeoutSeconds = 1800,

  # Dependencies
  [ValidatePattern('^\d+(\.\d+)*$')]
  [string] $PythonVersion = "3.10.1",

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

  [switch] $Summary
)

# build-sdk.ps1 starts from the same parameters.
$BuildParameters = @{}
foreach ($Parameter in $MyInvocation.MyCommand.Parameters.Values) {
  if ([Management.Automation.Cmdlet]::CommonParameters.Contains($Parameter.Name) -or
      [Management.Automation.Cmdlet]::OptionalCommonParameters.Contains($Parameter.Name)) {
    continue
  }
  $Value = Get-Variable -Name $Parameter.Name -ValueOnly
  if ($Value -is [IO.FileSystemInfo]) { $Value = $Value.ToString() }
  if ($Value -is [switch]) { $Value = $Value.IsPresent }
  $BuildParameters[$Parameter.Name] = $Value
}

. "$PSScriptRoot\build-common.ps1"

# Builds the Windows SDK of one architecture into the SDK named $SDKIdentifier.
function Build-WindowsSDK([Hashtable] $Platform, [string] $SDKIdentifier) {
  $ParametersPath = "$BinaryCache\build-sdk.$PID.xml"
  $BuildParameters | Export-Clixml -Path $ParametersPath
  try {
    Invoke-Program (Get-Process -Id $PID).Path -NoProfile -ExecutionPolicy (Get-ExecutionPolicy) `
      -File "$PSScriptRoot\build-sdk.ps1" -ParametersPath $ParametersPath `
      -Architecture $Platform.Architecture.LLVMName -SDKIdentifier $SDKIdentifier
  } finally {
    Remove-Item -ErrorAction Ignore $ParametersPath
  }
}

# Starts build-sdk.ps1 for the Windows SDK of one architecture, without waiting
# for it.
function Start-WindowsSDK([Hashtable] $Platform, [string] $SDKIdentifier) {
  $Log = "$BinaryCache\build-sdk.$PID.$($Platform.Architecture.LLVMName)"
  $BuildParameters | Export-Clixml -Path "$Log.xml"
  $Arguments = "-NoProfile -ExecutionPolicy $(Get-ExecutionPolicy) -File `"$PSScriptRoot\build-sdk.ps1`" " +
    "-ParametersPath `"$Log.xml`" -Architecture $($Platform.Architecture.LLVMName) -SDKIdentifier $SDKIdentifier"
  $Process = Start-Process -PassThru -NoNewWindow -FilePath (Get-Process -Id $PID).Path -ArgumentList $Arguments `
    -RedirectStandardOutput "$Log.out" -RedirectStandardError "$Log.err"
  # Windows PowerShell only reports the exit code of a process whose handle was
  # opened before it exited.
  $null = $Process.Handle
  # The toolchain build keeps priority over the SDK builds.
  $Process.PriorityClass = [Diagnostics.ProcessPriorityClass]::BelowNormal
  return @{ SDKIdentifier = $SDKIdentifier; Process = $Process; Log = $Log }
}

# Waits for the SDK builds that Start-WindowsSDK started and prints their output.
function Wait-WindowsSDK([Hashtable[]] $Builds) {
  Record-OperationTime $HostPlatform "Wait-WindowsSDK" {
    foreach ($Build in $Builds) {
      $Build.Process.WaitForExit()
    }
  }
  foreach ($Build in $Builds) {
    Write-Host -ForegroundColor Cyan "----- Output of build-sdk.ps1 for the $($Build.SDKIdentifier) SDK -----"
    Get-Content -ErrorAction Ignore "$($Build.Log).out", "$($Build.Log).err" | Write-Host
    Remove-Item -ErrorAction Ignore "$($Build.Log).*"
  }
  foreach ($Build in $Builds) {
    if ($Build.Process.ExitCode -ne 0) {
      throw "build-sdk.ps1 failed to build the $($Build.SDKIdentifier) SDK with exit code $($Build.Process.ExitCode)."
    }
  }
}

$WindowsSDKProcesses = @()

#-------------------------------------------------------------------

try {

Get-Dependencies

if ($Clean) {
  # Fail instead of leaving a partially-cleaned tree behind.
  function Clean-Path([string] $Path) {
    if (-not (Test-Path -LiteralPath $Path)) { return }
    try {
      [System.IO.Directory]::Delete("\\?\$Path", $true)
    } catch {
      throw "Failed to clean '$Path': $_"
    }
  }

  Clean-Path "$BinaryCache\$($BuildPlatform.Triple)"
  Clean-Path "$BinaryCache\$($HostPlatform.Triple)"
  foreach ($Build in $WindowsSDKBuilds) {
    Clean-Path "$BinaryCache\$($Build.Triple)"
  }
  foreach ($Build in $AndroidSDKBuilds) {
    Clean-Path "$BinaryCache\$($Build.Triple)"
  }
  Clean-Path "$BinaryCache\1"
  Clean-Path "$BinaryCache\5"
  Clean-Path (Get-InstallDir $HostPlatform)

  Get-SelectedSDKBuilds | ForEach-Object {
    Clean-Path (Get-ProjectBinaryCache $_ ClangBuiltins)
    Clean-Path (Get-ProjectBinaryCache $_ ClangRuntime)
  }
}

if ($Toolchain) {
  Remove-Item -Force -Recurse ([IO.Path]::Combine((Get-InstallDir $HostPlatform), "Platforms")) -ErrorAction Ignore
  if ($HostPlatform.OS -eq [OS]::Windows) {
    (@($HostPlatform) + @($WindowsSDKBuilds)) | ForEach-Object {
      Remove-Item -Force -Recurse (Get-WindowsRuntimeInstallRoot $_) -ErrorAction Ignore
    }
  }

  # ── Build Tools ───────────────────────────────────────────────────────────
  Invoke-BuildStep Build-CMark $BuildPlatform
  Invoke-BuildStep Build-BuildTools $BuildPlatform

  # ── Early Swift Driver ────────────────────────────────────────────────────
  Invoke-BuildStep Build-SQLite $BuildPlatform -CCompiler $Compilers.Host.C -Phase EarlySwiftDriver
  Invoke-BuildStep Build-EarlySwiftDriver $BuildPlatform

  # ── Stage1 Compiler ───────────────────────────────────────────────────────
  Invoke-BuildStep Build-XML2 $BuildPlatform -CCompiler $Compilers.Host.C -CXXCompiler $Compilers.Host.CXX -Phase "Bootstrap"
  Invoke-BuildStep Build-Compilers $BuildPlatform -Variant "Asserts" -Project Stage1Compilers @{
    CacheScript     = "$SourceCache\swift\cmake\caches\Windows-Bootstrap-Stage1-$($BuildPlatform.Architecture.LLVMName).cmake";
    Assembler       = $Assemblers.Host;
    CCompiler       = $Compilers.Host.C;
    CXXCompiler     = $Compilers.Host.CXX;
    SwiftCompiler   = $Compilers.Pinned.Swift;
    SwiftSDK        = Get-PinnedToolchainSDK -OS $BuildPlatform.OS;
    ToolchainRoot   = Get-ProjectToolchainRoot $BuildPlatform Stage1Compilers;
    RuntimeLocation = Get-PinnedToolchainToolsDir;
  }

  # ── Host Platform SDK ─────────────────────────────────────────────────────
  Invoke-BuildStep Build-BootstrapFoundationMacros $BuildPlatform @{
    SwiftCompiler   = $Compilers.Pinned.Swift;
    SwiftSDK        = Get-PinnedToolchainSDK -OS $BuildPlatform.OS;
    SwiftSyntax_DIR = (Get-ProjectCMakeModules $BuildPlatform Stage1Compilers);
  }
  Invoke-BuildStep Build-BootstrapTestingMacros $BuildPlatform @{
    SwiftCompiler   = $Compilers.Pinned.Swift;
    SwiftSDK        = Get-PinnedToolchainSDK -OS $BuildPlatform.OS;
    SwiftSyntax_DIR = (Get-ProjectCMakeModules $BuildPlatform Stage1Compilers);
  }
}

# The Windows SDKs of the other architectures only need the Stage1 compilers,
# so they build alongside the rest of the toolchain, each into its own SDK, and
# are merged into the Windows SDK below.
if ($Windows) {
  Sync-Runtimes
  $WindowsSDKProcesses = @($WindowsSDKBuilds | Where-Object { $_ -ne $HostPlatform } | ForEach-Object {
    Start-WindowsSDK $_ -SDKIdentifier "Windows-$($_.Architecture.LLVMName)"
  })
}

if ($Toolchain) {
  Invoke-BuildStep Build-SDK $HostPlatform -Context @{
    SDKIdentifier        = $HostPlatform.OS.ToString();
    Variant              = "Dynamic";
    Compilers            = $Compilers.Stage1;
    Static               = $false;
    BuildFoundation      = $true;
    SupplementalRuntimes = $SDKSupplementalRuntimes;
  }

  # ── Stage2 Compiler ───────────────────────────────────────────────────────
  Invoke-BuildStep Build-CMark $HostPlatform
  Invoke-BuildStep Build-XML2 $HostPlatform -CCompiler $Compilers.Stage1.C -CXXCompiler $Compilers.Stage1.CXX -Phase "Compiler"
  Invoke-BuildStep Build-Compilers $HostPlatform -Variant "Asserts" -Project Stage2Compilers @{
    Assembler       = $Assemblers.Stage1;
    CCompiler       = $Compilers.Stage1.C;
    CXXCompiler     = $Compilers.Stage1.CXX;
    SwiftCompiler   = $Compilers.Stage1.Swift;
    SwiftSDK        = Get-SwiftSDK -OS $HostPlatform.OS;
    DispatchPackage = Get-ProjectCMakeModules $HostPlatform DynamicCDispatch;
  }
  Invoke-BuildStep Write-ToolchainInfo $HostPlatform -Variant "Asserts"
  Invoke-BuildStep Write-PlatformInfoPlist $HostPlatform

  # ── Stage2 Compiler Runtimes ──────────────────────────────────────────────
  Get-SelectedSDKBuilds | ForEach-Object {
    Invoke-BuildStep Build-CompilerRuntime $_ -Assembler $Assemblers.Stage1 -Compilers $Compilers.Stage1
  }

  # ── Stage2 Compiler Macros ────────────────────────────────────────────────
  Invoke-BuildStep Build-FoundationMacros $HostPlatform @{
    SwiftCompiler   = $Compilers.Stage1.Swift;
    SwiftSDK        = Get-SwiftSDK -OS $HostPlatform.OS;
    SwiftSyntax_DIR = Get-ProjectCMakeModules $HostPlatform Stage2Compilers;
  }
  Invoke-BuildStep Build-TestingMacros $HostPlatform @{
    SwiftCompiler   = $Compilers.Stage1.Swift;
    SwiftSDK        = Get-SwiftSDK -OS $HostPlatform.OS;
    SwiftSyntax_DIR = Get-ProjectCMakeModules $HostPlatform Stage2Compilers;
  }

  # ── Stage2 Toolchain ──────────────────────────────────────────────────────
  Invoke-BuildStep Build-SQLite $HostPlatform -CCompiler $Compilers.Stage1.C -Phase ""
  Invoke-BuildStep Build-ToolsSupportCore $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-LLBuild $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-ArgumentParser $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-Driver $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;

    LLVM_DIR  = "$(Get-ProjectBinaryCache $HostPlatform Stage2Compilers)\lib\cmake\llvm";
    Clang_DIR = "$(Get-ProjectBinaryCache $HostPlatform Stage2Compilers)\lib\cmake\clang";
    Swift_DIR = "$(Get-ProjectBinaryCache $HostPlatform Stage2Compilers)\tools\swift\lib\cmake\swift";
  }
  Invoke-BuildStep Build-ASN1 $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-Crypto $HostPlatform @{
    Assembler = $Assemblers.Stage1;
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-Collections $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-Certificates $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-System $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-Subprocess $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-ToolsProtocols $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-Build $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-PackageManager $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
    SwiftSyntax_DIR = Get-ProjectCMakeModules $HostPlatform Stage2Compilers;
  }
  Invoke-BuildStep Build-Markdown $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-Format $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
    SwiftSyntax_DIR = Get-ProjectCMakeModules $HostPlatform Stage2Compilers;
  }
  Invoke-BuildStep Build-LMDB $HostPlatform -CCompiler $Compilers.Stage1.C
  Invoke-BuildStep Build-IndexStoreDB $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-SymbolKit $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-DocC $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK = Get-SwiftSDK -OS $HostPlatform.OS;
  }
  Invoke-BuildStep Build-SourceKitLSP $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
    SwiftSyntax_DIR = Get-ProjectCMakeModules $HostPlatform Stage2Compilers;
  }
  Invoke-BuildStep Build-Inspect $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }

  Repair-Toolchain $HostPlatform.ToolchainInstallRoot

  # ── Stage2 NoAsserts Compiler ─────────────────────────────────────────────
  if ($IncludeNoAsserts) {
    Invoke-BuildStep Build-Compilers $HostPlatform -Variant "NoAsserts" -Project Stage2Compilers @{
      Assembler       = $Assemblers.Stage1;
      CCompiler       = $Compilers.Stage1.C;
      CXXCompiler     = $Compilers.Stage1.CXX;
      SwiftCompiler   = $Compilers.Stage1.Swift;
      SwiftSDK        = Get-SwiftSDK -OS $HostPlatform.OS;
      DispatchPackage = Get-ProjectCMakeModules $HostPlatform DynamicCDispatch;
    }

    Repair-Toolchain $HostPlatform.NoAssertsToolchainInstallRoot

    # Only compilers have NoAsserts enabled. Copy the rest of the Toolchain binaries from the Asserts output
    # Use robocopy for efficient copying
    #   /E : Copies subdirectories, including empty ones.
    #   /XC: Excludes existing files with the same timestamp but different file sizes.
    #   /XN: Excludes existing files that are newer than the copy in the source directory.
    #   /XO: Excludes existing files that are older than the copy in the source directory.
    #   /NFL: Do not list copied files in output
    #   /NDL: Do not list directories in output
    #   /NJH: Do not write a job header
    #   /NC: Do not write file classes
    #   /NS: Do not write file sizes
    #   /NP: Do not show progress indicator
    & robocopy $HostPlatform.ToolchainInstallRoot $HostPlatform.NoAssertsToolchainInstallRoot /E /XC /XN /XO /NS /NC /NFL /NDL /NJH
    Invoke-BuildStep Write-ToolchainInfo $HostPlatform -Variant "NoAsserts"
  }

  if ($HostPlatform.OS -eq [OS]::Windows) {
    $HostSDKRoot = Get-SwiftSDK -OS $HostPlatform.OS
    $HostRuntimeBin = Get-SDKRuntimeBin $HostPlatform $HostSDKRoot
    Invoke-BuildStep Stage-WindowsToolchainSxS $HostPlatform @{
      ToolchainRoot   = $HostPlatform.ToolchainInstallRoot;
      RuntimeLocation = $HostRuntimeBin;
    }
    if ($IncludeNoAsserts) {
      Invoke-BuildStep Stage-WindowsToolchainSxS $HostPlatform @{
        ToolchainRoot   = $HostPlatform.NoAssertsToolchainInstallRoot;
        RuntimeLocation = $HostRuntimeBin;
      }
    }
  }
}

if ($IncludeDS2) {
  Invoke-BuildStep Build-RegsGen2 $BuildPlatform
}

if ($Windows) {
  $SDKROOT = Get-SwiftSDK -OS Windows
  if ($WindowsSDKBuilds -contains $HostPlatform) {
    Invoke-BuildStep Build-WindowsSDK $HostPlatform -SDKIdentifier "Windows"
  }
  Wait-WindowsSDK $WindowsSDKProcesses
  foreach ($Build in $WindowsSDKBuilds) {
    if ($Build -ne $HostPlatform) {
      $ArchitectureSDK = Get-SwiftSDK -OS Windows -Identifier "Windows-$($Build.Architecture.LLVMName)"
      Copy-Directory "$ArchitectureSDK\*" $SDKROOT
      Remove-Item -Force -Recurse $ArchitectureSDK
    }
  }

  Install-SDK $WindowsSDKBuilds
  Write-SDKSettings Windows

  Write-PlatformInfoPlist Windows

  Invoke-BuildStep Build-PackageManagerRuntime $HostPlatform @{
    Compilers = $Compilers.Stage1;
    SwiftSDK  = Get-SwiftSDK -OS $HostPlatform.OS;
  }

  # Copy static dependencies
  foreach ($Build in $WindowsSDKBuilds) {
    if (-not $Build.LinkModes.Contains("static")) { continue }

    $SwiftResourceDir = "${SDKROOT}\usr\lib\swift_static\$($Build.OS.ToString().ToLowerInvariant())\$($Build.Architecture.LLVMName)"
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build brotli)\brotlicommon.lib" -Destination "${SwiftResourceDir}\brotlicommon.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build brotli)\brotlidec.lib" -Destination "${SwiftResourceDir}\brotlidec.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build CURL)\lib\libcurl.lib" -Destination "${SwiftResourceDir}\libcurl.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build XML2)\libxml2s.lib" -Destination "${SwiftResourceDir}\libxml2s.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build ZLib)\zlibstatic.lib" -Destination "${SwiftResourceDir}\zlibstatic.lib" | Out-Null
  }

  $RebuiltHostDynamicRuntime = @(
    $WindowsSDKBuilds | Where-Object {
      $_ -eq $HostPlatform -and $_.LinkModes.Contains("dynamic")
    }
  ).Count -gt 0
  # If -Windows rebuilds the host dynamic runtime, refresh the private SxS
  # copies after the final runtime image is in place.
  if ($Toolchain -and $RebuiltHostDynamicRuntime) {
    $HostSDKRoot = Get-SwiftSDK -OS $HostPlatform.OS
    $HostRuntimeBin = Get-SDKRuntimeBin $HostPlatform $HostSDKRoot
    Invoke-BuildStep Stage-WindowsToolchainSxS $HostPlatform @{
      ToolchainRoot   = $HostPlatform.ToolchainInstallRoot;
      RuntimeLocation = $HostRuntimeBin;
    }
    if ($IncludeNoAsserts) {
      Invoke-BuildStep Stage-WindowsToolchainSxS $HostPlatform @{
        ToolchainRoot   = $HostPlatform.NoAssertsToolchainInstallRoot;
        RuntimeLocation = $HostRuntimeBin;
      }
    }
  }
}

if ($Android) {
  Build-SDKDependencies $AndroidSDKBuilds -Compilers $Compilers.Stage1

  $SDKROOT = Get-SwiftSDK -OS Android
  foreach ($Build in $AndroidSDKBuilds) {
    if ($Build.LinkModes.contains("dynamic")) {
      Invoke-BuildStep Build-SDK $Build -Context @{
        SDKIdentifier        = "Android";
        Variant              = "Dynamic";
        Compilers            = $Compilers.Stage1;
        Static               = $false;
        BuildFoundation      = $true;
        SupplementalRuntimes = $SDKSupplementalRuntimes;
      }
    }

    if ($Build.LinkModes.contains("static")) {
      Invoke-BuildStep Build-SDK $Build -Context @{
        SDKIdentifier        = "Android";
        Variant              = "Static";
        Compilers            = $Compilers.Stage1;
        Static               = $true;
        BuildFoundation      = $true;
        SupplementalRuntimes = $SDKSupplementalRuntimes;
      }
    }

    ConvertTo-ThickLayout -Platform $Build -Resources "${SDKROOT}\usr\lib\swift\android"        -Filter @("*.a", "*.so")
    ConvertTo-ThickLayout -Platform $Build -Resources "${SDKROOT}\usr\lib\swift_static\android" -Filter @("*.a", "*.so")
  }

  Install-SDK $AndroidSDKBuilds
  Write-SDKSettings Android

  Write-PlatformInfoPlist Android

  # Android swift-inspect only supports 64-bit platforms.
  $AndroidSDKBuilds | Where-Object { @("arm64-v8a", "x86_64") -contains $_.Architecture.ABI } | ForEach-Object {
    # Build-Inspect's `-CCompiler/-CXXCompiler $Compilers.C/.CXX` would feed
    # MSVC-style flags to the NDK clang; swap in a target-aware view of the
    # compiler hashtable that exposes GNU drivers under .C/.CXX.
    $InspectCompilers = @{
      C     = $Compilers.Stage1.GNUC
      CXX   = $Compilers.Stage1.GNUCXX
      Swift = $Compilers.Stage1.Swift
    }
    Invoke-BuildStep Build-Inspect $_ @{
      Compilers = $InspectCompilers;
      SwiftSDK  = Get-SwiftSDK -OS Android;
    }
  }

  # Copy static dependencies
  foreach ($Build in $AndroidSDKBuilds) {
    if (-not $Build.LinkModes.Contains("static")) { continue }

    $SwiftResourceDir = "${SDKROOT}\usr\lib\swift_static\$($Build.OS.ToString().ToLowerInvariant())\$($Build.Architecture.LLVMName)"
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build brotli)\libbrotlicommon.a" -Destination "${SwiftResourceDir}\libbrotlicommon.a" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build brotli)\libbrotlidec.a" -Destination "${SwiftResourceDir}\libbrotlidec.a" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build CURL)\lib\libcurl.a" -Destination "${SwiftResourceDir}\libcurl.a" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build XML2)\libxml2.a" -Destination "${SwiftResourceDir}\libxml2.a" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build ZLib)\libz.a" -Destination "${SwiftResourceDir}\libz.a" | Out-Null
  }
}

if ($Package) {
  Invoke-BuildStep Build-mimalloc $HostPlatform
  Invoke-BuildStep Patch-mimalloc $HostPlatform
  Invoke-BuildStep Build-Installer $HostPlatform
}

if ($Stage) {
  Copy-BuildArtifactsToStage $HostPlatform
}

if (-not $IsCrossCompiling) {
  if (-not $Toolchain -and $HostPlatform.OS -eq [OS]::Windows -and $Test.Count -gt 0) {
    $HostSDKRoot = Get-SwiftSDK -OS $HostPlatform.OS
    $HostRuntimeBin = Get-SDKRuntimeBin $HostPlatform $HostSDKRoot
    Invoke-BuildStep Stage-WindowsToolchainSxS $HostPlatform @{
      ToolchainRoot   = $HostPlatform.ToolchainInstallRoot;
      RuntimeLocation = $HostRuntimeBin;
    }
  }

  $CompilersTests = @("clang", "lld", "lldb", "lldb-swift", "llvm", "swift")
  if ($Test | Where-Object { $CompilersTests -contains $_ }) {
    $Tests = @{
      "-TestClang" = $Test -contains "clang";
      "-TestLLD" = $Test -contains "lld";
      "-TestLLDB" = $Test -contains "lldb";
      "-TestLLDBSwift" = $Test -contains "lldb-swift";
      "-TestLLVM" = $Test -contains "llvm";
      "-TestSwift" = $Test -contains "swift";
    }
    Invoke-BuildStep Test-Compilers $HostPlatform -Variant "Asserts" $Tests
  }

  # FIXME(jeffdav): Invoke-BuildStep needs a platform dictionary, even though the Test-
  # functions hardcode their platform needs.
  if ($Test -contains "dispatch") { Invoke-BuildStep Test-Dispatch $BuildPlatform }
  if ($Test -contains "foundation") { Invoke-BuildStep Test-Foundation $BuildPlatform }
  if ($Test -contains "xctest") { Invoke-BuildStep Test-XCTest $BuildPlatform }
  if ($Test -contains "testing") { Invoke-BuildStep Test-Testing $BuildPlatform }
  if ($Test -contains "llbuild") { Invoke-BuildStep Test-LLBuild $BuildPlatform }
  if ($Test -contains "swiftpm") { Invoke-BuildStep Test-PackageManager $BuildPlatform }
  if ($Test -contains "swift-format") { Invoke-BuildStep Test-Format $BuildPlatform }
  if ($Test -contains "sourcekit-lsp") { Invoke-BuildStep Test-SourceKitLSP $BuildPlatform}

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

if ($IncludeSBoM) {
  Invoke-IsolatingEnvVars {
    $env:SYFT_FILE_METADATA_SELECTION = "all"
    $env:SYFT_FILE_CONTENT_GLOBS = "**\*.h"
    $env:SYFT_FILE_METADATA_DIGESTS = "sha256"
    Invoke-Program (Get-Syft).Path -- `
        --base-path $BinaryCache `
        --source-name Swift `
        --source-version $ProductVersion `
        -o spdx-json=$ToolchainIdentifier-sbom.spdx.json `
        -o syft-json=$ToolchainIdentifier-sbom.syft.json `
        -o cyclonedx-xml=$ToolchainIdentifier-sbom.cyclone.xml `
        -o syft-table `
        dir:$(Get-InstallDir $HostPlatform)

    if ($Stage) {
      Copy-File $ToolchainIdentifier-sbom.spdx.json $Stage
      Copy-File $ToolchainIdentifier-sbom.syft.json $Stage
      Copy-File $ToolchainIdentifier-sbom.cyclone.xml $Stage
    }
  }
}

# Custom exception printing for more detailed exception information
} catch {
  # Stop the SDK builds that are still running.
  foreach ($Build in $WindowsSDKProcesses) {
    if (-not $Build.Process.HasExited) {
      & taskkill /T /F /PID $Build.Process.Id | Out-Null
    }
  }

  function Write-ErrorLines($Text, $Indent = 0) {
    $IndentString = " " * $Indent
    $Text.Replace("`r", "") -split "`n" | ForEach-Object {
      Write-Host "$IndentString$_" -ForegroundColor Red
    }
  }

  Write-ErrorLines "Error: $_"
  Write-ErrorLines $_.ScriptStackTrace -Indent 4

  # Walk the .NET inner exception chain to print all messages and stack traces
  $Exception = $_.Exception
  $Indent = 2
  while ($Exception -is [Exception]) {
      Write-ErrorLines "From $($Exception.GetType().FullName): $($Exception.Message)" -Indent $Indent
      if ($null -ne $Exception.StackTrace) {
          # .NET exceptions stack traces are already indented by 3 spaces
          Write-ErrorLines $Exception.StackTrace -Indent ($Indent + 1)
      }
      $Exception = $Exception.InnerException
      $Indent += 2
  }

  exit 1
} finally {
  if ($Summary) {
    Write-Summary
  }
}
