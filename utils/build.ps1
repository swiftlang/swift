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
Use 'swift-prepare' to configure and build everything the Swift test suite needs, without running it
(for example, so that utils\run-test.cmd can be used afterwards). It is not included in '*'.

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

$WindowsSxSAssemblyPublicKeyToken = $WindowsSxSAssemblyPublicKeyToken.ToLowerInvariant()

# Avoid $env:ProgramFiles in case this script is running as x86
$UnixToolsBinDir = "$env:SystemDrive\Program Files\Git\usr\bin"

## Cleanup build arguments.

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

# The SDKs build these kinds of runtime libraries.
$KnownPlatforms.Values | ForEach-Object {
  $_.LinkModes = if ($_.OS -eq [OS]::Windows) { $WindowsSDKLinkModes } else { $AndroidSDKLinkModes }
}

$msbuild = "$VSInstallRoot\MSBuild\Current\Bin\$BuildArchName\MSBuild.exe"

## Select and prepare build tools, platforms, parameters, etc.

if ($EnableCaching) {
  $UseHostToolchain = $false
}

if ($Android -and ($HostPlatform -ne $KnownPlatforms["WindowsX64"])) {
  throw "Unsupported host architecture for building android SDKs"
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

function Get-FlexExecutable {
  return (Get-DependencyManifest).FlexBison.Flex
}

function Get-BisonExecutable {
  return (Get-DependencyManifest).FlexBison.Bison
}

function ConvertTo-ThickLayout([Hashtable] $Platform, [string] $Resources, [string[]] $Filter) {
  Get-ChildItem "${Resources}\*" -File -Include ${Filter} -ErrorAction Ignore | ForEach-Object {
    Write-Host -BackgroundColor DarkRed -ForegroundColor White "$($_.FullName) is not nested in an architecture directory"
    Move-Item -Force $_.FullName "$Resources\$($Platform.Architecture.LLVMName)\" | Out-Null
  }
}

function Get-DotNet() {
  return (Get-DependencyManifest).DotNet.Path
}

function Invoke-WithDotNetRuntime([scriptblock] $Body) {
  Invoke-IsolatingEnvVars {
    $env:DOTNET_ROOT = (Get-DependencyManifest).DotNet.Root
    $env:DOTNET_HOST_PATH = Get-DotNet
    $env:DOTNET_MULTILEVEL_LOOKUP = "0"

    & $Body
  }
}

enum SPMBuildAction {
  # 'swift build'
  Build
  # 'swift test'
  Test
  # 'swift test --parallel'
  TestParallel
}

function Build-SPMProject {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [SPMBuildAction] $Action,
    [string] $Src,
    [string] $Bin,
    [hashtable] $Platform,
    [string] $Configuration = "release",
    [Parameter(ValueFromRemainingArguments)]
    [string[]] $AdditionalArguments
  )

  $ActionForOutput = switch ($Action) {
    Build { "Building" }
    Test { "Testing" }
    TestParallel { "Testing" }
  }

  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] $ActionForOutput '$Src' to '$Bin' ..."

  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()

  Invoke-IsolatingEnvVars {

    $HostSDKRoot = Get-SwiftSDK -OS $HostPlatform.OS
    $HostRuntimeBin = Get-SDKRuntimeBin $HostPlatform $HostSDKRoot
    $env:Path = "$HostRuntimeBin;$($HostPlatform.ToolchainInstallRoot)\usr\bin;${env:Path}"
    $env:SDKROOT = $HostSDKRoot
    $env:SWIFTCI_USE_LOCAL_DEPS = "1"

    $Arguments = @(
      "--scratch-path", $Bin,
      "--package-path", $Src,
      "-c", $Configuration
    )
    if ($DebugInfo) {
      $Arguments += @("-debug-info-format", $Platform.DebugFormat)
    } else {
      $Arguments += @("-debug-info-format", "none")
    }

    switch ($Action) {
      Build {
        $ActionName = "build"
      }
      Test {
        $ActionName = "test"
      }
      TestParallel {
        $ActionName = "test"
        $Arguments += @("--parallel")
      }
    }

    Invoke-Program swift $ActionName @Arguments @AdditionalArguments
  }

  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Finished building '$Src' to '$Bin' in $($Stopwatch.Elapsed)"
  Write-Host ""
}

function Build-WiXProject() {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position = 0, Mandatory = $true)]
    [string]$FileName,
    [Parameter(Mandatory = $true)]
    [hashtable]$Platform,
    [switch]$Bundle,
    [hashtable]$Properties = @{}
  )

  $ProductVersionArg = $ProductVersion
  if (-not $Bundle) {
    # WiX accepts a semantic version string for Bundles, but Packages still
    # require a purely numerical version number,
    # so trim any semantic versioning suffixes
    $ProductVersionArg = [regex]::Replace($ProductVersion, "[-+].*", "")
  }

  $Properties = $Properties.Clone()
  Add-KeyValueIfNew $Properties Configuration Release
  Add-KeyValueIfNew $Properties BaseOutputPath "$BinaryCache\$($Platform.Triple)\installer\"
  Add-KeyValueIfNew $Properties ProductArchitecture $Platform.Architecture.VSName
  Add-KeyValueIfNew $Properties ProductVersion $ProductVersionArg
  Add-KeyValueIfNew $Properties AcceptEula $WiX.EulaIdentifier

  $MSBuildArgs = @( "-noLogo", "-maxCpuCount", "-restore", "$SourceCache\swift-installer-scripts\platforms\Windows\$FileName" )
  foreach ($Property in $Properties.GetEnumerator()) {
    if ($Property.Value -is [string] -and $Property.Value.Contains(" ")) {
      $MSBuildArgs += "-p:$($Property.Key)=$($Property.Value.Replace('\', '\\'))"
    } else {
      $MSBuildArgs += "-p:$($Property.Key)=$($Property.Value)"
    }
  }
  $MSBuildArgs += "-binaryLogger:$BinaryCache\$($Platform.Triple)\msi\$($Platform.Architecture.VSName)-$([System.IO.Path]::GetFileNameWithoutExtension($FileName)).binlog"
  $MSBuildArgs += "-detailedSummary:False"

  Invoke-WithDotNetRuntime {
    Write-Host "$msbuild $MSBuildArgs"
    Invoke-Program $msbuild @MSBuildArgs
  }
}

function Build-CMark([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\cmark `
    -Bin (Get-CMarkBinaryCache $Platform) `
    -InstallTo "$(Get-InstallDir $Platform)\Toolchains\$ProductVersion+Asserts\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.Host.C `
    -CXXCompiler $Compilers.Host.CXX `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      BUILD_TESTING = "NO";
      CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP = "YES";
    }
}

function Copy-CMarkRuntimeToToolchain([Hashtable] $Platform, [string] $ToolchainRoot) {
  if ($Platform.OS -ne [OS]::Windows) {
    return
  }

  $BinDir = [IO.Path]::Combine($ToolchainRoot, "usr", "bin")
  New-Item -ItemType Directory -Path $BinDir -Force | Out-Null

  $CMarkBinaryCache = Get-CMarkBinaryCache $Platform
  foreach ($DLL in @(
    [IO.Path]::Combine($CMarkBinaryCache, "src", "cmark-gfm.dll"),
    [IO.Path]::Combine($CMarkBinaryCache, "extensions", "cmark-gfm-extensions.dll")
  )) {
    if (-not (Test-Path $DLL)) {
      throw "Copy-CMarkRuntimeToToolchain: '$DLL' not found; Build-CMark must run before Build-Compilers."
    }
    Copy-Item -Force -Path $DLL -Destination $BinDir
  }
}

function Copy-WindowsRuntimeToToolchain([Hashtable] $Platform,
                                        [string]    $ToolchainRoot,
                                        [string]    $RuntimeLocation) {
  if ($Platform.OS -ne [OS]::Windows) {
    return
  }

  if (-not (Test-Path $RuntimeLocation)) {
    throw "Copy-WindowsRuntimeToToolchain: '$RuntimeLocation' not found."
  }

  $BinDir = [IO.Path]::Combine($ToolchainRoot, "usr", "bin")
  New-Item -ItemType Directory -Path $BinDir -Force | Out-Null

  $RuntimeFiles = @(
    Get-ChildItem -Path $RuntimeLocation -Directory |
      ForEach-Object {
        $DLL = Join-Path $_.FullName "$($_.Name).dll"
        if (Test-Path $DLL -PathType Leaf) {
          Get-Item $DLL
        }
      } |
      Sort-Object Name
  )
  $DLLCount = @($RuntimeFiles | Where-Object { $_.Extension -ieq ".dll" }).Count
  if ($DLLCount -eq 0) {
    throw "Copy-WindowsRuntimeToToolchain: no DLLs found under '$RuntimeLocation'."
  }

  foreach ($File in $RuntimeFiles) {
    Copy-Item -Force -Path $File.FullName -Destination $BinDir
  }
  Write-Host "Copy-WindowsRuntimeToToolchain: copied $($RuntimeFiles.Count) runtime file(s) from '$RuntimeLocation' to '$BinDir'"
}

function Build-BuildTools([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\llvm-project\llvm `
    -Bin (Get-ProjectBinaryCache $Platform BuildTools) `
    -Platform $Platform `
    -Assembler $Assemblers.Host `
    -CCompiler $Compilers.Host.C `
    -CXXCompiler $Compilers.Host.CXX `
    -BuildTargets llvm-tblgen,clang-tblgen,clang-tidy-confusable-chars-gen,lldb-tblgen,llvm-config,swift-def-to-strings-converter,swift-serialize-diagnostics,swift-compatibility-symbols `
    -Defines @{
      CMAKE_CROSSCOMPILING = "NO";
      CLANG_ENABLE_LIBXML2 = "NO";
      LLDB_ENABLE_LIBXML2 = "NO";
      LLDB_ENABLE_PYTHON = "NO";
      LLDB_INCLUDE_TESTS = "NO";
      LLDB_ENABLE_SWIFT_SUPPORT = "NO";
      LLVM_ENABLE_ASSERTIONS = "NO";
      LLVM_ENABLE_LIBEDIT = "NO";
      LLVM_ENABLE_LIBXML2 = "NO";
      LLVM_ENABLE_PROJECTS = "clang;clang-tools-extra;lldb";
      LLVM_EXTERNAL_PROJECTS = "swift";
      LLVM_EXTERNAL_SWIFT_SOURCE_DIR = "$SourceCache\swift";
      SWIFT_BUILD_DYNAMIC_SDK_OVERLAY = "NO";
      SWIFT_BUILD_DYNAMIC_STDLIB = "NO";
      SWIFT_BUILD_HOST_DISPATCH = "NO";
      SWIFT_BUILD_LIBEXEC = "NO";
      SWIFT_BUILD_REGEX_PARSER_IN_COMPILER = "NO";
      SWIFT_BUILD_REMOTE_MIRROR = "NO";
      SWIFT_BUILD_SOURCEKIT = "NO";
      SWIFT_BUILD_STATIC_SDK_OVERLAY = "NO";
      SWIFT_BUILD_STATIC_STDLIB = "NO";
      SWIFT_BUILD_SWIFT_SYNTAX = "NO";
      SWIFT_ENABLE_DISPATCH = "NO";
      SWIFT_INCLUDE_APINOTES = "NO";
      SWIFT_INCLUDE_DOCS = "NO";
      SWIFT_INCLUDE_TESTS = "NO";
      "cmark-gfm_DIR" = "$(Get-InstallDir $Platform)\Toolchains\$ProductVersion+Asserts\usr\lib\cmake";
    }
}

function Build-EarlySwiftDriver([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-driver `
    -Bin (Get-ProjectBinaryCache $Platform EarlySwiftDriver) `
    -Platform $Platform `
    -CCompiler $Compilers.Pinned.C `
    -CXXCompiler $Compilers.Pinned.CXX `
    -SwiftCompiler $Compilers.Pinned.Swift `
    -SwiftSDK (Get-PinnedToolchainSDK -OS $Platform.OS) `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      BUILD_TESTING = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      # TODO(compnerd) - remove `-Xfrontend -use-static-resource-dir` - this is inferred by the `-static-stdlib`.
      CMAKE_Swift_FLAGS = @("-static-stdlib", "-Xfrontend", "-use-static-resource-dir");
      SWIFT_DRIVER_BUILD_TOOLS = "NO";
      SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
      SQLite3_LIBRARY = "$(Get-ProjectBinaryCache $Platform EarlySwiftDriverSQLite)\SQLite3.lib";

      # Prevent re-cloning the sources
      FETCHCONTENT_SOURCE_DIR_ARGUMENTPARSER = "$SourceCache\swift-argument-parser";
      FETCHCONTENT_SOURCE_DIR_LLBUILD = "$SourceCache\llbuild";
      FETCHCONTENT_SOURCE_DIR_TOOLSSUPPORTCORE = "$SourceCache\swift-tools-support-core";
    }
}

function Load-LitTestOverrides($Filename) {
  function Select-LitTestOverrides($Prefix) {
    $MatchingLines = Get-Content $Filename | Select-String -Pattern "`^${Prefix}.*$"
    return $MatchingLines | ForEach-Object { ($_ -replace $Prefix,"").Trim() }
  }

  $TestsToXFail = Select-LitTestOverrides "xfail"
  Write-Host "TestsToXFail=$TestsToXFail"
  if ($TestsToXFail -and $TestsToXFail.Length -ne 0) {
    $env:LIT_XFAIL = $TestsToXFail -join ";"
  }
  $TestsToSkip = Select-LitTestOverrides "skip"
  Write-Host "TestsToSkip=$TestsToSkip"
  if ($TestsToSkip -and $TestsToSkip.Length -gt 0) {
    $env:LIT_FILTER_OUT = "($($TestsToSkip -join '|'))"
  }
}

function Build-CDispatch([Hashtable] $Platform,
                         [Hashtable] $CCompiler,
                         [Hashtable] $CXXCompiler,
                         [string]    $Phase,
                         [switch]    $Static = $false) {
  Build-CMakeProject `
    -Src $SourceCache\swift-corelibs-libdispatch `
    -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Phase}CDispatch")) `
    -BuildTargets default `
    -Platform $Platform `
    -CCompiler $CCompiler `
    -CXXCompiler $CXXCompiler `
    -Defines (@{
      BUILD_SHARED_LIBS = $(if ($Static) { "NO" } else { "YES" });
      BUILD_TESTING = "NO";
      ENABLE_SWIFT = "NO";
    } + $(if ($Platform.OS -eq [OS]::Android) {
      # libdispatch doesn't use libc++, but with the shared C++ runtime it would
      # take `__emutls_get_address` from it and depend on libc++_shared.so.
      @{ CMAKE_ANDROID_STL_TYPE = "c++_static" }
    } else {
      @{}
    }))
}

function Get-CompilersDefines([Hashtable] $Platform,
                              [string]    $Variant,
                              [switch]    $Test,
                              [string]    $SwiftSDK = (Get-PinnedToolchainSDK -OS $Platform.OS),
                              [string]    $DispatchPackage = $null) {
  $BuildTools = [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform BuildTools), "bin")
  $PythonRoot = [IO.Path]::Combine((Get-PythonPath $Platform), "tools")
  $PythonLibName = "python{0}{1}" -f ([System.Version]$PythonVersion).Major, ([System.Version]$PythonVersion).Minor

  $TestDefines = if ($Test) {
    @{
      SWIFT_BUILD_DYNAMIC_SDK_OVERLAY = "YES";
      SWIFT_BUILD_DYNAMIC_STDLIB = "YES";
      SWIFT_BUILD_REMOTE_MIRROR = "YES";
      SWIFT_NATIVE_SWIFT_TOOLS_PATH = "";
    }
  } else {
    @{
      SWIFT_BUILD_DYNAMIC_SDK_OVERLAY = "NO";
      SWIFT_BUILD_DYNAMIC_STDLIB = "NO";
      # Pin OFF: a prior Test-Compilers reconfigure can have left this `YES`
      # in CMakeCache.txt.  With SWIFT_BUILD_DYNAMIC_STDLIB=NO the libexec
      # subdir's swift-backtrace target references absent stdlib targets.
      SWIFT_BUILD_LIBEXEC = "NO";
      SWIFT_BUILD_REMOTE_MIRROR = "NO";
      SWIFT_NATIVE_SWIFT_TOOLS_PATH = $BuildTools;
    }
  }

  # If DebugInfo is enabled limit the number of parallel links to avoid OOM.
  $DebugDefines = if ($DebugInfo) {
    @{
      SWIFT_PARALLEL_LINK_JOBS = "2";
      LLVM_PARALLEL_LINK_JOBS = "2";
    }
  } else {
    @{}
  }

  $DispatchDefines = if ($DispatchPackage) {
    @{ dispatch_DIR = $DispatchPackage }
  } else {
    @{}
  }

  return $TestDefines + $DebugDefines + $DispatchDefines + @{
    CLANG_TABLEGEN = (Join-Path -Path $BuildTools -ChildPath "clang-tblgen.exe");
    CLANG_TIDY_CONFUSABLE_CHARS_GEN = (Join-Path -Path $BuildTools -ChildPath "clang-tidy-confusable-chars-gen.exe");
    CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    CMAKE_Swift_FLAGS = if ($LTO -ne "none") { @("-use-ld=lld") } else { @() };
    LibXml2_DIR = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake\libxml2-2.11.5";
    LLDB_LIBXML2_VERSION = "2.11.5";
    LLDB_PYTHON_EXE_RELATIVE_PATH = "python.exe";
    LLDB_PYTHON_EXT_SUFFIX = ".pyd";
    LLDB_PYTHON_RELATIVE_PATH = "lib/site-packages";
    LLDB_PYTHON_DLL_RELATIVE_PATH = "../../../../Python-$PythonVersion/usr/bin";
    LLDB_TABLEGEN = (Join-Path -Path $BuildTools -ChildPath "lldb-tblgen.exe");
    LLDB_TEST_MAKE = "$ArtifactCache\GnuWin32Make-4.4.1\bin\make.exe";
    LLVM_CONFIG_PATH = (Join-Path -Path $BuildTools -ChildPath "llvm-config.exe");
    LLVM_ENABLE_ASSERTIONS = $(if ($Variant -eq "Asserts") { "YES" } else { "NO" })
    LLVM_ENABLE_LTO = $(switch ($LTO) {
      "none" { "OFF" }
      default {
        if ($UseHostToolchain) { throw "LTO is not supported with the host toolchain" }
        "$LTO"
      }
    })
    LLVM_ENABLE_LLD = $(switch ($LTO) {
      "none" { "NO" }
      default { "YES" }
    })
    LLVM_EXTERNAL_SWIFT_SOURCE_DIR = "$SourceCache\swift";
    LLVM_HOST_TRIPLE = $Platform.Triple;
    LLVM_NATIVE_TOOL_DIR = $BuildTools;
    LLVM_TABLEGEN = (Join-Path $BuildTools -ChildPath "llvm-tblgen.exe");
    LLVM_USE_HOST_TOOLS = "NO";
    Python3_EXECUTABLE = (Get-PythonExecutable);
    Python3_INCLUDE_DIR = "$PythonRoot\include";
    Python3_LIBRARY = "$PythonRoot\libs\$PythonLibName.lib";
    Python3_ROOT_DIR = $PythonRoot;
    Python3_VERSION = $PythonVersion;
    SWIFT_TOOLCHAIN_VERSION = "${ToolchainIdentifier}";
    SWIFT_BUILD_SWIFT_SYNTAX = "YES";
    SWIFT_CLANG_LOCATION = (Get-PinnedToolchainToolsDir);
    SWIFT_EARLY_SWIFT_DRIVER_BUILD = $(if ($Platform -eq $BuildPlatform) { "$(Get-ProjectBinaryCache $BuildPlatform EarlySwiftDriver)\bin" } else { "" })
    SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY = "YES";
    SWIFT_ENABLE_EXPERIMENTAL_CXX_INTEROP = "YES";
    SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING = "YES";
    SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED = "YES";
    SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION = "YES";
    SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING = "YES";

    # We can't enable this on Android yet
    # https://github.com/swiftlang/swift/issues/87445
    SWIFT_ENABLE_RUNTIME_MODULE = $(if ($Platform.OS -eq [OS]::Windows) {
        "YES"
      } else {
        "NO"
      });
    SWIFT_ENABLE_BACKTRACING = $(if ($Platform.OS -eq [OS]::Windows -and $Platform.Architecture.ShortName -ne "x86") {
        "YES"
      } else {
        "NO"
      });

    SWIFT_ENABLE_SYNCHRONIZATION = "YES";
    SWIFT_ENABLE_VOLATILE = "YES";
    SWIFT_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
    SWIFT_PATH_TO_STRING_PROCESSING_SOURCE = "$SourceCache\swift-experimental-string-processing";
    SWIFT_PATH_TO_SWIFT_SDK = $SwiftSDK;
    SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE = "$SourceCache\swift-syntax";
    SWIFT_STDLIB_ASSERTIONS = "NO";
    SWIFTSYNTAX_ENABLE_ASSERTIONS = "NO";
    "cmark-gfm_DIR" = "$($Platform.ToolchainInstallRoot)\usr\lib\cmake";
  }
}

function Build-Compilers([Hashtable] $Platform,
                         [string]    $Variant,
                         [Project]   $Project          = [Project]::Compilers,
                         [Hashtable] $Assembler        = $Assemblers.Host,
                         [Hashtable] $CCompiler        = $Compilers.Host.C,
                         [Hashtable] $CXXCompiler      = $Compilers.Host.CXX,
                         [Hashtable] $SwiftCompiler    = $Compilers.Pinned.Swift,
                         [string]    $SwiftSDK         = (Get-PinnedToolchainSDK -OS $Platform.OS),
                         [string]    $ToolchainRoot    = "$(Get-InstallDir $Platform)\Toolchains\$ProductVersion+$Variant",
                         [string]    $RuntimeLocation  = $null,
                         [string]    $DispatchPackage  = $null,
                         [string]    $CacheScript      = "$SourceCache\swift\cmake\caches\Windows-$($Platform.Architecture.LLVMName).cmake") {
  New-Item -ItemType Directory -Path $BinaryCache\$($HostPlatform.Triple) -ErrorAction Ignore | Out-Null

  Build-CMakeProject `
    -Src $SourceCache\llvm-project\llvm `
    -Bin (Get-ProjectBinaryCache $Platform $Project) `
    -InstallTo "$ToolchainRoot\usr" `
    -Platform $Platform `
    -Assembler $Assembler `
    -CCompiler $CCompiler `
    -CXXCompiler $CXXCompiler `
    -SwiftCompiler $SwiftCompiler `
    -SwiftSDK $SwiftSDK `
    -BuildTargets @("install-distribution") `
    -CacheScript $CacheScript `
    -Defines (Get-CompilersDefines $Platform $Variant -SwiftSDK $SwiftSDK -DispatchPackage $DispatchPackage)

  Copy-CMarkRuntimeToToolchain $Platform $ToolchainRoot
  if ($RuntimeLocation) {
    Copy-WindowsRuntimeToToolchain $Platform $ToolchainRoot $RuntimeLocation
  }
}

function Write-ToolchainInfo([Hashtable] $Platform,
                             [string]    $Variant       = "Asserts",
                             [string]    $ToolchainRoot = $(if ($Variant -eq "NoAsserts") { $Platform.NoAssertsToolchainInstallRoot } else { $Platform.ToolchainInstallRoot })) {
  $Settings = @{
    FallbackLibrarySearchPaths = @("usr/bin")
    Identifier = "${ToolchainIdentifier}.${Variant}"
    Version = "${ProductVersion}"
  }
  Write-PList -Settings $Settings -Path "$ToolchainRoot\ToolchainInfo.plist"
}

# Bind Windows toolchain executables to the Swift runtime they were linked
# against, instead of whichever runtime DLLs happen to sit next to them.
# In-process plugins inherit the EXE activation context and use the same
# private SxS bundle.  The build-tree test phase must use the bundle form:
# swift-test-stdlib later produces a flat runtime layout in bin\, which would
# overwrite or shadow per-DLL private assembly directories.
function Set-WindowsSxSToolchainRuntime {
  [CmdletBinding()]
  param(
    [Parameter(Mandatory)] [string]   $BinaryDir,
    [Parameter(Mandatory)] [string]   $RuntimeSourceDir,
    [Parameter(Mandatory)] [string[]] $Tools,
    # Keep this distinct from every DLL basename in $BinaryDir; SxS probes
    # `<assemblyName>.dll` before `<assemblyName>\`.
    [string]                          $AssemblyName          = "swiftToolchainRuntime",
    # Windows SxS requires the canonical four-part `a.b.c.d` form.
    [string]                          $AssemblyVersion       = "1.0.0.0",
    [Parameter(Mandatory)] [string]   $ProcessorArchitecture,
    [string]                          $PublicKeyToken        = $WindowsSxSAssemblyPublicKeyToken
  )

  if (-not (Test-Path $BinaryDir)) {
    throw "Set-WindowsSxSToolchainRuntime: BinaryDir '$BinaryDir' does not exist"
  }
  if (-not (Test-Path $RuntimeSourceDir)) {
    # Running without -Toolchain may happen before the runtime is installed.
    Write-Warning "Set-WindowsSxSToolchainRuntime: RuntimeSourceDir '$RuntimeSourceDir' does not exist; skipping SxS bind"
    return
  }

  if ($AssemblyVersion -notmatch '^\d+\.\d+\.\d+\.\d+$') {
    throw "Set-WindowsSxSToolchainRuntime: AssemblyVersion '$AssemblyVersion' is not in the required 4-part 'a.b.c.d' form (Windows SxS rejects any other shape)."
  }

  Write-Host "Set-WindowsSxSToolchainRuntime: source            = '$RuntimeSourceDir'"
  Write-Host "Set-WindowsSxSToolchainRuntime: destination       = '$BinaryDir'"
  Write-Host "Set-WindowsSxSToolchainRuntime: assembly name     = $AssemblyName"
  Write-Host "Set-WindowsSxSToolchainRuntime: assembly version  = $AssemblyVersion"
  Write-Host "Set-WindowsSxSToolchainRuntime: architecture      = $ProcessorArchitecture"

  $DLLItems = @(Get-WindowsSxSRuntimeDLLs $RuntimeSourceDir)
  if (-not $DLLItems) {
    Write-Warning "Set-WindowsSxSToolchainRuntime: no *.dll found under '$RuntimeSourceDir'; skipping SxS bind"
    return
  }
  Write-Host "Set-WindowsSxSToolchainRuntime: globbed $($DLLItems.Count) *.dll from source"

  # Use one bundle assembly so the later flat runtime DLLs can coexist with the
  # test tool binding.  A per-DLL private layout is correct for the final
  # install image, but it is not stable while the build tree is still producing
  # flat runtime DLLs in bin\.
  $AssemblyDir = Join-Path $BinaryDir $AssemblyName
  New-Item -ItemType Directory -Path $AssemblyDir -Force | Out-Null

  Write-Host "Set-WindowsSxSToolchainRuntime: staging $($DLLItems.Count) DLLs into bundle assembly '$AssemblyName':"
  $StagedFiles = New-Object System.Collections.Generic.List[string]
  foreach ($DLL in $DLLItems) {
    $StagedDLL = Join-Path $AssemblyDir $DLL.Name
    Copy-Item -Path $DLL.FullName -Destination $StagedDLL -Force
    Write-Host ("  [{0,-32}]  {1,10:N0}b  ->  {2}" -f $DLL.Name, $DLL.Length, $StagedDLL)
    [void]$StagedFiles.Add($DLL.Name)
  }

  $FilesXml = ($StagedFiles | ForEach-Object { "  <file name=`"$_`"/>" }) -join "`r`n"
  $BundleManifest = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <assemblyIdentity
      name="$AssemblyName"
      processorArchitecture="$ProcessorArchitecture"
      publicKeyToken="$PublicKeyToken"
      type="win32"
      version="$AssemblyVersion" />
$FilesXml
</assembly>
"@
  $BundleManifestPath = Join-Path $AssemblyDir "$AssemblyName.manifest"
  Set-Content -Path $BundleManifestPath -Value $BundleManifest -Encoding UTF8
  Write-Host "Set-WindowsSxSToolchainRuntime: wrote bundle assembly manifest -> '$BundleManifestPath'"

  $Dependency = [pscustomobject]@{
    Name                  = $AssemblyName
    Version               = $AssemblyVersion
    ProcessorArchitecture = $ProcessorArchitecture
    PublicKeyToken        = $PublicKeyToken
  }
  foreach ($Tool in $Tools) {
    $ToolPath = if ([System.IO.Path]::IsPathRooted($Tool)) {
      $Tool
    } else {
      Join-Path $BinaryDir $Tool
    }
    if (-not (Test-Path $ToolPath)) {
      Write-Warning "Set-WindowsSxSToolchainRuntime: tool '$ToolPath' does not exist; skipping"
      continue
    }

    Write-Host "Set-WindowsSxSToolchainRuntime: binding '$ToolPath' to assembly '$AssemblyName' v$AssemblyVersion"
    Set-WindowsExecutableManifestDependencies $ToolPath @($Dependency) "Set-WindowsSxSToolchainRuntime"
  }
}

# Return the imported DLL names from a PE file.  Use the pinned host toolchain
# so this also works while cross-compiling a non-host Windows toolchain.
function Get-DLLImports([string] $Path) {
  if (-not (Test-Path $Path)) {
    throw "Get-DLLImports: '$Path' does not exist"
  }
  $ReadObj = Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath "llvm-readobj.exe"
  if (-not (Test-Path $ReadObj)) {
    throw "Get-DLLImports: pinned 'llvm-readobj.exe' not found at '$ReadObj'"
  }
  # `llvm-readobj --coff-imports` prints a `Name: <dll>` line per import.
  $Output = & $ReadObj --coff-imports $Path 2>$null
  $Imports = New-Object System.Collections.Generic.List[string]
  foreach ($Line in $Output) {
    if ($Line -match '^\s*Name:\s*(\S+)\s*$') {
      [void]$Imports.Add($Matches[1])
    }
  }
  $Imports | Sort-Object -Unique
}

function Get-DirectRuntimeImports([string] $Path, $RuntimeSet) {
  $Imports = [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase
  )
  foreach ($Raw in (Get-DLLImports -Path $Path)) {
    $Base = [IO.Path]::GetFileNameWithoutExtension($Raw)
    if ($RuntimeSet.Contains($Base)) {
      [void]$Imports.Add($Base)
    }
  }
  $Imports | Sort-Object
}

function Get-StaticRuntimeImports([string] $Path,
                                  [string] $BinaryDir,
                                  [object] $RuntimeSet) {
  $Imports = [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase
  )
  $Visited = [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase
  )
  $Queue = [System.Collections.Generic.Queue[string]]::new()
  $Queue.Enqueue($Path)

  while ($Queue.Count -gt 0) {
    $Current = [IO.Path]::GetFullPath($Queue.Dequeue())
    if (-not $Visited.Add($Current)) { continue }

    foreach ($Raw in (Get-DLLImports -Path $Current)) {
      $DLLName = [IO.Path]::GetFileName($Raw)
      $Base = [IO.Path]::GetFileNameWithoutExtension($DLLName)
      if ($RuntimeSet.Contains($Base)) {
        [void]$Imports.Add($Base)
        continue
      }

      $LocalDLL = Join-Path $BinaryDir $DLLName
      if (Test-Path $LocalDLL -PathType Leaf) {
        $Queue.Enqueue($LocalDLL)
      }
    }
  }

  $Imports | Sort-Object
}

function Get-RuntimeImportClosure([string[]] $Roots, [Hashtable] $RuntimeGraph) {
  $Seen = [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase
  )
  $Queue = New-Object System.Collections.Generic.Queue[string]
  foreach ($Root in $Roots) {
    if ($RuntimeGraph.ContainsKey($Root)) {
      $Queue.Enqueue($Root)
    }
  }

  while ($Queue.Count -gt 0) {
    $Current = $Queue.Dequeue()
    if (-not $Seen.Add($Current)) { continue }
    if ($RuntimeGraph.ContainsKey($Current)) {
      foreach ($Next in $RuntimeGraph[$Current]) {
        $Queue.Enqueue($Next)
      }
    }
  }
  $Seen | Sort-Object
}

# Stage one private SxS assembly per runtime DLL.  Each EXE manifest names the
# runtime closure needed by its static import graph.
function Set-WindowsSxSToolchainRuntimePerDLL {
  [CmdletBinding()]
  param(
    [Parameter(Mandatory)] [string]    $BinaryDir,
    [Parameter(Mandatory)] [string]    $RuntimeSourceDir,
    [Parameter(Mandatory)] [Hashtable] $EXEDependencies,
    [Parameter(Mandatory)] [string[]]  $DLLsToInject,
    [Parameter(Mandatory)] [string]    $AssemblyVersion,
    [Parameter(Mandatory)] [string]    $ProcessorArchitecture,
    [string]                           $PublicKeyToken = $WindowsSxSAssemblyPublicKeyToken
  )

  if (-not (Test-Path $BinaryDir)) {
    throw "Set-WindowsSxSToolchainRuntimePerDLL: BinaryDir '$BinaryDir' does not exist"
  }
  if (-not (Test-Path $RuntimeSourceDir)) {
    Write-Warning "Set-WindowsSxSToolchainRuntimePerDLL: RuntimeSourceDir '$RuntimeSourceDir' does not exist; skipping SxS bind"
    return
  }
  if ($AssemblyVersion -notmatch '^\d+\.\d+\.\d+\.\d+$') {
    throw "Set-WindowsSxSToolchainRuntimePerDLL: AssemblyVersion '$AssemblyVersion' is not in the required 4-part 'a.b.c.d' form (Windows SxS rejects any other shape)."
  }

  Write-Host "Set-WindowsSxSToolchainRuntimePerDLL: source            = '$RuntimeSourceDir'"
  Write-Host "Set-WindowsSxSToolchainRuntimePerDLL: destination       = '$BinaryDir'"
  Write-Host "Set-WindowsSxSToolchainRuntimePerDLL: assembly version  = $AssemblyVersion"
  Write-Host "Set-WindowsSxSToolchainRuntimePerDLL: architecture      = $ProcessorArchitecture"

  Write-Host "Set-WindowsSxSToolchainRuntimePerDLL: staging $($DLLsToInject.Count) per-DLL assemblies:"
  $InjectedDLLs = New-Object System.Collections.Generic.HashSet[string]
  foreach ($DLLName in ($DLLsToInject | Sort-Object)) {
    $SourceDLL = Join-Path $RuntimeSourceDir "$DLLName.dll"
    if (-not (Test-Path $SourceDLL)) {
      throw "Set-WindowsSxSToolchainRuntimePerDLL: '$SourceDLL' not found in source; refusing to bind EXEs to a missing SxS assembly"
    }
    $AssemblyDir = Join-Path $BinaryDir $DLLName
    New-Item -ItemType Directory -Path $AssemblyDir -Force -ErrorAction Stop | Out-Null
    $StagedDLL = Join-Path $AssemblyDir "$DLLName.dll"
    Copy-Item -Path $SourceDLL -Destination $StagedDLL -Force -ErrorAction Stop
    Ensure-WindowsAssemblyManifest `
      -ImagePath              $StagedDLL `
      -AssemblyVersion        $AssemblyVersion `
      -ProcessorArchitecture  $ProcessorArchitecture `
      -PublicKeyToken         $PublicKeyToken `
      -LogPrefix              "Set-WindowsSxSToolchainRuntimePerDLL"
    Assert-WindowsManifestResourcesAreSxSSafe $StagedDLL "Set-WindowsSxSToolchainRuntimePerDLL"
    $Length = (Get-Item $StagedDLL).Length
    Write-Host ("  [{0,-32}]  {1,12:N0}b  ->  {2}" -f "$DLLName.dll", $Length, $AssemblyDir)
    [void]$InjectedDLLs.Add($DLLName)
  }

  # Do not inspect arbitrary DLLs in `usr\bin` here.  Windows DLLs commonly
  # carry RT_MANIFEST #2, and this pass only owns the staged runtime DLL
  # assemblies plus the EXE manifests it rewrites below.
  $BoundEXECount = 0
  $SkippedCount = 0
  foreach ($ToolPath in ($EXEDependencies.Keys | Sort-Object)) {
    $DirectRuntimeDeps = @($EXEDependencies[$ToolPath])
    if ($DirectRuntimeDeps.Count -eq 0) {
      Write-Host "Set-WindowsSxSToolchainRuntimePerDLL: skipped '$ToolPath' -- no Swift runtime imports"
      $SkippedCount++
      continue
    }
    if (-not (Test-Path $ToolPath)) {
      Write-Warning "Set-WindowsSxSToolchainRuntimePerDLL: tool '$ToolPath' does not exist; skipping"
      continue
    }

    Write-Host "Set-WindowsSxSToolchainRuntimePerDLL: binding EXE '$ToolPath' -> [$($DirectRuntimeDeps -join ', ')]"

    $MissingDLLs = @($DirectRuntimeDeps | Where-Object { -not $InjectedDLLs.Contains($_) })
    if ($MissingDLLs.Count -gt 0) {
      throw "Set-WindowsSxSToolchainRuntimePerDLL: refusing to bind '$ToolPath'; missing injected SxS DLL(s): $($MissingDLLs -join ', ')"
    }

    $Dependencies = @(
      foreach ($DLLName in ($DirectRuntimeDeps | Sort-Object)) {
        New-WindowsManifestDependency $DLLName $AssemblyVersion $ProcessorArchitecture $PublicKeyToken
      }
    )
    Set-WindowsExecutableManifestDependencies $ToolPath $Dependencies "Set-WindowsSxSToolchainRuntimePerDLL"
    $BoundEXECount++
  }

  Write-Host "Set-WindowsSxSToolchainRuntimePerDLL: bound $BoundEXECount EXE(s); skipped $SkippedCount EXE(s) with no runtime imports"
}

function Test-Compilers([Hashtable] $Platform, [string] $Variant, [switch] $TestClang, [switch] $TestLLD, [switch] $TestLLDB, [switch] $TestLLDBSwift, [switch] $TestLLVM, [switch] $TestSwift, [switch] $PrepareSwift) {
  Invoke-IsolatingEnvVars {
    $SwiftSDK = Get-SwiftSDK -OS $Platform.OS
    $SwiftRuntime = Get-SDKRuntimeBin $Platform $SwiftSDK
    $Stage2BinDir = [IO.Path]::Combine((Get-ProjectBinaryCache $Platform Stage2Compilers), "bin")
    $CDispatchBinaryCache = Get-ProjectBinaryCache $Platform DynamicCDispatch
    $env:Path = "$SwiftRuntime;$Stage2BinDir;$CDispatchBinaryCache;$CDispatchBinaryCache\bin;$(Get-CMarkBinaryCache $Platform)\src;$env:Path;$VSInstallRoot\DIA SDK\bin\$($HostPlatform.Architecture.VSName);$UnixToolsBinDir"
    $env:PYTHONUTF8 = "1"
    $TestingDefines = Get-CompilersDefines $Platform $Variant -Test -SwiftSDK $SwiftSDK
    # Stage2 tests must use the freshly-built native tools, not Stage1.
    $TestingDefines["SWIFT_NATIVE_SWIFT_TOOLS_PATH"] = $Stage2BinDir
    # check-swift depends on swift-backtrace-${SDK}; build the libexec helpers
    # with the rest of the test targets.
    $TestingDefines["SWIFT_BUILD_LIBEXEC"] = "YES"
    # Keep %host-build-swift on the same platform SDK that Stage2 uses.
    $TestingDefines["SWIFT_HOST_SDKROOT"] = $SwiftSDK
    $Targets = @()
    if ($TestLLVM) { $Targets += @("check-llvm") }
    if ($TestClang) { $Targets += @("check-clang") }
    if ($TestLLD) { $Targets += @("check-lld") }
    if ($TestSwift -or $PrepareSwift) {
      # check-swift does not depend on SwiftCompilerPlugin, but the macro tests
      # need it, so it is built before check-swift starts.
      $Targets += @("SwiftCompilerPlugin")
    }
    $LLDBTargets = @()
    if ($TestLLDB) { $LLDBTargets += @("check-lldb") }
    elseif ($TestLLDBSwift) { $LLDBTargets += @("check-lldb-swift") }
    if ($TestLLDB -or $TestLLDBSwift) {
      # Override test filter for known issues in downstream LLDB
      Load-LitTestOverrides ([IO.Path]::GetFullPath([IO.Path]::Combine($PSScriptRoot, "..", "..", "llvm-project", "lldb", "test", "windows-swift-llvm-lit-test-overrides.txt")))

      $CompilerCache = Get-ProjectBinaryCache $Platform Stage2Compilers
      $SwiftRTSubdir = "lib\swift\windows"

      # Transitive dependency closure of _lldb.pyd.
      $LLDBSitePackages = "$CompilerCache\lib\site-packages\lldb"
      New-Item -ItemType Directory -Force $LLDBSitePackages | Out-Null
      foreach ($RuntimeDLL in @(Get-WindowsSxSRuntimeDLLs $SwiftRuntime)) {
        Copy-Item -Path $RuntimeDLL.FullName -Destination $LLDBSitePackages -Force
      }
      $LLDBRuntimeDLLs = @(Get-ChildItem -Path "$CompilerCache\bin" -Filter "_CompilerSwift*.dll" -File)
      $LLDBRuntimeDLLs += Get-Item "$(Get-CMarkBinaryCache $Platform)\src\cmark-gfm.dll"
      foreach ($RuntimeDLL in $LLDBRuntimeDLLs) {
        Copy-Item -Path $RuntimeDLL.FullName -Destination $LLDBSitePackages -Force
      }

      # Runtime dependencies of repl_swift.exe.  The Stage2-compiled swiftCore.dll
      # already lives in $CompilerCache\bin and is what repl_swift.exe is linked
      # against, so only swiftrt.obj needs to be staged from the SDK here.
      New-Item -ItemType Directory -Force "$CompilerCache\$SwiftRTSubdir" | Out-Null
      Write-Host "Copying '$SwiftSDK\usr\$SwiftRTSubdir\$($Platform.Architecture.LLVMName)\swiftrt.obj' to '$CompilerCache\$SwiftRTSubdir'"
      Copy-Item `
        -Path "$SwiftSDK\usr\$SwiftRTSubdir\$($Platform.Architecture.LLVMName)\swiftrt.obj" `
        -Destination "$CompilerCache\$SwiftRTSubdir"
      $TestingDefines += @{
        LLDB_INCLUDE_TESTS = "YES";
        # Check for required Python modules in CMake
        LLDB_ENFORCE_STRICT_TEST_REQUIREMENTS = "YES";
        # No watchpoint support on windows: https://github.com/llvm/llvm-project/issues/24820
        LLDB_TEST_USER_ARGS = "--skip-category=watchpoint;--sysroot=$SwiftSDK";
        LLDB_TEST_SWIFT_DRIVER_EXTRA_FLAGS = "-sdk '$SwiftSDK'"
        LLDB_TEST_INFERIOR_RUNTIME_BIN = "$SwiftRuntime";
        # gtest sharding breaks llvm-lit's --xfail and LIT_XFAIL inputs: https://github.com/llvm/llvm-project/issues/102264
        LLVM_LIT_ARGS = "-v --no-gtest-sharding --time-tests";
        # LLDB Unit tests link against this library
        LLVM_UNITTEST_LINK_FLAGS = "$SwiftSDK\usr\lib\swift\windows\$($Platform.Architecture.LLVMName)\swiftCore.lib";
      }
    }

    if ((-not $Targets) -and (-not $LLDBTargets)) {
      Write-Warning "Test-Compilers invoked without specifying test target(s)."
    }

    # Build and bind the runtime-loading tools before swift-test-stdlib can
    # repopulate bin\ with freshly-built runtime DLLs.  This path intentionally
    # uses the bundle SxS helper rather than the per-DLL install-layout helper
    # because the test build keeps producing flat runtime DLLs in bin\.  Reuse
    # the same configure arguments for the final build so ninja does not relink
    # away the manifests.
    $BuildCMakeArgs = @{
      Src           = [IO.Path]::Combine($SourceCache, "llvm-project", "llvm")
      Bin           = (Get-ProjectBinaryCache $Platform Stage2Compilers)
      InstallTo     = "$($Platform.ToolchainInstallRoot)\usr"
      Platform      = $Platform
      CCompiler     = $Compilers.Stage1.C
      CXXCompiler   = $Compilers.Stage1.CXX
      SwiftCompiler = $Compilers.Stage1.Swift
      SwiftSDK      = $SwiftSDK
      CacheScript   = [IO.Path]::Combine($SourceCache, "swift", "cmake", "caches", "Windows-$($Platform.Architecture.LLVMName).cmake")
      Defines       = $TestingDefines
    }

    Build-CMakeProject @BuildCMakeArgs -BuildTargets @(
      "swift-frontend",
      "sourcekitd-test",
      "swift-refactor",
      "swift-ide-test",
      "swift-plugin-server"
    )

    # Prefer the platform SDK runtime.  The pre-staged bundle fallback keeps
    # running without -Toolchain usable when the install image is absent.
    $Stage2LibexecSwiftDir = [IO.Path]::Combine($BuildCMakeArgs.Bin, "libexec", "swift")
    $RuntimeSourceCandidates = @(
      $SwiftRuntime,
      (Join-Path $Stage2BinDir "swiftToolchainRuntime")
    )
    Write-Host "SxS bind: searching for toolchain runtime source dir:"
    $RuntimeSource = $null
    foreach ($Candidate in $RuntimeSourceCandidates) {
      $Exists = Test-Path $Candidate
      $DllCount = if ($Exists) { (Get-ChildItem $Candidate -Filter '*.dll' -File -ErrorAction SilentlyContinue).Count } else { 0 }
      $Marker = if ($RuntimeSource) { "skip" } elseif ($Exists -and $DllCount -gt 0) { "USE" } else { "miss" }
      Write-Host ("  [{0,-4}] {1} ({2} dlls)" -f $Marker, $Candidate, $DllCount)
      if (-not $RuntimeSource -and $Exists -and $DllCount -gt 0) { $RuntimeSource = $Candidate }
    }
    if (-not $RuntimeSource) {
      throw "Test-Compilers: no Swift runtime DLLs found in any candidate ($($RuntimeSourceCandidates -join ', ')). The platform SDK runtime was not staged; build the toolchain/SDK (e.g. with -Toolchain) before running the test phase."
    }

    Invoke-IsolatingEnvVars {
      # Test-time tools execute on the build host.
      # TODO(Steelskin): `repl_swift.exe` is explicitly excluded here because
      # the test reconfigure here makes lldb compile expressions against the in-
      # tree resilient stdlib, which breaks SwiftREPL tests if `repl_swift.exe`
      # uses the shipped runtime instead.
      # See https://github.com/swiftlang/swift/issues/91537 for details.
      Invoke-VsDevShell $BuildPlatform
      Set-WindowsSxSToolchainRuntime `
        -BinaryDir              $Stage2BinDir `
        -RuntimeSourceDir       $RuntimeSource `
        -ProcessorArchitecture  $BuildPlatform.Architecture.VSName `
        -Tools                  @(
                                   "swift.exe",
                                   "swiftc.exe",
                                   "swift-driver.exe",
                                   "swift-frontend.exe",
                                   "swift-synthesize-interface.exe",
                                   "sil-opt.exe",
                                   "sourcekitd-test.exe",
                                   "swift-refactor.exe",
                                   "swift-ide-test.exe",
                                   "swift-plugin-server.exe",
                                   "swiftc-legacy-driver.exe",
                                   "lldb.exe"
                                 )
      # SxS only probes the EXE's own directory for the named assembly.
      if (Test-Path (Join-Path $Stage2LibexecSwiftDir "swift-backtrace.exe")) {
        Set-WindowsSxSToolchainRuntime `
          -BinaryDir              $Stage2LibexecSwiftDir `
          -RuntimeSourceDir       $RuntimeSource `
          -ProcessorArchitecture  $BuildPlatform.Architecture.VSName `
          -Tools                  @("swift-backtrace.exe")
      } else {
        Write-Warning "SxS bind: '$Stage2LibexecSwiftDir\swift-backtrace.exe' not present; skipping backtracer bind (Build-TestBacktrace did not run or failed)"
      }
    }

    # TODO(roman-bcny): Workaround for https://github.com/swiftlang/swift/issues/87970
    # Stdlib DLLs must be fully linked before swift-frontend compilations
    # that load them, otherwise the linker races with memory-mapped DLLs
    # causing LNK1104. Build swift-test-stdlib first to enforce ordering.
    # Some tests run Stage2 tools with a scrubbed PATH, so cmark's DLLs can't
    # be found via the PATH set above; put them beside the executables.
    foreach ($DLL in @("src\cmark-gfm.dll", "extensions\cmark-gfm-extensions.dll")) {
      $CMarkDLL = Join-Path (Get-CMarkBinaryCache $Platform) $DLL
      if (Test-Path $CMarkDLL) {
        Copy-Item -Force -Path $CMarkDLL -Destination $Stage2BinDir
      }
    }

    Build-CMakeProject @BuildCMakeArgs -BuildTargets @("swift-test-stdlib")
    Invoke-WithLitTimeTraces "$($BuildCMakeArgs.Bin)\lit-time-traces" {
      if ($Targets) {
        Build-CMakeProject @BuildCMakeArgs -BuildTargets $Targets
      }
      if ($TestSwift) {
        Build-CMakeProject @BuildCMakeArgs -BuildTargets @("check-swift")
      }
    }

    if ($LLDBTargets) {
      Invoke-IsolatingEnvVars {
        $env:SDKROOT = $SwiftSDK
        Build-CMakeProject @BuildCMakeArgs -BuildTargets $LLDBTargets
      }
    }
  }
}

# Reference: https://github.com/microsoft/mimalloc/tree/dev/bin#minject
function Build-mimalloc() {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position = 0, Mandatory = $true)]
    [hashtable]$Platform
  )

  $MimallocBinaryCache = "$BinaryCache\$($Platform.Triple)\mimalloc"
  # Match the architecture baselines in mimalloc's Visual Studio projects.
  # MI_OPT_ARCH may select a newer ARM architecture as mimalloc evolves.
  $MimallocArchitectureFlag = switch ($Platform.Architecture.ShortName) {
    "x64" { "/arch:AVX2" }
    "arm64" { "/arch:armv8.2" }
    default { throw "Unsupported mimalloc architecture '$($Platform.Architecture.ShortName)'" }
  }

  $ToolchainRoots = @($Platform.ToolchainInstallRoot)
  if ($IncludeNoAsserts) {
    $ToolchainRoots += $Platform.NoAssertsToolchainInstallRoot
  }

  foreach ($ToolchainRoot in $ToolchainRoots) {
    Build-CMakeProject `
      -Src "$SourceCache\mimalloc" `
      -Bin $MimallocBinaryCache `
      -InstallTo "$ToolchainRoot\usr" `
      -Platform $Platform `
      -CCompiler $Compilers.MSVC.C `
      -CXXCompiler $Compilers.MSVC.CXX `
      -Defines @{
        CMAKE_CXX_FLAGS = @($MimallocArchitectureFlag);
        MI_BUILD_OBJECT = "NO";
        MI_BUILD_SHARED = "YES";
        MI_BUILD_STATIC = "NO";
        MI_BUILD_TESTS = "NO";
        MI_NO_OPT_ARCH = "YES";
      }
  }
}

function Patch-mimalloc() {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position = 0, Mandatory = $true)]
    [hashtable]$Platform
  )

  $BuildSuffix = if ($BuildPlatform -eq $KnownPlatforms["WindowsX64"]) { "" } else { "-arm64" }

  $Tools = @(
    "swift.exe",
    "swiftc.exe",
    "swift-api-digester.exe",
    "swift-autolink-extract.exe",
    "swift-driver.exe",
    "swift-frontend.exe",
    "swift-symbolgraph-extract.exe",
    "swift-synthesize-interface.exe",
    "clang.exe",
    "clang++.exe",
    "clang-cache.exe",
    "clang-cl.exe",
    "clang-cpp.exe",
    "lld.exe",
    "lld-link.exe",
    "ld.lld.exe",
    "ld64.lld.exe",
    "wasm-ld.exe"
  )
  $Binaries = $Tools | ForEach-Object {[IO.Path]::Combine($Platform.ToolchainInstallRoot, "usr", "bin", $_)}
  if ($IncludeNoAsserts) {
    $NoAssertBinaries = $Tools | ForEach-Object {[IO.Path]::Combine($Platform.NoAssertsToolchainInstallRoot, "usr", "bin", $_)}
    $Binaries = $Binaries + $NoAssertBinaries
  }

  $minject = "$SourceCache\mimalloc\bin\minject$BuildSuffix"
  # minject creates an intermediate path by extending the input filename.
  # Stage the input under a short name to avoid its pathname-length bug.
  $PatchBinary = [IO.Path]::Combine([IO.Path]::GetDirectoryName($minject), "binary.exe")

  try {
    foreach ($Binary in $Binaries) {
      $Name = [IO.Path]::GetFileName($Binary)

      Copy-Item -Force -LiteralPath $Binary -Destination $PatchBinary
      Invoke-Program $minject "-f" "-i" "$PatchBinary"
      Copy-Item -Force -LiteralPath $PatchBinary -Destination $Binary

      # Log the import table
      $LogFile = "$BinaryCache\$($Platform.Triple)\mimalloc\minject-log-$Name.txt"
      $ErrorFile = "$BinaryCache\$($Platform.Triple)\mimalloc\minject-log-$Name-error.txt"
      Invoke-Program "$minject" "-l" "$Binary" -OutFile $LogFile -ErrorFile $ErrorFile

      # Verify patching
      $Found = Select-String -Path $LogFile -Pattern "mimalloc"
      if (-not $Found) {
        Get-Content $ErrorFile
        throw "Failed to patch mimalloc for $Name"
      }
    }
  } finally {
    if (Test-Path -LiteralPath $PatchBinary) {
      Remove-Item -Force -LiteralPath $PatchBinary
    }
  }
}

function Build-CompilerRuntime([Hashtable] $Platform,
                               [Hashtable] $Assembler,
                               [Hashtable] $Compilers) {
  $LLVMBinaryCache = $(Get-ProjectBinaryCache $HostPlatform Stage2Compilers)

  $LITVersionStr = $(Invoke-Program $(Get-PythonExecutable) "$LLVMBinaryCache\bin\llvm-lit.py" --version)
  if (-not ($LITVersionStr -match "lit (\d+)\.\d+\.\d+.*")) {
    throw "Unexpected version string '$LITVersionStr' output from llvm-lit.py"
  }
  $LLVMVersionMajor = $Matches.1

  $InstallRoot = "$($HostPlatform.ToolchainInstallRoot)\usr\lib\clang\$LLVMVersionMajor"

  # Pick GNU-style (clang) drivers for non-MSVC targets so the MSVC-style
  # `$Compilers.C.Flags` (e.g. `/GS-`, `/Gw`) aren't passed through to the
  # NDK clang when cross-compiling compiler-rt for Android.
  $C   = if ($Platform.OS -eq [OS]::Windows) { $Compilers.C   } else { $Compilers.GNUC   }
  $CXX = if ($Platform.OS -eq [OS]::Windows) { $Compilers.CXX } else { $Compilers.GNUCXX }

  Build-CMakeProject `
    -Src $SourceCache\llvm-project\compiler-rt\lib\builtins `
    -Bin "$(Get-ProjectBinaryCache $Platform ClangBuiltins)" `
    -InstallTo $InstallRoot `
    -Platform $Platform `
    -Assembler $Assembler `
    -CCompiler $C `
    -CXXCompiler $CXX `
    -BuildTargets "install-compiler-rt" `
    -Defines @{
      LLVM_DIR = "$LLVMBinaryCache\lib\cmake\llvm";
      LLVM_ENABLE_PER_TARGET_RUNTIME_DIR = "YES";
      COMPILER_RT_DEFAULT_TARGET_ONLY = "YES";
    }

  Build-CMakeProject `
    -Src $SourceCache\llvm-project\compiler-rt `
    -Bin "$(Get-ProjectBinaryCache $Platform ClangRuntime)" `
    -InstallTo $InstallRoot `
    -Platform $Platform `
    -Assembler $Assembler `
    -CCompiler $C `
    -CXXCompiler $CXX `
    -BuildTargets "install-compiler-rt" `
    -Defines @{
      LLVM_DIR = "$LLVMBinaryCache\lib\cmake\llvm";
      LLVM_ENABLE_PER_TARGET_RUNTIME_DIR = "YES";
      COMPILER_RT_DEFAULT_TARGET_ONLY = "YES";
      COMPILER_RT_BUILD_BUILTINS = "NO";
      COMPILER_RT_BUILD_CRT = "NO";
      COMPILER_RT_BUILD_LIBFUZZER = "NO";
      COMPILER_RT_BUILD_ORC = "NO";
      COMPILER_RT_BUILD_XRAY = "NO";
      COMPILER_RT_BUILD_PROFILE = "YES";
      COMPILER_RT_BUILD_SANITIZERS = "YES";
    }
}

function Build-Brotli([Hashtable] $Platform,
                      [Hashtable] $CCompiler) {
  Build-CMakeProject `
    -Src $SourceCache\brotli `
    -Bin (Get-ProjectBinaryCache $Platform Brotli) `
    -Platform $Platform `
    -CCompiler $CCompiler `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_POSITION_INDEPENDENT_CODE = "YES";
      BROTLI_BUILD_TOOLS = "NO";
    }
}


function Build-ZLib([Hashtable] $Platform,
                    [Hashtable] $CCompiler) {
  Build-CMakeProject `
    -Src $SourceCache\zlib `
    -Bin (Get-ProjectBinaryCache $Platform ZLib) `
    -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
    -Platform $Platform `
    -CCompiler $CCompiler `
    -Defines @{
      CMAKE_POSITION_INDEPENDENT_CODE = "YES";
      ZLIB_BUILD_MINIZIP = "NO";
      ZLIB_BUILD_SHARED = "NO";
      ZLIB_BUILD_STATIC = "YES";
      ZLIB_BUILD_TESTING = "NO";
    }
}

function Build-XML2([Hashtable] $Platform,
                    [Hashtable] $CCompiler,
                    [Hashtable] $CXXCompiler,
                    [string]    $Phase) {
  Build-CMakeProject `
    -Src $SourceCache\libxml2 `
    -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Phase}XML2")) `
    -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
    -Platform $Platform `
    -CCompiler $CCompiler `
    -CXXCompiler $CXXCompiler `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_C_FLAGS = @("-w");
      CMAKE_POSITION_INDEPENDENT_CODE = "YES";
      LIBXML2_WITH_C14N = "NO";
      LIBXML2_WITH_CATALOG = "NO";
      LIBXML2_WITH_DEBUG = "NO";
      LIBXML2_WITH_FTP = "NO";
      LIBXML2_WITH_HTML = "NO";
      LIBXML2_WITH_HTTP = "NO";
      # NOTE(compnerd) this is technically needed for transcoding non-UTF-8 documents.
      LIBXML2_WITH_ICONV = "NO";
      LIBXML2_WITH_ICU = "NO";
      LIBXML2_WITH_ISO8859X = "NO";
      LIBXML2_WITH_LEGACY = "NO";
      LIBXML2_WITH_LZMA = "NO";
      LIBXML2_WITH_MEM_DEBUG = "NO";
      LIBXML2_WITH_MODULES = "NO";
      LIBXML2_WITH_OUTPUT = "YES";
      LIBXML2_WITH_PATTERN = "NO";
      LIBXML2_WITH_PROGRAMS = "NO";
      LIBXML2_WITH_PUSH = "YES";
      LIBXML2_WITH_PYTHON = "NO";
      LIBXML2_WITH_READER = "NO";
      LIBXML2_WITH_REGEXPS = "YES";
      LIBXML2_WITH_SAX1 = "NO";
      LIBXML2_WITH_SCHEMAS = "NO";
      LIBXML2_WITH_SCHEMATRON = "NO";
      LIBXML2_WITH_TESTS = "NO";
      LIBXML2_WITH_THREAD_ALLOC = "NO";
      LIBXML2_WITH_THREADS = "YES";
      LIBXML2_WITH_TREE = "YES";
      LIBXML2_WITH_VALID = "YES";
      LIBXML2_WITH_WRITER = "NO";
      LIBXML2_WITH_XINCLUDE = "NO";
      LIBXML2_WITH_XPATH = "YES";
      LIBXML2_WITH_XPTR = "NO";
      LIBXML2_WITH_XPTR_LOCS = "NO";
      LIBXML2_WITH_ZLIB = "NO";
    }
}

function Build-RegsGen2([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\ds2\Tools\RegsGen2 `
    -Bin (Get-ProjectBinaryCache $Platform RegsGen2) `
    -Platform $Platform `
    -BuildTargets default `
    -CCompiler $Compilers.Host.C `
    -CXXCompiler $Compilers.Host.CXX `
    -Defines @{
      BISON_EXECUTABLE = "$(Get-BisonExecutable)";
      FLEX_EXECUTABLE = "$(Get-FlexExecutable)";
    }
}

function Build-DS2([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src "$SourceCache\ds2" `
    -Bin "$BinaryCache\$($Platform.Triple)\ds2" `
    -InstallTo "$(Get-PlatformRoot $Platform.OS)\Developer\Library\ds2\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.Host.C `
    -CXXCompiler $Compilers.Host.CXX `
    -Defines (@{
      DS2_REGSGEN2 = "$(Get-ProjectBinaryCache $BuildPlatform RegsGen2)/regsgen2.exe";
      DS2_PROGRAM_PREFIX = "$(Get-ModuleTriple $Platform)-";
      BISON_EXECUTABLE = "$(Get-BisonExecutable)";
      FLEX_EXECUTABLE = "$(Get-FlexExecutable)";
    } + $(if ($Platform.OS -eq [OS]::Android) {
      # ds2 is a standalone executable, so keep the static C++ runtime.
      @{ CMAKE_ANDROID_STL_TYPE = "c++_static" }
    } else {
      @{}
    }))
}

function Build-BoringSSL([Hashtable] $Platform,
                         [Hashtable] $Assembler,
                         [Hashtable] $CCompiler,
                         [Hashtable] $CXXCompiler) {
  Build-CMakeProject `
    -Src $SourceCache\boringssl `
    -Bin (Get-ProjectBinaryCache $Platform BoringSSL) `
    -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
    -Platform $Platform `
    -Assembler $Assembler `
    -CCompiler $CCompiler `
    -CXXCompiler $CXXCompiler `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      BUILD_TESTING = "NO";
      CMAKE_POSITION_INDEPENDENT_CODE = "YES";
      INSTALL_ENABLED = "YES";
      OPENSSL_NO_ASM = "NO";
      RUST_BINDINGS = "NO";
    }
}

function Build-CURL([Hashtable] $Platform,
                    [Hashtable] $CCompiler) {
  $PlatformDefines = @{}
  if ($Platform.OS -eq [OS]::Android) {
    $PlatformDefines += @{
      HAVE_FSEEKO = "0";
    }
  }

  Build-CMakeProject `
    -Src $SourceCache\curl `
    -Bin (Get-ProjectBinaryCache $Platform CURL) `
    -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
    -Platform $Platform `
    -CCompiler $CCompiler `
    -Defines ($PlatformDefines + @{
      BUILD_SHARED_LIBS = "NO";
      BUILD_TESTING = "NO";
      CMAKE_POSITION_INDEPENDENT_CODE = "YES";
      BROTLI_INCLUDE_DIR = "$SourceCache\brotli\c\include";
      BROTLICOMMON_LIBRARY = if ($Platform.OS -eq [OS]::Windows) {
        "$(Get-ProjectBinaryCache $Platform Brotli)\brotlicommon.lib"
      } else {
        "$(Get-ProjectBinaryCache $Platform Brotli)\libbrotlicommon.a"
      };
      BROTLIDEC_LIBRARY = if ($Platform.OS -eq [OS]::Windows) {
        "$(Get-ProjectBinaryCache $Platform Brotli)\brotlidec.lib"
      } else {
        "$(Get-ProjectBinaryCache $Platform Brotli)\libbrotlidec.a"
      }
      BUILD_CURL_EXE = "NO";
      BUILD_EXAMPLES = "NO";
      BUILD_LIBCURL_DOCS = "NO";
      BUILD_MISC_DOCS = "NO";
      BUILD_STATIC_LIBS = "YES";
      CURL_BROTLI = "YES";
      CURL_BUILD_EVERYTHING = "NO";
      CURL_CA_BUNDLE = "none";
      CURL_CA_EMBED = "";
      CURL_CA_FALLBACK = "NO";
      CURL_CA_PATH = "none";
      CURL_CA_SEARCH_SAFE = "NO";
      CURL_CLANG_TIDY = "NO";
      CURL_CODE_COVERAGE = "NO";
      CURL_DEBUG_GLOBAL_MEM = "NO";
      CURL_DEFAULT_SSL_BACKEND = if ($Platform.OS -eq [OS]::Windows) { "schannel" } else { "openssl" };
      CURL_DISABLE_ALTSVC = "NO";
      CURL_DISABLE_AWS = "YES";
      CURL_DISABLE_BASIC_AUTH = "NO";
      CURL_DISABLE_BEARER_AUTH = "NO";
      CURL_DISABLE_BINDLOCAL = "NO";
      CURL_DISABLE_CA_SEARCH = "YES";
      CURL_DISABLE_COOKIES = "NO";
      CURL_DISABLE_DICT = "YES";
      CURL_DISABLE_DIGEST_AUTH = "NO";
      CURL_DISABLE_DOH = "NO";
      CURL_DISABLE_FILE = "YES";
      CURL_DISABLE_FORM_API = "NO";
      CURL_DISABLE_FTP = "YES";
      CURL_DISABLE_GETOPTIONS = "NO";
      CURL_DISABLE_GOPHER = "YES";
      CURL_DISABLE_HEADERS_API = "YES";
      CURL_DISABLE_HSTS = "NO";
      CURL_DISABLE_HTTP = "NO";
      CURL_DISABLE_HTTP_AUTH = "NO";
      CURL_DISABLE_IMAP = "YES";
      CURL_DISABLE_INSTALL = "NO";
      CURL_DISABLE_IPFS = "YES";
      CURL_DISABLE_KERBEROS_AUTH = "NO";
      CURL_DISABLE_LDAP = "YES";
      CURL_DISABLE_LDAPS = "YES";
      CURL_DISABLE_MIME = "NO";
      CURL_DISABLE_MQTT = "YES";
      CURL_DISABLE_NEGOTIATE_AUTH = "NO";
      CURL_DISABLE_NETRC = "NO";
      CURL_DISABLE_OPENSSL_AUTO_LOAD_CONFIG = "YES";
      CURL_DISABLE_PARSEDATE = "NO";
      CURL_DISABLE_POP3 = "YES";
      CURL_DISABLE_PROGRESS_METER = "YES";
      CURL_DISABLE_PROXY = "NO";
      CURL_DISABLE_RTSP = "YES";
      CURL_DISABLE_SHA512_256 = "NO";
      CURL_DISABLE_SHUFFLE_DNS = "YES";
      CURL_DISABLE_SMTP = "YES";
      CURL_DISABLE_SOCKETPAIR = "YES";
      CURL_DISABLE_SRP = "NO";
      CURL_DISABLE_TELNET = "YES";
      CURL_DISABLE_TFTP = "YES";
      CURL_DISABLE_TYPECHECK = "YES";
      CURL_DISABLE_VERBOSE_STRINGS = "NO";
      CURL_DISABLE_WEBSOCKETS = "NO";
      CURL_DROP_UNUSED = "YES";
      CURL_ENABLE_EXPORT_TARGET = "YES";
      CURL_ENABLE_NTLM = "NO";
      CURL_ENABLE_SMB = "NO";
      CURL_ENABLE_SSL = "YES";
      CURL_GCC_ANALYZER = "NO";
      CURL_LTO = "NO";
      CURL_NATIVE_CA = "YES";
      CURL_STATIC_CRT = "NO";
      CURL_USE_CMAKECONFIG = "YES";
      CURL_USE_GNUTLS = "NO";
      CURL_USE_GSASL = "NO";
      CURL_USE_GSSAPI = "NO";
      CURL_USE_LIBBACKTRACE = "NO";
      CURL_USE_LIBPSL = "NO";
      CURL_USE_LIBSSH = "NO";
      CURL_USE_LIBSSH2 = "NO";
      CURL_USE_LIBUV = "NO";
      CURL_USE_MBEDTLS = "NO";
      CURL_USE_OPENSSL = if ($Platform.OS -eq [OS]::Windows) { "NO" } else { "YES" };
      CURL_USE_PKGCONFIG = "NO";
      CURL_USE_RUSTLS = "NO";
      CURL_USE_SCHANNEL = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
      CURL_WINDOWS_SSPI = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
      CURL_ZLIB = "YES";
      CURL_ZSTD = "NO";
      ENABLE_ARES = "NO";
      ENABLE_CURL_MANUAL = "NO";
      ENABLE_DEBUG = "NO";
      ENABLE_IPV6 = "YES";
      ENABLE_THREADED_RESOLVER = "NO";
      ENABLE_UNICODE = "YES";
      ENABLE_UNIX_SOCKETS = "NO";
      HAVE_POLL_FINE = "NO";
      OPENSSL_ROOT_DIR = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake";
      OPENSSL_CRYPTO_LIBRARY = "$BinaryCache\$($Platform.Triple)\usr\lib\libcrypto.a"
      OPENSSL_INCLUDE_DIR = "$BinaryCache\$($Platform.Triple)\usr\include";
      OPENSSL_SSL_LIBRARY = "$BinaryCache\$($Platform.Triple)\usr\lib\libssl.a"
      OPENSSL_USE_STATIC_LIBS = "YES";
      USE_APPLE_IDN = "NO";
      USE_APPLE_SECTRUST = "NO";
      USE_ECH = "NO";
      USE_HTTPSRR = "NO";
      USE_IDN2 = "NO";
      USE_IPV6 = "YES";
      USE_NGHTTP2 = "NO";
      USE_NGTCP2 = "NO";
      USE_QUICHE = "NO";
      USE_OPENSSL = if ($Platform.OS -eq [OS]::Windows) { "NO" } else { "YES" };
      USE_SSLS_EXPORT = "NO";
      USE_WIN32_IDN = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
      USE_WIN32_LARGE_FILES = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
      USE_WIN32_LDAP = "NO";
      USE_WINDOWS_SSPI = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
      ZLIB_INCLUDE_DIR = "$BinaryCache\$($Platform.Triple)\usr\include";
      ZLIB_LIBRARY = if ($Platform.OS -eq [OS]::Windows) {
        "$BinaryCache\$($Platform.Triple)\usr\lib\libzs.lib"
      } else {
        "$BinaryCache\$($Platform.Triple)\usr\lib\libz.a"
      };
    })
}

function Write-SDKSettings([OS] $OS, [string] $Identifier = $OS.ToString()) {
  $SDKSettings = @{
    CanonicalName = $Identifier.ToLowerInvariant()
    DisplayName = $OS.ToString()
    IsBaseSDK = "YES"
    Version = "${ProductVersion}"
    VersionMap = @{}
    HeaderSearchPaths = @( "usr/include" );
    LibrarySearchPaths = @();
    DefaultProperties = @{
      PLATFORM_NAME = $OS.ToString().ToLowerInvariant()
    }
    SupportedTargets = @{
      $OS.ToString().ToLowerInvariant() = @{
        PlatformFamilyDisplayName = $OS.ToString()
        PlatformFamilyName = $OS.ToString()
      }
    }
  }
  switch ($OS) {
    Windows {
      $SDKSettings.DefaultProperties.DEFAULT_USE_RUNTIME = "MD"
      $SDKSettings.SupportedTargets.windows.LLVMTargetVendor = "unknown"
      $SDKSettings.SupportedTargets.windows.LLVMTargetSys = "windows"
      $SDKSettings.SupportedTargets.windows.LLVMTargetTripleEnvironment = "msvc"
      $SDKSettings.SupportedTargets.windows.Archs = $WindowsSDKBuilds | ForEach-Object { $_.Architecture.LLVMName } | Sort-Object
    }
    Android {
      $SDKSettings.SupportedTargets.android.LLVMTargetVendor = "unknown"
      $SDKSettings.SupportedTargets.android.LLVMTargetSys = "linux"
      $SDKSettings.SupportedTargets.android.LLVMTargetTripleEnvironment = "android${AndroidAPILevel}"
      $SDKSettings.SupportedTargets.android.Archs = $AndroidSDKBuilds | ForEach-Object { $_.Architecture.LLVMName } | Sort-Object
    }
  }
  $SDKSettings | ConvertTo-JSON -Depth 4 | Out-FIle -FilePath "$(Get-SwiftSDK -OS $OS -Identifier $Identifier)\SDKSettings.json"
  Write-PList -Settings $SDKSettings -Path "$(Get-SwiftSDK -OS $OS -Identifier $Identifier)\SDKSettings.plist"
}

function Test-Dispatch {
  Invoke-IsolatingEnvVars {
    $env:CTEST_OUTPUT_ON_FAILURE = "YES"

    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-libdispatch `
      -Bin (Get-ProjectBinaryCache $BuildPlatform DynamicDispatch) `
      -Platform $BuildPlatform `
      -CCompiler $Compilers.Stage1.C `
      -CXXCompiler $Compilers.Stage1.CXX `
      -SwiftCompiler $Compilers.Stage1.Swift `
      -SwiftSDK (Get-SwiftSDK -OS $BuildPlatform.OS) `
      -BuildTargets default,ExperimentalTest `
      -Defines @{
        BUILD_TESTING = "YES";
        ENABLE_SWIFT = "YES";
      }
  }
}

function Test-Foundation {
  $ScratchPath = "$BinaryCache\$($HostPlatform.Triple)\FoundationTests"

  # Foundation tests build via swiftpm rather than CMake
  Build-SPMProject `
    -Action Test `
    -Src $SourceCache\swift-foundation `
    -Bin "$ScratchPath" `
    -Platform $HostPlatform `
    -Configuration $FoundationTestConfiguration `
    --multiroot-data-file "$SourceCache\swift\utils\build_swift\resources\SwiftPM-Unified-Build.xcworkspace" `
    --test-product swift-foundationPackageTests

  Invoke-IsolatingEnvVars {
    $env:DISPATCH_INCLUDE_PATH="$(Get-SwiftSDK -OS $HostPlatform.OS)/usr/include"
    $env:LIBXML_LIBRARY_PATH="$BinaryCache/$($HostPlatform.Triple)/usr/lib"
    $env:LIBXML_INCLUDE_PATH="$BinaryCache/$($HostPlatform.Triple)/usr/include/libxml2"
    $env:ZLIB_LIBRARY_PATH="$BinaryCache/$($HostPlatform.Triple)/usr/lib"
    $env:BROTLI_LIBRARY_PATH="$(Get-ProjectBinaryCache $HostPlatform brotli)"
    $env:CURL_LIBRARY_PATH="$BinaryCache/$($HostPlatform.Triple)/usr/lib"
    $env:CURL_INCLUDE_PATH="$BinaryCache/$($HostPlatform.Triple)/usr/include"
    Build-SPMProject `
      -Action Test `
      -Src $SourceCache\swift-corelibs-foundation `
      -Bin "$ScratchPath" `
      -Platform $HostPlatform `
      -Configuration $FoundationTestConfiguration `
      --multiroot-data-file "$SourceCache\swift\utils\build_swift\resources\SwiftPM-Unified-Build.xcworkspace" `
      --test-product swift-corelibs-foundationPackageTests
  }
}

function Build-FoundationMacros([Hashtable] $Platform,
                                [Hashtable] $SwiftCompiler,
                                [string]    $SwiftSDK,
                                [string]    $SwiftSyntax_DIR) {
  Build-CMakeProject `
    -Src $SourceCache\swift-foundation\Sources\FoundationMacros `
    -Bin (Get-ProjectBinaryCache $Platform FoundationMacros) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -SwiftCompiler $SwiftCompiler `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      SwiftSyntax_DIR = $SwiftSyntax_DIR;
    }
}

function Build-XCTest([Hashtable] $Platform,
                      [Hashtable] $Compilers,
                      [string]    $SwiftSDK) {
  $SwiftFlags = if ($Platform.OS -eq [OS]::Windows) {
    @();
  } else {
    @("-I$(Get-SwiftSDK -OS $Platform.OS)\usr\include");
  }

  Build-CMakeProject `
    -Src $SourceCache\swift-corelibs-xctest `
    -Bin (Get-ProjectBinaryCache $Platform XCTest) `
    -InstallTo "$([IO.Path]::Combine((Get-PlatformRoot $Platform.OS), "Developer", "Library", "XCTest-$ProductVersion", "usr"))" `
    -Platform $Platform `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_INSTALL_BINDIR = $Platform.BinaryDir;
      CMAKE_Swift_FLAGS = $SwiftFlags;
      ENABLE_TESTING = "NO";
      XCTest_INSTALL_NESTED_SUBDIR = "YES";
      SwiftTesting_DIR = (Get-ProjectCMakeModules $Platform Testing);
      SwiftTestingMacros_DIR = (Get-ProjectCMakeModules $Platform BootstrapTestingMacros);
    }
}

function Test-XCTest {
  Invoke-IsolatingEnvVars {
    $SwiftRuntime          = [IO.Path]::Combine((Get-InstallDir $HostPlatform), "Runtimes", "$ProductVersion")
    $DispatchBinaryCache   = Get-ProjectBinaryCache $HostPlatform DynamicDispatch
    $FoundationBinaryCache = Get-ProjectBinaryCache $HostPlatform DynamicFoundation

    $env:Path = "$(Get-ProjectBinaryCache $HostPlatform XCTest);$(Get-ProjectBinaryCache $BuildPlatform Testing)\bin;${FoundationBinaryCache}\bin;${DispatchBinaryCache};${SwiftRuntime}\usr\bin;${env:Path};$UnixToolsBinDir"
    $env:SDKROOT = Get-SwiftSDK -OS $HostPlatform.OS

    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-xctest `
      -Bin (Get-ProjectBinaryCache $HostPlatform XCTest) `
      -Platform $HostPlatform `
      -CCompiler $Compilers.Stage1.C `
      -CXXCompiler $Compilers.Stage1.CXX `
      -SwiftCompiler $Compilers.Stage1.Swift `
      -SwiftSDK (Get-SwiftSDK -OS $HostPlatform.OS) `
      -BuildTargets default,check-xctest `
      -Defines @{
        ENABLE_TESTING = "YES";
        LLVM_DIR = "$(Get-ProjectBinaryCache $HostPlatform Stage2Compilers)\lib\cmake\llvm";
        XCTEST_PATH_TO_FOUNDATION_BUILD = $FoundationBinaryCache;
        XCTEST_PATH_TO_LIBDISPATCH_BUILD = $DispatchBinaryCache;
        XCTEST_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
      }
  }
}

function Build-Testing([Hashtable] $Platform,
                       [Hashtable] $Compilers,
                       [string]    $SwiftSDK) {
  $SwiftFlags = if ($Platform.OS -eq [OS]::Windows) {
    @();
  } else {
    @("-I$(Get-SwiftSDK -OS $Platform.OS)\usr\include");
  }

  # `$Compilers.CXX` is clang-cl with MSVC-style flags; for Android pick the
  # GNU driver so the NDK clang doesn't reject `/GS-` etc.
  $CXX = if ($Platform.OS -eq [OS]::Windows) { $Compilers.CXX } else { $Compilers.GNUCXX }

  Build-CMakeProject `
    -Src $SourceCache\swift-testing `
    -Bin (Get-ProjectBinaryCache $Platform Testing) `
    -InstallTo "$([IO.Path]::Combine((Get-PlatformRoot $Platform.OS), "Developer", "Library", "Testing-$ProductVersion", "usr"))" `
    -Platform $Platform `
    -CXXCompiler $CXX `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_INSTALL_BINDIR = $Platform.BinaryDir;
      CMAKE_Swift_FLAGS = $SwiftFlags;
      SwiftTesting_MACRO = "$(Get-ProjectBinaryCache $BuildPlatform BootstrapTestingMacros)\TestingMacros.dll";
      SwiftTesting_INSTALL_NESTED_SUBDIR = "YES";
    }
}

function Test-Testing {
  throw "testing Testing is not supported"
}

function Write-PlatformInfoPlist($PlatformOrOS) {
  $OS = if ($PlatformOrOS -is [Hashtable]) { $PlatformOrOS.OS } else { [OS]$PlatformOrOS }
  $Settings = @{
    DefaultProperties = @{
      SWIFT_TESTING_VERSION = "$ProductVersion"
      XCTEST_VERSION = "$ProductVersion"
    }
  }
  if ($OS -eq [OS]::Windows) {
    $Settings.DefaultProperties.SWIFTC_FLAGS = @( "-use-ld=lld" )
  }

  Write-PList -Settings $Settings -Path "$(Get-PlatformRoot $OS)\Info.plist"
}

function Get-SelectedSDKBuilds() {
  $Builds = @()
  if ($Windows) { $Builds += $WindowsSDKBuilds }
  if ($Android) { $Builds += $AndroidSDKBuilds }
  return $Builds
}

# Promotes C module header directories that libdispatch and Foundation install
# under usr/lib/swift[_static]/<Module>/ into usr/include/ so that SDK-based
# consumers can locate the corresponding module maps via the standard include
# search path. CMake consumers receive a -vfsoverlay from the target's PUBLIC
# interface; SDK consumers do not and therefore need the physical headers.
function Repair-SDKHeaders([string] $SDKRoot) {
  foreach ($Module in ("Block", "dispatch", "os", "_foundation_unicode", "_FoundationCShims")) {
    foreach ($ResourceType in ("swift", "swift_static")) {
      $ModuleDirectory = "$SDKRoot\usr\lib\$ResourceType\$Module"
      if (Test-Path $ModuleDirectory) {
        Move-Directory $ModuleDirectory "$SDKRoot\usr\include\"
      }
    }
  }
}

# Completes the SDK layout after all architecture slices have been installed.
# This reshapes Swift modules and mirrors resources whose CMake install rules
# do not distinguish between dynamic and static resource trees.
function Install-SDK([Hashtable[]] $Platforms, [OS] $OS = $Platforms[0].OS, [string] $Identifier = $OS.ToString()) {
  $SDKRoot = Get-SwiftSDK -OS $OS -Identifier $Identifier
  Repair-SDKHeaders $SDKRoot

  # Copy files from the arch subdirectory, including "*.swiftmodule" which need restructuring
  foreach ($Platform in $Platforms) {
    foreach ($ResourceType in ("swift", "swift_static")) {
      $PlatformResources = "$(Get-SwiftSDK -OS $OS -Identifier $Identifier)\usr\lib\$ResourceType\$($OS.ToString().ToLowerInvariant())"
      Get-ChildItem -ErrorAction SilentlyContinue -Recurse "$PlatformResources\$($Platform.Architecture.LLVMName)" | ForEach-Object {
        if (".swiftmodule", ".swiftdoc", ".swiftinterface" -contains $_.Extension) {
          Write-Host -BackgroundColor DarkRed -ForegroundColor White "$($_.FullName) is not in a thick module layout"
          Copy-File $_.FullName "$PlatformResources\$($_.BaseName).swiftmodule\$(Get-ModuleTriple $Platform)$($_.Extension)"
        }
      }
    }
  }

  $StaticPlatforms = @($Platforms | Where-Object { $_.LinkModes.Contains("static") })
  if ($OS -eq [OS]::Android -and $StaticPlatforms.Count -gt 0) {
    $DynamicResources = "$SDKRoot\usr\lib\swift"
    $StaticResources = "$SDKRoot\usr\lib\swift_static"

    # SwiftBuild currently passes the NDK sysroot as both -sdk and -sysroot,
    # while using Android.sdk only for the static -resource-dir. Unlike a direct
    # swiftc invocation, it cannot keep Android.sdk as -sdk and the NDK as
    # -sysroot, so these SDK resources must be copied into the static tree.
    Copy-Directory "$DynamicResources\shims\*" "$StaticResources\shims"
    foreach ($File in ("libcxxshim.h", "libcxxshim.modulemap", "libcxxstdlibshim.h")) {
      Copy-File "$DynamicResources\android\$File" "$StaticResources\android\$File"
    }

    foreach ($Platform in $StaticPlatforms) {
      $Architecture = $Platform.Architecture.LLVMName
      $DynamicArchitectureResources = "$DynamicResources\android\$Architecture"
      $StaticArchitectureResources = "$StaticResources\android\$Architecture"

      # The Android Clang overlay also has a fixed dynamic-resource install
      # destination. Keep the architecture-specific copies together until
      # swiftlang/swift#80293 allows the overlay to be shared by all slices.
      foreach ($File in ("android.modulemap", "SwiftAndroidNDK.h", "SwiftBionic.h")) {
        Copy-File "$DynamicArchitectureResources\$File" "$StaticArchitectureResources\$File"
      }
    }
  }
}

function Build-SDKDependencies([Hashtable[]] $ArchitectureSlices,
                               [Hashtable]   $Compilers) {
  foreach ($Slice in $ArchitectureSlices) {
    # `$Compilers.C/.CXX` are clang-cl drivers configured with MSVC-style
    # flags (`/GS-`, `/Gw`, ...).  For non-Windows targets the NDK clang
    # rejects those — pick the GNU drivers (`-fno-stack-protector`, ...).
    $C   = if ($Slice.OS -eq [OS]::Windows) { $Compilers.C   } else { $Compilers.GNUC   }
    $CXX = if ($Slice.OS -eq [OS]::Windows) { $Compilers.CXX } else { $Compilers.GNUCXX }
    if ($IncludeDS2) { Invoke-BuildStep Build-DS2 $Slice }
    Invoke-BuildStep Build-ZLib $Slice -CCompiler $C
    Invoke-BuildStep Build-Brotli $Slice -CCompiler $C
    Invoke-BuildStep Build-XML2 $Slice -CCompiler $C -CXXCompiler $CXX -Phase ""
    if ($Slice.OS -ne [OS]::Windows) {
      Invoke-BuildStep Build-BoringSSL $Slice -Assembler $Assemblers.Stage1 -CCompiler $C -CXXCompiler $CXX
    }
    Invoke-BuildStep Build-CURL $Slice -CCompiler $C
  }
}

# TODO(compnerd): remove this Build-SDK repair once libdispatch and
# Foundation embed their Windows assembly manifests in their own builds.
function Repair-WindowsSDKAssemblyManifests([Hashtable] $Platform, [string] $RuntimeBin) {
  $Targets = New-Object System.Collections.Generic.List[string]

  foreach ($DLL in @(
    "BlocksRuntime",
    "dispatch",
    "swiftDispatch",
    "Foundation",
    "FoundationEssentials",
    "FoundationInternationalization",
    "FoundationNetworking",
    "FoundationXML",
    "_FoundationICU"
  )) {
    $Path = Join-Path $RuntimeBin "$DLL.dll"
    if (Test-Path $Path) { [void]$Targets.Add($Path) }
  }

  if ($Targets.Count -eq 0) {
    return
  }

  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $Platform
    foreach ($Path in ($Targets | Sort-Object -Unique)) {
      Ensure-WindowsAssemblyManifest `
        -ImagePath              $Path `
        -AssemblyVersion        (ConvertTo-FourPartVersion $ProductVersion) `
        -ProcessorArchitecture  $Platform.Architecture.VSName `
        -PublicKeyToken         $WindowsSxSAssemblyPublicKeyToken `
        -LogPrefix              "Repair-WindowsSDKAssemblyManifests"
      Assert-WindowsManifestResourcesAreSxSSafe $Path "Repair-WindowsSDKAssemblyManifests"
    }
  }
}

$SDKSupplementalRuntimes = @(
  "StringProcessing",
  "Synchronization",
  "Distributed",
  "Observation",
  "Differentiation",
  "Volatile",
  "Runtime"
)

function Build-SDK([Hashtable] $Platform, [Hashtable] $Context) {
  $SDKIdentifier         = $Context.SDKIdentifier
  $Variant               = $Context.Variant
  $Compilers             = $Context.Compilers
  $Static                = [bool]$Context.Static
  $BuildFoundation       = [bool]$Context.BuildFoundation
  $SupplementalRuntimes  = @($Context.SupplementalRuntimes)
  $InstallRuntimeToStage = $Platform.OS -eq [OS]::Windows
  if ($Context.ContainsKey("InstallRuntimeToStage")) {
    $InstallRuntimeToStage = [bool]$Context.InstallRuntimeToStage
  }
  $SDKRoot               = Get-SwiftSDK -OS $Platform.OS -Identifier $SDKIdentifier
  $SDKRuntimeBin         = Get-SDKRuntimeBin $Platform $SDKRoot $InstallRuntimeToStage
  $SDKLibexecDir         = Get-SDKLibexecDir $Platform $SDKRoot $InstallRuntimeToStage
  # Windows SDK builds can redirect runtime install destinations out of the SDK
  # root. Bootstrap disables that through InstallRuntimeToStage.
  $SDKInstallDefines     = @{}
  if ($Platform.OS -eq [OS]::Windows) {
    $SDKInstallDefines.CMAKE_INSTALL_BINDIR = $SDKRuntimeBin
    $SDKInstallDefines.CMAKE_INSTALL_LIBEXECDIR = $SDKLibexecDir
  }
  $BUILD_SHARED_LIBS     = if ($Static) { "NO" } else { "YES" }
  $RuntimeBinaryCache    = Get-ProjectBinaryCache $Platform ([Project]"${Variant}Runtime")
  $OverlayBinaryCache    = Get-ProjectBinaryCache $Platform ([Project]"${Variant}Overlay")

  # TODO: remove this once the migration is completed.
  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $BuildPlatform

    Push-Location "${SourceCache}\swift\Runtimes"
    $env:CMAKE_ROOT = (Get-CMake).CMakeRoot
    $CMakeBin = (Get-CMake).Path
    Start-Process -Wait -WindowStyle Hidden -FilePath $CMakeBin -ArgumentList @("-P", "Resync.cmake")
    Pop-Location
  }

  Invoke-IsolatingEnvVars {
    # TODO(compnerd) build statically for static builds
    # Match Build-SDKDependencies: pick GNU drivers for non-Windows targets.
    $CDispatchC   = if ($Platform.OS -eq [OS]::Windows) { $Compilers.C   } else { $Compilers.GNUC   }
    $CDispatchCXX = if ($Platform.OS -eq [OS]::Windows) { $Compilers.CXX } else { $Compilers.GNUCXX }
    Build-CDispatch $Platform -CCompiler $CDispatchC -CXXCompiler $CDispatchCXX -Phase $Variant
    if ($BuildFoundation) {
      Build-SDKDependencies @($Platform) -Compilers $Compilers
    }

    # ── Core ──────────────────────────────────────────────────────────────────
    Record-OperationTime $Platform "Build-${Variant}Runtime" {
      Build-CMakeProject `
        -Src $SourceCache\swift\Runtimes\Core `
        -Bin $RuntimeBinaryCache `
        -InstallTo "$SDKRoot\usr" `
        -Platform $Platform `
        -CCompiler $Compilers.GNUC `
        -CXXCompiler $Compilers.GNUCXX `
        -SwiftCompiler $Compilers.Swift `
        -SwiftSDK $null `
        -Defines ($SDKInstallDefines + @{
          BUILD_SHARED_LIBS = $BUILD_SHARED_LIBS;
          CMAKE_NINJA_FORCE_RESPONSE_FILE = "YES";
          CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
          SWIFT_ASSEMBLY_VERSION = "$ProductVersion";
          SWIFT_PUBLIC_KEY_TOKEN = "$WindowsSxSAssemblyPublicKeyToken";

          dispatch_DIR = (Get-ProjectCMakeModules $Platform ([Project]"${Variant}CDispatch"));

          # FIXME(hjyamauchi) Should dynamic to libdispatch https://github.com/swiftlang/swift/issues/87548
          CMAKE_CXX_FLAGS   = if ($Static) { @("-Ddispatch_STATIC") } else { @() };
          CMAKE_Swift_FLAGS = $(if ($Static) { @("-Xcc", "-static-libclosure") } else { @() });

          # FIXME(carlpeto) remove this once the default option is flipped to `ON`.
          SwiftCore_ENABLE_ENVIRONMENT       = "YES";
          # FIXME(compnerd) remove this once the default option is flipped to `ON`.
          SwiftCore_ENABLE_BACKTRACING       = "YES";
          # FIXME(compnerd) remove this once the default option is flipped to `ON`.
          SwiftCore_ENABLE_CONCURRENCY       = "YES";
          # FIXME(compnerd) remove this once the default option is flipped to `ON`.
          SwiftCore_ENABLE_REMOTE_MIRROR     = "YES";
          # FIXME(compnerd) this currently causes a build failure on Windows, but
          # this should be enabled when building the dynamic runtime.
          SwiftCore_ENABLE_LIBRARY_EVOLUTION = "NO";
        })
    }

    # ── Overlay ────────────────────────────────────────────────────────────────
    Record-OperationTime $Platform "Build-${Variant}Overlay" {
      Build-CMakeProject `
        -Src $SourceCache\swift\Runtimes\Overlay `
        -Bin $OverlayBinaryCache `
        -InstallTo "$SDKRoot\usr" `
        -Platform $Platform `
        -CCompiler $Compilers.GNUC `
        -CXXCompiler $Compilers.GNUCXX `
        -SwiftCompiler $Compilers.Swift `
        -SwiftSDK $null `
        -Defines ($SDKInstallDefines + @{
          BUILD_SHARED_LIBS = $BUILD_SHARED_LIBS;
          CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
          SWIFT_ASSEMBLY_VERSION = "$ProductVersion";
          SWIFT_PUBLIC_KEY_TOKEN = "$WindowsSxSAssemblyPublicKeyToken";

          SwiftCore_DIR = "$RuntimeBinaryCache\cmake\SwiftCore";

          SwiftOverlay_ENABLE_CXX_INTEROP = "YES";
          # FIXME(compnerd) this currently causes a build failure on Windows, but
          # this should be enabled when building the dynamic runtime.
          SwiftOverlay_ENABLE_LIBRARY_EVOLUTION = "NO";
        })
    }


    if ($SupplementalRuntimes -contains "StringProcessing") {
      Record-OperationTime $Platform "Build-${Variant}StringProcessing" {
        Build-CMakeProject `
          -Src $SourceCache\swift\Runtimes\Supplemental\StringProcessing `
          -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Variant}StringProcessing")) `
          -InstallTo "$SDKRoot\usr" `
          -Platform $Platform `
          -CCompiler $Compilers.GNUC `
          -CXXCompiler $Compilers.GNUCXX `
          -SwiftCompiler $Compilers.Swift `
          -SwiftSDK $null `
          -Defines ($SDKInstallDefines + @{
            BUILD_SHARED_LIBS = $BUILD_SHARED_LIBS;
            CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
            SWIFT_ASSEMBLY_VERSION = "$ProductVersion";
            SWIFT_PUBLIC_KEY_TOKEN = "$WindowsSxSAssemblyPublicKeyToken";

            SwiftCore_DIR = "$RuntimeBinaryCache\cmake\SwiftCore";

            # FIXME(compnerd) this currently causes a build failure on Windows, but
            # this should be enabled when building the dynamic runtime.
            SwiftStringProcessing_ENABLE_LIBRARY_EVOLUTION = "NO";
          })
      }
    }

    # ── Supplementals ─────────────────────────────────────────────────────────
    if ($SupplementalRuntimes -contains "Synchronization") {
      Record-OperationTime $Platform "Build-${Variant}Synchronization" {
        Build-CMakeProject `
          -Src $SourceCache\swift\Runtimes\Supplemental\Synchronization `
          -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Variant}Synchronization")) `
          -InstallTo "$SDKRoot\usr" `
          -Platform $Platform `
          -CCompiler $Compilers.GNUC `
          -SwiftCompiler $Compilers.Swift `
          -SwiftSDK $null `
          -Defines ($SDKInstallDefines + @{
            BUILD_SHARED_LIBS = $BUILD_SHARED_LIBS;
            CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
            SWIFT_ASSEMBLY_VERSION = "$ProductVersion";
            SWIFT_PUBLIC_KEY_TOKEN = "$WindowsSxSAssemblyPublicKeyToken";

            SwiftCore_DIR    = "$RuntimeBinaryCache\cmake\SwiftCore";
            SwiftOverlay_DIR = "$OverlayBinaryCache\cmake\SwiftOverlay";

            # FIXME(compnerd) this currently causes a build failure on Windows, but
            # this should be enabled when building the dynamic runtime.
            SwiftSynchronization_ENABLE_LIBRARY_EVOLUTION = "NO";
          })
      }
    }

    if ($SupplementalRuntimes -contains "Distributed") {
      Record-OperationTime $Platform "Build-${Variant}Distributed" {
        Build-CMakeProject `
          -Src $SourceCache\swift\Runtimes\Supplemental\Distributed `
          -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Variant}Distributed")) `
          -InstallTo "$SDKRoot\usr" `
          -Platform $Platform `
          -CCompiler $Compilers.GNUC `
          -CXXCompiler $Compilers.GNUCXX `
          -SwiftCompiler $Compilers.Swift `
          -SwiftSDK $null `
          -Defines ($SDKInstallDefines + @{
            BUILD_SHARED_LIBS = $BUILD_SHARED_LIBS;
            # FIXME(#83449): avoid using `SwiftCMakeConfig.h`
            CMAKE_CXX_FLAGS = @("-I$RuntimeBinaryCache\include");
            CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
            SWIFT_ASSEMBLY_VERSION = "$ProductVersion";
            SWIFT_PUBLIC_KEY_TOKEN = "$WindowsSxSAssemblyPublicKeyToken";

            SwiftCore_DIR    = "$RuntimeBinaryCache\cmake\SwiftCore";
            SwiftOverlay_DIR = "$OverlayBinaryCache\cmake\SwiftOverlay";

            # FIXME(compnerd) this currently causes a build failure on Windows, but
            # this should be enabled when building the dynamic runtime.
            SwiftDistributed_ENABLE_LIBRARY_EVOLUTION = "NO";
          })
      }
    }

    if ($SupplementalRuntimes -contains "Observation") {
      Record-OperationTime $Platform "Build-${Variant}Observation" {
        Build-CMakeProject `
          -Src $SourceCache\swift\Runtimes\Supplemental\Observation `
          -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Variant}Observation")) `
          -InstallTo "$SDKRoot\usr" `
          -Platform $Platform `
          -CXXCompiler $Compilers.GNUCXX `
          -SwiftCompiler $Compilers.Swift `
          -SwiftSDK $null `
          -Defines ($SDKInstallDefines + @{
            BUILD_SHARED_LIBS = $BUILD_SHARED_LIBS;
            # FIXME(#83449): avoid using `SwiftCMakeConfig.h`
            CMAKE_CXX_FLAGS = @("-I$RuntimeBinaryCache\include");
            CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
            SWIFT_ASSEMBLY_VERSION = "$ProductVersion";
            SWIFT_PUBLIC_KEY_TOKEN = "$WindowsSxSAssemblyPublicKeyToken";

            SwiftCore_DIR    = "$RuntimeBinaryCache\cmake\SwiftCore";
            SwiftOverlay_DIR = "$OverlayBinaryCache\cmake\SwiftOverlay";

            # FIXME(compnerd) this currently causes a build failure on Windows, but
            # this should be enabled when building the dynamic runtime.
            SwiftObservation_ENABLE_LIBRARY_EVOLUTION = "NO";
          })
      }
    }

    if ($SupplementalRuntimes -contains "Differentiation") {
      Record-OperationTime $Platform "Build-${Variant}Differentiation" {
        Build-CMakeProject `
          -Src $SourceCache\swift\Runtimes\Supplemental\Differentiation `
          -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Variant}Differentiation")) `
          -InstallTo "$SDKRoot\usr" `
          -Platform $Platform `
          -CCompiler $Compilers.GNUC `
          -CXXCompiler $Compilers.GNUCXX `
          -SwiftCompiler $Compilers.Swift `
          -SwiftSDK $null `
          -Defines ($SDKInstallDefines + @{
            BUILD_SHARED_LIBS = $BUILD_SHARED_LIBS;
            CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
            SWIFT_ASSEMBLY_VERSION = "$ProductVersion";
            SWIFT_PUBLIC_KEY_TOKEN = "$WindowsSxSAssemblyPublicKeyToken";

            SwiftCore_DIR    = "$RuntimeBinaryCache\cmake\SwiftCore";
            SwiftOverlay_DIR = "$OverlayBinaryCache\cmake\SwiftOverlay";

            # FIXME(compnerd) this currently causes a build failure on Windows, but
            # this should be enabled when building the dynamic runtime.
            SwiftDifferentiation_ENABLE_LIBRARY_EVOLUTION = "NO";
          })
      }
    }

    if ($SupplementalRuntimes -contains "Volatile") {
      Record-OperationTime $Platform "Build-${Variant}Volatile" {
        Build-CMakeProject `
          -Src $SourceCache\swift\Runtimes\Supplemental\Volatile `
          -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Variant}Volatile")) `
          -InstallTo "$SDKRoot\usr" `
          -Platform $Platform `
          -CCompiler $Compilers.GNUC `
          -SwiftCompiler $Compilers.Swift `
          -SwiftSDK $null `
          -Defines ($SDKInstallDefines + @{
            BUILD_SHARED_LIBS = $BUILD_SHARED_LIBS;
            CMAKE_FIND_PACKAGE_PREFER_CONFIG = "YES";
            CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
            SWIFT_ASSEMBLY_VERSION = "$ProductVersion";
            SWIFT_PUBLIC_KEY_TOKEN = "$WindowsSxSAssemblyPublicKeyToken";

            SwiftCore_DIR    = "$RuntimeBinaryCache\cmake\SwiftCore";
            SwiftOverlay_DIR = "$OverlayBinaryCache\cmake\SwiftOverlay";

            # FIXME(compnerd) this currently causes a build failure on Windows, but
            # this should be enabled when building the dynamic runtime.
            SwiftVolatile_ENABLE_LIBRARY_EVOLUTION = "NO";
          })
      }
    }

    if ($SupplementalRuntimes -contains "Runtime" -and $Platform.OS -ne [OS]::Android) {
      Record-OperationTime $Platform "Build-${Variant}RuntimeModule" {
        Build-CMakeProject `
          -Src $SourceCache\swift\Runtimes\Supplemental\Runtime `
          -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Variant}RuntimeModule")) `
          -InstallTo "$SDKRoot\usr" `
          -Platform $Platform `
          -CCompiler $Compilers.GNUC `
          -CXXCompiler $Compilers.GNUCXX `
          -SwiftCompiler $Compilers.Swift `
          -SwiftSDK $null `
          -Defines ($SDKInstallDefines + @{
            BUILD_SHARED_LIBS = $BUILD_SHARED_LIBS;
            CMAKE_FIND_PACKAGE_PREFER_CONFIG = "YES";
            CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
            SWIFT_ASSEMBLY_VERSION = "$ProductVersion";
            SWIFT_PUBLIC_KEY_TOKEN = "$WindowsSxSAssemblyPublicKeyToken";

            SwiftCore_DIR       = "$RuntimeBinaryCache\cmake\SwiftCore";
            SwiftOverlay_DIR    = "$OverlayBinaryCache\cmake\SwiftOverlay";
            SwiftCxxOverlay_DIR = "$OverlayBinaryCache\Cxx\cmake\SwiftCxxOverlay";

            # FIXME(compnerd) this currently causes a build failure on Windows, but
            # this should be enabled when building the dynamic runtime.
            SwiftRuntime_ENABLE_LIBRARY_EVOLUTION = "NO";

            SwiftRuntime_ENABLE_BACKTRACING = "YES";
          })
      }
    }

    # ── Dispatch ──────────────────────────────────────────────────────────────
    Record-OperationTime $Platform "Build-${Variant}Dispatch" {
      $DispatchDefines = @{
        BUILD_TESTING                     = "NO";
        BUILD_SHARED_LIBS                 = $BUILD_SHARED_LIBS;
        CMAKE_FIND_PACKAGE_PREFER_CONFIG  = "YES";
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
        CMAKE_Swift_FLAGS                 = if ($Static) {
                                              @("-static-stdlib", "-Xfrontend", "-use-static-resource-dir")
                                            } else {
                                              @()
                                            };
        ENABLE_SWIFT                      = "YES";
        dispatch_INSTALL_ARCH_SUBDIR      = "YES";
      }
      $DispatchDefines = $SDKInstallDefines + $DispatchDefines
      if ($Platform.OS -eq [OS]::Android) {
        # libdispatch doesn't use libc++, but with the shared C++ runtime it
        # would take `__emutls_get_address` from it and depend on
        # libc++_shared.so.
        $DispatchDefines.CMAKE_ANDROID_STL_TYPE = "c++_static"
      }

      Build-CMakeProject `
        -Src $SourceCache\swift-corelibs-libdispatch `
        -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Variant}Dispatch")) `
        -InstallTo "$SDKRoot\usr" `
        -Platform $Platform `
        -CCompiler $Compilers.GNUC `
        -CXXCompiler $Compilers.GNUCXX `
        -SwiftCompiler $Compilers.Swift `
        -SwiftSDK $SDKRoot `
        -Defines $DispatchDefines
    }

    Repair-SDKHeaders $SDKRoot

    # ── Pre-Foundation SDK header repair ──────────────────────────────────────
    # Redirect the dispatch VFS overlay so the virtual module-map path recorded
    # in Dispatch.swiftmodule (at dispatch compile time) and swiftc's automatic
    # -isystem search both resolve to the same physical installed file.  This
    # prevents "cannot load underlying module for 'Dispatch'" on fresh builds
    # (no module.modulemap at the source-tree virtual path) and "redefinition of
    # module 'Dispatch'" on incremental builds (two copies otherwise found).
    $DispatchVFS = "$(Get-ProjectBinaryCache $Platform ([Project]"${Variant}Dispatch"))\dispatch-vfs-overlay.yaml"
    if (Test-Path $DispatchVFS) {
      $DispatchSrc  = "$SourceCache\swift-corelibs-libdispatch\dispatch".Replace('\', '/')
      $InstalledMap = "$SDKRoot\usr\include\dispatch\module.modulemap".Replace('\', '/')
      Set-Content -Path $DispatchVFS -Encoding UTF8 -Value @"
---
version: 0
case-sensitive: false
use-external-names: false
roots:
  - name: '$DispatchSrc'
    type: directory
    contents:
      - name: module.modulemap
        type: file
        external-contents: '$InstalledMap'
"@
    }

    if ($BuildFoundation) {
      # ── Foundation ────────────────────────────────────────────────────────────
      Record-OperationTime $Platform "Build-${Variant}Foundation" {
        $FoundationDefines = @{
          BUILD_SHARED_LIBS                 = $BUILD_SHARED_LIBS;
          CMAKE_FIND_PACKAGE_PREFER_CONFIG  = "YES";
          CMAKE_NINJA_FORCE_RESPONSE_FILE   = "YES";
          CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
          CMAKE_Swift_FLAGS                 = if ($Static) {
                                                @("-static-stdlib", "-Xfrontend", "-use-static-resource-dir")
                                              } else {
                                                @()
                                              };
          ENABLE_TESTING                    = "NO";
          BROTLI_INCLUDE_DIR                = "$SourceCache\brotli\c\include";
          BROTLICOMMON_LIBRARY              = if ($Platform.OS -eq [OS]::Windows) {
                                                "$(Get-ProjectBinaryCache $Platform Brotli)\brotlicommon.lib";
                                              } else {
                                                "$(Get-ProjectBinaryCache $Platform Brotli)\libbrotlicommon.a";
                                              };
          BROTLIDEC_LIBRARY                 = if ($Platform.OS -eq [OS]::Windows) {
                                                "$(Get-ProjectBinaryCache $Platform Brotli)\brotlidec.lib";
                                              } else {
                                                "$(Get-ProjectBinaryCache $Platform Brotli)\libbrotlidec.a";
                                              };
          FOUNDATION_BUILD_TOOLS            = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
          CURL_DIR                          = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake\CURL";
          LibXml2_DIR                       = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake\libxml2-2.11.5";
          ZLIB_LIBRARY                      = if ($Platform.OS -eq [OS]::Windows) {
                                                "$BinaryCache\$($Platform.Triple)\usr\lib\libzs.lib"
                                              } else {
                                                "$BinaryCache\$($Platform.Triple)\usr\lib\libz.a"
                                              };
          ZLIB_INCLUDE_DIR                  = "$BinaryCache\$($Platform.Triple)\usr\include";
          dispatch_DIR                      = Get-ProjectCMakeModules $Platform ([Project]"${Variant}Dispatch");
          _SwiftFoundation_SourceDIR        = "$SourceCache\swift-foundation";
          _SwiftFoundationICU_SourceDIR     = "$SourceCache\swift-foundation-icu";
          _SwiftCollections_SourceDIR       = "$SourceCache\swift-collections";
          SwiftFoundation_MACRO             = "$(Get-ProjectBinaryCache $BuildPlatform BootstrapFoundationMacros)\bin";
        }
        $PlatformDefines = if ($Platform.OS -eq [OS]::Windows) {
          @{};
        } else {
          @{
            OPENSSL_ROOT_DIR        = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake";
            OPENSSL_CRYPTO_LIBRARY  = "$BinaryCache\$($Platform.Triple)\usr\lib\libcrypto.a"
            OPENSSL_INCLUDE_DIR     = "$BinaryCache\$($Platform.Triple)\usr\include";
            OPENSSL_SSL_LIBRARY     = "$BinaryCache\$($Platform.Triple)\usr\lib\libssl.a"
            OPENSSL_USE_STATIC_LIBS = "YES";
          };
        }
        $FoundationDefines = $SDKInstallDefines + $FoundationDefines + $PlatformDefines

        Build-CMakeProject `
          -Src $SourceCache\swift-corelibs-foundation `
          -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Variant}Foundation")) `
          -InstallTo "$SDKRoot\usr" `
          -Platform $Platform `
          -CCompiler $Compilers.GNUC `
          -CXXCompiler $Compilers.GNUCXX `
          -SwiftCompiler $Compilers.Swift `
          -SwiftSDK $SDKRoot `
          -Defines $FoundationDefines
      }
    }

    # ── Backtrace (static Windows 64-bit only) ────────────────────────────────
    if ($Static -and $Platform.OS -eq [OS]::Windows -and $Platform.Architecture.ShortName -ne "x86") {
      Record-OperationTime $Platform "Build-Backtrace" {
        Build-CMakeProject `
          -Src $SourceCache\swift\Runtimes\Supplemental\StackWalker `
          -Bin (Get-ProjectBinaryCache $Platform ([Project]::Backtrace)) `
          -InstallTo "${SDKRoot}\usr" `
          -Platform $Platform `
          -CXXCompiler $Compilers.GNUCXX `
          -SwiftCompiler $Compilers.Swift `
          -SwiftSDK $null `
          -Defines ($SDKInstallDefines + @{
            CMAKE_Swift_FLAGS   = @("-static-stdlib");
            SwiftCore_DIR       = "$(Get-ProjectBinaryCache $Platform ([Project]"${Variant}Runtime"))\cmake\SwiftCore";
            SwiftCxxOverlay_DIR = "$(Get-ProjectBinaryCache $Platform ([Project]"${Variant}Overlay"))\Cxx\cmake\SwiftCxxOverlay";
            SwiftOverlay_DIR    = "$(Get-ProjectBinaryCache $Platform ([Project]"${Variant}Overlay"))\cmake\SwiftOverlay";
            SwiftRuntime_DIR    = "$(Get-ProjectBinaryCache $Platform ([Project]"${Variant}RuntimeModule"))\cmake\SwiftRuntime";
          })
      }
    }

    # ── SDK metadata ──────────────────────────────────────────────────────────
    Repair-SDKHeaders $SDKRoot
    Write-SDKSettings $Platform.OS -Identifier $SDKIdentifier

    # ── Testing frameworks ────────────────────────────────────────────────────
    # Skipped for the Bootstrap variant: BootstrapTestingMacros (which produces
    # TestingMacros.dll) is built after the Bootstrap SDK in the top-level
    # sequence, so the macro plugin is not available yet at this point.
    if (-not $Static -and $BuildFoundation -and $Variant -ne "Bootstrap") {
      Invoke-BuildStep Build-Testing $Platform -Compilers $Compilers -SwiftSDK (Get-SwiftSDK -OS $Platform.OS -Identifier $SDKIdentifier)
      Invoke-BuildStep Build-XCTest $Platform -Compilers $Compilers -SwiftSDK (Get-SwiftSDK -OS $Platform.OS -Identifier $SDKIdentifier)
    }

    if ($Platform.OS -eq [OS]::Windows -and -not $Static) {
      # TODO(compnerd): remove once these runtime projects embed their Windows
      # assembly manifests in their own builds.
      Repair-WindowsSDKAssemblyManifests $Platform $SDKRuntimeBin
    }

  }
}

function Build-SQLite([Hashtable] $Platform,
                      [Hashtable] $CCompiler,
                      [string]    $Phase) {
  Build-CMakeProject `
    -Src $SourceCache\swift-toolchain-sqlite `
    -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Phase}SQLite")) `
    -Platform $Platform `
    -CCompiler $CCompiler `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
    }
}

function Build-System([Hashtable] $Platform,
                      [Hashtable] $Compilers,
                      [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-system `
    -Bin (Get-ProjectBinaryCache $Platform System) `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    }
}

function Build-Subprocess([Hashtable] $Platform,
                          [Hashtable] $Compilers,
                          [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-subprocess `
    -Bin (Get-ProjectBinaryCache $Platform Subprocess) `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      SwiftSystem_DIR = (Get-ProjectCMakeModules $Platform System);
    }
}

function Build-ToolsProtocols([Hashtable] $Platform,
                              [Hashtable] $Compilers,
                              [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-tools-protocols `
    -Bin (Get-ProjectBinaryCache $Platform ToolsProtocols) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -CXXCompiler $Compilers.CXX `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
    }
}

function Build-Build([Hashtable] $Platform,
                     [Hashtable] $Compilers,
                     [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-build `
    -Bin (Get-ProjectBinaryCache $Platform Build) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -CXXCompiler $Compilers.CXX `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      # FIXME(79740) Use lld to workaround the ARM64 LNK1322 issue
      CMAKE_Swift_FLAGS = if ($Platform -eq $KnownPlatforms["WindowsARM64"]) {
            @("-use-ld=lld-link")
          } else {
            @()
          };
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      LLBuild_DIR = (Get-ProjectCMakeModules $Platform LLBuild);
      SwiftDriver_DIR = (Get-ProjectCMakeModules $Platform Driver);
      SwiftSystem_DIR = (Get-ProjectCMakeModules $Platform System);
      SwiftSubprocess_DIR = (Get-ProjectCMakeModules $Platform Subprocess);
      TSC_DIR = (Get-ProjectCMakeModules $Platform ToolsSupportCore);
      SwiftToolsProtocols_DIR = (Get-ProjectCMakeModules $Platform ToolsProtocols);
      SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
      SQLite3_LIBRARY = if ($Platform.OS -eq [OS]::Windows) {
            "$(Get-ProjectBinaryCache $Platform SQLite)\SQLite3.lib"
          } else {
            "$(Get-ProjectBinaryCache $Platform SQLite)\libsqlite3.lib"
          };
    }
}

function Build-ToolsSupportCore([Hashtable] $Platform,
                                [Hashtable] $Compilers,
                                [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-tools-support-core `
    -Bin (Get-ProjectBinaryCache $Platform ToolsSupportCore) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

      Foundation_DIR = $(Get-ProjectCMakeModules $Platform DynamicFoundation);
      XCTest_DIR = (Get-ProjectCMakeModules $Platform XCTest);
    }
}

function Build-LLBuild([Hashtable] $Platform,
                       [Hashtable] $Compilers,
                       [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\llbuild `
    -Bin (Get-ProjectBinaryCache $Platform LLBuild) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CXXCompiler $Compilers.CXX `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      BUILD_TESTING = "NO";
      LLBUILD_SUPPORT_BINDINGS = "Swift";
      SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
      SQLite3_LIBRARY = if ($Platform.OS -eq [OS]::Windows) {
        "$(Get-ProjectBinaryCache $Platform SQLite)\SQLite3.lib"
      } else {
        "$(Get-ProjectBinaryCache $Platform SQLite)\libsqlite3.a"
      };
    }
}

function Test-LLBuild([Hashtable] $Platform) {
  # Build additional llvm executables needed by tests
  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $BuildPlatform
    Invoke-Program (Get-Ninja).Path -C (Get-ProjectBinaryCache $BuildPlatform BuildTools) FileCheck not
  }

  Invoke-IsolatingEnvVars {
    $env:Path = "$env:Path;$UnixToolsBinDir"
    $Stage1BinDir = Get-ProjectToolchainBin $BuildPlatform Stage1Compilers
    $env:AR = [IO.Path]::Combine($Stage1BinDir, "llvm-ar.exe")
    $env:CLANG = [IO.Path]::Combine($Stage1BinDir, "clang.exe")

    Build-CMakeProject `
      -Src $SourceCache\llbuild `
      -Bin (Get-ProjectBinaryCache $BuildPlatform LLBuild) `
      -Platform $Platform `
      -CXXCompiler $Compilers.Stage1.CXX `
      -SwiftCompiler $Compilers.Stage1.Swift `
      -SwiftSDK (Get-SwiftSDK -OS $BuildPlatform.OS) `
      -BuildTargets default,test-llbuild `
      -Defines @{
        BUILD_SHARED_LIBS = "YES";
        # Build-LLBuild configures this same directory with BUILD_TESTING=NO,
        # which drops the tests subdirectory and the test-llbuild target.
        BUILD_TESTING = "YES";
        FILECHECK_EXECUTABLE = ([IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform BuildTools), "bin", "FileCheck.exe"));
        LIT_EXECUTABLE = "$SourceCache\llvm-project\llvm\utils\lit\lit.py";
        LLBUILD_SUPPORT_BINDINGS = "Swift";
        SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
        SQLite3_LIBRARY = "$(Get-ProjectBinaryCache $Platform SQLite)\SQLite3.lib";
      }
  }
}

function Build-ArgumentParser([Hashtable] $Platform,
                              [Hashtable] $Compilers,
                              [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-argument-parser `
    -Bin (Get-ProjectBinaryCache $Platform ArgumentParser) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      BUILD_TESTING = "NO";
      BUILD_EXAMPLES = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    }
}

function Build-Driver([Hashtable] $Platform,
                      [Hashtable] $Compilers,
                      [string]    $SwiftSDK,
                      [string]    $LLVM_DIR,
                      [string]    $Clang_DIR,
                      [string]    $Swift_DIR) {
  Build-CMakeProject `
    -Src $SourceCache\swift-driver `
    -Bin (Get-ProjectBinaryCache $Platform Driver) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -CXXCompiler $Compilers.CXX `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      TSC_DIR = (Get-ProjectCMakeModules $Platform ToolsSupportCore);
      LLBuild_DIR = (Get-ProjectCMakeModules $Platform LLBuild);
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
      SQLite3_LIBRARY = if ($Platform.OS -eq [OS]::Windows) {
        "$(Get-ProjectBinaryCache $Platform SQLite)\SQLite3.lib"
      } else {
        "$(Get-ProjectBinaryCache $Platform SQLite)\libsqlite3.lib"
      };
      SWIFT_DRIVER_BUILD_TOOLS = "YES";
      LLVM_DIR = $LLVM_DIR;
      Clang_DIR = $Clang_DIR;
      Swift_DIR = $Swift_DIR;
    }
}

function Build-Crypto([Hashtable] $Platform,
                      [Hashtable] $Assembler,
                      [Hashtable] $Compilers,
                      [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-crypto `
    -Bin (Get-ProjectBinaryCache $Platform Crypto) `
    -Platform $Platform `
    -Assembler $Assembler `
    -CCompiler $Compilers.C `
    -CXXCompiler $Compilers.CXX `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      SwiftASN1_DIR = (Get-ProjectCMakeModules $Platform ASN1);
    }
}

function Build-Collections([Hashtable] $Platform,
                           [Hashtable] $Compilers,
                           [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-collections `
    -Bin (Get-ProjectBinaryCache $Platform Collections) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    }
}

function Build-ASN1([Hashtable] $Platform,
                    [Hashtable] $Compilers,
                    [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-asn1 `
    -Bin (Get-ProjectBinaryCache $Platform ASN1) `
    -Platform $Platform `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    }
}

function Build-Certificates([Hashtable] $Platform,
                            [Hashtable] $Compilers,
                            [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-certificates `
    -Bin (Get-ProjectBinaryCache $Platform Certificates) `
    -Platform $Platform `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      SwiftCrypto_DIR = (Get-ProjectCMakeModules $Platform Crypto);
      SwiftASN1_DIR = (Get-ProjectCMakeModules $Platform ASN1);
    }
}

function Build-PackageManager([Hashtable] $Platform,
                              [Hashtable] $Compilers,
                              [string]    $SwiftSDK,
                              [string]    $SwiftSyntax_DIR) {
  $SrcDir = if (Test-Path -Path "$SourceCache\swift-package-manager" -PathType Container) {
    "$SourceCache\swift-package-manager"
  } else {
    "$SourceCache\swiftpm"
  }

  Build-CMakeProject `
    -Src $SrcDir `
    -Bin (Get-ProjectBinaryCache $Platform PackageManager) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_Swift_FLAGS = @("-DCRYPTO_v2");
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      SwiftSystem_DIR = (Get-ProjectCMakeModules $Platform System);
      SwiftSubprocess_DIR = (Get-ProjectCMakeModules $Platform Subprocess);
      TSC_DIR = (Get-ProjectCMakeModules $Platform ToolsSupportCore);
      LLBuild_DIR = (Get-ProjectCMakeModules $Platform LLBuild);
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      SwiftDriver_DIR = (Get-ProjectCMakeModules $Platform Driver);
      SwiftBuild_DIR = (Get-ProjectCMakeModules $Platform Build);
      SwiftToolsProtocols_DIR = (Get-ProjectCMakeModules $Platform ToolsProtocols);
      SwiftCrypto_DIR = (Get-ProjectCMakeModules $Platform Crypto);
      SwiftCollections_DIR = (Get-ProjectCMakeModules $Platform Collections);
      SwiftASN1_DIR = (Get-ProjectCMakeModules $Platform ASN1);
      SwiftCertificates_DIR = (Get-ProjectCMakeModules $Platform Certificates);
      SwiftSyntax_DIR = $SwiftSyntax_DIR;
      SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
      SQLite3_LIBRARY = if ($Platform.OS -eq [OS]::Windows) {
        "$(Get-ProjectBinaryCache $Platform SQLite)\SQLite3.lib"
      } else {
        "$(Get-ProjectBinaryCache $Platform SQLite)\libsqlite3.a"
      };
    }
}

function Build-PackageManagerRuntime([Hashtable] $Platform,
                                     [Hashtable] $Compilers,
                                     [string]    $SwiftSDK) {
  $SrcDir = if (Test-Path -Path "$SourceCache\swift-package-manager" -PathType Container) {
    "$SourceCache\swift-package-manager"
  } else {
    "$SourceCache\swiftpm"
  }

  Build-CMakeProject `
    -Src $SrcDir\Sources\Runtimes `
    -Bin (Get-ProjectBinaryCache $Platform PackageManagerRuntime) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.GNUC `
    -CXXCompiler $Compilers.GNUCXX `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      SwiftPM_ENABLE_RUNTIME = "NO";
    }
}

function Build-Markdown([Hashtable] $Platform,
                        [Hashtable] $Compilers,
                        [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-markdown `
    -Bin (Get-ProjectBinaryCache $Platform Markdown) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      "cmark-gfm_DIR" = "$($Platform.ToolchainInstallRoot)\usr\lib\cmake";
    }
}

function Build-Format([Hashtable] $Platform,
                      [Hashtable] $Compilers,
                      [string]    $SwiftSDK,
                      [string]    $SwiftSyntax_DIR) {
  Build-CMakeProject `
    -Src $SourceCache\swift-format `
    -Bin (Get-ProjectBinaryCache $Platform Format) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      SwiftSyntax_DIR = $SwiftSyntax_DIR;
      SwiftMarkdown_DIR = (Get-ProjectCMakeModules $Platform Markdown);
      "cmark-gfm_DIR" = "$($Platform.ToolchainInstallRoot)\usr\lib\cmake";
    }
}

function Test-Format {
  $SwiftSyntaxHostLibraries = [IO.Path]::Combine(
    (Get-ProjectBinaryCache $BuildPlatform Stage2Compilers),
    "lib",
    "swift",
    "host"
  )

  $SwiftPMArguments = @(
    # swift-syntax
    "-Xswiftc", "-I$SwiftSyntaxHostLibraries",
    "-Xswiftc", "-L$SwiftSyntaxHostLibraries",
    # swift-argument-parser
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform ArgumentParser)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform ArgumentParser)\lib",
    # swift-cmark
    "-Xswiftc", "-I$SourceCache\cmark\src\include",
    "-Xswiftc", "-I$SourceCache\cmark\extensions\include",
    "-Xlinker", "-I$SourceCache\cmark\extensions\include",
    "-Xlinker", "$(Get-CMarkBinaryCache $HostPlatform)\src\cmark-gfm.lib",
    "-Xlinker", "$(Get-CMarkBinaryCache $HostPlatform)\extensions\cmark-gfm-extensions.lib",
    # swift-markdown
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform Markdown)\lib\CAtomic.lib",
    "-Xswiftc", "-I$SourceCache\swift-markdown\Sources\CAtomic\include",
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform Markdown)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform Markdown)\lib",
    # swift-format
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform Format)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform Format)\lib"
  )

  Invoke-IsolatingEnvVars {
    $env:SWIFTFORMAT_BUILD_ONLY_TESTS=1
    # Testing swift-format is faster in serial mode than in parallel mode, probably because parallel test execution
    # launches a process for every test class and the process launching overhead on Windows is greater than any
    # gains from parallel test execution.
    Build-SPMProject `
      -Action Test `
      -Src "$SourceCache\swift-format" `
      -Bin "$BinaryCache\$($HostPlatform.Triple)\FormatTests" `
      -Platform $BuildPlatform `
      @SwiftPMArguments
  }
}

function Build-LMDB([Hashtable] $Platform,
                    [Hashtable] $CCompiler) {
  Build-CMakeProject `
    -Src $SourceCache\swift-lmdb `
    -Bin (Get-ProjectBinaryCache $Platform LMDB) `
    -Platform $Platform `
    -CCompiler $CCompiler `
    -BuildTargets default
}

function Build-IndexStoreDB([Hashtable] $Platform,
                            [Hashtable] $Compilers,
                            [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\indexstore-db `
    -Bin (Get-ProjectBinaryCache $Platform IndexStoreDB) `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -CXXCompiler $Compilers.CXX `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      CMAKE_C_FLAGS = @("-I$SwiftSDK\usr\include", "-I$SwiftSDK\usr\include\Block");
      CMAKE_CXX_FLAGS = @("-I$SwiftSDK\usr\include", "-I$SwiftSDK\usr\include\Block");
      LMDB_DIR = (Get-ProjectCMakeModules $Platform LMDB);
    }
}

function Build-SourceKitLSP([Hashtable] $Platform,
                            [Hashtable] $Compilers,
                            [string]    $SwiftSDK,
                            [string]    $SwiftSyntax_DIR) {
  Build-CMakeProject `
    -Src $SourceCache\sourcekit-lsp `
    -Bin (Get-ProjectBinaryCache $Platform SourceKitLSP) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      SwiftSyntax_DIR = $SwiftSyntax_DIR;
      TSC_DIR = (Get-ProjectCMakeModules $Platform ToolsSupportCore);
      LLBuild_DIR = (Get-ProjectCMakeModules $Platform LLBuild);
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      SwiftASN1_DIR = (Get-ProjectCMakeModules $Platform ASN1);
      SwiftCrypto_DIR = (Get-ProjectCMakeModules $Platform Crypto);
      SwiftCollections_DIR = (Get-ProjectCMakeModules $Platform Collections);
      SwiftBuild_DIR = (Get-ProjectCMakeModules $Platform Build);
      SwiftPM_DIR = (Get-ProjectCMakeModules $Platform PackageManager);
      LMDB_DIR = (Get-ProjectCMakeModules $Platform LMDB);
      IndexStoreDB_DIR = (Get-ProjectCMakeModules $Platform IndexStoreDB);
      SwiftToolsProtocols_DIR = (Get-ProjectCMakeModules $Platform ToolsProtocols);
    }
}

function Test-SourceKitLSP {
  $SwiftSyntaxHostLibraries = [IO.Path]::Combine(
    (Get-ProjectBinaryCache $BuildPlatform Stage2Compilers),
    "lib",
    "swift",
    "host"
  )

  $SwiftPMArguments = @(
    # dispatch
    "-Xcc", "-I$SourceCache\swift-corelibs-libdispatch",
    "-Xcc", "-I$SourceCache\swift-corelibs-libdispatch\src\BlocksRuntime",
    # swift-syntax
    "-Xswiftc", "-I$SwiftSyntaxHostLibraries",
    "-Xswiftc", "-L$SwiftSyntaxHostLibraries",
    # swift-cmark
    "-Xswiftc", "-I$SourceCache\cmark\src\include",
    "-Xswiftc", "-I$SourceCache\cmark\extensions\include",
    "-Xlinker", "-I$SourceCache\cmark\extensions\include",
    "-Xlinker", "$(Get-CMarkBinaryCache $HostPlatform)\src\cmark-gfm.lib",
    "-Xlinker", "$(Get-CMarkBinaryCache $HostPlatform)\extensions\cmark-gfm-extensions.lib",
    # swift-system
    "-Xswiftc", "-I$SourceCache\swift-system\Sources\CSystem\include",
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform System)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform System)\lib",
    # swift-tools-support-core
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform ToolsSupportCore)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform ToolsSupportCore)\lib",
    # swift-llbuild
    "-Xswiftc", "-I$SourceCache\llbuild\products\libllbuild\include",
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform LLBuild)\products\llbuildSwift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform LLBuild)\lib",
    # swift-argument-parser
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform ArgumentParser)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform ArgumentParser)\lib",
    # swift-crypto
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform Crypto)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform Crypto)\lib",
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform Crypto)\lib\libCCryptoBoringSSL.lib",
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform Crypto)\lib\libCCryptoBoringSSLShims.lib",
    # swift-asn1
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform ASN1)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform ASN1)\lib",
    # swift-certificates
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform Certificates)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform Certificates)\lib",
    # swift-package-manager
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform PackageManager)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform PackageManager)\lib",
    # swift-markdown
    "-Xswiftc", "-I$SourceCache\swift-markdown\Sources\CAtomic\include",
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform Markdown)\lib\CAtomic.lib",
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform Markdown)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform Markdown)\lib",
    # swift-format
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform Format)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform Format)\lib",
    # indexstore-db
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform IndexStoreDB)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform IndexStoreDB)\Sources\IndexStoreDB",
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform IndexStoreDB)\Sources\IndexStoreDB_CIndexStoreDB\CIndexStoreDB.lib",
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform IndexStoreDB)\Sources\IndexStoreDB_Core\Core.lib",
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform IndexStoreDB)\Sources\IndexStoreDB_Database\Database.lib",
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform IndexStoreDB)\Sources\IndexStoreDB_Index\Index.lib",
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform IndexStoreDB)\Sources\IndexStoreDB_LLVMSupport\LLVMSupport.lib",
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform IndexStoreDB)\Sources\IndexStoreDB_Support\Support.lib",
    # swift-tools-protocols
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform ToolsProtocols)\swift",
    "-Xswiftc", "-I$SourceCache\swift-tools-protocols\Sources\ToolsProtocolsCAtomics\include",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform ToolsProtocols)\lib",
    # LMDB
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform LMDB)\lib\CLMDB.lib",
    # sourcekit-lsp
    "-Xswiftc", "-I$SourceCache\sourcekit-lsp\Sources\CAtomics\include",
    "-Xswiftc", "-I$SourceCache\sourcekit-lsp\Sources\CSourcekitd\include",
    "-Xlinker", "$(Get-ProjectBinaryCache $BuildPlatform SourceKitLSP)\lib\CSourcekitd.lib",
    "-Xswiftc", "-I$SourceCache\sourcekit-lsp\Sources\CCompletionScoring\include",
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform SourceKitLSP)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform SourceKitLSP)\lib"
  )

  Invoke-IsolatingEnvVars {
    $env:SOURCEKIT_LSP_BUILD_ONLY_TESTS=1

    # CI doesn't contain any sensitive information. Log everything.
    $env:SOURCEKIT_LSP_LOG_PRIVACY_LEVEL="sensitive"

    # Log with the highest log level to simplify debugging of CI failures.
    $env:SOURCEKIT_LSP_LOG_LEVEL="debug"

    # The Windows build doesn't build the SourceKit plugins into the SwiftPM build directory (it builds them using CMake).
    # Tell the tests where to find the just-built plugins.
    $env:SOURCEKIT_LSP_TEST_PLUGIN_PATHS="$($HostPlatform.ToolchainInstallRoot)\usr\lib"

    Build-SPMProject `
      -Action TestParallel `
      -Src "$SourceCache\sourcekit-lsp" `
      -Bin "$BinaryCache\$($HostPlatform.Triple)\SourceKitLSPTests" `
      -Platform $BuildPlatform `
      @SwiftPMArguments
  }
}

function Build-BootstrapFoundationMacros([Hashtable] $Platform,
                                         [Hashtable] $SwiftCompiler,
                                         [string]    $SwiftSDK,
                                         [string]    $SwiftSyntax_DIR) {
  Build-CMakeProject `
    -Src $SourceCache\swift-foundation\Sources\FoundationMacros `
    -Bin (Get-ProjectBinaryCache $Platform BootstrapFoundationMacros) `
    -BuildTargets default `
    -Platform $Platform `
    -SwiftCompiler $SwiftCompiler `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      SwiftSyntax_DIR = $SwiftSyntax_DIR;
    }
}

function Build-BootstrapTestingMacros([Hashtable] $Platform,
                                      [Hashtable] $SwiftCompiler,
                                      [string]    $SwiftSDK,
                                      [string]    $SwiftSyntax_DIR) {
  Build-CMakeProject `
    -Src $SourceCache\swift-testing\Sources\TestingMacros `
    -Bin (Get-ProjectBinaryCache $Platform BootstrapTestingMacros) `
    -BuildTargets default `
    -Platform $Platform `
    -SwiftCompiler $SwiftCompiler `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      SwiftSyntax_DIR = $SwiftSyntax_DIR;
    }
}

function Build-TestingMacros([Hashtable] $Platform,
                             [Hashtable] $SwiftCompiler,
                             [string]    $SwiftSDK,
                             [string]    $SwiftSyntax_DIR) {
  Build-CMakeProject `
    -Src $SourceCache\swift-testing\Sources\TestingMacros `
    -Bin (Get-ProjectBinaryCache $Platform TestingMacros) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr"  `
    -Platform $Platform `
    -SwiftCompiler $SwiftCompiler `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      SwiftSyntax_DIR = $SwiftSyntax_DIR;
    }
}

function Repair-Toolchain([string] $ToolchainInstallRoot) {
  # We've already special-cased $HostPlatform.ToolchainInstallRoot to point to $ToolchainInstallRoot.
  # There are only a few extra restructuring steps we need to take care of.

  # Restructure _InternalSwiftScan (keep the original one for the installer)
  Copy-Item -Force `
    -Path "$ToolchainInstallRoot\usr\lib\swift\_InternalSwiftScan" `
    -Destination "$ToolchainInstallRoot\usr\include"
  Copy-Item -Force `
    -Path "$ToolchainInstallRoot\usr\lib\swift\windows\_InternalSwiftScan.lib" `
    -Destination "$ToolchainInstallRoot\usr\lib"

  # Switch to swift-driver
  $SwiftDriver = ([IO.Path]::Combine((Get-ProjectBinaryCache $HostPlatform Driver), "bin", "swift-driver.exe"))
  Copy-Item -Force `
    -Path $SwiftDriver `
    -Destination "$ToolchainInstallRoot\usr\bin\swift.exe"
  Copy-Item -Force `
    -Path $SwiftDriver `
    -Destination "$ToolchainInstallRoot\usr\bin\swiftc.exe"

  # Copy swift-inspect tools.
  Copy-Item -Force `
    -Path "$(Get-PlatformRoot $HostPlatform.OS)\Developer\Library\$(Get-ModuleTriple $HostPlatform)\bin\swift-inspect.exe" `
    -Destination "$ToolchainInstallRoot\usr\bin\swift-inspect.exe"
  Copy-Item -Force `
    -Path "$(Get-PlatformRoot $HostPlatform.OS)\Developer\Library\$(Get-ModuleTriple $HostPlatform)\bin\SwiftInspectClient.dll" `
    -Destination "$ToolchainInstallRoot\usr\bin\SwiftInspectClient.dll"

  # Copy embeddable Python
  New-Item -ItemType Directory -Path "$(Get-EmbeddedPythonInstallDir)" -ErrorAction Ignore | Out-Null
  Copy-Item -Force -Recurse `
    -Path "$(Get-EmbeddedPythonPath $HostPlatform)\*" `
    -Destination "$(Get-EmbeddedPythonInstallDir)"
}

function Build-Inspect([Hashtable] $Platform,
                       [Hashtable] $Compilers,
                       [string]    $SwiftSDK) {
  $Defines = @{
    CMAKE_Swift_FLAGS = @(
      "-Xcc", "-I$SwiftSDK\usr\include",
      "-Xcc", "-I$SwiftSDK\usr\lib\swift",
      "-Xcc", "-I$SwiftSDK\usr\include\swift\SwiftRemoteMirror",
      "-L$SwiftSDK\usr\lib\swift\$($Platform.OS.ToString())\$($Platform.Architecture.LLVMName)"
    );
    ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
  }
  $Defines.SwiftOverlay_DIR = "$(Get-ProjectBinaryCache $Platform DynamicOverlay)\cmake\SwiftOverlay"

  Build-CMakeProject `
    -Src $SourceCache\swift\tools\swift-inspect `
    -Bin (Get-ProjectBinaryCache $Platform SwiftInspect)`
    -InstallTo "$(Get-PlatformRoot $Platform.OS)\Developer\Library\$(Get-ModuleTriple $Platform)" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -CXXCompiler $Compilers.CXX `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines $Defines
}

function Build-SymbolKit([Hashtable] $Platform,
                         [Hashtable] $Compilers,
                         [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-docc-symbolkit `
    -bin (Get-ProjectBinaryCache $Platform SymbolKit) `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    }
}

function Build-DocC([Hashtable] $Platform,
                    [Hashtable] $Compilers,
                    [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift-docc `
    -Bin (Get-ProjectBinaryCache $Platform DocC) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      SwiftASN1_DIR = (Get-ProjectCMakeModules $Platform ASN1);
      SwiftCrypto_DIR = (Get-ProjectCMakeModules $Platform Crypto);
      SwiftMarkdown_DIR = (Get-ProjectCMakeModules $Platform Markdown);
      LMDB_DIR = (Get-ProjectCMakeModules $Platform LMDB);
      SymbolKit_DIR = (Get-ProjectCMakeModules $Platform SymbolKit);
      "cmark-gfm_DIR" = "$($Platform.ToolchainInstallRoot)\usr\lib\cmake";
    }
}

function Test-PackageManager() {
  $SrcDir = if (Test-Path -Path "$SourceCache\swift-package-manager" -PathType Container) {
    "$SourceCache\swift-package-manager"
  } else {
    "$SourceCache\swiftpm"
  }

  Build-SPMProject `
    -Action Test `
    -Src $SrcDir `
    -Bin "$BinaryCache\$($HostPlatform.Triple)\PackageManagerTests" `
    -Platform $HostPlatform `
    -Xcc "-I$(Get-InstallDir $HostPlatform)\Toolchains\$ProductVersion+Asserts\usr\include" `
    -Xlinker "-L$(Get-InstallDir $HostPlatform)\Toolchains\$ProductVersion+Asserts\usr\lib"
}

# Once the staged toolchain image is laid out, bind each toolchain EXE to
# private SxS assemblies for the Swift runtime DLLs in its static import
# closure.
# The caller provides the exact runtime image the toolchain was linked against.
# The WiX package authoring remains canonical; this pass only warns when the
# live import graph and the checked-in package layout drift.
function Stage-WindowsToolchainSxS([Hashtable] $Platform,
                                   [string]    $ToolchainRoot,
                                   [string]    $RuntimeLocation) {
  # Ordered by package dependency.  `bld` is the common base package for the
  # tool packages, and `dbg` is not useful without the base toolchain.
  $PackageWxiFiles = [ordered] @{
    "bld" = "$SourceCache\swift-installer-scripts\platforms\Windows\bld\bld.wxi"
    "cli" = "$SourceCache\swift-installer-scripts\platforms\Windows\cli\cli.wxi"
    "dbg" = "$SourceCache\swift-installer-scripts\platforms\Windows\dbg\dbg.wxi"
    "ide" = "$SourceCache\swift-installer-scripts\platforms\Windows\ide\ide.wxi"
  }

  # Parse each `*.wxi` once: EXE name -> package, and package -> declared SxS DLLs.
  $EXEToPackage   = @{}
  $PackageToSxS   = @{}
  foreach ($Module in $PackageWxiFiles.Keys) {
    $WxiPath = $PackageWxiFiles[$Module]
    if (-not (Test-Path $WxiPath)) {
      Write-Warning "Stage-WindowsToolchainSxS: '$WxiPath' not found; package '$Module' will be omitted from cross-reference"
      continue
    }
    $WxiText = Get-Content -Raw -LiteralPath $WxiPath
    # Match `$(ToolchainRoot)\usr\bin\<basename>.exe` references (any
    # quoting style, forward or back slashes).
    $EXEMatches = [regex]::Matches(
      $WxiText,
      '\$\(ToolchainRoot\)[\\/]+usr[\\/]+bin[\\/]+([A-Za-z0-9_+\-\.]+)\.exe'
    )
    foreach ($M in $EXEMatches) {
      $EXEName = $M.Groups[1].Value + ".exe"
      if (-not $EXEToPackage.ContainsKey($EXEName)) {
        $EXEToPackage[$EXEName] = $Module
      }
    }
    # Match the per-DLL SxS layout: `usr\bin\<basename>\<basename>.dll`.
    $SxSMatches = [regex]::Matches(
      $WxiText,
      '\$\(ToolchainRoot\)[\\/]+usr[\\/]+bin[\\/]+([A-Za-z0-9_+\-\.]+)[\\/]+\1\.dll'
    )
    $Names = New-Object System.Collections.Generic.HashSet[string]
    foreach ($M in $SxSMatches) {
      [void]$Names.Add($M.Groups[1].Value)
    }
    $PackageToSxS[$Module] = @($Names | Sort-Object)
  }

  if (-not (Test-Path $RuntimeLocation)) {
    throw "Stage-WindowsToolchainSxS: RuntimeLocation '$RuntimeLocation' not found"
  }

  if (-not (Test-Path $ToolchainRoot)) {
    throw "Stage-WindowsToolchainSxS: ToolchainRoot '$ToolchainRoot' not found"
  }

  $RuntimeDLLs = @(Get-WindowsSxSRuntimeDLLs $RuntimeLocation)
  if (-not $RuntimeDLLs) {
    throw "Stage-WindowsToolchainSxS: no *.dll found under '$RuntimeLocation'"
  }

  $BinDir = [IO.Path]::Combine($ToolchainRoot, "usr", "bin")
  if (-not (Test-Path $BinDir)) {
    throw "Stage-WindowsToolchainSxS: BinDir '$BinDir' not found"
  }

  Write-Host "Stage-WindowsToolchainSxS: toolchain root     = '$ToolchainRoot'"
  Write-Host "Stage-WindowsToolchainSxS: runtime source     = '$RuntimeLocation'"

  # Build basename -> direct Swift runtime imports.  Keep only edges within
  # the runtime DLL set; system and CRT DLLs are not part of the SxS graph.
  $RuntimeBaseNames = @(
    $RuntimeDLLs |
      ForEach-Object { [IO.Path]::GetFileNameWithoutExtension($_.Name) }
  )
  $RuntimeSet = New-Object System.Collections.Generic.HashSet[string]
  foreach ($D in $RuntimeBaseNames) { [void]$RuntimeSet.Add($D) }

  $RuntimeDependencies = @{}
  foreach ($DLL in $RuntimeBaseNames) {
    $DLLPath = Join-Path $RuntimeLocation "$DLL.dll"
    $RuntimeDependencies[$DLL] = @(Get-DirectRuntimeImports -Path $DLLPath -RuntimeSet $RuntimeSet)
  }
  Write-Host "Stage-WindowsToolchainSxS: built runtime graph with $($RuntimeDependencies.Count) DLL(s)"

  $EXEDependencies = @{}
  $EXEPackageClosures = @{}
  $DLLDependencies = @{}
  $DLLsNeeded = [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase
  )
  $TotalEXEs = 0

  Get-ChildItem -Path $BinDir -Filter "*.exe" -File | ForEach-Object {
    $TotalEXEs++
    $StaticRuntimeDeps = @(
      Get-StaticRuntimeImports `
        -Path       $_.FullName `
        -BinaryDir  $BinDir `
        -RuntimeSet $RuntimeSet
    )
    $EXEDependencies[$_.FullName] = @(
      Get-RuntimeImportClosure `
        -Roots        $StaticRuntimeDeps `
        -RuntimeGraph $RuntimeDependencies
    )
    $EXEPackageClosures[$_.FullName] = $EXEDependencies[$_.FullName]
    foreach ($D in $EXEDependencies[$_.FullName]) { [void]$DLLsNeeded.Add($D) }
  }

  Get-ChildItem -Path $BinDir -Filter "*.dll" -File -ErrorAction SilentlyContinue | ForEach-Object {
    $ImageName = [IO.Path]::GetFileNameWithoutExtension($_.Name)
    if (-not $RuntimeSet.Contains($ImageName)) {
      $DirectRuntimeDeps = @(Get-DirectRuntimeImports -Path $_.FullName -RuntimeSet $RuntimeSet)
      if ($DirectRuntimeDeps.Count -gt 0) {
        $DLLDependencies[$_.FullName] = $DirectRuntimeDeps
        foreach ($D in $DirectRuntimeDeps) { [void]$DLLsNeeded.Add($D) }
      }
    }
  }

  # Remove flat copies that would shadow the private SxS assemblies.  The
  # excluded flat DLL and all link-time import libraries are left in place.
  $RuntimeBaseNameSet = New-Object System.Collections.Generic.HashSet[string]
  foreach ($D in $RuntimeBaseNames) { [void]$RuntimeBaseNameSet.Add($D) }
  $RemovedFlat = New-Object System.Collections.Generic.List[string]
  Get-ChildItem -Path $BinDir -Filter "*.dll" -File -ErrorAction SilentlyContinue | ForEach-Object {
    $Base = [IO.Path]::GetFileNameWithoutExtension($_.Name)
    if ($RuntimeBaseNameSet.Contains($Base)) {
      Remove-Item -LiteralPath $_.FullName -Force
      [void]$RemovedFlat.Add($_.Name)
    }
  }
  if ($RemovedFlat.Count -gt 0) {
    Write-Host "Stage-WindowsToolchainSxS: removed flat runtime DLL(s) from '$BinDir' to make room for per-assembly SxS layout: $(@($RemovedFlat) -join ', ')"
  }

  $DLLRootImports = @($DLLsNeeded | Sort-Object)
  $DLLsToInject = @(Get-RuntimeImportClosure -Roots $DLLRootImports -RuntimeGraph $RuntimeDependencies)
  $EXEsToBind = ($EXEDependencies.Keys | Where-Object { @($EXEDependencies[$_]).Count -gt 0 } | Measure-Object).Count
  Write-Host "Stage-WindowsToolchainSxS: discovered $TotalEXEs exe(s); $EXEsToBind need EXE binding; $($DLLDependencies.Count) DLL(s) import Swift runtime DLLs; $($DLLsToInject.Count) DLL(s) in bind set"

  # Prefer an MSI that already ships a dependent tool.  If a DLL is needed by
  # a tool in `bld`, keep it in `bld` so the dependent `cli`/`ide` packages
  # do not need to duplicate it.
  $PackageOrder = @("bld", "cli", "dbg", "ide")
  $ExpectedPackage = @{}
  foreach ($DLL in $DLLsToInject) {
    # Avoid `$Home`, which is a read-only PowerShell automatic variable.
    $OwningPackage = $null
    foreach ($Module in $PackageOrder) {
      $PackageEXEs = @($EXEToPackage.GetEnumerator() | Where-Object { $_.Value -eq $Module } | ForEach-Object { $_.Key })
      $Found = $false
      foreach ($EXE in $PackageEXEs) {
        $FullPath = Join-Path $BinDir $EXE
        if ($EXEPackageClosures.ContainsKey($FullPath) -and ($EXEPackageClosures[$FullPath] -contains $DLL)) {
          $Found = $true
          break
        }
      }
      if ($Found) { $OwningPackage = $Module; break }
    }
    if ($OwningPackage) { $ExpectedPackage[$DLL] = $OwningPackage }
    else                { $ExpectedPackage[$DLL] = "(unattributed)" }
  }

  # Warn about WiX drift, but keep the checked-in package layout canonical.
  foreach ($DLL in $DLLsToInject) {
    $Want = $ExpectedPackage[$DLL]
    $WiXPackage = $null
    foreach ($Key in $PackageToSxS.Keys) {
      if ($PackageToSxS[$Key] -contains $DLL) {
        if ($WiXPackage) {
          Write-Host -BackgroundColor DarkRed -ForegroundColor White "Stage-WindowsToolchainSxS: '$DLL' is declared in multiple package .wxi files ($WiXPackage, $Key)"
        } else {
          $WiXPackage = $Key
        }
      }
    }
    if (-not $WiXPackage) {
      Write-Host -BackgroundColor DarkRed -ForegroundColor White "Stage-WindowsToolchainSxS: '$DLL' is needed by staged PE files (expected package: $Want) but not declared in any package .wxi"
    } elseif ($Want -ne "(unattributed)" -and $WiXPackage -ne $Want) {
      Write-Host -BackgroundColor DarkRed -ForegroundColor White "Stage-WindowsToolchainSxS: '$DLL' is declared in $WiXPackage.wxi but expected package is $Want.wxi"
    }
  }
  foreach ($Key in $PackageToSxS.Keys) {
    foreach ($DLL in $PackageToSxS[$Key]) {
      if (-not $ExpectedPackage.ContainsKey($DLL)) {
        Write-Host -BackgroundColor DarkRed -ForegroundColor White "Stage-WindowsToolchainSxS: '$DLL' is declared in $Key.wxi but no toolchain EXE imports it"
      }
    }
  }

  # Nothing to bind if no PE imports the Swift runtime graph.
  if ($DLLsToInject.Count -eq 0) {
    Write-Host "Stage-WindowsToolchainSxS: no PE in '$BinDir' has Swift runtime imports; skipping per-DLL bind"
    return
  }
  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $Platform
    Set-WindowsSxSToolchainRuntimePerDLL `
      -BinaryDir              $BinDir `
      -RuntimeSourceDir       $RuntimeLocation `
      -EXEDependencies        $EXEDependencies `
      -DLLsToInject           $DLLsToInject `
      -AssemblyVersion        (ConvertTo-FourPartVersion $ProductVersion) `
      -ProcessorArchitecture  $Platform.Architecture.VSName
  }
}

function Build-Installer([Hashtable] $Platform) {
  # TODO(hjyamauchi) Re-enable the swift-inspect and swift-docc builds
  # when cross-compiling https://github.com/apple/swift/issues/71655
  $INCLUDE_SWIFT_DOCC = if ($IsCrossCompiling) { "False" } else { "True" }

  $Properties = @{
    BundleFlavor = "offline";
    ImageRoot = "$(Get-InstallDir $Platform)\";
    INCLUDE_SWIFT_DOCC = $INCLUDE_SWIFT_DOCC;
    SWIFT_DOCC_BUILD = "$(Get-ProjectBinaryCache $HostPlatform DocC)\release";
    SWIFT_DOCC_RENDER_ARTIFACT_ROOT = "${SourceCache}\swift-docc-render-artifact";
    PythonVersion = $PythonVersion
  }

  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $Platform
    # Avoid hard-coding the VC tools version number
    $VCRedistDir = (Get-ChildItem "${env:VCToolsRedistDir}\$($HostPlatform.Architecture.ShortName)" -Filter "Microsoft.VC*.CRT").FullName
    if ($VCRedistDir) {
      $Properties["VCRedistDir"] = "$VCRedistDir\"
    }
  }

  $Properties["Platforms"] = "`"$(
    @(
      if ($Windows) { "windows" }
      if ($Android) { "android" }
    ) -Join ";"
  )`"";
  $Properties["AndroidArchitectures"] = "`"$(($AndroidSDKBuilds | ForEach-Object { $_.Architecture.LLVMName }) -Join ";")`""
  $Properties["WindowsArchitectures"] = "`"$(($WindowsSDKBuilds | ForEach-Object { $_.Architecture.LLVMName }) -Join ";")`""
  $Properties["ToolchainVariants"] = "`"asserts$(if ($IncludeNoAsserts) { ";noasserts" })`"";
  foreach ($Build in $WindowsSDKBuilds) {
    $Properties["WindowsRuntime$($Build.Architecture.ShortName.ToUpperInvariant())"] = Get-WindowsRuntimeInstallRoot $Build
  }

  Build-WiXProject bundle\installer.wixproj -Platform $Platform -Bundle -Properties $Properties
}

function Copy-BuildArtifactsToStage([Hashtable] $Platform) {
  # Save the installer binary log
  if (-not $Package) { return }
  Copy-File "$BinaryCache\$($Platform.Triple)\msi\$($Platform.Architecture.VSName)-$([System.IO.Path]::GetFileNameWithoutExtension("bundle\installer.wixproj")).binlog" $Stage
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\*.cab" $Stage
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\*.msi" $Stage
  # Needed by the swift.org code-signing pipeline, which re-links the bundle
  # from pre-built artifacts without rebuilding project references.
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\baf.dll" $Stage
  foreach ($Build in $WindowsSDKBuilds) {
    Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Build.Architecture.VSName)\*.msm" $Stage
  }
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\installer.exe" $Stage
  # Extract installer engine to ease code-signing on swift.org CI
  New-Item -ItemType Directory -Path "$BinaryCache\$($Platform.Triple)\installer\$($Platform.Architecture.VSName)" -ErrorAction Ignore | Out-Null
  Invoke-WithDotNetRuntime {
    Invoke-Program (Get-DotNet) "$($WiX.Path)\wix.dll" -- burn detach -acceptEula $WiX.EulaIdentifier "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\installer.exe" -engine "$Stage\installer-engine.exe" -intermediateFolder "$BinaryCache\$($Platform.Triple)\installer\$($Platform.Architecture.VSName)\"
  }
}

#-------------------------------------------------------------------

# The parameters of this script, which the build scripts take by name.
$Parameters = Get-ScriptParameters $PSCommandPath

try {

Invoke-BuildScript "SetupDependencies" SetupDependencies.ps1 $Parameters

if ($DependenciesOnly) {
  exit 0;
}

Import-DependencyManifest

$Compilers.Host = @{
  C = if ($UseHostToolchain) { $Compilers.MSVC.C } else { $Compilers.Pinned.C }
  CXX = if ($UseHostToolchain) { $Compilers.MSVC.CXX } else { $Compilers.Pinned.CXX }
}

$Assemblers.Host = if ($UseHostToolchain) { $Assemblers.MSVC } else { $Assemblers.Pinned }

$WiX = (Get-DependencyManifest).WiX

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
  Invoke-BuildStep Build-SQLite $BuildPlatform -CCompiler $Compilers.Host.C -Phase EarlySwiftDriver -SummaryVariant "EarlySwiftDriver"
  Invoke-BuildStep Build-EarlySwiftDriver $BuildPlatform

  # ── Stage1 Compiler ───────────────────────────────────────────────────────
  Invoke-BuildStep Build-XML2 $BuildPlatform -CCompiler $Compilers.Host.C -CXXCompiler $Compilers.Host.CXX -Phase "Bootstrap" -SummaryVariant "Bootstrap"
  Invoke-BuildStep Build-Compilers $BuildPlatform -Variant "Asserts" -Project Stage1Compilers -SummaryVariant "Stage1/Asserts" @{
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

  Invoke-BuildStep Build-SDK $HostPlatform -SummaryVariant "Dynamic" -Context @{
    SDKIdentifier        = $HostPlatform.OS.ToString();
    Variant              = "Dynamic";
    Compilers            = $Compilers.Stage1;
    Static               = $false;
    BuildFoundation      = $true;
    SupplementalRuntimes = $SDKSupplementalRuntimes;
  }

  # ── Stage2 Compiler ───────────────────────────────────────────────────────
  Invoke-BuildStep Build-CMark $HostPlatform
  Invoke-BuildStep Build-XML2 $HostPlatform -CCompiler $Compilers.Stage1.C -CXXCompiler $Compilers.Stage1.CXX -Phase "Compiler" -SummaryVariant "Compiler"
  Invoke-BuildStep Build-Compilers $HostPlatform -Variant "Asserts" -Project Stage2Compilers -SummaryVariant "Stage2/Asserts" @{
    Assembler       = $Assemblers.Stage1;
    CCompiler       = $Compilers.Stage1.C;
    CXXCompiler     = $Compilers.Stage1.CXX;
    SwiftCompiler   = $Compilers.Stage1.Swift;
    SwiftSDK        = Get-SwiftSDK -OS $HostPlatform.OS;
    DispatchPackage = Get-ProjectCMakeModules $HostPlatform DynamicCDispatch;
  }
  Invoke-BuildStep Write-ToolchainInfo $HostPlatform -Variant "Asserts" -SummaryVariant "Asserts"
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
    Invoke-BuildStep Build-Compilers $HostPlatform -Variant "NoAsserts" -Project Stage2Compilers -SummaryVariant "Stage2/NoAsserts" @{
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
    Invoke-BuildStep Write-ToolchainInfo $HostPlatform -Variant "NoAsserts" -SummaryVariant "NoAsserts"
  }

  if ($HostPlatform.OS -eq [OS]::Windows) {
    $HostSDKRoot = Get-SwiftSDK -OS $HostPlatform.OS
    $HostRuntimeBin = Get-SDKRuntimeBin $HostPlatform $HostSDKRoot
    Invoke-BuildStep Stage-WindowsToolchainSxS $HostPlatform -SummaryVariant "Asserts" @{
      ToolchainRoot   = $HostPlatform.ToolchainInstallRoot;
      RuntimeLocation = $HostRuntimeBin;
    }
    if ($IncludeNoAsserts) {
      Invoke-BuildStep Stage-WindowsToolchainSxS $HostPlatform -SummaryVariant "NoAsserts" @{
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
  Build-SDKDependencies $WindowsSDKBuilds -Compilers $Compilers.Stage1

  $SDKROOT = Get-SwiftSDK -OS Windows
  foreach ($Build in $WindowsSDKBuilds) {
    if ($Build.LinkModes.contains("dynamic")) {
      Invoke-BuildStep Build-SDK $Build -SummaryVariant "Dynamic" -Context @{
        SDKIdentifier        = "Windows";
        Variant              = "Dynamic";
        Compilers            = $Compilers.Stage1;
        Static               = $false;
        BuildFoundation      = $true;
        SupplementalRuntimes = $SDKSupplementalRuntimes;
      }
    }

    if ($Build.LinkModes.contains("static")) {
      Invoke-BuildStep Build-SDK $Build -SummaryVariant "Static" -Context @{
        SDKIdentifier        = "Windows";
        Variant              = "Static";
        Compilers            = $Compilers.Stage1;
        Static               = $true;
        BuildFoundation      = $true;
        SupplementalRuntimes = $SDKSupplementalRuntimes;
      }
    }

    ConvertTo-ThickLayout -Platform $Build -Resources "${SDKROOT}\usr\lib\swift\windows"        -Filter @("*.lib")
    ConvertTo-ThickLayout -Platform $Build -Resources "${SDKROOT}\usr\lib\swift_static\windows" -Filter @("*.lib")

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
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build ZLib)\libzs.lib" -Destination "${SwiftResourceDir}\libzs.lib" | Out-Null
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
    Invoke-BuildStep Stage-WindowsToolchainSxS $HostPlatform -SummaryVariant "Asserts" @{
      ToolchainRoot   = $HostPlatform.ToolchainInstallRoot;
      RuntimeLocation = $HostRuntimeBin;
    }
    if ($IncludeNoAsserts) {
      Invoke-BuildStep Stage-WindowsToolchainSxS $HostPlatform -SummaryVariant "NoAsserts" @{
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
      Invoke-BuildStep Build-SDK $Build -SummaryVariant "Dynamic" -Context @{
        SDKIdentifier        = "Android";
        Variant              = "Dynamic";
        Compilers            = $Compilers.Stage1;
        Static               = $false;
        BuildFoundation      = $true;
        SupplementalRuntimes = $SDKSupplementalRuntimes;
      }
    }

    if ($Build.LinkModes.contains("static")) {
      Invoke-BuildStep Build-SDK $Build -SummaryVariant "Static" -Context @{
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
    Invoke-BuildStep Stage-WindowsToolchainSxS $HostPlatform -SummaryVariant "Asserts" @{
      ToolchainRoot   = $HostPlatform.ToolchainInstallRoot;
      RuntimeLocation = $HostRuntimeBin;
    }
  }

  $CompilersTests = @("clang", "lld", "lldb", "lldb-swift", "llvm", "swift", "swift-prepare")
  if ($Test | Where-Object { $CompilersTests -contains $_ }) {
    $Tests = @{
      "-TestClang" = $Test -contains "clang";
      "-TestLLD" = $Test -contains "lld";
      "-TestLLDB" = $Test -contains "lldb";
      "-TestLLDBSwift" = $Test -contains "lldb-swift";
      "-TestLLVM" = $Test -contains "llvm";
      "-TestSwift" = $Test -contains "swift";
      "-PrepareSwift" = $Test -contains "swift-prepare";
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
    Invoke-Program (Get-DependencyManifest).Syft -- `
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
  if ($Trace) {
    # Stage the trace with the other CI artifacts so that it can be downloaded.
    $TimingTraceDir = if ($Stage) { $Stage } else { $BinaryCache }
    Write-TimingTrace "$TimingTraceDir\build-timings.json"
  }
}
