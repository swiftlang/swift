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
The path to a directory where projects contributing to the Swift.
toolchain have been cloned.

.PARAMETER BinaryCache
The path to a directory where to write build system files and outputs.

.PARAMETER ImageRoot
The path to a directory that mimics a file system image root,
under which "Library" and "Program Files" subdirectories will be created
with the files installed by CMake.

.PARAMETER CDebugFormat
The debug information format for C/C++ code: dwarf or codeview.

.PARAMETER SwiftDebugFormat
The debug information format for Swift code: dwarf or codeview.

.PARAMETER AndroidAPILevel
The API Level to target when building the Android SDKs

.PARAMETER Android
When set, build android SDKs.

.PARAMETER AndroidSDKs
An array of architectures for which the Android Swift SDK should be built.

.PARAMETER WindowsSDKs
An array of architectures for which the Windows Swift SDK should be built.

.PARAMETER ProductVersion
The product version to be used when building the installer.
Supports semantic version strings.

.PARAMETER PinnedBuild
The toolchain snapshot to build the early components with.

.PARAMETER PinnedSHA256
The SHA256 for the pinned toolchain.

.PARAMETER AndroidNDKVersion
The version number of the Android NDK to be used.

.PARAMETER WinSDKVersion
The version number of the Windows SDK to be used.
Overrides the value resolved by the Visual Studio command prompt.
If no such Windows SDK is installed, it will be downloaded from nuget.

.PARAMETER IncludeDS2
Include the ds2 remote debug server in the SDK.
This component is currently only supported in Android builds.

.PARAMETER SkipBuild
If set, does not run the build phase.

.PARAMETER SkipPackaging
If set, skips building the msi's and installer

.PARAMETER DebugInfo
If set, debug information will be generated for the builds.

.PARAMETER EnableCaching
If true, use `sccache` to cache the build rules. Configuration of sccache must be done through
the environment variables defined by the sccache project.

.PARAMETER Clean
If true, clean non-compiler builds while building.

.PARAMETER Test
An array of names of projects to run tests for.
'*' runs all tests

.PARAMETER Stage
The path to a directory where built msi's and the installer executable should be staged (for CI).

.PARAMETER ToBatch
When set, runs the script in a special mode which outputs a listing of command invocations
in batch file format instead of executing them.

.PARAMETER HostArchName
The architecture where the toolchain will execute.

.PARAMETER IncludeNoAsserts
If set, the no-assert toolchain variant is also build and included in the output.

.PARAMETER FoundationTestConfiguration
Whether to run swift-foundation and swift-corelibs-foundation tests in a debug or release configuration.

.EXAMPLE
PS> .\Build.ps1

.EXAMPLE
PS> .\Build.ps1 -WindowsSDKs x64 -ProductVersion 1.2.3 -Test foundation,xctest
#>
[CmdletBinding(PositionalBinding = $false)]
param
(
  [System.IO.FileInfo] $SourceCache = "S:\SourceCache",
  [System.IO.FileInfo] $BinaryCache = "S:\b",
  [System.IO.FileInfo] $ImageRoot = "S:",
  [ValidateSet("codeview", "dwarf")]
  [string] $CDebugFormat = "dwarf",
  [ValidateSet("codeview", "dwarf")]
  [string] $SwiftDebugFormat = "dwarf",
  [ValidateRange(1, 36)]
  [int] $AndroidAPILevel = 28,
  [string[]] $AndroidSDKs = @(),
  [string[]] $WindowsSDKs = @("X64","X86","Arm64"),
  [string] $ProductVersion = "0.0.0",
  [string] $ToolchainIdentifier = $(if ($env:TOOLCHAIN_VERSION) { $env:TOOLCHAIN_VERSION } else { "$env:USERNAME.development" }),
  [string] $PinnedBuild = "",
  [ValidatePattern("^([A-Fa-f0-9]{64}|)$")]
  [string] $PinnedSHA256 = "",
  [string] $PinnedVersion = "",
  [ValidatePattern('^\d+(\.\d+)*$')]
  [string] $PythonVersion = "3.9.10",
  [ValidatePattern("^r(?:[1-9]|[1-9][0-9])(?:[a-z])?$")]
  [string] $AndroidNDKVersion = "r27c",
  [ValidatePattern("^\d+\.\d+\.\d+(?:-\w+)?")]
  [string] $WinSDKVersion = "",
  [switch] $Android = $false,
  [switch] $SkipBuild = $false,
  [switch] $SkipPackaging = $false,
  [switch] $IncludeDS2 = $false,
  [switch] $IncludeSBoM = $false,
  [string[]] $Test = @(),
  [string] $Stage = "",
  [ValidateSet("AMD64", "ARM64")]
  [string] $HostArchName = $(if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }),
  [switch] $IncludeNoAsserts = $false,
  [switch] $Clean,
  [switch] $DebugInfo,
  [ValidatePattern('^\d+(\.\d+)*$')]
  [string] $SCCacheVersion = "0.10.0",
  [string] $SyftVersion = "1.29.1",
  [switch] $EnableCaching,
  [ValidateSet("debug", "release")]
  [string] $FoundationTestConfiguration = "debug",
  [switch] $Summary,
  [switch] $ToBatch
)

## Prepare the build environment.

$ErrorActionPreference = "Stop"
Set-StrictMode -Version 3.0

# Avoid being run in a "Developer" shell since this script launches its own sub-shells targeting
# different architectures, and these variables cause confusion.
if ($env:VSCMD_ARG_HOST_ARCH -or $env:VSCMD_ARG_TGT_ARCH) {
  throw "At least one of VSCMD_ARG_HOST_ARCH and VSCMD_ARG_TGT_ARCH is set, which is incompatible with this script. Likely need to run outside of a Developer shell."
}

# Prevent elsewhere-installed swift modules from confusing our builds.
$env:SDKROOT = ""

$CustomWinSDKRoot = $null # Overwritten if we download a Windows SDK from nuget

# Avoid $env:ProgramFiles in case this script is running as x86
$UnixToolsBinDir = "$env:SystemDrive\Program Files\Git\usr\bin"

## Cleanup build arguments.

# Validate that if one is set all are set.
if (($PinnedBuild -or $PinnedSHA256 -or $PinnedVersion) -and -not ($PinnedBuild -and $PinnedSHA256 -and $PinnedVersion)) {
  throw "If any of PinnedBuild, PinnedSHA256, or PinnedVersion is set, all three must be set."
}

if ($Android -and ($AndroidSDKs.Length -eq 0)) {
  # Enable all android SDKs by default.
  $AndroidSDKs = @("aarch64","armv7","i686","x86_64")
}

# Work around limitations of cmd passing in array arguments via powershell.exe -File
if ($AndroidSDKs.Length -eq 1) { $AndroidSDKs = $AndroidSDKs[0].Split(",") }
if ($WindowsSDKs.Length -eq 1) { $WindowsSDKs = $WindowsSDKs[0].Split(",") }
if ($Test.Length -eq 1) { $Test = $Test[0].Split(",") }

if ($AndroidSDKs.Length -gt 0) {
  # Always enable android when one of the SDKs is specified.
  $Android = $true
}

if ($Test -contains "*") {
  # Explicitly don't include llbuild yet since tests are known to fail on Windows
  $Test = @("lld", "lldb", "swift", "dispatch", "foundation", "xctest", "swift-format", "sourcekit-lsp")
}

## Declare static build and build tool parameters.

$DefaultPinned = @{
  AMD64 = @{
    PinnedBuild = "https://download.swift.org/swift-6.1.2-release/windows10/swift-6.1.2-RELEASE/swift-6.1.2-RELEASE-windows10.exe";
    PinnedSHA256 = "92A0323ED7DD333C3B05E6E0E428F3A91C77D159F6CCFC8626A996F2ACE09A0B";
    PinnedVersion = "6.1.2";
  };
  ARM64 = @{
    PinnedBuild = "https://download.swift.org/swift-6.1.2-release/windows10-arm64/swift-6.1.2-RELEASE/swift-6.1.2-RELEASE-windows10-arm64.exe";
    PinnedSHA256 = "121FB407E578178F82DCCF39A4D03527873D8F7611A801A8FC26DA52503A0C5C";
    PinnedVersion = "6.1.2";
  };
}

enum OS {
  Windows
  Android
}

$KnownPlatforms = @{
  WindowsARM64 = @{
    OS = [OS]::Windows;
    Triple = "aarch64-unknown-windows-msvc";
    Architecture = @{
      VSName = "arm64";
      CMakeName = "ARM64";
      LLVMName = "aarch64";
      ShortName = "arm64";
    };
    BinaryDir = "bin64a";
    Cache = @{};
  };

  WindowsX64 = @{
    OS = [OS]::Windows;
    Triple = "x86_64-unknown-windows-msvc";
    Architecture = @{
      VSName = "amd64";
      CMakeName = "AMD64";
      LLVMName = "x86_64";
      ShortName = "x64";
    };
    BinaryDir = "bin64";
    Cache = @{};
  };

  WindowsX86  = @{
    OS = [OS]::Windows;
    Triple = "i686-unknown-windows-msvc";
    Architecture = @{
      VSName = "x86";
      CMakeName = "i686";
      LLVMName = "i686";
      ShortName = "x86";
    };
    BinaryDir = "bin32";
    Cache = @{};
  };

  AndroidARMv7 = @{
    OS = [OS]::Android;
    Triple = "armv7-unknown-linux-androideabi$AndroidAPILevel";
    Architecture = @{
      ABI = "armeabi-v7a";
      CMakeName = "armv7-a";
      LLVMName = "armv7";
      ShortName = "armv7";
    };
    BinaryDir = "bin32a";
    Cache = @{};
  };

  AndroidARM64 = @{
    OS = [OS]::Android;
    Triple = "aarch64-unknown-linux-android$AndroidAPILevel";
    Architecture = @{
      ABI = "arm64-v8a";
      CMakeName = "aarch64";
      LLVMName = "aarch64";
      ShortName = "arm64";
    };
    BinaryDir = "bin64a";
    Cache = @{};
  };

  AndroidX86 = @{
    OS = [OS]::Android;
    Triple = "i686-unknown-linux-android$AndroidAPILevel";
    Architecture = @{
      ABI = "x86";
      CMakeName = "i686";
      LLVMName = "i686";
      ShortName = "x86";
    };
    BinaryDir = "bin32";
    Cache = @{};
  };

  AndroidX64 = @{
    OS = [OS]::Android;
    Triple = "x86_64-unknown-linux-android$AndroidAPILevel";
    Architecture = @{
      ABI = "x86_64";
      CMakeName = "x86_64";
      LLVMName = "x86_64";
      ShortName = "x64";
    };
    BinaryDir = "bin64";
    Cache = @{};
  };
}

$WiX = @{
  Version = "4.0.6";
  URL = "https://www.nuget.org/api/v2/package/wix/4.0.6";
  SHA256 = "A94DD42AE1FB56B32DA180E2173CEDA4F0D10B4C8871C5EE59ECB502131A1EB6";
  Path = [IO.Path]::Combine("$BinaryCache\WiX-4.0.6", "tools", "net6.0", "any");
}

$KnownPythons = @{
  "3.9.10" = @{
    AMD64 = @{
      URL = "https://www.nuget.org/api/v2/package/python/3.9.10";
      SHA256 = "ac43b491e9488ac926ed31c5594f0c9409a21ecbaf99dc7a93f8c7b24cf85867";
    };
    ARM64 = @{
      URL = "https://www.nuget.org/api/v2/package/pythonarm64/3.9.10";
      SHA256 = "429ada77e7f30e4bd8ff22953a1f35f98b2728e84c9b1d006712561785641f69";
    };
  }
}

$PythonModules = @{
  "packaging" = @{
    Version = "24.1";
    SHA256 = "026ed72c8ed3fcce5bf8950572258698927fd1dbda10a5e981cdf0ac37f4f002";
    Dependencies = @();
  };
  "setuptools" = @{
    Version = "75.1.0";
    SHA256 = "d59a21b17a275fb872a9c3dae73963160ae079f1049ed956880cd7c09b120538";
    Dependencies = @();
  };
  "psutil" = @{
    Version = "6.1.0";
    SHA256 = "353815f59a7f64cdaca1c0307ee13558a0512f6db064e92fe833784f08539c7a";
    Dependencies = @();
  };
  "argparse" = @{
    Version = "1.4.0";
    SHA256 = "c31647edb69fd3d465a847ea3157d37bed1f95f19760b11a47aa91c04b666314";
    Dependencies = @();
  };
  "six" = @{
    Version = "1.17.0";
    SHA256 = "4721f391ed90541fddacab5acf947aa0d3dc7d27b2e1e8eda2be8970586c3274";
    Dependencies = @();
  };
  "traceback2" = @{
    Version = "1.4.0";
    SHA256 = "8253cebec4b19094d67cc5ed5af99bf1dba1285292226e98a31929f87a5d6b23";
    Dependencies = @();
  };
  "linecache2" = @{
    Version = "1.0.0";
    SHA256 = "e78be9c0a0dfcbac712fe04fbf92b96cddae80b1b842f24248214c8496f006ef";
    Dependencies = @();
  };
}

$KnownNDKs = @{
  r26b = @{
    URL = "https://dl.google.com/android/repository/android-ndk-r26b-windows.zip"
    SHA256 = "A478D43D4A45D0D345CDA6BE50D79642B92FB175868D9DC0DFC86181D80F691E"
    ClangVersion = 17
  }
  r27c = @{
    URL = "https://dl.google.com/android/repository/android-ndk-r27c-windows.zip"
    SHA256 = "27E49F11E0CEE5800983D8AF8F4ACD5BF09987AA6F790D4439DDA9F3643D2494"
    ClangVersion = 18
  }
}

$KnownSCCache = @{
  "0.9.1" = @{
    AMD64 = @{
      URL = "https://github.com/mozilla/sccache/releases/download/v0.9.1/sccache-v0.9.1-x86_64-pc-windows-msvc.zip"
      SHA256 = "9C862BCAEF62804F2124DFC2605A0204F4FE0C5FA337BA4264E9BCAE9D2BA487"
      Path = [IO.Path]::Combine("$BinaryCache\sccache-0.9.1\sccache-v0.9.1-x86_64-pc-windows-msvc", "sccache.exe");
    }
    ARM64 = @{
      URL = "https://github.com/mozilla/sccache/releases/download/v0.9.1/sccache-v0.9.1-aarch64-pc-windows-msvc.tar.gz"
      SHA256 = "99BD024919430DE3C741658ADC60334305A61C0A109F7A334C030F0BB56007A6"
      Path = [IO.Path]::Combine("$BinaryCache\sccache-0.9.1\sccache-v0.9.1-aarch64-pc-windows-msvc", "sccache.exe")
    }
  }

  "0.10.0" = @{
    AMD64 = @{
      URL = "https://github.com/mozilla/sccache/releases/download/v0.10.0/sccache-v0.10.0-x86_64-pc-windows-msvc.zip"
      SHA256 = "6D8823B5C13E0DBA776D88C537229256ECB2F01A1D775B507FD141CB55D30578"
      Path = [IO.Path]::Combine("$BinaryCache\sccache-0.10.0\sccache-v0.10.0-x86_64-pc-windows-msvc", "sccache.exe")
    }
    ARM64 = @{
      URL = "https://github.com/mozilla/sccache/releases/download/v0.10.0/sccache-v0.10.0-aarch64-pc-windows-msvc.tar.gz"
      SHA256 = "5FD6CD6DD474E91C37510719BF27CFE1826F929E40DD383C22A7B96DA9A5458D"
      Path = [IO.Path]::Combine("$BinaryCache\sccache-0.10.0\sccache-v0.10.0-aarch64-pc-windows-msvc", "sccache.exe")
    }
  }
}

$KnownSyft = @{
  "1.29.1" = @{
    AMD64 = @{
      URL = "https://github.com/anchore/syft/releases/download/v1.29.1/syft_1.29.1_windows_amd64.zip"
      SHA256 = "3C67CD9AF40CDCC7FFCE041C8349B4A77F33810184820C05DF23440C8E0AA1D7"
      Path = [IO.Path]::Combine("$BinaryCache\syft-1.29.1", "syft.exe")
    }
  }
}

$BuildArchName = if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }
# TODO: Support other cross-compilation scenarios.
$BuildOS = [OS]::Windows
$HostOS = [OS]::Windows

$vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$VSInstallRoot = & $vswhere -nologo -latest -products "*" -all -prerelease -property installationPath
$msbuild = "$VSInstallRoot\MSBuild\Current\Bin\$BuildArchName\MSBuild.exe"

function Get-CMake {
  try {
    return (Get-Command "cmake.exe" -ErrorAction Stop).Source
  } catch {
    if (Test-Path -Path "${VSInstallRoot}\Common7\IDE\CommonExtensions\Microsoft\CMake\CMake\bin" -PathType Container) {
      return "${VSInstallRoot}\Common7\IDE\CommonExtensions\Microsoft\CMake\CMake\bin\cmake.exe"
    }
  }
  throw "CMake not found on Path nor in the Visual Studio Installation. Please Install CMake to continue."
}

function Get-Ninja {
  try {
    return (Get-Command "Ninja.exe" -ErrorAction Stop).Source
  } catch {
      if (Test-Path -Path "${VSInstallRoot}\Common7\IDE\CommonExtensions\Microsoft\CMake\Ninja" -PathType Container) {
        return "${VSInstallRoot}\Common7\IDE\CommonExtensions\Microsoft\CMake\Ninja\ninja.exe"
      }
  }
  throw "Ninja not found on Path nor in the Visual Studio Installation. Please Install Ninja to continue."
}

$cmake = Get-CMake
$ninja = Get-Ninja

$NugetRoot = "$BinaryCache\nuget"

## Select and prepare build tools, platforms, parameters, etc.

if (-not $PinnedBuild) {
  if (-not $DefaultPinned.ContainsKey($BuildArchName)) {
    throw "Default pinned toolchain definition does not contain an entry for '$BuildArchName'."
  }
  $PinnedBuild = $DefaultPinned[$BuildArchName].PinnedBuild
  $PinnedSHA256 = $DefaultPinned[$BuildArchName].PinnedSHA256
  $PinnedVersion = $DefaultPinned[$BuildArchName].PinnedVersion
}

$PinnedToolchain = [IO.Path]::GetFileNameWithoutExtension($PinnedBuild)

$HostPlatform = switch ($HostArchName) {
  "AMD64" { $KnownPlatforms[$HostOS.ToString() + "X64"] }
  "ARM64" { $KnownPlatforms[$HostOS.ToString() + "ARM64"] }
  default { throw "Unsupported processor architecture" }
}

$BuildPlatform = switch ($BuildArchName) {
  "AMD64" { $KnownPlatforms[$BuildOS.ToString() + "X64"] }
  "ARM64" { $KnownPlatforms[$BuildOS.ToString() + "ARM64"] }
  default { throw "Unsupported processor architecture" }
}

$IsCrossCompiling = $HostArchName -ne $BuildArchName

if ($Android -and ($HostPlatform -ne $KnownPlatforms["WindowsX64"])) {
  throw "Unsupported host architecture for building android SDKs"
}

# Resolve the architectures received as argument
$AndroidSDKPlatforms = @($AndroidSDKs | ForEach-Object {
  switch ($_) {
    "aarch64" { $KnownPlatforms["AndroidARM64"] }
    "armv7" { $KnownPlatforms["AndroidARMv7"] }
    "i686" { $KnownPlatforms["AndroidX86"] }
    "x86_64" { $KnownPlatforms["AndroidX64"] }
    default { throw "No Android platform for architecture $_" }
  }
})

$WindowsSDKPlatforms = @($WindowsSDKs | ForEach-Object {
  switch ($_) {
    "X64" { $KnownPlatforms["WindowsX64"] }
    "X86" { $KnownPlatforms["WindowsX86"] }
    "Arm64" { $KnownPlatforms["WindowsArm64"] }
    default { throw "No Windows platform for architecture $_" }
  }
})

## Helpers for logging and timing build steps.

$TimingData = New-Object System.Collections.Generic.List[System.Object]

function Add-TimingData {
  param
  (
    [Parameter(Mandatory)]
    [Hashtable] $Platform,
    [Parameter(Mandatory)]
    [string] $BuildStep,
    [Parameter(Mandatory)]
    [System.TimeSpan] $ElapsedTime
  )

  $TimingData.Add([PSCustomObject]@{
    Arch = $Platform.Architecture.LLVMName
    Platform = $Platform.OS.ToString()
    "Build Step" = $BuildStep
    "Elapsed Time" = $ElapsedTime
  })
}

function Write-Summary {
  Write-Host "Summary:" -ForegroundColor Cyan

  if ($EnableCaching) {
    Write-Host "SCCache:" -ForegroundColor Green
    Invoke-Program (Get-SCCache).Path --show-stats
  }

  $TotalTime = [TimeSpan]::Zero
  foreach ($Entry in $TimingData) {
    $TotalTime = $TotalTime.Add($Entry."Elapsed Time")
  }

  $SortedData = $TimingData | ForEach-Object {
    $Percentage = [math]::Round(($_.("Elapsed Time").TotalSeconds / $TotalTime.TotalSeconds) * 100, 1)
    $FormattedTime = "{0:hh\:mm\:ss\.ff}" -f $_."Elapsed Time"
    [PSCustomObject]@{
      "Build Step" = $_."Build Step"
      Platform = $_.Platform
      Arch = $_.Arch
      "Elapsed Time" = $FormattedTime
      "%" = "$Percentage%"
    }
  } | Sort-Object -Descending -Property "Elapsed Time"

  $FormattedTotalTime = "{0:hh\:mm\:ss\.ff}" -f $TotalTime
  $TotalRow = [PSCustomObject]@{
    "Build Step" = "TOTAL"
    Platform = ""
    Arch = ""
    "Elapsed Time" = $FormattedTotalTime
    "%" = "100.0%"
  }

  @($SortedData) + $TotalRow | Format-Table -AutoSize
}

function Get-AndroidNDK {
  $NDK = $KnownNDKs[$AndroidNDKVersion]
  if (-not $NDK) { throw "Unsupported Android NDK version" }
  return $NDK
}

function Get-AndroidNDKPath {
  return Join-Path -Path $BinaryCache -ChildPath "android-ndk-$AndroidNDKVersion"
}

function Get-FlexExecutable {
  return Join-Path -Path $BinaryCache -ChildPath "win_flex_bison\win_flex.exe"
}

function Get-BisonExecutable {
  return Join-Path -Path $BinaryCache -ChildPath "win_flex_bison\win_bison.exe"
}

function Get-SCCache {
  return $KnownSCCache[$SCCacheVersion][$BuildArchName]
}

function Get-PythonPath([Hashtable] $Platform) {
  return [IO.Path]::Combine("$BinaryCache\", "Python$($Platform.Architecture.CMakeName)-$PythonVersion")
}

function Get-PythonExecutable {
  return [IO.Path]::Combine((Get-PythonPath $BuildPlatform), "tools", "python.exe")
}

function Get-PythonScriptsPath {
  return [IO.Path]::Combine((Get-PythonPath $BuildPlatform), "tools", "Scripts")
}

function Get-Syft {
  return $KnownSyft[$SyftVersion][$BuildArchName]
}

function Get-SyftPath([Hashtable] $Platform) {
  return [IO.Path]::Combine("$BinaryCache\", "syft-$SyftVersion")
}

function Get-SyftExecutable {
  return [IO.Path]::Combine((Get-SyftPath $BuildPlatform), "syft.exe")
}

function Get-InstallDir([Hashtable] $Platform) {
  if ($Platform -eq $HostPlatform) {
    return [IO.Path]::Combine("$ImageRoot\", "Program Files", "Swift")
  }
  if ($Platform -eq $KnownPlatforms["WindowsARM64"]) {
    return [IO.Path]::Combine("$ImageRoot\", "Program Files (Arm64)", "Swift")
  }
  if ($Platform -eq $KnownPlatforms["WindowsX64"]) {
    return [IO.Path]::Combine("$ImageRoot\", "Program Files (Amd64)", "Swift")
  }
  if ($Platform -eq $KnownPlatforms["WindowsX86"]) {
    return [IO.Path]::Combine("$ImageRoot\", "Program Files (x86)", "Swift")
  }
  throw "Unknown Platform"
}

# For dev productivity, install the host toolchain directly using CMake.
# This allows iterating on the toolchain using ninja builds.
$HostPlatform.ToolchainInstallRoot = "$(Get-InstallDir $HostPlatform)\Toolchains\$ProductVersion+Asserts"
$HostPlatform.NoAssertsToolchainInstallRoot = "$(Get-InstallDir $HostPlatform)\Toolchains\$ProductVersion+NoAsserts"
$BuildPlatform.ToolchainInstallRoot = "$(Get-InstallDir $BuildPlatform)\Toolchains\$ProductVersion+Asserts"
$BuildPlatform.NoAssertsToolchainInstallRoot = "$(Get-InstallDir $BuildPlatform)\Toolchains\$ProductVersion+NoAsserts"

# Build functions
function Invoke-BuildStep {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position=0, Mandatory)]
    [string] $Name,
    [Parameter(Position=1, Mandatory)]
    [Hashtable] $Platform,
    [Parameter(ValueFromRemainingArguments)]
    [Object[]] $RemainingArgs
  )

  if ($Summary) {
    $Stopwatch = [Diagnostics.Stopwatch]::StartNew()
  }

  $SplatArgs = @{}
  if ($RemainingArgs) {
    $Enumerator = $RemainingArgs.GetEnumerator()
    while ($Enumerator.MoveNext()) {
      $Arg = $Enumerator.Current
      if ($Arg -is [Hashtable]) {
        $SplatArgs += $Arg
      } elseif ($Arg -is [string] -and $Arg.StartsWith('-')) {
        $ParamName = $Arg.TrimStart('-')
        $HasNextArg = $RemainingArgs.IndexOf($Arg) -lt ($RemainingArgs.Count - 1)
        if ($HasNextArg) {
          $NextArg = $RemainingArgs[$RemainingArgs.IndexOf($Arg) + 1]
          if ($NextArg -is [string] -and !$NextArg.StartsWith('-')) {
            $SplatArgs[$ParamName] = $NextArg
            $Enumerator.MoveNext() # Skip NextArg
            continue
          }
        }
        # Must be a flag.
        $SplatArgs[$ParamName] = $true
      } else {
        throw "Positional parameter '$Arg' found. The Invoke-BuildStep function only supports named parameters after the required Name and Platform parameters."
      }
    }
  }

  & $Name $Platform @SplatArgs

  if ($Summary) {
    Add-TimingData $Platform $Name $Stopwatch.Elapsed
  }
}

enum Project {
  BuildTools
  RegsGen2

  Compilers
  FoundationMacros
  TestingMacros
  ToolsSupportCore
  LLBuild
  ArgumentParser
  SQLite
  Driver
  Crypto
  Collections
  ASN1
  Certificates
  System
  Subprocess
  Build
  PackageManager
  Markdown
  Format
  LMDB
  IndexStoreDB
  SourceKitLSP
  SymbolKit
  DocC

  LLVM
  Runtime
  Dispatch
  DynamicFoundation
  XCTest
  Testing
  ClangBuiltins
  ClangRuntime
  SwiftInspect
  ExperimentalStaticRuntime
  ExperimentalStaticOverlay
  ExperimentalStaticStringProcessing
  ExperimentalStaticSynchronization
  ExperimentalStaticDistributed
  ExperimentalStaticObservation
  ExperimentalStaticDifferentiation
  ExperimentalStaticVolatile
  ExperimentalStaticDispatch
  ExperimentalStaticFoundation
}

function Get-ProjectBinaryCache([Hashtable] $Platform, [Project] $Project) {
  if ($Project -eq [Project]::Compilers) {
    if ($Platform -eq $HostPlatform) { return "$BinaryCache\5" }
    if ($Platform -eq $BuildPlatform) { return "$BinaryCache\1" }
    throw "Building Compilers for $($Platform.Triple) currently unsupported."
  }
  return "$([IO.Path]::Combine("$BinaryCache\", $Platform.Triple, $Project.ToString()))"
}

function Get-ProjectCMakeModules {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position = 0, Mandatory = $true)]
    [Hashtable] $Platform,
    [Parameter(Position = 1, Mandatory = $true)]
    [Project] $Project
  )

  return "$([IO.Path]::Combine((Get-ProjectBinaryCache $Platform $Project), "cmake", "modules"))"
}

function Get-TargetInfo([Hashtable] $Platform) {
  # Cache the result of "swiftc -print-target-info" as $Platform.Cache.TargetInfo
  $CacheKey = "TargetInfo"
  if (-not $Platform.Cache.ContainsKey($CacheKey)) {
    [void](Invoke-IsolatingEnvVars {
      $env:Path = "$(Get-PinnedToolchainRuntime);$(Get-PinnedToolchainToolsDir);${env:Path}"
      $TargetInfo = & swiftc -target $Platform.Triple -print-target-info
      if ($LastExitCode -ne 0) {
        throw "Unable to print target info for '$($Platform.Triple)'"
      }
      $TargetInfo = $TargetInfo | ConvertFrom-JSON
      $Platform.Cache[$CacheKey] = $TargetInfo.target
    })
  }
  return $Platform.Cache[$CacheKey]
}

function Get-ModuleTriple([Hashtable] $Platform) {
  return (Get-TargetInfo $Platform).moduleTriple
}

function Copy-File($Src, $Dst) {
  # Create the directory tree first so Copy-Item succeeds
  # If $Dst is the target directory, make sure it ends with "\"
  $DstDir = [IO.Path]::GetDirectoryName($Dst)
  if ($ToBatch) {
    Write-Output "md `"$DstDir`""
    Write-Output "copy /Y `"$Src`" `"$Dst`""
  } else {
    New-Item -ItemType Directory -ErrorAction Ignore $DstDir | Out-Null
    Copy-Item -Force `
      -Path $Src `
      -Destination $Dst
  }
}

function Copy-Directory($Src, $Dst) {
  if ($ToBatch) {
    Write-Output "md `"$Dst`""
    Write-Output "copy /Y `"$Src`" `"$Dst`""
  } else {
    New-Item -ItemType Directory -ErrorAction Ignore $Dst | Out-Null
    Copy-Item -Force -Recurse `
      -Path $Src `
      -Destination $Dst
  }
}

function Move-Directory($Src, $Dst) {
  if ($ToBatch) {
  } else {
    $Destination = Join-Path -Path $Dst -ChildPath (Split-Path -Path $Src -Leaf)
    if (Test-Path -Path $Destination -Type Container) {
      Remove-Item -Path $Destination -Recurse -Force | Out-Null
    }
    New-Item -ItemType Directory -ErrorAction Ignore $Dst | Out-Null
    Move-Item -Path $Src -Destination $Destination -Force | Out-Null
  }
}

function Invoke-Program() {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position = 0, Mandatory = $true)]
    [string] $Executable,
    [switch] $Silent,
    [switch] $OutNull,
    [string] $OutFile = "",
    [string] $ErrorFile = "",
    [Parameter(Position = 1, ValueFromRemainingArguments)]
    [string[]] $ExecutableArgs
  )

  if ($ToBatch) {
    # Print the invocation in batch file-compatible format
    $OutputLine = "`"$Executable`""
    $ShouldBreakLine = $false
    for ($i = 0; $i -lt $ExecutableArgs.Length; $i++) {
      if ($ShouldBreakLine -or $OutputLine.Length -ge 40) {
        $OutputLine += " ^"
        Write-Output $OutputLine
        $OutputLine = "  "
      }

      $Arg = $ExecutableArgs[$i]
      if ($Arg.Contains(" ")) {
        $OutputLine += " `"$Arg`""
      } else {
        $OutputLine += " $Arg"
      }

      # Break lines after non-switch arguments
      $ShouldBreakLine = -not $Arg.StartsWith("-")
    }

    if ($OutNull) {
      $OutputLine += " > nul"
    } elseif ($Silent) {
      $OutputLine += " *> nul"
    } else {
      if ($OutFile) { $OutputLine += " > `"$OutFile`"" }
      if ($ErrorFile) { $OutputLine += " 2> `"$ErrorFile`"" }
    }

    Write-Output $OutputLine
  } else {
    if ($OutNull) {
      & $Executable @ExecutableArgs | Out-Null
    } elseif ($Silent) {
      & $Executable @ExecutableArgs | Out-Null 2>&1| Out-Null
    } elseif ($OutFile -and $ErrorFile) {
      & $Executable @ExecutableArgs | Out-File -FilePath $OutFile -Encoding UTF8 2>&1| Out-File -FilePath $ErrorFile -Encoding UTF8
    } elseif ($OutFile) {
      & $Executable @ExecutableArgs | Out-File -FilePath $OutFile -Encoding UTF8
    } elseif ($ErrorFile) {
      & $Executable @ExecutableArgs 2>&1| Out-File -FilePath $ErrorFile -Encoding UTF8
    } else {
      & $Executable @ExecutableArgs
    }

    if ($LastExitCode -ne 0) {
      $ErrorMessage = "Error: $([IO.Path]::GetFileName($Executable)) exited with code $($LastExitCode).`n"

      $ErrorMessage += "Invocation:`n"
      $ErrorMessage += "  $Executable $ExecutableArgs`n"

      $ErrorMessage += "Call stack:`n"
      foreach ($Frame in @(Get-PSCallStack)) {
        $ErrorMessage += "  $Frame`n"
      }

      throw $ErrorMessage
    }
  }
}

function Invoke-IsolatingEnvVars([scriptblock]$Block) {
  if ($ToBatch) {
    Write-Output "setlocal enableextensions enabledelayedexpansion"
  }

  $OldVars = @{}
  foreach ($Var in (Get-ChildItem env:*).GetEnumerator()) {
    $OldVars.Add($Var.Key, $Var.Value)
  }
  try {
    & $Block
  } finally {
    Remove-Item env:*
    foreach ($Var in $OldVars.GetEnumerator()) {
      New-Item -Path "env:\$($Var.Key)" -Value $Var.Value -ErrorAction Ignore | Out-Null
    }

    if ($ToBatch) {
      Write-Output "endlocal"
    }
  }
}

function Invoke-VsDevShell([Hashtable] $Platform) {
  if (($Platform.OS -ne [OS]::Windows) -or ($BuildPlatform.OS -ne [OS]::Windows)) {
    Write-Warning "Invoke-VsDevShell called on non-Windows platform."
    return
  }

  $DevCmdArguments = "-no_logo -host_arch=$($BuildPlatform.Architecture.VSName) -arch=$($Platform.Architecture.VSName)"
  if ($CustomWinSDKRoot) {
    $DevCmdArguments += " -winsdk=none"
  } elseif ($WinSDKVersion) {
    $DevCmdArguments += " -winsdk=$WinSDKVersion"
  }

  if ($ToBatch) {
    Write-Output "call `"$VSInstallRoot\Common7\Tools\VsDevCmd.bat`" $DevCmdArguments"
  } else {
    # This dll path is valid for VS2019 and VS2022, but it was under a vsdevcmd subfolder in VS2017
    Import-Module "$VSInstallRoot\Common7\Tools\Microsoft.VisualStudio.DevShell.dll"
    Enter-VsDevShell -VsInstallPath $VSInstallRoot -SkipAutomaticLocation -DevCmdArguments $DevCmdArguments

    if ($CustomWinSDKRoot) {
      # Using a non-installed Windows SDK. Setup environment variables manually.
      $WinSDKVerIncludeRoot = "$CustomWinSDKRoot\include\$WinSDKVersion"
      $WinSDKIncludePath = "$WinSDKVerIncludeRoot\ucrt;$WinSDKVerIncludeRoot\um;$WinSDKVerIncludeRoot\shared;$WinSDKVerIncludeRoot\winrt;$WinSDKVerIncludeRoot\cppwinrt"
      $WinSDKVerLibRoot = "$CustomWinSDKRoot\lib\$WinSDKVersion"

      $env:WindowsLibPath = "$CustomWinSDKRoot\UnionMetadata\$WinSDKVersion;$CustomWinSDKRoot\References\$WinSDKVersion"
      $env:WindowsSdkBinPath = "$CustomWinSDKRoot\bin"
      $env:WindowsSDKLibVersion = "$WinSDKVersion\"
      $env:WindowsSdkVerBinPath = "$CustomWinSDKRoot\bin\$WinSDKVersion"
      $env:WindowsSDKVersion = "$WinSDKVersion\"

      $env:EXTERNAL_INCLUDE += ";$WinSDKIncludePath"
      $env:INCLUDE += ";$WinSDKIncludePath"
      $env:LIB += ";$WinSDKVerLibRoot\ucrt\$($Platform.Architecture.ShortName);$WinSDKVerLibRoot\um\$($Platform.Architecture.ShortName)"
      $env:LIBPATH += ";$env:WindowsLibPath"
      $env:PATH += ";$env:WindowsSdkVerBinPath\$($Platform.Architecture.ShortName);$env:WindowsSdkBinPath\$($Platform.Architecture.ShortName)"
      $env:UCRTVersion = $WinSDKVersion
      $env:UniversalCRTSdkDir = $CustomWinSDKRoot
    }
  }
}

function Get-Dependencies {
  Write-Host "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies ..." -ForegroundColor Cyan
  $ProgressPreference = "SilentlyContinue"

  $WebClient = New-Object Net.WebClient

  function DownloadAndVerify($URL, $Destination, $Hash) {
    if (Test-Path $Destination) {
      return
    }

    Write-Output "$Destination not found. Downloading ..."
    if ($ToBatch) {
      Write-Output "md `"$(Split-Path -Path $Destination -Parent)`""
      Write-Output "curl.exe -sL $URL -o $Destination"
      Write-Output "(certutil -HashFile $Destination SHA256) == $Hash || (exit /b)"
    } else {
      New-Item -ItemType Directory (Split-Path -Path $Destination -Parent) -ErrorAction Ignore | Out-Null
      $WebClient.DownloadFile($URL, $Destination)
      $SHA256 = Get-FileHash -Path $Destination -Algorithm SHA256
      if ($SHA256.Hash -ne $Hash) {
        throw "SHA256 mismatch ($($SHA256.Hash) vs $Hash)"
      }
    }
  }

  function Expand-ZipFile {
    param
    (
        [string]$ZipFileName,
        [string]$BinaryCache,
        [string]$ExtractPath,
        [bool]$CreateExtractPath = $true
    )

    $source = Join-Path -Path $BinaryCache -ChildPath $ZipFileName
    $destination = Join-Path -Path $BinaryCache -ChildPath $ExtractPath

    # Check if the extracted directory already exists and is up to date.
    if (Test-Path $destination) {
        $zipLastWriteTime = (Get-Item $source).LastWriteTime
        $extractedLastWriteTime = (Get-Item $destination).LastWriteTime
        # Compare the last write times
        if ($zipLastWriteTime -le $extractedLastWriteTime) {
            Write-Output "'$ZipFileName' is already extracted and up to date."
            return
        }
    }

    $destination = if ($CreateExtractPath) { $destination } else { $BinaryCache }

    Write-Output "Extracting '$ZipFileName' ..."
    New-Item -ItemType Directory -ErrorAction Ignore -Path $BinaryCache | Out-Null
    Expand-Archive -Path $source -DestinationPath $destination -Force
  }

  function Export-Toolchain {
    param
    (
        [string]$InstallerExeName,
        [string]$BinaryCache,
        [string]$ToolchainName
    )

    $source = Join-Path -Path $BinaryCache -ChildPath $InstallerExeName
    $destination = Join-Path -Path $BinaryCache -ChildPath toolchains\$ToolchainName

    # Check if the extracted directory already exists and is up to date.
    if (Test-Path $destination) {
        $installerWriteTime = (Get-Item $source).LastWriteTime
        $extractedWriteTime = (Get-Item $destination).LastWriteTime
        if ($installerWriteTime -le $extractedWriteTime) {
            Write-Output "'$InstallerExeName' is already extracted and up to date."
            return
        }
    }

    Write-Output "Extracting '$InstallerExeName' ..."

    # The new runtime MSI is built to expand files into the immediate directory. So, setup the installation location.
    New-Item -ItemType Directory -ErrorAction Ignore $BinaryCache\toolchains\$PinnedToolchain\LocalApp\Programs\Swift\Runtimes\$(Get-PinnedToolchainVersion)\usr\bin | Out-Null
    Invoke-Program "$($WiX.Path)\wix.exe" -- burn extract $BinaryCache\$InstallerExeName -out $BinaryCache\toolchains\ -outba $BinaryCache\toolchains\
    Get-ChildItem "$BinaryCache\toolchains\WixAttachedContainer" -Filter "*.msi" | ForEach-Object {
      $LogFile = [System.IO.Path]::ChangeExtension($_.Name, "log")
      $TARGETDIR = if ($_.Name -eq "rtl.msi") { "$BinaryCache\toolchains\$ToolchainName\LocalApp\Programs\Swift\Runtimes\$(Get-PinnedToolchainVersion)\usr\bin" } else { "$BinaryCache\toolchains\$ToolchainName" }
      Invoke-Program -OutNull msiexec.exe /lvx! $BinaryCache\toolchains\$LogFile /qn /a $BinaryCache\toolchains\WixAttachedContainer\$($_.Name) ALLUSERS=0 TARGETDIR=$TARGETDIR
    }
  }

  if ($IncludeSBoM) {
    $syft = Get-Syft
    DownloadAndVerify $syft.URL "$BinaryCache\syft-$SyftVersion.zip" $syft.SHA256
    Expand-ZipFile syft-$SyftVersion.zip $BinaryCache syft-$SyftVersion
  }

  if ($SkipBuild -and $SkipPackaging) { return }

  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()
  if ($ToBatch) {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies..."
  }

  DownloadAndVerify $WiX.URL "$BinaryCache\WiX-$($WiX.Version).zip" $WiX.SHA256
  Expand-ZipFile WiX-$($WiX.Version).zip $BinaryCache WiX-$($WiX.Version)

  if ($SkipBuild) { return }

  if ($EnableCaching) {
    $SCCache = Get-SCCache
    DownloadAndVerify $SCCache.URL "$BinaryCache\sccache-$SCCacheVersion.zip" $SCCache.SHA256
    Expand-ZipFile sccache-$SCCacheVersion.zip $BinaryCache sccache-$SCCacheVersion
  }

  DownloadAndVerify $PinnedBuild "$BinaryCache\$PinnedToolchain.exe" $PinnedSHA256

  if ($Test -contains "lldb") {
    # The make tool isn't part of MSYS
    $GnuWin32MakeURL = "https://downloads.sourceforge.net/project/ezwinports/make-4.4.1-without-guile-w32-bin.zip"
    $GnuWin32MakeHash = "fb66a02b530f7466f6222ce53c0b602c5288e601547a034e4156a512dd895ee7"
    DownloadAndVerify $GnuWin32MakeURL "$BinaryCache\GnuWin32Make-4.4.1.zip" $GnuWin32MakeHash
    Expand-ZipFile GnuWin32Make-4.4.1.zip $BinaryCache GnuWin32Make-4.4.1
  }

  # TODO(compnerd) stamp/validate that we need to re-extract
  New-Item -ItemType Directory -ErrorAction Ignore $BinaryCache\toolchains | Out-Null
  Export-Toolchain "$PinnedToolchain.exe" $BinaryCache $PinnedToolchain

  function Get-KnownPython([string] $ArchName) {
    if (-not $KnownPythons.ContainsKey($PythonVersion)) {
      throw "Unknown python version: $PythonVersion"
    }
    return $KnownPythons[$PythonVersion].$ArchName
  }

  function Install-Python([string] $ArchName) {
    $Python = Get-KnownPython $ArchName
    DownloadAndVerify $Python.URL "$BinaryCache\Python$ArchName-$PythonVersion.zip" $Python.SHA256
    if (-not $ToBatch) {
      Expand-ZipFile Python$ArchName-$PythonVersion.zip "$BinaryCache" Python$ArchName-$PythonVersion
    }
  }

  function Install-PIPIfNeeded() {
    try {
      Invoke-Program -Silent "$(Get-PythonExecutable)" -m pip
    } catch {
      Write-Output "Installing pip ..."
      Invoke-Program -OutNull "$(Get-PythonExecutable)" '-I' -m ensurepip -U --default-pip
    } finally {
      Write-Output "pip installed."
    }
  }

  function Test-PythonModuleInstalled([string] $ModuleName) {
    try {
      Invoke-Program -Silent "$(Get-PythonExecutable)" -c "import $ModuleName"
      return $true;
    } catch {
      return $false;
    }
  }

  function Install-PythonModule([string] $ModuleName) {
    if (Test-PythonModuleInstalled $ModuleName) {
      Write-Output "$ModuleName already installed."
      return;
    }
    $TempRequirementsTxt = New-TemporaryFile
    $Module = $PythonModules[$ModuleName]
    $Dependencies = $Module["Dependencies"]
    Write-Output "$ModuleName==$($Module.Version) --hash=`"sha256:$($Module.SHA256)`"" >> $TempRequirementsTxt
    for ($i = 0; $i -lt $Dependencies.Length; $i++) {
      $Dependency = $PythonModules[$Dependencies[$i]]
      Write-Output "$($Dependencies[$i])==$($Dependency.Version) --hash=`"sha256:$($Dependency.SHA256)`"" >> $TempRequirementsTxt
    }
    Invoke-Program -OutNull "$(Get-PythonExecutable)" '-I' -m pip install -r $TempRequirementsTxt --require-hashes --no-binary==:all: --disable-pip-version-check
    Write-Output "$ModuleName installed."
  }

  function Install-PythonModules() {
    Install-PIPIfNeeded
    Install-PythonModule "packaging" # For building LLVM 18+
    Install-PythonModule "setuptools" # Required for SWIG support
    if ($Test -contains "lldb") {
      Install-PythonModule "psutil" # Required for testing LLDB
    }
  }

  Install-Python $HostArchName
  if ($IsCrossCompiling) {
    Install-Python $BuildArchName
  }

  # Ensure Python modules that are required as host build tools
  Install-PythonModules

  if ($Android) {
    $NDK = Get-AndroidNDK
    DownloadAndVerify $NDK.URL "$BinaryCache\android-ndk-$AndroidNDKVersion-windows.zip" $NDK.SHA256
    Expand-ZipFile -ZipFileName "android-ndk-$AndroidNDKVersion-windows.zip" -BinaryCache $BinaryCache -ExtractPath "android-ndk-$AndroidNDKVersion" -CreateExtractPath $false
  }

  if ($IncludeDS2) {
    $WinFlexBisonVersion = "2.5.25"
    $WinFlexBisonURL = "https://github.com/lexxmark/winflexbison/releases/download/v$WinFlexBisonVersion/win_flex_bison-$WinFlexBisonVersion.zip"
    $WinFlexBisonHash = "8D324B62BE33604B2C45AD1DD34AB93D722534448F55A16CA7292DE32B6AC135"
    DownloadAndVerify $WinFlexBisonURL "$BinaryCache\win_flex_bison-$WinFlexBisonVersion.zip" $WinFlexBisonHash

    Expand-ZipFile -ZipFileName "win_flex_bison-$WinFlexBisonVersion.zip" -BinaryCache $BinaryCache -ExtractPath "win_flex_bison"
  }

  if ($WinSDKVersion) {
    try {
      # Check whether VsDevShell can already resolve the requested Windows SDK Version
      Invoke-IsolatingEnvVars { Invoke-VsDevShell $HostPlatform }
    } catch {
      $Package = Microsoft.Windows.SDK.CPP

      Write-Output "Windows SDK $WinSDKVersion not found. Downloading from nuget.org ..."
      Invoke-Program nuget install $Package -Version $WinSDKVersion -OutputDirectory $NugetRoot

      # Set to script scope so Invoke-VsDevShell can read it.
      $script:CustomWinSDKRoot = "$NugetRoot\$Package.$WinSDKVersion\c"

      # Install each required architecture package and move files under the base /lib directory.
      $WinSDKPlatforms = $WindowsSDKPlatforms.Clone()
      if (-not ($HostPlatform -in $WinSDKPlatforms)) {
        $WinSDKPlatforms += $HostPlatform
      }

      foreach ($Platform in $WinSDKPlatforms) {
        Invoke-Program nuget install $Package.$($Platform.Architecture.ShortName) -Version $WinSDKVersion -OutputDirectory $NugetRoot
        Copy-Directory "$NugetRoot\$Package.$($Platform.Architecture.ShortName).$WinSDKVersion\c\*" "$CustomWinSDKRoot\lib\$WinSDKVersion"
      }
    }
  }

  if (-not $ToBatch) {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies took $($Stopwatch.Elapsed)"
    Write-Host ""
  }
  if ($Summary) {
    Add-TimingData $BuildPlatform "Get-Dependencies" $Stopwatch.Elapsed
  }
}

function Get-PinnedToolchainToolsDir() {
  $ToolchainsRoot = [IO.Path]::Combine("$BinaryCache\toolchains", "$PinnedToolchain", "LocalApp", "Programs", "Swift", "Toolchains")

  # NOTE: Add a workaround for the main snapshots that inadvertently used the
  # wrong version when they were built. This allows use of the nightly snapshot
  # as a pinned toolchain.
  if ((Get-PinnedToolchainVersion) -eq "0.0.0") {
    if (-not (Test-Path "$ToolchainsRoot\0.0.0+Asserts\usr\bin")) {
      if (Test-Path "$ToolchainsRoot\6.0.0+Asserts\usr\bin") {
        return "$ToolchainsRoot\6.0.0+Asserts\usr\bin"
      }
    }
  }

  $VariantToolchainPath = [IO.Path]::Combine($ToolchainsRoot, "$(Get-PinnedToolchainVersion)+Asserts", "usr", "bin")

  if (Test-Path $VariantToolchainPath) {
    return $VariantToolchainPath
  }

  return [IO.Path]::Combine("$BinaryCache\", "toolchains", $PinnedToolchain,
    "Library", "Developer", "Toolchains",
    "unknown-Asserts-development.xctoolchain", "usr", "bin")
}

function Get-PinnedToolchainSDK() {
  return [IO.Path]::Combine("$BinaryCache\", "toolchains", $PinnedToolchain,
    "LocalApp", "Programs", "Swift", "Platforms", (Get-PinnedToolchainVersion),
    "Windows.platform", "Developer", "SDKs", "Windows.sdk")
}

function Get-PinnedToolchainRuntime() {
  return [IO.Path]::Combine("$BinaryCache\", "toolchains", $PinnedToolchain,
    "LocalApp", "Programs", "Swift", "Runtimes", (Get-PinnedToolchainVersion),
    "usr", "bin")
}

function Get-PinnedToolchainVersion() {
  if (Test-Path variable:PinnedVersion) {
    return $PinnedVersion
  }
  throw "PinnedVersion must be set"
}

function Add-KeyValueIfNew([hashtable]$Hashtable, [string]$Key, [string]$Value) {
  if (-not $Hashtable.Contains($Key)) {
    $Hashtable.Add($Key, $Value)
  }
}

function Add-FlagsDefine([hashtable]$Defines, [string]$Name, [string[]]$Value) {
  if ($Defines.Contains($Name)) {
    $Defines[$name] = @($Defines[$name]) + $Value
  } else {
    $Defines.Add($Name, $Value)
  }
}

function Get-PlatformRoot([OS] $OS) {
  return ([IO.Path]::Combine((Get-InstallDir $HostPlatform), "Platforms", "$($OS.ToString()).platform"))
}

function Get-SwiftSDK {
  param
  (
    [Parameter(Mandatory)]
    [OS] $OS,
    [string] $Identifier = $OS.ToString()
  )
  return ([IO.Path]::Combine((Get-PlatformRoot $OS), "Developer", "SDKs", "$Identifier.sdk"))
}

function Build-CMakeProject {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [string] $Src,
    [string] $Bin,
    [string] $InstallTo = "",
    [hashtable] $Platform,
    [string] $Generator = "Ninja",
    [string] $CacheScript = "",
    [ValidateSet("ASM_MASM", "C", "CXX")]
    [string[]] $UseMSVCCompilers = @(),
    [ValidateSet("ASM", "C", "CXX", "Swift")]
    [string[]] $UseBuiltCompilers = @(),
    [ValidateSet("ASM", "C", "CXX", "Swift")]
    [string[]] $UsePinnedCompilers = @(),
    [switch] $AddAndroidCMakeEnv = $false,
    [switch] $UseGNUDriver = $false,
    [string] $SwiftSDK = $null,
    [hashtable] $Defines = @{}, # Values are either single strings or arrays of flags
    [string[]] $BuildTargets = @()
  )

  if ($ToBatch) {
    Write-Output ""
    Write-Output "echo Building '$Src' to '$Bin' ..."
  } else {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Building '$Src' to '$Bin' ..."
  }

  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()

  # Enter the developer command shell early so we can resolve cmake.exe
  # for version checks.
  Invoke-IsolatingEnvVars {
    if ($Platform.OS -eq [OS]::Windows) {
      Invoke-VsDevShell $Platform
    }

    if ($AddAndroidCMakeEnv) {
      # Set generic android options if we need to build an Android runtime component
      # while building the compiler. Use an environment variable to pass it, to
      # ensure that it can be accessed from the cmake cache file.
      $env:NDKPATH = Get-AndroidNDKPath
    }

    $UseASM = $UseBuiltCompilers.Contains("ASM") -or $UsePinnedCompilers.Contains("ASM")
    $UseASM_MASM = $UseMSVCCompilers.Contains("ASM_MASM")
    $UseC = $UseBuiltCompilers.Contains("C") -or $UseMSVCCompilers.Contains("C") -or $UsePinnedCompilers.Contains("C")
    $UseCXX = $UseBuiltCompilers.Contains("CXX") -or $UseMSVCCompilers.Contains("CXX") -or $UsePinnedCompilers.Contains("CXX")
    $UseSwift = $UseBuiltCompilers.Contains("Swift") -or $UsePinnedCompilers.Contains("Swift")

    # Add additional defines (unless already present)
    $Defines = $Defines.Clone()

    Add-KeyValueIfNew $Defines CMAKE_BUILD_TYPE Release

    # Avoid specifying `CMAKE_SYSTEM_NAME` and `CMAKE_SYSTEM_PROCESSOR` on
    # Windows and in the case that we are not cross-compiling.
    #
    # TODO(etcwilde) consider removing this once we have removed
    # SwiftSupport.cmake across the project.
    if (($Platform.OS -ne [OS]::Windows) -or ($Platform.Architecture.CMakeName -ne $BuildPlatform.Architecture.CMakeName)) {
      Add-KeyValueIfNew $Defines CMAKE_SYSTEM_NAME $Platform.OS.ToString()
      Add-KeyValueIfNew $Defines CMAKE_SYSTEM_PROCESSOR $Platform.Architecture.CMakeName
    }

    # Always prefer the CONFIG format for the packages so that we can build
    # against the build tree.
    Add-KeyValueIfNew $Defines CMAKE_FIND_PACKAGE_PREFER_CONFIG YES

    switch ($Platform.OS) {
      Windows {
        if ($UseASM) {
          $Driver = $(if ($UseGNUDriver) { "clang.exe" } else { "clang-cl.exe" })
          $ASM = if ($UseBuiltCompilers.Contains("ASM")) {
            [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", $Driver)
          } elseif ($UsePinnedCompilers.Contains("ASM")) {
            Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath $Driver
          }

          Add-KeyValueIfNew $Defines CMAKE_ASM_COMPILER $ASM
          Add-KeyValueIfNew $Defines CMAKE_ASM_FLAGS @("--target=$($Platform.Triple)")
          Add-KeyValueIfNew $Defines CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDLL "/MD"

          if ($DebugInfo) {
            $ASMDebugFlags = if ($CDebugFormat -eq "dwarf") {
              if ($UseGNUDriver) { @("-gdwarf") } else { @("-clang:-gdwarf") }
            } else {
              if ($UseGNUDriver) { @("-gcodeview") } else { @("-clang:-gcodeview") }
            }

            # CMake does not set a default value for the ASM compiler debug
            # information format flags with non-MSVC compilers, so we explicitly
            # set a default here.
            Add-FlagsDefine $Defines CMAKE_ASM_COMPILE_OPTIONS_MSVC_DEBUG_INFORMATION_FORMAT_Embedded $ASMDebugFlags
          }
        }

        if ($UseASM_MASM) {
          $ASM_MASM = if ($Platform.Architecture.VSName -eq "x86") {
            "ml.exe"
          } else {
            "ml64.exe"
          }

          Add-KeyValueIfNew $Defines CMAKE_ASM_MASM_COMPILER $ASM_MASM
          Add-KeyValueIfNew $Defines CMAKE_ASM_MASM_FLAGS @("/nologo" ,"/quiet")
        }

        if ($UseC) {
          $CC = if ($UseMSVCCompilers.Contains("C")) {
            "cl.exe"
          } else {
            $Driver = $(if ($UseGNUDriver) { "clang.exe" } else { "clang-cl.exe" })
            if ($UseBuiltCompilers.Contains("C")) {
              [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", $Driver)
            } elseif ($UsePinnedCompilers.Contains("C")) {
              Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath $Driver
            }
          }

          Add-KeyValueIfNew $Defines CMAKE_C_COMPILER $CC
          Add-KeyValueIfNew $Defines CMAKE_C_COMPILER_TARGET $Platform.Triple

          $CFLAGS = if ($UseGNUDriver) {
            # TODO(compnerd) we should consider enabling stack protector usage for standard libraries.
            @("-fno-stack-protector", "-ffunction-sections", "-fdata-sections", "-fomit-frame-pointer")
          } elseif ($UseMSVCCompilers.Contains("C")) {
            @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:preprocessor", "/Zc:inline")
          } else {
            # clang-cl does not support the /Zc:preprocessor flag.
            @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline")
          }

          if ($DebugInfo) {
            if ($UsePinnedCompilers.Contains("C") -or $UseBuiltCompilers.Contains("C")) {
              if ($CDebugFormat -eq "dwarf") {
                $CFLAGS += if ($UseGNUDriver) {
                  @("-gdwarf")
                } else {
                  @("-clang:-gdwarf")
                }
              }
            }
          }

          Add-FlagsDefine $Defines CMAKE_C_FLAGS $CFLAGS
        }

        if ($UseCXX) {
          $CXX = if ($UseMSVCCompilers.Contains("CXX")) {
            "cl.exe"
          } else {
            $Driver = $(if ($UseGNUDriver) { "clang++.exe" } else { "clang-cl.exe" })
            if ($UseBuiltCompilers.Contains("CXX")) {
              [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", $Driver)
            } elseif ($UsePinnedCompilers.Contains("CXX")) {
              Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath $Driver
            }
          }

          Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER $CXX
          Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER_TARGET $Platform.Triple

          $CXXFLAGS = if ($UseGNUDriver) {
            # TODO(compnerd) we should consider enabling stack protector usage for standard libraries.
            @("-fno-stack-protector", "-ffunction-sections", "-fdata-sections", "-fomit-frame-pointer")
          } elseif ($UseMSVCCompilers.Contains("CXX")) {
            @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:preprocessor", "/Zc:inline", "/Zc:__cplusplus")
          } else {
            # clang-cl does not support the /Zc:preprocessor flag.
            @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline", "/Zc:__cplusplus")
          }

          if ($DebugInfo) {
            if ($UsePinnedCompilers.Contains("CXX") -or $UseBuiltCompilers.Contains("CXX")) {
              if ($CDebugFormat -eq "dwarf") {
                $CXXFLAGS += if ($UseGNUDriver) {
                  @("-gdwarf")
                } else {
                  @("-clang:-gdwarf")
                }
              }
            }
          }

          Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXFLAGS
        }

        if ($UseSwift) {
          if ($UseBuiltCompilers.Contains("Swift")) {
            Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_WORKS "YES"
          }

          $SWIFTC = if ($UseBuiltCompilers.Contains("Swift")) {
            [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "swiftc.exe")
          } elseif ($UsePinnedCompilers.Contains("Swift")) {
            Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath "swiftc.exe"
          }

          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER $SWIFTC
          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_TARGET (Get-ModuleTriple $Platform)

          # TODO(compnerd): remove this once we have the early swift-driver
          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_USE_OLD_DRIVER "YES"

          [string[]] $SwiftFlags = @();

          $SwiftFlags += if ($SwiftSDK) {
            @("-sdk", $SwiftSDK)
          } else {
            @()
          }

          $SwiftFlags += if ($DebugInfo) {
            if ($SwiftDebugFormat -eq "dwarf") {
              @("-g", "-debug-info-format=dwarf", "-use-ld=lld-link", "-Xlinker", "/DEBUG:DWARF")
            } else {
              @("-g", "-debug-info-format=codeview", "-Xlinker", "/DEBUG")
            }
          } else {
            @("-gnone")
          }

          # Disable EnC as that introduces padding in the conformance tables
          $SwiftFlags += @("-Xlinker", "/INCREMENTAL:NO")
          # Swift requires COMDAT folding and de-duplication
          $SwiftFlags += @("-Xlinker", "/OPT:REF", "-Xlinker", "/OPT:ICF")

          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftFlags
          # Workaround CMake 3.26+ enabling `-wmo` by default on release builds
          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELEASE "-O"
          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELWITHDEBINFO "-O"
        }

        $LinkerFlags = if ($UseGNUDriver) {
          @("-Xlinker", "/INCREMENTAL:NO", "-Xlinker", "/OPT:REF", "-Xlinker", "/OPT:ICF")
        } else {
          @("/INCREMENTAL:NO", "/OPT:REF", "/OPT:ICF")
        }

        if ($DebugInfo) {
          if ($UseASM -or $UseC -or $UseCXX) {
            # Prefer `/Z7` over `/ZI`
            # By setting the debug information format, the appropriate C/C++
            # flags will be set for codeview debug information format so there
            # is no need to set them explicitly above.
            Add-KeyValueIfNew $Defines CMAKE_MSVC_DEBUG_INFORMATION_FORMAT Embedded
            Add-KeyValueIfNew $Defines CMAKE_POLICY_DEFAULT_CMP0141 NEW

            $LinkerFlags += if ($UseGNUDriver) {
              @("-Xlinker", "/DEBUG")
            } else {
              @("/DEBUG")
            }
          }
        }

        Add-FlagsDefine $Defines CMAKE_EXE_LINKER_FLAGS $LinkerFlags
        Add-FlagsDefine $Defines CMAKE_SHARED_LINKER_FLAGS $LinkerFlags
      }

      Android {
        $AndroidNDKPath = Get-AndroidNDKPath
        $AndroidPrebuiltRoot = "$AndroidNDKPath\toolchains\llvm\prebuilt\$($BuildPlatform.OS.ToString().ToLowerInvariant())-$($BuildPlatform.Architecture.LLVMName)"
        $AndroidSysroot = "$AndroidPrebuiltRoot\sysroot"

        Add-KeyValueIfNew $Defines CMAKE_ANDROID_API "$AndroidAPILevel"
        Add-KeyValueIfNew $Defines CMAKE_ANDROID_ARCH_ABI $Platform.Architecture.ABI
        Add-KeyValueIfNew $Defines CMAKE_ANDROID_NDK "$AndroidNDKPath"

        if ($UseASM) {
        }

        if ($UseC) {
          Add-KeyValueIfNew $Defines CMAKE_C_COMPILER_TARGET $Platform.Triple

          $CFLAGS = @("--sysroot=${AndroidSysroot}", "-ffunction-sections", "-fdata-sections")
          if ($DebugInfo) {
            $CFLAGS += @("-gdwarf")
          }
          Add-FlagsDefine $Defines CMAKE_C_FLAGS $CFLAGS
        }

        if ($UseCXX) {
          Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER_TARGET $Platform.Triple

          $CXXFLAGS = @("--sysroot=${AndroidSysroot}", "-ffunction-sections", "-fdata-sections")
          if ($DebugInfo) {
            $CXXFLAGS += @("-gdwarf")
          }
          Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXFLAGS
        }

        if ($UseSwift) {
          if ($UseBuiltCompilers.Contains("Swift")) {
            Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_WORKS "YES"
          }

          # FIXME(compnerd) remove this once the old runtimes build path is removed.
          Add-KeyValueIfNew $Defines SWIFT_ANDROID_NDK_PATH "$AndroidNDKPath"

          $SWIFTC = if ($UseBuiltCompilers.Contains("Swift")) {
            [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "swiftc.exe")
          } else {
            Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath  "swiftc.exe"
          }
          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER $SWIFTC
          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_TARGET (Get-ModuleTriple $Platform)

          # TODO(compnerd) remove this once we have the early swift-driver
          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_USE_OLD_DRIVER "YES"

          [string[]] $SwiftFlags = @()

          $SwiftFlags += if ($SwiftSDK) {
            @("-sdk", $SwiftSDK, "-sysroot", $AndroidSysroot)
          } else {
            @()
          }

          $SwiftFlags += @(
            "-Xclang-linker", "-target", "-Xclang-linker", $Platform.Triple,
            "-Xclang-linker", "--sysroot", "-Xclang-linker", $AndroidSysroot,
            "-Xclang-linker", "-resource-dir", "-Xclang-linker", "${AndroidPrebuiltRoot}\lib\clang\$($(Get-AndroidNDK).ClangVersion)"
          )

          $SwiftFlags += if ($DebugInfo) { @("-g") } else { @("-gnone") }

          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftFlags
          # Workaround CMake 3.26+ enabling `-wmo` by default on release builds
          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELEASE "-O"
          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELWITHDEBINFO "-O"
        }

        $UseBuiltASMCompiler = $UseBuiltCompilers.Contains("ASM")
        $UseBuiltCCompiler = $UseBuiltCompilers.Contains("C")
        $UseBuiltCXXCompiler = $UseBuiltCompilers.Contains("CXX")

        if ($UseBuiltASMCompiler -or $UseBuiltCCompiler -or $UseBuiltCXXCompiler) {
          # Use a built lld linker as the Android's NDK linker might be too old
          # and not support all required relocations needed by the Swift
          # runtime.
          $ld = ([IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "ld.lld"))
          Add-FlagsDefine $Defines CMAKE_SHARED_LINKER_FLAGS "--ld-path=$ld"
          Add-FlagsDefine $Defines CMAKE_EXE_LINKER_FLAGS "--ld-path=$ld"
        }

        # TODO(compnerd) we should understand why CMake does not understand
        # that the object file format is ELF when targeting Android on Windows.
        # This indication allows it to understand that it can use `chrpath` to
        # change the RPATH on the dynamic libraries.
        Add-FlagsDefine $Defines CMAKE_EXECUTABLE_FORMAT "ELF"
      }
    }

    if ($EnableCaching) {
      if ($UseC) {
        Add-KeyValueIfNew $Defines CMAKE_C_COMPILER_LAUNCHER $(Get-SCCache).Path
      }

      if ($UseCXX) {
        Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER_LAUNCHER $(Get-SCCache).Path
      }
    }

    if ($InstallTo) {
      Add-KeyValueIfNew $Defines CMAKE_INSTALL_PREFIX $InstallTo
    }

    Add-KeyValueIfNew $Defines CMAKE_MAKE_PROGRAM "$ninja"

    # Generate the project
    $cmakeGenerateArgs = @("-B", $Bin, "-S", $Src, "-G", $Generator)
    if ($CacheScript) {
      $cmakeGenerateArgs += @("-C", $CacheScript)
    }
    foreach ($Define in ($Defines.GetEnumerator() | Sort-Object Name)) {
      # The quoting gets tricky to support defines containing compiler flags args,
      # some of which can contain spaces, for example `-D` `Flags=-flag "C:/Program Files"`
      # Avoid backslashes since they are going into CMakeCache.txt,
      # where they are interpreted as escapes.
      if ($Define.Value -is [string]) {
        # Single token value, no need to quote spaces, the splat operator does the right thing.
        $Value = $Define.Value.Replace("\", "/")
      } else {
        # Flags array, multiple tokens, quoting needed for tokens containing spaces
        $Value = ""
        foreach ($Arg in $Define.Value) {
          if ($Value.Length -gt 0) {
            $Value += " "
          }

          $ArgWithForwardSlashes = $Arg.Replace("\", "/")
          if ($ArgWithForwardSlashes.Contains(" ")) {
            # Escape the quote so it makes it through. PowerShell 5 and Core
            # handle quotes differently, so we need to check the version.
            $quote = if ($PSEdition  -eq "Core") { '"' } else { '\"' }
            $Value += "$quote$ArgWithForwardSlashes$quote"
          } else {
            $Value += $ArgWithForwardSlashes
          }
        }
      }

      $cmakeGenerateArgs += @("-D", "$($Define.Key)=$Value")
    }

    if ($UseBuiltCompilers.Contains("Swift")) {
      $env:Path = "$([IO.Path]::Combine((Get-InstallDir $BuildPlatform), "Runtimes", $ProductVersion, "usr", "bin"));$(Get-CMarkBinaryCache $BuildPlatform)\src;$($BuildPlatform.ToolchainInstallRoot)\usr\bin;$(Get-PinnedToolchainRuntime);${env:Path}"
    } elseif ($UsePinnedCompilers.Contains("Swift")) {
      $env:Path = "$(Get-PinnedToolchainRuntime);${env:Path}"
    }
    if ($ToBatch) {
      Write-Output ""
      Write-Output "echo $cmake $cmakeGenerateArgs"
    } else {
      Write-Host "$cmake $cmakeGenerateArgs"
    }
    Invoke-Program $cmake @cmakeGenerateArgs

    # Build all requested targets
    foreach ($Target in $BuildTargets) {
      if ($Target -eq "default") {
        Invoke-Program $cmake --build $Bin
      } else {
        Invoke-Program $cmake --build $Bin --target $Target
      }
    }

    if ($BuildTargets.Length -eq 0 -and $InstallTo) {
      Invoke-Program $cmake --build $Bin --target install
    }
  }

  if (-not $ToBatch) {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Finished building '$Src' to '$Bin' in $($Stopwatch.Elapsed)"
    Write-Host ""
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

  if ($ToBatch) {
    Write-Output ""
    Write-Output "echo $ActionForOutput '$Src' to '$Bin' ..."
  } else {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] $ActionForOutput '$Src' to '$Bin' ..."
  }

  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()

  Invoke-IsolatingEnvVars {
    $RuntimeInstallRoot = [IO.Path]::Combine((Get-InstallDir $BuildPlatform), "Runtimes", $ProductVersion)

    $env:Path = "$RuntimeInstallRoot\usr\bin;$($BuildPlatform.ToolchainInstallRoot)\usr\bin;${env:Path}"
    $env:SDKROOT = (Get-SwiftSDK Windows)
    $env:SWIFTCI_USE_LOCAL_DEPS = "1"

    $Arguments = @(
      "--scratch-path", $Bin,
      "--package-path", $Src,
      "-c", $Configuration,
      "-Xbuild-tools-swiftc", "-I$(Get-SwiftSDK Windows)\usr\lib\swift",
      "-Xbuild-tools-swiftc", "-L$(Get-SwiftSDK Windows)\usr\lib\swift\windows",
      "-Xcc", "-I$(Get-SwiftSDK Windows)\usr\lib\swift",
      "-Xlinker", "-L$(Get-SwiftSDK Windows)\usr\lib\swift\windows"
    )
    if ($DebugInfo) {
      if ($SwiftDebugFormat -eq "dwarf") {
        $Arguments += @("-debug-info-format", "dwarf")
      } else {
        $Arguments += @("-debug-info-format", "codeview")
      }
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

    Invoke-Program "$($BuildPlatform.ToolchainInstallRoot)\usr\bin\swift.exe" $ActionName @Arguments @AdditionalArguments
  }

  if (-not $ToBatch) {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Finished building '$Src' to '$Bin' in $($Stopwatch.Elapsed)"
    Write-Host ""
  }
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
    # WiX v4 will accept a semantic version string for Bundles,
    # but Packages still require a purely numerical version number,
    # so trim any semantic versioning suffixes
    $ProductVersionArg = [regex]::Replace($ProductVersion, "[-+].*", "")
  }

  $Properties = $Properties.Clone()
  Add-KeyValueIfNew $Properties Configuration Release
  Add-KeyValueIfNew $Properties BaseOutputPath "$BinaryCache\$($Platform.Triple)\installer\"
  Add-KeyValueIfNew $Properties ProductArchitecture $Platform.Architecture.VSName
  Add-KeyValueIfNew $Properties ProductVersion $ProductVersionArg

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

  Invoke-Program $msbuild @MSBuildArgs
}

# TODO(compnerd): replace this with Get-{Build,Host,Target}ProjectBinaryCache
function Get-CMarkBinaryCache([Hashtable] $Platform) {
  return "$BinaryCache\$($Platform.Triple)\cmark-gfm-0.29.0.gfm.13"
}

function Build-CMark([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\cmark `
    -Bin (Get-CMarkBinaryCache $Platform) `
    -InstallTo "$(Get-InstallDir $Platform)\Toolchains\$ProductVersion+Asserts\usr" `
    -Platform $Platform `
    -UseMSVCCompilers C,CXX `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      BUILD_TESTING = "NO";
      CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_SKIP = "YES";
    }
}

function Build-BuildTools([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\llvm-project\llvm `
    -Bin (Get-ProjectBinaryCache $Platform BuildTools) `
    -Platform $Platform `
    -UseMSVCCompilers ASM_MASM,C,CXX `
    -BuildTargets llvm-tblgen,clang-tblgen,clang-pseudo-gen,clang-tidy-confusable-chars-gen,lldb-tblgen,llvm-config,swift-def-to-strings-converter,swift-serialize-diagnostics,swift-compatibility-symbols `
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

function Write-PList {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Mandatory = $true)]
    [PSCustomObject] $Settings,
    [Parameter(Mandatory = $true)]
    [string] $Path
  )

  Invoke-Program "$(Get-PythonExecutable)" -c "import plistlib; print(str(plistlib.dumps($(($Settings | ConvertTo-JSON -Compress) -replace '"', "'")), encoding='utf-8'))" `
      -OutFile $Path
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

function Get-CompilersDefines([Hashtable] $Platform, [string] $Variant, [switch] $Test) {
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
      SWIFT_BUILD_REMOTE_MIRROR = "NO";
      SWIFT_NATIVE_SWIFT_TOOLS_PATH = $BuildTools;
    }
  }

  # If DebugInfo is enabled limit the number of parallel links to avoid OOM.
  $DebugDefines = if ($DebugInfo) { @{ SWIFT_PARALLEL_LINK_JOBS = "4"; } } else { @{} }

  # In the latest versions of VS, STL typically requires a newer version of
  # Clang than released Swift toolchains include. Relax this requirement when
  # bootstrapping with an older toolchain. Note developer builds are (currently)
  # up-to-date.
  $SwiftFlags = @();
  if ([System.Version](Get-PinnedToolchainVersion) -ne [System.Version]"0.0.0") {
    $SwiftFlags += @("-Xcc", "-D_ALLOW_COMPILER_AND_STL_VERSION_MISMATCH");
  }

  return $TestDefines + $DebugDefines + @{
    CLANG_TABLEGEN = (Join-Path -Path $BuildTools -ChildPath "clang-tblgen.exe");
    CLANG_TIDY_CONFUSABLE_CHARS_GEN = (Join-Path -Path $BuildTools -ChildPath "clang-tidy-confusable-chars-gen.exe");
    CMAKE_Swift_FLAGS = $SwiftFlags;
    LibXml2_DIR = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake\libxml2-2.11.5";
    LLDB_LIBXML2_VERSION = "2.11.5";
    LLDB_PYTHON_EXE_RELATIVE_PATH = "python.exe";
    LLDB_PYTHON_EXT_SUFFIX = ".pyd";
    LLDB_PYTHON_RELATIVE_PATH = "lib/site-packages";
    LLDB_TABLEGEN = (Join-Path -Path $BuildTools -ChildPath "lldb-tblgen.exe");
    LLDB_TEST_MAKE = "$BinaryCache\GnuWin32Make-4.4.1\bin\make.exe";
    LLVM_CONFIG_PATH = (Join-Path -Path $BuildTools -ChildPath "llvm-config.exe");
    LLVM_ENABLE_ASSERTIONS = $(if ($Variant -eq "Asserts") { "YES" } else { "NO" })
    LLVM_EXTERNAL_SWIFT_SOURCE_DIR = "$SourceCache\swift";
    LLVM_HOST_TRIPLE = $Platform.Triple;
    LLVM_NATIVE_TOOL_DIR = $BuildTools;
    LLVM_TABLEGEN = (Join-Path $BuildTools -ChildPath "llvm-tblgen.exe");
    LLVM_USE_HOST_TOOLS = "NO";
    Python3_EXECUTABLE = (Get-PythonExecutable);
    Python3_INCLUDE_DIR = "$PythonRoot\include";
    Python3_LIBRARY = "$PythonRoot\libs\$PythonLibName.lib";
    Python3_ROOT_DIR = $PythonRoot;
    SWIFT_TOOLCHAIN_VERSION = "${ToolchainIdentifier}";
    SWIFT_BUILD_SWIFT_SYNTAX = "YES";
    SWIFT_CLANG_LOCATION = (Get-PinnedToolchainToolsDir);
    SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY = "YES";
    SWIFT_ENABLE_EXPERIMENTAL_CXX_INTEROP = "YES";
    SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING = "YES";
    SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED = "YES";
    SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION = "YES";
    SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING = "YES";
    SWIFT_ENABLE_SYNCHRONIZATION = "YES";
    SWIFT_ENABLE_VOLATILE = "YES";
    SWIFT_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
    SWIFT_PATH_TO_STRING_PROCESSING_SOURCE = "$SourceCache\swift-experimental-string-processing";
    SWIFT_PATH_TO_SWIFT_SDK = (Get-PinnedToolchainSDK);
    SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE = "$SourceCache\swift-syntax";
    SWIFT_STDLIB_ASSERTIONS = "NO";
    SWIFTSYNTAX_ENABLE_ASSERTIONS = "NO";
    "cmark-gfm_DIR" = "$($Platform.ToolchainInstallRoot)\usr\lib\cmake";
  }
}

function Build-Compilers([Hashtable] $Platform, [string] $Variant) {
  New-Item -ItemType SymbolicLink -Path "$BinaryCache\$($HostPlatform.Triple)\compilers" -Target "$BinaryCache\5" -ErrorAction Ignore
  Build-CMakeProject `
    -Src $SourceCache\llvm-project\llvm `
    -Bin (Get-ProjectBinaryCache $Platform Compilers) `
    -InstallTo "$(Get-InstallDir $Platform)\Toolchains\$ProductVersion+$Variant\usr" `
    -Platform $Platform `
    -UseMSVCCompilers C,CXX `
    -UsePinnedCompilers Swift `
    -SwiftSDK (Get-PinnedToolchainSDK) `
    -BuildTargets @("install-distribution") `
    -CacheScript $SourceCache\swift\cmake\caches\Windows-$($Platform.Architecture.LLVMName).cmake `
    -Defines (Get-CompilersDefines $Platform $Variant)

  $Settings = @{
    FallbackLibrarySearchPaths = @("usr/bin")
    Identifier = "${ToolchainIdentifier}"
    Version = "${ProductVersion}"
  }
  Write-PList -Settings $Settings -Path "$($Platform.ToolchainInstallRoot)\ToolchainInfo.plist"
}

function Test-Compilers([Hashtable] $Platform, [string] $Variant, [switch] $TestClang, [switch] $TestLLD, [switch] $TestLLDB, [switch] $TestLLVM, [switch] $TestSwift) {
  Invoke-IsolatingEnvVars {
    $env:Path = "$(Get-CMarkBinaryCache $Platform)\src;$(Get-ProjectBinaryCache $BuildPlatform Compilers)\tools\swift\libdispatch-windows-$($Platform.Architecture.LLVMName)-prefix\bin;$(Get-ProjectBinaryCache $BuildPlatform Compilers)\bin;$env:Path;$VSInstallRoot\DIA SDK\bin\$($HostPlatform.Architecture.VSName);$UnixToolsBinDir"
    $TestingDefines = Get-CompilersDefines $Platform $Variant -Test
    if ($TestLLVM) { $Targets += @("check-llvm") }
    if ($TestClang) { $Targets += @("check-clang") }
    if ($TestLLD) { $Targets += @("check-lld") }
    if ($TestSwift) { $Targets += @("check-swift", "SwiftCompilerPlugin") }
    if ($TestLLDB) {
      $Targets += @("check-lldb")

      # Override test filter for known issues in downstream LLDB
      Load-LitTestOverrides $PSScriptRoot/windows-llvm-lit-test-overrides.txt

      # Transitive dependency of _lldb.pyd
      $RuntimeBinaryCache = Get-ProjectBinaryCache $BuildPlatform Runtime
      Copy-Item `
        -Path $RuntimeBinaryCache\bin\swiftCore.dll `
        -Destination "$(Get-ProjectBinaryCache $BuildPlatform Compilers)\lib\site-packages\lldb"

      # Runtime dependencies of repl_swift.exe
      $SwiftRTSubdir = "lib\swift\windows"
      Write-Host "Copying '$RuntimeBinaryCache\$SwiftRTSubdir\$($Platform.Architecture.LLVMName)\swiftrt.obj' to '$(Get-ProjectBinaryCache $BuildPlatform Compilers)\$SwiftRTSubdir'"
      Copy-Item `
        -Path "$RuntimeBinaryCache\$SwiftRTSubdir\$($Platform.Architecture.LLVMName)\swiftrt.obj" `
        -Destination "$(Get-ProjectBinaryCache $BuildPlatform Compilers)\$SwiftRTSubdir"
      Write-Host "Copying '$RuntimeBinaryCache\bin\swiftCore.dll' to '$(Get-ProjectBinaryCache $BuildPlatform Compilers)\bin'"
      Copy-Item `
        -Path "$RuntimeBinaryCache\bin\swiftCore.dll" `
        -Destination "$(Get-ProjectBinaryCache $BuildPlatform Compilers)\bin"

      $TestingDefines += @{
        LLDB_INCLUDE_TESTS = "YES";
        # Check for required Python modules in CMake
        LLDB_ENFORCE_STRICT_TEST_REQUIREMENTS = "YES";
        # No watchpoint support on windows: https://github.com/llvm/llvm-project/issues/24820
        LLDB_TEST_USER_ARGS = "--skip-category=watchpoint";
        # gtest sharding breaks llvm-lit's --xfail and LIT_XFAIL inputs: https://github.com/llvm/llvm-project/issues/102264
        LLVM_LIT_ARGS = "-v --no-gtest-sharding --time-tests";
        # LLDB Unit tests link against this library
        LLVM_UNITTEST_LINK_FLAGS = "$(Get-SwiftSDK Windows)\usr\lib\swift\windows\$($Platform.Architecture.LLVMName)\swiftCore.lib";
      }
    }

    if (-not $Targets) {
      Write-Warning "Test-Compilers invoked without specifying test target(s)."
    }

    Build-CMakeProject `
      -Src $SourceCache\llvm-project\llvm `
      -Bin $(Get-ProjectBinaryCache $Platform Compilers) `
      -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
      -Platform $Platform `
      -UseMSVCCompilers C,CXX `
      -UsePinnedCompilers Swift `
      -SwiftSDK (Get-PinnedToolchainSDK) `
      -BuildTargets $Targets `
      -CacheScript $SourceCache\swift\cmake\caches\Windows-$($Platform.Architecture.LLVMName).cmake `
      -Defines $TestingDefines
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

  # TODO: migrate to the CMake build
  $MSBuildArgs = @()
  $MSBuildArgs += "-noLogo"
  $MSBuildArgs += "-maxCpuCount"

  $Properties = @{}
  Add-KeyValueIfNew $Properties Configuration Release
  Add-KeyValueIfNew $Properties OutDir "$BinaryCache\$($Platform.Triple)\mimalloc\bin\"
  Add-KeyValueIfNew $Properties Platform "$($Platform.Architecture.ShortName)"

  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $Platform
    # Avoid hard-coding the VC tools version number
    $VCRedistDir = (Get-ChildItem "${env:VCToolsRedistDir}\$($HostPlatform.Architecture.ShortName)" -Filter "Microsoft.VC*.CRT").FullName
    if ($VCRedistDir) {
      Add-KeyValueIfNew $Properties VCRedistDir "$VCRedistDir\"
    }
  }

  foreach ($Property in $Properties.GetEnumerator()) {
    if ($Property.Value.Contains(" ")) {
      $MSBuildArgs += "-p:$($Property.Key)=$($Property.Value.Replace('\', '\\'))"
    } else {
      $MSBuildArgs += "-p:$($Property.Key)=$($Property.Value)"
    }
  }

  Invoke-Program $msbuild "$SourceCache\mimalloc\ide\vs2022\mimalloc-lib.vcxproj" @MSBuildArgs "-p:IntDir=$BinaryCache\$($Platform.Triple)\mimalloc\mimalloc\"
  Invoke-Program $msbuild "$SourceCache\mimalloc\ide\vs2022\mimalloc-override-dll.vcxproj" @MSBuildArgs "-p:IntDir=$BinaryCache\$($Platform.Triple)\mimalloc\mimalloc-override-dll\"

  $HostSuffix = if ($Platform -eq $KnownPlatforms["WindowsX64"]) { "" } else { "-arm64" }
  $BuildSuffix = if ($BuildPlatform -eq $KnownPlatforms["WindowsX64"]) { "" } else { "-arm64" }

  foreach ($item in "mimalloc.dll", "mimalloc-redirect$HostSuffix.dll") {
    Copy-Item `
      -Path "$BinaryCache\$($Platform.Triple)\mimalloc\bin\$item" `
      -Destination "$($Platform.ToolchainInstallRoot)\usr\bin\"
  }

  # TODO: should we split this out into its own function?
  $Tools = @(
    "swift.exe",
    "swiftc.exe",
    "swift-driver.exe",
    "swift-frontend.exe",
    "clang.exe",
    "clang++.exe",
    "clang-cl.exe",
    "lld.exe",
    "lld-link.exe",
    "ld.lld.exe",
    "ld64.lld.exe"
  )
  $Binaries = $Tools | ForEach-Object {[IO.Path]::Combine($Platform.ToolchainInstallRoot, "usr", "bin", $_)}
  if ($IncludeNoAsserts) {
    $NoAssertBinaries = $Tools | ForEach-Object {[IO.Path]::Combine($Platform.NoAssertsToolchainInstallRoot, "usr", "bin", $_)}
    $Binaries = $Binaries + $NoAssertBinaries
  }
  foreach ($Binary in $Binaries) {
    $Name = [IO.Path]::GetFileName($Binary)
    # Binary-patch in place
    Invoke-Program "$SourceCache\mimalloc\bin\minject$BuildSuffix" "-f" "-i" "$Binary"
    # Log the import table
    $LogFile = "$BinaryCache\$($Platform.Triple)\mimalloc\minject-log-$Name.txt"
    $ErrorFile = "$BinaryCache\$($Platform.Triple)\mimalloc\minject-log-$Name-error.txt"
    Invoke-Program "$SourceCache\mimalloc\bin\minject$BuildSuffix" "-l" "$Binary" -OutFile $LogFile -ErrorFile $ErrorFile
    # Verify patching
    $Found = Select-String -Path $LogFile -Pattern "mimalloc"
    if (-not $Found) {
      Get-Content $ErrorFile
      throw "Failed to patch mimalloc for $Name"
    }
  }
}

function Build-LLVM([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\llvm-project\llvm `
    -Bin (Get-ProjectBinaryCache $Platform LLVM) `
    -Platform $Platform `
    -UseBuiltCompilers C,CXX `
    -Defines @{
      LLVM_HOST_TRIPLE = $Platform.Triple;
    }
}

function Build-Sanitizers([Hashtable] $Platform) {
  $LLVMTargetCache = $(Get-ProjectBinaryCache $Platform LLVM)
  $LITVersionStr = $(Invoke-Program $(Get-PythonExecutable) "$LLVMTargetCache\bin\llvm-lit.py" --version)
  if (-not $ToBatch -and -not ($LITVersionStr -match "lit (\d+)\.\d+\.\d+.*")) {
    throw "Unexpected version string '$LITVersionStr' output from llvm-lit.py"
  }
  $LLVMVersionMajor = $Matches.1
  $InstallTo = "$($HostPlatform.ToolchainInstallRoot)\usr\lib\clang\$LLVMVersionMajor"
  Write-Host "Sanitizers SDK directory: $InstallTo"

  Build-CMakeProject `
    -Src $SourceCache\llvm-project\compiler-rt\lib\builtins `
    -Bin "$(Get-ProjectBinaryCache $Platform ClangBuiltins)" `
    -InstallTo $InstallTo `
    -Platform $Platform `
    -UseBuiltCompilers ASM,C,CXX `
    -BuildTargets "install-compiler-rt" `
    -Defines (@{
      LLVM_DIR = "$LLVMTargetCache\lib\cmake\llvm";
      LLVM_ENABLE_PER_TARGET_RUNTIME_DIR = "YES";
      COMPILER_RT_DEFAULT_TARGET_ONLY = "YES";
    })

  Build-CMakeProject `
    -Src $SourceCache\llvm-project\compiler-rt `
    -Bin "$(Get-ProjectBinaryCache $Platform ClangRuntime)" `
    -InstallTo $InstallTo `
    -Platform $Platform `
    -UseBuiltCompilers ASM,C,CXX `
    -BuildTargets "install-compiler-rt" `
    -Defines @{
      LLVM_DIR = "$LLVMTargetCache\lib\cmake\llvm";
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

function Build-ZLib([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\zlib `
    -Bin "$BinaryCache\$($Platform.Triple)\zlib" `
    -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
    -Platform $Platform `
    -UseMSVCCompilers C `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_POSITION_INDEPENDENT_CODE = "YES";
    }
}

function Build-XML2([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\libxml2 `
    -Bin "$BinaryCache\$($Platform.Triple)\libxml2-2.11.5" `
    -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
    -Platform $Platform `
    -UseMSVCCompilers C,CXX `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_POSITION_INDEPENDENT_CODE = "YES";
      LIBXML2_WITH_ICONV = "NO";
      LIBXML2_WITH_ICU = "NO";
      LIBXML2_WITH_LZMA = "NO";
      LIBXML2_WITH_PYTHON = "NO";
      LIBXML2_WITH_TESTS = "NO";
      LIBXML2_WITH_THREADS = "YES";
      LIBXML2_WITH_ZLIB = "NO";
    }
}

function Build-RegsGen2([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\ds2\Tools\RegsGen2 `
    -Bin (Get-ProjectBinaryCache $Platform RegsGen2) `
    -Platform $Platform `
    -BuildTargets default `
    -UseMSVCCompilers C,CXX `
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
    -Defines @{
      DS2_REGSGEN2 = "$(Get-ProjectBinaryCache $BuildPlatform RegsGen2)/regsgen2.exe";
      DS2_PROGRAM_PREFIX = "$(Get-ModuleTriple $Platform)-";
      BISON_EXECUTABLE = "$(Get-BisonExecutable)";
      FLEX_EXECUTABLE = "$(Get-FlexExecutable)";
    }
}

function Build-CURL([Hashtable] $Platform) {
  $PlatformDefines = @{}
  if ($Platform.OS -eq [OS]::Android) {
    $PlatformDefines += @{
      HAVE_FSEEKO = "0";
    }
  }

  Build-CMakeProject `
    -Src $SourceCache\curl `
    -Bin "$BinaryCache\$($Platform.Triple)\curl" `
    -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
    -Platform $Platform `
    -UseMSVCCompilers C `
    -Defines ($PlatformDefines + @{
      BUILD_SHARED_LIBS = "NO";
      BUILD_TESTING = "NO";
      CMAKE_POSITION_INDEPENDENT_CODE = "YES";
      BUILD_CURL_EXE = "NO";
      BUILD_LIBCURL_DOCS = "NO";
      BUILD_MISC_DOCS = "NO";
      CURL_CA_BUNDLE = "none";
      CURL_CA_FALLBACK = "NO";
      CURL_CA_PATH = "none";
      CURL_BROTLI = "NO";
      CURL_DISABLE_ALTSVC = "NO";
      CURL_DISABLE_AWS = "YES";
      CURL_DISABLE_BASIC_AUTH = "NO";
      CURL_DISABLE_BEARER_AUTH = "NO";
      CURL_DISABLE_BINDLOCAL = "NO";
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
      CURL_DISABLE_KERBEROS_AUTH = "NO";
      CURL_DISABLE_LDAP = "YES";
      CURL_DISABLE_LDAPS = "YES";
      CURL_DISABLE_MIME = "NO";
      CURL_DISABLE_MQTT = "YES";
      CURL_DISABLE_NEGOTIATE_AUTH = "NO";
      CURL_DISABLE_NETRC = "NO";
      CURL_DISABLE_NTLM = "NO";
      CURL_DISABLE_PARSEDATE = "NO";
      CURL_DISABLE_POP3 = "YES";
      CURL_DISABLE_PROGRESS_METER = "YES";
      CURL_DISABLE_PROXY = "NO";
      CURL_DISABLE_RTSP = "YES";
      CURL_DISABLE_SHUFFLE_DNS = "YES";
      CURL_DISABLE_SMB = "YES";
      CURL_DISABLE_SMTP = "YES";
      CURL_DISABLE_SOCKETPAIR = "YES";
      CURL_DISABLE_SRP = "NO";
      CURL_DISABLE_TELNET = "YES";
      CURL_DISABLE_TFTP = "YES";
      CURL_DISABLE_VERBOSE_STRINGS = "NO";
      CURL_LTO = "NO";
      CURL_USE_BEARSSL = "NO";
      CURL_USE_GNUTLS = "NO";
      CURL_USE_GSSAPI = "NO";
      CURL_USE_LIBPSL = "NO";
      CURL_USE_LIBSSH = "NO";
      CURL_USE_LIBSSH2 = "NO";
      CURL_USE_MBEDTLS = "NO";
      CURL_USE_OPENSSL = "NO";
      CURL_USE_SCHANNEL = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
      CURL_USE_WOLFSSL = "NO";
      CURL_WINDOWS_SSPI = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
      CURL_ZLIB = "YES";
      CURL_ZSTD = "NO";
      ENABLE_ARES = "NO";
      ENABLE_CURLDEBUG = "NO";
      ENABLE_CURL_MANUAL = "NO";
      ENABLE_DEBUG = "NO";
      ENABLE_IPV6 = "YES";
      ENABLE_THREADED_RESOLVER = "NO";
      ENABLE_UNICODE = "YES";
      ENABLE_UNIX_SOCKETS = "NO";
      ENABLE_WEBSOCKETS = "YES";
      HAVE_POLL_FINE = "NO";
      USE_ECH = "NO";
      USE_HTTPSRR = "NO";
      USE_IDN2 = "NO";
      USE_MSH3 = "NO";
      USE_NGHTTP2 = "NO";
      USE_NGTCP2 = "NO";
      USE_QUICHE = "NO";
      USE_OPENSSL_QUIC = "NO";
      USE_WIN32_IDN = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
      USE_WIN32_LARGE_FILES = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
      USE_WIN32_LDAP = "NO";
      ZLIB_ROOT = "$BinaryCache\$($Platform.Triple)\usr";
      ZLIB_LIBRARY = "$BinaryCache\$($Platform.Triple)\usr\lib\zlibstatic.lib";
    })
}

function Build-Runtime([Hashtable] $Platform) {
  $PlatformDefines = @{}
  if ($Platform.OS -eq [OS]::Android) {
    $PlatformDefines += @{
      LLVM_ENABLE_LIBCXX = "YES";
      SWIFT_USE_LINKER = "lld";
    }

    if ((Get-AndroidNDK).ClangVersion -lt 18) {
      $PlatformDefines += @{
        SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT = "YES";
      }
    }
  }

  Build-CMakeProject `
    -Src $SourceCache\swift `
    -Bin (Get-ProjectBinaryCache $Platform Runtime) `
    -InstallTo "$(Get-SwiftSDK $Platform.OS)\usr" `
    -Platform $Platform `
    -UseBuiltCompilers C,CXX,Swift `
    -SwiftSDK $null `
    -CacheScript $SourceCache\swift\cmake\caches\Runtime-$($Platform.OS.ToString())-$($Platform.Architecture.LLVMName).cmake `
    -Defines ($PlatformDefines + @{
      LLVM_DIR = "$(Get-ProjectBinaryCache $Platform LLVM)\lib\cmake\llvm";
      SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_CXX_INTEROP = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING = "YES";
      SWIFT_ENABLE_SYNCHRONIZATION = "YES";
      SWIFT_ENABLE_VOLATILE = "YES";
      SWIFT_NATIVE_SWIFT_TOOLS_PATH = ([IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin"));
      SWIFT_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
      SWIFT_PATH_TO_STRING_PROCESSING_SOURCE = "$SourceCache\swift-experimental-string-processing";
    })
}

function Test-Runtime([Hashtable] $Platform) {
  if ($IsCrossCompiling) {
    throw "Swift runtime tests are not supported when cross-compiling"
  }
  if (-not (Test-Path (Get-ProjectBinaryCache $Platform Runtime))) {
    throw "Swift runtime tests are supposed to reconfigure the existing build"
  }
  $CompilersBinaryCache = Get-ProjectBinaryCache $BuildPlatform Compilers
  if (-not (Test-Path "$CompilersBinaryCache\bin\FileCheck.exe")) {
    # These will exist if we test any of llvm/clang/lldb/lld/swift as well
    throw "LIT test utilities not found in $CompilersBinaryCache\bin"
  }

  Invoke-IsolatingEnvVars {
    # Filter known issues when testing on Windows
    Load-LitTestOverrides $PSScriptRoot/windows-swift-android-lit-test-overrides.txt
    $env:Path = "$(Get-CMarkBinaryCache $Platform)\src;$(Get-PinnedToolchainRuntime);${env:Path};$UnixToolsBinDir"
    Build-CMakeProject `
      -Src $SourceCache\swift `
      -Bin (Get-ProjectBinaryCache $Platform Runtime) `
      -Platform $Platform `
      -UseBuiltCompilers C,CXX,Swift `
      -SwiftSDK $null `
      -BuildTargets check-swift-validation-only_non_executable `
      -Defines @{
        SWIFT_INCLUDE_TESTS = "YES";
        SWIFT_INCLUDE_TEST_BINARIES = "YES";
        SWIFT_BUILD_TEST_SUPPORT_MODULES = "YES";
        SWIFT_NATIVE_LLVM_TOOLS_PATH = Join-Path -Path $CompilersBinaryCache -ChildPath "bin";
        LLVM_LIT_ARGS = "-vv";
      }
  }
}

function Build-ExperimentalRuntime([Hashtable] $Platform, [switch] $Static = $false) {
  # TODO: remove this once the migration is completed.
  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $BuildPlatform

    Push-Location "${SourceCache}\swift\Runtimes"
    Start-Process -Wait -WindowStyle Hidden -FilePath $cmake -ArgumentList @("-P", "Resync.cmake")
    Pop-Location
  }

  Invoke-IsolatingEnvVars {
    $env:Path = "$(Get-CMarkBinaryCache $Platform)\src;$(Get-PinnedToolchainRuntime);${env:Path}"

    $SDKRoot = Get-SwiftSDK $Platform.OS -Identifier "$($Platform.OS)Experimental"

    $RuntimeBinaryCache = if ($Static) {
      Get-ProjectBinaryCache $Platform ExperimentalStaticRuntime
    } else {
      throw "dynamic Experimental Runtime is not yet implemented"
    }

    $OverlayBinaryCache = if ($Static) {
      Get-ProjectBinaryCache $Platform ExperimentalStaticOverlay
    } else {
      throw "dynamic Experimental Runtime is not yet implemented"
    }

    $StringProcessingBinaryCache = if ($Static) {
      Get-ProjectBinarycache $Platform ExperimentalStaticStringProcessing
    } else {
      throw "dynamic Experimental Runtime is not yet implemented"
    }

    $SynchronizationBinaryCache = if ($Static) {
      Get-ProjectBinarycache $Platform ExperimentalStaticSynchronization
    } else {
      throw "dynamic Experimental Runtime is not yet implemented"
    }

    $DistributedBinaryCache = if ($Static) {
      Get-ProjectBinarycache $Platform ExperimentalStaticDistributed
    } else {
      throw "dynamic Experimental Runtime is not yet implemented"
    }

    $ObservationBinaryCache = if ($Static) {
      Get-ProjectBinarycache $Platform ExperimentalStaticObservation
    } else {
      throw "dynamic Experimental Runtime is not yet implemented"
    }

    $DifferentiationBinaryCache = if ($Static) {
      Get-ProjectBinarycache $Platform ExperimentalStaticDifferentiation
    } else {
      throw "dynamic Experimental Differentiation is not yet implemented"
    }

    $VolatileBinaryCache = if ($Static) {
      Get-ProjectBinarycache $Platform ExperimentalStaticVolatile
    } else {
      throw "dynamic Experimental Volatile is not yet implemented"
    }

    Build-CMakeProject `
      -Src $SourceCache\swift\Runtimes\Core `
      -Bin $RuntimeBinaryCache `
      -InstallTo "${SDKRoot}\usr" `
      -Platform $Platform `
      -UseBuiltCompilers C,CXX,Swift `
      -SwiftSDK $null `
      -UseGNUDriver `
      -Defines @{
        BUILD_SHARED_LIBS = if ($Static) { "NO" } else { "YES" };
        # TODO(compnerd) enforce dynamic linking of BlocksRuntime and dispatch.
        CMAKE_CXX_FLAGS = $(if ($Static) { @("-Ddispatch_STATIC") } else { @() });
        CMAKE_Swift_FLAGS = $(if ($Static) { @("-Xcc", "-static-libclosure") } else { @() });
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

        # NOTE(compnerd) we can get away with this currently because we only
        # use the C portion of the dispatch build, which is supposed to always
        # be built dynamically. Currently, we do not do this due to limitations
        # of the build system, but because we are building statically, we do
        # not link against the runtime and can get away with it.
        dispatch_DIR = (Get-ProjectCMakeModules $Platform Dispatch);
        SwiftCore_ENABLE_CONCURRENCY = "YES";
      }

    Build-CMakeProject `
      -Src $SourceCache\swift\Runtimes\Overlay `
      -Bin $OverlayBinaryCache `
      -InstallTo "${SDKRoot}\usr" `
      -Platform $Platform `
      -UseBuiltCompilers C,CXX,Swift `
      -SwiftSDK $null `
      -UseGNUDriver `
      -Defines @{
        BUILD_SHARED_LIBS = if ($Static) { "NO" } else { "YES" };
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

        SwiftCore_DIR = "${RuntimeBinaryCache}\cmake\SwiftCore";
      }

    Build-CMakeProject `
      -Src $SourceCache\swift\Runtimes\Supplemental\StringProcessing `
      -Bin $StringProcessingBinaryCache `
      -InstallTo "${SDKRoot}\usr" `
      -Platform $Platform `
      -UseBuiltCompilers C,Swift `
      -SwiftSDK $null `
      -UseGNUDriver `
      -Defines @{
        BUILD_SHARED_LIBS = if ($Static) { "NO" } else { "YES" };
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

        SwiftCore_DIR = "${RuntimeBinaryCache}\cmake\SwiftCore";
      }

    Build-CMakeProject `
      -Src $SourceCache\swift\Runtimes\Supplemental\Synchronization `
      -Bin $SynchronizationBinaryCache `
      -InstallTo "${SDKRoot}\usr" `
      -Platform $Platform `
      -UseBuiltCompilers C,Swift `
      -SwiftSDK $null `
      -UseGNUDriver `
      -Defines @{
        BUILD_SHARED_LIBS = if ($Static) { "NO" } else { "YES" };
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

        SwiftCore_DIR = "${RuntimeBinaryCache}\cmake\SwiftCore";
        SwiftOverlay_DIR = "${OverlayBinaryCache}\cmake\SwiftOverlay";
      }

    Build-CMakeProject `
      -Src $SourceCache\swift\Runtimes\Supplemental\Distributed `
      -Bin $DistributedBinaryCache `
      -InstallTo "${SDKRoot}\usr" `
      -Platform $Platform `
      -UseBuiltCompilers C,CXX,Swift `
      -SwiftSDK $null `
      -UseGNUDriver `
      -Defines @{
        BUILD_SHARED_LIBS = if ($Static) { "NO" } else { "YES" };
        # FIXME(#83449): avoid using `SwiftCMakeConfig.h`
        CMAKE_CXX_FLAGS = @("-I${RuntimeBinaryCache}\include");
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

        SwiftCore_DIR = "${RuntimeBinaryCache}\cmake\SwiftCore";
        SwiftOverlay_DIR = "${OverlayBinaryCache}\cmake\SwiftOverlay";
      }

    Build-CMakeProject `
      -Src $SourceCache\swift\Runtimes\Supplemental\Observation `
      -Bin $ObservationBinaryCache `
      -InstallTo "${SDKRoot}\usr" `
      -Platform $Platform `
      -UseBuiltCompilers CXX,Swift `
      -SwiftSDK $null `
      -UseGNUDriver `
      -Defines @{
        BUILD_SHARED_LIBS = if ($Static) { "NO" } else { "YES" };
        # FIXME(#83449): avoid using `SwiftCMakeConfig.h`
        CMAKE_CXX_FLAGS = @("-I${RuntimeBinaryCache}\include");
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

        SwiftCore_DIR = "${RuntimeBinaryCache}\cmake\SwiftCore";
        SwiftOverlay_DIR = "${OverlayBinaryCache}\cmake\SwiftOverlay";
      }

    Build-CMakeProject `
      -Src $SourceCache\swift\Runtimes\Supplemental\Differentiation `
      -Bin $DifferentiationBinaryCache `
      -InstallTo "${SDKRoot}\usr" `
      -Platform $Platform `
      -UseBuiltCompilers C,CXX,Swift `
      -SwiftSDK $null `
      -UseGNUDriver `
      -Defines @{
        BUILD_SHARED_LIBS = if ($Static) { "NO" } else { "YES" };
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

        SwiftCore_DIR = "${RuntimeBinaryCache}\cmake\SwiftCore";
        SwiftOverlay_DIR = "${OverlayBinaryCache}\cmake\SwiftOverlay";
      }

    Build-CMakeProject `
      -Src $SourceCache\swift\Runtimes\Supplemental\Volatile `
      -Bin $VolatileBinaryCache `
      -InstallTo "${SDKROOT}\usr" `
      -Platform $Platform `
      -UseBuiltCompilers C,Swift `
      -SwiftSDK $null `
      -UseGNUDriver `
      -Defines @{
        BUILD_SHARED_LIBS = if ($Static) { "NO" } else { "YES" };
        CMAKE_FIND_PACKAGE_PREFER_CONFIG = "YES";
        CMAKE_Swift_COMPILER_TARGET = (Get-ModuleTriple $Platform);
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

        SwiftCore_DIR = "${RuntimeBinaryCache}\cmake\SwiftCore";
        SwiftOverlay_DIR = "${OverlayBinaryCache}\cmake\SwiftOverlay";
        # FIXME(compnerd) this currently causes a build failure on Windows, but
        # this should be enabled when building the dynamic runtime.
        SwiftVolatile_ENABLE_LIBRARY_EVOLUTION = "NO";
      }
  }
}

function Write-SDKSettings([OS] $OS, [string] $Identifier = $OS.ToString()) {
  $SDKSettings = @{
    CanonicalName = $OS.ToString().ToLowerInvariant()
    DisplayName = $OS.ToString()
    IsBaseSDK = "YES"
    Version = "${ProductVersion}"
    VersionMap = @{}
    HeaderSearchPaths = @( "usr/include" );
    LibrarySearchPaths = @();
    Toolchains = @( "${ToolchainIdentifier}" );
    DefaultProperties = @{
      PLATFORM_NAME = $OS.ToString().ToLowerInvariant()
      DEFAULT_COMPILER = "${ToolchainIdentifier}"
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
      $SDKSettings.SupportedTargets.windows.Archs = $WindowsSDKPlatforms | ForEach-Object { $_.Architecture.LLVMName } | Sort-Object
    }
    Android {
      $SDKSettings.SupportedTargets.android.LLVMTargetVendor = "unknown"
      $SDKSettings.SupportedTargets.android.LLVMTargetSys = "linux"
      $SDKSettings.SupportedTargets.android.LLVMTargetTripleEnvironment = "android${AndroidAPILevel}"
      $SDKSettings.SupportedTargets.android.Archs = $AndroidSDKPlatforms | ForEach-Object { $_.Architecture.LLVMName } | Sort-Object
    }
  }
  $SDKSettings | ConvertTo-JSON -Depth 4 | Out-FIle -FilePath "$(Get-SwiftSDK $OS -Identifier $Identifier)\SDKSettings.json"
  Write-PList -Settings $SDKSettings -Path "$(Get-SwiftSDK $OS -Identifier $Identifier)\SDKSettings.plist"
}

function Build-Dispatch([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-corelibs-libdispatch `
    -Bin (Get-ProjectBinaryCache $Platform Dispatch) `
    -InstallTo "$(Get-SwiftSDK $Platform.OS)\usr" `
    -Platform $Platform `
    -UseBuiltCompilers C,CXX,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      ENABLE_SWIFT = "YES";
    }
}

function Test-Dispatch {
  Invoke-IsolatingEnvVars {
    $env:CTEST_OUTPUT_ON_FAILURE = "YES"

    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-libdispatch `
      -Bin (Get-ProjectBinaryCache $BuildPlatform Dispatch) `
      -Platform $BuildPlatform `
      -UseBuiltCompilers C,CXX,Swift `
      -SwiftSDK (Get-SwiftSDK $BuildPlatform.OS) `
      -BuildTargets default,ExperimentalTest `
      -Defines @{
        ENABLE_SWIFT = "YES";
      }
  }
}

function Build-Foundation {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position = 0, Mandatory = $true)]
    [Hashtable] $Platform,
    [switch] $Static = $false
  )

  $FoundationBinaryCache = if ($Static) {
    Get-ProjectBinaryCache $Platform ExperimentalStaticFoundation
  } else {
    Get-ProjectBinaryCache $Platform DynamicFoundation
  }

  $FoundationImage = if ($Static) {
    "$(Get-SwiftSDK $Platform.OS -Identifier "$($Platform.OS)Experimental")\usr"
  } else {
    "$(Get-SwiftSDK $Platform.OS)\usr"
  }

  $SwiftSDK = if ($Static) {
    Get-SwiftSDK $Platform.OS -Identifier "$($Platform.OS)Experimental"
  } else {
    Get-SwiftSDK $Platform.OS
  }

  $SwiftFlags = if ($Static) {
    @("-static-stdlib", "-Xfrontend", "-use-static-resource-dir")
  } else {
    @()
  }

  $DispatchCMakeModules = if ($Static) {
    Get-ProjectCMakeModules $Platform ExperimentalStaticDispatch
  } else {
    Get-ProjectCMakeModules $Platform Dispatch
  }

  Build-CMakeProject `
    -Src $SourceCache\swift-corelibs-foundation `
    -Bin $FoundationBinaryCache `
    -InstallTo $FoundationImage `
    -Platform $Platform `
    -UseBuiltCompilers ASM,C,CXX,Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      BUILD_SHARED_LIBS = if ($Static) { "NO" } else { "YES" };
      CMAKE_NINJA_FORCE_RESPONSE_FILE = "YES";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      CMAKE_Swift_FLAGS = $SwiftFlags;
      ENABLE_TESTING = "NO";
      FOUNDATION_BUILD_TOOLS = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
      CURL_DIR = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake\CURL";
      LibXml2_DIR = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake\libxml2-2.11.5";
      ZLIB_LIBRARY = if ($Platform.OS -eq [OS]::Windows) {
        "$BinaryCache\$($Platform.Triple)\usr\lib\zlibstatic.lib"
      } else {
        "$BinaryCache\$($Platform.Triple)\usr\lib\libz.a"
      };
      ZLIB_INCLUDE_DIR = "$BinaryCache\$($Platform.Triple)\usr\include";
      dispatch_DIR = $DispatchCMakeModules;
      SwiftSyntax_DIR = (Get-ProjectBinaryCache $HostPlatform Compilers);
      _SwiftFoundation_SourceDIR = "$SourceCache\swift-foundation";
      _SwiftFoundationICU_SourceDIR = "$SourceCache\swift-foundation-icu";
      _SwiftCollections_SourceDIR = "$SourceCache\swift-collections";
      SwiftFoundation_MACRO = "$(Get-ProjectBinaryCache $BuildPlatform FoundationMacros)\bin"
    }
}

function Test-Foundation {
  # Foundation tests build via swiftpm rather than CMake
  Build-SPMProject `
    -Action Test `
    -Src $SourceCache\swift-foundation `
    -Bin "$BinaryCache\$($BuildPlatform.Triple)\CoreFoundationTests" `
    -Platform $BuildPlatform

  Invoke-IsolatingEnvVars {
    $env:DISPATCH_INCLUDE_PATH="$(Get-SwiftSDK $BuildPlatform.OS)/usr/include"
    $env:LIBXML_LIBRARY_PATH="$BinaryCache/$($Platform.Triple)/usr/lib"
    $env:LIBXML_INCLUDE_PATH="$BinaryCache/$($Platform.Triple)/usr/include/libxml2"
    $env:ZLIB_LIBRARY_PATH="$BinaryCache/$($Platform.Triple)/usr/lib"
    $env:CURL_LIBRARY_PATH="$BinaryCache/$($Platform.Triple)/usr/lib"
    $env:CURL_INCLUDE_PATH="$BinaryCache/$($Platform.Triple)/usr/include"
    Build-SPMProject `
      -Action Test `
      -Src $SourceCache\swift-corelibs-foundation `
      -Bin "$BinaryCache\$($BuildPlatform.Triple)\FoundationTests" `
      -Platform $BuildPlatform `
      -Configuration $FoundationTestConfiguration `
      -j 1
  }
}

function Build-FoundationMacros([Hashtable] $Platform) {
  $SwiftSyntaxDir = (Get-ProjectCMakeModules $Platform Compilers)
  if (-not (Test-Path $SwiftSyntaxDir)) {
    throw "The swift-syntax from the compiler build for $($Platform.OS) $($Platform.Architecture.ShortName) isn't available"
  }

  Build-CMakeProject `
    -Src $SourceCache\swift-foundation\Sources\FoundationMacros `
    -Bin (Get-ProjectBinaryCache $Platform FoundationMacros) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -UseBuiltCompilers Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      SwiftSyntax_DIR = $SwiftSyntaxDir;
    }
}

function Build-XCTest([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-corelibs-xctest `
    -Bin (Get-ProjectBinaryCache $Platform XCTest) `
    -InstallTo "$([IO.Path]::Combine((Get-PlatformRoot $Platform.OS), "Developer", "Library", "XCTest-$ProductVersion", "usr"))" `
    -Platform $Platform `
    -UseBuiltCompilers Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_INSTALL_BINDIR = $Platform.BinaryDir;
      ENABLE_TESTING = "NO";
      dispatch_DIR = $(Get-ProjectCMakeModules $Platform Dispatch);
      Foundation_DIR = $(Get-ProjectCMakeModules $Platform DynamicFoundation);
      XCTest_INSTALL_NESTED_SUBDIR = "YES";
    }
}

function Test-XCTest {
  Invoke-IsolatingEnvVars {
    $env:Path = "$(Get-ProjectBinaryCache $BuildPlatform XCTest);$(Get-ProjectBinaryCache $BuildPlatform DynamicFoundation)\bin;$(Get-ProjectBinaryCache $BuildPlatform Dispatch);$(Get-ProjectBinaryCache $BuildPlatform Runtime)\bin;${env:Path};$UnixToolsBinDir"

    $RuntimeBinaryCache = Get-ProjectBinaryCache $BuildPlatform Runtime
    $SwiftRuntimeDirectory = "${RuntimeBinaryCache}\lib\swift"

    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-xctest `
      -Bin (Get-ProjectBinaryCache $BuildPlatform XCTest) `
      -Platform $BuildPlatform `
      -UseBuiltCompilers C,CXX,Swift `
      -SwiftSDK $null `
      -BuildTargets default,check-xctest `
      -Defines @{
        CMAKE_Swift_FLAGS = @("-resource-dir", $SwiftRuntimeDirectory, "-vfsoverlay", "${RuntimeBinaryCache}\stdlib\windows-vfs-overlay.yaml");
        ENABLE_TESTING = "YES";
        dispatch_DIR = $(Get-ProjectCMakeModules $BuildPlatform Dispatch);
        Foundation_DIR = $(Get-ProjectCMakeModules $BuildPlatform DynamicFoundation);
        LLVM_DIR = "$(Get-ProjectBinaryCache $BuildPlatform LLVM)\lib\cmake\llvm";
        XCTEST_PATH_TO_FOUNDATION_BUILD = $(Get-ProjectBinaryCache $BuildPlatform DynamicFoundation);
        XCTEST_PATH_TO_LIBDISPATCH_BUILD = $(Get-ProjectBinaryCache $BuildPlatform Dispatch);
        XCTEST_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
      }
  }
}

function Build-Testing([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-testing `
    -Bin (Get-ProjectBinaryCache $Platform Testing) `
    -InstallTo "$([IO.Path]::Combine((Get-PlatformRoot $Platform.OS), "Developer", "Library", "Testing-$ProductVersion", "usr"))" `
    -Platform $Platform `
    -UseBuiltCompilers C,CXX,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_INSTALL_BINDIR = $Platform.BinaryDir;
      dispatch_DIR = (Get-ProjectCMakeModules $Platform Dispatch);
      Foundation_DIR = (Get-ProjectCMakeModules $Platform DynamicFoundation);
      SwiftSyntax_DIR = (Get-ProjectBinaryCache $HostPlatform Compilers);
      SwiftTesting_MACRO = "$(Get-ProjectBinaryCache $BuildPlatform TestingMacros)\TestingMacros.dll";
      SwiftTesting_INSTALL_NESTED_SUBDIR = "YES";
    }
}

function Test-Testing {
  throw "testing Testing is not supported"
}

function Write-PlatformInfoPlist([OS] $OS) {
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

# Copies files installed by CMake from the arch-specific platform root,
# where they follow the layout expected by the installer,
# to the final platform root, following the installer layout.
function Install-SDK([Hashtable[]] $Platforms, [OS] $OS = $Platforms[0].OS, [string] $Identifier = $OS.ToString()) {
  # Copy SDK header files
  foreach ($Module in ("Block", "dispatch", "os", "_foundation_unicode", "_FoundationCShims")) {
    foreach ($ResourceType in ("swift", "swift_static")) {
      $ModuleDirectory = "$(Get-SwiftSDK $OS -Identifier $Identifier)\usr\lib\$ResourceType\$Module"
      if (Test-Path $ModuleDirectory) {
        Move-Directory $ModuleDirectory "$(Get-SwiftSDK $OS -Identifier $Identifier)\usr\include\"
      }
    }
  }

  # Copy files from the arch subdirectory, including "*.swiftmodule" which need restructuring
  foreach ($Platform in $Platforms) {
    foreach ($ResourceType in ("swift", "swift_static")) {
      $PlatformResources = "$(Get-SwiftSDK $OS -Identifier $Identifier)\usr\lib\$ResourceType\$($OS.ToString().ToLowerInvariant())"
      Get-ChildItem -Recurse "$PlatformResources\$($Platform.Architecture.LLVMName)" | ForEach-Object {
        if (".swiftmodule", ".swiftdoc", ".swiftinterface" -contains $_.Extension) {
          Write-Host -BackgroundColor DarkRed -ForegroundColor White "$($_.FullName) is not in a thick module layout"
          Copy-File $_.FullName "$PlatformResources\$($_.BaseName).swiftmodule\$(Get-ModuleTriple $Platform)$($_.Extension)"
        }
      }
    }
  }
}

function Build-SDK([Hashtable] $Platform, [switch] $IncludeMacros = $false) {
  if ($IncludeDS2) {
    Invoke-BuildStep Build-DS2 $Platform
  }

  # Third Party Dependencies
  Invoke-BuildStep Build-ZLib $Platform
  Invoke-BuildStep Build-XML2 $Platform
  Invoke-BuildStep Build-CURL $Platform
  Invoke-BuildStep Build-LLVM $Platform

  # Libraries
  Invoke-BuildStep Build-Runtime $Platform
  Invoke-BuildStep Build-Dispatch $Platform
  if ($IncludeMacros) {
    Invoke-BuildStep Build-FoundationMacros $Platform
    Invoke-BuildStep Build-TestingMacros $Platform
  }
  Invoke-BuildStep Build-Foundation $Platform
  Invoke-BuildStep Build-Sanitizers $Platform
  Invoke-BuildStep Build-XCTest $Platform
  Invoke-BuildStep Build-Testing $Platform
}

function Build-ExperimentalSDK([Hashtable] $Platform) {
  # TODO(compnerd) we currently build the experimental SDK with just the static
  # variant. We should aim to build both dynamic and static variants.
  Invoke-BuildStep Build-ExperimentalRuntime $Platform -Static

  Invoke-IsolatingEnvVars {
    $env:Path = "$(Get-CMarkBinaryCache $Platform)\src;$(Get-PinnedToolchainRuntime);${env:Path}"
    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-libdispatch `
      -Bin (Get-ProjectBinaryCache $Platform ExperimentalStaticDispatch) `
      -InstallTo "$(Get-SwiftSDK $Platform.OS -Identifier "$($Platform.OS)Experimental")\usr" `
      -Platform $Platform `
      -UseBuiltCompilers C,CXX,Swift `
      -SwiftSDK (Get-SwiftSDK $Platform.OS -Identifier "$($Platform.OS)Experimental") `
      -Defines @{
        BUILD_SHARED_LIBS = "NO";
        CMAKE_Swift_FLAGS = @("-static-stdlib", "-Xfrontend", "-use-static-resource-dir");
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

        ENABLE_SWIFT = "YES";
      }
  }

  Invoke-BuildStep Build-Foundation $Platform -Static
}

function Build-SQLite([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-toolchain-sqlite `
    -Bin (Get-ProjectBinaryCache $Platform SQLite) `
    -Platform $Platform `
    -UseMSVCCompilers C `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
    }
}

function Build-System([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-system `
    -Bin (Get-ProjectBinaryCache $Platform System) `
    -Platform $Platform `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    }
}

function Build-Subprocess([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $sourceCache\swift-subprocess `
    -Bin (Get-ProjectBinaryCache $Platform Subprocess) `
    -Platform $Platform `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    }
}

function Build-Build([Hashtable] $Platform) {
  # Use lld to workaround the ARM64 LNK1322 issue: https://github.com/swiftlang/swift/issues/79740
  # FIXME(hjyamauchi) Have a real fix
  $ArchSpecificOptions = if ($Platform -eq $KnownPlatforms["WindowsARM64"]) { @{ CMAKE_Swift_FLAGS = "-use-ld=lld-link"; } } else { @{} }

  Build-CMakeProject `
    -Src $SourceCache\swift-build `
    -Bin (Get-ProjectBinaryCache $Platform Build) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -UseBuiltCompilers C,CXX,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines (@{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      LLBuild_DIR = (Get-ProjectCMakeModules $Platform LLBuild);
      SwiftDriver_DIR = (Get-ProjectCMakeModules $Platform Driver);
      SwiftSystem_DIR = (Get-ProjectCMakeModules $Platform System);
      TSC_DIR = (Get-ProjectCMakeModules $Platform ToolsSupportCore);
      SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
      SQLite3_LIBRARY = "$(Get-ProjectBinaryCache $Platform SQLite)\SQLite3.lib";
    } + $ArchSpecificOptions)
}

function Build-ToolsSupportCore([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-tools-support-core `
    -Bin (Get-ProjectBinaryCache $Platform ToolsSupportCore) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

      Foundation_DIR = $(Get-ProjectCMakeModules $Platform DynamicFoundation);
      XCTest_DIR = (Get-ProjectCMakeModules $Platform XCTest);
    }
}

function Build-LLBuild([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\llbuild `
    -Bin (Get-ProjectBinaryCache $Platform LLBuild) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -UseMSVCCompilers CXX `
    -UseBuiltCompilers Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      LLBUILD_SUPPORT_BINDINGS = "Swift";
      SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
      SQLite3_LIBRARY = "$(Get-ProjectBinaryCache $Platform SQLite)\SQLite3.lib";
    }
}

function Test-LLBuild {
  # Build additional llvm executables needed by tests
  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $BuildPlatform
    Invoke-Program $ninja -C (Get-ProjectBinaryCache $BuildPlatform BuildTools) FileCheck not
  }

  Invoke-IsolatingEnvVars {
    $env:Path = "$env:Path;$UnixToolsBinDir"
    $env:AR = ([IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "llvm-ar.exe"))
    $env:CLANG = ([IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "clang.exe"))

    Build-CMakeProject `
      -Src $SourceCache\llbuild `
      -Bin (Get-ProjectBinaryCache $BuildPlatform LLBuild) `
      -Platform $Platform `
      -UseMSVCCompilers CXX `
      -UseBuiltCompilers Swift `
      -SwiftSDK (Get-SwiftSDK $BuildPlatform.OS) `
      -BuildTargets default,test-llbuild `
      -Defines = @{
        BUILD_SHARED_LIBS = "YES";
        FILECHECK_EXECUTABLE = ([IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform BuildTools), "bin", "FileCheck.exe"));
        LIT_EXECUTABLE = "$SourceCache\llvm-project\llvm\utils\lit\lit.py";
        LLBUILD_SUPPORT_BINDINGS = "Swift";
        SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
        SQLite3_LIBRARY = "$(Get-ProjectBinaryCache $Platform SQLite)\SQLite3.lib";
      }
  }
}

function Build-ArgumentParser([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-argument-parser `
    -Bin (Get-ProjectBinaryCache $Platform ArgumentParser) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -UseBuiltCompilers Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      BUILD_TESTING = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    }
}

function Build-Driver([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-driver `
    -Bin (Get-ProjectBinaryCache $Platform Driver) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -UseBuiltCompilers C,CXX,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      TSC_DIR = (Get-ProjectCMakeModules $Platform ToolsSupportCore);
      LLBuild_DIR = (Get-ProjectCMakeModules $Platform LLBuild);
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
      SQLite3_LIBRARY = "$(Get-ProjectBinaryCache $Platform SQLite)\SQLite3.lib";
      SWIFT_DRIVER_BUILD_TOOLS = "YES";
      LLVM_DIR = "$(Get-ProjectBinaryCache $Platform Compilers)\lib\cmake\llvm";
      Clang_DIR = "$(Get-ProjectBinaryCache $Platform Compilers)\lib\cmake\clang";
      Swift_DIR = "$(Get-ProjectBinaryCache $Platform Compilers)\tools\swift\lib\cmake\swift";
    }
}

function Build-Crypto([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-crypto `
    -Bin (Get-ProjectBinaryCache $Platform Crypto) `
    -Platform $Platform `
    -UseBuiltCompilers ASM, C, CXX, Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      SwiftASN1_DIR = (Get-ProjectCMakeModules $Platform ASN1);
    }
}

function Build-Collections([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-collections `
    -Bin (Get-ProjectBinaryCache $Platform Collections) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    }
}

function Build-ASN1([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-asn1 `
    -Bin (Get-ProjectBinaryCache $Platform ASN1) `
    -Platform $Platform `
    -UseBuiltCompilers Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    }
}

function Build-Certificates([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-certificates `
    -Bin (Get-ProjectBinaryCache $Platform Certificates) `
    -Platform $Platform `
    -UseBuiltCompilers Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      SwiftCrypto_DIR = (Get-ProjectCMakeModules $Platform Crypto);
      SwiftASN1_DIR = (Get-ProjectCMakeModules $Platform ASN1);
    }
}

function Build-PackageManager([Hashtable] $Platform) {
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
    -UseBuiltCompilers C,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_Swift_FLAGS = @("-DCRYPTO_v2");
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      SwiftSystem_DIR = (Get-ProjectCMakeModules $Platform System);
      TSC_DIR = (Get-ProjectCMakeModules $Platform ToolsSupportCore);
      LLBuild_DIR = (Get-ProjectCMakeModules $Platform LLBuild);
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      SwiftDriver_DIR = (Get-ProjectCMakeModules $Platform Driver);
      SwiftBuild_DIR = (Get-ProjectCMakeModules $Platform Build);
      SwiftCrypto_DIR = (Get-ProjectCMakeModules $Platform Crypto);
      SwiftCollections_DIR = (Get-ProjectCMakeModules $Platform Collections);
      SwiftASN1_DIR = (Get-ProjectCMakeModules $Platform ASN1);
      SwiftCertificates_DIR = (Get-ProjectCMakeModules $Platform Certificates);
      SwiftSyntax_DIR = (Get-ProjectCMakeModules $Platform Compilers);
      SQLite3_INCLUDE_DIR = "$SourceCache\swift-toolchain-sqlite\Sources\CSQLite\include";
      SQLite3_LIBRARY = "$(Get-ProjectBinaryCache $Platform SQLite)\SQLite3.lib";
    }
}

function Build-Markdown([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-markdown `
    -Bin (Get-ProjectBinaryCache $Platform Markdown) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      "cmark-gfm_DIR" = "$($Platform.ToolchainInstallRoot)\usr\lib\cmake";
    }
}

function Build-Format([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-format `
    -Bin (Get-ProjectBinaryCache $Platform Format) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -UseMSVCCompilers C `
    -UseBuiltCompilers Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
      SwiftSyntax_DIR = (Get-ProjectCMakeModules $Platform Compilers);
      SwiftMarkdown_DIR = (Get-ProjectCMakeModules $Platform Markdown);
      "cmark-gfm_DIR" = "$($Platform.ToolchainInstallRoot)\usr\lib\cmake";
    }
}

function Test-Format {
  $SwiftPMArguments = @(
    # swift-syntax
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform Compilers)\lib\swift\host",
    "-Xswiftc", "-L$(Get-ProjectBinaryCache $BuildPlatform Compilers)\lib\swift\host",
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

function Build-LMDB([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-lmdb `
    -Bin (Get-ProjectBinaryCache $Platform LMDB) `
    -Platform $Platform `
    -UseMSVCCompilers C `
    -BuildTargets default
}

function Build-IndexStoreDB([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\indexstore-db `
    -Bin (Get-ProjectBinaryCache $Platform IndexStoreDB) `
    -Platform $Platform `
    -UseBuiltCompilers C,CXX,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      CMAKE_C_FLAGS = @("-I$(Get-SwiftSDK $Platform.OS)\usr\include", "-I$(Get-SwiftSDK $Platform.OS)\usr\include\Block");
      CMAKE_CXX_FLAGS = @("-I$(Get-SwiftSDK $Platform.OS)\usr\include", "-I$(Get-SwiftSDK $Platform.OS)\usr\include\Block");
      LMDB_DIR = (Get-ProjectCMakeModules $Platform LMDB);
    }
}

function Build-SourceKitLSP([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\sourcekit-lsp `
    -Bin (Get-ProjectBinaryCache $Platform SourceKitLSP) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr" `
    -Platform $Platform `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
      SwiftSyntax_DIR = (Get-ProjectCMakeModules $Platform Compilers);
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
    }
}

function Test-SourceKitLSP {
  $SwiftPMArguments = @(
    # dispatch
    "-Xcc", "-I$SourceCache\swift-corelibs-libdispatch",
    "-Xcc", "-I$SourceCache\swift-corelibs-libdispatch\src\BlocksRuntime",
    # swift-syntax
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform Compilers)\lib\swift\host",
    "-Xswiftc", "-L$(Get-ProjectBinaryCache $BuildPlatform Compilers)\lib\swift\host",
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
    # swift-asn1
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform ASN1)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform ASN1)\lib",
    # swift-package-manager
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform PackageManager)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform PackageManager)\lib",
    # swift-markdown
    "-Xswiftc", "-I$SourceCache\swift-markdown\Sources\CAtomic\inclde",
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

function Build-TestingMacros([Hashtable] $Platform) {
  Build-CMakeProject `
    -Src $SourceCache\swift-testing\Sources\TestingMacros `
    -Bin (Get-ProjectBinaryCache $Platform TestingMacros) `
    -InstallTo "$($Platform.ToolchainInstallRoot)\usr"  `
    -Platform $Platform `
    -UseBuiltCompilers Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      SwiftSyntax_DIR = (Get-ProjectCMakeModules $Platform Compilers);
    }
}

function Install-HostToolchain() {
  if ($ToBatch) { return }

  # We've already special-cased $HostPlatform.ToolchainInstallRoot to point to $ToolchainInstallRoot.
  # There are only a few extra restructuring steps we need to take care of.

  # Restructure _InternalSwiftScan (keep the original one for the installer)
  Copy-Item -Force `
    -Path "$($HostPlatform.ToolchainInstallRoot)\usr\lib\swift\_InternalSwiftScan" `
    -Destination "$($HostPlatform.ToolchainInstallRoot)\usr\include"
  Copy-Item -Force `
    -Path "$($HostPlatform.ToolchainInstallRoot)\usr\lib\swift\windows\_InternalSwiftScan.lib" `
    -Destination "$($HostPlatform.ToolchainInstallRoot)\usr\lib"

  # Switch to swift-driver
  $SwiftDriver = ([IO.Path]::Combine((Get-ProjectBinaryCache $HostPlatform Driver), "bin", "swift-driver.exe"))
  Copy-Item -Force `
    -Path $SwiftDriver `
    -Destination "$($HostPlatform.ToolchainInstallRoot)\usr\bin\swift.exe"
  Copy-Item -Force `
    -Path $SwiftDriver `
    -Destination "$($HostPlatform.ToolchainInstallRoot)\usr\bin\swiftc.exe"
}

function Build-Inspect([Hashtable] $Platform) {
  if ($Platform -eq $HostPlatform) {
    # When building for the host target, use the host version of the swift-argument-parser,
    # and place the host swift-inspect executable with the other host toolchain binaries.
    $ArgumentParserDir = Get-ProjectCMakeModules $HostPlatform ArgumentParser
    $InstallPath = "$($HostPlatform.ToolchainInstallRoot)\usr"
  } else {
    # When building for non-host target, let CMake fetch the swift-argument-parser dependency
    # since it is currently only built for the host and and cannot be built for Android until
    # the pinned version is >= 1.5.0.
    $ArgumentParserDir = ""
    $InstallPath = "$(Get-PlatformRoot $Platform.OS)\Developer\Library\$(Get-ModuleTriple $Platform)"
  }

  Build-CMakeProject `
    -Src $SourceCache\swift\tools\swift-inspect `
    -Bin (Get-ProjectBinaryCache $Platform SwiftInspect)`
    -InstallTo $InstallPath `
    -Platform $Platform `
    -UseBuiltCompilers C,CXX,Swift `
    -SwiftSDK (Get-SwiftSDK $Platform.OS) `
    -Defines @{
      CMAKE_Swift_FLAGS = @(
        "-Xcc", "-I$(Get-SwiftSDK $Platform.OS)\usr\include",
        "-Xcc", "-I$(Get-SwiftSDK $Platform.OS)\usr\lib\swift",
        "-Xcc", "-I$(Get-SwiftSDK $Platform.OS)\usr\include\swift\SwiftRemoteMirror",
        "-L$(Get-SwiftSDK $Platform.OS)\usr\lib\swift\$($Platform.OS.ToString())\$($Platform.Architecture.LLVMName)"
      );
      ArgumentParser_DIR = $ArgumentParserDir;
    }
}

function Build-DocC() {
  Build-SPMProject `
    -Action Build `
    -Src $SourceCache\swift-docc `
    -Bin $(Get-ProjectBinaryCache $BuildPlatform DocC) `
    -Platform $BuildPlatform `
    --product docc
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
    -Xcc "-I$(Get-InstallDir $Platform)\Toolchains\$ProductVersion+Asserts\usr\include" `
    -Xlinker "-L$(Get-InstallDir $Platform)\Toolchains\$ProductVersion+Asserts\usr\lib"
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
  }

  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $Platform
    # Avoid hard-coding the VC tools version number
    $VCRedistDir = (Get-ChildItem "${env:VCToolsRedistDir}\$($HostPlatform.Architecture.ShortName)" -Filter "Microsoft.VC*.CRT").FullName
    if ($VCRedistDir) {
      $Properties["VCRedistDir"] = "$VCRedistDir\"
    }
  }

  $Properties["Platforms"] = "`"windows$(if ($Android) { ";android" })`"";
  $Properties["AndroidArchitectures"] = "`"$(($AndroidSDKPlatforms | ForEach-Object { $_.Architecture.LLVMName }) -Join ";")`""
  $Properties["WindowsArchitectures"] = "`"$(($WindowsSDKPlatforms | ForEach-Object { $_.Architecture.LLVMName }) -Join ";")`""
  $Properties["ToolchainVariants"] = "`"asserts$(if ($IncludeNoAsserts) { ";noasserts" })`"";
  foreach ($SDKPlatform in $WindowsSDKPlatforms) {
    $Properties["WindowsRuntime$($SDKPlatform.Architecture.ShortName.ToUpperInvariant())"] = [IO.Path]::Combine((Get-InstallDir $SDKPlatform), "Runtimes", "$ProductVersion");
  }

  Build-WiXProject bundle\installer.wixproj -Platform $Platform -Bundle -Properties $Properties
}

function Copy-BuildArtifactsToStage([Hashtable] $Platform) {
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\*.cab" $Stage
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\*.msi" $Stage
  foreach ($SDKPlatform in $WindowsSDKPlatforms) {
    Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($SDKPlatform.Architecture.VSName)\*.msm" $Stage
  }
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\installer.exe" $Stage
  # Extract installer engine to ease code-signing on swift.org CI
  if ($ToBatch) {
    Write-Output "md `"$BinaryCache\$($Platform.Triple)\installer\$($Platform.Architecture.VSName)`""
  } else {
    New-Item -Type Directory -Path "$BinaryCache\$($Platform.Triple)\installer\$($Platform.Architecture.VSName)" -ErrorAction Ignore | Out-Null
  }
  Invoke-Program "$($WiX.Path)\wix.exe" -- burn detach "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\installer.exe" -engine "$Stage\installer-engine.exe" -intermediateFolder "$BinaryCache\$($Platform.Triple)\installer\$($Platform.Architecture.VSName)\"
}

function Build-NoAssertsToolchain() {
  if ($ToBatch) {
    Write-Output ""
    Write-Output "Building NoAsserts Toolchain ..."
  } else {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Building NoAsserts Toolchain ..."
  }
  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()

  Invoke-BuildStep Build-Compilers $HostPlatform -Variant "NoAsserts"

  # Only compilers have NoAsserts enabled. Copy the rest of the Toolcahin binaries from the Asserts output
  # Use robocopy for efficient copying
  #   /E : Copies subdirectories, including empty ones.
  #   /XC: Excludes existing files with the same timestamp but different file sizes.
  #   /XN: Excludes existing files that are newer than the copy in the source directory.
  #   /XO: Excludes existing files that are older than the copy in the source directory.
  #   /NFL: Do not list coppied files in output
  #   /NDL: Do not list directories in output
  #   /NJH: Do not write a job header
  #   /NC: Do not write file classes
  #   /NS: Do not write file sizes
  #   /NP: Do not show progress indicator
  &robocopy $HostPlatform.ToolchainInstallRoot $HostPlatform.NoAssertsToolchainInstallRoot /E /XC /XN /XO /NS /NC /NFL /NDL /NJH

  if (-not $ToBatch) {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Building Instalting NoAsserts Toolchain in $($Stopwatch.Elapsed)"
    Write-Host ""
  }
}

#-------------------------------------------------------------------
try {

Get-Dependencies

if ($Clean) {
  Remove-Item -Force -Recurse -Path "$BinaryCache\$($BuildPlatform.Triple)\" -ErrorAction Ignore
  Remove-Item -Force -Recurse -Path "$BinaryCache\$($HostPlatform.Triple)\" -ErrorAction Ignore
  foreach ($Platform in $WindowsSDKPlatforms) {
    Remove-Item -Force -Recurse -Path "$BinaryCache\$($Platform.Triple)\" -ErrorAction Ignore
  }
  foreach ($Platform in $AndroidSDKPlatforms) {
    Remove-Item -Force -Recurse -Path "$BinaryCache\$($Platform.Triple)\" -ErrorAction Ignore
  }
  Remove-Item -Force -Recurse -Path "$BinaryCache\1" -ErrorAction Ignore
  Remove-Item -Force -Recurse -Path "$BinaryCache\5" -ErrorAction Ignore
  Remove-Item -Force -Recurse -Path (Get-InstallDir $HostPlatform) -ErrorAction Ignore
}

if (-not $SkipBuild) {
  if ($EnableCaching) {
    Invoke-Program (Get-SCCache).Path --zero-stats
  }

  Remove-Item -Force -Recurse ([IO.Path]::Combine((Get-InstallDir $HostPlatform), "Platforms")) -ErrorAction Ignore

  Invoke-BuildStep Build-CMark $BuildPlatform
  Invoke-BuildStep Build-BuildTools $BuildPlatform
  if ($IsCrossCompiling) {
    Invoke-BuildStep Build-XML2 $BuildPlatform
    Invoke-BuildStep Build-Compilers $BuildPlatform -Variant "Asserts"
  }
  if ($IncludeDS2) {
    Invoke-BuildStep Build-RegsGen2 $BuildPlatform
  }

  Invoke-BuildStep Build-CMark $HostPlatform
  Invoke-BuildStep Build-XML2 $HostPlatform
  Invoke-BuildStep Build-Compilers $HostPlatform -Variant "Asserts"

  Invoke-BuildStep Build-SDK $BuildPlatform -IncludeMacros

  foreach ($Platform in $WindowsSDKPlatforms) {
    Invoke-BuildStep Build-SDK $Platform

    Get-ChildItem "$(Get-SwiftSDK Windows)\usr\lib\swift\windows" -Filter "*.lib" -File -ErrorAction Ignore | ForEach-Object {
      Write-Host -BackgroundColor DarkRed -ForegroundColor White "$($_.FullName) is not nested in an architecture directory"
      Move-Item $_.FullName "$(Get-SwiftSDK Windows)\usr\lib\swift\windows\$($Platform.Architecture.LLVMName)\" | Out-Null
    }

    Copy-Directory "$(Get-SwiftSDK Windows)\usr\bin" "$([IO.Path]::Combine((Get-InstallDir $Platform), "Runtimes", $ProductVersion, "usr"))"

    Invoke-BuildStep Build-ExperimentalSDK $Platform

    Get-ChildItem "$(Get-SwiftSDK Windows -Identifier WindowsExperimental)\usr\lib\swift_static\windows" -Filter "*.lib" -File -ErrorAction Ignore | ForEach-Object {
      Write-Host -BackgroundColor DarkRed -ForegroundColor White "$($_.FullName) is not nested in an architecture directory"
      Move-Item $_.FullName "$(Get-SwiftSDK Windows -Identifier WindowsExperimental)\usr\lib\swift_static\windows\$($Platform.Architecture.LLVMName)\" | Out-Null
    }
  }

  Write-PlatformInfoPlist Windows
  Install-SDK $WindowsSDKPlatforms
  Write-SDKSettings Windows
  Install-SDK $WindowsSDKPlatforms -Identifier WindowsExperimental
  Write-SDKSettings Windows -Identifier WindowsExperimental

  if ($Android) {
    foreach ($Platform in $AndroidSDKPlatforms) {
      Invoke-BuildStep Build-SDK $Platform

      Get-ChildItem "$(Get-SwiftSDK Android)\usr\lib\swift\android" -File | Where-Object { $_.Name -match ".a$|.so$" } | ForEach-Object {
        Write-Host -BackgroundColor DarkRed -ForegroundColor White "$($_.FullName) is not nested in an architecture directory"
        Move-Item $_.FullName "$(Get-SwiftSDK Android)\usr\lib\swift\android\$($Platform.Architecture.LLVMName)\" | Out-Null
      }

      Invoke-BuildStep Build-ExperimentalSDK $Platform

      Get-ChildItem "$(Get-SwiftSDK Android -Identifier AndroidExperimental)\usr\lib\swift_static\android" -File | Where-Object { $_.Name -match ".a$|.so$" } | ForEach-Object {
        Write-Host -BackgroundColor DarkRed -ForegroundColor White "$($_.FullName) is not nested in an architecture directory"
        Move-Item $_.FullName "$(Get-SwiftSDK Android -Identifier AndroidExperimental)\usr\lib\swift_static\android\$($Platform.Architecture.LLVMName)\" | Out-Null
      }
    }

    Write-PlatformInfoPlist Android
    Install-SDK $AndroidSDKPlatforms
    Write-SDKSettings Android
    Install-SDK $AndroidSDKPlatforms -Identifiers AndroidExperimental
    Write-SDKSettings Android -Identifier AndroidExperimental

    # Android swift-inspect only supports 64-bit platforms.
    $AndroidSDKPlatforms | Where-Object { @("arm64-v8a", "x86_64") -contains $_.Architecture.ABI } | ForEach-Object {
      Invoke-BuildStep Build-Inspect $_
    }
  }

  # Build Macros for distribution
  Invoke-BuildStep Build-FoundationMacros $HostPlatform
  Invoke-BuildStep Build-TestingMacros $HostPlatform

  Invoke-BuildStep Build-SQLite $HostPlatform
  Invoke-BuildStep Build-ToolsSupportCore $HostPlatform
  Invoke-BuildStep Build-LLBuild $HostPlatform
  Invoke-BuildStep Build-ArgumentParser $HostPlatform
  Invoke-BuildStep Build-Driver $HostPlatform
  Invoke-BuildStep Build-ASN1 $HostPlatform
  Invoke-BuildStep Build-Crypto $HostPlatform
  Invoke-BuildStep Build-Collections $HostPlatform
  Invoke-BuildStep Build-Certificates $HostPlatform
  Invoke-BuildStep Build-System $HostPlatform
  Invoke-BuildStep Build-Subprocess $HostPlatform
  Invoke-BuildStep Build-Build $HostPlatform
  Invoke-BuildStep Build-PackageManager $HostPlatform
  Invoke-BuildStep Build-Markdown $HostPlatform
  Invoke-BuildStep Build-Format $HostPlatform
  Invoke-BuildStep Build-LMDB $HostPlatform
  Invoke-BuildStep Build-IndexStoreDB $HostPlatform
  Invoke-BuildStep Build-SourceKitLSP $HostPlatform
  Invoke-BuildStep Build-Inspect $HostPlatform
}

Install-HostToolchain

if (-not $SkipBuild) {
  Invoke-BuildStep Build-mimalloc $HostPlatform
}

if (-not $SkipBuild -and $IncludeNoAsserts) {
  Build-NoAssertsToolchain
}

if (-not $SkipBuild -and -not $IsCrossCompiling) {
  Invoke-BuildStep Build-DocC $HostPlatform
}

if (-not $SkipPackaging) {
  Invoke-BuildStep Build-Installer $HostPlatform
}

if ($Stage) {
  Copy-BuildArtifactsToStage $HostPlatform
}

if (-not $IsCrossCompiling) {
  $CompilersTests = @("clang", "lld", "lldb", "llvm", "swift")
  if ($Test | Where-Object { $CompilersTests -contains $_ }) {
    $Tests = @{
      "-TestClang" = $Test -contains "clang";
      "-TestLLD" = $Test -contains "lld";
      "-TestLLDB" = $Test -contains "lldb";
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

  if ($Test -contains "swift") {
    foreach ($Platform in $AndroidSDKPlatforms) {
      try {
        Invoke-BuildStep Test-Runtime $Platform
      } catch {}
    }
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
