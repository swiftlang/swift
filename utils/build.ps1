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

.PARAMETER ImageRoot
The path to a directory that mimics a file system image root, under which the
"Program Files" subdirectories will be created with the files installed by CMake.
Default: 'S:\'

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

.PARAMETER ProductVersion
The product version to be used when building the installer. Supports semantic
version strings (e.g., "1.0.0"). Default: "0.0.0"

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

.PARAMETER DebugFormat
The debug information format for. Valid values: dwarf, codeview.
Default: codeview

.PARAMETER Android
Build Android SDKs. Requires Android NDK to be available.

.PARAMETER AndroidNDKVersion
The version number of the Android NDK to be used.
Format: r{number}[{letter}] (e.g., r28c)
Default: "r28c"

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

.PARAMETER SkipBuild
Skip the build phase entirely. Useful for testing packaging or other post-build
steps.

.PARAMETER SkipPackaging
Skip building the MSI installers and packaging. Useful for development builds.

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
  [System.IO.FileInfo] $BinaryCache = "S:\b",
  [System.IO.FileInfo] $ImageRoot = "S:",
  [System.IO.FileInfo] $Cache = "S:\CAS",
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

  # Dependencies
  [ValidatePattern('^\d+(\.\d+)*$')]
  [string] $PythonVersion = "3.10.1",

  # Toolchain Version Info
  [string] $ProductVersion = "0.0.0",
  [string] $ToolchainIdentifier = $(if ($env:TOOLCHAIN_VERSION) { $env:TOOLCHAIN_VERSION } else { "$env:USERNAME.development" }),

  # Toolchain Cross-compilation
  [ValidateSet("AMD64", "ARM64")]
  [string] $HostArchName = $(if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }),
  [object] $UseHostToolchain = $true,

  # Debug Information
  [switch] $DebugInfo,
  [ValidateSet("codeview", "dwarf")]
  [string] $DebugFormat = "codeview",

  # Android SDK Options
  [switch] $Android = $false,
  [ValidatePattern("^r(?:[1-9]|[1-9][0-9])(?:[a-z])?$")]
  [string] $AndroidNDKVersion = "r28c",
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

  # Incremental Build Support
  [switch] $Clean,
  [switch] $SkipBuild = $false,
  [switch] $SkipPackaging = $false,
  [string[]] $Test = @(),

  [switch] $IncludeDS2 = $false,
  [ValidateSet("none", "full", "thin")]
  [string] $LTO = "none",
  [switch] $IncludeNoAsserts = $false,
  [ValidateSet("debug", "release")]
  [string] $FoundationTestConfiguration = "debug",

  [switch] $Summary
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

## Declare static build and build tool parameters.

$DefaultPinned = @{
  AMD64 = @{
    PinnedBuild = "https://download.swift.org/development/windows10/swift-DEVELOPMENT-SNAPSHOT-2026-03-16-a/swift-DEVELOPMENT-SNAPSHOT-2026-03-16-a-windows10.exe";
    PinnedSHA256 = "34C90B5535A2D137C874A12D591201D2C3E324FB437CE51B6D057B8A8BA2CC4E";
    PinnedVersion = "0.0.0";
  };
  ARM64 = @{
    PinnedBuild = "https://download.swift.org/development/windows10-arm64/swift-DEVELOPMENT-SNAPSHOT-2026-03-16-a/swift-DEVELOPMENT-SNAPSHOT-2026-03-16-a-windows10-arm64.exe"
    PinnedSHA256 = "A60198647128269812AA00179801725BBD58D714AE52F2D19E7D0133DC035BF2";
    PinnedVersion = "0.0.0";
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
    LinkModes = $WindowsSDKLinkModes;
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
    LinkModes = $WindowsSDKLinkModes;
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
    LinkModes = $WindowsSDKLinkModes;
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
    LinkModes = $AndroidSDKLinkModes;
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
    LinkModes = $AndroidSDKLinkModes;
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
    LinkModes = $AndroidSDKLinkModes;
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
    LinkModes = $AndroidSDKLinkModes;
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
  };
  "3.10.1" = @{
    AMD64 = @{
      URL = "https://www.nuget.org/api/v2/package/python/3.10.1";
      SHA256 = "987a0e446d68900f58297bc47dc7a235ee4640a49dace58bc9f573797d3a8b33";
    };
    AMD64_Embedded = @{
      URL = "https://www.python.org/ftp/python/3.10.1/python-3.10.1-embed-amd64.zip";
      SHA256 = "502670dcdff0083847abf6a33f30be666594e7e5201cd6fccd4a523b577403de";
    };
    ARM64 = @{
      URL = "https://www.nuget.org/api/v2/package/pythonarm64/3.10.1";
      SHA256 = "16becfccedf1269ff0b8695a13c64fac2102a524d66cecf69a8f9229a43b10d3";
    };
    ARM64_Embedded = @{
      URL = "https://www.python.org/ftp/python/3.10.1/python-3.10.1-embed-arm64.zip";
      SHA256 = "1f9e215fe4e8f22a8e8fba1859efb1426437044fb3103ce85794630e3b511bc2";
    };
  };
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
  r27d = @{
    URL = "https://dl.google.com/android/repository/android-ndk-r27d-windows.zip"
    SHA256 = "82094f53e66a76b6a9ec4fc35a5076091a92de3b91d13c5d4a7cfdb226304c59"
    ClangVersion = 18
  }
  r28c = @{
    URL = "https://dl.google.com/android/repository/android-ndk-r28c-windows.zip"
    SHA256 = "6bec98ac2354d8a919760889a1a41d020132e5e8cfa1b1fe51610a72c36a466b"
    ClangVersion = 19
  }
}

$KnownSyft = @{
  "1.29.1" = @{
    AMD64 = @{
      URL = "https://github.com/anchore/syft/releases/download/v1.29.1/syft_1.29.1_windows_amd64.zip"
      SHA256 = "3C67CD9AF40CDCC7FFCE041C8349B4A77F33810184820C05DF23440C8E0AA1D7"
      Path = [IO.Path]::Combine("$BinaryCache\syft-1.29.1", "syft.exe")
    }
  };
  "1.40.0" = @{
    AMD64 = @{
      URL = "https://github.com/anchore/syft/releases/download/v1.40.0/syft_1.40.0_windows_amd64.zip"
      SHA256 = "3F4021EC098B4BCBAF19BBA7028CF7704FEF12936970778CEC3C6D669B740E6D"
      Path = [IO.Path]::Combine("$BinaryCache\syft-1.40.0", "syft.exe")
    };
    ARM64 = @{
      URL = "https://github.com/anchore/syft/releases/download/v1.40.0/syft_1.40.0_windows_arm64.zip"
      SHA256 = "CE7129DBCC39809542C9BC5032B179131DFEE72C68C5B3741E3270A3D9ED46E4"
      Path = [IO.Path]::Combine("$BinaryCache\syft-1.40.0", "syft.exe")
    };
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
$CMakeVersion = if ((& $cmake --version | Select-Object -First 1) -Match '(\d+\.\d+\.\d+)') {
  [version]$Matches[1]
}
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
# Use a shorter name in paths to avoid going over the path length limit.
$ToolchainVersionIdentifier = $PinnedToolchain -replace 'swift-(.+?)-windows10.*', '$1'

if ($EnableCaching) {
  if ($PinnedVersion -ne "0.0.0") {
    throw "CAS currently requires using a main-branch pinned toolchain."
  }
  $UseHostToolchain = $false
}

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

## Helpers for logging and timing build steps.

$TimingData = New-Object System.Collections.Generic.List[System.Object]
$CurrentOperation = $null

function Add-TimingData {
  param
  (
    [Parameter(Mandatory)]
    [Hashtable] $Platform,
    [Parameter(Mandatory)]
    [string] $BuildStep,
    [PSCustomObject] $Parent = $null
  )

  $TimingEntry = [PSCustomObject]@{
    Arch = $Platform.Architecture.LLVMName
    Platform = $Platform.OS.ToString()
    "Build Step" = $BuildStep
    "Elapsed Time" = [TimeSpan]::Zero
    Parent = $Parent
    Children = @()
  }

  if ($Parent) {
    $Parent.Children += $TimingEntry
  }

  $TimingData.Add($TimingEntry)
  return $TimingEntry
}

function Record-OperationTime {
  param
  (
    [Parameter(Mandatory)]
    [Hashtable] $Platform,
    [Parameter(Mandatory)]
    [string] $BuildStep,
    [Parameter(Mandatory)]
    [ScriptBlock] $ScriptBlock
  )
  if (!$Summary) {
    & $ScriptBlock
    return
  }

  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()
  $TimingEntry = Add-TimingData $Platform $BuildStep $script:CurrentOperation
  $script:CurrentOperation = $TimingEntry

  try {
    & $ScriptBlock
  } finally {
    $Stopwatch.Stop()
    $TimingEntry."Elapsed Time" = $Stopwatch.Elapsed
    $script:CurrentOperation = $TimingEntry.Parent
  }
}

function Flatten-TimingEntry {
  param(
    [Parameter(Mandatory)]
    [PSCustomObject] $Entry,
    [Parameter(Mandatory)]
    [TimeSpan] $TotalTime,
    [int] $Depth = 0
  )

  $Indent = "  " * $Depth
  $Percentage = [math]::Round(($Entry."Elapsed Time".TotalSeconds / $TotalTime.TotalSeconds) * 100, 1)
  $FormattedTime = "{0:hh\:mm\:ss\.ff}" -f $Entry."Elapsed Time"

  [PSCustomObject]@{
    "Build Step" = "$Indent$($Entry.'Build Step')"
    Platform = $Entry.Platform
    Arch = $Entry.Arch
    "Elapsed Time" = $FormattedTime
    "%" = "$Percentage%"
  }

  $SortedChildren = $Entry.Children | Sort-Object -Descending -Property "Elapsed Time"
  foreach ($Child in $SortedChildren) {
    Flatten-TimingEntry $Child $TotalTime ($Depth + 1)
  }
}

function Write-Summary {
  Write-Host "Summary:" -ForegroundColor Cyan

  $TotalTime = [TimeSpan]::Zero
  foreach ($Entry in $TimingData) {
    if (-not $Entry.Parent) {
      $TotalTime = $TotalTime.Add($Entry."Elapsed Time")
    }
  }

  $RootEntries = $TimingData | Where-Object { -not $_.Parent } | Sort-Object -Descending -Property "Elapsed Time"
  $Result = foreach ($RootEntry in $RootEntries) {
    Flatten-TimingEntry $RootEntry $TotalTime
  }

  $FormattedTotalTime = "{0:hh\:mm\:ss\.ff}" -f $TotalTime
  $TotalRow = [PSCustomObject]@{
    "Build Step" = "TOTAL"
    Platform = ""
    Arch = ""
    "Elapsed Time" = $FormattedTotalTime
    "%" = "100.0%"
  }

  @($Result) + $TotalRow | Format-Table -AutoSize
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

function Get-PythonPath([Hashtable] $Platform) {
  return [IO.Path]::Combine("$BinaryCache\", "Python$($Platform.Architecture.CMakeName)-$PythonVersion")
}

function Get-EmbeddedPythonPath([Hashtable] $Platform) {
  return [IO.Path]::Combine("$BinaryCache\", "EmbeddedPython$($Platform.Architecture.CMakeName)-$PythonVersion")
}

function Get-PythonExecutable {
  return [IO.Path]::Combine((Get-PythonPath $BuildPlatform), "tools", "python.exe")
}

function Get-EmbeddedPythonInstallDir() {
  return [IO.Path]::Combine("$ImageRoot\", "Program Files", "Swift", "Python-$PythonVersion")
}

function Get-Syft {
  return $KnownSyft[$SyftVersion][$BuildArchName]
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

function Get-WindowsRuntimeInstallRoot([Hashtable] $Platform) {
  return [IO.Path]::Combine((Get-InstallDir $Platform), "Runtimes", $ProductVersion)
}

function Get-WindowsRuntimeBin([Hashtable] $Platform) {
  return [IO.Path]::Combine((Get-WindowsRuntimeInstallRoot $Platform), "usr", "bin")
}

function Get-WindowsRuntimeLibexec([Hashtable] $Platform) {
  return [IO.Path]::Combine((Get-WindowsRuntimeInstallRoot $Platform), "usr", "libexec")
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

  $SplatArgs = @{}
  if ($RemainingArgs) {
    for ($Index = 0; $Index -lt $RemainingArgs.Count; $Index++) {
      $Arg = $RemainingArgs[$Index]
      if ($Arg -is [Hashtable]) {
        $SplatArgs += $Arg
      } elseif ($Arg -is [string] -and $Arg.StartsWith('-')) {
        $ParamName = $Arg.TrimStart('-')
        $HasNextArg = $Index -lt ($RemainingArgs.Count - 1)
        if ($HasNextArg) {
          $NextArg = $RemainingArgs[$Index + 1]
          if (-not ($NextArg -is [string] -and $NextArg.StartsWith('-'))) {
            $SplatArgs[$ParamName] = $NextArg
            $Index++ # Skip NextArg
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

  Record-OperationTime $Platform $Name {
    & $Name $Platform @SplatArgs
  }
}

enum Project {
  BuildTools
  RegsGen2

  EarlySwiftDriver
  EarlySwiftDriverSQLite
  Stage0Compilers
  Stage0XML2
  BootstrapRuntime
  BootstrapOverlay
  BootstrapRuntimeModule
  BootstrapStringProcessing
  BootstrapSynchronization
  BootstrapDistributed
  BootstrapObservation
  BootstrapDifferentiation
  BootstrapVolatile
  BootstrapFoundationMacros
  BootstrapTestingMacros
  BootstrapCDispatch
  BootstrapZLib
  BootstrapBrotli
  BootstrapXML2
  BootstrapCURL
  BootstrapDispatch
  BootstrapFoundation
  Stage1Compilers

  CDispatch
  Stage2Compilers
  Stage2XML2
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
  ToolsProtocols
  Build
  PackageManager
  PackageManagerRuntime
  Markdown
  Format
  LMDB
  IndexStoreDB
  SourceKitLSP
  SymbolKit
  DocC
  ZLib
  brotli
  XML2
  CURL

  XCTest
  Testing
  ClangBuiltins
  ClangRuntime
  SwiftInspect
  DynamicCDispatch
  DynamicZLib
  DynamicBrotli
  DynamicXML2
  DynamicCURL
  DynamicRuntime
  DynamicOverlay
  DynamicRuntimeModule
  DynamicStringProcessing
  DynamicSynchronization
  DynamicDistributed
  DynamicObservation
  DynamicDispatch
  DynamicDifferentiation
  DynamicVolatile
  DynamicFoundation
  StaticCDispatch
  StaticZLib
  StaticBrotli
  StaticXML2
  StaticCURL
  StaticRuntime
  StaticOverlay
  StaticRuntimeModule
  StaticStringProcessing
  StaticSynchronization
  StaticDistributed
  StaticObservation
  StaticDifferentiation
  StaticVolatile
  StaticDispatch
  StaticFoundation
  Backtrace
}

function Get-ProjectBinaryCache([Hashtable] $Platform, [Project] $Project) {
  if ($Project -eq [Project]::Compilers) {
    if ($Platform -eq $HostPlatform) { return "$BinaryCache\5" }
    if ($Platform -eq $BuildPlatform) { return "$BinaryCache\1" }
    throw "Building Compilers for $($Platform.Triple) currently unsupported."
  }
  return "$([IO.Path]::Combine("$BinaryCache\", $Platform.Triple, $Project.ToString()))"
}

function Get-ProjectToolchainRoot([Hashtable] $Platform, [Project] $Project) {
  return [IO.Path]::Combine((Get-ProjectBinaryCache $Platform $Project), "toolchain")
}

function Get-ProjectToolchainBin([Hashtable] $Platform, [Project] $Project) {
  return [IO.Path]::Combine((Get-ProjectToolchainRoot $Platform $Project), "usr", "bin")
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
  New-Item -ItemType Directory -ErrorAction Ignore $DstDir | Out-Null
  Copy-Item -Force -Path $Src -Destination $Dst
}

function Copy-Directory($Src, $Dst) {
  New-Item -ItemType Directory -ErrorAction Ignore $Dst | Out-Null
  Copy-Item -Force -Recurse -Path $Src -Destination $Dst
}

function Move-Directory($Src, $Dst) {
  $Destination = Join-Path -Path $Dst -ChildPath (Split-Path -Path $Src -Leaf)
  New-Item -ItemType Directory -ErrorAction Ignore $Dst | Out-Null
  if (Test-Path -Path $Destination -Type Container) {
    # Destination already exists — merge source files in rather than replacing
    # the whole tree, so that files already placed by earlier steps (e.g. the
    # dispatch module.modulemap copied by the post-dispatch repair) are preserved.
    Get-ChildItem $Src | ForEach-Object {
      Move-Item -Force $_.FullName $Destination | Out-Null
    }
    Remove-Item -Force $Src | Out-Null
  } else {
    Move-Item -Path $Src -Destination $Destination -Force | Out-Null
  }
}

function ConvertTo-ThickLayout([Hashtable] $Platform, [string] $Resources, [string[]] $Filter) {
  Get-ChildItem "${Resources}\*" -File -Include ${Filter} -ErrorAction Ignore | ForEach-Object {
    Write-Host -BackgroundColor DarkRed -ForegroundColor White "$($_.FullName) is not nested in an architecture directory"
    Move-Item -Force $_.FullName "$Resources\$($Platform.Architecture.LLVMName)\" | Out-Null
  }
}

# Windows SxS requires `<assemblyIdentity version=...>` to be `a.b.c.d`.
# Match the WiX product-version normalization, then pad to four components.
function ConvertTo-FourPartVersion([string] $Version) {
  $Numeric = [regex]::Replace($Version, "[-+].*", "")
  $Parts = @($Numeric.Split('.'))
  while ($Parts.Count -lt 4) { $Parts += "0" }
  return ($Parts[0..3] -join ".")
}

function Set-WindowsAssemblyManifest([string] $ImagePath,
                                     [string] $AssemblyVersion,
                                     [string] $ProcessorArchitecture,
                                     [string] $LogPrefix) {
  if (-not (Test-Path $ImagePath)) {
    throw "${LogPrefix}: '$ImagePath' does not exist"
  }
  if ($AssemblyVersion -notmatch '^\d+\.\d+\.\d+\.\d+$') {
    throw "${LogPrefix}: AssemblyVersion '$AssemblyVersion' is not in the required 4-part 'a.b.c.d' form"
  }

  $ManifestPath = Join-Path ([IO.Path]::GetTempPath()) `
    "swift-sxs-assembly-$([guid]::NewGuid().Guid).manifest"
  $AssemblyName = [IO.Path]::GetFileNameWithoutExtension($ImagePath)
  $FileName = [IO.Path]::GetFileName($ImagePath)

  try {
    $ManifestXml = @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <assemblyIdentity type="win32" name="$AssemblyName" version="$AssemblyVersion"
      processorArchitecture="$ProcessorArchitecture"/>
  <file name="$FileName"/>
</assembly>
"@
    Set-Content -Path $ManifestPath -Value $ManifestXml -Encoding UTF8 -ErrorAction Stop

    Write-Host "${LogPrefix}: embedding assembly manifest into '$ImagePath'"
    $Output = & "mt.exe" -nologo -manifest $ManifestPath "-outputresource:$ImagePath;#1" 2>&1
    $ExitCode = $LASTEXITCODE
    if ($Output) { $Output | ForEach-Object { Write-Host "${LogPrefix}:   mt: $_" } }
    if ($ExitCode -ne 0) {
      throw "${LogPrefix}: mt failed for '$ImagePath' (exit $ExitCode)"
    }
  } finally {
    Remove-Item -Path $ManifestPath -Force -ErrorAction SilentlyContinue
  }
}

function Test-WindowsAssemblyManifestMatchesImage([string] $ManifestPath,
                                                  [string] $ImagePath,
                                                  [string] $AssemblyVersion,
                                                  [string] $ProcessorArchitecture) {
  $Doc = New-Object System.Xml.XmlDocument
  $Doc.Load($ManifestPath)

  $Assembly = $Doc.SelectSingleNode("/*[local-name()='assembly']")
  if ($null -eq $Assembly) { return $false }

  $AssemblyIdentity =
    $Assembly.SelectSingleNode("*[local-name()='assemblyIdentity']")
  if ($null -eq $AssemblyIdentity) { return $false }

  $File = $Assembly.SelectSingleNode("*[local-name()='file']")
  if ($null -eq $File) { return $false }

  $AssemblyName = [IO.Path]::GetFileNameWithoutExtension($ImagePath)
  $FileName = [IO.Path]::GetFileName($ImagePath)

  return (
    ($AssemblyIdentity.GetAttribute("type") -ieq "win32") -and
    ($AssemblyIdentity.GetAttribute("name") -eq $AssemblyName) -and
    ($AssemblyIdentity.GetAttribute("version") -eq $AssemblyVersion) -and
    ($AssemblyIdentity.GetAttribute("processorArchitecture") -ieq $ProcessorArchitecture) -and
    ($File.GetAttribute("name") -eq $FileName)
  )
}

function Ensure-WindowsAssemblyManifest([string] $ImagePath,
                                        [string] $AssemblyVersion,
                                        [string] $ProcessorArchitecture,
                                        [string] $LogPrefix) {
  if (-not (Test-Path $ImagePath)) {
    throw "${LogPrefix}: '$ImagePath' does not exist"
  }

  $ExistingManifestPath = Join-Path ([IO.Path]::GetTempPath()) `
    "swift-sxs-existing-$([guid]::NewGuid().Guid).manifest"
  $KeepExistingManifest = $false

  try {
    & "mt.exe" -nologo "-inputresource:$ImagePath;#1" -out:$ExistingManifestPath 2>&1 | Out-Null
    if (Test-Path $ExistingManifestPath) {
      try {
        $Matches = Test-WindowsAssemblyManifestMatchesImage `
          -ManifestPath           $ExistingManifestPath `
          -ImagePath              $ImagePath `
          -AssemblyVersion        $AssemblyVersion `
          -ProcessorArchitecture  $ProcessorArchitecture
      } catch {
        $KeepExistingManifest = $true
        throw "${LogPrefix}: '$ImagePath' has an unparsable RT_MANIFEST #1 (extracted to '$ExistingManifestPath')"
      }

      if ($Matches) {
        Write-Host "${LogPrefix}: '$ImagePath' already has the expected assembly manifest"
        return
      }

      $KeepExistingManifest = $true
      $Message = "{0}: '{1}' has RT_MANIFEST #1, but it is not the " +
                 "expected SxS assembly manifest (extracted to '{2}'); " +
                 "rebuild the producer without a linker-generated manifest " +
                 "instead of overwriting it"
      throw ($Message -f $LogPrefix, $ImagePath, $ExistingManifestPath)
    }

    Set-WindowsAssemblyManifest `
      -ImagePath              $ImagePath `
      -AssemblyVersion        $AssemblyVersion `
      -ProcessorArchitecture  $ProcessorArchitecture `
      -LogPrefix              $LogPrefix
  } finally {
    if (-not $KeepExistingManifest) {
      Remove-Item -Path $ExistingManifestPath -Force -ErrorAction SilentlyContinue
    }
  }
}

function New-WindowsManifestDependency([string] $Name,
                                       [string] $AssemblyVersion,
                                       [string] $ProcessorArchitecture) {
  return [pscustomobject]@{
    Name                  = $Name
    Version               = $AssemblyVersion
    ProcessorArchitecture = $ProcessorArchitecture
  }
}

function Set-WindowsExecutableManifestDependencies([string] $ToolPath,
                                                   [object[]] $Dependencies,
                                                   [string] $LogPrefix) {
  if (-not $Dependencies -or $Dependencies.Count -eq 0) {
    throw "${LogPrefix}: no manifest dependencies provided for '$ToolPath'"
  }

  $AsmNS = "urn:schemas-microsoft-com:asm.v1"
  $ExistingManifestPath = Join-Path ([IO.Path]::GetTempPath()) `
    "swift-sxs-existing-$([guid]::NewGuid().Guid).manifest"
  $MergedManifestPath = Join-Path ([IO.Path]::GetTempPath()) `
    "swift-sxs-merged-$([guid]::NewGuid().Guid).manifest"
  $KeepMergedManifest = $false

  try {
    & "mt.exe" -nologo -inputresource:"$ToolPath;#1" -out:$ExistingManifestPath 2>&1 | Out-Null

    $Doc = New-Object System.Xml.XmlDocument
    if (Test-Path $ExistingManifestPath) {
      try { $Doc.Load($ExistingManifestPath) }
      catch {
        Write-Warning "${LogPrefix}: existing manifest in '$ToolPath' is unparsable ($($_.Exception.Message)); replacing it"
        $Doc = New-Object System.Xml.XmlDocument
      }
    }
    if (-not $Doc.DocumentElement) {
      [void]$Doc.AppendChild($Doc.CreateXmlDeclaration("1.0", "UTF-8", "yes"))
      $Root = $Doc.CreateElement("assembly", $AsmNS)
      $Root.SetAttribute("manifestVersion", "1.0")
      [void]$Doc.AppendChild($Root)
    }
    $Root = $Doc.DocumentElement

    $NSMgr = New-Object System.Xml.XmlNamespaceManager($Doc.NameTable)
    $NSMgr.AddNamespace("a", $AsmNS)
    if (-not $Root.SelectSingleNode("a:assemblyIdentity", $NSMgr)) {
      $AsmId = $Doc.CreateElement("assemblyIdentity", $AsmNS)
      $AsmId.SetAttribute("type", "win32")
      $AsmId.SetAttribute("name", [IO.Path]::GetFileNameWithoutExtension($ToolPath))
      $AsmId.SetAttribute("version", $Dependencies[0].Version)
      $AsmId.SetAttribute("processorArchitecture", $Dependencies[0].ProcessorArchitecture)
      [void]$Root.PrependChild($AsmId)
    }

    foreach ($Node in @($Root.SelectNodes("a:dependency", $NSMgr))) {
      [void]$Root.RemoveChild($Node)
    }

    foreach ($Dependency in $Dependencies) {
      $Dep = $Doc.CreateElement("dependency", $AsmNS)
      $DepAsm = $Doc.CreateElement("dependentAssembly", $AsmNS)
      $AsmId  = $Doc.CreateElement("assemblyIdentity", $AsmNS)
      $AsmId.SetAttribute("type", "win32")
      $AsmId.SetAttribute("name", $Dependency.Name)
      $AsmId.SetAttribute("version", $Dependency.Version)
      $AsmId.SetAttribute("processorArchitecture", $Dependency.ProcessorArchitecture)
      $AsmId.SetAttribute("language", "*")
      [void]$DepAsm.AppendChild($AsmId)
      [void]$Dep.AppendChild($DepAsm)
      [void]$Root.AppendChild($Dep)
    }

    $Doc.Save($MergedManifestPath)
    $Output = & "mt.exe" -nologo -manifest $MergedManifestPath "-outputresource:$ToolPath;#1" 2>&1
    $ExitCode = $LASTEXITCODE
    if ($Output) { $Output | ForEach-Object { Write-Host "${LogPrefix}:   mt: $_" } }
    if ($ExitCode -ne 0) {
      $KeepMergedManifest = $true
      throw "${LogPrefix}: mt failed for '$ToolPath' (exit $ExitCode). Merged manifest preserved at '$MergedManifestPath'."
    }
  } finally {
    Remove-Item -Path $ExistingManifestPath -Force -ErrorAction SilentlyContinue
    if (-not $KeepMergedManifest) {
      Remove-Item -Path $MergedManifestPath -Force -ErrorAction SilentlyContinue
    }
  }
}

function Get-WindowsManifestResourceIDs([string] $ImagePath,
                                        [string] $LogPrefix) {
  $ReadObj = Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath "llvm-readobj.exe"
  if (-not (Test-Path $ReadObj)) {
    throw "${LogPrefix}: pinned 'llvm-readobj.exe' not found at '$ReadObj'"
  }

  $Output = & $ReadObj --coff-resources $ImagePath 2>&1
  $ExitCode = $LASTEXITCODE
  if ($ExitCode -ne 0) {
    throw "${LogPrefix}: llvm-readobj failed to inspect resources in '$ImagePath' (exit $ExitCode): $($Output -join "`n")"
  }

  $CandidateIDs = [System.Collections.Generic.HashSet[int]]::new()
  $InManifestType = $false
  foreach ($Line in $Output) {
    if ($Line -match '^\s*Type:\s*(?:RT_MANIFEST\b.*|.*\(ID\s+24\)|24\b)') {
      $InManifestType = $true
      continue
    }
    if ($Line -match '^\s*Type:') {
      $InManifestType = $false
    }

    if ($InManifestType -and
        ($Line -match '^\s*Name:\s*(?:\(ID\s*)?#?([0-9]+)\)?' -or
         $Line -match '^\s*Name:.*\(ID\s+([0-9]+)\)')) {
      [void]$CandidateIDs.Add([int]$Matches[1])
    }
  }

  # llvm-readobj reports the resource tree, but resource names such as #2 can
  # occur under other resource types, e.g. RT_VERSION.  Confirm candidates with
  # mt.exe, where `;#N` is explicitly an RT_MANIFEST resource name.
  foreach ($ResourceID in 1..16) {
    [void]$CandidateIDs.Add($ResourceID)
  }

  $ManifestIDs = [System.Collections.Generic.HashSet[int]]::new()
  foreach ($ResourceID in ($CandidateIDs | Sort-Object)) {
    $ManifestPath = Join-Path ([IO.Path]::GetTempPath()) `
      "swift-sxs-probe-$([IO.Path]::GetFileName($ImagePath))-#$ResourceID-$([guid]::NewGuid().Guid).manifest"

    try {
      & "mt.exe" -nologo "-inputresource:$ImagePath;#$ResourceID" -out:$ManifestPath 2>&1 | Out-Null
      if ($LASTEXITCODE -eq 0 -and (Test-Path $ManifestPath)) {
        [void]$ManifestIDs.Add([int]$ResourceID)
      }
    } finally {
      Remove-Item -Path $ManifestPath -Force -ErrorAction SilentlyContinue
    }
  }

  return @($ManifestIDs | Sort-Object)
}

function Export-WindowsManifestResource([string] $ImagePath,
                                        [int] $ResourceID,
                                        [string] $LogPrefix) {
  $ManifestPath = Join-Path ([IO.Path]::GetTempPath()) `
    "swift-sxs-$([IO.Path]::GetFileName($ImagePath))-#$ResourceID-$([guid]::NewGuid().Guid).manifest"

  & "mt.exe" -nologo "-inputresource:$ImagePath;#$ResourceID" -out:$ManifestPath 2>&1 | Out-Null
  if ($LASTEXITCODE -ne 0 -or -not (Test-Path $ManifestPath)) {
    throw "${LogPrefix}: failed to extract RT_MANIFEST resource #$ResourceID from '$ImagePath'"
  }
  return $ManifestPath
}

function Test-WindowsManifestHasExecutionLevel([string] $ManifestPath) {
  $Doc = New-Object System.Xml.XmlDocument
  $Doc.Load($ManifestPath)
  return ($Doc.SelectSingleNode("//*[local-name()='trustInfo' or local-name()='requestedPrivileges' or local-name()='requestedExecutionLevel']") -ne $null)
}

function Assert-WindowsManifestResourcesAreSxSSafe([string] $ImagePath,
                                                   [string] $LogPrefix) {
  $ResourceIDs = @(Get-WindowsManifestResourceIDs $ImagePath $LogPrefix)
  $ReservedResourceIDs = @($ResourceIDs | Where-Object { $_ -ge 1 -and $_ -le 16 })
  $UnexpectedReservedResourceIDs = @($ReservedResourceIDs | Where-Object { $_ -ne 1 })
  if ($UnexpectedReservedResourceIDs.Count -gt 0) {
    $UnexpectedList = "#$($UnexpectedReservedResourceIDs -join ', #')"
    $Message = "{0}: '{1}' has unexpected reserved RT_MANIFEST resources " +
               "({2}); Windows reserves IDs 1..16, and this layout uses " +
               "RT_MANIFEST #1 for SxS manifests"
    throw ($Message -f $LogPrefix, $ImagePath, $UnexpectedList)
  }

  foreach ($ResourceID in $ResourceIDs) {
    $ManifestPath = Export-WindowsManifestResource $ImagePath $ResourceID $LogPrefix
    $KeepManifest = $false

    try {
      try {
        $HasExecutionLevel = Test-WindowsManifestHasExecutionLevel $ManifestPath
      } catch {
        $KeepManifest = $true
        throw "${LogPrefix}: '$ImagePath' has an unparsable RT_MANIFEST resource #$ResourceID (extracted to '$ManifestPath')"
      }

      if ($HasExecutionLevel) {
        $KeepManifest = $true
        throw "${LogPrefix}: '$ImagePath' has RT_MANIFEST resource #$ResourceID with UAC execution-level metadata (extracted to '$ManifestPath')"
      }
    } finally {
      if (-not $KeepManifest) {
        Remove-Item -Path $ManifestPath -Force -ErrorAction SilentlyContinue
      }
    }
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

function Invoke-IsolatingEnvVars([scriptblock]$Block) {
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

function Get-Dependencies {
  Record-OperationTime $BuildPlatform "Get-Dependencies" {
    function Write-Success([string] $Description) {
      $HeavyCheckMark = @{
        Object = [Char]0x2714
        ForegroundColor = 'DarkGreen'
        NoNewLine = $true
      }
      Write-Host @HeavyCheckMark
      Write-Host " $Description"
    }

    $Stopwatch = [Diagnostics.Stopwatch]::StartNew()
    Write-Host "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies ..." -ForegroundColor Cyan
    $ProgressPreference = "SilentlyContinue"

    $WebClient = New-Object Net.WebClient

    function DownloadAndVerify($URL, $Destination, $Hash) {
      if (Test-Path $Destination) { return }

      New-Item -ItemType Directory (Split-Path -Path $Destination -Parent) -ErrorAction Ignore | Out-Null
      $WebClient.DownloadFile($URL, $Destination)
      $SHA256 = Get-FileHash -Path $Destination -Algorithm SHA256
      if ($SHA256.Hash -ne $Hash) {
        throw "SHA256 mismatch ($($SHA256.Hash) vs $Hash)"
      }
    }

    function Expand-ZipFile {
      param
      (
          [string]$ZipFileName,
          [string]$ExtractPath,
          [bool]$CreateExtractPath = $true
      )

      $Source = Join-Path -Path $BinaryCache -ChildPath $ZipFileName
      $Destination = Join-Path -Path $BinaryCache -ChildPath $ExtractPath

      # Check if the extracted directory already exists and is up to date.
      if (Test-Path $Destination) {
          $ZipLastWriteTime = (Get-Item $Source).LastWriteTime
          $ExtractedLastWriteTime = (Get-Item $Destination).LastWriteTime
          # Compare the last write times
          if ($ZipLastWriteTime -le $ExtractedLastWriteTime) {
              # Write-Output "'$ZipFileName' is already extracted and up to date."
              return
          }
      }

      $Destination = if ($CreateExtractPath) { $Destination } else { $BinaryCache }

      # Write-Output "Extracting '$ZipFileName' ..."
      New-Item -ItemType Directory -ErrorAction Ignore -Path $BinaryCache | Out-Null
      Expand-Archive -Path $Source -DestinationPath $Destination -Force
    }

    function Extract-Toolchain {
      param
      (
          [string]$InstallerExeName,
          [string]$ToolchainName
      )

      $source = Join-Path -Path $BinaryCache -ChildPath $InstallerExeName
      $destination = Join-Path -Path $BinaryCache -ChildPath toolchains\$ToolchainName

      # Check if the extracted directory already exists and is up to date.
      if (Test-Path $destination) {
          $installerWriteTime = (Get-Item $source).LastWriteTime
          $extractedWriteTime = (Get-Item $destination).LastWriteTime
          if ($installerWriteTime -le $extractedWriteTime) {
              # Write-Output "'$InstallerExeName' is already extracted and up to date."
              return
          }
      }

      # Write-Output "Extracting '$InstallerExeName' ..."

      # The new runtime MSI is built to expand files into the immediate directory. So, setup the installation location.
      New-Item -ItemType Directory -ErrorAction Ignore $BinaryCache\toolchains\$ToolchainName\LocalApp\Programs\Swift\Runtimes\$PinnedVersion\usr\bin | Out-Null
      Invoke-Program "$($WiX.Path)\wix.exe" -- burn extract $BinaryCache\$InstallerExeName -out $BinaryCache\toolchains\ -outba $BinaryCache\toolchains\
      Get-ChildItem "$BinaryCache\toolchains\WixAttachedContainer" -Filter "*.msi" | ForEach-Object {
        $LogFile = [System.IO.Path]::ChangeExtension($_.Name, "log")
        $TARGETDIR = if ($_.Name -eq "rtl.msi") { "$BinaryCache\toolchains\$ToolchainName\LocalApp\Programs\Swift\Runtimes\$PinnedVersion\usr\bin" } else { "$BinaryCache\toolchains\$ToolchainName" }
        Invoke-Program -OutNull msiexec.exe /lvx! $BinaryCache\toolchains\$LogFile /qn /a $BinaryCache\toolchains\WixAttachedContainer\$($_.Name) ALLUSERS=0 TARGETDIR=$TARGETDIR
      }
    }

    if ($IncludeSBoM) {
      $syft = Get-Syft
      DownloadAndVerify $syft.URL "$BinaryCache\syft-$SyftVersion.zip" $syft.SHA256
      Expand-ZipFile syft-$SyftVersion.zip -ExtractPath syft-$SyftVersion
      Write-Success "syft $SyftVersion"
    }

    function Get-KnownPython([string] $ArchName, [bool] $EmbeddedPython = $false) {
      if (-not $KnownPythons.ContainsKey($PythonVersion)) {
        throw "Unknown python version: $PythonVersion"
      }
      $Key = $(if ($EmbeddedPython) { "${ArchName}_Embedded" } else { $ArchName })
      return $KnownPythons[$PythonVersion][$Key]
    }

    function Install-Python([string] $ArchName, [bool] $EmbeddedPython = $false) {
      $Python = Get-KnownPython $ArchName $EmbeddedPython
      $FileName = $(if ($EmbeddedPython) { "EmbeddedPython$ArchName-$PythonVersion" } else { "Python$ArchName-$PythonVersion" })
      DownloadAndVerify $Python.URL "$BinaryCache\$FileName.zip" $Python.SHA256
      Expand-ZipFile "$FileName.zip" -ExtractPath "$FileName"
      Write-Success "$ArchName Python $PythonVersion"
    }

    function Install-PIPIfNeeded {
      try {
        Invoke-Program -Silent "$(Get-PythonExecutable)" -m pip
      } catch {
        Invoke-Program -OutNull "$(Get-PythonExecutable)" '-I' -m ensurepip -U --default-pip
      } finally {
        Write-Success "pip"
      }
    }

    function Test-PythonModuleInstalled([string] $ModuleName) {
      try {
        Invoke-Program -Silent "$(Get-PythonExecutable)" -c "import importlib.util, sys; sys.exit(0 if importlib.util.find_spec('$ModuleName') else 1)"
        return $true
      } catch {
        return $false
      }
    }

    function Install-PythonModule([string] $ModuleName) {
      if (Test-PythonModuleInstalled $ModuleName) {
        # Write-Output "$ModuleName already installed."
        return
      }

      $TempRequirementsTxt = New-TemporaryFile

      $Module = $PythonModules[$ModuleName]
      "$ModuleName==$($Module.Version) --hash=`"sha256:$($Module.SHA256)`"" | Out-File -FilePath $TempRequirementsTxt -Append -Encoding utf8
      foreach ($Dependency in $Module.Dependencies) {
        $Module = $PythonModules[$Dependency]
        "$Dependency==$($Dependency.Version) --hash=`"sha256:$($Module.SHA256)`"" | Out-File -FilePath $TempRequirementsTxt -Append -Encoding utf8
      }

      Invoke-Program -OutNull "$(Get-PythonExecutable)" '-I' -m pip install -r $TempRequirementsTxt --require-hashes --no-binary==:all: --disable-pip-version-check

      Write-Success "$ModuleName"
    }

    function Install-PythonModules {
      Install-PIPIfNeeded
      Install-PythonModule "packaging"  # For building LLVM 18+
      Install-PythonModule "setuptools" # Required for SWIG support
      if ($Test -contains "lldb" -or $Test -contains "lldb-swift") {
        Install-PythonModule "psutil"   # Required for testing LLDB
      }
    }

    # Ensure Python modules that are required as host build tools
    Install-Python $HostArchName
    Install-Python $HostArchName $true
    if ($IsCrossCompiling) {
      Install-Python $BuildArchName
      Install-Python $BuildArchName $true
    }
    Install-PythonModules

    if ($SkipBuild -and $SkipPackaging) { return }

    DownloadAndVerify $WiX.URL "$BinaryCache\WiX-$($WiX.Version).zip" $WiX.SHA256
    Expand-ZipFile WiX-$($WiX.Version).zip -ExtractPath WiX-$($WiX.Version)
    Write-Success "WiX $($WiX.Version)"

    if ($SkipBuild) { return }

    DownloadAndVerify $PinnedBuild "$BinaryCache\$PinnedToolchain.exe" $PinnedSHA256

    if ($Test -contains "lldb" -or $Test -contains "lldb-swift") {
      # The make tool isn't part of MSYS
      $GnuWin32MakeURL = "https://downloads.sourceforge.net/project/ezwinports/make-4.4.1-without-guile-w32-bin.zip"
      $GnuWin32MakeHash = "fb66a02b530f7466f6222ce53c0b602c5288e601547a034e4156a512dd895ee7"
      DownloadAndVerify $GnuWin32MakeURL "$BinaryCache\GnuWin32Make-4.4.1.zip" $GnuWin32MakeHash
      Expand-ZipFile GnuWin32Make-4.4.1.zip -ExtractPath GnuWin32Make-4.4.1
      Write-Success "GNUWin32 make 4.4.1"
    }

    # TODO(compnerd) stamp/validate that we need to re-extract
    New-Item -ItemType Directory -ErrorAction Ignore $BinaryCache\toolchains | Out-Null
    Extract-Toolchain "$PinnedToolchain.exe" -ToolchainName $ToolchainVersionIdentifier
    Write-Success "Swift Toolchain $PinnedVersion"

    if ($Android) {
      $NDK = Get-AndroidNDK
      DownloadAndVerify $NDK.URL "$BinaryCache\android-ndk-$AndroidNDKVersion-windows.zip" $NDK.SHA256
      Expand-ZipFile "android-ndk-$AndroidNDKVersion-windows.zip" -ExtractPath "android-ndk-$AndroidNDKVersion" -CreateExtractPath $false
      Write-Success "Android NDK $AndroidNDKVersion"
    }

    if ($IncludeDS2) {
      $WinFlexBisonVersion = "2.5.25"
      $WinFlexBisonURL = "https://github.com/lexxmark/winflexbison/releases/download/v$WinFlexBisonVersion/win_flex_bison-$WinFlexBisonVersion.zip"
      $WinFlexBisonHash = "8D324B62BE33604B2C45AD1DD34AB93D722534448F55A16CA7292DE32B6AC135"
      DownloadAndVerify $WinFlexBisonURL "$BinaryCache\win_flex_bison-$WinFlexBisonVersion.zip" $WinFlexBisonHash

      Expand-ZipFile "win_flex_bison-$WinFlexBisonVersion.zip" -BinaryCache $BinaryCache -ExtractPath "win_flex_bison"
      Write-Success "flex/bison $WinFlexBisonVersion"
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
        $Builds = $WindowsSDKBuilds.Clone()
        if (-not ($HostPlatform -in $Builds)) {
          $Builds += $HostPlatform
        }

        foreach ($Build in $Builds) {
          Invoke-Program nuget install $Package.$($Build.Architecture.ShortName) -Version $WinSDKVersion -OutputDirectory $NugetRoot
          Copy-Directory "$NugetRoot\$Package.$($Build.Architecture.ShortName).$WinSDKVersion\c\*" "$CustomWinSDKRoot\lib\$WinSDKVersion"
        }
      }
    }

    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies took $($Stopwatch.Elapsed)"
    Write-Host ""
  }
}

function Get-PinnedToolchainToolsDir() {
  return [IO.Path]::Combine("$BinaryCache\toolchains", "$ToolchainVersionIdentifier",
    "LocalApp", "Programs", "Swift", "Toolchains", "$PinnedVersion+Asserts",
    "usr", "bin")
}

function Get-PinnedToolchainSDK([OS] $OS = $BuildPlatform.OS, [string] $Identifier = $OS.ToString()) {
  # NOTE: the pinned snapshot ships TWO SDKs side-by-side:
  #   * `<OS>.sdk`             — built with library evolution ON (resilient)
  #   * `<OS>Experimental.sdk` — built with library evolution OFF
  # The single `swiftCore.dll` shipped under `Runtimes/<ver>/usr/bin/` is the
  # resilient one.  Compiler-build callers (whose binaries dynamically load
  # that DLL) MUST link against `<OS>.sdk` — linking against the non-resilient
  # swiftmodule leads to runtime metadata-state mismatches (e.g. `swift_check
  # MetadataState` illegal-instruction faulting on `DecodingError.Context`
  # during plugin JSON message decoding).
  #
  # Static-stdlib clients of pinned (Build-EarlySwiftDriver) opt explicitly
  # into `<OS>Experimental.sdk` and don't load the runtime DLL, so the
  # resilience mismatch doesn't reach them.
  return [IO.Path]::Combine("$BinaryCache\", "toolchains", $ToolchainVersionIdentifier,
    "LocalApp", "Programs", "Swift", "Platforms", $PinnedVersion,
    "$($OS.ToString()).platform", "Developer", "SDKs", "$Identifier.sdk")
}

function Get-PinnedToolchainRuntime() {
  return [IO.Path]::Combine("$BinaryCache\", "toolchains", $ToolchainVersionIdentifier,
    "LocalApp", "Programs", "Swift", "Runtimes", $PinnedVersion,
    "usr", "bin")
}

function Add-KeyValueIfNew([hashtable]$Hashtable, [string]$Key, [string]$Value) {
  if (-not $Hashtable.Contains($Key)) {
    $Hashtable.Add($Key, $Value)
  }
}

function Add-FlagsDefine([hashtable]$Defines, [string]$Name, [string[]]$Value) {
  $Value = @($Value | Where-Object { $null -ne $_ })
  if ($Value.Count -eq 0) {
    return
  }

  if ($Defines.Contains($Name)) {
    $Defines[$name] = @($Defines[$name] | Where-Object { $null -ne $_ }) + $Value
  } else {
    $Defines.Add($Name, $Value)
  }
}

function Get-PlatformRoot([OS] $OS) {
  return ([IO.Path]::Combine((Get-InstallDir $HostPlatform), "Platforms", "$($OS.ToString()).platform"))
}

function Get-SwiftSDK([OS] $OS, [string] $Identifier = $OS.ToString()) {
  return ([IO.Path]::Combine((Get-PlatformRoot $OS), "Developer", "SDKs", "$Identifier.sdk"))
}

function Get-SDKRuntimeBin([Hashtable] $Platform, [string] $SDKRoot, [bool] $InstallRuntimeToStage = $true) {
  # The Windows runtime is installed beside the architecture-specific toolchain
  # image for staged SDK builds. Bootstrap and non-Windows SDKs keep their
  # runtime payloads in the SDK root.
  if ($Platform.OS -eq [OS]::Windows -and $InstallRuntimeToStage) {
    return Get-WindowsRuntimeBin $Platform
  }
  return [IO.Path]::Combine($SDKRoot, "usr", "bin")
}

function Get-SDKLibexecDir([Hashtable] $Platform, [string] $SDKRoot, [bool] $InstallRuntimeToStage = $true) {
  # See Get-SDKRuntimeBin for the Windows-only install layout split.
  if ($Platform.OS -eq [OS]::Windows -and $InstallRuntimeToStage) {
    return Get-WindowsRuntimeLibexec $Platform
  }
  return [IO.Path]::Combine($SDKRoot, "usr", "libexec")
}

function Resolve-SDKRuntimeBin([Hashtable] $Platform, [string] $SDKRoot, [bool] $InstallRuntimeToStage = $true) {
  $RuntimeBin = Get-SDKRuntimeBin $Platform $SDKRoot $InstallRuntimeToStage
  if (Test-Path $RuntimeBin -PathType Container) {
    return $RuntimeBin
  }
  return [IO.Path]::Combine($SDKRoot, "usr", "bin")
}

enum DriverStyle {
  CL
  ClangCL
  GNU
  Swift
}

# Compiler Configurations
$Compilers = @{
  MSVC = @{
    C = @{
      Executable        = "cl.exe"
      DriverStyle       = [DriverStyle]::CL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline", "/Zc:preprocessor")
      DebugFlags        = { param([string] $Format)
        @()
      }
      AssumeFunctional  = $false
    }
    CXX = @{
      Executable        = "cl.exe"
      DriverStyle       = [DriverStyle]::CL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline", "/Zc:preprocessor", "/Zc:__cplusplus")
      DebugFlags        = { param([string] $Format)
        @()
      }
      AssumeFunctional  = $false
    }
  }

  Pinned = @{
    C = @{
      Executable        = Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath "clang-cl.exe"
      DriverStyle       = [DriverStyle]::ClangCL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @() }
      }
      AssumeFunctional  = $false
    }

    CXX = @{
      Executable        = Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath "clang-cl.exe"
      DriverStyle       = [DriverStyle]::ClangCL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline", "/Zc:__cplusplus")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @() }
      }
      AssumeFunctional  = $false
    }

    Swift = @{
      Executable        = Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath "swiftc.exe"
      DriverStyle       = [DriverStyle]::Swift
      Flags             = @()
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") {
          return @("-g", "-debug-info-format=dwarf", "-use-ld=lld-link", "-Xlinker", "/DEBUG:DWARF")
        }
        return @("-g", "-debug-info-format=codeview", "-Xlinker", "/DEBUG")
      }
      AssumeFunctional  = $false
    }
  }

  Built = @{
    C = @{
      Executable        = [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "clang-cl.exe")
      DriverStyle       = [DriverStyle]::ClangCL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @() }
      }
      AssumeFunctional  = $true
    }

    CXX = @{
      Executable        = [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "clang-cl.exe")
      DriverStyle       = [DriverStyle]::ClangCL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline", "/Zc:__cplusplus")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @() }
      }
      AssumeFunctional  = $true
    }

    GNUC = @{
      Executable        = [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "clang.exe")
      DriverStyle       = [DriverStyle]::GNU
      Flags             = @("-fno-stack-protector", "-ffunction-sections", "-fdata-sections", "-fomit-frame-pointer", "-finline-functions")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-gdwarf") } else { @("-gcodeview") }
      }
      AssumeFunctional  = $true
    }

    GNUCXX = @{
      Executable        = [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "clang++.exe")
      DriverStyle       = [DriverStyle]::GNU
      Flags             = @("-fno-stack-protector", "-ffunction-sections", "-fdata-sections", "-fomit-frame-pointer", "-finline-functions")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-gdwarf") } else { @("-gcodeview") }
      }
      AssumeFunctional  = $true
    }

    Swift = @{
      Executable        = [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "swiftc.exe")
      DriverStyle       = [DriverStyle]::Swift
      Flags             = @()
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") {
          return @("-g", "-debug-info-format=dwarf", "-use-ld=lld-link", "-Xlinker", "/DEBUG:DWARF")
        }
        return @("-g", "-debug-info-format=codeview", "-Xlinker", "/DEBUG")
      }
      AssumeFunctional  = $true
    }
  }

  Stage0 = @{
    C = @{
      Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage0Compilers), "clang-cl.exe")
      DriverStyle       = [DriverStyle]::ClangCL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @() }
      }
      AssumeFunctional  = $true
    }

    CXX = @{
      Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage0Compilers), "clang-cl.exe")
      DriverStyle       = [DriverStyle]::ClangCL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline", "/Zc:__cplusplus")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @() }
      }
      AssumeFunctional  = $true
    }

    GNUC = @{
      Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage0Compilers), "clang.exe")
      DriverStyle       = [DriverStyle]::GNU
      Flags             = @("-fno-stack-protector", "-ffunction-sections", "-fdata-sections", "-fomit-frame-pointer", "-finline-functions")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-gdwarf") } else { @("-gcodeview") }
      }
      AssumeFunctional  = $true
    }

    GNUCXX = @{
      Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage0Compilers), "clang++.exe")
      DriverStyle       = [DriverStyle]::GNU
      Flags             = @("-fno-stack-protector", "-ffunction-sections", "-fdata-sections", "-fomit-frame-pointer", "-finline-functions")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-gdwarf") } else { @("-gcodeview") }
      }
      AssumeFunctional  = $true
    }

    Swift = @{
      Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage0Compilers), "swiftc.exe")
      DriverStyle       = [DriverStyle]::Swift
      Flags             = @()
      DebugFlags        = { param([string] $Format)
        if ($Format -eq $null) { return @("-gnone") }
        if ($Format -eq "dwarf") {
          return @("-g", "-debug-info-format=dwarf", "-use-ld=lld-link", "-Xlinker", "/DEBUG:DWARF")
        }
        return @("-g", "-debug-info-format=codeview", "-Xlinker", "/DEBUG")
      }
      AssumeFunctional  = $true
    }
  }

  Stage1 = @{
    C = @{
      Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage1Compilers), "clang-cl.exe")
      DriverStyle       = [DriverStyle]::ClangCL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @() }
      }
      AssumeFunctional  = $true
    }

    CXX = @{
      Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage1Compilers), "clang-cl.exe")
      DriverStyle       = [DriverStyle]::ClangCL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline", "/Zc:__cplusplus")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @() }
      }
      AssumeFunctional  = $true
    }

    GNUC = @{
      Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage1Compilers), "clang.exe")
      DriverStyle       = [DriverStyle]::GNU
      Flags             = @("-fno-stack-protector", "-ffunction-sections", "-fdata-sections", "-fomit-frame-pointer", "-finline-functions")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-gdwarf") } else { @("-gcodeview") }
      }
      AssumeFunctional  = $true
    }

    GNUCXX = @{
      Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage1Compilers), "clang++.exe")
      DriverStyle       = [DriverStyle]::GNU
      Flags             = @("-fno-stack-protector", "-ffunction-sections", "-fdata-sections", "-fomit-frame-pointer", "-finline-functions")
      DebugFlags        = { param([string] $Format)
        if ($Format -eq "dwarf") { @("-gdwarf") } else { @("-gcodeview") }
      }
      AssumeFunctional  = $true
    }

    Swift = @{
      Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage1Compilers), "swiftc.exe")
      DriverStyle       = [DriverStyle]::Swift
      Flags             = @()
      DebugFlags        = { param([string] $Format)
        if ($Format -eq $null) { return @("-gnone") }
        if ($Format -eq "dwarf") {
          return @("-g", "-debug-info-format=dwarf", "-use-ld=lld-link", "-Xlinker", "/DEBUG:DWARF")
        }
        return @("-g", "-debug-info-format=codeview", "-Xlinker", "/DEBUG")
      }
      AssumeFunctional  = $true
    }
  }
}

$Compilers.Host = @{
  C = if ($UseHostToolchain) { $Compilers.MSVC.C } else { $Compilers.Pinned.C }
  CXX = if ($UseHostToolchain) { $Compilers.MSVC.CXX } else { $Compilers.Pinned.CXX }
}

$Assemblers = @{
  Built = @{
    Executable        = [IO.Path]::Combine((Get-ProjectBinaryCache $BuildPlatform Compilers), "bin", "clang-cl.exe")
    DriverStyle       = [DriverStyle]::ClangCL
    Flags             = @()
    DebugFlags        = { param([string] $Format)
      if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @("-clang:-gcodeview") }
    }
    AssumeFunctional  = $true
  }

  Pinned = @{
    Executable        = Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath "clang-cl.exe"
    DriverStyle       = [DriverStyle]::ClangCL
    Flags             = @()
    DebugFlags        = { param([string] $Format)
      if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @("-clang:-gcodeview") }
    }
    AssumeFunctional  = $false
  }

  Stage0 = @{
    Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage0Compilers), "clang-cl.exe")
    DriverStyle       = [DriverStyle]::ClangCL
    Flags             = @()
    DebugFlags        = { param([string] $Format)
      if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @("-clang:-gcodeview") }
    }
    AssumeFunctional  = $true
  }

  Stage1 = @{
    Executable        = [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage1Compilers), "clang-cl.exe")
    DriverStyle       = [DriverStyle]::ClangCL
    Flags             = @()
    DebugFlags        = { param([string] $Format)
      if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @("-clang:-gcodeview") }
    }
    AssumeFunctional  = $true
  }
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
    [Hashtable] $Assembler = $null,
    [Hashtable] $CCompiler = $null,
    [Hashtable] $CXXCompiler = $null,
    [Hashtable] $SwiftCompiler = $null,
    [switch] $UseASMMASM = $false,
    [switch] $AddAndroidCMakeEnv = $false,
    [string] $SwiftSDK = $null,
    [hashtable] $Defines = @{}, # Values are either single strings or arrays of flags
    [string[]] $BuildTargets = @()
  )

  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Building '$Src' to '$Bin' ..."

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

    $UseASM = $Assembler -ne $null
    $UseASM_MASM = [bool]$UseASMMASM
    $UseC = $CCompiler -ne $null
    $UseCXX = $CXXCompiler -ne $null
    $UseSwift = $SwiftCompiler -ne $null
    $UseMSVC = ($UseC -and $CCompiler.DriverStyle -eq [DriverStyle]::CL) -or ($UseCXX -and $CXXCompiler.DriverStyle -eq [DriverStyle]::CL)

    # Starting with CMake 3.30, CMake propagates linker flags to Swift.
    $CMakePassesSwiftLinkerFlags = $CMakeVersion -ge [version]'3.30'
    # CMP0181 enables support for the `LINKER:flag1,flag2,...` syntax in
    # `CMAKE_[EXE|SHARED|MODULE]_LINKER_FLAGS[_<CONFIG>]` variables.
    $CMakeSupportsCMP0181 = $CMakeVersion -ge [version]'4.0'

    # We need to manually prefix linker flags with `-Xlinker` if we are using
    # the GNU driver or if Swift is used as the linker driver.
    # This is not necessary with CMake 4.0+ as CMP0181 simplifies the handling
    # of linker arguments.
    enum LinkerFlagHandling {
      CMP0181
      XLinkerPrefix
      None
    }
    # Whether CMake invokes the linker directly with MSVC-style flags (true
    # for both `cl.exe` and `clang-cl.exe` since CMake detects clang-cl as
    # MSVC-like and bypasses the compiler driver for the link step).
    $UsesDirectMSVCLinker =
      ($UseC   -and $CCompiler.DriverStyle   -in @([DriverStyle]::CL, [DriverStyle]::ClangCL)) -or
      ($UseCXX -and $CXXCompiler.DriverStyle -in @([DriverStyle]::CL, [DriverStyle]::ClangCL))
    $FlagHandling = if ($CMakeSupportsCMP0181) {
      # With CMP0181, the `LINKER:` generator expression can always be used.
      [LinkerFlagHandling]::CMP0181
    } elseif ($UsesDirectMSVCLinker) {
      # `link.exe` / `lld-link.exe` invoked directly does not understand the
      # `-Xlinker` prefix.  MSVC-style flags (`/INCREMENTAL:NO`, etc.) pass
      # through verbatim.
      [LinkerFlagHandling]::None
    } else {
      # Otherwise, we are probably using clang and/or swift as the link
      # driver; prefix the linker flags with `-Xlinker`.
      [LinkerFlagHandling]::XLinkerPrefix
    }

    # Helper cmdlet to add linker flags with the appropriate handling based on
    # the linker driver and CMake version.
    function Convert-LinkerFlags([string[]]$Value) {
      switch ($FlagHandling) {
        CMP0181 {
          $Value | ForEach-Object { "LINKER:$_" }
        }
        XLinkerPrefix {
          $NewValue = @()
          foreach ($Flag in $Value) {
            $NewValue += "-Xlinker"
            $NewValue += $Flag
          }
          $NewValue
        }
        None {
          $Value
        }
      }
    }

    function Add-LinkerFlagsDefine([hashtable]$Defines, [string[]]$Value) {
      $Value = Convert-LinkerFlags $Value
      Add-FlagsDefine $Defines CMAKE_EXE_LINKER_FLAGS $Value
      Add-FlagsDefine $Defines CMAKE_SHARED_LINKER_FLAGS $Value
    }

    function Add-SharedLinkerFlagsDefine([hashtable]$Defines, [string[]]$Value) {
      $Value = Convert-LinkerFlags $Value
      Add-FlagsDefine $Defines CMAKE_SHARED_LINKER_FLAGS $Value
      Add-FlagsDefine $Defines CMAKE_MODULE_LINKER_FLAGS $Value
    }

    # Add additional defines (unless already present)
    $Defines = $Defines.Clone()

    # Always enable CMP0181 if available.
    if ($CMakeSupportsCMP0181) {
      Add-KeyValueIfNew $Defines CMAKE_POLICY_DEFAULT_CMP0181 NEW
    }

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
          Add-KeyValueIfNew $Defines CMAKE_ASM_COMPILER $Assembler.Executable
          Add-KeyValueIfNew $Defines CMAKE_ASM_FLAGS @("--target=$($Platform.Triple)")
          Add-KeyValueIfNew $Defines CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDLL "/MD"

          if ($DebugInfo) {
            # CMake's MSVC_DEBUG_INFORMATION_FORMAT support also applies to ASM
            # targets, but clang-cl-as-ASM does not get a built-in mapping for
            # the Embedded format. Provide the mapping before setting the global
            # CMAKE_MSVC_DEBUG_INFORMATION_FORMAT below.
            Add-FlagsDefine $Defines CMAKE_ASM_COMPILE_OPTIONS_MSVC_DEBUG_INFORMATION_FORMAT_Embedded `
              $(& $Assembler.DebugFlags $DebugFormat)
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
          Add-KeyValueIfNew $Defines CMAKE_C_COMPILER $CCompiler.Executable
          Add-KeyValueIfNew $Defines CMAKE_C_COMPILER_TARGET $Platform.Triple
          Add-FlagsDefine $Defines CMAKE_C_FLAGS $CCompiler.Flags

          if ($DebugInfo) {
            Add-FlagsDefine $Defines CMAKE_C_FLAGS $(& $CCompiler.DebugFlags $DebugFormat)
          }
        }

        if ($UseCXX) {
          Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER $CXXCompiler.Executable
          Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER_TARGET $Platform.Triple
          Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXCompiler.Flags

          # With clang-cl, CMake generates MSVC-style archive rules (/nologo /out:...).
          # If llvm-lib.exe is not found next to clang-cl.exe, CMake can fall back to
          # whatever 'ar' is on PATH (e.g. the installed toolchain's POSIX ar.exe),
          # causing a flags mismatch.  Explicitly pin CMAKE_AR to llvm-lib.exe from
          # the compiler bin directory so the right tool is always selected.
          if ($CXXCompiler.DriverStyle -eq [DriverStyle]::ClangCL) {
            $librarian = [IO.Path]::Combine([IO.Path]::GetDirectoryName($CXXCompiler.Executable), "llvm-lib.exe")
            if (Test-Path $librarian) {
              Add-KeyValueIfNew $Defines CMAKE_AR $librarian
            }
          }

          if ($DebugInfo) {
            Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $(& $CXXCompiler.DebugFlags $DebugFormat)
          }
        }

        if ($UseSwift) {
          if ($SwiftCompiler.AssumeFunctional) {
            Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_WORKS "YES"
          }

          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER $SwiftCompiler.Executable
          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_TARGET $Platform.Triple
          # Skip compiler ID detection: avoids compiling+scanning a multi-MB test binary on every configure.
          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_ID "Apple"

          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftCompiler.Flags
          if ($SwiftSDK) {
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-sdk", $SwiftSDK)
          }
          if ($DebugInfo) {
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $(& $SwiftCompiler.DebugFlags $DebugFormat)
          } else {
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-gnone")
          }

          if ($CMakePassesSwiftLinkerFlags) {
            # CMake 3.30+ passes all linker flags to Swift as the linker driver,
            # including those from the internal CMake modules files, without
            # a `-Xlinker` prefix. This causes build failures as Swift cannot
            # parse linker flags.
            # Overwrite the release linker flags to be empty to avoid this.
            Add-KeyValueIfNew $Defines CMAKE_EXE_LINKER_FLAGS_RELEASE ""
            Add-KeyValueIfNew $Defines CMAKE_SHARED_LINKER_FLAGS_RELEASE ""
          } else {
            # Disable EnC as that introduces padding in the conformance tables
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-Xlinker", "/INCREMENTAL:NO")
            # Swift requires COMDAT folding and de-duplication
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-Xlinker", "/OPT:REF", "-Xlinker", "/OPT:ICF")
          }

          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftCompiler.Flags
          # Workaround CMake 3.26+ enabling `-wmo` by default on release builds
          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELEASE "-O"
          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELWITHDEBINFO "-O"
        }

        Add-LinkerFlagsDefine $Defines @("/INCREMENTAL:NO", "/OPT:REF", "/OPT:ICF")
        Add-SharedLinkerFlagsDefine $Defines @("/MANIFEST:NO")
        Add-KeyValueIfNew $Defines CMAKE_USER_MAKE_RULES_OVERRIDE `
          "$SourceCache\swift\utils\windows-clang-overrides.cmake"

        if ($DebugInfo) {
          if ($UseASM -or $UseC -or $UseCXX) {
            # Prefer `/Z7` over `/ZI`
            # By setting the debug information format, the appropriate C/C++
            # flags will be set for codeview debug information format so there
            # is no need to set them explicitly above.
            Add-KeyValueIfNew $Defines CMAKE_MSVC_DEBUG_INFORMATION_FORMAT Embedded
            Add-KeyValueIfNew $Defines CMAKE_POLICY_DEFAULT_CMP0141 NEW

            Add-LinkerFlagsDefine $Defines @("/DEBUG")

            # The linker flags are shared across every language, and `/IGNORE:longsections` is an
            # `lld-link.exe` argument, not `link.exe`, so this can only be enabled when we use
            # `lld-link.exe` for linking.
            # TODO: Investigate supporting fission with PE/COFF, this should avoid this warning.
            if ($DebugFormat -eq "dwarf" -and -not $UseMSVC) {
              Add-LinkerFlagsDefine $Defines @("/IGNORE:longsections")
            }
          }
        }
      }

      Android {
        $AndroidNDKPath = Get-AndroidNDKPath
        $AndroidPrebuiltRoot = "$AndroidNDKPath\toolchains\llvm\prebuilt\$($BuildPlatform.OS.ToString().ToLowerInvariant())-$($BuildPlatform.Architecture.LLVMName)"
        $AndroidSysroot = "$AndroidPrebuiltRoot\sysroot"

        Add-KeyValueIfNew $Defines CMAKE_ANDROID_API "$AndroidAPILevel"
        Add-KeyValueIfNew $Defines CMAKE_ANDROID_ARCH_ABI $Platform.Architecture.ABI
        Add-KeyValueIfNew $Defines CMAKE_ANDROID_NDK "$AndroidNDKPath"
        Add-KeyValueIfNew $Defines CMAKE_SYSROOT "$AndroidSysroot"

        if ($UseASM) {
        }

        if ($UseC) {
          Add-KeyValueIfNew $Defines CMAKE_C_COMPILER_TARGET $Platform.Triple
          Add-FlagsDefine $Defines CMAKE_C_FLAGS $CCompiler.Flags
          if ($DebugInfo) {
            Add-FlagsDefine $Defines CMAKE_C_FLAGS $(& $CCompiler.DebugFlags $DebugFormat)
          }
        }

        if ($UseCXX) {
          Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER_TARGET $Platform.Triple
          Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXCompiler.Flags
          if ($DebugInfo) {
            Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $(& $CXXCompiler.DebugFlags $DebugFormat)
          }
        }

        if ($UseSwift) {
          if ($SwiftCompiler.AssumeFunctional) {
            Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_WORKS "YES"
          }

          # FIXME(compnerd) remove this once the old runtimes build path is removed.
          Add-KeyValueIfNew $Defines SWIFT_ANDROID_NDK_PATH "$AndroidNDKPath"

          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER $SwiftCompiler.Executable
          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_TARGET $Platform.Triple
          # Skip compiler ID detection: avoids compiling+scanning a multi-MB test binary on every configure.
          Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_ID "Apple"

          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftCompiler.Flags
          if ($SwiftSDK) {
            # TODO: CMake does not yet have support for passing `CMAKE_SYSROOT`
            # to the Swift compiler yet.  Once we have that, we can drop
            # `-sysroot $AndroidSysroot` here.
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-sdk", $SwiftSDK, "-sysroot", $AndroidSysroot)
          }

          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @(
            "-Xclang-linker", "-target", "-Xclang-linker", $Platform.Triple,
            "-Xclang-linker", "--sysroot", "-Xclang-linker", $AndroidSysroot,
            "-Xclang-linker", "-resource-dir", "-Xclang-linker", "${AndroidPrebuiltRoot}\lib\clang\$($(Get-AndroidNDK).ClangVersion)"
          )

          if ($DebugInfo) {
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS (& $SwiftCompiler.DebugFlags $DebugFormat)
          } else {
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-gnone")
          }

          # Workaround CMake 3.26+ enabling `-wmo` by default on release builds
          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELEASE "-O"
          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELWITHDEBINFO "-O"
        }

        if (($UseASM -and $Assembler.AssumeFunctional) -or ($UseC -and $CCompiler.AssumeFunctional) -or ($UseCXX -and $CXXCompiler.AssumeFunctional)) {
          # Use a built lld linker as the Android's NDK linker might be too old
          # and not support all required relocations needed by the Swift
          # runtime.
          $Executable = if ($UseC) {
            $CCompiler.Executable
          } elseif ($UseCXX) {
            $CXXCompiler.Executable
          } elseif ($UseASM) {
            $Assembler.Executable
          }
          $ld = Join-Path -Path (Split-Path $Executable) -ChildPath "ld.lld"
          if ($UseSwift) {
            # The Android NDK injects `-Wl,<arg>` flags into
            # `CMAKE_*_LINKER_FLAGS` via `CMAKE_*_LINKER_FLAGS_INIT` variables.
            # CMake 3.30+ passes these to the Swift driver, which doesn't
            # understand the `-Wl,` syntax. Pre-set the flags in a portable form
            # (`-Xlinker <arg>`) from the command line as this takes precedence
            # over the NDK's `*_INIT` mechanism.
            #
            # `--ld-path` and `-Qunused-arguments` are Clang driver flags,
            # handled via `CMAKE_PROJECT_INCLUDE` with a `LINK_LANGUAGE` guard,
            # so they do not affect Swift linking.
            $AndroidLinkerFlags = @(
              "-Xlinker", "--build-id=sha1",
              "-Xlinker", "--no-rosegment",
              "-Xlinker", "--no-undefined-version",
              "-Xlinker", "--fatal-warnings",
              "-Xlinker", "--gc-sections",
              "-Xlinker", "--no-undefined"
            )
            Add-FlagsDefine $Defines CMAKE_SHARED_LINKER_FLAGS $AndroidLinkerFlags
            Add-FlagsDefine $Defines CMAKE_EXE_LINKER_FLAGS ($AndroidLinkerFlags + @("-Xlinker", "--gc-sections"))
            Add-FlagsDefine $Defines CMAKE_MODULE_LINKER_FLAGS $AndroidLinkerFlags
            Add-KeyValueIfNew $Defines SWIFT_ANDROID_LD_PATH $ld
            Add-KeyValueIfNew $Defines CMAKE_PROJECT_INCLUDE "$SourceCache\swift\utils\android-overrides.cmake"
          } else {
            # Clang Runtime explicitly sets linker flags for every target,
            # making the `add_link_options()` approach via
            # `CMAKE_PROJECT_INCLUDE` not viable. Since the problem that the
            # above block aims to solve only concerns projects that use Swift,
            # we can get away with just overriding `CMAKE_*_LINKER_FLAGS` for
            # non-Swift projects.
            Add-FlagsDefine $Defines CMAKE_SHARED_LINKER_FLAGS "--ld-path=$ld"
            Add-FlagsDefine $Defines CMAKE_EXE_LINKER_FLAGS "--ld-path=$ld"
          }
        }

        # TODO(compnerd) we should understand why CMake does not understand
        # that the object file format is ELF when targeting Android on Windows.
        # This indication allows it to understand that it can use `chrpath` to
        # change the RPATH on the dynamic libraries.
        Add-FlagsDefine $Defines CMAKE_EXECUTABLE_FORMAT "ELF"
      }
    }

    if ($EnableCaching) {
      $env:LLVM_CACHE_CAS_PATH = "$Cache"

      # Skip the clang-cache launcher when targeting Android: cmake auto-detects
      # the NDK's clang (e.g. 19.x) as the actual compiler, but the launcher
      # we'd point at lives under Stage1Compilers (a newer LLVM, e.g. 21.x).
      # The version skew makes the in-process dep scanner look for builtin
      # headers (`stddef.h`, `float.h`, ...) under the wrong resource-dir
      # layout and fail with "CAS-based dependency scan failed: failed to get
      # include-tree".  Compiler-rt builds for Android are fast enough without
      # the launcher.
      $LauncherSafe = ($Platform.OS -ne [OS]::Android)

      if ($LauncherSafe -and $UseC -and $CCompiler.DriverStyle -ne [DriverStyle]::CL) {
        Add-KeyValueIfNew $Defines CMAKE_C_COMPILER_LAUNCHER `
            (Join-Path -Path (Split-Path $CCompiler.Executable) -ChildPath "clang-cache.exe")
      }

      if ($LauncherSafe -and $UseCXX -and $CXXCompiler.DriverStyle -ne [DriverStyle]::CL) {
        Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER_LAUNCHER `
            (Join-Path -Path (Split-Path $CXXCompiler.Executable) -ChildPath "clang-cache.exe")
      }

      if ($UseSwift) {
        Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @(
          "-explicit-module-build",
          "-cache-compile-job",
          "-cas-path", $Cache,
          "-incremental-dependency-scan"
        )
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

    Write-Host "$cmake $cmakeGenerateArgs"
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

  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Finished building '$Src' to '$Bin' in $($Stopwatch.Elapsed)"
  Write-Host ""
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
    $HostRuntimeBin = Resolve-SDKRuntimeBin $HostPlatform $HostSDKRoot
    $env:Path = "$HostRuntimeBin;$($HostPlatform.ToolchainInstallRoot)\usr\bin;${env:Path}"
    $env:SDKROOT = $HostSDKRoot
    $env:SWIFTCI_USE_LOCAL_DEPS = "1"

    $Arguments = @(
      "--scratch-path", $Bin,
      "--package-path", $Src,
      "-c", $Configuration
    )
    if ($DebugInfo) {
      if ($Platform.OS -eq [OS]::Windows -and $DebugFormat -eq "codeview") {
        $Arguments += @("-debug-info-format", "codeview")
      } else {
        $Arguments += @("-debug-info-format", "dwarf")
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

  Write-Host "$msbuild $MSBuildArgs"
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

  $RuntimeFiles = @(Get-ChildItem -Path $RuntimeLocation -File | Sort-Object Name)
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
    -Assembler $(if ($UseHostToolchain) { $null } else { $Assemblers.Pinned }) `
    -UseASMMASM:$UseHostToolchain `
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
  # `-static-stdlib` requires static-stdlib content under `lib/swift_static/`,
  # which only the experimental SDK in pinned ships; the resilient `Windows
  # .sdk` only has the dynamic stdlib.  Hence the explicit `-Identifier`.
  Invoke-IsolatingEnvVars {
    $env:Path = "$(Get-PinnedToolchainRuntime);${env:Path}"
    Build-CMakeProject `
      -Src $SourceCache\swift-driver `
      -Bin (Get-ProjectBinaryCache $Platform EarlySwiftDriver) `
      -Platform $Platform `
      -CCompiler $Compilers.Pinned.C `
      -CXXCompiler $Compilers.Pinned.CXX `
      -SwiftCompiler $Compilers.Pinned.Swift `
      -SwiftSDK (Get-PinnedToolchainSDK -OS $Platform.OS -Identifier "$($Platform.OS)Experimental") `
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

        # Prevent re-cloning the soruces
        FETCHCONTENT_SOURCE_DIR_ARGUMENTPARSER = "$SourceCache\swift-argument-parser";
        FETCHCONTENT_SOURCE_DIR_LLBUILD = "$SourceCache\llbuild";
        FETCHCONTENT_SOURCE_DIR_TOOLSSUPPORTCORE = "$SourceCache\swift-tools-support-core";
      }
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
    -Defines @{
      BUILD_SHARED_LIBS = $(if ($Static) { "NO" } else { "YES" });
      BUILD_TESTING = "NO";
      ENABLE_SWIFT = "NO";
    }
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
    LLDB_TEST_MAKE = "$BinaryCache\GnuWin32Make-4.4.1\bin\make.exe";
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
                         [Hashtable] $CCompiler        = $Compilers.Host.C,
                         [Hashtable] $CXXCompiler      = $Compilers.Host.CXX,
                         [Hashtable] $SwiftCompiler    = $Compilers.Pinned.Swift,
                         [string]    $SwiftSDK         = (Get-PinnedToolchainSDK -OS $Platform.OS),
                         [string]    $ToolchainRoot    = "$(Get-InstallDir $Platform)\Toolchains\$ProductVersion+$Variant",
                         [string]    $RuntimeLocation  = $null,
                         [string]    $DispatchPackage  = $null,
                         [string]    $CacheScript      = "$SourceCache\swift\cmake\caches\Windows-$($Platform.Architecture.LLVMName).cmake") {
  New-Item -ItemType Directory -Path $BinaryCache\$($HostPlatform.Triple) -ErrorAction Ignore | Out-Null
  New-Item -ItemType SymbolicLink -Path "$BinaryCache\$($HostPlatform.Triple)\compilers" -Target "$BinaryCache\5" -ErrorAction Ignore | Out-Null

  Invoke-IsolatingEnvVars {
    if ($SwiftCompiler -and $SwiftCompiler.Executable -eq $Compilers.Pinned.Swift.Executable) {
      $env:Path = "$(Get-PinnedToolchainRuntime);${env:Path}"
    }

    Build-CMakeProject `
      -Src $SourceCache\llvm-project\llvm `
      -Bin (Get-ProjectBinaryCache $Platform $Project) `
      -InstallTo "$ToolchainRoot\usr" `
      -Platform $Platform `
      -CCompiler $CCompiler `
      -CXXCompiler $CXXCompiler `
      -SwiftCompiler $SwiftCompiler `
      -SwiftSDK $SwiftSDK `
      -BuildTargets @("install-distribution") `
      -CacheScript $CacheScript `
      -Defines (Get-CompilersDefines $Platform $Variant -SwiftSDK $SwiftSDK -DispatchPackage $DispatchPackage)
  }

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

function Get-WindowsSxSRuntimeDLLs([string] $RuntimeSourceDir) {
  $DeveloperDLLs = [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase
  )
  foreach ($DLL in @(
    "Testing",
    "_Testing_Foundation",
    "_Testing_WinSDK",
    "_TestingInterop",
    "XCTest"
  )) {
    [void]$DeveloperDLLs.Add($DLL)
  }

  Get-ChildItem -Path $RuntimeSourceDir -Filter "*.dll" -File |
    Where-Object {
      -not $DeveloperDLLs.Contains([IO.Path]::GetFileNameWithoutExtension($_.Name))
    } |
    Sort-Object Name
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
    [Parameter(Mandatory)] [string]   $ProcessorArchitecture
  )

  if (-not (Test-Path $BinaryDir)) {
    throw "Set-WindowsSxSToolchainRuntime: BinaryDir '$BinaryDir' does not exist"
  }
  if (-not (Test-Path $RuntimeSourceDir)) {
    # -SkipBuild may run before the runtime is installed.
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
  <assemblyIdentity type="win32" name="$AssemblyName" version="$AssemblyVersion"
      processorArchitecture="$ProcessorArchitecture"/>
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
    [Parameter(Mandatory)] [string]    $ProcessorArchitecture
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
        New-WindowsManifestDependency $DLLName $AssemblyVersion $ProcessorArchitecture
      }
    )
    Set-WindowsExecutableManifestDependencies $ToolPath $Dependencies "Set-WindowsSxSToolchainRuntimePerDLL"
    $BoundEXECount++
  }

  Write-Host "Set-WindowsSxSToolchainRuntimePerDLL: bound $BoundEXECount EXE(s); skipped $SkippedCount EXE(s) with no runtime imports"
}

function Test-Compilers([Hashtable] $Platform, [string] $Variant, [switch] $TestClang, [switch] $TestLLD, [switch] $TestLLDB, [switch] $TestLLDBSwift, [switch] $TestLLVM, [switch] $TestSwift) {
  Invoke-IsolatingEnvVars {
    $SwiftSDK = Get-SwiftSDK -OS $Platform.OS
    $SwiftRuntime = Resolve-SDKRuntimeBin $Platform $SwiftSDK
    $Stage2BinDir = [IO.Path]::Combine((Get-ProjectBinaryCache $Platform Stage2Compilers), "bin")
    $CDispatchBinaryCache = Get-ProjectBinaryCache $Platform DynamicCDispatch
    $env:Path = "$Stage2BinDir;$CDispatchBinaryCache;$CDispatchBinaryCache\bin;$(Get-CMarkBinaryCache $Platform)\src;$env:Path;$VSInstallRoot\DIA SDK\bin\$($HostPlatform.Architecture.VSName);$UnixToolsBinDir"
    $env:PYTHONUTF8 = "1"
    $TestingDefines = Get-CompilersDefines $Platform $Variant -Test -SwiftSDK $SwiftSDK
    # Stage2 tests must use the freshly-built native tools, not Stage1.
    $TestingDefines["SWIFT_NATIVE_SWIFT_TOOLS_PATH"] = $Stage2BinDir
    # check-swift depends on swift-backtrace-${SDK}; build the libexec helpers
    # with the rest of the test targets.
    $TestingDefines["SWIFT_BUILD_LIBEXEC"] = "YES"
    # Keep %host-build-swift on the same platform SDK that Stage2 uses.
    $TestingDefines["SWIFT_HOST_SDKROOT"] = $SwiftSDK
    if ($TestLLVM) { $Targets += @("check-llvm") }
    if ($TestClang) { $Targets += @("check-clang") }
    if ($TestLLD) { $Targets += @("check-lld") }
    if ($TestSwift) {
      $Targets += @("SwiftCompilerPlugin", "check-swift")
    }
    if ($TestLLDB) { $Targets += @("check-lldb") }
    if ($TestLLDBSwift) { $Targets += @("check-lldb-swift") }
    if ($TestLLDB -or $TestLLDBSwift) {
      # Override test filter for known issues in downstream LLDB
      Load-LitTestOverrides ([IO.Path]::GetFullPath([IO.Path]::Combine($PSScriptRoot, "..", "..", "llvm-project", "lldb", "test", "windows-swift-llvm-lit-test-overrides.txt")))

      $CompilerCache = Get-ProjectBinaryCache $Platform Stage2Compilers
      $SwiftRTSubdir = "lib\swift\windows"

      # Transitive dependency of _lldb.pyd
      Copy-Item `
        -Path (Join-Path $SwiftRuntime "swiftCore.dll") `
        -Destination "$CompilerCache\lib\site-packages\lldb"

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
        # gtest sharding breaks llvm-lit's --xfail and LIT_XFAIL inputs: https://github.com/llvm/llvm-project/issues/102264
        LLVM_LIT_ARGS = "-v --no-gtest-sharding --time-tests";
        # LLDB Unit tests link against this library
        LLVM_UNITTEST_LINK_FLAGS = "$SwiftSDK\usr\lib\swift\windows\$($Platform.Architecture.LLVMName)\swiftCore.lib";
      }
    }

    if (-not $Targets) {
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
      "swift-ide-test",
      "swift-plugin-server"
    )

    # Prefer the platform SDK runtime.  The pre-staged bundle fallback keeps
    # -SkipBuild usable when the install image is absent.
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
    if (-not $RuntimeSource) { $RuntimeSource = $RuntimeSourceCandidates[0] }

    Invoke-IsolatingEnvVars {
      # Test-time tools execute on the build host.
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
                                   "swift-ide-test.exe",
                                   "swift-plugin-server.exe",
                                   "swiftc-legacy-driver.exe"
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
    $Targets = @("swift-test-stdlib") + $Targets

    Build-CMakeProject @BuildCMakeArgs -BuildTargets $Targets
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

  foreach ($item in "mimalloc.dll", "mimalloc-redirect$HostSuffix.dll") {
    Copy-Item `
      -Path "$BinaryCache\$($Platform.Triple)\mimalloc\bin\$item" `
      -Destination "$($Platform.ToolchainInstallRoot)\usr\bin\"
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
                      [Hashtable] $CCompiler,
                      [string]    $Phase) {
  Build-CMakeProject `
    -Src $SourceCache\brotli `
    -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Phase}Brotli")) `
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
                    [Hashtable] $CCompiler,
                    [string]    $Phase) {
  Build-CMakeProject `
    -Src $SourceCache\zlib `
    -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Phase}ZLib")) `
    -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
    -Platform $Platform `
    -CCompiler $CCompiler `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_POSITION_INDEPENDENT_CODE = "YES";
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
    -Defines @{
      DS2_REGSGEN2 = "$(Get-ProjectBinaryCache $BuildPlatform RegsGen2)/regsgen2.exe";
      DS2_PROGRAM_PREFIX = "$(Get-ModuleTriple $Platform)-";
      BISON_EXECUTABLE = "$(Get-BisonExecutable)";
      FLEX_EXECUTABLE = "$(Get-FlexExecutable)";
    }
}

function Build-CURL([Hashtable] $Platform,
                    [Hashtable] $CCompiler,
                    [string]    $Phase) {
  $PlatformDefines = @{}
  if ($Platform.OS -eq [OS]::Android) {
    $PlatformDefines += @{
      HAVE_FSEEKO = "0";
    }
  }

  Build-CMakeProject `
    -Src $SourceCache\curl `
    -Bin (Get-ProjectBinaryCache $Platform ([Project]"${Phase}CURL")) `
    -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
    -Platform $Platform `
    -CCompiler $CCompiler `
    -Defines ($PlatformDefines + @{
      BUILD_SHARED_LIBS = "NO";
      BUILD_TESTING = "NO";
      CMAKE_POSITION_INDEPENDENT_CODE = "YES";
      BROTLI_INCLUDE_DIR = "$SourceCache\brotli\c\include";
      BROTLICOMMON_LIBRARY = if ($Platform.OS -eq [OS]::Windows) {
        "$(Get-ProjectBinaryCache $Platform ([Project]"${Phase}Brotli"))\brotlicommon.lib"
      } else {
        "$(Get-ProjectBinaryCache $Platform ([Project]"${Phase}Brotli"))\libbrotlicommon.a"
      };
      BROTLIDEC_LIBRARY = if ($Platform.OS -eq [OS]::Windows) {
        "$(Get-ProjectBinaryCache $Platform ([Project]"${Phase}Brotli"))\brotlidec.lib"
      } else {
        "$(Get-ProjectBinaryCache $Platform ([Project]"${Phase}Brotli"))\libbrotlidec.a"
      }
      BUILD_CURL_EXE = "NO";
      BUILD_LIBCURL_DOCS = "NO";
      BUILD_MISC_DOCS = "NO";
      CURL_CA_BUNDLE = "none";
      CURL_CA_FALLBACK = "NO";
      CURL_CA_PATH = "none";
      CURL_BROTLI = "YES";
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
  $ScratchPath = "$BinaryCache\$($BuildPlatform.Triple)\FoundationTests"

  # Foundation tests build via swiftpm rather than CMake
  Build-SPMProject `
    -Action Test `
    -Src $SourceCache\swift-foundation `
    -Bin "$ScratchPath" `
    -Platform $BuildPlatform `
    -Configuration $FoundationTestConfiguration `
    --multiroot-data-file "$SourceCache\swift\utils\build_swift\resources\SwiftPM-Unified-Build.xcworkspace" `
    --test-product swift-foundationPackageTests

  Invoke-IsolatingEnvVars {
    $env:DISPATCH_INCLUDE_PATH="$(Get-SwiftSDK -OS $BuildPlatform.OS)/usr/include"
    $env:LIBXML_LIBRARY_PATH="$BinaryCache/$($Platform.Triple)/usr/lib"
    $env:LIBXML_INCLUDE_PATH="$BinaryCache/$($Platform.Triple)/usr/include/libxml2"
    $env:ZLIB_LIBRARY_PATH="$BinaryCache/$($Platform.Triple)/usr/lib"
    $env:BROTLI_LIBRARY_PATH="$(Get-ProjectBinaryCache $BuildPlatform brotli)"
    $env:CURL_LIBRARY_PATH="$BinaryCache/$($Platform.Triple)/usr/lib"
    $env:CURL_INCLUDE_PATH="$BinaryCache/$($Platform.Triple)/usr/include"
    Build-SPMProject `
      -Action Test `
      -Src $SourceCache\swift-corelibs-foundation `
      -Bin "$ScratchPath" `
      -Platform $BuildPlatform `
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
    $SwiftRuntime          = [IO.Path]::Combine((Get-InstallDir $BuildPlatform), "Runtimes", "$ProductVersion")
    $DispatchBinaryCache   = Get-ProjectBinaryCache $BuildPlatform DynamicDispatch
    $FoundationBinaryCache = Get-ProjectBinaryCache $BuildPlatform DynamicFoundation

    $env:Path = "$(Get-ProjectBinaryCache $BuildPlatform XCTest);$(Get-ProjectBinaryCache $BuildPlatform Testing)\bin;${FoundationBinaryCache}\bin;${DispatchBinaryCache};${SwiftRuntime}\usr\bin;${env:Path};$UnixToolsBinDir"
    $env:SDKROOT = Get-SwiftSDK -OS $Platform.OS

    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-xctest `
      -Bin (Get-ProjectBinaryCache $BuildPlatform XCTest) `
      -Platform $BuildPlatform `
      -CCompiler $Compilers.Stage1.C `
      -CXXCompiler $Compilers.Stage1.CXX `
      -SwiftCompiler $Compilers.Stage1.Swift `
      -SwiftSDK (Get-SwiftSDK -OS $Platform.OS) `
      -BuildTargets default,check-xctest `
      -Defines @{
        ENABLE_TESTING = "YES";
        LLVM_DIR = "$(Get-ProjectBinaryCache $BuildPlatform Stage2Compilers)\lib\cmake\llvm";
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
  return $KnownPlatforms.Values | Where-Object {
    switch ($_.OS) {
      Windows { $Windows }
      Android { $Android }
      default { $false }
    }
  }
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

# Copies files installed by CMake from the arch-specific platform root,
# where they follow the layout expected by the installer,
# to the final platform root, following the installer layout.
function Install-SDK([Hashtable[]] $Platforms, [OS] $OS = $Platforms[0].OS, [string] $Identifier = $OS.ToString()) {
  Repair-SDKHeaders (Get-SwiftSDK -OS $OS -Identifier $Identifier)

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
}

function Build-SDKDependencies([Hashtable[]] $ArchitectureSlices,
                               [Hashtable]   $Compilers,
                               [string]      $Phase) {
  foreach ($Slice in $ArchitectureSlices) {
    # `$Compilers.C/.CXX` are clang-cl drivers configured with MSVC-style
    # flags (`/GS-`, `/Gw`, ...).  For non-Windows targets the NDK clang
    # rejects those — pick the GNU drivers (`-fno-stack-protector`, ...).
    $C   = if ($Slice.OS -eq [OS]::Windows) { $Compilers.C   } else { $Compilers.GNUC   }
    $CXX = if ($Slice.OS -eq [OS]::Windows) { $Compilers.CXX } else { $Compilers.GNUCXX }
    if ($IncludeDS2) { Invoke-BuildStep Build-DS2 $Slice }
    Invoke-BuildStep Build-ZLib $Slice -CCompiler $C -Phase $Phase
    Invoke-BuildStep Build-Brotli $Slice -CCompiler $C -Phase $Phase
    Invoke-BuildStep Build-XML2 $Slice -CCompiler $C -CXXCompiler $CXX -Phase $Phase
    Invoke-BuildStep Build-CURL $Slice -CCompiler $C -Phase $Phase
  }
}

# TODO(compnerd): remove this Build-SDK repair once libdispatch and
# Foundation embed their Windows assembly manifests in their own builds.
function Repair-WindowsSDKAssemblyManifests([Hashtable] $Platform, [string] $RuntimeBin) {
  $Targets = New-Object System.Collections.Generic.List[string]

  foreach ($DLL in @(
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
    Start-Process -Wait -WindowStyle Hidden -FilePath $cmake -ArgumentList @("-P", "Resync.cmake")
    Pop-Location
  }

  Invoke-IsolatingEnvVars {
    # TODO(compnerd) build statically for static builds
    # Match Build-SDKDependencies: pick GNU drivers for non-Windows targets.
    $CDispatchC   = if ($Platform.OS -eq [OS]::Windows) { $Compilers.C   } else { $Compilers.GNUC   }
    $CDispatchCXX = if ($Platform.OS -eq [OS]::Windows) { $Compilers.CXX } else { $Compilers.GNUCXX }
    Build-CDispatch $Platform -CCompiler $CDispatchC -CXXCompiler $CDispatchCXX -Phase $Variant
    if ($BuildFoundation) {
      Build-SDKDependencies @($Platform) -Compilers $Compilers -Phase $Variant
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
          CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";

          dispatch_DIR = (Get-ProjectCMakeModules $Platform ([Project]"${Variant}CDispatch"));

          # FIXME(hjyamauchi) Should dynamic to libdispatch https://github.com/swiftlang/swift/issues/87548
          CMAKE_CXX_FLAGS   = if ($Static) { @("-Ddispatch_STATIC") } else { @() };
          CMAKE_Swift_FLAGS = $(if ($Static) { @("-Xcc", "-static-libclosure") } else { @() });

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
          BROTLICOMMON_LIBRARY              = "$(Get-ProjectBinaryCache $Platform ([Project]"${Variant}Brotli"))\brotlicommon.lib";
          BROTLIDEC_LIBRARY                 = "$(Get-ProjectBinaryCache $Platform ([Project]"${Variant}Brotli"))\brotlidec.lib";
          FOUNDATION_BUILD_TOOLS            = if ($Platform.OS -eq [OS]::Windows) { "YES" } else { "NO" };
          CURL_DIR                          = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake\CURL";
          LibXml2_DIR                       = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake\libxml2-2.11.5";
          ZLIB_LIBRARY                      = if ($Platform.OS -eq [OS]::Windows) {
                                                "$BinaryCache\$($Platform.Triple)\usr\lib\zlibstatic.lib"
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
        $FoundationDefines = $SDKInstallDefines + $FoundationDefines

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
    -Src $sourceCache\swift-subprocess `
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

function Test-LLBuild {
  # Build additional llvm executables needed by tests
  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $BuildPlatform
    Invoke-Program $ninja -C (Get-ProjectBinaryCache $BuildPlatform BuildTools) FileCheck not
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
      -CXXCompiler $Compilers.Host.CXX `
      -SwiftCompiler $Compilers.Stage1.Swift `
      -SwiftSDK (Get-SwiftSDK -OS $BuildPlatform.OS) `
      -BuildTargets default,test-llbuild `
      -Defines @{
        BUILD_SHARED_LIBS = "YES";
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
    # swift-asn1
    "-Xswiftc", "-I$(Get-ProjectBinaryCache $BuildPlatform ASN1)\swift",
    "-Xlinker", "-L$(Get-ProjectBinaryCache $BuildPlatform ASN1)\lib",
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

  # Merge swift swift-inspect.
  copy-Item -Force `
    -Path "$(Get-PlatformRoot $HostPlatform.OS)\Developer\Library\$(Get-ModuleTriple $HostPlatform)\bin\swift-inspect.exe" `
    -Destination "$ToolchainInstallRoot\usr\bin\swift-inspect.exe"
  copy-Item -Force `
    -Path "$(Get-PlatformRoot $HostPlatform.OS)\Developer\Library\$(Get-ModuleTriple $HostPlatform)\bin\SwiftInspectClient.dll" `
    -Destination "$ToolchainInstallRoot\usr\bin\SwiftInspectClient.dll"

  # Copy embeddable Python
  New-Item -Type Directory -Path "$(Get-EmbeddedPythonInstallDir)" -ErrorAction Ignore | Out-Null
  Copy-Item -Force -Recurse `
    -Path "$(Get-EmbeddedPythonPath $HostPlatform)\*" `
    -Destination "$(Get-EmbeddedPythonInstallDir)"
}

function Build-Inspect([Hashtable] $Platform,
                       [Hashtable] $Compilers,
                       [string]    $SwiftSDK) {
  Build-CMakeProject `
    -Src $SourceCache\swift\tools\swift-inspect `
    -Bin (Get-ProjectBinaryCache $Platform SwiftInspect)`
    -InstallTo "$(Get-PlatformRoot $Platform.OS)\Developer\Library\$(Get-ModuleTriple $Platform)" `
    -Platform $Platform `
    -CCompiler $Compilers.C `
    -CXXCompiler $Compilers.CXX `
    -SwiftCompiler $Compilers.Swift `
    -SwiftSDK $SwiftSDK `
    -Defines @{
      CMAKE_Swift_FLAGS = @(
        "-Xcc", "-I$SwiftSDK\usr\include",
        "-Xcc", "-I$SwiftSDK\usr\lib\swift",
        "-Xcc", "-I$SwiftSDK\usr\include\swift\SwiftRemoteMirror",
        "-L$SwiftSDK\usr\lib\swift\$($Platform.OS.ToString())\$($Platform.Architecture.LLVMName)"
      );
      ArgumentParser_DIR = (Get-ProjectCMakeModules $Platform ArgumentParser);
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
  foreach ($Package in $PackageWxiFiles.Keys) {
    $WxiPath = $PackageWxiFiles[$Package]
    if (-not (Test-Path $WxiPath)) {
      Write-Warning "Stage-WindowsToolchainSxS: '$WxiPath' not found; package '$Package' will be omitted from cross-reference"
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
        $EXEToPackage[$EXEName] = $Package
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
    $PackageToSxS[$Package] = @($Names | Sort-Object)
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
  #
  # BlocksRuntime stays flat; the Swift runtime DLLs use private SxS.
  $NonSxSRuntimeDLLs = New-Object System.Collections.Generic.HashSet[string]
  [void]$NonSxSRuntimeDLLs.Add("BlocksRuntime")

  $RuntimeBaseNames = @(
    $RuntimeDLLs |
      ForEach-Object { [IO.Path]::GetFileNameWithoutExtension($_.Name) } |
      Where-Object { -not $NonSxSRuntimeDLLs.Contains($_) }
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
    foreach ($Package in $PackageOrder) {
      $PackageEXEs = @($EXEToPackage.GetEnumerator() | Where-Object { $_.Value -eq $Package } | ForEach-Object { $_.Key })
      $Found = $false
      foreach ($EXE in $PackageEXEs) {
        $FullPath = Join-Path $BinDir $EXE
        if ($EXEPackageClosures.ContainsKey($FullPath) -and ($EXEPackageClosures[$FullPath] -contains $DLL)) {
          $Found = $true
          break
        }
      }
      if ($Found) { $OwningPackage = $Package; break }
    }
    if ($OwningPackage) { $ExpectedPackage[$DLL] = $OwningPackage }
    else                { $ExpectedPackage[$DLL] = "(unattributed)" }
  }

  # Warn about WiX drift, but keep the checked-in package layout canonical.
  foreach ($DLL in $DLLsToInject) {
    $Want = $ExpectedPackage[$DLL]
    $WiXPackage = $null
    foreach ($Package in $PackageToSxS.Keys) {
      if ($PackageToSxS[$Package] -contains $DLL) {
        if ($WiXPackage) {
          Write-Host -BackgroundColor DarkRed -ForegroundColor White "Stage-WindowsToolchainSxS: '$DLL' is declared in multiple package .wxi files ($WiXPackage, $Package)"
        } else {
          $WiXPackage = $Package
        }
      }
    }
    if (-not $WiXPackage) {
      Write-Host -BackgroundColor DarkRed -ForegroundColor White "Stage-WindowsToolchainSxS: '$DLL' is needed by staged PE files (expected package: $Want) but not declared in any package .wxi"
    } elseif ($Want -ne "(unattributed)" -and $WiXPackage -ne $Want) {
      Write-Host -BackgroundColor DarkRed -ForegroundColor White "Stage-WindowsToolchainSxS: '$DLL' is declared in $WiXPackage.wxi but expected package is $Want.wxi"
    }
  }
  foreach ($Package in $PackageToSxS.Keys) {
    foreach ($DLL in $PackageToSxS[$Package]) {
      if (-not $ExpectedPackage.ContainsKey($DLL)) {
        Write-Host -BackgroundColor DarkRed -ForegroundColor White "Stage-WindowsToolchainSxS: '$DLL' is declared in $Package.wxi but no toolchain EXE imports it"
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
  Copy-File "$BinaryCache\$($Platform.Triple)\msi\$($Platform.Architecture.VSName)-$([System.IO.Path]::GetFileNameWithoutExtension("bundle\installer.wixproj")).binlog" $Stage
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\*.cab" $Stage
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\*.msi" $Stage
  foreach ($Build in $WindowsSDKBuilds) {
    Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Build.Architecture.VSName)\*.msm" $Stage
  }
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\installer.exe" $Stage
  # Extract installer engine to ease code-signing on swift.org CI
  New-Item -Type Directory -Path "$BinaryCache\$($Platform.Triple)\installer\$($Platform.Architecture.VSName)" -ErrorAction Ignore | Out-Null
  Invoke-Program "$($WiX.Path)\wix.exe" -- burn detach "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\installer.exe" -engine "$Stage\installer-engine.exe" -intermediateFolder "$BinaryCache\$($Platform.Triple)\installer\$($Platform.Architecture.VSName)\"
}

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

if (-not $SkipBuild) {
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

  # ── Stage0 Compiler ───────────────────────────────────────────────────────
  Invoke-BuildStep Build-XML2 $BuildPlatform -CCompiler $Compilers.Host.C -CXXCompiler $Compilers.Host.CXX -Phase "Stage0"
  Invoke-BuildStep Build-Compilers $BuildPlatform -Variant "Asserts" -Project Stage0Compilers @{
    CacheScript     = "$SourceCache\swift\cmake\caches\Windows-Bootstrap-Stage0-$($BuildPlatform.Architecture.LLVMName).cmake";
    CCompiler       = $Compilers.Host.C;
    CXXCompiler     = $Compilers.Host.CXX;
    SwiftCompiler   = $Compilers.Pinned.Swift;
    SwiftSDK        = Get-PinnedToolchainSDK -OS $BuildPlatform.OS;
    ToolchainRoot   = Get-ProjectToolchainRoot $BuildPlatform Stage0Compilers;
    RuntimeLocation = Get-PinnedToolchainRuntime;
  }

  # ── Bootstrap SDK ─────────────────────────────────────────────────────────
  Invoke-BuildStep Build-SDK $BuildPlatform -Context @{
    SDKIdentifier         = "Bootstrap";
    Variant               = "Bootstrap";
    Compilers             = $Compilers.Stage0;
    Static                = $false;
    BuildFoundation       = $false;
    InstallRuntimeToStage = $false;
    SupplementalRuntimes  = @("StringProcessing");
  }

  # ── Stage1 Compiler ───────────────────────────────────────────────────────
  Invoke-BuildStep Build-Compilers $BuildPlatform -Variant "Asserts" -Project Stage1Compilers @{
    CacheScript     = "$SourceCache\swift\cmake\caches\Windows-Bootstrap-Stage1-$($BuildPlatform.Architecture.LLVMName).cmake";
    CCompiler       = $Compilers.Stage0.C;
    CXXCompiler     = $Compilers.Stage0.CXX;
    SwiftCompiler   = $Compilers.Stage0.Swift;
    SwiftSDK        = Get-SwiftSDK -OS $BuildPlatform.OS -Identifier Bootstrap;
    ToolchainRoot   = Get-ProjectToolchainRoot $BuildPlatform Stage1Compilers;
    RuntimeLocation = Get-SDKRuntimeBin $BuildPlatform (Get-SwiftSDK -OS $BuildPlatform.OS -Identifier Bootstrap) $false;
  }

  # ── Host Platform SDK ─────────────────────────────────────────────────────
  Invoke-BuildStep Build-BootstrapFoundationMacros $BuildPlatform @{
    SwiftCompiler   = $Compilers.Stage0.Swift;
    SwiftSDK        = (Get-SwiftSDK -OS $BuildPlatform.OS -Identifier Bootstrap);
    SwiftSyntax_DIR = (Get-ProjectCMakeModules $BuildPlatform Stage1Compilers);
  }
  Invoke-BuildStep Build-BootstrapTestingMacros $BuildPlatform @{
    SwiftCompiler   = $Compilers.Stage0.Swift;
    SwiftSDK        = (Get-SwiftSDK -OS $BuildPlatform.OS -Identifier Bootstrap);
    SwiftSyntax_DIR = (Get-ProjectCMakeModules $BuildPlatform Stage1Compilers);
  }

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
  Invoke-BuildStep Build-XML2 $HostPlatform -CCompiler $Compilers.Stage1.C -CXXCompiler $Compilers.Stage1.CXX -Phase "Stage2"
  Invoke-BuildStep Build-Compilers $HostPlatform -Variant "Asserts" -Project Stage2Compilers @{
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

  # FIXME(compnerd) this requires the CMake build to be enabled.
  if ($false -and -not $IsCrossCompiling) {
    Invoke-BuildStep Build-DocC $HostPlatform
  }

  # ── Stage2 NoAsserts Compiler ─────────────────────────────────────────────
  if ($IncludeNoAsserts) {
    Invoke-BuildStep Build-Compilers $HostPlatform -Variant "NoAsserts" -Project Stage2Compilers @{
      CCompiler       = $Compilers.Stage1.C;
      CXXCompiler     = $Compilers.Stage1.CXX;
      SwiftCompiler   = $Compilers.Stage1.Swift;
      SwiftSDK        = Get-SwiftSDK -OS $HostPlatform.OS;
      DispatchPackage = Get-ProjectCMakeModules $HostPlatform DynamicCDispatch;
    }

    Repair-Toolchain $HostPlatform.NoAssertsToolchainInstallRoot

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
    & robocopy $HostPlatform.ToolchainInstallRoot $HostPlatform.NoAssertsToolchainInstallRoot /E /XC /XN /XO /NS /NC /NFL /NDL /NJH
    Invoke-BuildStep Write-ToolchainInfo $HostPlatform -Variant "NoAsserts"
  }

  if ($HostPlatform.OS -eq [OS]::Windows) {
    $HostSDKRoot = Get-SwiftSDK -OS $HostPlatform.OS
    $HostRuntimeBin = Resolve-SDKRuntimeBin $HostPlatform $HostSDKRoot
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
  Build-SDKDependencies $WindowsSDKBuilds -Compilers $Compilers.Stage1 -Phase ""

  $SDKROOT = Get-SwiftSDK -OS Windows
  foreach ($Build in $WindowsSDKBuilds) {
    if ($Build.LinkModes.contains("dynamic")) {
      Invoke-BuildStep Build-SDK $Build -Context @{
        SDKIdentifier        = "Windows";
        Variant              = "Dynamic";
        Compilers            = $Compilers.Stage1;
        Static               = $false;
        BuildFoundation      = $true;
        SupplementalRuntimes = $SDKSupplementalRuntimes;
      }
    }

    if ($Build.LinkModes.contains("static")) {
      Invoke-BuildStep Build-SDK $Build -Context @{
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
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build ZLib)\zlibstatic.lib" -Destination "${SwiftResourceDir}\zlibstatic.lib" | Out-Null
  }

  $RebuiltHostDynamicRuntime = @(
    $WindowsSDKBuilds | Where-Object {
      $_ -eq $HostPlatform -and $_.LinkModes.Contains("dynamic")
    }
  ).Count -gt 0
  # If -Windows rebuilds the host dynamic runtime, refresh the private SxS
  # copies after the final runtime image is in place.
  if (-not $SkipBuild -and $RebuiltHostDynamicRuntime) {
    $HostSDKRoot = Get-SwiftSDK -OS $HostPlatform.OS
    $HostRuntimeBin = Resolve-SDKRuntimeBin $HostPlatform $HostSDKRoot
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
  Build-SDKDependencies $AndroidSDKBuilds -Compilers $Compilers.Stage1 -Phase ""

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

if (-not $SkipPackaging) {
  Invoke-BuildStep Build-mimalloc $HostPlatform
  Invoke-BuildStep Patch-mimalloc $HostPlatform
  Invoke-BuildStep Build-Installer $HostPlatform
}

if ($Stage) {
  Copy-BuildArtifactsToStage $HostPlatform
}

if (-not $IsCrossCompiling) {
  if ($SkipBuild -and $HostPlatform.OS -eq [OS]::Windows -and $Test.Count -gt 0) {
    $HostSDKRoot = Get-SwiftSDK -OS $HostPlatform.OS
    $HostRuntimeBin = Resolve-SDKRuntimeBin $HostPlatform $HostSDKRoot
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
