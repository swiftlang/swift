# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

<#
.SYNOPSIS
Downloads the tools that build the Swift toolchain on Windows.

.DESCRIPTION
Downloads and extracts into the artifact cache: the pinned Swift toolchain that
bootstraps the build, CMake, Ninja, Python and its modules, the .NET runtime
and WiX, and as needed GNU make, the Android NDK, flex and bison, syft and the
Windows SDK. Then writes their paths to <BinaryCache>\dependencies.json, where
the other build scripts find them.

This script runs before anything else is set up, so it does not use
BuildSupport.ps1.

.PARAMETER BinaryCache
The path to the directory of the build, which receives dependencies.json.
Default: 'S:\BinaryCache'

.PARAMETER ArtifactCache
The path to a directory containing downloaded build artifacts that can be
shared by multiple build trees.
Default: 'S:\ArtifactCache'

.PARAMETER PinnedBuild
The pinned bootstrap Swift toolchain used to build the Swift components with.

.PARAMETER PinnedSHA256
The SHA256 for the pinned toolchain.

.PARAMETER PinnedVersion
The version of the pinned toolchain.

.PARAMETER IncludeSBoM
Download syft, which generates the Software Bill of Materials.

.PARAMETER DownloadRetryCount
The number of attempts to make when downloading a dependency before giving up.
Default: 3

.PARAMETER ArtifactLockTimeoutSeconds
The maximum time to wait for another process to release an artifact lock.
Default: 1800

.PARAMETER HostArchName
The architecture where the toolchain will execute, for which Python is also
downloaded. Valid values: AMD64, ARM64

.PARAMETER Android
Download the Android NDK.

.PARAMETER AndroidNDKVersion
The version number of the Android NDK to be used.
Format: r{number}[{letter}][-revision-suffix] (e.g., r28c or r30-beta2)
Default: "r30"

.PARAMETER IncludeDS2
Download flex and bison, which build the ds2 remote debug server.

.PARAMETER WinSDKVersion
The version number of the Windows SDK to be used. If no such Windows SDK is
installed, it will be downloaded from nuget.

.PARAMETER WindowsSDKArchitectures
The architectures of the Windows SDK to download from nuget.
Default: @("X64","X86","Arm64")

.PARAMETER Test
The tests that will run, which need more dependencies for lldb.

.EXAMPLE
PS> .\SetupDependencies.ps1 -BinaryCache S:\b -ArtifactCache S:\ArtifactCache
#>
[CmdletBinding(PositionalBinding = $false)]
param
(
  [System.IO.FileInfo] $BinaryCache = "S:\BinaryCache",
  [System.IO.FileInfo] $ArtifactCache = "S:\ArtifactCache",

  # (Pinned) Bootstrap Toolchain
  [string] $PinnedBuild = "",
  [ValidatePattern("^([A-Fa-f0-9]{64}|)$")]
  [string] $PinnedSHA256 = "",
  [string] $PinnedVersion = "",

  [switch] $IncludeSBoM = $false,
  [string] $SyftVersion = "1.40.0",
  [string] $CMakeVersion = "4.4.1",
  [string] $NinjaVersion = "1.13.2",
  [ValidateRange(1, [int]::MaxValue)]
  [int] $DownloadRetryCount = 3,
  [ValidateRange(1, [int]::MaxValue)]
  [int] $ArtifactLockTimeoutSeconds = 1800,
  [ValidatePattern('^\d+(\.\d+)*$')]
  [string] $PythonVersion = "3.10.1",

  [ValidateSet("AMD64", "ARM64")]
  [string] $HostArchName = $(if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }),

  [switch] $Android = $false,
  [ValidatePattern("^r(?:[1-9]|[1-9][0-9])(?:[a-z])?(-beta[1-9])?$")]
  [string] $AndroidNDKVersion = "r30",
  [switch] $IncludeDS2 = $false,

  [ValidatePattern("^\d+\.\d+\.\d+(?:-\w+)?")]
  [string] $WinSDKVersion = "",
  [string[]] $WindowsSDKArchitectures = @("X64","X86","Arm64"),

  [string[]] $Test = @()
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version 3.0

# Validate that if one is set all are set.
if (($PinnedBuild -or $PinnedSHA256 -or $PinnedVersion) -and -not ($PinnedBuild -and $PinnedSHA256 -and $PinnedVersion)) {
  throw "If any of PinnedBuild, PinnedSHA256, or PinnedVersion is set, all three must be set."
}

# Work around limitations of cmd passing in array arguments via powershell.exe -File
if ($WindowsSDKArchitectures.Length -eq 1) { $WindowsSDKArchitectures = $WindowsSDKArchitectures[0].Split(",") }

if ($Test.Length -eq 1) { $Test = $Test[0].Split(",") }

if ($Test -contains "*") {
  # Explicitly don't include llbuild yet since tests are known to fail on Windows
  $Test = @("lld", "lldb", "lldb-swift", "swift", "dispatch", "foundation", "xctest", "swift-format", "sourcekit-lsp")
}

$DefaultPinned = @{
  AMD64 = @{
    PinnedBuild = "https://download.swift.org/swift-6.4.x-branch/windows10/swift-6.4.x-DEVELOPMENT-SNAPSHOT-2026-08-01-a/swift-6.4.x-DEVELOPMENT-SNAPSHOT-2026-08-01-a-windows10.exe";
    PinnedSHA256 = "C287DD533A65A73D657B1B9F2305BE50552F89B46199A0F6162A287DEE547149";
    PinnedVersion = "6.4.0";
  };
  ARM64 = @{
    PinnedBuild = "https://download.swift.org/swift-6.4.x-branch/windows10-arm64/swift-6.4.x-DEVELOPMENT-SNAPSHOT-2026-08-01-a/swift-6.4.x-DEVELOPMENT-SNAPSHOT-2026-08-01-a-windows10-arm64.exe"
    PinnedSHA256 = "8C9F35E37DA08CC598E0CBC7821880D2282E4D02633DD78ECBB248370A5C9985";
    PinnedVersion = "6.4.0";
  };
}

$WiX = @{
  Version = "7.0.0";
  EulaIdentifier = "wix7";
  URL = "https://www.nuget.org/api/v2/package/wix/7.0.0";
  SHA256 = "7f992e57c356dcbda2ea961bf3b348e1bd7d31d96795be4ac391e04cf140536d";
  Path = [IO.Path]::Combine("$ArtifactCache\WiX-7.0.0", "tools", "net8.0", "any");
}

$DotNetRuntime = @{
  Version = "8.0.27";
  Runtimes = @{
    AMD64 = @{
      RuntimeIdentifier = "win-x64";
      URL = "https://builds.dotnet.microsoft.com/dotnet/Runtime/8.0.27/dotnet-runtime-8.0.27-win-x64.zip";
      SHA256 = "0708AEAB018AB9C2BFAAC248AD2F3A2A1046913D96CB9A0A904A107C4CE4A813";
    };
    ARM64 = @{
      RuntimeIdentifier = "win-arm64";
      URL = "https://builds.dotnet.microsoft.com/dotnet/Runtime/8.0.27/dotnet-runtime-8.0.27-win-arm64.zip";
      SHA256 = "6FFBD29E58B71AB1EE7DD02741C420AF172D27A3963098D57F6FE8887FC7BEBF";
    };
  };
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
  # One SHA256 per architecture. Most modules are pinned to an architecture
  # independent source distribution and have the same hashes.
  "packaging" = @{
    Version = "24.1";
    SHA256 = @{
      AMD64 = "026ed72c8ed3fcce5bf8950572258698927fd1dbda10a5e981cdf0ac37f4f002";
      ARM64 = "026ed72c8ed3fcce5bf8950572258698927fd1dbda10a5e981cdf0ac37f4f002";
    };
    Dependencies = @();
  };
  "setuptools" = @{
    Version = "75.1.0";
    SHA256 = @{
      AMD64 = "d59a21b17a275fb872a9c3dae73963160ae079f1049ed956880cd7c09b120538";
      ARM64 = "d59a21b17a275fb872a9c3dae73963160ae079f1049ed956880cd7c09b120538";
    };
    Dependencies = @();
  };
  "psutil" = @{
    Version = "6.1.0";
    SHA256 = @{
      AMD64 = "a8fb3752b491d246034fa4d279ff076501588ce8cbcdbb62c32fd7a377d996be";
      ARM64 = "353815f59a7f64cdaca1c0307ee13558a0512f6db064e92fe833784f08539c7a";
    };
    Dependencies = @();
  };
  "cryptography" = @{
    Version = "46.0.3";
    SHA256 = @{
      AMD64 = "416260257577718c05135c55958b674000baef9a1c7d9e8f306ec60d71db850f";
      ARM64 = "d89c3468de4cdc4f08a57e214384d0471911a3830fcdaf7a8cc587e42a866372";
    };
    Dependencies = @("cffi", "pycparser", "typing_extensions");
  };
  "cffi" = @{
    Version = "2.0.0";
    # There is no cp310 win_arm64 wheel; ARM64 builds from the sdist.
    SHA256 = @{
      AMD64 = "b18a3ed7d5b3bd8d9ef7a8cb226502c6bf8308df1525e1cc676c3680e7176739";
      ARM64 = "44d1b5909021139fe36001ae048dbdde8214afa20200eda0f64c068cac5d5529";
    };
    Dependencies = @();
  };
  "pycparser" = @{
    Version = "2.23";
    SHA256 = @{
      AMD64 = "e5c6e8d3fbad53479cab09ac03729e0a9faf2bee3db8208a550daf5af81a5934";
      ARM64 = "e5c6e8d3fbad53479cab09ac03729e0a9faf2bee3db8208a550daf5af81a5934";
    };
    Dependencies = @();
  };
  "typing_extensions" = @{
    Version = "4.15.0";
    SHA256 = @{
      AMD64 = "f0fa19c6845758ab08074a0cfa8b7aecb71c999ca73d62883bc25cc018c4e548";
      ARM64 = "f0fa19c6845758ab08074a0cfa8b7aecb71c999ca73d62883bc25cc018c4e548";
    };
    Dependencies = @();
  };
  "argparse" = @{
    Version = "1.4.0";
    SHA256 = @{
      AMD64 = "c31647edb69fd3d465a847ea3157d37bed1f95f19760b11a47aa91c04b666314";
      ARM64 = "c31647edb69fd3d465a847ea3157d37bed1f95f19760b11a47aa91c04b666314";
    };
    Dependencies = @();
  };
  "six" = @{
    Version = "1.17.0";
    SHA256 = @{
      AMD64 = "4721f391ed90541fddacab5acf947aa0d3dc7d27b2e1e8eda2be8970586c3274";
      ARM64 = "4721f391ed90541fddacab5acf947aa0d3dc7d27b2e1e8eda2be8970586c3274";
    };
    Dependencies = @();
  };
  "traceback2" = @{
    Version = "1.4.0";
    SHA256 = @{
      AMD64 = "8253cebec4b19094d67cc5ed5af99bf1dba1285292226e98a31929f87a5d6b23";
      ARM64 = "8253cebec4b19094d67cc5ed5af99bf1dba1285292226e98a31929f87a5d6b23";
    };
    Dependencies = @();
  };
  "linecache2" = @{
    Version = "1.0.0";
    SHA256 = @{
      AMD64 = "e78be9c0a0dfcbac712fe04fbf92b96cddae80b1b842f24248214c8496f006ef";
      ARM64 = "e78be9c0a0dfcbac712fe04fbf92b96cddae80b1b842f24248214c8496f006ef";
    };
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
  r30 = @{
    URL = "https://dl.google.com/android/repository/android-ndk-r30-windows.zip"
    SHA256 = "b830098aaf18b67a42eb831c404e15e5f2990a474f054ac145b0bc957ac6d729"
    ClangVersion = 21
  }
}

$WinFlexBison = @{
  Version = "2.5.25"
  URL = "https://github.com/lexxmark/winflexbison/releases/download/v2.5.25/win_flex_bison-2.5.25.zip"
  SHA256 = "8D324B62BE33604B2C45AD1DD34AB93D722534448F55A16CA7292DE32B6AC135"
}

$KnownSyft = @{
  "1.29.1" = @{
    AMD64 = @{
      Artifact = "syft-1.29.1-windows-amd64"
      URL = "https://github.com/anchore/syft/releases/download/v1.29.1/syft_1.29.1_windows_amd64.zip"
      SHA256 = "3C67CD9AF40CDCC7FFCE041C8349B4A77F33810184820C05DF23440C8E0AA1D7"
      Path = [IO.Path]::Combine("$ArtifactCache\syft-1.29.1-windows-amd64", "syft.exe")
    }
  };
  "1.40.0" = @{
    AMD64 = @{
      Artifact = "syft-1.40.0-windows-amd64"
      URL = "https://github.com/anchore/syft/releases/download/v1.40.0/syft_1.40.0_windows_amd64.zip"
      SHA256 = "3F4021EC098B4BCBAF19BBA7028CF7704FEF12936970778CEC3C6D669B740E6D"
      Path = [IO.Path]::Combine("$ArtifactCache\syft-1.40.0-windows-amd64", "syft.exe")
    };
    ARM64 = @{
      Artifact = "syft-1.40.0-windows-arm64"
      URL = "https://github.com/anchore/syft/releases/download/v1.40.0/syft_1.40.0_windows_arm64.zip"
      SHA256 = "CE7129DBCC39809542C9BC5032B179131DFEE72C68C5B3741E3270A3D9ED46E4"
      Path = [IO.Path]::Combine("$ArtifactCache\syft-1.40.0-windows-arm64", "syft.exe")
    };
  }
}

$KnownCMakes = @{
  "4.4.1" = @{
    AMD64 = @{
      Artifact = "cmake-4.4.1-windows-amd64"
      URL = "https://github.com/Kitware/CMake/releases/download/v4.4.1/cmake-4.4.1-windows-x86_64.zip"
      SHA256 = "091919E1CDE162B69D2D5E0F3B1F5670C973E72133F78126FBB18042947D6F19"
      FileName = "cmake-4.4.1-windows-x86_64.zip"
      CMakeRoot = [IO.Path]::Combine("$ArtifactCache", "cmake-4.4.1-windows-amd64", "cmake-4.4.1-windows-x86_64", "share", "cmake-4.4")
      Path = [IO.Path]::Combine("$ArtifactCache", "cmake-4.4.1-windows-amd64", "cmake-4.4.1-windows-x86_64", "bin", "cmake.exe")
    };
    ARM64 = @{
      Artifact = "cmake-4.4.1-windows-arm64"
      URL = "https://github.com/Kitware/CMake/releases/download/v4.4.1/cmake-4.4.1-windows-arm64.zip"
      SHA256 = "DC59D9F377F891B8DA42EDE22F53717034A9D093092FCEAF6297FEEEC6AFBA29"
      FileName = "cmake-4.4.1-windows-arm64.zip"
      CMakeRoot = [IO.Path]::Combine("$ArtifactCache", "cmake-4.4.1-windows-arm64", "cmake-4.4.1-windows-arm64", "share", "cmake-4.4")
      Path = [IO.Path]::Combine("$ArtifactCache", "cmake-4.4.1-windows-arm64", "cmake-4.4.1-windows-arm64", "bin", "cmake.exe")
    };
  }
}

$KnownNinjas = @{
  "1.13.2" = @{
    AMD64 = @{
      Artifact = "ninja-1.13.2-windows-amd64"
      URL = "https://github.com/ninja-build/ninja/releases/download/v1.13.2/ninja-win.zip"
      SHA256 = "07FC8261B42B20E71D1720B39068C2E14FFCEE6396B76FB7A795FB460B78DC65"
      FileName = "ninja-win.zip"
      Path = [IO.Path]::Combine("$ArtifactCache", "ninja-1.13.2-windows-amd64", "ninja.exe")
    };
    ARM64 = @{
      Artifact = "ninja-1.13.2-windows-arm64"
      URL = "https://github.com/ninja-build/ninja/releases/download/v1.13.2/ninja-winarm64.zip"
      SHA256 = "E52F0BDEF9DFB1003229DBD6508A508C4073FD017247002ADC66E5E806CB0391"
      FileName = "ninja-winarm64.zip"
      Path = [IO.Path]::Combine("$ArtifactCache", "ninja-1.13.2-windows-arm64", "ninja.exe")
    };
  }
}

$BuildArchName = if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }
$IsCrossCompiling = $HostArchName -ne $BuildArchName

$vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$VSInstallRoot = & $vswhere -nologo -latest -products "*" -all -prerelease -property installationPath

$NugetRoot = "$ArtifactCache\nuget"

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

# The names of the architectures for the Visual Studio developer shell and the
# Windows SDK packages.
$VSArchNames = @{ AMD64 = "amd64"; ARM64 = "arm64" }
$WindowsSDKArchNames = @{ AMD64 = "x64"; ARM64 = "arm64"; X64 = "x64"; X86 = "x86" }

function Get-AndroidNDK {
  $NDK = $KnownNDKs[$AndroidNDKVersion]
  if (-not $NDK) { throw "Unsupported Android NDK version" }
  return $NDK
}

function Get-AndroidNDKPath {
  return Join-Path -Path $ArtifactCache -ChildPath "android-ndk-$AndroidNDKVersion"
}

function Get-PythonPath([string] $ArchName) {
  return [IO.Path]::Combine("$ArtifactCache\", "Python$ArchName-$PythonVersion")
}

function Get-EmbeddedPythonPath([string] $ArchName) {
  return [IO.Path]::Combine("$ArtifactCache\", "EmbeddedPython$ArchName-$PythonVersion")
}

function Get-PythonExecutable {
  return [IO.Path]::Combine((Get-PythonPath $BuildArchName), "tools", "python.exe")
}

function Get-Syft {
  return $KnownSyft[$SyftVersion][$BuildArchName]
}

function Get-CMake {
  return $KnownCMakes[$CMakeVersion][$BuildArchName]
}

function Get-Ninja {
  return $KnownNinjas[$NinjaVersion][$BuildArchName]
}

function Copy-Directory($Src, $Dst) {
  New-Item -ItemType Directory -ErrorAction Ignore $Dst | Out-Null
  Copy-Item -Force -Recurse -Path $Src -Destination $Dst
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

function Get-DotNetRuntime() {
  if (-not $DotNetRuntime.Runtimes.ContainsKey($BuildArchName)) {
    throw "Unsupported .NET runtime host architecture '$BuildArchName'"
  }

  return $DotNetRuntime.Runtimes[$BuildArchName]
}

function Get-DotNetRuntimeRoot() {
  $Runtime = Get-DotNetRuntime
  return [IO.Path]::Combine("$ArtifactCache", "dotnet-runtime-$($DotNetRuntime.Version)-$($Runtime.RuntimeIdentifier)")
}

function Get-DotNet() {
  return [IO.Path]::Combine((Get-DotNetRuntimeRoot), "dotnet.exe")
}

function Invoke-WithDotNetRuntime([scriptblock] $Body) {
  Invoke-IsolatingEnvVars {
    $DotNetRuntimeRoot = Get-DotNetRuntimeRoot
    $env:DOTNET_ROOT = $DotNetRuntimeRoot
    $env:DOTNET_HOST_PATH = Get-DotNet
    $env:DOTNET_MULTILEVEL_LOOKUP = "0"

    & $Body
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

# Whether the Visual Studio developer shell finds the requested Windows SDK.
function Test-WindowsSDK {
  try {
    Invoke-IsolatingEnvVars {
      Import-Module "$VSInstallRoot\Common7\Tools\Microsoft.VisualStudio.DevShell.dll"
      Enter-VsDevShell -VsInstallPath $VSInstallRoot -SkipAutomaticLocation -DevCmdArguments "-no_logo -host_arch=$($VSArchNames[$BuildArchName]) -arch=$($VSArchNames[$HostArchName]) -winsdk=$WinSDKVersion"
    }
    return $true
  } catch {
    return $false
  }
}

function Write-Success([string] $Description) {
  $SuccessTag = @{
    Object = "[OK]"
    ForegroundColor = 'DarkGreen'
    NoNewLine = $true
  }
  Write-Host @SuccessTag
  Write-Host " $Description"
}

$Stopwatch = [Diagnostics.Stopwatch]::StartNew()
Write-Host "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies ..." -ForegroundColor Cyan
$ProgressPreference = "SilentlyContinue"

$WebClient = New-Object Net.WebClient

function DownloadAndVerify($URL, $Destination, $Hash) {
  if (Test-Path $Destination) { return }

  New-Item -ItemType Directory (Split-Path -Path $Destination -Parent) -ErrorAction Ignore | Out-Null

  for ($Attempt = 1; $Attempt -le $DownloadRetryCount; $Attempt++) {
    $TemporaryDestination = "$Destination.$PID.$([Guid]::NewGuid()).tmp"
    try {
      $WebClient.DownloadFile($URL, $TemporaryDestination)
      $SHA256 = Get-FileHash -Path $TemporaryDestination -Algorithm SHA256
      if ($SHA256.Hash -ne $Hash) {
        throw "SHA256 mismatch ($($SHA256.Hash) vs $Hash)"
      }

      try {
        [IO.File]::Move($TemporaryDestination, $Destination)
      } catch {
        if (-not (Test-Path $Destination)) { throw }
      }
      return
    } catch {
      if ($Attempt -eq $DownloadRetryCount) {
        throw
      }
      Write-Warning "Download of $URL failed (attempt $Attempt/$DownloadRetryCount): $_"
      Start-Sleep -Seconds ([Math]::Pow(2, $Attempt))
    } finally {
      Remove-Item -LiteralPath $TemporaryDestination -ErrorAction Ignore
    }
  }
}

function Expand-ArtifactZip([string] $ZipFileName,
                            [string] $ExtractPath,
                            [string] $ArchiveRoot = "") {
  $Source = Join-Path -Path $ArtifactCache -ChildPath $ZipFileName
  $Destination = Join-Path -Path $ArtifactCache -ChildPath $ExtractPath
  if (Test-Path $Destination) { return }

  $TemporaryDestination = Join-Path -Path $ArtifactCache -ChildPath ".$ExtractPath.$PID.$([Guid]::NewGuid()).tmp"
  try {
    # Expand-Archive is several times slower on Windows PowerShell 5.1.
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    [IO.Compression.ZipFile]::ExtractToDirectory($Source, $TemporaryDestination)
    $PublishedSource = if ($ArchiveRoot) {
      Join-Path -Path $TemporaryDestination -ChildPath $ArchiveRoot
    } else {
      $TemporaryDestination
    }
    try {
      [IO.Directory]::Move($PublishedSource, $Destination)
    } catch {
      if (-not (Test-Path $Destination)) { throw }
    }
  } finally {
    Remove-Item -LiteralPath $TemporaryDestination -Recurse -Force -ErrorAction Ignore
  }
}

function Invoke-WithArtifactLock([string] $Name, [ScriptBlock] $ScriptBlock) {
  $LockRoot = Join-Path -Path $ArtifactCache -ChildPath ".locks"
  New-Item -ItemType Directory -Path $LockRoot -ErrorAction Ignore | Out-Null
  $LockPath = Join-Path -Path $LockRoot -ChildPath "$Name.lock"

  $Lock = $null
  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()
  while (-not $Lock) {
    try {
      $Lock = [IO.File]::Open($LockPath, [IO.FileMode]::OpenOrCreate,
                              [IO.FileAccess]::ReadWrite, [IO.FileShare]::None)
    } catch [IO.IOException] {
      $ErrorCode = $_.Exception.HResult -band 0xffff
      if ($ErrorCode -notin 32, 33) { throw }
      if ($Stopwatch.Elapsed.TotalSeconds -ge $ArtifactLockTimeoutSeconds) {
        throw "Timed out after $ArtifactLockTimeoutSeconds seconds waiting for artifact lock '$LockPath'"
      }
      Start-Sleep -Milliseconds 100
    }
  }
  $Stopwatch.Stop()

  try {
    & $ScriptBlock
  } finally {
    $Lock.Dispose()
  }
}

function Extract-Toolchain {
  param
  (
      [string]$InstallerExeName,
      [string]$ToolchainName
  )

  $source = Join-Path -Path $ArtifactCache -ChildPath $InstallerExeName
  $ToolchainRoot = Join-Path -Path $ArtifactCache -ChildPath "toolchains"
  $destination = Join-Path -Path $ToolchainRoot -ChildPath $ToolchainName
  if (Test-Path $destination) { return }

  New-Item -ItemType Directory -Path $ToolchainRoot -ErrorAction Ignore | Out-Null
  $TemporaryRoot = Join-Path -Path $ToolchainRoot -ChildPath ".$ToolchainName.$PID.$([Guid]::NewGuid()).tmp"
  $BundleRoot = Join-Path -Path $TemporaryRoot -ChildPath "bundle"
  $InstallRoot = Join-Path -Path $TemporaryRoot -ChildPath "root"
  New-Item -ItemType Directory -Path $InstallRoot | Out-Null

  $RuntimePath = "LocalApp\Programs\Swift\Runtimes\$PinnedVersion\usr\bin"
  $RuntimeDestination = Join-Path -Path $InstallRoot -ChildPath $RuntimePath
  $RuntimeTarget = [IO.Path]::Combine("X:\", $RuntimePath)
  New-Item -ItemType Directory -Path $RuntimeDestination -Force | Out-Null
  $DriveMapped = $false
  try {
    Invoke-WithDotNetRuntime {
      Invoke-Program (Get-DotNet) "$($WiX.Path)\wix.dll" -- burn extract -acceptEula $WiX.EulaIdentifier $source -out $BundleRoot -outba $BundleRoot
    }

    Invoke-Program -OutNull subst.exe X: "$InstallRoot"
    $DriveMapped = $true
    Get-ChildItem "$BundleRoot\WixAttachedContainer" -Filter "*.msi" | ForEach-Object {
      $LogFile = [System.IO.Path]::ChangeExtension($_.Name, "log")
      # Administrative installs do not run rtl.msi's SetDirectory actions.
      $TargetDirectory = if ($_.Name -eq "rtl.msi") { $RuntimeTarget } else { "X:\" }
      Invoke-Program -OutNull msiexec.exe /lvx! $TemporaryRoot\$LogFile /qn /a $_.FullName ALLUSERS=0 TARGETDIR=$TargetDirectory
    }

    subst.exe /d X: | Out-Null
    $DriveMapped = $false
    [IO.Directory]::Move($InstallRoot, $destination)
  } finally {
    if ($DriveMapped) {
      subst.exe /d X: | Out-Null
    }
    Remove-Item -LiteralPath $TemporaryRoot -Recurse -Force -ErrorAction Ignore
  }
}

if ($IncludeSBoM) {
  $syft = Get-Syft
  DownloadAndVerify $syft.URL "$ArtifactCache\$($syft.Artifact).zip" $syft.SHA256
  Expand-ArtifactZip "$($syft.Artifact).zip" $syft.Artifact
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
  DownloadAndVerify $Python.URL "$ArtifactCache\$FileName.zip" $Python.SHA256
  Expand-ArtifactZip "$FileName.zip" $FileName
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
  # Also check the dependencies so that caches populated before one was
  # pinned get repaired.
  $Modules = @($ModuleName) + $PythonModules[$ModuleName].Dependencies
  try {
    Invoke-Program -Silent "$(Get-PythonExecutable)" -c "import importlib.util, sys; sys.exit(0 if all(importlib.util.find_spec(m) for m in sys.argv[1:]) else 1)" @Modules
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
  $ArchName = $BuildArchName

  $Module = $PythonModules[$ModuleName]
  "$ModuleName==$($Module.Version) --hash=`"sha256:$($Module.SHA256[$ArchName])`"" | Out-File -FilePath $TempRequirementsTxt -Append -Encoding utf8
  foreach ($Dependency in $Module.Dependencies) {
    $DependencyModule = $PythonModules[$Dependency]
    "$Dependency==$($DependencyModule.Version) --hash=`"sha256:$($DependencyModule.SHA256[$ArchName])`"" | Out-File -FilePath $TempRequirementsTxt -Append -Encoding utf8
  }

  # Dependencies are pinned above; --require-hashes rejects anything else
  # pip would resolve on its own.
  Invoke-Program -OutNull "$(Get-PythonExecutable)" '-I' -m pip install -r $TempRequirementsTxt --require-hashes --disable-pip-version-check

  Write-Success "$ModuleName"
}

function Install-PythonModules {
  Install-PIPIfNeeded
  Install-PythonModule "packaging"  # For building LLVM 18+
  Install-PythonModule "setuptools" # Required for SWIG support
  if ($Test -contains "swift" -or $Test -contains "lldb" -or $Test -contains "lldb-swift") {
    Install-PythonModule "psutil"       # Required for lit per-test timeouts, and for testing LLDB
  }
  if ($Test -contains "lldb" -or $Test -contains "lldb-swift") {
    Install-PythonModule "cryptography" # Required for testing LLDB
  }
}

# Ensure Python modules that are required as host build tools
Install-Python $HostArchName
Install-Python $HostArchName $true
if ($IsCrossCompiling) {
  Install-Python $BuildArchName
  Install-Python $BuildArchName $true
}
Invoke-WithArtifactLock (Split-Path -Leaf (Get-PythonPath $BuildArchName)) {
  Install-PythonModules
}

# WiX is needed both for packaging and for extracting the pinned toolchain
# installer that bootstraps toolchain builds.
$DotNetRuntimeInfo = Get-DotNetRuntime
$DotNetArchive = "dotnet-runtime-$($DotNetRuntime.Version)-$($DotNetRuntimeInfo.RuntimeIdentifier).zip"
DownloadAndVerify $DotNetRuntimeInfo.URL "$ArtifactCache\$DotNetArchive" $DotNetRuntimeInfo.SHA256
Expand-ArtifactZip $DotNetArchive "dotnet-runtime-$($DotNetRuntime.Version)-$($DotNetRuntimeInfo.RuntimeIdentifier)"
Write-Success ".NET Runtime $($DotNetRuntime.Version) ($($DotNetRuntimeInfo.RuntimeIdentifier))"

DownloadAndVerify $WiX.URL "$ArtifactCache\WiX-$($WiX.Version).zip" $WiX.SHA256
Expand-ArtifactZip "WiX-$($WiX.Version).zip" "WiX-$($WiX.Version)"
Write-Success "WiX $($WiX.Version)"

if ($Test -contains "lldb" -or $Test -contains "lldb-swift") {
  # The make tool isn't part of MSYS
  $GnuWin32MakeURL = "https://downloads.sourceforge.net/project/ezwinports/make-4.4.1-without-guile-w32-bin.zip"
  $GnuWin32MakeHash = "fb66a02b530f7466f6222ce53c0b602c5288e601547a034e4156a512dd895ee7"
  DownloadAndVerify $GnuWin32MakeURL "$ArtifactCache\GnuWin32Make-4.4.1.zip" $GnuWin32MakeHash
  Expand-ArtifactZip GnuWin32Make-4.4.1.zip GnuWin32Make-4.4.1
  Write-Success "GNUWin32 make 4.4.1"
}

DownloadAndVerify $PinnedBuild "$ArtifactCache\$PinnedToolchain.exe" $PinnedSHA256

$ToolchainArtifact = "$ToolchainVersionIdentifier-$($BuildArchName.ToLowerInvariant())"
Invoke-WithArtifactLock "SwiftToolchainExtraction" {
  Extract-Toolchain "$PinnedToolchain.exe" -ToolchainName $ToolchainArtifact
}
Write-Success "Swift Toolchain $PinnedVersion"

# Install CMake.
$CMake = Get-CMake
DownloadAndVerify $CMake.URL "$ArtifactCache\$($CMake.FileName)" $CMake.SHA256
Expand-ArtifactZip $CMake.FileName $CMake.Artifact
Write-Success "CMake $CMakeVersion"

# Install Ninja.
$Ninja = Get-Ninja
DownloadAndVerify $Ninja.URL "$ArtifactCache\$($Ninja.FileName)" $Ninja.SHA256
Expand-ArtifactZip $Ninja.FileName $Ninja.Artifact
Write-Success "Ninja $NinjaVersion"

if ($Android) {
  $NDK = Get-AndroidNDK
  DownloadAndVerify $NDK.URL "$ArtifactCache\android-ndk-$AndroidNDKVersion-windows.zip" $NDK.SHA256
  Expand-ArtifactZip "android-ndk-$AndroidNDKVersion-windows.zip" "android-ndk-$AndroidNDKVersion" "android-ndk-$AndroidNDKVersion"
  Write-Success "Android NDK $AndroidNDKVersion"
}

if ($IncludeDS2) {
  $Artifact = "win_flex_bison-$($WinFlexBison.Version)"
  DownloadAndVerify $WinFlexBison.URL "$ArtifactCache\$Artifact.zip" $WinFlexBison.SHA256
  Expand-ArtifactZip "$Artifact.zip" $Artifact
  Write-Success "flex/bison $($WinFlexBison.Version)"
}

$CustomWinSDKRoot = $null
if ($WinSDKVersion -and -not (Test-WindowsSDK)) {
  Write-Output "Windows SDK $WinSDKVersion not found. Downloading from nuget.org ..."
  Invoke-WithArtifactLock "WindowsSDK-$WinSDKVersion" {
    Invoke-Program nuget install Microsoft.Windows.SDK.CPP -Version $WinSDKVersion -OutputDirectory $NugetRoot

    # Set to script scope so the manifest can record it.
    $script:CustomWinSDKRoot = "$NugetRoot\Microsoft.Windows.SDK.CPP.$WinSDKVersion\c"

    # Install each required architecture package and move files under the base /lib directory.
    $ArchNames = @($WindowsSDKArchitectures + $HostArchName | ForEach-Object { $WindowsSDKArchNames[$_] } | Sort-Object -Unique)
    foreach ($ArchName in $ArchNames) {
      Invoke-Program nuget install Microsoft.Windows.SDK.CPP.$ArchName -Version $WinSDKVersion -OutputDirectory $NugetRoot
      Copy-Directory "$NugetRoot\Microsoft.Windows.SDK.CPP.$ArchName.$WinSDKVersion\c\*" "$CustomWinSDKRoot\lib\$WinSDKVersion"
    }
  }
}

# Record where the dependencies are for the other build scripts.
$ToolchainRoot = [IO.Path]::Combine("$ArtifactCache\toolchains", $ToolchainArtifact, "LocalApp", "Programs", "Swift")
$PythonArchNames = @($HostArchName, $BuildArchName) | Sort-Object -Unique
$Manifest = [ordered]@{
  Python = [ordered]@{
    Version = $PythonVersion
    Executable = Get-PythonExecutable
    Roots = [ordered]@{}
    EmbeddedRoots = [ordered]@{}
  }
  PinnedToolchain = [ordered]@{
    Version = $PinnedVersion
    ToolsDir = [IO.Path]::Combine($ToolchainRoot, "Toolchains", "$PinnedVersion+Asserts", "usr", "bin")
    PlatformsDir = [IO.Path]::Combine($ToolchainRoot, "Platforms", $PinnedVersion)
  }
  CMake = [ordered]@{
    Path = $CMake.Path
    CMakeRoot = $CMake.CMakeRoot
  }
  Ninja = [ordered]@{
    Path = $Ninja.Path
  }
  DotNet = [ordered]@{
    Root = Get-DotNetRuntimeRoot
    Path = Get-DotNet
  }
  WiX = [ordered]@{
    Path = $WiX.Path
    EulaIdentifier = $WiX.EulaIdentifier
  }
  GnuMake = "$ArtifactCache\GnuWin32Make-4.4.1\bin\make.exe"
  AndroidNDK = if ($Android) {
    [ordered]@{
      Path = Get-AndroidNDKPath
      ClangVersion = (Get-AndroidNDK).ClangVersion
    }
  } else {
    $null
  }
  FlexBison = if ($IncludeDS2) {
    [ordered]@{
      Flex = Join-Path -Path $ArtifactCache -ChildPath "win_flex_bison-$($WinFlexBison.Version)\win_flex.exe"
      Bison = Join-Path -Path $ArtifactCache -ChildPath "win_flex_bison-$($WinFlexBison.Version)\win_bison.exe"
    }
  } else {
    $null
  }
  Syft = if ($IncludeSBoM) { (Get-Syft).Path } else { $null }
  WinSDK = [ordered]@{
    Version = $WinSDKVersion
    Root = $CustomWinSDKRoot
  }
}
foreach ($ArchName in $PythonArchNames) {
  $Manifest.Python.Roots[$ArchName] = Get-PythonPath $ArchName
  $Manifest.Python.EmbeddedRoots[$ArchName] = Get-EmbeddedPythonPath $ArchName
}

New-Item -ItemType Directory -Path $BinaryCache -ErrorAction Ignore | Out-Null
[IO.File]::WriteAllText("$BinaryCache\dependencies.json", (ConvertTo-Json -Depth 4 -InputObject $Manifest))

Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies took $($Stopwatch.Elapsed)"
Write-Host ""
