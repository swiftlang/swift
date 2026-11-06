# build-sdk.ps1
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

<#
.SYNOPSIS
Builds the Swift SDK for one Windows architecture.

.DESCRIPTION
This script builds the runtime, the overlays, the supplemental libraries,
libdispatch, Foundation, swift-testing and XCTest for one Windows architecture,
and installs them into a Windows platform root.

It is self-contained: it does not load build.ps1 and needs only a compiler
toolchain and the bootstrap macros. Several instances can run at the same time
for different architectures, as long as each one installs into its own
platform root.

The binary cache layout, `<BinaryCache>\<triple>\<project>`, matches build.ps1,
which consumes the results:
- the Stage2 compilers use DynamicCDispatch,
- the tests use DynamicDispatch, DynamicFoundation and Testing.

.PARAMETER Architecture
The architecture of the SDK to build: X64, X86 or Arm64.

.PARAMETER SourceCache
The path to a directory where the projects have been cloned.
Default: 'S:\SourceCache'

.PARAMETER BinaryCache
The path to a directory where to write build system files and outputs.
Default: 'S:\BinaryCache'

.PARAMETER ArtifactCache
The path to a directory containing the downloaded CMake and Ninja.
Default: 'S:\ArtifactCache'

.PARAMETER BuildRoot
The path to the image root. The runtime is installed beneath
`<BuildRoot>\Program Files*\Swift\Runtimes\<ProductVersion>`.
Default: 'S:'

.PARAMETER PlatformRoot
The path to the Windows.platform directory to install the SDK, swift-testing
and XCTest into.
Default: '<BuildRoot>\Program Files\Swift\Platforms\Windows.platform'

.PARAMETER ToolchainBin
The directory containing clang-cl.exe, clang.exe, clang++.exe and swiftc.exe.
Default: the Stage1 toolchain built by build.ps1.

.PARAMETER FoundationMacros
The directory containing FoundationMacros.dll.
Default: the BootstrapFoundationMacros built by build.ps1.

.PARAMETER TestingMacros
The path to TestingMacros.dll.
Default: the BootstrapTestingMacros built by build.ps1.

.PARAMETER LinkModes
The link modes to build the SDK for: dynamic, static or both.
Default: dynamic, static

.PARAMETER ProductVersion
The product version embedded in the runtime and the SDK settings.
Default: "0.0.0"

.PARAMETER WindowsSxSAssemblyPublicKeyToken
The public key token to embed in the Windows side-by-side assembly manifests.
Default: "0000000000000000"

.PARAMETER HostArchName
The architecture of the toolchain the SDK is installed with. The runtime for
this architecture is installed in `Program Files`, the others in
`Program Files (<arch>)`.
Default: the processor architecture of the machine.

.PARAMETER WinSDKVersion
The version of the Windows SDK to use. It must be installed, or downloaded into
`<ArtifactCache>\nuget` by build.ps1.

.PARAMETER DebugInfo
Emit debug information.

.PARAMETER Summary
Print the time spent in each step at the end of the build.

.EXAMPLE
PS> .\build-sdk.ps1 -Architecture X86

.EXAMPLE
PS> .\build-sdk.ps1 -Architecture Arm64 -LinkModes dynamic -PlatformRoot S:\Staging\Arm64\Windows.platform
#>

[CmdletBinding(PositionalBinding = $false)]
param
(
  [Parameter(Mandatory = $true)]
  [ValidateSet("X64", "X86", "Arm64")]
  [string] $Architecture,
  [System.IO.FileInfo] $SourceCache = "S:\SourceCache",
  [System.IO.FileInfo] $BinaryCache = "S:\BinaryCache",
  [System.IO.FileInfo] $ArtifactCache = "S:\ArtifactCache",
  [System.IO.FileInfo] $BuildRoot = "S:",
  [string] $PlatformRoot = "",
  [string] $ToolchainBin = "",
  [string] $FoundationMacros = "",
  [string] $TestingMacros = "",
  [ValidateSet("dynamic", "static")]
  [string[]] $LinkModes = @("dynamic", "static"),
  [string] $ProductVersion = "0.0.0",
  [ValidatePattern("^[A-Fa-f0-9]{16}$")]
  [string] $WindowsSxSAssemblyPublicKeyToken = "0000000000000000",
  [ValidateSet("AMD64", "ARM64")]
  [string] $HostArchName = $(if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }),
  [ValidatePattern("^\d+\.\d+\.\d+(?:-\w+)?")]
  [string] $WinSDKVersion = "",
  [string] $CMakeVersion = "4.4.1",
  [string] $NinjaVersion = "1.13.2",
  [switch] $DebugInfo,
  [switch] $Summary
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version 3.0

if ($env:VSCMD_ARG_HOST_ARCH -or $env:VSCMD_ARG_TGT_ARCH) {
  throw "At least one of VSCMD_ARG_HOST_ARCH and VSCMD_ARG_TGT_ARCH is set, which is incompatible with this script. Likely need to run outside of a Developer shell."
}

if ($LinkModes.Length -eq 1) { $LinkModes = $LinkModes[0].Split(",") }

$WindowsSxSAssemblyPublicKeyToken = $WindowsSxSAssemblyPublicKeyToken.ToLowerInvariant()
$ModuleCache = "$BinaryCache\ModuleCache"
$env:CLANG_MODULE_CACHE_PATH = $ModuleCache
$env:SDKROOT = ""

$KnownPlatforms = @{
  X64 = @{
    Triple = "x86_64-unknown-windows-msvc";
    Architecture = @{
      VSName = "amd64";
      CMakeName = "AMD64";
      LLVMName = "x86_64";
      ShortName = "x64";
    };
    BinaryDir = "bin64";
    InstallDir = "Program Files (Amd64)";
  };

  X86 = @{
    Triple = "i686-unknown-windows-msvc";
    Architecture = @{
      VSName = "x86";
      CMakeName = "i686";
      LLVMName = "i686";
      ShortName = "x86";
    };
    BinaryDir = "bin32";
    InstallDir = "Program Files (x86)";
  };

  Arm64 = @{
    Triple = "aarch64-unknown-windows-msvc";
    Architecture = @{
      VSName = "arm64";
      CMakeName = "ARM64";
      LLVMName = "aarch64";
      ShortName = "arm64";
    };
    BinaryDir = "bin64a";
    InstallDir = "Program Files (Arm64)";
  };
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
$BuildPlatform = switch ($BuildArchName) {
  "AMD64" { $KnownPlatforms.X64 }
  "ARM64" { $KnownPlatforms.Arm64 }
  default { throw "Unsupported processor architecture" }
}
$HostPlatform = switch ($HostArchName) {
  "AMD64" { $KnownPlatforms.X64 }
  "ARM64" { $KnownPlatforms.Arm64 }
}
$Platform = $KnownPlatforms[$Architecture]

$vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$VSInstallRoot = & $vswhere -nologo -latest -products "*" -all -prerelease -property installationPath

# A Windows SDK that build.ps1 downloaded from nuget, rather than an installed one.
$CustomWinSDKRoot = $null
if ($WinSDKVersion -and (Test-Path "$ArtifactCache\nuget\Microsoft.Windows.SDK.CPP.$WinSDKVersion\c")) {
  $CustomWinSDKRoot = "$ArtifactCache\nuget\Microsoft.Windows.SDK.CPP.$WinSDKVersion\c"
}

$InstallDir = if ($Platform -eq $HostPlatform) { "Program Files" } else { $Platform.InstallDir }
$RuntimeRoot = [IO.Path]::Combine("$BuildRoot\", $InstallDir, "Swift", "Runtimes", $ProductVersion)
if (-not $PlatformRoot) {
  $PlatformRoot = [IO.Path]::Combine("$BuildRoot\", "Program Files", "Swift", "Platforms", "Windows.platform")
}
$SDKRoot = [IO.Path]::Combine($PlatformRoot, "Developer", "SDKs", "Windows.sdk")

if (-not $ToolchainBin) {
  $ToolchainBin = "$BinaryCache\$($BuildPlatform.Triple)\Stage1Compilers\toolchain\usr\bin"
}
if (-not $FoundationMacros) {
  $FoundationMacros = "$BinaryCache\$($BuildPlatform.Triple)\BootstrapFoundationMacros\bin"
}
if (-not $TestingMacros) {
  $TestingMacros = "$BinaryCache\$($BuildPlatform.Triple)\BootstrapTestingMacros\TestingMacros.dll"
}

$Compilers = @{
  C = @{
    Executable = Join-Path $ToolchainBin "clang-cl.exe";
    Flags = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline");
    DebugFlags = @();
    ClangCL = $true;
  };
  CXX = @{
    Executable = Join-Path $ToolchainBin "clang-cl.exe";
    Flags = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline", "/Zc:__cplusplus");
    DebugFlags = @();
    ClangCL = $true;
  };
  GNUC = @{
    Executable = Join-Path $ToolchainBin "clang.exe";
    Flags = @("-fno-stack-protector", "-ffunction-sections", "-fdata-sections", "-fomit-frame-pointer", "-finline-functions");
    DebugFlags = @("-gcodeview");
    ClangCL = $false;
  };
  GNUCXX = @{
    Executable = Join-Path $ToolchainBin "clang++.exe";
    Flags = @("-fno-stack-protector", "-ffunction-sections", "-fdata-sections", "-fomit-frame-pointer", "-finline-functions");
    DebugFlags = @("-gcodeview");
    ClangCL = $false;
  };
  Swift = @{
    Executable = Join-Path $ToolchainBin "swiftc.exe";
    Flags = @();
    DebugFlags = @("-g", "-debug-info-format=codeview");
  };
}

$Timings = New-Object System.Collections.Generic.List[System.Object]

# The block runs in a child scope, so these names must not shadow its variables.
function Invoke-Step([string] $StepName, [ScriptBlock] $StepBlock) {
  $StepStopwatch = [Diagnostics.Stopwatch]::StartNew()
  try {
    & $StepBlock
  } finally {
    $Timings.Add([PSCustomObject]@{ Step = $StepName; "Elapsed Time" = $StepStopwatch.Elapsed.ToString("hh\:mm\:ss") })
  }
}

function Invoke-Program() {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position = 0, Mandatory = $true)]
    [string] $Executable,
    [Parameter(Position = 1, ValueFromRemainingArguments)]
    [string[]] $ExecutableArgs
  )

  & $Executable @ExecutableArgs
  if ($LastExitCode -ne 0) {
    throw "Error: $([IO.Path]::GetFileName($Executable)) exited with code $($LastExitCode).`nInvocation:`n  $Executable $ExecutableArgs"
  }
}

function Invoke-IsolatingEnvVars([ScriptBlock] $Block) {
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

# Serializes the work of the instances building other architectures.
function Invoke-Exclusively([string] $Name, [ScriptBlock] $Block) {
  $Mutex = New-Object System.Threading.Mutex($false, "Global\swift-build-sdk-$Name")
  try {
    try {
      [void]$Mutex.WaitOne()
    } catch [System.Threading.AbandonedMutexException] {
      # The previous owner died; the mutex is ours now.
    }
    & $Block
  } finally {
    $Mutex.ReleaseMutex()
    $Mutex.Dispose()
  }
}

function Invoke-VsDevShell([Hashtable] $Platform) {
  $DevCmdArguments = "-no_logo -host_arch=$($BuildPlatform.Architecture.VSName) -arch=$($Platform.Architecture.VSName)"
  if ($CustomWinSDKRoot) {
    $DevCmdArguments += " -winsdk=none"
  } elseif ($WinSDKVersion) {
    $DevCmdArguments += " -winsdk=$WinSDKVersion"
  }

  Import-Module "$VSInstallRoot\Common7\Tools\Microsoft.VisualStudio.DevShell.dll"
  Enter-VsDevShell -VsInstallPath $VSInstallRoot -SkipAutomaticLocation -DevCmdArguments $DevCmdArguments

  if ($CustomWinSDKRoot) {
    $WinSDKVerIncludeRoot = "$CustomWinSDKRoot\include\$WinSDKVersion"
    $WinSDKIncludePath = "$WinSDKVerIncludeRoot\ucrt;$WinSDKVerIncludeRoot\um;$WinSDKVerIncludeRoot\shared;$WinSDKVerIncludeRoot\winrt;$WinSDKVerIncludeRoot\cppwinrt"
    $WinSDKVerLibRoot = "$CustomWinSDKRoot\lib\$WinSDKVersion"

    $env:WindowsLibPath = "$CustomWinSDKRoot\UnionMetadata\$WinSDKVersion;$CustomWinSDKRoot\References\$WinSDKVersion"
    $env:WindowsSdkBinPath = "$CustomWinSDKRoot\bin"
    $env:WindowsSDKLibVersion = "$WinSDKVersion\"
    $env:WindowsSdkVerBinPath = "$CustomWinSDKRoot\bin\$WinSDKVersion"
    $env:WindowsSdkDir = $CustomWinSDKRoot
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

# Downloads and extracts an archive into the artifact cache, unless an earlier
# build already did. Concurrent instances publish with an atomic move.
function Get-Artifact([Hashtable] $Artifact) {
  $Destination = Join-Path $ArtifactCache $Artifact.Artifact
  if (Test-Path $Destination) { return }

  New-Item -ItemType Directory -ErrorAction Ignore $ArtifactCache | Out-Null
  $Temporary = Join-Path $ArtifactCache ".$($Artifact.Artifact).$PID.$([Guid]::NewGuid())"
  try {
    Write-Host "Downloading $($Artifact.URL) ..."
    $ProgressPreference = "SilentlyContinue"
    (New-Object Net.WebClient).DownloadFile($Artifact.URL, "$Temporary.zip")
    $SHA256 = (Get-FileHash -Path "$Temporary.zip" -Algorithm SHA256).Hash
    if ($SHA256 -ne $Artifact.SHA256) {
      throw "SHA256 mismatch for $($Artifact.URL) ($SHA256 vs $($Artifact.SHA256))"
    }

    Add-Type -AssemblyName System.IO.Compression.FileSystem
    [IO.Compression.ZipFile]::ExtractToDirectory("$Temporary.zip", $Temporary)
    try {
      [IO.Directory]::Move($Temporary, $Destination)
    } catch {
      if (-not (Test-Path $Destination)) { throw }
    }
  } finally {
    Remove-Item -LiteralPath "$Temporary.zip" -Force -ErrorAction Ignore
    Remove-Item -LiteralPath $Temporary -Recurse -Force -ErrorAction Ignore
  }
}

$CMake = $KnownCMakes[$CMakeVersion][$BuildArchName]
$Ninja = $KnownNinjas[$NinjaVersion][$BuildArchName]

function Add-KeyValueIfNew([Hashtable] $Hashtable, [string] $Key, [string] $Value) {
  if (-not $Hashtable.Contains($Key)) {
    $Hashtable.Add($Key, $Value)
  }
}

function Add-FlagsDefine([Hashtable] $Defines, [string] $Name, [string[]] $Value) {
  $Value = @($Value | Where-Object { $null -ne $_ })
  if ($Value.Count -eq 0) {
    return
  }

  if ($Defines.Contains($Name)) {
    $Defines[$Name] = @($Defines[$Name] | Where-Object { $null -ne $_ }) + $Value
  } else {
    $Defines.Add($Name, $Value)
  }
}

# Backslashes in CMakeCache.txt are escape characters. Keep the leading `\\` of
# a UNC path, possibly as the value of an option using `=`.
function ConvertTo-CMakeArgument([string] $Argument) {
  $UNCPath = if ($Argument.StartsWith("\\")) {
    0
  } else {
    $OptionValue = $Argument.IndexOf("=\\")
    if ($OptionValue -ge 0) { $OptionValue + 1 } else { -1 }
  }
  if ($UNCPath -lt 0) {
    return $Argument.Replace("\", "/")
  }

  $Prefix = $Argument.Substring(0, $UNCPath).Replace("\", "/")
  $Suffix = $Argument.Substring($UNCPath + 2).Replace("\", "/")
  return "$Prefix\\$Suffix"
}

function Get-ProjectBinaryCache([string] $Project) {
  return [IO.Path]::Combine("$BinaryCache\", $Platform.Triple, $Project)
}

function Get-ProjectCMakeModules([string] $Project) {
  return [IO.Path]::Combine((Get-ProjectBinaryCache $Project), "cmake", "modules")
}

function Build-CMakeProject {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [string] $Src,
    [string] $Bin,
    [string] $InstallTo = "",
    [Hashtable] $CCompiler = $null,
    [Hashtable] $CXXCompiler = $null,
    [Hashtable] $SwiftCompiler = $null,
    [string] $SwiftSDK = $null,
    [Hashtable] $Defines = @{},
    [string[]] $BuildTargets = @()
  )

  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Building '$Src' to '$Bin' ..."
  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()

  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $Platform

    function Add-LinkerFlagsDefine([Hashtable] $Defines, [string[]] $Value) {
      $Value = $Value | ForEach-Object { "LINKER:$_" }
      Add-FlagsDefine $Defines CMAKE_EXE_LINKER_FLAGS $Value
      Add-FlagsDefine $Defines CMAKE_SHARED_LINKER_FLAGS $Value
    }

    function Add-SharedLinkerFlagsDefine([Hashtable] $Defines, [string[]] $Value) {
      $Value = $Value | ForEach-Object { "LINKER:$_" }
      Add-FlagsDefine $Defines CMAKE_SHARED_LINKER_FLAGS $Value
      Add-FlagsDefine $Defines CMAKE_MODULE_LINKER_FLAGS $Value
    }

    $Defines = $Defines.Clone()

    $env:CMAKE_ROOT = $CMake.CMakeRoot
    Add-KeyValueIfNew $Defines CMAKE_POLICY_DEFAULT_CMP0181 NEW
    Add-KeyValueIfNew $Defines CMAKE_POLICY_DEFAULT_CMP0214 NEW
    Add-KeyValueIfNew $Defines CMAKE_BUILD_TYPE Release

    if ($Platform.Architecture.CMakeName -ne $BuildPlatform.Architecture.CMakeName) {
      Add-KeyValueIfNew $Defines CMAKE_SYSTEM_NAME Windows
      Add-KeyValueIfNew $Defines CMAKE_SYSTEM_PROCESSOR $Platform.Architecture.CMakeName
    }

    Add-KeyValueIfNew $Defines CMAKE_FIND_PACKAGE_PREFER_CONFIG YES

    if ($CCompiler) {
      Add-KeyValueIfNew $Defines CMAKE_C_COMPILER $CCompiler.Executable
      Add-KeyValueIfNew $Defines CMAKE_C_COMPILER_TARGET $Platform.Triple
      Add-FlagsDefine $Defines CMAKE_C_FLAGS $CCompiler.Flags
      if ($DebugInfo) {
        Add-FlagsDefine $Defines CMAKE_C_FLAGS $CCompiler.DebugFlags
      }
    }

    if ($CXXCompiler) {
      Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER $CXXCompiler.Executable
      Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER_TARGET $Platform.Triple
      Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXCompiler.Flags

      # Keep CMake from picking up a POSIX `ar` from PATH for clang-cl.
      if ($CXXCompiler.ClangCL) {
        $Librarian = Join-Path (Split-Path $CXXCompiler.Executable) "llvm-lib.exe"
        if (Test-Path $Librarian) {
          Add-KeyValueIfNew $Defines CMAKE_AR $Librarian
        }
      }

      if ($DebugInfo) {
        Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXCompiler.DebugFlags
      }
    }

    if ($SwiftCompiler) {
      Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_WORKS "YES"
      Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER $SwiftCompiler.Executable
      Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_TARGET $Platform.Triple
      Add-KeyValueIfNew $Defines CMAKE_Swift_COMPILER_ID "Apple"

      Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftCompiler.Flags
      if ($SwiftSDK) {
        Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-sdk", $SwiftSDK)
      }
      if ($DebugInfo) {
        Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftCompiler.DebugFlags
      } else {
        Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-gnone")
      }

      # CMake passes the release linker flags to swiftc without `-Xlinker`.
      Add-KeyValueIfNew $Defines CMAKE_EXE_LINKER_FLAGS_RELEASE ""
      Add-KeyValueIfNew $Defines CMAKE_SHARED_LINKER_FLAGS_RELEASE ""
    }

    Add-LinkerFlagsDefine $Defines @("/INCREMENTAL:NO", "/OPT:REF", "/OPT:ICF")
    Add-SharedLinkerFlagsDefine $Defines @("/MANIFEST:NO")
    Add-KeyValueIfNew $Defines CMAKE_USER_MAKE_RULES_OVERRIDE `
      "$SourceCache\swift\utils\windows-clang-overrides.cmake"

    if ($DebugInfo -and ($CCompiler -or $CXXCompiler)) {
      Add-KeyValueIfNew $Defines CMAKE_MSVC_DEBUG_INFORMATION_FORMAT Embedded
      Add-KeyValueIfNew $Defines CMAKE_POLICY_DEFAULT_CMP0141 NEW
      Add-LinkerFlagsDefine $Defines @("/DEBUG")
    }

    if ($SwiftCompiler) {
      Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-module-cache-path", $ModuleCache)
    }

    if ($InstallTo) {
      Add-KeyValueIfNew $Defines CMAKE_INSTALL_PREFIX $InstallTo
    }

    Add-KeyValueIfNew $Defines CMAKE_MAKE_PROGRAM $Ninja.Path

    $GenerateArgs = @("-B", $Bin, "-S", $Src, "-G", "Ninja")
    foreach ($Define in ($Defines.GetEnumerator() | Sort-Object Name)) {
      if ($Define.Value -is [string]) {
        $Value = ConvertTo-CMakeArgument $Define.Value
      } else {
        # Quote the flags containing spaces. PowerShell 5 and Core handle the
        # quotes differently.
        $Quote = if ($PSEdition -eq "Core") { '"' } else { '\"' }
        $Value = (@($Define.Value) | ForEach-Object {
          $Arg = ConvertTo-CMakeArgument $_
          if ($Arg.Contains(" ")) { "$Quote$Arg$Quote" } else { $Arg }
        }) -join " "
      }
      $GenerateArgs += @("-D", "$($Define.Key)=$Value")
    }

    Write-Host "$($CMake.Path) $GenerateArgs"
    Invoke-Program $CMake.Path @GenerateArgs

    if ($BuildTargets -contains "default") {
      Invoke-Program $CMake.Path --build $Bin
    }
    $Targets = @($BuildTargets | Where-Object { $_ -ne "default" })
    if ($Targets.Length -gt 0) {
      Invoke-Program $CMake.Path --build $Bin --target @Targets
    }
    if ($BuildTargets.Length -eq 0 -and $InstallTo) {
      Invoke-Program $CMake.Path --build $Bin --target install
    }
  }

  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Finished building '$Src' to '$Bin' in $($Stopwatch.Elapsed)"
  Write-Host ""
}

function Move-Directory($Src, $Dst) {
  $Destination = Join-Path -Path $Dst -ChildPath (Split-Path -Path $Src -Leaf)
  New-Item -ItemType Directory -ErrorAction Ignore $Dst | Out-Null
  if (Test-Path -Path $Destination -Type Container) {
    Get-ChildItem $Src | ForEach-Object {
      Move-Item -Force $_.FullName $Destination | Out-Null
    }
    Remove-Item -Force $Src | Out-Null
  } else {
    Move-Item -Path $Src -Destination $Destination -Force | Out-Null
  }
}

# Moves the C module headers that libdispatch and Foundation install beneath
# usr/lib/swift[_static]/<Module> into usr/include, where the clients of the SDK
# look for them.
function Repair-SDKHeaders {
  foreach ($Module in ("Block", "dispatch", "os", "_foundation_unicode", "_FoundationCShims")) {
    foreach ($ResourceType in ("swift", "swift_static")) {
      $ModuleDirectory = "$SDKRoot\usr\lib\$ResourceType\$Module"
      if (Test-Path $ModuleDirectory) {
        Move-Directory $ModuleDirectory "$SDKRoot\usr\include\"
      }
    }
  }
}

function ConvertTo-ThickLayout([string] $Resources, [string[]] $Filter) {
  Get-ChildItem "${Resources}\*" -File -Include ${Filter} -ErrorAction Ignore | ForEach-Object {
    Write-Host -BackgroundColor DarkRed -ForegroundColor White "$($_.FullName) is not nested in an architecture directory"
    Move-Item -Force $_.FullName "$Resources\$($Platform.Architecture.LLVMName)\" | Out-Null
  }
}

function ConvertTo-PList($Value, [string] $Indent = "") {
  if ($Value -is [Hashtable]) {
    $Lines = @("$Indent<dict>")
    foreach ($Key in ($Value.Keys | Sort-Object)) {
      $Lines += "$Indent`t<key>$([Security.SecurityElement]::Escape($Key))</key>"
      $Lines += ConvertTo-PList $Value[$Key] "$Indent`t"
    }
    return $Lines + "$Indent</dict>"
  }
  if ($Value -is [Array]) {
    $Lines = @("$Indent<array>")
    foreach ($Element in $Value) {
      $Lines += ConvertTo-PList $Element "$Indent`t"
    }
    return $Lines + "$Indent</array>"
  }
  return "$Indent<string>$([Security.SecurityElement]::Escape([string]$Value))</string>"
}

# Lists only this architecture; build.ps1 rewrites the settings once all the
# architectures are installed in the same SDK.
function Write-SDKSettings {
  $SDKSettings = @{
    CanonicalName = "windows"
    DisplayName = "Windows"
    IsBaseSDK = "YES"
    Version = "${ProductVersion}"
    VersionMap = @{}
    HeaderSearchPaths = @( "usr/include" );
    LibrarySearchPaths = @();
    DefaultProperties = @{
      PLATFORM_NAME = "windows"
      DEFAULT_USE_RUNTIME = "MD"
    }
    SupportedTargets = @{
      windows = @{
        PlatformFamilyDisplayName = "Windows"
        PlatformFamilyName = "Windows"
        LLVMTargetVendor = "unknown"
        LLVMTargetSys = "windows"
        LLVMTargetTripleEnvironment = "msvc"
        Archs = @($Platform.Architecture.LLVMName)
      }
    }
  }

  $SDKSettings | ConvertTo-JSON -Depth 4 | Out-File -FilePath "$SDKRoot\SDKSettings.json"
  $PList = @(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">',
    '<plist version="1.0">'
  ) + (ConvertTo-PList $SDKSettings) + @('</plist>')
  $PList | Out-File -FilePath "$SDKRoot\SDKSettings.plist" -Encoding UTF8
}

# Windows SxS requires `<assemblyIdentity version=...>` to be `a.b.c.d`.
function ConvertTo-FourPartVersion([string] $Version) {
  $Numeric = [regex]::Replace($Version, "[-+].*", "")
  $Parts = @($Numeric.Split('.'))
  while ($Parts.Count -lt 4) { $Parts += "0" }
  return ($Parts[0..3] -join ".")
}

Add-Type -Namespace Win32 -Name Resources -MemberDefinition @"
[DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
static extern IntPtr LoadLibraryExW(string lpLibFileName, IntPtr hFile, uint dwFlags);
[DllImport("kernel32.dll")]
static extern bool FreeLibrary(IntPtr hModule);
delegate bool EnumResNameProc(IntPtr hModule, IntPtr lpType, IntPtr lpName, IntPtr lParam);
[DllImport("kernel32.dll", SetLastError = true)]
static extern bool EnumResourceNamesW(IntPtr hModule, IntPtr lpType, EnumResNameProc lpEnumFunc, IntPtr lParam);

// Returns the integer names of the RT_MANIFEST resources of an image of any
// architecture.
public static int[] GetManifestIDs(string path) {
  const uint LOAD_LIBRARY_AS_DATAFILE = 0x2;
  const uint LOAD_LIBRARY_AS_IMAGE_RESOURCE = 0x20;
  IntPtr module = LoadLibraryExW(path, IntPtr.Zero, LOAD_LIBRARY_AS_DATAFILE | LOAD_LIBRARY_AS_IMAGE_RESOURCE);
  if (module == IntPtr.Zero) {
    throw new System.ComponentModel.Win32Exception(Marshal.GetLastWin32Error());
  }
  var IDs = new System.Collections.Generic.List<int>();
  try {
    EnumResourceNamesW(module, (IntPtr)24, (m, type, name, param) => {
      long ID = name.ToInt64();
      if ((ID >> 16) == 0) { IDs.Add((int)ID); }
      return true;
    }, IntPtr.Zero);
  } finally {
    FreeLibrary(module);
  }
  return IDs.ToArray();
}
"@

function Export-WindowsManifestResource([string] $ImagePath, [int] $ResourceID) {
  $ManifestPath = Join-Path ([IO.Path]::GetTempPath()) `
    "swift-sxs-$([IO.Path]::GetFileName($ImagePath))-#$ResourceID-$([guid]::NewGuid().Guid).manifest"

  & "mt.exe" -nologo "-inputresource:$ImagePath;#$ResourceID" -out:$ManifestPath 2>&1 | Out-Null
  if ($LASTEXITCODE -ne 0 -or -not (Test-Path $ManifestPath)) {
    return $null
  }
  return $ManifestPath
}

function Test-WindowsAssemblyManifestMatchesImage([string] $ManifestPath, [string] $ImagePath) {
  $Doc = New-Object System.Xml.XmlDocument
  $Doc.Load($ManifestPath)

  $AssemblyIdentity = $Doc.SelectSingleNode("/*[local-name()='assembly']/*[local-name()='assemblyIdentity']")
  $File = $Doc.SelectSingleNode("/*[local-name()='assembly']/*[local-name()='file']")
  if ($null -eq $AssemblyIdentity -or $null -eq $File) { return $false }

  return (
    ($AssemblyIdentity.GetAttribute("type") -ieq "win32") -and
    ($AssemblyIdentity.GetAttribute("name") -eq [IO.Path]::GetFileNameWithoutExtension($ImagePath)) -and
    ($AssemblyIdentity.GetAttribute("version") -eq (ConvertTo-FourPartVersion $ProductVersion)) -and
    ($AssemblyIdentity.GetAttribute("processorArchitecture") -ieq $Platform.Architecture.VSName) -and
    ($AssemblyIdentity.GetAttribute("publicKeyToken") -ieq $WindowsSxSAssemblyPublicKeyToken) -and
    ($File.GetAttribute("name") -eq [IO.Path]::GetFileName($ImagePath))
  )
}

# Embeds the SxS assembly manifest as RT_MANIFEST #1, unless the image already
# has it. Any other manifest is an error: the producer must not generate one.
function Ensure-WindowsAssemblyManifest([string] $ImagePath) {
  $LogPrefix = "Repair-WindowsSDKAssemblyManifests"

  $ExistingManifestPath = Export-WindowsManifestResource $ImagePath 1
  if ($ExistingManifestPath) {
    try {
      $IsExpected = Test-WindowsAssemblyManifestMatchesImage $ExistingManifestPath $ImagePath
    } catch {
      throw "${LogPrefix}: '$ImagePath' has an unparsable RT_MANIFEST #1 (extracted to '$ExistingManifestPath')"
    }
    if (-not $IsExpected) {
      throw "${LogPrefix}: '$ImagePath' has RT_MANIFEST #1, but it is not the expected SxS assembly manifest (extracted to '$ExistingManifestPath'); rebuild the producer without a linker-generated manifest instead of overwriting it"
    }
    Remove-Item -Path $ExistingManifestPath -Force -ErrorAction SilentlyContinue
    Write-Host "${LogPrefix}: '$ImagePath' already has the expected assembly manifest"
    return
  }

  $ManifestPath = Join-Path ([IO.Path]::GetTempPath()) "swift-sxs-assembly-$([guid]::NewGuid().Guid).manifest"
  try {
    Set-Content -Path $ManifestPath -Encoding UTF8 -Value @"
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <assemblyIdentity
      name="$([IO.Path]::GetFileNameWithoutExtension($ImagePath))"
      processorArchitecture="$($Platform.Architecture.VSName)"
      publicKeyToken="$WindowsSxSAssemblyPublicKeyToken"
      type="win32"
      version="$(ConvertTo-FourPartVersion $ProductVersion)" />
  <file name="$([IO.Path]::GetFileName($ImagePath))"/>
</assembly>
"@

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

# Windows reserves the RT_MANIFEST IDs 1..16; this layout uses #1 only, and no
# manifest may request a UAC execution level.
function Assert-WindowsManifestResourcesAreSxSSafe([string] $ImagePath) {
  $LogPrefix = "Repair-WindowsSDKAssemblyManifests"

  $ResourceIDs = @([Win32.Resources]::GetManifestIDs($ImagePath))
  $UnexpectedReservedResourceIDs = @($ResourceIDs | Where-Object { $_ -ge 2 -and $_ -le 16 })
  if ($UnexpectedReservedResourceIDs.Count -gt 0) {
    throw "${LogPrefix}: '$ImagePath' has unexpected reserved RT_MANIFEST resources (#$($UnexpectedReservedResourceIDs -join ', #')); Windows reserves IDs 1..16, and this layout uses RT_MANIFEST #1 for SxS manifests"
  }

  foreach ($ResourceID in $ResourceIDs) {
    $ManifestPath = Export-WindowsManifestResource $ImagePath $ResourceID
    if (-not $ManifestPath) {
      throw "${LogPrefix}: failed to extract RT_MANIFEST resource #$ResourceID from '$ImagePath'"
    }

    try {
      $Doc = New-Object System.Xml.XmlDocument
      $Doc.Load($ManifestPath)
    } catch {
      throw "${LogPrefix}: '$ImagePath' has an unparsable RT_MANIFEST resource #$ResourceID (extracted to '$ManifestPath')"
    }
    if ($Doc.SelectSingleNode("//*[local-name()='trustInfo' or local-name()='requestedPrivileges' or local-name()='requestedExecutionLevel']")) {
      throw "${LogPrefix}: '$ImagePath' has RT_MANIFEST resource #$ResourceID with UAC execution-level metadata (extracted to '$ManifestPath')"
    }
    Remove-Item -Path $ManifestPath -Force -ErrorAction SilentlyContinue
  }
}

# TODO(compnerd): remove this repair once libdispatch and Foundation embed
# their Windows assembly manifests in their own builds.
function Repair-WindowsSDKAssemblyManifests {
  $RuntimeBin = "$RuntimeRoot\usr\bin"
  $Targets = @(
    "BlocksRuntime",
    "dispatch",
    "swiftDispatch",
    "Foundation",
    "FoundationEssentials",
    "FoundationInternationalization",
    "FoundationNetworking",
    "FoundationXML",
    "_FoundationICU"
  ) | ForEach-Object { "$RuntimeBin\$_.dll" } | Where-Object { Test-Path $_ }

  if (@($Targets).Count -eq 0) {
    return
  }

  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $Platform
    foreach ($Path in $Targets) {
      Ensure-WindowsAssemblyManifest $Path
      Assert-WindowsManifestResourcesAreSxSSafe $Path
    }
  }
}

function Build-SDKDependencies {
  Invoke-Step "ZLib" {
    Build-CMakeProject `
      -Src $SourceCache\zlib `
      -Bin (Get-ProjectBinaryCache ZLib) `
      -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
      -CCompiler $Compilers.C `
      -Defines @{
        CMAKE_POSITION_INDEPENDENT_CODE = "YES";
        ZLIB_BUILD_MINIZIP = "NO";
        ZLIB_BUILD_SHARED = "NO";
        ZLIB_BUILD_STATIC = "YES";
        ZLIB_BUILD_TESTING = "NO";
      }
  }

  Invoke-Step "Brotli" {
    Build-CMakeProject `
      -Src $SourceCache\brotli `
      -Bin (Get-ProjectBinaryCache Brotli) `
      -CCompiler $Compilers.C `
      -BuildTargets default `
      -Defines @{
        BUILD_SHARED_LIBS = "NO";
        CMAKE_POSITION_INDEPENDENT_CODE = "YES";
        BROTLI_BUILD_TOOLS = "NO";
      }
  }

  Invoke-Step "XML2" {
    Build-CMakeProject `
      -Src $SourceCache\libxml2 `
      -Bin (Get-ProjectBinaryCache XML2) `
      -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
      -CCompiler $Compilers.C `
      -CXXCompiler $Compilers.CXX `
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

  Invoke-Step "CURL" {
    Build-CMakeProject `
      -Src $SourceCache\curl `
      -Bin (Get-ProjectBinaryCache CURL) `
      -InstallTo "$BinaryCache\$($Platform.Triple)\usr" `
      -CCompiler $Compilers.C `
      -Defines @{
        BUILD_SHARED_LIBS = "NO";
        BUILD_TESTING = "NO";
        CMAKE_POSITION_INDEPENDENT_CODE = "YES";
        BROTLI_INCLUDE_DIR = "$SourceCache\brotli\c\include";
        BROTLICOMMON_LIBRARY = "$(Get-ProjectBinaryCache Brotli)\brotlicommon.lib";
        BROTLIDEC_LIBRARY = "$(Get-ProjectBinaryCache Brotli)\brotlidec.lib";
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
        CURL_DEFAULT_SSL_BACKEND = "schannel";
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
        CURL_USE_OPENSSL = "NO";
        CURL_USE_PKGCONFIG = "NO";
        CURL_USE_RUSTLS = "NO";
        CURL_USE_SCHANNEL = "YES";
        CURL_WINDOWS_SSPI = "YES";
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
        USE_OPENSSL = "NO";
        USE_SSLS_EXPORT = "NO";
        USE_WIN32_IDN = "YES";
        USE_WIN32_LARGE_FILES = "YES";
        USE_WIN32_LDAP = "NO";
        USE_WINDOWS_SSPI = "YES";
        ZLIB_INCLUDE_DIR = "$BinaryCache\$($Platform.Triple)\usr\include";
        ZLIB_LIBRARY = "$BinaryCache\$($Platform.Triple)\usr\lib\zs.lib";
      }
  }
}

$SupplementalRuntimes = @(
  "StringProcessing",
  "Synchronization",
  "Distributed",
  "Observation",
  "Differentiation",
  "Volatile",
  "Runtime"
)

function Build-SDK([string] $Variant, [bool] $Static) {
  $BUILD_SHARED_LIBS  = if ($Static) { "NO" } else { "YES" }
  $RuntimeBinaryCache = Get-ProjectBinaryCache "${Variant}Runtime"
  $OverlayBinaryCache = Get-ProjectBinaryCache "${Variant}Overlay"

  # The runtime DLLs are installed beside the toolchain image, not in the SDK.
  $InstallDefines = @{
    CMAKE_INSTALL_BINDIR = "$RuntimeRoot\usr\bin";
    CMAKE_INSTALL_LIBEXECDIR = "$RuntimeRoot\usr\libexec";
  }
  $RuntimeDefines = $InstallDefines + @{
    BUILD_SHARED_LIBS = $BUILD_SHARED_LIBS;
    CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
    SWIFT_ASSEMBLY_VERSION = "$ProductVersion";
    SWIFT_PUBLIC_KEY_TOKEN = "$WindowsSxSAssemblyPublicKeyToken";
  }

  # TODO: remove this once the migration is completed. Resync writes into the
  # source tree, which the instances building other architectures share.
  Invoke-Exclusively "Resync" {
    Invoke-IsolatingEnvVars {
      Invoke-VsDevShell $BuildPlatform

      Push-Location "${SourceCache}\swift\Runtimes"
      $env:CMAKE_ROOT = $CMake.CMakeRoot
      Start-Process -Wait -WindowStyle Hidden -FilePath $CMake.Path -ArgumentList @("-P", "Resync.cmake")
      Pop-Location
    }
  }

  # TODO(compnerd) build statically for static builds
  Invoke-Step "${Variant}CDispatch" {
    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-libdispatch `
      -Bin (Get-ProjectBinaryCache "${Variant}CDispatch") `
      -BuildTargets default `
      -CCompiler $Compilers.C `
      -CXXCompiler $Compilers.CXX `
      -Defines @{
        BUILD_SHARED_LIBS = "YES";
        BUILD_TESTING = "NO";
        ENABLE_SWIFT = "NO";
      }
  }

  Build-SDKDependencies

  # ── Core ──────────────────────────────────────────────────────────────────
  Invoke-Step "${Variant}Runtime" {
    Build-CMakeProject `
      -Src $SourceCache\swift\Runtimes\Core `
      -Bin $RuntimeBinaryCache `
      -InstallTo "$SDKRoot\usr" `
      -CCompiler $Compilers.GNUC `
      -CXXCompiler $Compilers.GNUCXX `
      -SwiftCompiler $Compilers.Swift `
      -Defines ($RuntimeDefines + @{
        CMAKE_NINJA_FORCE_RESPONSE_FILE = "YES";

        dispatch_DIR = (Get-ProjectCMakeModules "${Variant}CDispatch");

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

  # ── Overlay ───────────────────────────────────────────────────────────────
  Invoke-Step "${Variant}Overlay" {
    Build-CMakeProject `
      -Src $SourceCache\swift\Runtimes\Overlay `
      -Bin $OverlayBinaryCache `
      -InstallTo "$SDKRoot\usr" `
      -CCompiler $Compilers.GNUC `
      -CXXCompiler $Compilers.GNUCXX `
      -SwiftCompiler $Compilers.Swift `
      -Defines ($RuntimeDefines + @{
        SwiftCore_DIR = "$RuntimeBinaryCache\cmake\SwiftCore";

        SwiftOverlay_ENABLE_CXX_INTEROP = "YES";
        # FIXME(compnerd) this currently causes a build failure on Windows, but
        # this should be enabled when building the dynamic runtime.
        SwiftOverlay_ENABLE_LIBRARY_EVOLUTION = "NO";
      })
  }

  # ── Supplementals ─────────────────────────────────────────────────────────
  # FIXME(compnerd) library evolution currently causes a build failure on
  # Windows, but it should be enabled when building the dynamic runtime.
  $Supplementals = [ordered]@{
    StringProcessing = @{
      C = $true; CXX = $true;
      Defines = @{
        SwiftCore_DIR = "$RuntimeBinaryCache\cmake\SwiftCore";
        SwiftStringProcessing_ENABLE_LIBRARY_EVOLUTION = "NO";
      };
    };
    Synchronization = @{
      C = $true; CXX = $false;
      Defines = @{
        SwiftCore_DIR    = "$RuntimeBinaryCache\cmake\SwiftCore";
        SwiftOverlay_DIR = "$OverlayBinaryCache\cmake\SwiftOverlay";
        SwiftSynchronization_ENABLE_LIBRARY_EVOLUTION = "NO";
      };
    };
    Distributed = @{
      C = $true; CXX = $true;
      Defines = @{
        # FIXME(#83449): avoid using `SwiftCMakeConfig.h`
        CMAKE_CXX_FLAGS  = @("-I$RuntimeBinaryCache\include");
        SwiftCore_DIR    = "$RuntimeBinaryCache\cmake\SwiftCore";
        SwiftOverlay_DIR = "$OverlayBinaryCache\cmake\SwiftOverlay";
        SwiftDistributed_ENABLE_LIBRARY_EVOLUTION = "NO";
      };
    };
    Observation = @{
      C = $false; CXX = $true;
      Defines = @{
        # FIXME(#83449): avoid using `SwiftCMakeConfig.h`
        CMAKE_CXX_FLAGS  = @("-I$RuntimeBinaryCache\include");
        SwiftCore_DIR    = "$RuntimeBinaryCache\cmake\SwiftCore";
        SwiftOverlay_DIR = "$OverlayBinaryCache\cmake\SwiftOverlay";
        SwiftObservation_ENABLE_LIBRARY_EVOLUTION = "NO";
      };
    };
    Differentiation = @{
      C = $true; CXX = $true;
      Defines = @{
        SwiftCore_DIR    = "$RuntimeBinaryCache\cmake\SwiftCore";
        SwiftOverlay_DIR = "$OverlayBinaryCache\cmake\SwiftOverlay";
        SwiftDifferentiation_ENABLE_LIBRARY_EVOLUTION = "NO";
      };
    };
    Volatile = @{
      C = $true; CXX = $false;
      Defines = @{
        CMAKE_FIND_PACKAGE_PREFER_CONFIG = "YES";
        SwiftCore_DIR    = "$RuntimeBinaryCache\cmake\SwiftCore";
        SwiftOverlay_DIR = "$OverlayBinaryCache\cmake\SwiftOverlay";
        SwiftVolatile_ENABLE_LIBRARY_EVOLUTION = "NO";
      };
    };
    Runtime = @{
      C = $true; CXX = $true;
      Project = "RuntimeModule";
      Defines = @{
        CMAKE_FIND_PACKAGE_PREFER_CONFIG = "YES";
        SwiftCore_DIR       = "$RuntimeBinaryCache\cmake\SwiftCore";
        SwiftOverlay_DIR    = "$OverlayBinaryCache\cmake\SwiftOverlay";
        SwiftCxxOverlay_DIR = "$OverlayBinaryCache\Cxx\cmake\SwiftCxxOverlay";
        SwiftRuntime_ENABLE_LIBRARY_EVOLUTION = "NO";
        SwiftRuntime_ENABLE_BACKTRACING = "YES";
      };
    };
  }

  foreach ($Name in $Supplementals.Keys) {
    if ($SupplementalRuntimes -notcontains $Name) { continue }
    $Supplemental = $Supplementals[$Name]
    $Project = if ($Supplemental.ContainsKey("Project")) { $Supplemental.Project } else { $Name }

    Invoke-Step "${Variant}${Project}" {
      Build-CMakeProject `
        -Src $SourceCache\swift\Runtimes\Supplemental\$Name `
        -Bin (Get-ProjectBinaryCache "${Variant}${Project}") `
        -InstallTo "$SDKRoot\usr" `
        -CCompiler $(if ($Supplemental.C) { $Compilers.GNUC } else { $null }) `
        -CXXCompiler $(if ($Supplemental.CXX) { $Compilers.GNUCXX } else { $null }) `
        -SwiftCompiler $Compilers.Swift `
        -Defines ($RuntimeDefines + $Supplemental.Defines)
    }
  }

  # ── Dispatch ──────────────────────────────────────────────────────────────
  $StaticSwiftFlags = if ($Static) {
    @("-static-stdlib", "-Xfrontend", "-use-static-resource-dir")
  } else {
    @()
  }

  Invoke-Step "${Variant}Dispatch" {
    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-libdispatch `
      -Bin (Get-ProjectBinaryCache "${Variant}Dispatch") `
      -InstallTo "$SDKRoot\usr" `
      -CCompiler $Compilers.GNUC `
      -CXXCompiler $Compilers.GNUCXX `
      -SwiftCompiler $Compilers.Swift `
      -SwiftSDK $SDKRoot `
      -Defines ($InstallDefines + @{
        BUILD_TESTING                     = "NO";
        BUILD_SHARED_LIBS                 = $BUILD_SHARED_LIBS;
        CMAKE_FIND_PACKAGE_PREFER_CONFIG  = "YES";
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
        CMAKE_Swift_FLAGS                 = $StaticSwiftFlags;
        ENABLE_SWIFT                      = "YES";
        dispatch_INSTALL_ARCH_SUBDIR      = "YES";
      })
  }

  Repair-SDKHeaders

  # Point the module map path recorded in Dispatch.swiftmodule and the one that
  # swiftc finds through -isystem at the same installed file. Otherwise a fresh
  # build cannot load the underlying module for 'Dispatch' and an incremental
  # build sees a redefinition of module 'Dispatch'.
  $DispatchVFS = "$(Get-ProjectBinaryCache "${Variant}Dispatch")\dispatch-vfs-overlay.yaml"
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

  # ── Foundation ────────────────────────────────────────────────────────────
  Invoke-Step "${Variant}Foundation" {
    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-foundation `
      -Bin (Get-ProjectBinaryCache "${Variant}Foundation") `
      -InstallTo "$SDKRoot\usr" `
      -CCompiler $Compilers.GNUC `
      -CXXCompiler $Compilers.GNUCXX `
      -SwiftCompiler $Compilers.Swift `
      -SwiftSDK $SDKRoot `
      -Defines ($InstallDefines + @{
        BUILD_SHARED_LIBS                 = $BUILD_SHARED_LIBS;
        CMAKE_FIND_PACKAGE_PREFER_CONFIG  = "YES";
        CMAKE_NINJA_FORCE_RESPONSE_FILE   = "YES";
        CMAKE_STATIC_LIBRARY_PREFIX_Swift = "lib";
        CMAKE_Swift_FLAGS                 = $StaticSwiftFlags;
        ENABLE_TESTING                    = "NO";
        BROTLI_INCLUDE_DIR                = "$SourceCache\brotli\c\include";
        BROTLICOMMON_LIBRARY              = "$(Get-ProjectBinaryCache Brotli)\brotlicommon.lib";
        BROTLIDEC_LIBRARY                 = "$(Get-ProjectBinaryCache Brotli)\brotlidec.lib";
        FOUNDATION_BUILD_TOOLS            = "YES";
        CURL_DIR                          = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake\CURL";
        LibXml2_DIR                       = "$BinaryCache\$($Platform.Triple)\usr\lib\cmake\libxml2-2.11.5";
        ZLIB_LIBRARY                      = "$BinaryCache\$($Platform.Triple)\usr\lib\zs.lib";
        ZLIB_INCLUDE_DIR                  = "$BinaryCache\$($Platform.Triple)\usr\include";
        dispatch_DIR                      = Get-ProjectCMakeModules "${Variant}Dispatch";
        _SwiftFoundation_SourceDIR        = "$SourceCache\swift-foundation";
        _SwiftFoundationICU_SourceDIR     = "$SourceCache\swift-foundation-icu";
        _SwiftCollections_SourceDIR       = "$SourceCache\swift-collections";
        SwiftFoundation_MACRO             = $FoundationMacros;
      })
  }

  # ── Backtrace (static 64-bit only) ────────────────────────────────────────
  if ($Static -and $Platform.Architecture.ShortName -ne "x86") {
    Invoke-Step "Backtrace" {
      Build-CMakeProject `
        -Src $SourceCache\swift\Runtimes\Supplemental\StackWalker `
        -Bin (Get-ProjectBinaryCache Backtrace) `
        -InstallTo "$SDKRoot\usr" `
        -CXXCompiler $Compilers.GNUCXX `
        -SwiftCompiler $Compilers.Swift `
        -Defines ($InstallDefines + @{
          CMAKE_Swift_FLAGS   = @("-static-stdlib");
          SwiftCore_DIR       = "$RuntimeBinaryCache\cmake\SwiftCore";
          SwiftCxxOverlay_DIR = "$OverlayBinaryCache\Cxx\cmake\SwiftCxxOverlay";
          SwiftOverlay_DIR    = "$OverlayBinaryCache\cmake\SwiftOverlay";
          SwiftRuntime_DIR    = "$(Get-ProjectBinaryCache "${Variant}RuntimeModule")\cmake\SwiftRuntime";
        })
    }
  }

  # ── SDK metadata ──────────────────────────────────────────────────────────
  Repair-SDKHeaders
  Write-SDKSettings

  # ── Testing frameworks ────────────────────────────────────────────────────
  if (-not $Static) {
    Invoke-Step "Testing" {
      Build-CMakeProject `
        -Src $SourceCache\swift-testing `
        -Bin (Get-ProjectBinaryCache Testing) `
        -InstallTo "$PlatformRoot\Developer\Library\Testing-$ProductVersion\usr" `
        -CXXCompiler $Compilers.CXX `
        -SwiftCompiler $Compilers.Swift `
        -SwiftSDK $SDKRoot `
        -Defines @{
          BUILD_SHARED_LIBS = "YES";
          CMAKE_INSTALL_BINDIR = $Platform.BinaryDir;
          CMAKE_Swift_FLAGS = @();
          SwiftTesting_MACRO = $TestingMacros;
          SwiftTesting_INSTALL_NESTED_SUBDIR = "YES";
        }
    }

    Invoke-Step "XCTest" {
      Build-CMakeProject `
        -Src $SourceCache\swift-corelibs-xctest `
        -Bin (Get-ProjectBinaryCache XCTest) `
        -InstallTo "$PlatformRoot\Developer\Library\XCTest-$ProductVersion\usr" `
        -SwiftCompiler $Compilers.Swift `
        -SwiftSDK $SDKRoot `
        -Defines @{
          BUILD_SHARED_LIBS = "YES";
          CMAKE_INSTALL_BINDIR = $Platform.BinaryDir;
          CMAKE_Swift_FLAGS = @();
          ENABLE_TESTING = "NO";
          XCTest_INSTALL_NESTED_SUBDIR = "YES";
          SwiftTesting_DIR = (Get-ProjectCMakeModules Testing);
          SwiftTestingMacros_DIR = (Get-ProjectCMakeModules BootstrapTestingMacros);
        }
    }

    # TODO(compnerd): remove once these runtime projects embed their Windows
    # assembly manifests in their own builds.
    Repair-WindowsSDKAssemblyManifests
  }
}

#-------------------------------------------------------------------

$BuildStopwatch = [Diagnostics.Stopwatch]::StartNew()
try {
  Get-Artifact $CMake
  Get-Artifact $Ninja

  if ($LinkModes -contains "dynamic") {
    Build-SDK -Variant "Dynamic" -Static $false
  }
  if ($LinkModes -contains "static") {
    Build-SDK -Variant "Static" -Static $true
  }

  ConvertTo-ThickLayout -Resources "$SDKRoot\usr\lib\swift\windows"        -Filter @("*.lib")
  ConvertTo-ThickLayout -Resources "$SDKRoot\usr\lib\swift_static\windows" -Filter @("*.lib")

  # Copy the static dependencies.
  if ($LinkModes -contains "static") {
    $SwiftResourceDir = "$SDKRoot\usr\lib\swift_static\windows\$($Platform.Architecture.LLVMName)"
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache Brotli)\brotlicommon.lib" -Destination "$SwiftResourceDir\brotlicommon.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache Brotli)\brotlidec.lib" -Destination "$SwiftResourceDir\brotlidec.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache CURL)\lib\libcurl.lib" -Destination "$SwiftResourceDir\libcurl.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache XML2)\libxml2s.lib" -Destination "$SwiftResourceDir\libxml2s.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache ZLib)\zs.lib" -Destination "$SwiftResourceDir\zs.lib" | Out-Null
  }
} catch {
  Write-Host -ForegroundColor Red "Error: $_"
  Write-Host -ForegroundColor Red $_.ScriptStackTrace
  exit 1
} finally {
  if ($Summary) {
    Write-Host "Windows $Architecture SDK built in $($BuildStopwatch.Elapsed.ToString("hh\:mm\:ss"))"
    $Timings | Format-Table -AutoSize | Out-String | Write-Host
  }
}
