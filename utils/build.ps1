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

.PARAMETER BuildType
The CMake build type to use, one of: Release, RelWithDebInfo, Debug.

.PARAMETER CDebugFormat
The debug information format for C/C++ code: dwarf or codeview.

.PARAMETER SwiftDebugFormat
The debug information format for Swift code: dwarf or codeview.

.PARAMETER SDKs
An array of architectures for which the Swift SDK should be built.

.PARAMETER ProductVersion
The product version to be used when building the installer.
Supports semantic version strings.

.PARAMETER PinnedBranch
The branch for the snapshot to build the early components with.

.PARAMETER PinnedToolchain
The toolchain snapshot to build the early components with.

.PARAMETER WinSDKVersion
The version number of the Windows SDK to be used.
Overrides the value resolved by the Visual Studio command prompt.
If no such Windows SDK is installed, it will be downloaded from nuget.

.PARAMETER SkipBuild
If set, does not run the build phase.

.PARAMETER SkipPackaging
If set, skips building the msi's and installer

.PARAMETER DefaultsLLD
If false, use `link.exe` as the default linker with the SDK (with SPM)

.PARAMETER Test
An array of names of projects to run tests for.
'*' runs all tests

.PARAMETER Stage
The path to a directory where built msi's and the installer executable should be staged (for CI).

.PARAMETER BuildTo
The name of a build step after which the script should terminate.
For example: -BuildTo ToolsSupportCore

.PARAMETER ToBatch
When set, runs the script in a special mode which outputs a listing of command invocations
in batch file format instead of executing them.

.EXAMPLE
PS> .\Build.ps1

.EXAMPLE
PS> .\Build.ps1 -SDKs x64 -ProductVersion 1.2.3 -Test foundation,xctest
#>
[CmdletBinding(PositionalBinding = $false)]
param(
  [string] $SourceCache = "S:\SourceCache",
  [string] $BinaryCache = "S:\b",
  [string] $ImageRoot = "S:",
  [string] $BuildType = "Release",
  [string] $CDebugFormat = "dwarf",
  [string] $SwiftDebugFormat = "dwarf",
  [string[]] $SDKs = @("X64","X86","Arm64"),
  [string] $ProductVersion = "0.0.0",
  [string] $PinnedBranch = "swift-5.9-release",
  [string] $PinnedToolchain = "swift-5.9-RELEASE",
  [string] $WinSDKVersion = "",
  [switch] $SkipBuild = $false,
  [switch] $SkipRedistInstall = $false,
  [switch] $SkipPackaging = $false,
  [bool] $DefaultsLLD = $true,
  [string[]] $Test = @(),
  [string] $Stage = "",
  [string] $BuildTo = "",
  [switch] $ToBatch
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version 3.0

# Avoid being run in a "Developer" shell since this script launches its own sub-shells targeting
# different architectures, and these variables cause confusion.
if ($null -ne $env:VSCMD_ARG_HOST_ARCH -or $null -ne $env:VSCMD_ARG_TGT_ARCH) {
  throw "At least one of VSCMD_ARG_HOST_ARCH and VSCMD_ARG_TGT_ARCH is set, which is incompatible with this script. Likely need to run outside of a Developer shell."
}

# Prevent elsewhere-installed swift modules from confusing our builds.
$env:SDKROOT = ""
$NativeProcessorArchName = $env:PROCESSOR_ARCHITEW6432
if ($null -eq $NativeProcessorArchName) { $NativeProcessorArchName = $env:PROCESSOR_ARCHITECTURE }

# Store the revision zero variant of the Windows SDK version (no-op if unspecified)
$WindowsSDKMajorMinorBuildMatch = [Regex]::Match($WinSDKVersion, "^\d+\.\d+\.\d+")
$WinSDKVersionRevisionZero = if ($WindowsSDKMajorMinorBuildMatch.Success) { $WindowsSDKMajorMinorBuildMatch.Value + ".0" } else { "" }
$CustomWinSDKRoot = $null # Overwritten if we download a Windows SDK from nuget

$vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$VSInstallRoot = & $vswhere -nologo -latest -products "*" -all -prerelease -property installationPath
$msbuild = "$VSInstallRoot\MSBuild\Current\Bin\$NativeProcessorArchName\MSBuild.exe"

# Avoid $env:ProgramFiles in case this script is running as x86
$UnixToolsBinDir = "$env:SystemDrive\Program Files\Git\usr\bin"

$python = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Shared\Python39_64\python.exe"
if (-not (Test-Path $python)) {
  $python = (where.exe python) | Select-Object -First 1
  if (-not (Test-Path $python)) {
    throw "Python.exe not found"
  }
}

# Work around limitations of cmd passing in array arguments via powershell.exe -File
if ($SDKs.Length -eq 1) { $SDKs = $SDKs[0].Split(",") }
if ($Test.Length -eq 1) { $Test = $Test[0].Split(",") }

if ($Test -contains "*") {
  # Explicitly don't include llbuild yet since tests are known to fail on Windows
  $Test = @("swift", "dispatch", "foundation", "xctest")
}

# Architecture definitions
$ArchX64 = @{
  VSName = "amd64";
  ShortName = "x64";
  LLVMName = "x86_64";
  LLVMTarget = "x86_64-unknown-windows-msvc";
  CMakeName = "AMD64";
  BinaryDir = "bin64";
  BuildID = 100;
  BinaryCache = "$BinaryCache\x64";
  PlatformInstallRoot = "$BinaryCache\x64\Windows.platform";
  SDKInstallRoot = "$BinaryCache\x64\Windows.platform\Developer\SDKs\Windows.sdk";
  XCTestInstallRoot = "$BinaryCache\x64\Windows.platform\Developer\Library\XCTest-development";
  ToolchainInstallRoot = "$BinaryCache\x64\toolchains\$ProductVersion+Asserts";
}

$ArchX86 = @{
  VSName = "x86";
  ShortName = "x86";
  LLVMName = "i686";
  LLVMTarget = "i686-unknown-windows-msvc";
  CMakeName = "i686";
  BinaryDir = "bin32";
  BuildID = 200;
  BinaryCache = "$BinaryCache\x86";
  PlatformInstallRoot = "$BinaryCache\x86\Windows.platform";
  SDKInstallRoot = "$BinaryCache\x86\Windows.platform\Developer\SDKs\Windows.sdk";
  XCTestInstallRoot = "$BinaryCache\x86\Windows.platform\Developer\Library\XCTest-development";
}

$ArchARM64 = @{
  VSName = "arm64";
  ShortName = "arm64";
  LLVMName = "aarch64";
  LLVMTarget = "aarch64-unknown-windows-msvc";
  CMakeName = "aarch64";
  BinaryDir = "bin64a";
  BuildID = 300;
  BinaryCache = "$BinaryCache\arm64";
  PlatformInstallRoot = "$BinaryCache\arm64\Windows.platform";
  SDKInstallRoot = "$BinaryCache\arm64\Windows.platform\Developer\SDKs\Windows.sdk";
  XCTestInstallRoot = "$BinaryCache\arm64\Windows.platform\Developer\Library\XCTest-development";
  ToolchainInstallRoot = "$BinaryCache\arm64\toolchains\$ProductVersion+Asserts";
}

$HostArch = switch ($NativeProcessorArchName) {
  "AMD64" { $ArchX64 }
  "ARM64" { $ArchARM64 }
  default { throw "Unsupported processor architecture" }
}

function Get-InstallDir($Arch) {
  if ($Arch -eq $HostArch) {
    $ProgramFilesName = "Program Files"
  } elseif ($Arch -eq $ArchX86) {
    $ProgramFilesName = "Program Files (x86)"
  } elseif (($HostArch -eq $ArchArm64) -and ($Arch -eq $ArchX64)) {
    # x64 programs actually install under "Program Files" on arm64,
    # but this would conflict with the native installation.
    $ProgramFilesName = "Program Files (Amd64)"
  } else {
    # arm64 cannot be installed on x64
    return $null
  }
  return "$ImageRoot\$ProgramFilesName\Swift"
}

$NugetRoot = "$BinaryCache\nuget"

$LibraryRoot = "$ImageRoot\Library"
$ToolchainInstallRoot = "$(Get-InstallDir $HostArch)\Toolchains\$ProductVersion+Asserts"
$PlatformInstallRoot = "$(Get-InstallDir $HostArch)\Platforms\Windows.platform"
$RuntimeInstallRoot = "$(Get-InstallDir $HostArch)\Runtimes\$ProductVersion"
$SDKInstallRoot = "$PlatformInstallRoot\Developer\SDKs\Windows.sdk"

# For dev productivity, install the host toolchain directly using CMake.
# This allows iterating on the toolchain using ninja builds.
$HostArch.ToolchainInstallRoot = $ToolchainInstallRoot

# Resolve the architectures received as argument
$SDKArchs = @($SDKs | ForEach-Object {
  switch ($_) {
    "X64" { $ArchX64 }
    "X86" { $ArchX86 }
    "Arm64" { $ArchArm64 }
    default { throw "Unknown architecture $_" }
  }
})

# Build functions
function Invoke-BuildStep([string]$Name) {
  & $Name @Args
  if ($Name.Replace("Build-", "") -eq $BuildTo) {
    exit 0
  }
}

function Get-ProjectBinaryCache($Arch, $ID) {
  return "$BinaryCache\" + ($Arch.BuildID + $ID)
}

function Copy-File($Src, $Dst) {
  # Create the directory tree first so Copy-Item succeeds
  # If $Dst is the target directory, make sure it ends with "\"
  $DstDir = [IO.Path]::GetDirectoryName($Dst)
  New-Item -ItemType Directory -ErrorAction Ignore $DstDir | Out-Null
  Copy-Item -Force $Src $Dst
}

function Copy-Directory($Src, $Dst) {
  New-Item -ItemType Directory -ErrorAction Ignore $Dst | Out-Null
  Copy-Item -Force -Recurse $Src $Dst
}

function Invoke-Program() {
  [CmdletBinding(PositionalBinding = $false)]
  param(
    [Parameter(Position = 0, Mandatory = $true)]
    [string] $Executable,
    [switch] $OutNull = $false,
    [string] $OutFile = "",
    [Parameter(Position = 1, ValueFromRemainingArguments)]
    [string[]] $Args
  )

  if ($ToBatch) {
    # Print the invocation in batch file-compatible format
    $OutputLine = "`"$Executable`""
    $ShouldBreakLine = $false
    for ($i = 0; $i -lt $Args.Length; $i++) {
      if ($ShouldBreakLine -or $OutputLine.Length -ge 40) {
        $OutputLine += " ^"
        Write-Output $OutputLine
        $OutputLine = "  "
      }

      $Arg = $Args[$i]
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
    } elseif ("" -ne $OutFile) {
      $OutputLine += " > `"$OutFile`""
    }

    Write-Output $OutputLine
  } else {
    if ($OutNull) {
      & $Executable @Args | Out-Null
    } elseif ("" -ne $OutFile) {
      & $Executable @Args | Out-File -Encoding UTF8 $OutFile
    } else {
      & $Executable @Args
    }

    if ($LastExitCode -ne 0) {
      $ErrorMessage = "Error: $([IO.Path]::GetFileName($Executable)) exited with code $($LastExitCode).`n"

      $ErrorMessage += "Invocation:`n"
      $ErrorMessage += "  $Executable $Args`n"

      $ErrorMessage += "Call stack:`n"
      foreach ($Frame in @(Get-PSCallStack)) {
        $ErrorMessage += "  $Frame`n"
      }

      throw $ErrorMessage
    }
  }
}

function Isolate-EnvVars([scriptblock]$Block) {
  if ($ToBatch) {
    Write-Output "setlocal enableextensions enabledelayedexpansion"
  }

  $OldVars = @{}
  foreach ($Var in (Get-ChildItem env:*).GetEnumerator()) {
    $OldVars.Add($Var.Key, $Var.Value)
  }

  & $Block

  Remove-Item env:*
  foreach ($Var in $OldVars.GetEnumerator()) {
    New-Item -Path "env:\$($Var.Key)" -Value $Var.Value -ErrorAction Ignore | Out-Null
  }

  if ($ToBatch) {
    Write-Output "endlocal"
  }
}

function Invoke-VsDevShell($Arch) {
  $DevCmdArguments = "-no_logo -host_arch=$($HostArch.VSName) -arch=$($Arch.VSName)"
  if ($CustomWinSDKRoot) {
    $DevCmdArguments += " -winsdk=none"
  } elseif ($WinSDKVersion) {
    $DevCmdArguments += " -winsdk=$WinSDKVersionRevisionZero"
  }

  if ($ToBatch) {
    Write-Output "call `"$VSInstallRoot\Common7\Tools\VsDevCmd.bat`" $DevCmdArguments"
  } else {
    # This dll path is valid for VS2019 and VS2022, but it was under a vsdevcmd subfolder in VS2017 
    Import-Module "$VSInstallRoot\Common7\Tools\Microsoft.VisualStudio.DevShell.dll"
    Enter-VsDevShell -VsInstallPath $VSInstallRoot -SkipAutomaticLocation -DevCmdArguments $DevCmdArguments

    if ($CustomWinSDKRoot) {
      # Using a non-installed Windows SDK. Setup environment variables manually.
      $WinSDKVerIncludeRoot = "$CustomWinSDKRoot\include\$WinSDKVersionRevisionZero"
      $WinSDKIncludePath = "$WinSDKVerIncludeRoot\ucrt;$WinSDKVerIncludeRoot\um;$WinSDKVerIncludeRoot\shared;$WinSDKVerIncludeRoot\winrt;$WinSDKVerIncludeRoot\cppwinrt"
      $WinSDKVerLibRoot = "$CustomWinSDKRoot\lib\$WinSDKVersionRevisionZero"

      $env:WindowsLibPath = "$CustomWinSDKRoot\UnionMetadata\$WinSDKVersionRevisionZero;$CustomWinSDKRoot\References\$WinSDKVersionRevisionZero"
      $env:WindowsSdkBinPath = "$CustomWinSDKRoot\bin"
      $env:WindowsSDKLibVersion = "$WinSDKVersionRevisionZero\"
      $env:WindowsSdkVerBinPath = "$CustomWinSDKRoot\bin\$WinSDKVersionRevisionZero"
      $env:WindowsSDKVersion = "$WinSDKVersionRevisionZero\"

      $env:EXTERNAL_INCLUDE += ";$WinSDKIncludePath"
      $env:INCLUDE += ";$WinSDKIncludePath"
      $env:LIB += ";$WinSDKVerLibRoot\ucrt\$($Arch.ShortName);$WinSDKVerLibRoot\um\$($Arch.ShortName)"
      $env:LIBPATH += ";$env:WindowsLibPath"
      $env:PATH += ";$env:WindowsSdkVerBinPath\$($Arch.ShortName);$env:WindowsSdkBinPath\$($Arch.ShortName)"
      $env:UCRTVersion = $WinSDKVersionRevisionZero
      $env:UniversalCRTSdkDir = $CustomWinSDKRoot
    }
  }
}

function Ensure-WindowsSDK {
  # Assume we always have a default Windows SDK version available
  if (-not $WinSDKVersion) { return }

  # Check whether VsDevShell can already resolve the requested Windows SDK version
  try {
    Isolate-EnvVars { Invoke-VsDevShell $HostArch }
    return
  } catch {}

  Write-Output "Windows SDK $WinSDKVersion not found. Downloading from nuget..."

  # Assume the requested Windows SDK is not available and we need to download it
  # Install the base nuget package that contains header files
  $WinSDKBasePackageName = "Microsoft.Windows.SDK.CPP"
  Invoke-Program nuget install $WinSDKBasePackageName -Version $WinSDKVersion -OutputDirectory $NugetRoot

  # Export to script scope so Invoke-VsDevShell can read it
  $script:CustomWinSDKRoot = "$NugetRoot\$WinSDKBasePackageName.$WinSDKVersion\c"

  # Install each required arch-specific package and move the files under the base /lib directory
  $WinSDKArchs = $SDKArchs.Clone()
  if (-not ($HostArch -in $WinSDKArchs)) {
    $WinSDKArchs += $HostArch
  }

  foreach ($Arch in $WinSDKArchs) {
    $WinSDKArchPackageName = "$WinSDKBasePackageName.$($Arch.ShortName)"
    Invoke-Program nuget install $WinSDKArchPackageName -Version $WinSDKVersion -OutputDirectory $NugetRoot
    Copy-Directory "$NugetRoot\$WinSDKArchPackageName.$WinSDKVersion\c\*" "$CustomWinSDKRoot\lib\$WinSDKVersionRevisionZero"
  }
}

function Ensure-SwiftToolchain($Arch) {
  if (Test-Path "$BinaryCache\toolchains\$PinnedToolchain\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr\bin\swiftc.exe") {
    return
  }

  if (-not (Test-Path $BinaryCache\wix-4.0.1.zip)) {
    Invoke-Program curl.exe -sL https://www.nuget.org/api/v2/package/wix/4.0.1 --output $BinaryCache\wix-4.0.1.zip --create-dirs
  }
  New-Item -ItemType Directory -ErrorAction Ignore $BinaryCache\wix-4.0.1 | Out-Null
  Expand-Archive -Path $BinaryCache\wix-4.0.1.zip -Destination $BinaryCache\wix-4.0.1 -Force

  Write-Output "Swift toolchain not found. Downloading from swift.org..."
  $SwiftToolchainURL = "https://swift.org/builds/${PinnedBranch}/windows10/${PinnedToolchain}/${PinnedToolchain}-windows10.exe"
  New-Item -ItemType Directory -ErrorAction Ignore "$BinaryCache\toolchains" | Out-Null
  if (-not (Test-Path "$BinaryCache\toolchains\${PinnedToolchain}.exe")) {
    (New-Object Net.WebClient).DownloadFile($SwiftToolchainURL, "$BinaryCache\toolchains\${PinnedToolchain}.exe")
  }

  Write-Output "Installing Swift toolchain..."
  Invoke-Program "$BinaryCache\wix-4.0.1\tools\net6.0\any\wix.exe" -- burn extract "$BinaryCache\toolchains\${PinnedToolchain}.exe" -out "$BinaryCache\toolchains\"
  Invoke-Program -OutNull msiexec.exe /qn /a "$BinaryCache\toolchains\a0" TARGETDIR="$BinaryCache\toolchains\${PinnedToolchain}"
  Invoke-Program -OutNull msiexec.exe /qn /a "$BinaryCache\toolchains\a1" TARGETDIR="$BinaryCache\toolchains\${PinnedToolchain}"
  Invoke-Program -OutNull msiexec.exe /qn /a "$BinaryCache\toolchains\a2" TARGETDIR="$BinaryCache\toolchains\${PinnedToolchain}"
  Invoke-Program -OutNull msiexec.exe /qn /a "$BinaryCache\toolchains\a3" TARGETDIR="$BinaryCache\toolchains\${PinnedToolchain}"
}

function TryAdd-KeyValue([hashtable]$Hashtable, [string]$Key, [string]$Value) {
  if (-not $Hashtable.Contains($Key)) {
    $Hashtable.Add($Key, $Value)
  }
}

function Append-FlagsDefine([hashtable]$Defines, [string]$Name, [string[]]$Value) {
  if ($Defines.Contains($Name)) {
    $Defines[$name] = @($Defines[$name]) + $Value
  } else {
    $Defines.Add($Name, $Value)
  }
}

function Test-CMakeAtLeast([int]$Major, [int]$Minor, [int]$Patch = 0) {
  if ($ToBatch) { return $false }

  $CMakeVersionString = @(& cmake.exe --version)[0]
  if (-not ($CMakeVersionString -match "^cmake version (\d+)\.(\d+)(?:\.(\d+))?")) {
    throw "Unexpected CMake version string format"
  }

  if ([int]$Matches.1 -ne $Major) { return [int]$Matches.1 -gt $Major }
  if ([int]$Matches.2 -ne $Minor) { return [int]$Matches.2 -gt $Minor }
  if ($null -eq $Matches.3) { return 0 -gt $Patch }
  return [int]$Matches.3 -ge $Patch
}

function Build-CMakeProject {
  [CmdletBinding(PositionalBinding = $false)]
  param(
    [string] $Src,
    [string] $Bin,
    [string] $InstallTo = "",
    [hashtable] $Arch,
    [string] $Generator = "Ninja",
    [string] $CacheScript = "",
    [string[]] $UseMSVCCompilers = @(), # C,CXX
    [string[]] $UseBuiltCompilers = @(), # ASM,C,CXX,Swift
    [string[]] $UsePinnedCompilers = @(), # ASM,C,CXX,Swift
    [string] $SwiftSDK = "",
    [hashtable] $Defines = @{}, # Values are either single strings or arrays of flags
    [string[]] $BuildTargets = @()
  )

  if ($ToBatch) {
    Write-Output ""
    Write-Output "echo Building '$Src' to '$Bin' for arch '$($Arch.ShortName)'..."
  } else {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Building '$Src' to '$Bin' for arch '$($Arch.ShortName)'..."
  }

  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()

  # Enter the developer command shell early so we can resolve cmake.exe
  # for version checks.
  Isolate-EnvVars {
    Invoke-VsDevShell $Arch

    # Add additional defines (unless already present)
    $Defines = $Defines.Clone()
  
    TryAdd-KeyValue $Defines CMAKE_BUILD_TYPE $BuildType
    TryAdd-KeyValue $Defines CMAKE_MT "mt"

    $GenerateDebugInfo = $Defines["CMAKE_BUILD_TYPE"] -ne "Release"

    $CFlags = @("/GS-", "/Gw", "/Gy", "/Oi", "/Oy", "/Zc:inline")
    if ($GenerateDebugInfo) { $CFlags += "/Zi" }
    $CXXFlags = $CFlags.Clone() + "/Zc:__cplusplus"

    if ($UseMSVCCompilers.Contains("C")) {
      TryAdd-KeyValue $Defines CMAKE_C_COMPILER cl
      Append-FlagsDefine $Defines CMAKE_C_FLAGS $CFlags
    }
    if ($UseMSVCCompilers.Contains("CXX")) {
      TryAdd-KeyValue $Defines CMAKE_CXX_COMPILER cl
      Append-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXFlags
    }
    if ($UsePinnedCompilers.Contains("ASM") -Or $UseBuiltCompilers.Contains("ASM")) {
      if ($UseBuiltCompilers.Contains("ASM")) {
        TryAdd-KeyValue $Defines CMAKE_ASM_COMPILER "$BinaryCache\1\bin\clang-cl.exe"
      } else {
        TryAdd-KeyValue $Defines CMAKE_ASM_COMPILER "$BinaryCache\toolchains\$PinnedToolchain\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr\bin\clang-cl.exe"
      }
      Append-FlagsDefine $Defines CMAKE_ASM_FLAGS "--target=$($Arch.LLVMTarget)"
      TryAdd-KeyValue $Defines CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDLL "/MD"
    }
    if ($UsePinnedCompilers.Contains("C") -Or $UseBuiltCompilers.Contains("C")) {
      if ($UseBuiltCompilers.Contains("C")) {
        TryAdd-KeyValue $Defines CMAKE_C_COMPILER "$BinaryCache\1\bin\clang-cl.exe"
      } else {
        TryAdd-KeyValue $Defines CMAKE_ASM_COMPILER "$BinaryCache\toolchains\$PinnedToolchain\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr\bin\clang-cl.exe"
      }
      TryAdd-KeyValue $Defines CMAKE_C_COMPILER_TARGET $Arch.LLVMTarget

      if (-not (Test-CMakeAtLeast -Major 3 -Minor 26 -Patch 3)) {
        # Workaround for https://github.com/ninja-build/ninja/issues/2280
        TryAdd-KeyValue $Defines CMAKE_CL_SHOWINCLUDES_PREFIX "Note: including file: "
      }

      if ($GenerateDebugInfo -and $CDebugFormat -eq "dwarf") {
        Append-FlagsDefine $Defines CMAKE_C_FLAGS "-gdwarf"
      }
      Append-FlagsDefine $Defines CMAKE_C_FLAGS $CFlags
    }
    if ($UsePinnedCompilers.Contains("CXX") -Or $UseBuiltCompilers.Contains("CXX")) {
      if ($UseBuiltCompilers.Contains("CXX")) {
        TryAdd-KeyValue $Defines CMAKE_CXX_COMPILER "$BinaryCache\1\bin\clang-cl.exe"
      } else {
        TryAdd-KeyValue $Defines CMAKE_ASM_COMPILER "$BinaryCache\toolchains\$PinnedToolchain\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr\bin\clang-cl.exe"
      }
      TryAdd-KeyValue $Defines CMAKE_CXX_COMPILER_TARGET $Arch.LLVMTarget

      if (-not (Test-CMakeAtLeast -Major 3 -Minor 26 -Patch 3)) {
        # Workaround for https://github.com/ninja-build/ninja/issues/2280
        TryAdd-KeyValue $Defines CMAKE_CL_SHOWINCLUDES_PREFIX "Note: including file: "
      }

      if ($GenerateDebugInfo -and $CDebugFormat -eq "dwarf") {
        Append-FlagsDefine $Defines CMAKE_CXX_FLAGS "-gdwarf"
      }
      Append-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXFlags
    }
    if ($UsePinnedCompilers.Contains("Swift") -Or $UseBuiltCompilers.Contains("Swift")) {
      $SwiftArgs = @()

      if ($UseBuiltCompilers.Contains("Swift")) {
        TryAdd-KeyValue $Defines CMAKE_Swift_COMPILER "$BinaryCache\1\bin\swiftc.exe"
      } else {
        TryAdd-KeyValue $Defines CMAKE_Swift_COMPILER "$BinaryCache\toolchains\$PinnedToolchain\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr\bin\swiftc.exe"
      }
      TryAdd-KeyValue $Defines CMAKE_Swift_COMPILER_TARGET $Arch.LLVMTarget
      if ($UseBuiltCompilers.Contains("Swift")) {
        if ($SwiftSDK -ne "") {
          $SwiftArgs += @("-sdk", $SwiftSDK)
        } else {
          $RuntimeBinaryCache = Get-ProjectBinaryCache $Arch 1
          $SwiftResourceDir = "${RuntimeBinaryCache}\lib\swift"

          $SwiftArgs += @("-resource-dir", "$SwiftResourceDir")
          $SwiftArgs += @("-L", "$SwiftResourceDir\windows")
          $SwiftArgs += @("-vfsoverlay", "$RuntimeBinaryCache\stdlib\windows-vfs-overlay.yaml")
        }
      } else {
        $SwiftArgs += @("-sdk", "$BinaryCache\toolchains\$PinnedToolchain\Library\Developer\Platforms\Windows.platform\Developer\SDKs\Windows.sdk")
      }

      # Debug Information
      if ($GenerateDebugInfo) {
        if ($SwiftDebugFormat -eq "dwarf") {
          $SwiftArgs += @("-g", "-Xlinker", "/DEBUG:DWARF", "-use-ld=lld-link")
        } else {
          $SwiftArgs += @("-g", "-debug-info-format=codeview", "-Xlinker", "-debug")
        }
      } else {
        $SwiftArgs += "-gnone"
      }
      $SwiftArgs += @("-Xlinker", "/INCREMENTAL:NO")

      # Swift Requries COMDAT folding and de-duplication
      $SwiftArgs += @("-Xlinker", "/OPT:REF")
      $SwiftArgs += @("-Xlinker", "/OPT:ICF")
      Append-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftArgs

      # Workaround CMake 3.26+ enabling `-wmo` by default on release builds
      Append-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELEASE "-O"
      Append-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELWITHDEBINFO "-O"
    }
    if ("" -ne $InstallTo) {
      TryAdd-KeyValue $Defines CMAKE_INSTALL_PREFIX $InstallTo
    }

    # Generate the project
    $cmakeGenerateArgs = @("-B", $Bin, "-S", $Src, "-G", $Generator)
    if ("" -ne $CacheScript) {
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
            # Quote and escape the quote so it makes it through
            $Value += "\""$ArgWithForwardSlashes\"""
          } else {
            $Value += $ArgWithForwardSlashes
          }
        }
      }

      $cmakeGenerateArgs += @("-D", "$($Define.Key)=$Value")
    }

    if ($UseBuiltCompilers.Contains("Swift")) {
      $env:Path = "$($HostArch.SDKInstallRoot)\usr\bin;$ToolchainInstallRoot\usr\bin;${env:Path}"
    }
    Invoke-Program cmake.exe @cmakeGenerateArgs

    # Build all requested targets
    foreach ($Target in $BuildTargets) {
      if ($Target -eq "default") {
        Invoke-Program cmake.exe --build $Bin
      } else {
        Invoke-Program cmake.exe --build $Bin --target $Target
      }
    }

    if ("" -ne $InstallTo) {
      Invoke-Program cmake.exe --build $Bin --target install
    }
  }

  if (-not $ToBatch) {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Finished building '$Src' to '$Bin' for arch '$($Arch.ShortName)' in $($Stopwatch.Elapsed)"
    Write-Host ""
  }
}

function Build-SPMProject {
  [CmdletBinding(PositionalBinding = $false)]
  param(
    [string] $Src,
    [string] $Bin,
    [hashtable] $Arch,
    [Parameter(ValueFromRemainingArguments)]
    [string[]] $AdditionalArguments
  )

  if ($ToBatch) {
    Write-Output ""
    Write-Output "echo Building '$Src' to '$Bin' for arch '$($Arch.ShortName)'..."
  } else {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Building '$Src' to '$Bin' for arch '$($Arch.ShortName)'..."
  }

  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()

  Isolate-EnvVars {
    $env:Path = "$RuntimeInstallRoot\usr\bin;$ToolchainInstallRoot\usr\bin;${env:Path}"
    $env:SDKROOT = $SDKInstallRoot

    $Arguments = @(
        "--scratch-path", $Bin,
        "--package-path", $Src,
        "-c", "release",
        "-Xbuild-tools-swiftc", "-I$SDKInstallRoot\usr\lib\swift",
        "-Xbuild-tools-swiftc", "-L$SDKInstallRoot\usr\lib\swift\windows",
        "-Xcc", "-I$SDKInstallRoot\usr\lib\swift",
        "-Xlinker", "-L$SDKInstallRoot\usr\lib\swift\windows"
    )
    if ($BuildType -eq "Release") {
      $Arguments += @("-debug-info-format", "none")
    } else {
      if ($SwiftDebugFormat -eq "dwarf") {
        $Arguments += @("-debug-info-format", "dwarf")
      } else {
        $Arguments += @("-debug-info-format", "codeview")
      }
    }

    Invoke-Program "$ToolchainInstallRoot\usr\bin\swift.exe" "build" @Arguments @AdditionalArguments
  }

  if (-not $ToBatch) {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Finished building '$Src' to '$Bin' for arch '$($Arch.ShortName)' in $($Stopwatch.Elapsed)"
    Write-Host ""
  }
}

function Build-WiXProject() {
  [CmdletBinding(PositionalBinding = $false)]
  param(
    [Parameter(Position = 0, Mandatory = $true)]
    [string]$FileName,
    [Parameter(Mandatory = $true)]
    [hashtable]$Arch,
    [switch]$Bundle,
    [hashtable]$Properties = @{}
  )

  $ArchName = $Arch.VSName

  $ProductVersionArg = $ProductVersion
  if (-not $Bundle) {
    # WiX v4 will accept a semantic version string for Bundles,
    # but Packages still require a purely numerical version number, 
    # so trim any semantic versionning suffixes
    $ProductVersionArg = [regex]::Replace($ProductVersion, "[-+].*", "")
  }

  $Properties = $Properties.Clone()
  TryAdd-KeyValue $Properties Configuration Release
  TryAdd-KeyValue $Properties BaseOutputPath "$($Arch.BinaryCache)\installer\"
  TryAdd-KeyValue $Properties ProductArchitecture $ArchName
  TryAdd-KeyValue $Properties ProductVersion $ProductVersionArg

  $MSBuildArgs = @("$SourceCache\swift-installer-scripts\platforms\Windows\$FileName")
  $MSBuildArgs += "-noLogo"
  $MSBuildArgs += "-restore"
  $MSBuildArgs += "-maxCpuCount"
  foreach ($Property in $Properties.GetEnumerator()) {
    if ($Property.Value.Contains(" ")) {
      $MSBuildArgs += "-p:$($Property.Key)=$($Property.Value.Replace('\', '\\'))"
    } else {
      $MSBuildArgs += "-p:$($Property.Key)=$($Property.Value)"
    }
  }
  $MSBuildArgs += "-binaryLogger:$($Arch.BinaryCache)\msi\$ArchName-$([System.IO.Path]::GetFileNameWithoutExtension($FileName)).binlog"
  $MSBuildArgs += "-detailedSummary:False"

  Invoke-Program $msbuild @MSBuildArgs
}

function Build-BuildTools($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\llvm-project\llvm `
    -Bin $BinaryCache\0 `
    -Arch $Arch `
    -BuildTargets llvm-tblgen,clang-tblgen,clang-pseudo-gen,clang-tidy-confusable-chars-gen,lldb-tblgen,llvm-config,swift-def-to-strings-converter,swift-serialize-diagnostics,swift-compatibility-symbols `
    -Defines @{
      LLDB_ENABLE_PYTHON = "NO";
      LLDB_INCLUDE_TESTS = "NO";
      LLDB_ENABLE_SWIFT_SUPPORT = "NO";
      LLVM_ENABLE_ASSERTIONS = "NO";
      LLVM_ENABLE_LIBEDIT = "NO";
      LLVM_ENABLE_LIBXML2 = "NO";
      LLVM_ENABLE_PROJECTS = "clang;clang-tools-extra;lldb";
      LLVM_EXTERNAL_PROJECTS = "cmark;swift";
      LLVM_EXTERNAL_CMARK_SOURCE_DIR = "$SourceCache\cmark";
      LLVM_EXTERNAL_SWIFT_SOURCE_DIR = "$SourceCache\swift";
      SWIFT_BUILD_DYNAMIC_SDK_OVERLAY = "NO";
      SWIFT_BUILD_DYNAMIC_STDLIB = "NO";
      SWIFT_BUILD_REMOTE_MIRROR = "NO";
      SWIFT_BUILD_STATIC_SDK_OVERLAY = "NO";
      SWIFT_BUILD_STATIC_STDLIB = "NO";
      SWIFT_INCLUDE_DOCS = "NO";
      SWIFT_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
      SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE = "$SourceCache\swift-syntax";
    }
}

function Build-Compilers($Arch, [switch]$Test = $false) {
  Isolate-EnvVars {
    if ($Test) {
      $LibdispatchBinDir = "$BinaryCache\1\tools\swift\libdispatch-windows-$($Arch.LLVMName)-prefix\bin"
      $env:Path = "$LibdispatchBinDir;$BinaryCache\1\bin;$env:Path;$UnixToolsBinDir"
      $Targets = @("check-swift")
      $TestingDefines = @{
        SWIFT_BUILD_DYNAMIC_SDK_OVERLAY = "YES";
        SWIFT_BUILD_DYNAMIC_STDLIB = "YES";
        SWIFT_BUILD_REMOTE_MIRROR = "YES";
        SWIFT_NATIVE_SWIFT_TOOLS_PATH = "";
      }
    } else {
      $Targets = @("distribution", "install-distribution")
      $TestingDefines = @{
        SWIFT_BUILD_DYNAMIC_SDK_OVERLAY = "NO";
        SWIFT_BUILD_DYNAMIC_STDLIB = "NO";
        SWIFT_BUILD_REMOTE_MIRROR = "NO";
        SWIFT_NATIVE_SWIFT_TOOLS_PATH = "$BinaryCache\0\bin";
      }
    }

    $env:Path = "$BinaryCache\toolchains\$PinnedToolchain\PFiles64\Swift\runtime-development\usr\bin;${env:Path}"

    $LLVM_ENABLE_PDB = switch ($BuildType) {
      "Release" { "NO" }
      default { "YES" }
    }

    Build-CMakeProject `
      -Src $SourceCache\llvm-project\llvm `
      -Bin $BinaryCache\1 `
      -Arch $Arch `
      -UseMSVCCompilers C,CXX `
      -BuildTargets $Targets `
      -CacheScript $SourceCache\swift\cmake\caches\Windows-$($Arch.LLVMName).cmake `
      -Defines ($TestingDefines + @{
        CLANG_TABLEGEN = "$BinaryCache\0\bin\clang-tblgen.exe";
        CLANG_TIDY_CONFUSABLE_CHARS_GEN = "$BinaryCache\0\bin\clang-tidy-confusable-chars-gen.exe";
        # LLVM plays tricks with flags and prefers to use `LLVM_ENABLE_PDB` for
        # debug information on Windows rather than the CMake handling.  This
        # give us a sligtly faster build.
        CMAKE_BUILD_TYPE = "Release";
        CMAKE_INSTALL_PREFIX = "$($Arch.ToolchainInstallRoot)\usr";
        CMAKE_Swift_COMPILER = "$BinaryCache\toolchains\$PinnedToolchain\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr\bin\swiftc.exe";
        CMAKE_Swift_FLAGS = @("-sdk", "$BinaryCache\toolchains\$PinnedToolchain\Library\Developer\Platforms\Windows.platform\Developer\SDKs\Windows.sdk");
        LLDB_PYTHON_EXE_RELATIVE_PATH = "python.exe";
        LLDB_PYTHON_EXT_SUFFIX = ".pyd";
        LLDB_PYTHON_RELATIVE_PATH = "lib/site-packages";
        LLDB_TABLEGEN = "$BinaryCache\0\bin\lldb-tblgen.exe";
        LLVM_CONFIG_PATH = "$BinaryCache\0\bin\llvm-config.exe";
        LLVM_ENABLE_PDB = $LLVM_ENABLE_PDB;
        LLVM_EXTERNAL_CMARK_SOURCE_DIR = "$SourceCache\cmark";
        LLVM_EXTERNAL_SWIFT_SOURCE_DIR = "$SourceCache\swift";
        LLVM_NATIVE_TOOL_DIR = "$BinaryCache\0\bin";
        LLVM_TABLEGEN = "$BinaryCache\0\bin\llvm-tblgen.exe";
        LLVM_USE_HOST_TOOLS = "NO";
        SWIFT_BUILD_SWIFT_SYNTAX = "YES";
        SWIFT_CLANG_LOCATION = "$BinaryCache\toolchains\$PinnedToolchain\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr\bin";
        SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_CXX_INTEROP = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING = "YES";
        SWIFT_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
        SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE = "$SourceCache\swift-syntax";
        SWIFT_PATH_TO_STRING_PROCESSING_SOURCE = "$SourceCache\swift-experimental-string-processing";
        SWIFT_PATH_TO_SWIFT_SDK = "$BinaryCache\toolchains\$PinnedToolchain\Library\Developer\Platforms\Windows.platform\Developer\SDKs\Windows.sdk";
      })
  }
}

function Build-LLVM($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\llvm-project\llvm `
    -Bin (Get-ProjectBinaryCache $Arch 0) `
    -Arch $Arch `
    -Defines @{
      LLVM_HOST_TRIPLE = $Arch.LLVMTarget;
    }
}

function Build-ZLib($Arch) {
  $ArchName = $Arch.ShortName

  Build-CMakeProject `
    -Src $SourceCache\zlib `
    -Bin "$($Arch.BinaryCache)\zlib-1.3" `
    -InstallTo $LibraryRoot\zlib-1.3\usr `
    -Arch $Arch `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      INSTALL_BIN_DIR = "$LibraryRoot\zlib-1.3\usr\bin\$ArchName";
      INSTALL_LIB_DIR = "$LibraryRoot\zlib-1.3\usr\lib\$ArchName";
    }
}

function Build-XML2($Arch) {
  $ArchName = $Arch.ShortName

  Build-CMakeProject `
    -Src $SourceCache\libxml2 `
    -Bin "$($Arch.BinaryCache)\libxml2-2.11.5" `
    -InstallTo "$LibraryRoot\libxml2-2.11.5\usr" `
    -Arch $Arch `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_INSTALL_BINDIR = "bin/$ArchName";
      CMAKE_INSTALL_LIBDIR = "lib/$ArchName";
      LIBXML2_WITH_ICONV = "NO";
      LIBXML2_WITH_ICU = "NO";
      LIBXML2_WITH_LZMA = "NO";
      LIBXML2_WITH_PYTHON = "NO";
      LIBXML2_WITH_TESTS = "NO";
      LIBXML2_WITH_THREADS = "YES";
      LIBXML2_WITH_ZLIB = "NO";
    }
}

function Build-CURL($Arch) {
  $ArchName = $Arch.ShortName

  Build-CMakeProject `
    -Src $SourceCache\curl `
    -Bin "$($Arch.BinaryCache)\curl-8.4.0" `
    -InstallTo "$LibraryRoot\curl-8.4.0\usr" `
    -Arch $Arch `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_INSTALL_BINDIR = "bin/$ArchName";
      CMAKE_INSTALL_LIBDIR = "lib/$ArchName";
      BUILD_CURL_EXE = "NO";
      CMAKE_USE_OPENSSL = "NO";
      CURL_CA_PATH = "none";
      CMAKE_USE_SCHANNEL = "YES";
      CMAKE_USE_LIBSSH2 = "NO";
      HAVE_POLL_FINE = "NO";
      CURL_DISABLE_LDAP = "YES";
      CURL_DISABLE_LDAPS = "YES";
      CURL_DISABLE_TELNET = "YES";
      CURL_DISABLE_DICT = "YES";
      CURL_DISABLE_FILE = "YES";
      CURL_DISABLE_TFTP = "YES";
      CURL_DISABLE_RTSP = "YES";
      CURL_DISABLE_PROXY = "YES";
      CURL_DISABLE_POP3 = "YES";
      CURL_DISABLE_IMAP = "YES";
      CURL_DISABLE_SMTP = "YES";
      CURL_DISABLE_GOPHER = "YES";
      CURL_ZLIB = "YES";
      ENABLE_UNIX_SOCKETS = "NO";
      ENABLE_THREADED_RESOLVER = "NO";
      ZLIB_ROOT = "$LibraryRoot\zlib-1.3\usr";
      ZLIB_LIBRARY = "$LibraryRoot\zlib-1.3\usr\lib\$ArchName\zlibstatic.lib";
    }
}

function Build-ICU($Arch) {
  $ArchName = $Arch.ShortName

  if (-not $ToBatch) {
    if (-not (Test-Path -Path "$SourceCache\icu\icu4c\CMakeLists.txt")) {
      Copy-Item $SourceCache\swift-installer-scripts\shared\ICU\CMakeLists.txt $SourceCache\icu\icu4c\
      Copy-Item $SourceCache\swift-installer-scripts\shared\ICU\icupkg.inc.cmake $SourceCache\icu\icu4c\
    }
  }

  if ($Arch -eq $ArchARM64 -and $HostArch -ne $ArchARM64) {
    # Use previously built x64 tools
    $BuildToolsDefines = @{
      BUILD_TOOLS = "NO";
      ICU_TOOLS_DIR = "$($ArchX64.BinaryCache)\icu-69.1"
    }
  } else {
    $BuildToolsDefines = @{BUILD_TOOLS = "YES"}
  }

  Build-CMakeProject `
    -Src $SourceCache\icu\icu4c `
    -Bin "$($Arch.BinaryCache)\icu-69.1" `
    -InstallTo "$LibraryRoot\icu-69.1\usr" `
    -Arch $Arch `
    -BuildTargets default `
    -Defines ($BuildToolsDefines + @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_INSTALL_BINDIR = "bin/$ArchName";
      CMAKE_INSTALL_LIBDIR = "lib/$ArchName";
    })
}

function Build-Runtime($Arch) {
  $LLVMBinaryCache = Get-ProjectBinaryCache $Arch 0

  Isolate-EnvVars {
    $env:Path = "$BinaryCache\toolchains\$PinnedToolchain\PFiles64\Swift\runtime-development\usr\bin;${env:Path}"

    Build-CMakeProject `
      -Src $SourceCache\swift `
      -Bin (Get-ProjectBinaryCache $Arch 1) `
      -InstallTo "$($Arch.SDKInstallRoot)\usr" `
      -Arch $Arch `
      -CacheScript $SourceCache\swift\cmake\caches\Runtime-Windows-$($Arch.LLVMName).cmake `
      -UseBuiltCompilers C,CXX `
      -BuildTargets default `
      -Defines @{
        CMAKE_Swift_COMPILER_TARGET = $Arch.LLVMTarget;
        LLVM_DIR = "$LLVMBinaryCache\lib\cmake\llvm";
        SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_CXX_INTEROP = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING = "YES";
        SWIFT_NATIVE_SWIFT_TOOLS_PATH = "$BinaryCache\1\bin";
        SWIFT_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
        SWIFT_PATH_TO_STRING_PROCESSING_SOURCE = "$SourceCache\swift-experimental-string-processing";
        SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE = "$SourceCache\swift-syntax";
        CMAKE_SHARED_LINKER_FLAGS = @("/INCREMENTAL:NO", "/OPT:REF", "/OPT:ICF");
      }
  }

  Invoke-Program $python -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'DEFAULT_USE_RUNTIME': 'MD' } }), encoding='utf-8'))" `
    -OutFile "$($Arch.SDKInstallRoot)\SDKSettings.plist"
}

function Build-Dispatch($Arch, [switch]$Test = $false) {
  $Targets = if ($Test) { @("default", "ExperimentalTest") } else { @("default", "install") }

  Build-CMakeProject `
    -Src $SourceCache\swift-corelibs-libdispatch `
    -Bin (Get-ProjectBinaryCache $Arch 2) `
    -Arch $Arch `
    -UseBuiltCompilers C,CXX,Swift `
    -BuildTargets $Targets `
    -Defines @{
      CMAKE_INSTALL_PREFIX = "$($Arch.SDKInstallRoot)\usr";
      CMAKE_SYSTEM_NAME = "Windows";
      CMAKE_SYSTEM_PROCESSOR = $Arch.CMakeName;
      ENABLE_SWIFT = "YES";
    }
}

function Build-Foundation($Arch, [switch]$Test = $false) {
  $DispatchBinaryCache = Get-ProjectBinaryCache $Arch 2
  $FoundationBinaryCache = Get-ProjectBinaryCache $Arch 3
  $ShortArch = $Arch.ShortName

  Isolate-EnvVars {
    if ($Test) {
      $RuntimeBinaryCache = Get-ProjectBinaryCache $Arch 1
      $XCTestBinaryCache = Get-ProjectBinaryCache $Arch 4
      $TestingDefines = @{
        ENABLE_TESTING = "YES";
        XCTest_DIR = "$XCTestBinaryCache\cmake\modules";
      }
      $Targets = @("default", "test")
      $env:Path = "$XCTestBinaryCache;$FoundationBinaryCache\bin;$DispatchBinaryCache;$RuntimeBinaryCache\bin;$env:Path"
    } else {
      $TestingDefines = @{ ENABLE_TESTING = "NO" }
      $Targets = @("default", "install")
    }

    $env:CTEST_OUTPUT_ON_FAILURE = 1
    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-foundation `
      -Bin $FoundationBinaryCache `
      -Arch $Arch `
      -UseBuiltCompilers ASM,C,Swift `
      -BuildTargets $Targets `
      -Defines (@{
        CMAKE_INSTALL_PREFIX = "$($Arch.SDKInstallRoot)\usr";
        CMAKE_SYSTEM_NAME = "Windows";
        CMAKE_SYSTEM_PROCESSOR = $Arch.CMakeName;
        # Turn off safeseh for lld as it has safeseh enabled by default
        # and fails with an ICU data object file icudt69l_dat.obj. This
        # matters to X86 only.
        CMAKE_Swift_FLAGS = if ($Arch -eq $ArchX86) { @("-Xlinker", "/SAFESEH:NO") } else { "" };
        CURL_DIR = "$LibraryRoot\curl-8.4.0\usr\lib\$ShortArch\cmake\CURL";
        ICU_DATA_LIBRARY_RELEASE = "$LibraryRoot\icu-69.1\usr\lib\$ShortArch\sicudt69.lib";
        ICU_I18N_LIBRARY_RELEASE = "$LibraryRoot\icu-69.1\usr\lib\$ShortArch\sicuin69.lib";
        ICU_ROOT = "$LibraryRoot\icu-69.1\usr";
        ICU_UC_LIBRARY_RELEASE = "$LibraryRoot\icu-69.1\usr\lib\$ShortArch\sicuuc69.lib";
        LIBXML2_LIBRARY = "$LibraryRoot\libxml2-2.11.5\usr\lib\$ShortArch\libxml2s.lib";
        LIBXML2_INCLUDE_DIR = "$LibraryRoot\libxml2-2.11.5\usr\include\libxml2";
        LIBXML2_DEFINITIONS = "/DLIBXML_STATIC";
        ZLIB_LIBRARY = "$LibraryRoot\zlib-1.3\usr\lib\$ShortArch\zlibstatic.lib";
        ZLIB_INCLUDE_DIR = "$LibraryRoot\zlib-1.3\usr\include";
        dispatch_DIR = "$DispatchBinaryCache\cmake\modules";
      } + $TestingDefines)
  }
}

function Build-XCTest($Arch, [switch]$Test = $false) {
  $LLVMBinaryCache = Get-ProjectBinaryCache $Arch 0
  $DispatchBinaryCache = Get-ProjectBinaryCache $Arch 2
  $FoundationBinaryCache = Get-ProjectBinaryCache $Arch 3
  $XCTestBinaryCache = Get-ProjectBinaryCache $Arch 4

  Isolate-EnvVars {
    if ($Test) {
      $RuntimeBinaryCache = Get-ProjectBinaryCache $Arch 1
      $TestingDefines = @{
        ENABLE_TESTING = "YES";
        LLVM_DIR = "$LLVMBinaryCache/lib/cmake/llvm";
        XCTEST_PATH_TO_LIBDISPATCH_BUILD = $DispatchBinaryCache;
        XCTEST_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
        XCTEST_PATH_TO_FOUNDATION_BUILD = $FoundationBinaryCache;
      }
      $Targets = @("default", "check-xctest")
      $env:Path = "$XCTestBinaryCache;$FoundationBinaryCache\bin;$DispatchBinaryCache;$RuntimeBinaryCache\bin;$env:Path;$UnixToolsBinDir"
    } else {
      $TestingDefines = @{ ENABLE_TESTING = "NO" }
      $Targets = @("default", "install")
    }

    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-xctest `
      -Bin $XCTestBinaryCache `
      -Arch $Arch `
      -UseBuiltCompilers Swift `
      -BuildTargets $Targets `
      -Defines (@{
        CMAKE_INSTALL_PREFIX = "$($Arch.XCTestInstallRoot)\usr";
        CMAKE_SYSTEM_NAME = "Windows";
        CMAKE_SYSTEM_PROCESSOR = $Arch.CMakeName;
        dispatch_DIR = "$DispatchBinaryCache\cmake\modules";
        Foundation_DIR = "$FoundationBinaryCache\cmake\modules";
      } + $TestingDefines)

    if ($DefaultsLLD) {
      Invoke-Program $python -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'XCTEST_VERSION': 'development', 'SWIFTC_FLAGS': ['-use-ld=lld'] } }), encoding='utf-8'))" `
        -OutFile "$($Arch.PlatformInstallRoot)\Info.plist"
    } else {
      Invoke-Program $python -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'XCTEST_VERSION': 'development' } }), encoding='utf-8'))" `
        -OutFile "$($Arch.PlatformInstallRoot)\Info.plist"
    }
  }
}

# Copies files installed by CMake from the arch-specific platform root,
# where they follow the layout expected by the installer,
# to the final platform root, following the installer layout.
function Install-Platform($Arch) {
  if ($ToBatch) { return }

  New-Item -ItemType Directory -ErrorAction Ignore $SDKInstallRoot\usr | Out-Null

  # Copy SDK header files
  Copy-Directory "$($Arch.SDKInstallRoot)\usr\include\swift\SwiftRemoteMirror" $SDKInstallRoot\usr\include\swift
  Copy-Directory "$($Arch.SDKInstallRoot)\usr\lib\swift\shims" $SDKInstallRoot\usr\lib\swift
  foreach ($Module in ("Block", "dispatch", "os")) {
    Copy-Directory "$($Arch.SDKInstallRoot)\usr\lib\swift\$Module" $SDKInstallRoot\usr\include
  }

  # Copy SDK share folder
  Copy-File "$($Arch.SDKInstallRoot)\usr\share\*.*" $SDKInstallRoot\usr\share\

  # Copy SDK libs, placing them in an arch-specific directory
  $WindowsLibSrc = "$($Arch.SDKInstallRoot)\usr\lib\swift\windows"
  $WindowsLibDst = "$SDKInstallRoot\usr\lib\swift\windows"

  Copy-File "$WindowsLibSrc\*.lib" "$WindowsLibDst\$($Arch.LLVMName)\"
  Copy-File "$WindowsLibSrc\$($Arch.LLVMName)\*.lib" "$WindowsLibDst\$($Arch.LLVMName)\"

  # Copy well-structured SDK modules
  Copy-Directory "$WindowsLibSrc\*.swiftmodule" "$WindowsLibDst\"

  # Copy files from the arch subdirectory, including "*.swiftmodule" which need restructuring
  Get-ChildItem -Recurse "$WindowsLibSrc\$($Arch.LLVMName)" | ForEach-Object {
    if (".swiftmodule", ".swiftdoc", ".swiftinterface" -contains $_.Extension) {
      $DstDir = "$WindowsLibDst\$($_.BaseName).swiftmodule"
      Copy-File $_.FullName "$DstDir\$($Arch.LLVMTarget)$($_.Extension)"
    } else {
      Copy-File $_.FullName "$WindowsLibDst\$($Arch.LLVMName)\"
    }
  }

  # Copy the CxxShim module
  foreach ($Source in ("libcxxshim.h", "libcxxshim.modulemap", "libcxxstdlibshim.h")) {
    Copy-File "$WindowsLibSrc\$Source" "$WindowsLibDst"
  }

  # Copy plist files (same across architectures)
  Copy-File "$($Arch.PlatformInstallRoot)\Info.plist" $PlatformInstallRoot\
  Copy-File "$($Arch.SDKInstallRoot)\SDKSettings.plist" $SDKInstallRoot\

  # Copy XCTest
  $XCTestInstallRoot = "$PlatformInstallRoot\Developer\Library\XCTest-development"
  Copy-File "$($Arch.XCTestInstallRoot)\usr\bin\XCTest.dll" "$XCTestInstallRoot\usr\$($Arch.BinaryDir)\"
  Copy-File "$($Arch.XCTestInstallRoot)\usr\lib\swift\windows\XCTest.lib" "$XCTestInstallRoot\usr\lib\swift\windows\$($Arch.LLVMName)\"
  Copy-File "$($Arch.XCTestInstallRoot)\usr\lib\swift\windows\$($Arch.LLVMName)\XCTest.swiftmodule" "$XCTestInstallRoot\usr\lib\swift\windows\XCTest.swiftmodule\$($Arch.LLVMTarget).swiftmodule"
  Copy-File "$($Arch.XCTestInstallRoot)\usr\lib\swift\windows\$($Arch.LLVMName)\XCTest.swiftdoc" "$XCTestInstallRoot\usr\lib\swift\windows\XCTest.swiftmodule\$($Arch.LLVMTarget).swiftdoc"
}

function Build-SQLite($Arch) {
  $SrcPath = "$SourceCache\sqlite-3.43.2"

  # Download the sources
  if (-not (Test-Path $SrcPath)) {
    $ZipPath = "$env:TEMP\sqlite-amalgamation-3430200.zip"
    if (-not $ToBatch) { Remove-item $ZipPath -ErrorAction Ignore | Out-Null }
    Invoke-Program curl.exe -- -sL https://sqlite.org/2023/sqlite-amalgamation-3430200.zip -o $ZipPath

    if (-not $ToBatch) { New-Item -Type Directory -Path $SrcPath -ErrorAction Ignore | Out-Null }
    Invoke-Program "$UnixToolsBinDir\unzip.exe" -- -j -o $ZipPath -d $SrcPath
    if (-not $ToBatch) { Remove-item $ZipPath | Out-Null }

    if (-not $ToBatch) {
      # Inject a CMakeLists.txt so we can build sqlite
@"
cmake_minimum_required(VERSION 3.12.3)
project(SQLite LANGUAGES C)

set(CMAKE_POSITION_INDEPENDENT_CODE YES)
add_library(SQLite3 sqlite3.c)

if(CMAKE_SYSTEM_NAME STREQUAL Windows AND BUILD_SHARED_LIBS)
  target_compile_definitions(SQLite3 PRIVATE "SQLITE_API=__declspec(dllexport)")
endif()

install(TARGETS SQLite3
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  RUNTIME DESTINATION bin)
install(FILES sqlite3.h sqlite3ext.h DESTINATION include)
"@ | Out-File -Encoding UTF8 $SrcPath\CMakeLists.txt
    }
  }

  Build-CMakeProject `
    -Src $SrcPath `
    -Bin "$($Arch.BinaryCache)\sqlite-3.43.2" `
    -InstallTo $LibraryRoot\sqlite-3.43.2\usr `
    -Arch $Arch `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
    }
}

function Build-System($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\swift-system `
    -Bin $BinaryCache\2 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
    }
}

function Build-ToolsSupportCore($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\swift-tools-support-core `
    -Bin $BinaryCache\3 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      SwiftSystem_DIR = "$BinaryCache\2\cmake\modules";
    }
}

function Build-LLBuild($Arch, [switch]$Test = $false) {
  Isolate-EnvVars {
    if ($Test) {
      # Build additional llvm executables needed by tests
      Isolate-EnvVars {
        Invoke-VsDevShell $HostArch
        Invoke-Program ninja.exe -C "$BinaryCache\0" FileCheck not
      }

      $Targets = @("default", "test-llbuild")
      $TestingDefines = @{
        FILECHECK_EXECUTABLE = "$BinaryCache\0\bin\FileCheck.exe";
        LIT_EXECUTABLE = "$SourceCache\llvm-project\llvm\utils\lit\lit.py";
      }
      $env:Path = "$env:Path;$UnixToolsBinDir"
      $env:AR = "$BinaryCache\1\llvm-ar.exe"
      $env:CLANG = "$BinaryCache\1\clang.exe"
    } else {
      $Targets = @("default", "install")
      $TestingDefines = @{}
    }

    Build-CMakeProject `
      -Src $SourceCache\llbuild `
      -Bin $BinaryCache\4 `
      -Arch $Arch `
      -UseMSVCCompilers CXX `
      -UseBuiltCompilers Swift `
      -SwiftSDK $SDKInstallRoot `
      -BuildTargets $Targets `
      -Defines ($TestingDefines + @{
        CMAKE_INSTALL_PREFIX = "$($Arch.ToolchainInstallRoot)\usr";
        BUILD_SHARED_LIBS = "YES";
        LLBUILD_SUPPORT_BINDINGS = "Swift";
        SQLite3_INCLUDE_DIR = "$LibraryRoot\sqlite-3.43.2\usr\include";
        SQLite3_LIBRARY = "$LibraryRoot\sqlite-3.43.2\usr\lib\SQLite3.lib";
      })
  }
}

function Build-Yams($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\Yams `
    -Bin $BinaryCache\5 `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      BUILD_TESTING = "NO";
    }
}

function Build-ArgumentParser($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\swift-argument-parser `
    -Bin $BinaryCache\6 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      BUILD_TESTING = "NO";
    }
}

function Build-Driver($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\swift-driver `
    -Bin $BinaryCache\7 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      SwiftSystem_DIR = "$BinaryCache\2\cmake\modules";
      TSC_DIR = "$BinaryCache\3\cmake\modules";
      LLBuild_DIR = "$BinaryCache\4\cmake\modules";
      Yams_DIR = "$BinaryCache\5\cmake\modules";
      ArgumentParser_DIR = "$BinaryCache\6\cmake\modules";
      SQLite3_INCLUDE_DIR = "$LibraryRoot\sqlite-3.43.2\usr\include";
      SQLite3_LIBRARY = "$LibraryRoot\sqlite-3.43.2\usr\lib\SQLite3.lib";
    }
}

function Build-Crypto($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\swift-crypto `
    -Bin $BinaryCache\8 `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
    }
}

function Build-Collections($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\swift-collections `
    -Bin $BinaryCache\9 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
    }
}

function Build-ASN1($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\swift-asn1 `
    -Bin $BinaryCache\10 `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
    }
}

function Build-Certificates($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\swift-certificates `
    -Bin $BinaryCache\11 `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      SwiftASN1_DIR = "$BinaryCache\10\cmake\modules";
      SwiftCrypto_DIR = "$BinaryCache\8\cmake\modules";
    }
}

function Build-PackageManager($Arch) {
  $SrcPath = "$SourceCache\swift-package-manager"
  if (-not (Test-Path -PathType Container $SrcPath)) {
    # The Apple CI clones this repo as "swiftpm"
    $SrcPath = "$SourceCache\swiftpm"
  }

  Build-CMakeProject `
    -Src $SrcPath `
    -Bin $BinaryCache\12 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_Swift_FLAGS = @("-DCRYPTO_v2");
      SwiftSystem_DIR = "$BinaryCache\2\cmake\modules";
      TSC_DIR = "$BinaryCache\3\cmake\modules";
      LLBuild_DIR = "$BinaryCache\4\cmake\modules";
      ArgumentParser_DIR = "$BinaryCache\6\cmake\modules";
      SwiftDriver_DIR = "$BinaryCache\7\cmake\modules";
      SwiftCrypto_DIR = "$BinaryCache\8\cmake\modules";
      SwiftCollections_DIR = "$BinaryCache\9\cmake\modules";
      SwiftASN1_DIR = "$BinaryCache\10\cmake\modules";
      SwiftCertificates_DIR = "$BinaryCache\11\cmake\modules";
      SQLite3_INCLUDE_DIR = "$LibraryRoot\sqlite-3.43.2\usr\include";
      SQLite3_LIBRARY = "$LibraryRoot\sqlite-3.43.2\usr\lib\SQLite3.lib";
    }
}

function Build-IndexStoreDB($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\indexstore-db `
    -Bin $BinaryCache\13 `
    -Arch $Arch `
    -UseBuiltCompilers C,CXX,Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_C_FLAGS = @("-Xclang", "-fno-split-cold-code", "-I$SDKInstallRoot\usr\include", "-I$SDKInstallRoot\usr\include\Block");
      CMAKE_CXX_FLAGS = @("-Xclang", "-fno-split-cold-code", "-I$SDKInstallRoot\usr\include", "-I$SDKInstallRoot\usr\include\Block");
    }
}

function Build-Syntax($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\swift-syntax `
    -Bin $BinaryCache\14 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
    }
}

function Build-SourceKitLSP($Arch) {
  Build-CMakeProject `
    -Src $SourceCache\sourcekit-lsp `
    -Bin $BinaryCache\15 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      SwiftSystem_DIR = "$BinaryCache\2\cmake\modules";
      TSC_DIR = "$BinaryCache\3\cmake\modules";
      LLBuild_DIR = "$BinaryCache\4\cmake\modules";
      ArgumentParser_DIR = "$BinaryCache\6\cmake\modules";
      SwiftCrypto_DIR = "$BinaryCache\8\cmake\modules";
      SwiftCollections_DIR = "$BinaryCache\9\cmake\modules";
      SwiftPM_DIR = "$BinaryCache\12\cmake\modules";
      IndexStoreDB_DIR = "$BinaryCache\13\cmake\modules";
      SwiftSyntax_DIR = "$BinaryCache\14\cmake\modules";
    }
}

function Install-HostToolchain() {
  if ($ToBatch) { return }

  # We've already special-cased $HostArch.ToolchainInstallRoot to point to $ToolchainInstallRoot.
  # There are only a few extra restructuring steps we need to take care of.

  # Restructure _InternalSwiftScan (keep the original one for the installer)
  Copy-Item -Force `
    $ToolchainInstallRoot\usr\lib\swift\_InternalSwiftScan `
    $ToolchainInstallRoot\usr\include
  Copy-Item -Force `
    $ToolchainInstallRoot\usr\lib\swift\windows\_InternalSwiftScan.lib `
    $ToolchainInstallRoot\usr\lib

  # Switch to swift-driver
  Copy-Item -Force $BinaryCache\7\bin\swift-driver.exe $ToolchainInstallRoot\usr\bin\swift.exe
  Copy-Item -Force $BinaryCache\7\bin\swift-driver.exe $ToolchainInstallRoot\usr\bin\swiftc.exe
}

function Build-Inspect() {
  $OutDir = Join-Path -Path $HostArch.BinaryCache -ChildPath swift-inspect

  Build-SPMProject `
    -Src $SourceCache\swift\tools\swift-inspect `
    -Bin $OutDir `
    -Arch $HostArch `
    -Xcc "-I$SDKInstallRoot\usr\include\swift\SwiftRemoteMirror" -Xlinker "$SDKInstallRoot\usr\lib\swift\windows\$($HostArch.LLVMName)\swiftRemoteMirror.lib" `
    -Xcc -Xclang -Xcc -fno-split-cold-code # Workaround https://github.com/llvm/llvm-project/issues/40056
}

function Build-Format() {
  $OutDir = Join-Path -Path $HostArch.BinaryCache -ChildPath swift-format

  Isolate-EnvVars {
    $env:SWIFTCI_USE_LOCAL_DEPS=1
    Build-SPMProject `
      -Src $SourceCache\swift-format `
      -Bin $OutDir `
      -Arch $HostArch
  }
}

function Build-DocC() {
  $OutDir = Join-Path -Path $HostArch.BinaryCache -ChildPath swift-docc

  Isolate-EnvVars {
    $env:SWIFTCI_USE_LOCAL_DEPS=1
    Build-SPMProject `
      -Src $SourceCache\swift-docc `
      -Bin $OutDir `
      -Arch $HostArch `
      --product docc
  }
}

function Build-Installer() {
  $Properties = @{
    BundleFlavor = "offline";
    DEVTOOLS_ROOT = "$($HostArch.ToolchainInstallRoot)\";
    TOOLCHAIN_ROOT = "$($HostArch.ToolchainInstallRoot)\";
    INCLUDE_SWIFT_FORMAT = "true";
    SWIFT_FORMAT_BUILD = "$($HostArch.BinaryCache)\swift-format\release";
    INCLUDE_SWIFT_INSPECT = "true";
    SWIFT_INSPECT_BUILD = "$($HostArch.BinaryCache)\swift-inspect\release";
    INCLUDE_SWIFT_DOCC = "true";
    SWIFT_DOCC_BUILD = "$($HostArch.BinaryCache)\swift-docc\release";
    SWIFT_DOCC_RENDER_ARTIFACT_ROOT = "${SourceCache}\swift-docc-render-artifact";
  }

  Isolate-EnvVars {
    Invoke-VsDevShell $HostArch
    $VCRedistInstallerPath = "${env:VCToolsRedistDir}\vc_redist.$($HostArch.ShortName).exe"
    if (Test-Path $VCRedistInstallerPath) {
      $Properties["VCRedistInstaller"] = $VCRedistInstallerPath
      $Properties["VSVersion"] = $env:VSCMD_VER
    }
  }

  foreach ($Arch in $SDKArchs) {
    $Properties["INCLUDE_$($Arch.VSName.ToUpperInvariant())_SDK"] = "true"
    $Properties["PLATFORM_ROOT_$($Arch.VSName.ToUpperInvariant())"] = "$($Arch.PlatformInstallRoot)\"
    $Properties["SDK_ROOT_$($Arch.VSName.ToUpperInvariant())"] = "$($Arch.SDKInstallRoot)\"
  }

  Build-WiXProject bundle\installer.wixproj -Arch $HostArch -Properties $Properties

  if ($Stage -and (-not $ToBatch)) {
    Copy-File "$($HostArch.BinaryCache)\installer\Release\$($HostArch.VSName)\*.cab" "$Stage\"
    Copy-File "$($HostArch.BinaryCache)\installer\Release\$($HostArch.VSName)\*.msi" "$Stage\"
    Copy-File "$($HostArch.BinaryCache)\installer\Release\$($HostArch.VSName)\*.msm" "$Stage\"
    Copy-File "$($HostArch.BinaryCache)\installer\Release\$($HostArch.VSName)\installer.exe" "$Stage\"
    # Extract installer engine to ease code-signing on swift.org CI
    New-Item -Type Directory -Path "$($HostArch.BinaryCache)\installer\$($HostArch.VSName)\" -ErrorAction Ignore | Out-Null
    Invoke-Program "$BinaryCache\wix-4.0.1\tools\net6.0\any\wix.exe" -- burn detach "$($HostArch.BinaryCache)\installer\Release\$($HostArch.VSName)\installer.exe" -engine "$Stage\installer-engine.exe" -intermediateFolder "$($HostArch.BinaryCache)\installer\$($HostArch.VSName)\"
  }
}

#-------------------------------------------------------------------

if (-not $SkipBuild) {
  Ensure-WindowsSDK
}

if (-not $SkipBuild) {
  Ensure-SwiftToolchain $HostArch
  Invoke-BuildStep Build-BuildTools $HostArch
  Invoke-BuildStep Build-Compilers $HostArch
}

foreach ($Arch in $SDKArchs) {
  if (-not $SkipBuild) {
    Invoke-BuildStep Build-ZLib $Arch
    Invoke-BuildStep Build-XML2 $Arch
    Invoke-BuildStep Build-CURL $Arch
    Invoke-BuildStep Build-ICU $Arch
    Invoke-BuildStep Build-LLVM $Arch

    # Build platform: SDK, Redist and XCTest
    Invoke-BuildStep Build-Runtime $Arch
    Invoke-BuildStep Build-Dispatch $Arch
    Invoke-BuildStep Build-Foundation $Arch
    Invoke-BuildStep Build-XCTest $Arch
  }
}

if (-not $ToBatch) {
  if ($HostArch -in $SDKArchs) {
    Remove-Item -Force -Recurse $RuntimeInstallRoot -ErrorAction Ignore
    Copy-Directory "$($HostArch.SDKInstallRoot)\usr\bin" "$RuntimeInstallRoot\usr"
  }

  Remove-Item -Force -Recurse $PlatformInstallRoot -ErrorAction Ignore
  foreach ($Arch in $SDKArchs) {
    Install-Platform $Arch
  }
}

if (-not $SkipBuild) {
  Invoke-BuildStep Build-SQLite $HostArch
  Invoke-BuildStep Build-System $HostArch
  Invoke-BuildStep Build-ToolsSupportCore $HostArch
  Invoke-BuildStep Build-LLBuild $HostArch
  Invoke-BuildStep Build-Yams $HostArch
  Invoke-BuildStep Build-ArgumentParser $HostArch
  Invoke-BuildStep Build-Driver $HostArch
  Invoke-BuildStep Build-Crypto $HostArch
  Invoke-BuildStep Build-Collections $HostArch
  Invoke-BuildStep Build-ASN1 $HostArch
  Invoke-BuildStep Build-Certificates $HostArch
  Invoke-BuildStep Build-PackageManager $HostArch
  Invoke-BuildStep Build-IndexStoreDB $HostArch
  Invoke-BuildStep Build-Syntax $HostArch
  Invoke-BuildStep Build-SourceKitLSP $HostArch
}

Install-HostToolchain

if (-not $SkipBuild) {
  Invoke-BuildStep Build-Inspect $HostArch
  Invoke-BuildStep Build-Format $HostArch
  Invoke-BuildStep Build-DocC $HostArch
}

if (-not $SkipPackaging) {
  Invoke-BuildStep Build-Installer
}

if ($Test -contains "swift") { Build-Compilers $HostArch -Test }
if ($Test -contains "dispatch") { Build-Dispatch $HostArch -Test }
if ($Test -contains "foundation") { Build-Foundation $HostArch -Test }
if ($Test -contains "xctest") { Build-XCTest $HostArch -Test }
if ($Test -contains "llbuild") { Build-LLBuild $HostArch -Test }
