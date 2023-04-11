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

.PARAMETER LibraryRoot
The path to a directory where built libraries should be placed,
similar to the /Library or ~/Library directories on macOS.

.PARAMETER BuildType
The CMake build type to use, one of: Release, RelWithDebInfo, Debug.

.PARAMETER SDKs
An array of architectures for which the Swift SDK should be built.

.PARAMETER ProductVersion
The product version to be used when building the installer.
Supports semantic version strings.

.PARAMETER SkipRedistInstall
If set, does not create S:\Program Files to mimic an installed redistributable.

.PARAMETER SkipPackaging
If set, skips building the msi's and installer

.PARAMETER Test
An array of names of projects to run tests for.
'*' runs all tests

.PARAMETER Stage
The path to a directory where built msi's and the installer executable should be staged (for CI).

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
  [string] $LibraryRoot = "S:\Library",
  [string] $BuildType = "Release",
  [string[]] $SDKs = @("X64","X86","Arm64"),
  [string] $ProductVersion = "0.0.0",
  [switch] $SkipRedistInstall = $false,
  [switch] $SkipPackaging = $false,
  [string[]] $Test = @(),
  [string] $Stage = "",
  [switch] $ToBatch
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version 3.0

$ToolchainInstallRoot = "$LibraryRoot\Developer\Toolchains\unknown-Asserts-development.xctoolchain"
$PlatformInstallRoot = "$LibraryRoot\Developer\Platforms\Windows.platform"
$SDKInstallRoot = "$PlatformInstallRoot\Developer\SDKs\Windows.sdk"

$vswhere = "${Env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$VSInstallRoot = & $vswhere -nologo -latest -products "*" -all -prerelease -property installationPath
$msbuild = "$VSInstallRoot\MSBuild\Current\Bin\$env:PROCESSOR_ARCHITECTURE\MSBuild.exe"

$python = "${Env:ProgramFiles(x86)}\Microsoft Visual Studio\Shared\Python39_64\python.exe"
if (-not (Test-Path $python))
{
  $python = (where.exe python) | Select-Object -First 1
  if (-not (Test-Path $python))
  {
    throw "Python.exe not found"
  }
}

# Work around limitations of cmd passing in array arguments via powershell.exe -File
if ($SDKs.Length -eq 1) { $SDKs = $SDKs[0].Split(",") }
if ($Test.Length -eq 1) { $Test = $Test[0].Split(",") }

if ($Test -contains "*")
{
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
  BinaryRoot = "$BinaryCache\x64";
  PlatformInstallRoot = "$BinaryCache\x64\Windows.platform";
  SDKInstallRoot = "$BinaryCache\x64\Windows.platform\Developer\SDKs\Windows.sdk";
  XCTestInstallRoot = "$BinaryCache\x64\Windows.platform\Developer\Library\XCTest-development";
  ToolchainInstallRoot = "$BinaryCache\x64\unknown-Asserts-development.xctoolchain";
}

$ArchX86 = @{
  VSName = "x86";
  ShortName = "x86";
  LLVMName = "i686";
  LLVMTarget = "i686-unknown-windows-msvc";
  CMakeName = "i686";
  BinaryDir = "bin32";
  BuildID = 200;
  BinaryRoot = "$BinaryCache\x86";
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
  BinaryRoot = "$BinaryCache\arm64";
  PlatformInstallRoot = "$BinaryCache\arm64\Windows.platform";
  SDKInstallRoot = "$BinaryCache\arm64\Windows.platform\Developer\SDKs\Windows.sdk";
  XCTestInstallRoot = "$BinaryCache\arm64\Windows.platform\Developer\Library\XCTest-development";
  ToolchainInstallRoot = "$BinaryCache\arm64\unknown-Asserts-development.xctoolchain";
}

$HostArch = switch (${Env:PROCESSOR_ARCHITECTURE}) {
  'ARM64' { $ArchARM64 }
  default { $ArchX64 }
}

# Resolve the architectures received as argument
$SDKArchs = $SDKs | ForEach-Object {
  switch ($_) {
    "X64" { $ArchX64 }
    "X86" { $ArchX86 }
    "Arm64" { $ArchArm64 }
    default { throw "Unknown architecture $_" }
  }
}

# Build functions
function Get-ProjectBuildDir($Arch, $ID)
{
  return "$BinaryCache\" + ($Arch.BuildID + $ID)
}

function Invoke-Program()
{
  [CmdletBinding(PositionalBinding = $false)]
  param(
    [Parameter(Position = 0, Mandatory = $true)]
    [string] $Executable,
    [switch] $OutNull = $false,
    [string] $OutFile = "",
    [Parameter(Position = 1, ValueFromRemainingArguments)]
    [string[]] $Args
  )

  if ($ToBatch)
  {
    # Print the invocation in batch file-compatible format
    $OutputLine = "`"$Executable`""
    $ShouldBreakLine = $false
    for ($i = 0; $i -lt $Args.Length; $i++)
    {
      if ($ShouldBreakLine -or $OutputLine.Length -ge 40)
      {
        $OutputLine += " ^"
        Write-Output $OutputLine
        $OutputLine = "  "
      }

      $Arg = $Args[$i]
      if ($Arg.Contains(" "))
      {
        $OutputLine += " `"$Arg`""
      }
      else
      {
        $OutputLine += " $Arg"
      }

      # Break lines after non-switch arguments
      $ShouldBreakLine = -not $Arg.StartsWith("-")
    }

    if ($OutNull)
    {
      $OutputLine += " > nul"
    }
    elseif ("" -ne $OutFile)
    {
      $OutputLine += " > `"$OutFile`""
    }

    Write-Output $OutputLine
  }
  else
  {
    if ($OutNull)
    {
      & $Executable @Args | Out-Null
    }
    elseif ("" -ne $OutFile)
    {
      & $Executable @Args | Out-File -Encoding UTF8 $OutFile
    }
    else
    {
      & $Executable @Args
    }

    if ($LastExitCode -ne 0)
    {
      $callstack = @(Get-PSCallStack) -Join "`n"
      throw "Command execution returned $LastExitCode. Call stack:`n$callstack"
    }
  }
}

function Isolate-EnvVars([scriptblock]$Block)
{
  if ($ToBatch)
  {
    Write-Output "setlocal enableextensions enabledelayedexpansion"
  }

  $OldVars = @{}
  foreach ($Var in (Get-ChildItem env:*).GetEnumerator())
  {
    $OldVars.Add($Var.Key, $Var.Value)
  }

  & $Block

  Remove-Item env:*
  foreach ($Var in $OldVars.GetEnumerator())
  {
    New-Item -Path "env:\$($Var.Key)" -Value $Var.Value -ErrorAction Ignore | Out-Null
  }
  
  if ($ToBatch)
  {
    Write-Output "endlocal"
  }
}

function Invoke-VsDevShell($Arch)
{
  if ($ToBatch)
  {
    Write-Output "call `"$VSInstallRoot\Common7\Tools\VsDevCmd.bat`" -no_logo -host_arch=$($HostArch.VSName) -arch=$($Arch.VSName)"
  }
  else
  {
    # This dll path is valid for VS2019 and VS2022, but it was under a vsdevcmd subfolder in VS2017 
    Import-Module "$VSInstallRoot\Common7\Tools\Microsoft.VisualStudio.DevShell.dll"
    Enter-VsDevShell -VsInstallPath $VSInstallRoot -SkipAutomaticLocation -DevCmdArguments "-no_logo -host_arch=$($HostArch.VSName) -arch=$($Arch.VSName)"
  }
}

function TryAdd-KeyValue([hashtable]$Hashtable, [string]$Key, [string]$Value)
{
  if (-not $Hashtable.Contains($Key))
  {
    $Hashtable.Add($Key, $Value)
  }
}

function Append-FlagsDefine([hashtable]$Defines, [string]$Name, [string]$Value)
{
  if ($Defines.Contains($Name))
  {
    $Defines[$name] += " $Value" 
  }
  else
  {
    $Defines.Add($Name, $Value)
  }
}

function Build-CMakeProject
{
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [string] $Src,
    [string] $Bin,
    [string] $InstallTo = "",
    [hashtable] $Arch,
    [string] $Generator = "Ninja",
    [string] $CacheScript = "",
    [string[]] $UseMSVCCompilers = @(), # C,CXX
    [string[]] $UseBuiltCompilers = @(), # ASM,C,CXX,Swift
    [string] $SwiftSDK = "",
    [hashtable] $Defines = @{},
    [string[]] $BuildTargets = @()
  )

  if ($ToBatch)
  {
    Write-Output ""
    Write-Output "echo Building '$Src' to '$Bin' for arch '$($Arch.ShortName)'..."
  }
  else
  {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Building '$Src' to '$Bin' for arch '$($Arch.ShortName)'..."
  }
  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()

  # Add additional defines (unless already present)
  $Defines = $Defines.Clone()
  TryAdd-KeyValue $Defines CMAKE_BUILD_TYPE $BuildType
  TryAdd-KeyValue $Defines CMAKE_MT "mt"

  $CFlags = "/GS- /Gw /Gy /Oi /Oy /Zi /Zc:inline"
  $CXXFlags = "/GS- /Gw /Gy /Oi /Oy /Zi /Zc:inline /Zc:__cplusplus"
  if ($UseMSVCCompilers.Contains("C"))
  {
    TryAdd-KeyValue $Defines CMAKE_C_COMPILER cl
    Append-FlagsDefine $Defines CMAKE_C_FLAGS $CFlags
  }
  if ($UseMSVCCompilers.Contains("CXX"))
  {
    TryAdd-KeyValue $Defines CMAKE_CXX_COMPILER cl
    Append-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXFlags
  }
  if ($UseBuiltCompilers.Contains("ASM")) {
    TryAdd-KeyValue $Defines CMAKE_ASM_COMPILER "$BinaryCache\1\bin\clang-cl.exe".Replace("\", "/")
    Append-FlagsDefine $Defines CMAKE_ASM_FLAGS "--target=$($Arch.LLVMTarget)"
    TryAdd-KeyValue $Defines CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDLL "/MD"
  }
  if ($UseBuiltCompilers.Contains("C")) {
    TryAdd-KeyValue $Defines CMAKE_C_COMPILER "$BinaryCache\1\bin\clang-cl.exe".Replace("\", "/")
    TryAdd-KeyValue $Defines CMAKE_C_COMPILER_TARGET $Arch.LLVMTarget
    Append-FlagsDefine $Defines CMAKE_C_FLAGS $CFlags
  }
  if ($UseBuiltCompilers.Contains("CXX")) {
    TryAdd-KeyValue $Defines CMAKE_CXX_COMPILER "$BinaryCache\1\bin\clang-cl.exe".Replace("\", "/")
    TryAdd-KeyValue $Defines CMAKE_CXX_COMPILER_TARGET $Arch.LLVMTarget
    Append-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXFlags
  }
  if ($UseBuiltCompilers.Contains("Swift")) {
    TryAdd-KeyValue $Defines CMAKE_Swift_COMPILER "$BinaryCache\1\bin\swiftc.exe".Replace("\", "/")
    TryAdd-KeyValue $Defines CMAKE_Swift_COMPILER_TARGET $Arch.LLVMTarget

    $RuntimeBuildDir = Get-ProjectBuildDir $Arch 1
    $SwiftResourceDir = "${RuntimeBuildDir}\lib\swift"

    $SwiftArgs = [System.Collections.ArrayList]@()

    if ($SwiftSDK -ne "") {
      $SwiftArgs.Add("-sdk $SwiftSDK") | Out-Null
    } else {
      $SwiftArgs.Add("-resource-dir $SwiftResourceDir") | Out-Null
      $SwiftArgs.Add("-L $SwiftResourceDir\windows") | Out-Null
      $SwiftArgs.Add("-vfsoverlay $RuntimeBuildDir\stdlib\windows-vfs-overlay.yaml") | Out-Null
    }

    # Debug Information
    $SwiftArgs.Add("-g -debug-info-format=codeview") | Out-Null
    $SwiftArgs.Add("-Xlinker /INCREMENTAL:NO") | Out-Null
    $SwiftArgs.Add("-Xlinker /DEBUG") | Out-Null

    # Swift Requries COMDAT folding and de-duplication
    $SwiftArgs.Add("-Xlinker /OPT:REF") | Out-Null
    $SwiftArgs.Add("-Xlinker /OPT:ICF") | Out-Null

    $SwiftcFlags = $SwiftArgs.ToArray() -Join " "
    Append-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftcFlags

    # Workaround CMake 3.26+ enabling `-wmo` by default on release builds
    Append-FlagsDefine $Defines CMAKE_Swift_FLAGS_RELEASE "-O"
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
    $cmakeGenerateArgs += @("-D", "$($Define.Key)=$($Define.Value)")
  }

  Isolate-EnvVars {
    Invoke-VsDevShell $Arch

    Invoke-Program cmake.exe @cmakeGenerateArgs

    # Build all requested targets
    foreach ($Target in $BuildTargets)
    {
      if ($Target -eq "default")
      {
        Invoke-Program cmake.exe --build $Bin
      }
      else
      {
        Invoke-Program cmake.exe --build $Bin --target $Target
      }
    }

    if ("" -ne $InstallTo)
    {
      Invoke-Program cmake.exe --build $Bin --target install
    }
  }

  if (-not $ToBatch)
  {
    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Finished building '$Src' to '$Bin' for arch '$($Arch.ShortName)' in $($Stopwatch.Elapsed)"
    Write-Host ""
  }
}

function Build-WiXProject()
{
  [CmdletBinding(PositionalBinding = $false)]
  param(
    [Parameter(Position = 0, Mandatory = $true)]
    [string]$FileName,
    [Parameter(Mandatory = $true)]
    [hashtable]$Arch,
    [switch]$Bundle,
    [hashtable]$Properties = @{}
  )

  $Name = $FileName.Split('.')[0]
  $ArchName = $Arch.VSName

  $ProductVersionArg = $ProductVersion
  if (-not $Bundle)
  {
    # WiX v4 will accept a semantic version string for Bundles,
    # but Packages still require a purely numerical version number, 
    # so trim any semantic versionning suffixes
    $ProductVersionArg = [regex]::Replace($ProductVersion, "[-+].*", "")
  }

  $Properties = $Properties.Clone()
  TryAdd-KeyValue $Properties ProductArchitecture $ArchName
  TryAdd-KeyValue $Properties ProductVersion $ProductVersionArg
  TryAdd-KeyValue $Properties RunWixToolsOutOfProc true
  TryAdd-KeyValue $Properties OutputPath "$($Arch.BinaryRoot)\msi\"
  TryAdd-KeyValue $Properties IntermediateOutputPath "$($Arch.BinaryRoot)\$Name\"

  $MSBuildArgs = @("$SourceCache\swift-installer-scripts\platforms\Windows\$FileName")
  $MSBuildArgs += "-noLogo"
  $MSBuildArgs += "-restore"
  foreach ($Property in $Properties.GetEnumerator()) {
    $MSBuildArgs += "-p:$($Property.Key)=$($Property.Value)"
  }

  Invoke-Program $msbuild @MSBuildArgs
}

function Build-BuildTools($Arch)
{
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

function Build-Compilers($Arch, [switch]$Test = $false)
{
  Isolate-EnvVars {
    if ($Test)
    {
      $LibdispatchBinDir = "$BinaryCache\1\tools\swift\libdispatch-windows-$($Arch.LLVMName)-prefix\bin"
      $env:Path = "$LibdispatchBinDir;$BinaryCache\1\bin;$env:Path;$env:ProgramFiles\Git\usr\bin"
      $Targets = @("check-swift")
      $TestingDefines = @{
        SWIFT_BUILD_DYNAMIC_SDK_OVERLAY = "YES";
        SWIFT_BUILD_DYNAMIC_STDLIB = "YES";
        SWIFT_BUILD_REMOTE_MIRROR = "YES";
        SWIFT_NATIVE_SWIFT_TOOLS_PATH = "";
      }
    }
    else
    {
      $Targets = @("distribution", "install-distribution")
      $TestingDefines = @{
        SWIFT_BUILD_DYNAMIC_SDK_OVERLAY = "NO";
        SWIFT_BUILD_DYNAMIC_STDLIB = "NO";
        SWIFT_BUILD_REMOTE_MIRROR = "NO";
        SWIFT_NATIVE_SWIFT_TOOLS_PATH = "$BinaryCache\0\bin";
      }
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
        CMAKE_INSTALL_PREFIX = "$($Arch.ToolchainInstallRoot)\usr";
        LLDB_PYTHON_EXT_SUFFIX = ".pyd";
        LLDB_TABLEGEN = "$BinaryCache\0\bin\lldb-tblgen.exe";
        LLVM_CONFIG_PATH = "$BinaryCache\0\bin\llvm-config.exe";
        LLVM_ENABLE_PDB = "YES";
        LLVM_EXTERNAL_CMARK_SOURCE_DIR = "$SourceCache\cmark";
        LLVM_EXTERNAL_SWIFT_SOURCE_DIR = "$SourceCache\swift";
        LLVM_NATIVE_TOOL_DIR = "$BinaryCache\0\bin";
        LLVM_TABLEGEN = "$BinaryCache\0\bin\llvm-tblgen.exe";
        LLVM_USE_HOST_TOOLS = "NO";
        SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING = "YES";
        SWIFT_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
        SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE = "$SourceCache\swift-syntax";
        SWIFT_PATH_TO_STRING_PROCESSING_SOURCE = "$SourceCache\swift-experimental-string-processing";
      })
  }
}

function Build-LLVM($Arch)
{
  Build-CMakeProject `
    -Src $SourceCache\llvm-project\llvm `
    -Bin (Get-ProjectBuildDir $Arch 0) `
    -Arch $Arch `
    -Defines @{
      LLVM_HOST_TRIPLE = $Arch.LLVMTarget;
    }
}

function Build-ZLib($Arch)
{
  $ArchName = $Arch.ShortName

  Build-CMakeProject `
    -Src $SourceCache\zlib `
    -Bin "$($Arch.BinaryRoot)\zlib-1.2.11" `
    -InstallTo $LibraryRoot\zlib-1.2.11\usr `
    -Arch $Arch `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      INSTALL_BIN_DIR = "$LibraryRoot\zlib-1.2.11\usr\bin\$ArchName";
      INSTALL_LIB_DIR = "$LibraryRoot\zlib-1.2.11\usr\lib\$ArchName";
    }
}

function Build-XML2($Arch)
{
  $ArchName = $Arch.ShortName

  Build-CMakeProject `
    -Src $SourceCache\libxml2 `
    -Bin "$($Arch.BinaryRoot)\libxml2-2.9.12" `
    -InstallTo "$LibraryRoot\libxml2-2.9.12\usr" `
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

function Build-CURL($Arch)
{
  $ArchName = $Arch.ShortName

  Build-CMakeProject `
    -Src $SourceCache\curl `
    -Bin "$($Arch.BinaryRoot)\curl-7.77.0" `
    -InstallTo "$LibraryRoot\curl-7.77.0\usr" `
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
      ZLIB_ROOT = "$LibraryRoot\zlib-1.2.11\usr";
      ZLIB_LIBRARY = "$LibraryRoot\zlib-1.2.11\usr\lib\$ArchName\zlibstatic.lib";
    }
}

function Build-ICU($Arch)
{
  $ArchName = $Arch.ShortName

  if (-not $ToBatch)
  {
    if (-not(Test-Path -Path "$SourceCache\icu\icu4c\CMakeLists.txt"))
    {
      Copy-Item $SourceCache\swift-installer-scripts\shared\ICU\CMakeLists.txt $SourceCache\icu\icu4c\
      Copy-Item $SourceCache\swift-installer-scripts\shared\ICU\icupkg.inc.cmake $SourceCache\icu\icu4c\
    }
  }

  if ($Arch -eq $ArchARM64)
  {
    # Use previously built x64 tools
    $BuildToolsDefines = @{
      BUILD_TOOLS = "NO";
      ICU_TOOLS_DIR = "$($ArchX64.BinaryRoot)\icu-69.1"
    }
  }
  else
  {
    $BuildToolsDefines = @{BUILD_TOOLS = "YES"}
  }

  Build-CMakeProject `
    -Src $SourceCache\icu\icu4c `
    -Bin "$($Arch.BinaryRoot)\icu-69.1" `
    -InstallTo "$LibraryRoot\icu-69.1\usr" `
    -Arch $Arch `
    -BuildTargets default `
    -Defines ($BuildToolsDefines + @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_INSTALL_BINDIR = "bin/$ArchName";
      CMAKE_INSTALL_LIBDIR = "lib/$ArchName";
    })
}

function Build-Runtime($Arch)
{
  $LLVMBuildDir = Get-ProjectBuildDir $Arch 0

    Build-CMakeProject `
      -Src $SourceCache\swift `
    -Bin (Get-ProjectBuildDir $Arch 1) `
    -InstallTo "$($Arch.SDKInstallRoot)\usr" `
      -Arch $Arch `
      -CacheScript $SourceCache\swift\cmake\caches\Runtime-Windows-$($Arch.LLVMName).cmake `
      -UseBuiltCompilers C,CXX `
      -BuildTargets default `
      -Defines @{
        CMAKE_Swift_COMPILER_TARGET = $Arch.LLVMTarget;
        LLVM_DIR = "$LLVMBuildDir\lib\cmake\llvm";
        SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION = "YES";
        SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING = "YES";
        SWIFT_NATIVE_SWIFT_TOOLS_PATH = "$BinaryCache\1\bin";
        SWIFT_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
        SWIFT_PATH_TO_STRING_PROCESSING_SOURCE = "$SourceCache\swift-experimental-string-processing";
        SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE = "$SourceCache\swift-syntax";
      }

  Invoke-Program $python -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'DEFAULT_USE_RUNTIME': 'MD' } }), encoding='utf-8'))" `
    -OutFile "$($Arch.SDKInstallRoot)\SDKSettings.plist"
}

function Build-Dispatch($Arch, [switch]$Test = $false)
{
  $Targets = if ($Test) { @("default", "ExperimentalTest") } else { @("default", "install") }

  Build-CMakeProject `
    -Src $SourceCache\swift-corelibs-libdispatch `
    -Bin (Get-ProjectBuildDir $Arch 2) `
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

function Build-Foundation($Arch, [switch]$Test = $false)
{
  $DispatchBinDir = Get-ProjectBuildDir $Arch 2
  $FoundationBinDir = Get-ProjectBuildDir $Arch 3
  $ShortArch = $Arch.ShortName

  Isolate-EnvVars {
    if ($Test)
    {
      $RuntimeBinDir = Get-ProjectBuildDir $Arch 1
      $XCTestBinDir = Get-ProjectBuildDir $Arch 4
      $TestingDefines = @{
        ENABLE_TESTING = "YES";
        XCTest_DIR = "$XCTestBinDir\cmake\modules";
      }
      $Targets = @("default", "test")
      $env:Path = "$XCTestBinDir;$FoundationBinDir\bin;$DispatchBinDir;$RuntimeBinDir\bin;$env:Path"
    }
    else
    {
      $TestingDefines = @{ ENABLE_TESTING = "NO" }
      $Targets = @("default", "install")
    }

    $env:CTEST_OUTPUT_ON_FAILURE = 1
    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-foundation `
      -Bin $FoundationBinDir `
      -Arch $Arch `
      -UseBuiltCompilers ASM,C,Swift `
      -BuildTargets $Targets `
      -Defines (@{
        CMAKE_INSTALL_PREFIX = "$($Arch.SDKInstallRoot)\usr";
        CMAKE_SYSTEM_NAME = "Windows";
        CMAKE_SYSTEM_PROCESSOR = $Arch.CMakeName;
        CURL_DIR = "$LibraryRoot\curl-7.77.0\usr\lib\$ShortArch\cmake\CURL";
        ICU_DATA_LIBRARY_RELEASE = "$LibraryRoot\icu-69.1\usr\lib\$ShortArch\sicudt69.lib";
        ICU_I18N_LIBRARY_RELEASE = "$LibraryRoot\icu-69.1\usr\lib\$ShortArch\sicuin69.lib";
        ICU_ROOT = "$LibraryRoot\icu-69.1\usr";
        ICU_UC_LIBRARY_RELEASE = "$LibraryRoot\icu-69.1\usr\lib\$ShortArch\sicuuc69.lib";
        LIBXML2_LIBRARY = "$LibraryRoot\libxml2-2.9.12\usr\lib\$ShortArch\libxml2s.lib";
        LIBXML2_INCLUDE_DIR = "$LibraryRoot\libxml2-2.9.12\usr\include\libxml2";
        LIBXML2_DEFINITIONS = "/DLIBXML_STATIC";
        ZLIB_LIBRARY = "$LibraryRoot\zlib-1.2.11\usr\lib\$ShortArch\zlibstatic.lib";
        ZLIB_INCLUDE_DIR = "$LibraryRoot\zlib-1.2.11\usr\include";
        dispatch_DIR = "$DispatchBinDir\cmake\modules";
      } + $TestingDefines)
  }
}

function Build-XCTest($Arch, [switch]$Test = $false)
{
  $LLVMBinDir = Get-ProjectBuildDir $Arch 0
  $DispatchBinDir = Get-ProjectBuildDir $Arch 2
  $FoundationBinDir = Get-ProjectBuildDir $Arch 3
  $XCTestBinDir = Get-ProjectBuildDir $Arch 4

  Isolate-EnvVars {
    if ($Test)
    {
      $RuntimeBinDir = Get-ProjectBuildDir $Arch 1
      $TestingDefines = @{
        ENABLE_TESTING = "YES";
        LLVM_DIR = "$LLVMBinDir/lib/cmake/llvm";
        XCTEST_PATH_TO_LIBDISPATCH_BUILD = $DispatchBinDir;
        XCTEST_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
        XCTEST_PATH_TO_FOUNDATION_BUILD = $FoundationBinDir;
      }
      $Targets = @("default", "check-xctest")
      $env:Path = "$XCTestBinDir;$FoundationBinDir\bin;$DispatchBinDir;$RuntimeBinDir\bin;$env:Path;$env:ProgramFiles\Git\usr\bin"
    }
    else
    {
      $TestingDefines = @{ ENABLE_TESTING = "NO" }
      $Targets = @("default", "install")
    }

    Build-CMakeProject `
      -Src $SourceCache\swift-corelibs-xctest `
      -Bin $XCTestBinDir `
      -Arch $Arch `
      -UseBuiltCompilers Swift `
      -BuildTargets $Targets `
      -Defines (@{
        CMAKE_INSTALL_PREFIX = "$($Arch.XCTestInstallRoot)\usr";
        CMAKE_SYSTEM_NAME = "Windows";
        CMAKE_SYSTEM_PROCESSOR = $Arch.CMakeName;
        dispatch_DIR = "$DispatchBinDir\cmake\modules";
        Foundation_DIR = "$FoundationBinDir\cmake\modules";
      } + $TestingDefines)
    
    Invoke-Program $python -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'XCTEST_VERSION': 'development' } }), encoding='utf-8'))" `
      -OutFile "$($Arch.PlatformInstallRoot)\Info.plist"
  }
}

function Copy-File($Src, $Dst)
{
  # Create the directory tree first so Copy-Item succeeds
  # If $Dst is the target directory, make sure it ends with "\"
  $DstDir = [IO.Path]::GetDirectoryName($Dst)
  New-Item -ItemType Directory -ErrorAction Ignore $DstDir | Out-Null
  Copy-Item -Force $Src $Dst
}

function Copy-Directory($Src, $Dst)
{
  New-Item -ItemType Directory -ErrorAction Ignore $Dst | Out-Null
  Copy-Item -Force -Recurse $Src $Dst
}

function Install-Redist($Arch)
{
  if ($ToBatch) { return }

  if ($Arch -eq $HostArch)
  {
    $ProgramFilesName = "Program Files"
  }
  elseif ($Arch -eq $ArchX86)
  {
    $ProgramFilesName = "Program Files (x86)"
  }
  elseif (($HostArch -eq $ArchArm64) -and ($Arch -eq $ArchX64))
  {
    # x64 programs actually install under "Program Files" on arm64,
    # but this would conflict with the native installation.
    $ProgramFilesName = "Program Files (Amd64)"
  }
  else
  {
    # arm64 cannot be installed on x64
    return
  }

  $RedistInstallRoot = "S:\$ProgramFilesName\swift\runtime-development"

  Remove-Item -Force -Recurse $RedistInstallRoot -ErrorAction Ignore
  Copy-Directory "$($Arch.SDKInstallRoot)\usr\bin" "$RedistInstallRoot\usr"
}

# Copies files installed by CMake from the arch-specific platform root,
# where they follow the layout expected by the installer,
# to the final platform root, following the installer layout.
function Install-Platform($Arch)
{
  if ($ToBatch) { return }

  New-Item -ItemType Directory -ErrorAction Ignore $SDKInstallRoot\usr | Out-Null

  # Copy SDK header files
  Copy-Directory "$($Arch.SDKInstallRoot)\usr\include\swift\SwiftRemoteMirror" $SDKInstallRoot\usr\include\swift
  Copy-Directory "$($Arch.SDKInstallRoot)\usr\lib\swift\shims" $SDKInstallRoot\usr\lib\swift
  foreach ($Module in ("Block", "dispatch", "os"))
  {
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
    if (".swiftmodule", ".swiftdoc", ".swiftinterface" -contains $_.Extension)
    {
      $DstDir = "$WindowsLibDst\$($_.BaseName).swiftmodule"
      Copy-File $_.FullName "$DstDir\$($Arch.LLVMTarget)$($_.Extension)"
    }
    else
    {
      Copy-File $_.FullName "$WindowsLibDst\$($Arch.LLVMName)\"
    }
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

function Build-SQLite($Arch)
{
  $SrcPath = "$SourceCache\sqlite-3.36.0"

  # Download the sources
  if (-not (Test-Path $SrcPath))
  {
    $ZipPath = "$env:TEMP\sqlite-amalgamation-3360000.zip"
    if (-not $ToBatch) { Remove-item $ZipPath -ErrorAction Ignore | Out-Null }
    Invoke-Program curl.exe -- -sL https://sqlite.org/2021/sqlite-amalgamation-3360000.zip -o $ZipPath

    if (-not $ToBatch) { New-Item -Type Directory -Path $SrcPath -ErrorAction Ignore | Out-Null }
    Invoke-Program "$env:ProgramFiles\Git\usr\bin\unzip.exe" -- -j -o $ZipPath -d $SrcPath
    if (-not $ToBatch) { Copy-Item $SourceCache\swift-build\cmake\SQLite\CMakeLists.txt $SrcPath\ }

    if (-not $ToBatch) { Remove-item $ZipPath | Out-Null }
  }

  Build-CMakeProject `
    -Src $SrcPath `
    -Bin "$($Arch.BinaryRoot)\sqlite-3.36.0" `
    -InstallTo $LibraryRoot\sqlite-3.36.0\usr `
    -Arch $Arch `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
    }
}

function Build-System($Arch)
{
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

function Build-ToolsSupportCore($Arch)
{
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
      SQLite3_INCLUDE_DIR = "$LibraryRoot\sqlite-3.36.0\usr\include";
      SQLite3_LIBRARY = "$LibraryRoot\sqlite-3.36.0\usr\lib\SQLite3.lib";
    }
}

function Build-LLBuild($Arch)
{
  Build-CMakeProject `
    -Src $SourceCache\llbuild `
    -Bin $BinaryCache\4 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseMSVCCompilers CXX `
    -UseBuiltCompilers Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      LLBUILD_SUPPORT_BINDINGS = "Swift";
      SQLite3_INCLUDE_DIR = "$LibraryRoot\sqlite-3.36.0\usr\include";
      SQLite3_LIBRARY = "$LibraryRoot\sqlite-3.36.0\usr\lib\SQLite3.lib";
    }
}

function Build-Yams($Arch)
{
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

function Build-ArgumentParser($Arch)
{
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

function Build-Driver($Arch)
{
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
      SQLite3_INCLUDE_DIR = "$LibraryRoot\sqlite-3.36.0\usr\include";
      SQLite3_LIBRARY = "$LibraryRoot\sqlite-3.36.0\usr\lib\SQLite3.lib";
    }
}

function Build-Crypto($Arch)
{
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

function Build-Collections($Arch)
{
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

function Build-ASN1($Arch)
{
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

function Build-Certificates($Arch)
{
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

function Build-PackageManager($Arch)
{
  Build-CMakeProject `
    -Src $SourceCache\swift-package-manager `
    -Bin $BinaryCache\12 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseBuiltCompilers C,Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_Swift_FLAGS = "-DCRYPTO_v2";
      SwiftSystem_DIR = "$BinaryCache\2\cmake\modules";
      TSC_DIR = "$BinaryCache\3\cmake\modules";
      LLBuild_DIR = "$BinaryCache\4\cmake\modules";
      ArgumentParser_DIR = "$BinaryCache\6\cmake\modules";
      SwiftDriver_DIR = "$BinaryCache\7\cmake\modules";
      SwiftCrypto_DIR = "$BinaryCache\8\cmake\modules";
      SwiftCollections_DIR = "$BinaryCache\9\cmake\modules";
      SwiftASN1_DIR = "$BinaryCache\10\cmake\modules";
      SwiftCertificates_DIR = "$BinaryCache\11\cmake\modules";
      SQLite3_INCLUDE_DIR = "$LibraryRoot\sqlite-3.36.0\usr\include";
      SQLite3_LIBRARY = "$LibraryRoot\sqlite-3.36.0\usr\lib\SQLite3.lib";
    }
}

function Build-IndexStoreDB($Arch)
{
  Build-CMakeProject `
    -Src $SourceCache\indexstore-db `
    -Bin $BinaryCache\13 `
    -Arch $Arch `
    -UseBuiltCompilers C,CXX,Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_C_FLAGS = "-Xclang -fno-split-cold-code -I$SDKInstallRoot\usr\include -I$SDKInstallRoot\usr\include\Block";
      CMAKE_CXX_FLAGS = "-Xclang -fno-split-cold-code -I$SDKInstallRoot\usr\include -I$SDKInstallRoot\usr\include\Block";
    }
}

function Build-Syntax($Arch)
{
  Build-CMakeProject `
    -Src $SourceCache\swift-syntax `
    -Bin $BinaryCache\14 `
    -InstallTo "$($Arch.ToolchainInstallRoot)\usr" `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -SwiftSDK $SDKInstallRoot `
    -BuildTargets default `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
    }
}

function Build-SourceKitLSP($Arch)
{
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
      SwiftCollections_DIR = "$BinaryCache\9\cmake\modules";
      SwiftPM_DIR = "$BinaryCache\12\cmake\modules";
      IndexStoreDB_DIR = "$BinaryCache\13\cmake\modules";
      SwiftSyntax_DIR = "$BinaryCache\14\cmake\modules";
    }
}

function Install-HostToolchain()
{
  if ($ToBatch) { return }

  Remove-Item -Force -Recurse $ToolchainInstallRoot -ErrorAction Ignore
  Copy-Directory "$($HostArch.ToolchainInstallRoot)\usr" $ToolchainInstallRoot\

  # Restructure _InternalSwiftScan
  Move-Item -Force `
    $ToolchainInstallRoot\usr\lib\swift\_InternalSwiftScan `
    $ToolchainInstallRoot\usr\include
  Move-Item -Force `
    $ToolchainInstallRoot\usr\lib\swift\windows\_InternalSwiftScan.lib `
    $ToolchainInstallRoot\usr\lib

  # Switch to swift-driver
  Copy-Item -Force $BinaryCache\7\bin\swift-driver.exe $ToolchainInstallRoot\usr\bin\swift.exe
  Copy-Item -Force $BinaryCache\7\bin\swift-driver.exe $ToolchainInstallRoot\usr\bin\swiftc.exe
}

function Build-Installer()
{
  Build-WiXProject toolchain.wixproj -Arch $HostArch -Properties @{
    DEVTOOLS_ROOT = "$($HostArch.ToolchainInstallRoot)\";
    TOOLCHAIN_ROOT = "$($HostArch.ToolchainInstallRoot)\";
  }

  foreach ($Arch in $SDKArchs)
  {
    Build-WiXProject runtime.wixproj -Arch $Arch -Properties @{
      SDK_ROOT = "$($Arch.SDKInstallRoot)\";
    }
    
    Build-WiXProject sdk.wixproj -Arch $Arch -Properties @{
      PLATFORM_ROOT = "$($Arch.PlatformInstallRoot)\";
      SDK_ROOT = "$($Arch.SDKInstallRoot)\";
      SWIFT_SOURCE_DIR = "$SourceCache\swift\";
    }
  }

  Build-WiXProject devtools.wixproj -Arch $HostArch -Properties @{
    DEVTOOLS_ROOT = "$($HostArch.ToolchainInstallRoot)\";
  }

  Build-WiXProject installer.wixproj -Arch $HostArch -Bundle -Properties @{
    OutputPath = "$($HostArch.BinaryRoot)\";
    MSI_LOCATION = "$($HostArch.BinaryRoot)\msi\";
  }
}

#-------------------------------------------------------------------

Build-BuildTools $HostArch
Build-Compilers $HostArch

foreach ($Arch in $SDKArchs)
{
  Build-ZLib $Arch
  Build-XML2 $Arch
  Build-CURL $Arch
  Build-ICU $Arch
  Build-LLVM $Arch

  # Build platform: SDK, Redist and XCTest
  Build-Runtime $Arch
  Build-Dispatch $Arch
  Build-Foundation $Arch
  Build-XCTest $Arch

  if (-not $SkipRedistInstall)
  {
    Install-Redist $Arch
  }
}

if (-not $ToBatch)
{
  Remove-Item -Force -Recurse $PlatformInstallRoot -ErrorAction Ignore
  foreach ($Arch in $SDKArchs)
  {
    Install-Platform $Arch
  }
}

Build-SQLite $HostArch
Build-System $HostArch
Build-ToolsSupportCore $HostArch
Build-LLBuild $HostArch
Build-Yams $HostArch
Build-ArgumentParser $HostArch
Build-Driver $HostArch
Build-Crypto $HostArch
Build-Collections $HostArch
Build-ASN1 $HostArch
Build-Certificates $HostArch
Build-PackageManager $HostArch
Build-IndexStoreDB $HostArch
Build-Syntax $HostArch
Build-SourceKitLSP $HostArch

Install-HostToolchain

if (-not $SkipPackaging)
{
  Build-Installer
}

if ($Test -contains "swift") { Build-Compilers $HostArch -Test }
if ($Test -contains "dispatch") { Build-Dispatch $HostArch -Test }
if ($Test -contains "foundation") { Build-Foundation $HostArch -Test }
if ($Test -contains "xctest") { Build-XCTest $HostArch -Test }

if (-not $SkipPackaging -and $Stage -ne "")
{
  $Stage += "\" # Interpret as target directory

  Copy-File "$($HostArch.BinaryRoot)\msi\*.msi" $Stage
  Copy-File "$($HostArch.BinaryRoot)\installer.exe" $Stage
}
