# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

param(
  [string[]] $SDKs = @("X64","X86","Arm64"),
  [string] $SourceCache = "S:\SourceCache"
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version 3.0

$BinaryCache = "S:\b"
$InstallRoot = "S:\Library"
$ToolchainInstallRoot = "$InstallRoot\Developer\Toolchains\unknown-Asserts-development.xctoolchain"
$PlatformInstallRoot = "$InstallRoot\Developer\Platforms\Windows.platform"
$SDKInstallRoot = "$PlatformInstallRoot\Developer\SDKs\Windows.sdk"

$vswhere = "${Env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$VSInstallRoot = & $vswhere -nologo -latest -products "*" -all -prerelease -property installationPath
$msbuild = "$VSInstallRoot\MSBuild\Current\Bin\$env:PROCESSOR_ARCHITECTURE\MSBuild.exe"

# Architecture definitions
$ArchX64 = @{
  VSName = "amd64";
  ShortName = "x64";
  LLVMName = "x86_64";
  LLVMTarget = "x86_64-unknown-windows-msvc";
  CMakeName = "AMD64";
  BinaryDir = "bin64";
  BuildID = 100
}

$ArchX86 = @{
  VSName = "x86";
  ShortName = "x86";
  LLVMName = "i686";
  LLVMTarget = "i686-unknown-windows-msvc";
  CMakeName = "i686";
  BinaryDir = "bin32";
  BuildID = 200
}

$ArchARM64 = @{
  VSName = "arm64";
  ShortName = "arm64";
  LLVMName = "aarch64";
  LLVMTarget = "aarch64-unknown-windows-msvc";
  CMakeName = "aarch64";
  BinaryDir = "bin64a";
  BuildID = 300
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

$CurrentVSDevShellTargetArch = $null

$InitialEnvPaths = @{
  EXTERNAL_INCLUDE = $env:EXTERNAL_INCLUDE;
  INCLUDE = $env:INCLUDE;
  LIB = $env:LIB;
  LIBPATH = $env:LIBPATH;
  Path = $env:Path;
  __VSCMD_PREINIT_PATH = $env:__VSCMD_PREINIT_PATH
}

# Build functions
function Get-ProjectBuildDir($Arch, $ID)
{
  return "$BinaryCache\" + ($Arch.BuildID + $ID)
}

function Get-RuntimeInstallDir($Arch, $SubDir = "")
{
  $Path = "$InstallRoot\swift-development\$($Arch.ShortName)"
  if ("" -ne $SubDir) {
    $Path += "\$SubDir"
  }
  return $Path
}

function Check-LastExitCode
{
  if ($LastExitCode -ne 0)
  {
    $callstack = @(Get-PSCallStack) -Join "`n"
    throw "Command execution returned $LastExitCode. Call stack:`n$callstack"
  }
}

function Invoke-VsDevShell($Arch)
{
  # Restore path-style environment variables to avoid appending ever more entries
  foreach ($entry in $InitialEnvPaths.GetEnumerator())
  {
    [Environment]::SetEnvironmentVariable($entry.Name, $entry.Value, "Process")
  }

  & "$VSInstallRoot\Common7\Tools\Launch-VsDevShell.ps1" -VsInstallationPath $VSInstallRoot -HostArch amd64 -Arch $Arch.VSName | Out-Null
  Check-LastExitCode
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
    [string] $BuildType = "Release", 
    [string] $Generator = "Ninja",
    [string] $CacheScript = "",
    [string[]] $UseMSVCCompilers = @(), # C,CXX
    [string[]] $UseBuiltCompilers = @(), # ASM,C,CXX,Swift
    [hashtable] $Defines = @{},
    [switch] $BuildDefaultTarget = $false,
    [string[]] $BuildTargets = @()
  )

  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Building '$Src' to '$Bin' for arch '$($Arch.ShortName)'..."
  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()

  # Make sure we have the right VSDevShell target architecture for building
  if ($Arch -ne $CurrentVSDevShellTargetArch) {
    Invoke-VsDevShell $Arch
    $CurrentVSDevShellTargetArch = $Arch
  }

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
    TryAdd-KeyValue $Defines CMAKE_ASM_COMPILER S:/b/1/bin/clang-cl.exe
    Append-FlagsDefine $Defines CMAKE_ASM_FLAGS "--target=$($Arch.LLVMTarget)"
    TryAdd-KeyValue $Defines CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDLL "/MD"
  }
  if ($UseBuiltCompilers.Contains("C")) {
    TryAdd-KeyValue $Defines CMAKE_C_COMPILER S:/b/1/bin/clang-cl.exe
    TryAdd-KeyValue $Defines CMAKE_C_COMPILER_TARGET $Arch.LLVMTarget
    Append-FlagsDefine $Defines CMAKE_C_FLAGS $CFlags
  }
  if ($UseBuiltCompilers.Contains("CXX")) {
    TryAdd-KeyValue $Defines CMAKE_CXX_COMPILER S:/b/1/bin/clang-cl.exe
    TryAdd-KeyValue $Defines CMAKE_CXX_COMPILER_TARGET $Arch.LLVMTarget
    Append-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXFlags
  }
  if ($UseBuiltCompilers.Contains("Swift")) {
    TryAdd-KeyValue $Defines CMAKE_Swift_COMPILER S:/b/1/bin/swiftc.exe
    TryAdd-KeyValue $Defines CMAKE_Swift_COMPILER_TARGET $Arch.LLVMTarget

    $RuntimeBuildDir = Get-ProjectBuildDir $Arch 1
    $SwiftResourceDir = "${RuntimeBuildDir}\lib\swift"
    $SwiftcFlags = @(
      "-resource-dir $SwiftResourceDir",
      "-L $SwiftResourceDir\windows",
      "-vfsoverlay $RuntimeBuildDir\stdlib\windows-vfs-overlay.yaml",
      "-g -debug-info-format=codeview",
      "-Xlinker /INCREMENTAL:NO",
      "-Xlinker /DEBUG",
      "-Xlinker /OPT:REF",
      "-Xlinker /OPT:ICF"
    ) -Join " "

    Append-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftcFlags
  }
  if ("" -ne $InstallTo) {
    TryAdd-KeyValue $Defines CMAKE_INSTALL_PREFIX $InstallTo
  }

  # Generate the project
  $cmakeGenerateArgs = @("-B", $Bin, "-S", $Src, "-G", $Generator)
  if ("" -ne $CacheScript) {
    $cmakeGenerateArgs += @("-C", $CacheScript)
  }
  foreach ($Define in $Defines.GetEnumerator()) {
    $cmakeGenerateArgs += @("-D", "$($Define.Key)=$($Define.Value)")
  }

  cmake @cmakeGenerateArgs
  Check-LastExitCode

  # Build all requested targets
  if ($BuildDefaultTarget)
  {
    cmake --build $Bin
    Check-LastExitCode
  }

  foreach ($Target in $BuildTargets)
  {
    cmake --build $Bin --target $Target
    Check-LastExitCode
  }

  if ("" -ne $InstallTo)
  {
    cmake --build $Bin --target install
    Check-LastExitCode
  }

  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Finished building '$Src' to '$Bin' for arch '$($Arch.ShortName)' in $($Stopwatch.Elapsed)"
  Write-Host ""
}

function Build-WiXProject()
{
  [CmdletBinding(PositionalBinding = $false)]
  param(
    [Parameter(Position = 0)]
    [string]$FileName,
    [hashtable]$Properties = @{}
  )

  $Name = $FileName.Split('.')[0]

  $Properties = $Properties.Clone()
  TryAdd-KeyValue $Properties RunWixToolsOutOfProc true
  TryAdd-KeyValue $Properties OutputPath $BinaryCache\msi\
  TryAdd-KeyValue $Properties IntermediateOutputPath BinaryCache\$Name\

  $MSBuildArgs = @("$SourceCache\swift-installer-scripts\platforms\Windows\$FileName")
  $MSBuildArgs += "-noLogo"
  foreach ($Property in $Properties.GetEnumerator()) {
    $MSBuildArgs += "-p:$($Property.Key)=$($Property.Value)"
  }

  & $msbuild @MSBuildArgs
  Check-LastExitCode
}

function Build-Compilers($Arch)
{
  Build-CMakeProject `
    -Src $SourceCache\llvm-project\llvm `
    -Bin $BinaryCache\1 `
    -Arch $Arch `
    -BuildTargets distribution,install-distribution `
    -CacheScript $SourceCache\swift\cmake\caches\Windows-$($Arch.LLVMName).cmake `
    -Defines @{
      CMAKE_INSTALL_PREFIX = "$ToolchainInstallRoot\usr";
      LLVM_ENABLE_PDB = "YES";
      LLVM_EXTERNAL_CMARK_SOURCE_DIR = "$SourceCache\cmark";
      LLVM_EXTERNAL_SWIFT_SOURCE_DIR = "$SourceCache\swift";
      SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_REFLECTION = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING = "YES";
      SWIFT_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
      SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE = "$SourceCache\swift-syntax";
      SWIFT_PATH_TO_STRING_PROCESSING_SOURCE = "$SourceCache\swift-experimental-string-processing";
    }

  # Restructure Internal Modules
  Remove-Item -Recurse -Force  -ErrorAction Ignore `
    $ToolchainInstallRoot\usr\include\_InternalSwiftScan
  Move-Item -Force `
    $ToolchainInstallRoot\usr\lib\swift\_InternalSwiftScan `
    $ToolchainInstallRoot\usr\include
  Move-Item -Force `
    $ToolchainInstallRoot\usr\lib\swift\windows\_InternalSwiftScan.lib `
    $ToolchainInstallRoot\usr\lib
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
    -Bin $BinaryCache\zlib-1.2.11.$ArchName `
    -InstallTo $InstallRoot\zlib-1.2.11\usr `
    -Arch $Arch `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      INSTALL_BIN_DIR = "$InstallRoot\zlib-1.2.11\usr\bin\$ArchName";
      INSTALL_LIB_DIR = "$InstallRoot\zlib-1.2.11\usr\lib\$ArchName";
    }
}

function Build-XML2($Arch)
{
  $ArchName = $Arch.ShortName

  Build-CMakeProject `
    -Src $SourceCache\libxml2 `
    -Bin $BinaryCache\libxml2-2.9.12.$ArchName `
    -InstallTo "$InstallRoot\libxml2-2.9.12\usr" `
    -Arch $Arch `
    -BuildDefaultTarget `
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
    -Bin $BinaryCache\curl-7.77.0.$ArchName `
    -InstallTo "$InstallRoot\curl-7.77.0\usr" `
    -Arch $Arch `
    -BuildDefaultTarget `
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
      ZLIB_ROOT = "$InstallRoot\zlib-1.2.11\usr";
      ZLIB_LIBRARY = "$InstallRoot\zlib-1.2.11\usr\lib\$ArchName\zlibstatic.lib";
    }
}

function Build-ICU($Arch)
{
  $ArchName = $Arch.ShortName

  if (-not(Test-Path -Path "$SourceCache\icu\icu4c\CMakeLists.txt"))
  {
    Copy-Item $SourceCache\swift-installer-scripts\shared\ICU\CMakeLists.txt $SourceCache\icu\icu4c\
    Copy-Item $SourceCache\swift-installer-scripts\shared\ICU\icupkg.inc.cmake $SourceCache\icu\icu4c\
  }

  if ($Arch -eq $ArchARM64)
  {
    # Use previously built x64 tools
    $BuildToolsDefines = @{
      BUILD_TOOLS = "NO";
      ICU_TOOLS_DIR = "S:\b\icu-69.1.x64"
    }
  }
  else
  {
    $BuildToolsDefines = @{BUILD_TOOLS = "YES"}
  }

  Build-CMakeProject `
    -Src $SourceCache\icu\icu4c `
    -Bin $BinaryCache\icu-69.1.$ArchName `
    -InstallTo "$InstallRoot\icu-69.1\usr" `
    -Arch $Arch `
    -BuildDefaultTarget `
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
    -InstallTo $SDKInstallRoot\usr `
    -Arch $Arch `
    -CacheScript $SourceCache\swift\cmake\caches\Runtime-Windows-$($Arch.LLVMName).cmake `
    -UseBuiltCompilers C,CXX `
    -BuildDefaultTarget `
    -Defines @{
      CMAKE_INSTALL_BINDIR = Get-RuntimeInstallDir $Arch "usr\bin";
      LLVM_DIR = "$LLVMBuildDir\lib\cmake\llvm";
      SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_REFLECTION = "YES";
      SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING = "YES";
      SWIFT_NATIVE_SWIFT_TOOLS_PATH = "$BinaryCache\1\bin";
      SWIFT_PATH_TO_LIBDISPATCH_SOURCE = "$SourceCache\swift-corelibs-libdispatch";
      SWIFT_PATH_TO_STRING_PROCESSING_SOURCE = "$SourceCache\swift-experimental-string-processing";
      SWIFT_PATH_TO_SWIFT_SYNTAX_SOURCE = "$SourceCache\swift-syntax";
    }
}

function Build-Dispatch($Arch)
{
  Build-CMakeProject `
    -Src $SourceCache\swift-corelibs-libdispatch `
    -Bin (Get-ProjectBuildDir $Arch 2) `
    -InstallTo $SDKInstallRoot\usr `
    -Arch $Arch `
    -UseBuiltCompilers C,CXX,Swift `
    -BuildDefaultTarget `
    -Defines @{
      CMAKE_INSTALL_BINDIR = Get-RuntimeInstallDir $Arch "usr\bin";
      CMAKE_SYSTEM_NAME = "Windows";
      CMAKE_SYSTEM_PROCESSOR = $Arch.CMakeName;
      ENABLE_SWIFT = "YES";
      BUILD_TESTING = "NO";
    }

  # Restructure BlocksRuntime, dispatch headers
  foreach ($module in ("Block", "dispatch", "os"))
  {
    Remove-Item -Recurse -Force -ErrorAction Ignore `
      $SDKInstallRoot\usr\include\$module
    Move-Item -Force `
      $SDKInstallRoot\usr\lib\swift\$module `
      $SDKInstallRoot\usr\include\
  }

  # Restructure Import Libraries
  foreach ($module in ("BlocksRuntime", "dispatch", "swiftDispatch"))
  {
    Move-Item -Force `
      $SDKInstallRoot\usr\lib\swift\windows\$($module).lib `
      $SDKInstallRoot\usr\lib\swift\windows\$($Arch.LLVMName)
  }

  # Restructure Module
  New-item -ErrorAction Ignore -Type Directory `
    -Path $SDKInstallRoot\usr\lib\swift\windows\Dispatch.swiftmodule
  Move-Item -Force `
    $SDKInstallRoot\usr\lib\swift\windows\$($Arch.LLVMName)\Dispatch.swiftmodule `
    $SDKInstallRoot\usr\lib\swift\windows\Dispatch.swiftmodule\$($Arch.LLVMTarget).swiftmodule
  Move-Item -Force `
    $SDKInstallRoot\usr\lib\swift\windows\$($Arch.LLVMName)\Dispatch.swiftdoc `
    $SDKInstallRoot\usr\lib\swift\windows\Dispatch.swiftmodule\$($Arch.LLVMTarget).swiftdoc
}

function Build-Foundation($Arch)
{
  $DispatchBinDir = Get-ProjectBuildDir $Arch 2
  $ShortArch = $Arch.ShortName

  Build-CMakeProject `
    -Src $SourceCache\swift-corelibs-foundation `
    -Bin (Get-ProjectBuildDir $Arch 3) `
    -InstallTo $SDKInstallRoot\usr `
    -Arch $Arch `
    -UseBuiltCompilers ASM,C,Swift `
    -BuildDefaultTarget `
    -Defines @{
      CMAKE_INSTALL_BINDIR = Get-RuntimeInstallDir $Arch "usr\bin";
      CMAKE_SYSTEM_NAME = "Windows";
      CMAKE_SYSTEM_PROCESSOR = $Arch.CMakeName;
      CURL_DIR = "$InstallRoot\curl-7.77.0\usr\lib\$ShortArch\cmake\CURL";
      ICU_DATA_LIBRARY_RELEASE = "$InstallRoot\icu-69.1\usr\lib\$ShortArch\sicudt69.lib";
      ICU_I18N_LIBRARY_RELEASE = "$InstallRoot\icu-69.1\usr\lib\$ShortArch\sicuin69.lib";
      ICU_ROOT = "$InstallRoot\icu-69.1\usr";
      ICU_UC_LIBRARY_RELEASE = "$InstallRoot\icu-69.1\usr\lib\$ShortArch\sicuuc69.lib";
      LIBXML2_LIBRARY = "$InstallRoot\libxml2-2.9.12\usr\lib\$ShortArch\libxml2s.lib";
      LIBXML2_INCLUDE_DIR = "$InstallRoot\libxml2-2.9.12\usr\include\libxml2";
      LIBXML2_DEFINITIONS = "/DLIBXML_STATIC";
      ZLIB_LIBRARY = "$InstallRoot\zlib-1.2.11\usr\lib\$ShortArch\zlibstatic.lib";
      ZLIB_INCLUDE_DIR = "$InstallRoot\zlib-1.2.11\usr\include";
      dispatch_DIR = "$DispatchBinDir\cmake\modules";
      ENABLE_TESTING = "NO";
    }

  # Remove CoreFoundation Headers
  foreach ($module in ("CoreFoundation", "CFXMLInterface", "CFURLSessionInterface"))
  {
    Remove-Item -Recurse -Force -ErrorAction Ignore `
      $SDKInstallRoot\usr\lib\swift\$module
  }

  # Restructure Import Libraries, Modules
  foreach ($module in ("Foundation", "FoundationNetworking", "FoundationXML"))
  {
    Move-Item -Force `
      $SDKInstallRoot\usr\lib\swift\windows\$($module).lib `
      $SDKInstallRoot\usr\lib\swift\windows\$($Arch.LLVMName)

    New-Item -ErrorAction Ignore -Type Directory `
      -Path $SDKInstallRoot\usr\lib\swift\windows\$($module).swiftmodule
    Move-Item -Force `
      $SDKInstallRoot\usr\lib\swift\windows\$($Arch.LLVMName)\$($module).swiftmodule `
      $SDKInstallRoot\usr\lib\swift\windows\$($module).swiftmodule\$($Arch.LLVMTarget).swiftmodule
    Move-Item -Force `
      $SDKInstallRoot\usr\lib\swift\windows\$($Arch.LLVMName)\$($module).swiftdoc `
      $SDKInstallRoot\usr\lib\swift\windows\$($module).swiftmodule\$($Arch.LLVMTarget).swiftdoc
  }
}

function Build-XCTest($Arch)
{
  $DispatchBinDir = Get-ProjectBuildDir $Arch 2
  $FoundationBinDir = Get-ProjectBuildDir $Arch 3
  $InstallDir = "$PlatformInstallRoot\Developer\Library\XCTest-development\usr"

  Build-CMakeProject `
    -Src $SourceCache\swift-corelibs-xctest `
    -Bin (Get-ProjectBuildDir $Arch 4) `
    -InstallTo $InstallDir `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -BuildDefaultTarget `
    -Defines @{
      CMAKE_SYSTEM_NAME = "Windows";
      CMAKE_SYSTEM_PROCESSOR = $Arch.CMakeName;
      dispatch_DIR = "$DispatchBinDir\cmake\modules";
      Foundation_DIR = "$FoundationBinDir\cmake\modules";
    }

  # Restructure Runtime
  Remove-Item -Recurse -Force -ErrorAction Ignore `
    $InstallDir\$($Arch.BinaryDir)
  Move-Item -Force `
    $InstallDir\bin `
    $InstallDir\$($Arch.BinaryDir)

  # Restructure Import Libraries
  New-Item -ErrorAction Ignore -Type Directory `
    -Path $InstallDir\lib\swift\windows\$($Arch.LLVMName)
  Move-Item -Force `
    $InstallDir\lib\swift\windows\XCTest.lib `
    $InstallDir\lib\swift\windows\$($Arch.LLVMName)\XCTest.lib

  # Restructure Module
  New-Item -ErrorAction Ignore -Type Directory `
    -Path $InstallDir\lib\swift\windows\XCTest.swiftmodule
  Move-Item -Force `
    $InstallDir\lib\swift\windows\$($Arch.LLVMName)\XCTest.swiftdoc `
    $InstallDir\lib\swift\windows\XCTest.swiftmodule\$($Arch.LLVMTarget).swiftdoc
  Move-Item -Force `
    $InstallDir\lib\swift\windows\$($Arch.LLVMName)\XCTest.swiftmodule `
    $InstallDir\lib\swift\windows\XCTest.swiftmodule\$($Arch.LLVMTarget).swiftmodule
}

function Build-SQLite($Arch)
{
  $ArchName = $Arch.ShortName
  $Dest = "$SourceCache\sqlite-3.36.0"

  # Download the sources
  New-Item -ErrorAction Ignore -Type Directory `
    -Path "S:\var\cache"
  if (-not (Test-Path -Path "S:\var\cache\sqlite-amalgamation-3360000.zip"))
  {
    curl.exe -sL https://sqlite.org/2021/sqlite-amalgamation-3360000.zip -o S:\var\cache\sqlite-amalgamation-3360000.zip
  }

  if (-not (Test-Path -Path $Dest))
  {
    New-Item -ErrorAction Ignore -Type Directory -Path $Dest
    ."$env:ProgramFiles\Git\usr\bin\unzip.exe" -j -o S:\var\cache\sqlite-amalgamation-3360000.zip -d $Dest
    Copy-Item $SourceCache\swift-build\cmake\SQLite\CMakeLists.txt $Dest\
  }

  Build-CMakeProject `
    -Src $SourceCache\sqlite-3.36.0 `
    -Bin $BinaryCache\sqlite-3.36.0.$ArchName `
    -InstallTo $InstallRoot\sqlite-3.36.0\usr `
    -Arch $Arch `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
    }
}

function Build-System($Arch)
{
  Build-CMakeProject `
    -Src $SourceCache\swift-system `
    -Bin $BinaryCache\2 `
    -InstallTo $ToolchainInstallRoot\usr `
    -Arch $Arch `
    -UseBuiltCompilers C,Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
    }
}

function Build-ToolsSupportCore($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3

  Build-CMakeProject `
    -Src $SourceCache\swift-tools-support-core `
    -Bin $BinaryCache\3 `
    -InstallTo $ToolchainInstallRoot\usr `
    -Arch $Arch `
    -UseBuiltCompilers C,Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
      SwiftSystem_DIR = "$BinaryCache\2\cmake\modules";
      SQLite3_INCLUDE_DIR = "$InstallRoot\sqlite-3.36.0\usr\include";
      SQLite3_LIBRARY = "$InstallRoot\sqlite-3.36.0\usr\lib\SQLite3.lib";
    }
}

function Build-LLBuild($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3

  Build-CMakeProject `
    -Src $SourceCache\llbuild `
    -Bin $BinaryCache\4 `
    -InstallTo $ToolchainInstallRoot\usr `
    -Arch $Arch `
    -UseMSVCCompilers CXX `
    -UseBuiltCompilers Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      LLBUILD_SUPPORT_BINDINGS = "Swift";
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
      SQLite3_INCLUDE_DIR = "$InstallRoot\sqlite-3.36.0\usr\include";
      SQLite3_LIBRARY = "$InstallRoot\sqlite-3.36.0\usr\lib\SQLite3.lib";
    }
}

function Build-Yams($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3
  $XCTestBuildDir = Get-ProjectBuildDir $Arch 4

  Build-CMakeProject `
    -Src $SourceCache\Yams `
    -Bin $BinaryCache\5 `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
      XCTest_DIR = "$XCTestBuildDir\cmake\modules";
    }
}

function Build-ArgumentParser($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3
  $XCTestBuildDir = Get-ProjectBuildDir $Arch 4

  Build-CMakeProject `
    -Src $SourceCache\swift-argument-parser `
    -Bin $BinaryCache\6 `
    -InstallTo $ToolchainInstallRoot\usr `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      BUILD_TESTING = "NO";
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
      XCTest_DIR = "$XCTestBuildDir\cmake\modules";
    }
}

function Build-Driver($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3

  Build-CMakeProject `
    -Src $SourceCache\swift-driver `
    -Bin $BinaryCache\7 `
    -InstallTo $ToolchainInstallRoot\usr `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
      SwiftSystem_DIR = "$BinaryCache\2\cmake\modules";
      TSC_DIR = "$BinaryCache\3\cmake\modules";
      LLBuild_DIR = "$BinaryCache\4\cmake\modules";
      Yams_DIR = "$BinaryCache\5\cmake\modules";
      ArgumentParser_DIR = "$BinaryCache\6\cmake\modules";
      SQLite3_INCLUDE_DIR = "$InstallRoot\sqlite-3.36.0\usr\include";
      SQLite3_LIBRARY = "$InstallRoot\sqlite-3.36.0\usr\lib\SQLite3.lib";
    }
}

function Build-Crypto($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3

  Build-CMakeProject `
    -Src $SourceCache\swift-crypto `
    -Bin $BinaryCache\8 `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
    }
}

function Build-Collections($Arch)
{
  Build-CMakeProject `
    -Src $SourceCache\swift-collections `
    -Bin $BinaryCache\9 `
    -InstallTo $ToolchainInstallRoot\usr `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
    }
}

function Build-ASN1($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3

  Build-CMakeProject `
    -Src $SourceCache\swift-asn1 `
    -Bin $BinaryCache\10 `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
    }
}

function Build-Certificates($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3

  Build-CMakeProject `
    -Src $SourceCache\swift-certificates `
    -Bin $BinaryCache\11 `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
      SwiftASN1_DIR = "$BinaryCache\10\cmake\modules";
      SwiftCrypto_DIR = "$BinaryCache\8\cmake\modules";
    }
}

function Build-PackageManager($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3

  Build-CMakeProject `
    -Src $SourceCache\swift-package-manager `
    -Bin $BinaryCache\12 `
    -InstallTo $ToolchainInstallRoot\usr `
    -Arch $Arch `
    -UseBuiltCompilers C,Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
      CMAKE_Swift_FLAGS = "-DCRYPTO_v2";
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
      SwiftSystem_DIR = "$BinaryCache\2\cmake\modules";
      TSC_DIR = "$BinaryCache\3\cmake\modules";
      LLBuild_DIR = "$BinaryCache\4\cmake\modules";
      ArgumentParser_DIR = "$BinaryCache\6\cmake\modules";
      SwiftDriver_DIR = "$BinaryCache\7\cmake\modules";
      SwiftCrypto_DIR = "$BinaryCache\8\cmake\modules";
      SwiftCollections_DIR = "$BinaryCache\9\cmake\modules";
      SwiftASN1_DIR = "$BinaryCache\10\cmake\modules";
      SwiftCertificates_DIR = "$BinaryCache\11\cmake\modules";
      SQLite3_INCLUDE_DIR = "$InstallRoot\sqlite-3.36.0\usr\include";
      SQLite3_LIBRARY = "$InstallRoot\sqlite-3.36.0\usr\lib\SQLite3.lib";
    }
}

function Build-IndexStoreDB($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3

  Build-CMakeProject `
    -Src $SourceCache\indexstore-db `
    -Bin $BinaryCache\13 `
    -Arch $Arch `
    -UseBuiltCompilers C,CXX,Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "NO";
      CMAKE_C_FLAGS = "-Xclang -fno-split-cold-code";
      CMAKE_CXX_FLAGS = "-Xclang -fno-split-cold-code";
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
    }
}

function Build-Syntax($Arch)
{
  Build-CMakeProject `
    -Src $SourceCache\swift-syntax `
    -Bin $BinaryCache\14 `
    -InstallTo $ToolchainInstallRoot\usr `
    -Arch $Arch `
    -UseBuiltCompilers Swift `
    -BuildDefaultTarget `
    -Defines @{
      BUILD_SHARED_LIBS = "YES";
    }
}

function Build-SourceKitLSP($Arch)
{
  $DispatchBuildDir = Get-ProjectBuildDir $Arch 2
  $FoundationBuildDir = Get-ProjectBuildDir $Arch 3

  Build-CMakeProject `
    -Src $SourceCache\sourcekit-lsp `
    -Bin $BinaryCache\15 `
    -InstallTo $ToolchainInstallRoot\usr `
    -Arch $Arch `
    -UseBuiltCompilers C,Swift `
    -BuildDefaultTarget `
    -Defines @{
      dispatch_DIR = "$DispatchBuildDir\cmake\modules";
      Foundation_DIR = "$FoundationBuildDir\cmake\modules";
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

function Build-Installer()
{
  # Currently fails due to _InternalSwiftScan paths
  # Build-WiXProject toolchain.wixproj -Properties @{
  #   DEVTOOLS_ROOT = "$ToolchainInstallRoot\";
  #   TOOLCHAIN_ROOT = "$ToolchainInstallRoot\";
  # }

  # TODO: The XCTest depends on the architecture
  # Build-WiXProject sdk.wixproj -Properties @{
  #   PLATFORM_ROOT = "$PlatformInstallRoot\";
  #   SDK_ROOT = "$SDKInstallRoot\";
  #   SWIFT_SOURCE_DIR = "$SourceCache\swift\";
  # }

  Build-WiXProject runtime.wixproj -Properties @{
    SDK_ROOT = (Get-RuntimeInstallDir $ArchX64) + "\";
  }

  Build-WiXProject devtools.wixproj -Properties @{
    DEVTOOLS_ROOT = "$ToolchainInstallRoot\";
  }

  # TODO: The above wixprojs need to build
  # Build-WiXProject installer.wixproj -Properties @{
  #   OutputPath = "$BinaryCache\";
  #   MSI_LOCATION = "$BinaryCache\msi\";
  # }
}

#-------------------------------------------------------------------

Build-Compilers $HostArch

foreach ($Arch in $SDKArchs)
{
  Build-ZLib $Arch
  Build-XML2 $Arch
  Build-CURL $Arch
  Build-ICU $Arch
  Build-LLVM $Arch
  Build-Runtime $Arch
  Build-Dispatch $Arch
  Build-Foundation $Arch
  Build-XCTest $Arch
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

# Switch to swift-driver
Copy-Item -Force $BinaryCache\7\bin\swift-driver.exe $ToolchainInstallRoot\usr\bin\swift.exe
Copy-Item -Force $BinaryCache\7\bin\swift-driver.exe $ToolchainInstallRoot\usr\bin\swiftc.exe

$python = "${Env:ProgramFiles(x86)}\Microsoft Visual Studio\Shared\Python39_64\python.exe"
if (-not (Test-Path $python))
{
  $python = (where.exe python) | Select-Object -First 1
  if (-not (Test-Path $python))
  {
    throw "Python.exe not found"
  }
}

# SDKSettings.plist
& $python -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'DEFAULT_USE_RUNTIME': 'MD' } }), encoding='utf-8'))" > $SDKInstallRoot\SDKSettings.plist

# Info.plist
& $python -c "import plistlib; print(str(plistlib.dumps({ 'DefaultProperties': { 'XCTEST_VERSION': 'development' } }), encoding='utf-8'))" > $PlatformInstallRoot\Info.plist
