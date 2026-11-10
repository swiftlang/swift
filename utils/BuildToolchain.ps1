# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

<#
.SYNOPSIS
Builds the Swift toolchain.

.DESCRIPTION
With -Mode Bootstrap, builds the Stage1 toolchain with the pinned toolchain
that SetupDependencies.ps1 downloaded: the tools that run during the build, the
Stage1 compilers, and the macros that the SDKs use. They stay in the binary
cache, where BuildSDK.ps1 and the toolchain build find them.

Otherwise, builds the toolchain with the Stage1 compilers, against the Windows
SDK that BuildSDK.ps1 built for the host: the compilers, their runtimes for the
SDK architectures, the macros and the developer tools. It installs them in the
image and binds the executables to the Swift runtime.

The two modes configure the compilers with different CMake caches,
cmake\caches\Windows-Bootstrap-Stage1-<arch>.cmake and
cmake\caches\Windows-<arch>.cmake.

.PARAMETER SourceCache
The path to a directory where projects contributing to the Swift toolchain have
been cloned.
Default: 'S:\SourceCache'

.PARAMETER BinaryCache
The path to a directory where to write build system files and outputs.
Default: 'S:\BinaryCache'

.PARAMETER BuildRoot
The path to the merged file system image populated by the build. The
"Program Files" subdirectories will be created beneath this directory.
ImageRoot is retained as a compatibility alias.
Default: 'S:\'

.PARAMETER ObjectStore
The path to the content-addressed object store used for build caching.
Cache is retained as a compatibility alias.
Default: 'S:\ObjectStore'

.PARAMETER Mode
Bootstrap to build the Stage1 toolchain, or empty to build the toolchain.
Default: ""

.PARAMETER EnableCaching
Enable build caching using LLVM CAS to speed up rebuilds.

.PARAMETER ProductVersion
The product version of the toolchain. Default: "0.0.0"

.PARAMETER WindowsSxSAssemblyPublicKeyToken
The public key token to embed in Windows side-by-side assembly manifests.
Default: "0000000000000000"

.PARAMETER ToolchainIdentifier
The toolchain version identifier for the toolchain being built.
Default: Uses TOOLCHAIN_VERSION environment variable or "$USERNAME.development"

.PARAMETER HostArchName
The architecture where the toolchain will execute. Valid values: AMD64, ARM64

.PARAMETER UseHostToolchain
Build the Stage1 toolchain with MSVC rather than with the pinned toolchain.

.PARAMETER DebugInfo
Include debug information in the builds.

.PARAMETER LTO
The kind of link time optimization of the compilers: none, full or thin.

.PARAMETER IncludeNoAsserts
Build and include the no-assert toolchain variant in the output.

.PARAMETER WindowsSDKArchitectures
The architectures of the Windows SDK to build the compiler runtimes for.

.PARAMETER AndroidSDKArchitectures
The architectures of the Android SDK to build the compiler runtimes for.

.PARAMETER AndroidAPILevel
The API Level of the Android SDK. Default: 23

.PARAMETER Summary
Display a build time summary at the end of the build.

.PARAMETER Trace
Write the build timings to build-timings-<name>.json in the binary cache.

.EXAMPLE
PS> .\BuildToolchain.ps1 -Mode Bootstrap

.EXAMPLE
PS> .\BuildToolchain.ps1 -WindowsSDKArchitectures X64,Arm64
#>
[CmdletBinding(PositionalBinding = $false)]
param
(
  [System.IO.FileInfo] $SourceCache = "S:\SourceCache",
  [System.IO.FileInfo] $BinaryCache = "S:\BinaryCache",
  [Alias("ImageRoot")]
  [System.IO.FileInfo] $BuildRoot = "S:",
  [Alias("Cache")]
  [System.IO.FileInfo] $ObjectStore = "S:\ObjectStore",

  [ValidateSet("", "Bootstrap")]
  [string] $Mode = "",

  [switch] $EnableCaching,

  [string] $ProductVersion = "0.0.0",
  [ValidatePattern("^[A-Fa-f0-9]{16}$")]
  [string] $WindowsSxSAssemblyPublicKeyToken = "0000000000000000",
  [string] $ToolchainIdentifier = $(if ($env:TOOLCHAIN_VERSION) { $env:TOOLCHAIN_VERSION } else { "$env:USERNAME.development" }),

  # Toolchain Cross-compilation
  [ValidateSet("AMD64", "ARM64")]
  [string] $HostArchName = $(if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }),
  [object] $UseHostToolchain = $true,

  [switch] $DebugInfo,
  [ValidateSet("none", "full", "thin")]
  [string] $LTO = "none",
  [switch] $IncludeNoAsserts = $false,
  [string[]] $WindowsSDKArchitectures = @(),
  [string[]] $AndroidSDKArchitectures = @(),
  [ValidateRange(21, 36)]
  [int] $AndroidAPILevel = 23,

  [switch] $Summary,
  [switch] $Trace
)

. "$PSScriptRoot\BuildSupport.ps1"

$WindowsSxSAssemblyPublicKeyToken = $WindowsSxSAssemblyPublicKeyToken.ToLowerInvariant()

# Work around limitations of cmd passing in array arguments via powershell.exe -File
if ($AndroidSDKArchitectures.Length -eq 1) { $AndroidSDKArchitectures = $AndroidSDKArchitectures[0].Split(",") }

if ($WindowsSDKArchitectures.Length -eq 1) { $WindowsSDKArchitectures = $WindowsSDKArchitectures[0].Split(",") }

if ($UseHostToolchain -is [string]) {
  $UseHostToolchain = [System.Convert]::ToBoolean($UseHostToolchain)
}

if ($EnableCaching) {
  $UseHostToolchain = $false
}

if ($AndroidSDKArchitectures -and ($HostPlatform -ne $KnownPlatforms["WindowsX64"])) {
  throw "Unsupported host architecture for building android SDKs"
}

# The compilers have runtimes for each SDK architecture.
$CompilerRuntimeBuilds = @($WindowsSDKArchitectures | ForEach-Object {
  switch ($_) {
    "X64" { $KnownPlatforms["WindowsX64"] }
    "X86" { $KnownPlatforms["WindowsX86"] }
    "Arm64" { $KnownPlatforms["WindowsArm64"] }
    default { throw "No Windows platform for architecture $_" }
  }
}) + @($AndroidSDKArchitectures | ForEach-Object {
  switch ($_) {
    "aarch64" { $KnownPlatforms["AndroidARM64"] }
    "armv7" { $KnownPlatforms["AndroidARMv7"] }
    "i686" { $KnownPlatforms["AndroidX86"] }
    "x86_64" { $KnownPlatforms["AndroidX64"] }
    default { throw "No Android platform for architecture $_" }
  }
})

$PythonVersion = (Get-DependencyManifest).Python.Version

$Compilers.Host = @{
  C = if ($UseHostToolchain) { $Compilers.MSVC.C } else { $Compilers.Pinned.C }
  CXX = if ($UseHostToolchain) { $Compilers.MSVC.CXX } else { $Compilers.Pinned.CXX }
}

$Assemblers.Host = if ($UseHostToolchain) { $Assemblers.MSVC } else { $Assemblers.Pinned }

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
    LLDB_TEST_MAKE = (Get-DependencyManifest).GnuMake;
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

#-------------------------------------------------------------------

try {

if ($Mode -eq "Bootstrap") {
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

  # ── Bootstrap Macros ──────────────────────────────────────────────────────
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
} else {
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

  # ── Stage2 Compiler Runtimes ──────────────────────────────────────────────
  $CompilerRuntimeBuilds | ForEach-Object {
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

  Invoke-BuildStep Build-PackageManagerRuntime $HostPlatform @{
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

} catch {
  Write-BuildError $_
  exit 1
} finally {
  Write-BuildTimings "BuildToolchain$(if ($Mode) { "-$Mode" })"
}
