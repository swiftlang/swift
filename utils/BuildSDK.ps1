# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

<#
.SYNOPSIS
Builds the Swift SDK for Windows or Android.

.DESCRIPTION
Builds the runtime, the overlays, the supplemental libraries, libdispatch,
Foundation and its dependencies, swift-testing and XCTest for each architecture,
with the Stage1 compilers and the macros that BuildToolchain.ps1 -Mode Bootstrap
built. Installs them in the platform root of the image, and the Windows runtime
next to the toolchain.

The architectures install some files to the same paths, so builds of different
architectures that run at the same time need their own platform roots, which
can be merged afterwards.

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

.PARAMETER Platform
The platform of the SDK: Windows or Android.
Default: Windows

.PARAMETER Architectures
The architectures to build the SDK for: X64, X86 and Arm64 for Windows, and
aarch64, armv7, i686 and x86_64 for Android.
Default: the architecture of the host for Windows, all of them for Android.

.PARAMETER SupportedArchitectures
The architectures that the SDK settings list, when other invocations build the
other architectures.
Default: Architectures

.PARAMETER LinkModes
The kinds of runtime libraries to build: dynamic, static or both.
Default: dynamic, static

.PARAMETER PlatformRoot
The directory of the platform to install the SDK into.
Default: <BuildRoot>\Program Files\Swift\Platforms\<Platform>.platform

.PARAMETER EnableCaching
Enable build caching using LLVM CAS to speed up rebuilds.

.PARAMETER ProductVersion
The product version of the runtime and of the SDK. Default: "0.0.0"

.PARAMETER WindowsSxSAssemblyPublicKeyToken
The public key token to embed in Windows side-by-side assembly manifests.
Default: "0000000000000000"

.PARAMETER HostArchName
The architecture where the toolchain will execute. The Windows runtime of this
architecture is installed in "Program Files", the others in
"Program Files (<arch>)". Valid values: AMD64, ARM64

.PARAMETER DebugInfo
Include debug information in the builds.

.PARAMETER AndroidAPILevel
The API Level to target when building the Android SDK. Must be between 21 and 36.
Default: 23

.PARAMETER IncludeDS2
Include the ds2 remote debug server in the Android SDK.

.PARAMETER Summary
Display a build time summary at the end of the build.

.PARAMETER Trace
Write the build timings to build-timings-<name>.json in the binary cache.

.EXAMPLE
PS> .\BuildSDK.ps1 -Architectures X64,X86,Arm64

.EXAMPLE
PS> .\BuildSDK.ps1 -Platform Android -Architectures aarch64 -LinkModes static
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

  [ValidateSet("Windows", "Android")]
  [string] $Platform = "Windows",
  [string[]] $Architectures = @(),
  [string[]] $SupportedArchitectures = @(),
  [ValidatePattern("^(dynamic|static)(,(dynamic|static))*$")]
  [string[]] $LinkModes = @("dynamic", "static"),
  [string] $PlatformRoot = "",

  [switch] $EnableCaching,

  [string] $ProductVersion = "0.0.0",
  [ValidatePattern("^[A-Fa-f0-9]{16}$")]
  [string] $WindowsSxSAssemblyPublicKeyToken = "0000000000000000",
  [ValidateSet("AMD64", "ARM64")]
  [string] $HostArchName = $(if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }),

  [switch] $DebugInfo,
  [ValidateRange(21, 36)]
  [int] $AndroidAPILevel = 23,
  [switch] $IncludeDS2 = $false,

  [switch] $Summary,
  [switch] $Trace
)

. "$PSScriptRoot\BuildSupport.ps1"

$WindowsSxSAssemblyPublicKeyToken = $WindowsSxSAssemblyPublicKeyToken.ToLowerInvariant()

# Work around limitations of cmd passing in array arguments via powershell.exe -File
if ($Architectures.Length -eq 1) { $Architectures = $Architectures[0].Split(",") }

if ($SupportedArchitectures.Length -eq 1) { $SupportedArchitectures = $SupportedArchitectures[0].Split(",") }

if ($LinkModes.Length -eq 1) { $LinkModes = $LinkModes[0].Split(",") }

$OS = [OS]$Platform

if (-not $Architectures) {
  $Architectures = switch ($OS) {
    Windows { if ($HostArchName -eq "ARM64") { @("Arm64") } else { @("X64") } }
    Android { @("aarch64", "armv7", "i686", "x86_64") }
  }
}

if (-not $SupportedArchitectures) {
  $SupportedArchitectures = $Architectures
}

if ($OS -eq [OS]::Android -and ($HostPlatform -ne $KnownPlatforms["WindowsX64"])) {
  throw "Unsupported host architecture for building android SDKs"
}

function Get-WindowsSDKBuilds([string[]] $Architectures) {
  return @($Architectures | ForEach-Object {
    switch ($_) {
      "X64" { $KnownPlatforms["WindowsX64"] }
      "X86" { $KnownPlatforms["WindowsX86"] }
      "Arm64" { $KnownPlatforms["WindowsArm64"] }
      default { throw "No Windows platform for architecture $_" }
    }
  })
}

function Get-AndroidSDKBuilds([string[]] $Architectures) {
  return @($Architectures | ForEach-Object {
    switch ($_) {
      "aarch64" { $KnownPlatforms["AndroidARM64"] }
      "armv7" { $KnownPlatforms["AndroidARMv7"] }
      "i686" { $KnownPlatforms["AndroidX86"] }
      "x86_64" { $KnownPlatforms["AndroidX64"] }
      default { throw "No Android platform for architecture $_" }
    }
  })
}

# The architectures to build, and the ones that the SDK supports.
if ($OS -eq [OS]::Windows) {
  $SDKBuilds = @(Get-WindowsSDKBuilds $Architectures)
  $SupportedSDKBuilds = @(Get-WindowsSDKBuilds $SupportedArchitectures)
} else {
  $SDKBuilds = @(Get-AndroidSDKBuilds $Architectures)
  $SupportedSDKBuilds = @(Get-AndroidSDKBuilds $SupportedArchitectures)
}
foreach ($Build in $SDKBuilds) {
  $Build.LinkModes = $LinkModes
}

# Install into -PlatformRoot when it is set.
if ($PlatformRoot) {
  function Get-PlatformRoot([OS] $OS) {
    return $PlatformRoot
  }
}

# ds2 and its generator build with the compilers of the host, like the tools in
# BuildToolchain.ps1.
$Compilers.Host = if ($EnableCaching) { $Compilers.Pinned } else { $Compilers.MSVC }

function Get-FlexExecutable {
  return (Get-DependencyManifest).FlexBison.Flex
}

function Get-BisonExecutable {
  return (Get-DependencyManifest).FlexBison.Bison
}

# Runs a block while no other build script runs a block of the same name.
function Invoke-Exclusively([string] $Name, [ScriptBlock] $Block) {
  $Mutex = New-Object System.Threading.Mutex($false, "Global\swift-build-$Name")
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

function ConvertTo-ThickLayout([Hashtable] $Platform, [string] $Resources, [string[]] $Filter) {
  Get-ChildItem "${Resources}\*" -File -Include ${Filter} -ErrorAction Ignore | ForEach-Object {
    Write-Host -BackgroundColor DarkRed -ForegroundColor White "$($_.FullName) is not nested in an architecture directory"
    Move-Item -Force $_.FullName "$Resources\$($Platform.Architecture.LLVMName)\" | Out-Null
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
      $SDKSettings.SupportedTargets.windows.Archs = $SupportedSDKBuilds | ForEach-Object { $_.Architecture.LLVMName } | Sort-Object
    }
    Android {
      $SDKSettings.SupportedTargets.android.LLVMTargetVendor = "unknown"
      $SDKSettings.SupportedTargets.android.LLVMTargetSys = "linux"
      $SDKSettings.SupportedTargets.android.LLVMTargetTripleEnvironment = "android${AndroidAPILevel}"
      $SDKSettings.SupportedTargets.android.Archs = $SupportedSDKBuilds | ForEach-Object { $_.Architecture.LLVMName } | Sort-Object
    }
  }
  $SDKSettings | ConvertTo-JSON -Depth 4 | Out-FIle -FilePath "$(Get-SwiftSDK -OS $OS -Identifier $Identifier)\SDKSettings.json"
  Write-PList -Settings $SDKSettings -Path "$(Get-SwiftSDK -OS $OS -Identifier $Identifier)\SDKSettings.plist"
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


  # TODO: remove this once the migration is completed. Resync writes into the
  # source tree, which the builds of other architectures share.
  Invoke-Exclusively "Resync" {
    Invoke-IsolatingEnvVars {
      Invoke-VsDevShell $BuildPlatform

      Push-Location "${SourceCache}\swift\Runtimes"
      $env:CMAKE_ROOT = (Get-CMake).CMakeRoot
      $CMakeBin = (Get-CMake).Path
      Start-Process -Wait -WindowStyle Hidden -FilePath $CMakeBin -ArgumentList @("-P", "Resync.cmake")
      Pop-Location
    }
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

#-------------------------------------------------------------------

try {

if ($IncludeDS2) {
  # The builds of the other platforms share the build tree of this tool.
  Invoke-Exclusively "RegsGen2" {
    Invoke-BuildStep Build-RegsGen2 $BuildPlatform
  }
}

if ($OS -eq [OS]::Windows) {
  Build-SDKDependencies $SDKBuilds -Compilers $Compilers.Stage1

  $SDKROOT = Get-SwiftSDK -OS Windows
  foreach ($Build in $SDKBuilds) {
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

  Install-SDK $SDKBuilds
  Write-SDKSettings Windows

  Write-PlatformInfoPlist Windows


  # Copy static dependencies
  foreach ($Build in $SDKBuilds) {
    if (-not $Build.LinkModes.Contains("static")) { continue }

    $SwiftResourceDir = "${SDKROOT}\usr\lib\swift_static\$($Build.OS.ToString().ToLowerInvariant())\$($Build.Architecture.LLVMName)"
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build brotli)\brotlicommon.lib" -Destination "${SwiftResourceDir}\brotlicommon.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build brotli)\brotlidec.lib" -Destination "${SwiftResourceDir}\brotlidec.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build CURL)\lib\libcurl.lib" -Destination "${SwiftResourceDir}\libcurl.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build XML2)\libxml2s.lib" -Destination "${SwiftResourceDir}\libxml2s.lib" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build ZLib)\libzs.lib" -Destination "${SwiftResourceDir}\libzs.lib" | Out-Null
  }
} else {
  Build-SDKDependencies $SDKBuilds -Compilers $Compilers.Stage1

  $SDKROOT = Get-SwiftSDK -OS Android
  foreach ($Build in $SDKBuilds) {
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

  Install-SDK $SDKBuilds
  Write-SDKSettings Android

  Write-PlatformInfoPlist Android

  # Android swift-inspect only supports 64-bit platforms.
  $SDKBuilds | Where-Object { @("arm64-v8a", "x86_64") -contains $_.Architecture.ABI } | ForEach-Object {
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
  foreach ($Build in $SDKBuilds) {
    if (-not $Build.LinkModes.Contains("static")) { continue }

    $SwiftResourceDir = "${SDKROOT}\usr\lib\swift_static\$($Build.OS.ToString().ToLowerInvariant())\$($Build.Architecture.LLVMName)"
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build brotli)\libbrotlicommon.a" -Destination "${SwiftResourceDir}\libbrotlicommon.a" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build brotli)\libbrotlidec.a" -Destination "${SwiftResourceDir}\libbrotlidec.a" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build CURL)\lib\libcurl.a" -Destination "${SwiftResourceDir}\libcurl.a" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build XML2)\libxml2.a" -Destination "${SwiftResourceDir}\libxml2.a" | Out-Null
    Copy-Item -Force -Path "$(Get-ProjectBinaryCache $Build ZLib)\libz.a" -Destination "${SwiftResourceDir}\libz.a" | Out-Null
  }
}

} catch {
  Write-BuildError $_
  exit 1
} finally {
  Write-BuildTimings "BuildSDK-$Platform-$($Architectures -join "-")"
}
