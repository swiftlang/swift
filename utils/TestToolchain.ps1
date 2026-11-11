# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

<#
.SYNOPSIS
Runs the tests of the Swift toolchain.

.DESCRIPTION
Runs the tests of the toolchain that BuildToolchain.ps1 built, against the
Windows SDK of the host that BuildSDK.ps1 built.

The test suites of the compilers (swift, lld, lldb, ...) run in the build tree
of the compilers. The Build step configures it for testing and builds what the
requested suites need, and the Run step runs them. Once the Build step is done,
several Run steps can run different suites at the same time. The other tests
build and run in their own build trees in the Run step.

.PARAMETER SourceCache
The path to a directory where projects contributing to the Swift toolchain have
been cloned.
Default: 'S:\SourceCache'

.PARAMETER BinaryCache
The path to a directory where to write build system files and outputs.
Default: 'S:\BinaryCache'

.PARAMETER BuildRoot
The path to the merged file system image populated by the build.
ImageRoot is retained as a compatibility alias.
Default: 'S:\'

.PARAMETER ObjectStore
The path to the content-addressed object store used for build caching.
Cache is retained as a compatibility alias.
Default: 'S:\ObjectStore'

.PARAMETER Test
An array of names of projects to run tests for. Use '*' to run all tests.
Available tests: lld, lldb, lldb-swift, swift, dispatch, foundation, xctest,
swift-format, sourcekit-lsp, llbuild, swiftpm

.PARAMETER Steps
Build, Run, or both.
Default: Build, Run

.PARAMETER EnableCaching
Enable build caching using LLVM CAS, as for the build of the toolchain.

.PARAMETER ProductVersion
The product version of the toolchain. Default: "0.0.0"

.PARAMETER WindowsSxSAssemblyPublicKeyToken
The public key token to embed in Windows side-by-side assembly manifests.
Default: "0000000000000000"

.PARAMETER HostArchName
The architecture where the toolchain executes. Valid values: AMD64, ARM64

.PARAMETER DebugInfo
Include debug information, as in the build of the toolchain.

.PARAMETER FoundationTestConfiguration
The configuration of the Foundation tests: debug or release.
Default: debug

.PARAMETER Summary
Display a build time summary at the end of the build.

.PARAMETER Trace
Write the build timings to build-timings-<name>.json in the binary cache.

.EXAMPLE
PS> .\TestToolchain.ps1 -Test swift,lld

.EXAMPLE
PS> .\TestToolchain.ps1 -Test swift,lld -Steps Build
PS> .\TestToolchain.ps1 -Test swift -Steps Run
PS> .\TestToolchain.ps1 -Test lld -Steps Run
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

  [string[]] $Test = @(),
  [ValidatePattern("^(Build|Run)(,(Build|Run))*$")]
  [string[]] $Steps = @("Build", "Run"),

  [switch] $EnableCaching,

  [string] $ProductVersion = "0.0.0",
  [ValidatePattern("^[A-Fa-f0-9]{16}$")]
  [string] $WindowsSxSAssemblyPublicKeyToken = "0000000000000000",
  [ValidateSet("AMD64", "ARM64")]
  [string] $HostArchName = $(if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }),

  [switch] $DebugInfo,
  [ValidateSet("debug", "release")]
  [string] $FoundationTestConfiguration = "debug",

  [switch] $Summary,
  [switch] $Trace
)

. "$PSScriptRoot\BuildSupport.ps1"

$WindowsSxSAssemblyPublicKeyToken = $WindowsSxSAssemblyPublicKeyToken.ToLowerInvariant()

# Work around limitations of cmd passing in array arguments via powershell.exe -File
if ($Test.Length -eq 1) { $Test = $Test[0].Split(",") }

if ($Steps.Length -eq 1) { $Steps = $Steps[0].Split(",") }

if ($Test -contains "*") {
  # Explicitly don't include llbuild yet since tests are known to fail on Windows
  $Test = @("lld", "lldb", "lldb-swift", "swift", "dispatch", "foundation", "xctest", "swift-format", "sourcekit-lsp")
}

# Avoid $env:ProgramFiles in case this script is running as x86
$UnixToolsBinDir = "$env:SystemDrive\Program Files\Git\usr\bin"

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

# The test suites of the compilers, with the targets that build what they need
# and the targets that run them.
$CompilerTestSuites = [ordered]@{
  "llvm"       = @{ Depends = @("llvm-test-depends");  Target = "check-llvm" };
  "clang"      = @{ Depends = @("clang-test-depends"); Target = "check-clang" };
  "lld"        = @{ Depends = @("lld-test-depends");   Target = "check-lld" };
  # check-swift does not depend on SwiftCompilerPlugin, but the macro tests need it.
  "swift"      = @{ Depends = @("SwiftCompilerPlugin", "swift-test-depends"); Target = "check-swift" };
  "lldb"       = @{ Depends = @("lldb-api-test-deps", "lldb-shell-test-deps", "lldb-unit-test-deps"); Target = "check-lldb" };
  "lldb-swift" = @{ Depends = @("lldb-test-deps", "lldb-unit-test-deps"); Target = "check-lldb-swift" };
}

# Runs the lit command of a check target, without building what it depends on.
# Ninja runs these commands one at a time, so running them directly lets the
# suites of one build tree run at the same time.
function Invoke-LitTarget([string] $Bin, [string] $Target) {
  $Ninja = (Get-Ninja).Path

  # Follow the phony targets of CMake to the custom command that runs lit.
  $Nodes = New-Object System.Collections.Queue
  $Nodes.Enqueue($Target)
  $Commands = @()
  while ($Nodes.Count -gt 0) {
    $Node = $Nodes.Dequeue()
    $Query = @(& $Ninja -C $Bin -t query $Node)
    if ($LastExitCode -ne 0) {
      throw "ninja could not find '$Node' in '$Bin'"
    }
    $Rule = ""
    $Inputs = @()
    foreach ($Line in $Query) {
      if ($Line -match "^  input: (.+)$") {
        $Rule = $Matches[1]
      } elseif ($Line -match "^  outputs:") {
        break
      } elseif ($Line -match "^    ([^|].*)$") {
        $Inputs += $Matches[1]
      }
    }
    if ($Rule -eq "phony") {
      foreach ($InputNode in $Inputs) {
        if ($InputNode -match "/CMakeFiles/|(^|/)check-[^/]*$") {
          $Nodes.Enqueue($InputNode)
        }
      }
    } elseif ($Rule) {
      $Commands += @(& $Ninja -C $Bin -t commands -s $Node | Where-Object { $_ -match "lit" })
    }
  }
  if ($Commands.Count -ne 1) {
    throw "Expected one command for '$Target', found $($Commands.Count)."
  }

  # Run the command line as ninja would.
  if ($Commands[0] -notmatch '^("[^"]+"|\S+)\s+(.*)$') {
    throw "Cannot parse the command of '$Target': $($Commands[0])"
  }
  Write-Host $Commands[0]
  $Process = Start-Process -FilePath $Matches[1].Trim('"') -ArgumentList $Matches[2] -NoNewWindow -PassThru
  # Windows PowerShell only reports the exit code after the handle was opened.
  $null = $Process.Handle
  $Process.WaitForExit()
  if ($Process.ExitCode -ne 0) {
    throw "$Target failed with exit code $($Process.ExitCode)"
  }
}

function Test-Compilers([Hashtable] $Platform, [string[]] $Suites, [switch] $Build, [switch] $Run) {
  Invoke-IsolatingEnvVars {
    $SwiftSDK = Get-SwiftSDK -OS $Platform.OS
    $SwiftRuntime = Get-SDKRuntimeBin $Platform $SwiftSDK
    $Stage2BinDir = [IO.Path]::Combine((Get-ProjectBinaryCache $Platform Stage2Compilers), "bin")
    $CDispatchBinaryCache = Get-ProjectBinaryCache $Platform DynamicCDispatch
    $env:Path = "$SwiftRuntime;$Stage2BinDir;$CDispatchBinaryCache;$CDispatchBinaryCache\bin;$(Get-CMarkBinaryCache $Platform)\src;$env:Path;$VSInstallRoot\DIA SDK\bin\$($HostPlatform.Architecture.VSName);$UnixToolsBinDir"
    $env:PYTHONUTF8 = "1"
    $CompilerCache = Get-ProjectBinaryCache $Platform Stage2Compilers

    if ($Build) {
      # CMake keeps the configuration that BuildToolchain.ps1 set up, and only
      # changes what the tests need.
      $TestingDefines = @{
        SWIFT_BUILD_DYNAMIC_SDK_OVERLAY = "YES";
        SWIFT_BUILD_DYNAMIC_STDLIB = "YES";
        SWIFT_BUILD_REMOTE_MIRROR = "YES";
        # Stage2 tests must use the freshly-built native tools, not Stage1.
        SWIFT_NATIVE_SWIFT_TOOLS_PATH = $Stage2BinDir;
        # check-swift depends on swift-backtrace-${SDK}; build the libexec helpers
        # with the rest of the test targets.
        SWIFT_BUILD_LIBEXEC = "YES";
        # Keep %host-build-swift on the same platform SDK that Stage2 uses.
        SWIFT_HOST_SDKROOT = $SwiftSDK;
      }

      if ($Suites -contains "lldb" -or $Suites -contains "lldb-swift") {
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
      Build-CMakeProject @BuildCMakeArgs -BuildTargets @($Suites | ForEach-Object { $CompilerTestSuites[$_].Depends })
    }

    if ($Run) {
      foreach ($Suite in $Suites) {
        Record-OperationTime $Platform $CompilerTestSuites[$Suite].Target {
          Invoke-IsolatingEnvVars {
            # The tests build C and C++ code, as when ninja runs them.
            Invoke-VsDevShell $Platform
            if ($Suite -eq "lldb" -or $Suite -eq "lldb-swift") {
              # Override test filter for known issues in downstream LLDB
              Load-LitTestOverrides ([IO.Path]::GetFullPath([IO.Path]::Combine($PSScriptRoot, "..", "..", "llvm-project", "lldb", "test", "windows-swift-llvm-lit-test-overrides.txt")))
              $env:SDKROOT = $SwiftSDK
            }
            Invoke-LitTarget $CompilerCache $CompilerTestSuites[$Suite].Target
          }
        }
      }
    }
  }
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

function Test-Testing {
  throw "testing Testing is not supported"
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

#-------------------------------------------------------------------

try {

$CompilerSuites = @($Test | Where-Object { $CompilerTestSuites.Contains($_) })
# check-lldb includes the tests of check-lldb-swift.
if ($CompilerSuites -contains "lldb") {
  $CompilerSuites = @($CompilerSuites | Where-Object { $_ -ne "lldb-swift" })
}

if ($CompilerSuites -and $Steps -contains "Build") {
  Invoke-BuildStep Test-Compilers $HostPlatform -Suites $CompilerSuites -Build -SummaryVariant "Build"
}

if ($Steps -contains "Run") {
  if ($CompilerSuites) {
    Invoke-BuildStep Test-Compilers $HostPlatform -Suites $CompilerSuites -Run -SummaryVariant "Run"
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
}

} catch {
  Write-BuildError $_
  exit 1
} finally {
  Write-BuildTimings "TestToolchain-$($Steps -join '')-$($Test -join '-')"
}
