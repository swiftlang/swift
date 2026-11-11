# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

# The helpers that the scripts building the Windows toolchain share: running
# programs and CMake projects, the layout of the build tree and of the image,
# the compilers, and the build timings. The scripts dot-source this file after
# their parameters. It knows nothing about the projects that they build.
#
# The scripts define $SourceCache, $BinaryCache, $BuildRoot, $ObjectStore,
# $ProductVersion, $HostArchName, $DebugInfo, $EnableCaching, $Summary and
# $Trace. SetupDependencies.ps1 writes the paths of the tools it downloads to
# <BinaryCache>\dependencies.json.

## Prepare the build environment.

$ErrorActionPreference = "Stop"
Set-StrictMode -Version 3.0
# Share Clang modules across build phases without using the per-user cache.
$ModuleCache = "$BinaryCache\ModuleCache"
$env:CLANG_MODULE_CACHE_PATH = $ModuleCache

# Avoid being run in a "Developer" shell since this script launches its own sub-shells targeting
# different architectures, and these variables cause confusion.
if ($env:VSCMD_ARG_HOST_ARCH -or $env:VSCMD_ARG_TGT_ARCH) {
  throw "At least one of VSCMD_ARG_HOST_ARCH and VSCMD_ARG_TGT_ARCH is set, which is incompatible with this script. Likely need to run outside of a Developer shell."
}

# Prevent elsewhere-installed swift modules from confusing our builds.
$env:SDKROOT = ""

$DependencyManifestPath = "$BinaryCache\dependencies.json"
$Dependencies = if (Test-Path $DependencyManifestPath) {
  Get-Content -Raw $DependencyManifestPath | ConvertFrom-Json
} else {
  $null
}

function Get-DependencyManifest {
  if (-not $Dependencies) {
    throw "'$DependencyManifestPath' does not exist; run SetupDependencies.ps1 first."
  }
  return $Dependencies
}

## Platforms.

# Only the scripts that build for Android take an API level.
if (-not (Get-Variable AndroidAPILevel -ErrorAction Ignore)) {
  $AndroidAPILevel = 23
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
    DebugFormat = "codeview";
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
    DebugFormat = "codeview";
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
    DebugFormat = "codeview";
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
    DebugFormat = "dwarf";
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
    DebugFormat = "dwarf";
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
    DebugFormat = "dwarf";
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
    DebugFormat = "dwarf";
  };
}

$BuildArchName = if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }
# TODO: Support other cross-compilation scenarios.
$BuildOS = [OS]::Windows
$HostOS = [OS]::Windows

$vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$VSInstallRoot = & $vswhere -nologo -latest -products "*" -all -prerelease -property installationPath

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

## Helpers for logging and timing build steps.

$TimingData = New-Object System.Collections.Generic.List[System.Object]
$CurrentOperation = $null
$BuildStartTime = [DateTime]::UtcNow
$BuildStopwatch = [Diagnostics.Stopwatch]::StartNew()
$RecordTimings = $Summary -or $Trace

if ($RecordTimings) {
  Add-Type -Namespace Win32 -Name Kernel32 -MemberDefinition @"
[DllImport("kernel32.dll")]
public static extern bool GetSystemTimes(out long lpIdleTime, out long lpKernelTime, out long lpUserTime);
"@
}

# Returns the time all processors have spent busy and idle since boot.
function Get-ProcessorTimes {
  $IdleTime = $KernelTime = $UserTime = [long] 0
  $null = [Win32.Kernel32]::GetSystemTimes([ref] $IdleTime, [ref] $KernelTime, [ref] $UserTime)
  # The kernel time includes the idle time.
  return @{ Busy = $KernelTime + $UserTime - $IdleTime; Idle = $IdleTime }
}

$BuildStartProcessorTimes = if ($RecordTimings) { Get-ProcessorTimes } else { $null }

function Add-TimingData {
  param
  (
    [Parameter(Mandatory)]
    [Hashtable] $Platform,
    [Parameter(Mandatory)]
    [string] $BuildStep,
    [string] $Variant = "",
    [PSCustomObject] $Parent = $null
  )

  $TimingEntry = [PSCustomObject]@{
    Arch = $Platform.Architecture.LLVMName
    Platform = $Platform.OS.ToString()
    "Build Step" = $BuildStep
    Variant = $Variant
    Start = $BuildStopwatch.Elapsed
    "Elapsed Time" = [TimeSpan]::Zero
    CPU = 0
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
    [ScriptBlock] $ScriptBlock,
    [string] $SummaryVariant = ""
  )
  if (!$RecordTimings) {
    & $ScriptBlock
    return
  }

  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()
  $StartTimes = Get-ProcessorTimes
  $TimingEntry = Add-TimingData $Platform $BuildStep -Variant $SummaryVariant -Parent $script:CurrentOperation
  $script:CurrentOperation = $TimingEntry

  try {
    & $ScriptBlock
  } finally {
    $Stopwatch.Stop()
    $EndTimes = Get-ProcessorTimes
    $BusyTime = $EndTimes.Busy - $StartTimes.Busy
    $TotalTime = $BusyTime + $EndTimes.Idle - $StartTimes.Idle
    $TimingEntry."Elapsed Time" = $Stopwatch.Elapsed
    $TimingEntry.CPU = if ($TotalTime -gt 0) { $BusyTime / $TotalTime } else { 0 }
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
  # Time spent in the step itself rather than in its children (configuring,
  # installing, copying files, ...). Only meaningful for steps with children.
  $FormattedSelfTime = ""
  if ($Entry.Children) {
    $ChildrenTicks = ($Entry.Children | ForEach-Object { $_."Elapsed Time".Ticks } | Measure-Object -Sum).Sum
    $FormattedSelfTime = "{0:hh\:mm\:ss\.ff}" -f ($Entry."Elapsed Time" - [TimeSpan]::FromTicks($ChildrenTicks))
  }

  [PSCustomObject]@{
    "Build Step" = "$Indent$($Entry.'Build Step')"
    Variant = $Entry.Variant
    Platform = $Entry.Platform
    Arch = $Entry.Arch
    "Elapsed Time" = $FormattedTime
    Self = $FormattedSelfTime
    "%" = "$Percentage%"
    CPU = "$([math]::Round($Entry.CPU * 100))%"
  }

  $SortedChildren = $Entry.Children | Sort-Object -Descending -Property "Elapsed Time"
  foreach ($Child in $SortedChildren) {
    Flatten-TimingEntry $Child $TotalTime ($Depth + 1)
  }
}

function Write-Summary {
  Write-Host "Summary:" -ForegroundColor Cyan

  $TotalTime = $BuildStopwatch.Elapsed
  $EndTimes = Get-ProcessorTimes
  $BusyTime = $EndTimes.Busy - $BuildStartProcessorTimes.Busy
  $AllTime = $BusyTime + $EndTimes.Idle - $BuildStartProcessorTimes.Idle
  $TotalCPU = if ($AllTime -gt 0) { $BusyTime / $AllTime } else { 0 }

  $TrackedTime = [TimeSpan]::Zero
  $TrackedBusySeconds = 0
  $RootEntries = @($TimingData | Where-Object { -not $_.Parent })
  # The steps can run at the same time, so count the time during which any of
  # them runs.
  $TrackedEnd = [TimeSpan]::Zero
  foreach ($Entry in ($RootEntries | Sort-Object Start)) {
    $End = $Entry.Start + $Entry."Elapsed Time"
    if ($End -le $TrackedEnd) { continue }
    $Start = if ($Entry.Start -gt $TrackedEnd) { $Entry.Start } else { $TrackedEnd }
    $TrackedTime = $TrackedTime.Add($End - $Start)
    $TrackedBusySeconds += $Entry.CPU * ($End - $Start).TotalSeconds
    $TrackedEnd = $End
  }

  # Account for the time spent outside of any recorded build step.
  $UntrackedTime = $TotalTime - $TrackedTime
  if ($UntrackedTime -gt [TimeSpan]::Zero) {
    $UntrackedBusySeconds = $TotalCPU * $TotalTime.TotalSeconds - $TrackedBusySeconds
    $RootEntries += [PSCustomObject]@{
      Arch = ""
      Platform = ""
      "Build Step" = "(untracked)"
      Variant = ""
      "Elapsed Time" = $UntrackedTime
      CPU = [math]::Max(0, [math]::Min(1, $UntrackedBusySeconds / $UntrackedTime.TotalSeconds))
      Parent = $null
      Children = @()
    }
  }

  $RootEntries = $RootEntries | Sort-Object -Descending -Property "Elapsed Time"
  $Result = foreach ($RootEntry in $RootEntries) {
    Flatten-TimingEntry $RootEntry $TotalTime
  }

  $FormattedTotalTime = "{0:hh\:mm\:ss\.ff}" -f $TotalTime
  $TotalRow = [PSCustomObject]@{
    "Build Step" = "TOTAL"
    Variant = ""
    Platform = ""
    Arch = ""
    "Elapsed Time" = $FormattedTotalTime
    Self = ""
    "%" = "100.0%"
    CPU = "$([math]::Round($TotalCPU * 100))%"
  }

  @($Result) + $TotalRow | Format-Table -AutoSize
}


# Writes the timing data in the Chrome Trace Event format, which can be loaded
# in https://ui.perfetto.dev or chrome://tracing to view the build timeline.
# With -TrackPerStep, each top-level step shows in its own track, so that the
# steps that run at the same time do not overlap.
function Write-TimingTrace([string] $Path, [switch] $TrackPerStep) {
  function Get-TraceEvents([PSCustomObject] $Entry, [int] $Track) {
    [ordered]@{
      name = $Entry."Build Step"
      cat = "$($Entry.Platform) $($Entry.Arch)"
      ph = "X"
      # Timestamps are in microseconds, TimeSpan ticks are 100ns.
      ts = [long]($Entry.Start.Ticks / 10)
      dur = [long]($Entry."Elapsed Time".Ticks / 10)
      pid = 0
      tid = $Track
      args = [ordered]@{
        Platform = $Entry.Platform
        Arch = $Entry.Arch
        CPU = [math]::Round($Entry.CPU, 2)
      }
    }
    foreach ($Child in $Entry.Children) {
      Get-TraceEvents $Child $Track
    }
  }

  $Track = 0
  $Events = foreach ($Entry in $TimingData) {
    if ($Entry.Parent) { continue }
    if ($TrackPerStep) {
      $Track += 1
      [ordered]@{ name = "thread_name"; ph = "M"; pid = 0; tid = $Track; args = @{ name = $Entry."Build Step" } }
    }
    Get-TraceEvents $Entry $Track
  }

  New-Item -ItemType Directory (Split-Path -Path $Path -Parent) -ErrorAction Ignore | Out-Null
  $Json = ConvertTo-Json -Depth 4 -InputObject ([ordered]@{ traceEvents = @($Events) })
  [IO.File]::WriteAllText($Path, $Json)
  Write-Host "Build timeline written to $Path" -ForegroundColor Cyan
}

# Writes the timing data for Import-TimingData.
function Export-TimingData([string] $Path) {
  function ConvertTo-TimingRecord([PSCustomObject] $Entry) {
    [ordered]@{
      Name = $Entry."Build Step"
      Variant = $Entry.Variant
      Platform = $Entry.Platform
      Arch = $Entry.Arch
      Start = $Entry.Start.Ticks
      Elapsed = $Entry."Elapsed Time".Ticks
      CPU = $Entry.CPU
      Children = @(foreach ($Child in $Entry.Children) { ConvertTo-TimingRecord $Child })
    }
  }

  $Steps = @(foreach ($Entry in $TimingData) {
    if (-not $Entry.Parent) { ConvertTo-TimingRecord $Entry }
  })
  New-Item -ItemType Directory (Split-Path -Path $Path -Parent) -ErrorAction Ignore | Out-Null
  $Json = ConvertTo-Json -Depth 100 -InputObject ([ordered]@{ Start = $BuildStartTime.Ticks; Steps = $Steps })
  [IO.File]::WriteAllText($Path, $Json)
}

# Adds the steps that a build script wrote with Export-TimingData under $Parent.
function Import-TimingData([PSCustomObject] $Parent, [string] $Path) {
  if (-not $Parent -or -not (Test-Path $Path)) { return }
  $Data = [IO.File]::ReadAllText($Path) | ConvertFrom-Json
  Remove-Item $Path
  # The steps start relative to the start of the script.
  $Offset = [TimeSpan]::FromTicks($Data.Start - $BuildStartTime.Ticks)

  function Import-TimingRecord($Record, [PSCustomObject] $ParentEntry) {
    $Entry = [PSCustomObject]@{
      Arch = $Record.Arch
      Platform = $Record.Platform
      "Build Step" = $Record.Name
      Variant = $Record.Variant
      Start = $Offset + [TimeSpan]::FromTicks($Record.Start)
      "Elapsed Time" = [TimeSpan]::FromTicks($Record.Elapsed)
      CPU = $Record.CPU
      Parent = $ParentEntry
      Children = @()
    }
    $ParentEntry.Children += $Entry
    $TimingData.Add($Entry)
    foreach ($Child in $Record.Children) {
      Import-TimingRecord $Child $Entry
    }
  }

  foreach ($Record in $Data.Steps) {
    Import-TimingRecord $Record $Parent
  }
}

# BuildSequential.ps1 and BuildMulti.ps1 make the scripts that they run write
# their timings to this path instead of printing them, so that the summary and
# the timeline cover the whole build.
$TimingDataExportPath = $env:SWIFT_BUILD_TIMINGS_PATH
Remove-Item env:SWIFT_BUILD_TIMINGS_PATH -ErrorAction Ignore

# Prints the summary and writes the timeline of the build of a script, or
# writes its timings for the script that runs it.
function Write-BuildTimings([string] $Name) {
  if ($TimingDataExportPath) {
    if ($RecordTimings) {
      Export-TimingData $TimingDataExportPath
    }
    return
  }
  if ($Summary) {
    Write-Summary
  }
  if ($Trace) {
    Write-TimingTrace "$BinaryCache\build-timings-$Name.json"
  }
}

# Prints an error with its PowerShell and .NET stack traces.
function Write-BuildError([System.Management.Automation.ErrorRecord] $ErrorRecord) {
  function Write-ErrorLines($Text, $Indent = 0) {
    $IndentString = " " * $Indent
    $Text.Replace("`r", "") -split "`n" | ForEach-Object {
      Write-Host "$IndentString$_" -ForegroundColor Red
    }
  }

  Write-ErrorLines "Error: $ErrorRecord"
  Write-ErrorLines $ErrorRecord.ScriptStackTrace -Indent 4

  # Walk the .NET inner exception chain to print all messages and stack traces
  $Exception = $ErrorRecord.Exception
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
}

## The tools that SetupDependencies.ps1 downloaded.

function Get-AndroidNDK {
  return (Get-DependencyManifest).AndroidNDK
}

function Get-AndroidNDKPath {
  return (Get-AndroidNDK).Path
}

function Get-PythonPath([Hashtable] $Platform) {
  return (Get-DependencyManifest).Python.Roots.($Platform.Architecture.CMakeName)
}

function Get-EmbeddedPythonPath([Hashtable] $Platform) {
  return (Get-DependencyManifest).Python.EmbeddedRoots.($Platform.Architecture.CMakeName)
}

function Get-PythonExecutable {
  return (Get-DependencyManifest).Python.Executable
}

function Get-EmbeddedPythonInstallDir() {
  return [IO.Path]::Combine("$BuildRoot\", "Program Files", "Swift", "Python-$((Get-DependencyManifest).Python.Version)")
}

function Get-CMake {
  return (Get-DependencyManifest).CMake
}

function Get-Ninja {
  return (Get-DependencyManifest).Ninja
}

function Get-PinnedToolchainToolsDir() {
  return (Get-DependencyManifest).PinnedToolchain.ToolsDir
}

function Get-PinnedToolchainSDK([OS] $OS = $BuildPlatform.OS, [string] $Identifier = $OS.ToString()) {
  return [IO.Path]::Combine((Get-DependencyManifest).PinnedToolchain.PlatformsDir,
    "$($OS.ToString()).platform", "Developer", "SDKs", "$Identifier.sdk")
}

## The layout of the image and of the build tree.

function Get-InstallDir([Hashtable] $Platform) {
  if ($Platform -eq $HostPlatform) {
    return [IO.Path]::Combine("$BuildRoot\", "Program Files", "Swift")
  }
  if ($Platform -eq $KnownPlatforms["WindowsARM64"]) {
    return [IO.Path]::Combine("$BuildRoot\", "Program Files (Arm64)", "Swift")
  }
  if ($Platform -eq $KnownPlatforms["WindowsX64"]) {
    return [IO.Path]::Combine("$BuildRoot\", "Program Files (Amd64)", "Swift")
  }
  if ($Platform -eq $KnownPlatforms["WindowsX86"]) {
    return [IO.Path]::Combine("$BuildRoot\", "Program Files (x86)", "Swift")
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

  $SummaryVariant = $SplatArgs["SummaryVariant"]
  $SplatArgs.Remove("SummaryVariant")

  Record-OperationTime $Platform $Name -SummaryVariant $SummaryVariant {
    & $Name $Platform @SplatArgs
  }
}

enum Project {
  BuildTools
  RegsGen2

  EarlySwiftDriver
  EarlySwiftDriverSQLite
  CompilerXML2
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
  BoringSSL

  XCTest
  Testing
  ClangBuiltins
  ClangRuntime
  SwiftInspect
  DynamicCDispatch
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
    $SwiftCompiler = Join-Path (Get-PinnedToolchainToolsDir) "swiftc.exe"
    $TargetInfo = & $SwiftCompiler -target $Platform.Triple -print-target-info
    if ($LastExitCode -ne 0) {
      throw "Unable to print target info for '$($Platform.Triple)'"
    }
    $TargetInfo = $TargetInfo | ConvertFrom-JSON
    $Platform.Cache[$CacheKey] = $TargetInfo.target
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

# TODO(compnerd): replace this with Get-{Build,Host,Target}ProjectBinaryCache
function Get-CMarkBinaryCache([Hashtable] $Platform) {
  return "$BinaryCache\$($Platform.Triple)\cmark-gfm-0.29.0.gfm.13"
}

## Windows side-by-side assemblies.

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
                                     [string] $PublicKeyToken,
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
  <assemblyIdentity
      name="$AssemblyName"
      processorArchitecture="$ProcessorArchitecture"
      publicKeyToken="$PublicKeyToken"
      type="win32"
      version="$AssemblyVersion" />
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
                                                  [string] $ProcessorArchitecture,
                                                  [string] $PublicKeyToken) {
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
    ($AssemblyIdentity.GetAttribute("publicKeyToken") -ieq $PublicKeyToken) -and
    ($File.GetAttribute("name") -eq $FileName)
  )
}

function Ensure-WindowsAssemblyManifest([string] $ImagePath,
                                        [string] $AssemblyVersion,
                                        [string] $ProcessorArchitecture,
                                        [string] $PublicKeyToken,
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
          -ProcessorArchitecture  $ProcessorArchitecture `
          -PublicKeyToken         $PublicKeyToken
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
      -PublicKeyToken         $PublicKeyToken `
      -LogPrefix              $LogPrefix
  } finally {
    if (-not $KeepExistingManifest) {
      Remove-Item -Path $ExistingManifestPath -Force -ErrorAction SilentlyContinue
    }
  }
}

function New-WindowsManifestDependency([string] $Name,
                                       [string] $AssemblyVersion,
                                       [string] $ProcessorArchitecture,
                                       [string] $PublicKeyToken) {
  return [pscustomobject]@{
    Name                  = $Name
    Version               = $AssemblyVersion
    ProcessorArchitecture = $ProcessorArchitecture
    PublicKeyToken        = $PublicKeyToken
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
      $AsmId.SetAttribute("publicKeyToken", $Dependency.PublicKeyToken)
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
  return ([bool]$Doc.SelectSingleNode("//*[local-name()='trustInfo' or local-name()='requestedPrivileges' or local-name()='requestedExecutionLevel']"))
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

## Running programs.

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

  $WinSDKVersion = (Get-DependencyManifest).WinSDK.Version
  $CustomWinSDKRoot = (Get-DependencyManifest).WinSDK.Root

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

## Building CMake projects.

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

function ConvertTo-CMakeArgument([string] $Argument) {
  # Backslashes in CMakeCache.txt are escape characters, but the synthetic CAS
  # roots deliberately use a UNC spelling. Preserve the complete synthetic
  # path while normalizing ordinary Windows path separators. A synthetic path
  # may be the complete argument or the value of an option using `=`.
  $SyntheticPath = $Argument.IndexOf('\\swift\')
  if ($SyntheticPath -eq 0 -or
      ($SyntheticPath -gt 0 -and $Argument[$SyntheticPath - 1] -eq '=')) {
    $Prefix = $Argument.Substring(0, $SyntheticPath).Replace("\", "/")
    $Path = $Argument.Substring($SyntheticPath).Replace('$', '$$')
    return $Prefix + $Path
  }

  # Preserve a leading double backslash for ordinary UNC paths, while using
  # forward slashes for the remaining components as before. As above, the UNC
  # path may be the value of an option using `=`.
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
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline", "/Zc:preprocessor", "/source-charset:utf-8")
      DebugFlags        = { param([string] $Format)
        @()
      }
      AssumeFunctional  = $false
    }
    CXX = @{
      Executable        = "cl.exe"
      DriverStyle       = [DriverStyle]::CL
      Flags             = @("/GS-", "/Gw", "/Gy", "/Oy", "/Oi", "/Zc:inline", "/Zc:preprocessor", "/Zc:__cplusplus", "/source-charset:utf-8")
      DebugFlags        = { param([string] $Format)
        @()
      }
      AssumeFunctional  = $false
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
        @("-g", "-debug-info-format=${Format}")
      }
      AssumeFunctional  = $true
      SourceInfoMapping = $true
    }
  }
}

# The pinned toolchain is known once SetupDependencies.ps1 has run.
if ($Dependencies) {
  $Compilers.Pinned = @{
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
        @("-g", "-debug-info-format=${Format}")
      }
      AssumeFunctional  = $false
      SourceInfoMapping = $false
    }
  }
}

$Assemblers = @{
  MSVC = @{
    Executable        = { param([Hashtable] $Platform)
      if ($Platform.Architecture.VSName -eq "x86") { "ml.exe" } else { "ml64.exe" }
    }
    Dialect           = "ASM_MASM"
    Flags             = { param([Hashtable] $Platform)
      @("/nologo", "/quiet")
    }
    DebugFlags        = { param([string] $Format)
      @()
    }
    AssumeFunctional  = $true
  }

  Pinned = @{
    Executable        = { param([Hashtable] $Platform)
      Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath "clang-cl.exe"
    }
    Dialect           = "ASM"
    DriverStyle       = [DriverStyle]::ClangCL
    Flags             = { param([Hashtable] $Platform)
      @("--target=$($Platform.Triple)")
    }
    DebugFlags        = { param([string] $Format)
      if ($Format -eq "dwarf") { @("-clang:-gdwarf") } else { @("-clang:-gcodeview") }
    }
    AssumeFunctional  = $false
  }

  Stage1 = @{
    Executable        = { param([Hashtable] $Platform)
      [IO.Path]::Combine((Get-ProjectToolchainBin $BuildPlatform Stage1Compilers), "clang-cl.exe")
    }
    Dialect           = "ASM"
    DriverStyle       = [DriverStyle]::ClangCL
    Flags             = { param([Hashtable] $Platform)
      @("--target=$($Platform.Triple)")
    }
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
    [switch] $AddAndroidCMakeEnv = $false,
    [string] $SwiftSDK = $null,
    [hashtable] $Defines = @{}, # Values are either single strings or arrays of flags
    [string[]] $BuildTargets = @()
  )

  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Building '$Src' to '$Bin' ..."

  $Stopwatch = [Diagnostics.Stopwatch]::StartNew()

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
    $UseC = $CCompiler -ne $null
    $UseCXX = $CXXCompiler -ne $null
    $UseSwift = $SwiftCompiler -ne $null
    $PlatformDebugFormat = $Platform.DebugFormat

    function Add-LinkerFlagsDefine([hashtable]$Defines, [string[]]$Value) {
      $Value = $Value | ForEach-Object { "LINKER:$_" }
      Add-FlagsDefine $Defines CMAKE_EXE_LINKER_FLAGS $Value
      Add-FlagsDefine $Defines CMAKE_SHARED_LINKER_FLAGS $Value
    }

    function Add-SharedLinkerFlagsDefine([hashtable]$Defines, [string[]]$Value) {
      $Value = $Value | ForEach-Object { "LINKER:$_" }
      Add-FlagsDefine $Defines CMAKE_SHARED_LINKER_FLAGS $Value
      Add-FlagsDefine $Defines CMAKE_MODULE_LINKER_FLAGS $Value
    }

    # Add additional defines (unless already present)
    $Defines = $Defines.Clone()

    # CMake configuration.
    $env:CMAKE_ROOT = (Get-CMake).CMakeRoot
    $CMakeBin = (Get-CMake).Path
    # Enable CMP0181: Link command-line fragment variables are parsed and re-quoted.
    Add-KeyValueIfNew $Defines CMAKE_POLICY_DEFAULT_CMP0181 NEW
    # Enable CMP0214: Honor CMAKE_EXE_LINKER_FLAGS for Swift executable targets.
    Add-KeyValueIfNew $Defines CMAKE_POLICY_DEFAULT_CMP0214 NEW

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
          $ASMDialect = $Assembler.Dialect

          Add-KeyValueIfNew $Defines "CMAKE_${ASMDialect}_COMPILER" (& $Assembler.Executable $Platform)
          Add-KeyValueIfNew $Defines "CMAKE_${ASMDialect}_FLAGS" (& $Assembler.Flags $Platform)

          # CMake's assembler detection computes the MSVC-like frontend
          # correctly but does not cache CMAKE_<ASMDialect>_SIMULATE_ID and
          # CMAKE_<ASMDialect>_COMPILER_FRONTEND_VARIANT. On every re-configure,
          # the assembler is reloaded from the saved compiler file with both
          # fields empty, the Ninja generator then misidentifies it as GCC on
          # Windows and rewrites the include path with forward slashes,
          # resulting in a full rebuild.
          Add-KeyValueIfNew $Defines "CMAKE_${ASMDialect}_SIMULATE_ID" MSVC
          Add-KeyValueIfNew $Defines "CMAKE_${ASMDialect}_COMPILER_FRONTEND_VARIANT" MSVC

          if ($ASMDialect -eq "ASM") {
            Add-KeyValueIfNew $Defines CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDLL "/MD"

            if ($DebugInfo) {
              # CMake's MSVC_DEBUG_INFORMATION_FORMAT support also applies to ASM
              # targets, but clang-cl-as-ASM does not get a built-in mapping for
              # the Embedded format. Provide the mapping before setting the global
              # CMAKE_MSVC_DEBUG_INFORMATION_FORMAT below. MASM has no equivalent.
              Add-FlagsDefine $Defines CMAKE_ASM_COMPILE_OPTIONS_MSVC_DEBUG_INFORMATION_FORMAT_Embedded `
                $(& $Assembler.DebugFlags $PlatformDebugFormat)
            }
          }
        }

        if ($UseC) {
          Add-KeyValueIfNew $Defines CMAKE_C_COMPILER $CCompiler.Executable
          Add-KeyValueIfNew $Defines CMAKE_C_COMPILER_TARGET $Platform.Triple
          Add-FlagsDefine $Defines CMAKE_C_FLAGS $CCompiler.Flags

          if ($DebugInfo) {
            Add-FlagsDefine $Defines CMAKE_C_FLAGS $(& $CCompiler.DebugFlags $PlatformDebugFormat)
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
            Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $(& $CXXCompiler.DebugFlags $PlatformDebugFormat)
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
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $(& $SwiftCompiler.DebugFlags $PlatformDebugFormat)
          } else {
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-gnone")
          }

          # CMake 3.30+ passes all linker flags to Swift as the linker driver,
          # including those from the internal CMake modules files, without
          # a `-Xlinker` prefix. This causes build failures as Swift cannot
          # parse linker flags.
          # Overwrite the release linker flags to be empty to avoid this.
          Add-KeyValueIfNew $Defines CMAKE_EXE_LINKER_FLAGS_RELEASE ""
          Add-KeyValueIfNew $Defines CMAKE_SHARED_LINKER_FLAGS_RELEASE ""
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
        # Use the shared C++ runtime so that libraries like libswiftCore and
        # _FoundationICU don't carry their own copy.
        Add-KeyValueIfNew $Defines CMAKE_ANDROID_STL_TYPE c++_shared
        Add-KeyValueIfNew $Defines CMAKE_SYSROOT "$AndroidSysroot"

        if ($UseASM) {
        }

        if ($UseC) {
          Add-KeyValueIfNew $Defines CMAKE_C_COMPILER_TARGET $Platform.Triple
          Add-FlagsDefine $Defines CMAKE_C_FLAGS $CCompiler.Flags
          if ($DebugInfo) {
            Add-FlagsDefine $Defines CMAKE_C_FLAGS $(& $CCompiler.DebugFlags $PlatformDebugFormat)
          }
        }

        if ($UseCXX) {
          Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER_TARGET $Platform.Triple
          Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $CXXCompiler.Flags
          if ($DebugInfo) {
            Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $(& $CXXCompiler.DebugFlags $PlatformDebugFormat)
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
          if ($DebugInfo) {
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $(& $SwiftCompiler.DebugFlags $PlatformDebugFormat)
          } else {
            Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @("-gnone")
          }

          Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @(
            "-Xclang-linker", "-target", "-Xclang-linker", $Platform.Triple,
            "-Xclang-linker", "--sysroot", "-Xclang-linker", $AndroidSysroot,
            "-Xclang-linker", "-resource-dir", "-Xclang-linker", "${AndroidPrebuiltRoot}\lib\clang\$($(Get-AndroidNDK).ClangVersion)"
          )
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
            (& $Assembler.Executable $Platform)
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

    if ($UseSwift) {
      Add-FlagsDefine $Defines CMAKE_Swift_FLAGS @(
        "-module-cache-path", $ModuleCache
      )
    }

    if ($EnableCaching) {
      $env:LLVM_CACHE_CAS_PATH = "$ObjectStore"
      $SyntheticSourceCache = '\\swift\SourceCache$'
      $SyntheticBinaryCache = '\\swift\BinaryCache$'
      $CASPrefixMappings = [ordered]@{
        [IO.Path]::GetFullPath($SourceCache.FullName) = $SyntheticSourceCache;
        [IO.Path]::GetFullPath($BinaryCache.FullName) = $SyntheticBinaryCache;
      }
      $env:LLVM_CACHE_PREFIX_MAPS = ($CASPrefixMappings.GetEnumerator() | ForEach-Object {
        "$($_.Key)=$($_.Value)"
      }) -join ";"

      # Skip the clang-cache launcher when targeting Android: cmake auto-detects
      # the NDK's clang (e.g. 19.x) as the actual compiler, but the launcher
      # we'd point at lives under Stage1Compilers (a newer LLVM, e.g. 21.x).
      # The version skew makes the in-process dep scanner look for builtin
      # headers (`stddef.h`, `float.h`, ...) under the wrong resource-dir
      # layout and fail with "CAS-based dependency scan failed: failed to get
      # include-tree".  Compiler-rt builds for Android are fast enough without
      # the launcher.
      $LauncherSafe = ($Platform.OS -ne [OS]::Android)

      # The dependency scanner presents source inputs to Clang using the
      # synthetic paths from LLVM_CACHE_PREFIX_MAPS. This covers source paths
      # in debug info, coverage, and macros without putting a host-specific
      # `-ffile-prefix-map=<physical>=<synthetic>` in the cached command. Set
      # the remaining compilation directory explicitly, and omit the CodeView
      # command line because it contains the physical scanner replay mappings.
      $ClangCachingFlags = @("-ffile-compilation-dir=$SyntheticBinaryCache")
      if ($DebugInfo) {
        $ClangCachingFlags += "-gno-codeview-command-line"
      }

      if ($LauncherSafe -and $UseC -and $CCompiler.DriverStyle -ne [DriverStyle]::CL) {
        Add-FlagsDefine $Defines CMAKE_C_FLAGS $ClangCachingFlags
        Add-KeyValueIfNew $Defines CMAKE_C_COMPILER_LAUNCHER `
            (Join-Path -Path (Split-Path $CCompiler.Executable) -ChildPath "clang-cache.exe")
      }

      if ($LauncherSafe -and $UseCXX -and $CXXCompiler.DriverStyle -ne [DriverStyle]::CL) {
        Add-FlagsDefine $Defines CMAKE_CXX_FLAGS $ClangCachingFlags
        Add-KeyValueIfNew $Defines CMAKE_CXX_COMPILER_LAUNCHER `
            (Join-Path -Path (Split-Path $CXXCompiler.Executable) -ChildPath "clang-cache.exe")
      }

      if ($UseSwift) {
        $SwiftCachingFlags = @(
          "-explicit-module-build",
          "-cache-compile-job",
          "-cas-path", $ObjectStore,
          "-incremental-dependency-scan",
          "-file-compilation-dir", $SyntheticBinaryCache
        )

        if ($SwiftCompiler.SourceInfoMapping) {
          $SwiftCachingFlags += @("-Xfrontend", "-prefix-map-sourceinfo")
        }

        foreach ($Mapping in $CASPrefixMappings.GetEnumerator()) {
          $SwiftCachingFlags += @(
            "-scanner-prefix-map-paths", $Mapping.Key, $Mapping.Value
          )
        }

        Add-FlagsDefine $Defines CMAKE_Swift_FLAGS $SwiftCachingFlags
      }
    }

    if ($InstallTo) {
      Add-KeyValueIfNew $Defines CMAKE_INSTALL_PREFIX $InstallTo
    }

    Add-KeyValueIfNew $Defines CMAKE_MAKE_PROGRAM "$((Get-Ninja).Path)"

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
        $Value = ConvertTo-CMakeArgument $Define.Value
      } else {
        # Flags array, multiple tokens, quoting needed for tokens containing spaces
        $Value = ""
        foreach ($Arg in $Define.Value) {
          if ($Value.Length -gt 0) {
            $Value += " "
          }

          $ArgWithForwardSlashes = ConvertTo-CMakeArgument $Arg
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

    Write-Host "$CMakeBin $cmakeGenerateArgs"
    Invoke-Program $CMakeBin @cmakeGenerateArgs

    # Build all requested targets in one invocation. Test targets such as
    # ExperimentalTest do not depend on the default target, so build it first.
    if ($BuildTargets -contains "default") {
      Invoke-Program $CMakeBin --build $Bin
    }
    $Targets = @($BuildTargets | Where-Object { $_ -ne "default" })
    if ($Targets.Length -gt 0) {
      Invoke-Program $CMakeBin --build $Bin --target @Targets
    }

    if ($BuildTargets.Length -eq 0 -and $InstallTo) {
      Invoke-Program $CMakeBin --build $Bin --target install
    }
  }

  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Finished building '$Src' to '$Bin' in $($Stopwatch.Elapsed)"
  Write-Host ""
}

## Running the build scripts.

# The build scripts run in the PowerShell that runs this one.
$PowerShell = [Diagnostics.Process]::GetCurrentProcess().MainModule.FileName

# Returns the values of the parameters of the script at $Path, which is running.
function Get-ScriptParameters([string] $Path) {
  $Parameters = @{}
  foreach ($Name in (Get-Command $Path).Parameters.Keys) {
    if ([System.Management.Automation.PSCmdlet]::CommonParameters.Contains($Name)) { continue }
    $Parameters[$Name] = Get-Variable -Name $Name -ValueOnly
  }
  return $Parameters
}

# Returns the arguments that pass to a build script the values in $Parameters of
# the parameters that it takes, after applying $Overrides. Switches are passed
# only when they are set, and arrays as comma-separated lists.
function Get-BuildScriptArguments([string] $Script, [Hashtable] $Parameters, [Hashtable] $Overrides = @{}) {
  $ScriptParameters = (Get-Command "$PSScriptRoot\$Script").Parameters
  $Values = $Parameters.Clone()
  foreach ($Override in $Overrides.GetEnumerator()) {
    if (-not $ScriptParameters.ContainsKey($Override.Key)) {
      throw "$Script does not take -$($Override.Key)"
    }
    $Values[$Override.Key] = $Override.Value
  }

  foreach ($Parameter in ($Values.GetEnumerator() | Sort-Object Name)) {
    if (-not $ScriptParameters.ContainsKey($Parameter.Key)) { continue }
    $Value = $Parameter.Value
    if ($ScriptParameters[$Parameter.Key].SwitchParameter) {
      if ($Value) { "-$($Parameter.Key)" }
    } elseif ($Value -is [array]) {
      if ($Value.Count -gt 0) { "-$($Parameter.Key)"; $Value -join "," }
    } elseif ("$Value" -ne "") {
      "-$($Parameter.Key)"; "$Value"
    }
  }
}

# Runs one of the build scripts in its own process, with the timings of its
# steps under this step.
function Invoke-BuildScript([string] $Name, [string] $Script, [Hashtable] $Parameters, [Hashtable] $Overrides = @{}) {
  $Arguments = @(Get-BuildScriptArguments $Script $Parameters $Overrides)
  Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] $Script $Arguments"
  Record-OperationTime $BuildPlatform $Name {
    $ScriptTimingDataPath = "$BinaryCache\BuildTimings\$Name.json"
    Remove-Item $ScriptTimingDataPath -ErrorAction Ignore
    Invoke-IsolatingEnvVars {
      $env:SWIFT_BUILD_TIMINGS_PATH = $ScriptTimingDataPath
      & $PowerShell -NoProfile -ExecutionPolicy RemoteSigned -File "$PSScriptRoot\$Script" @Arguments
    }
    $ExitCode = $LastExitCode
    Import-TimingData $script:CurrentOperation $ScriptTimingDataPath
    if ($ExitCode -ne 0) {
      throw "$Script exited with code $ExitCode"
    }
  }
}

# Removes the build trees of the given platforms, and the image of the host
# toolchain.
function Clear-BuildTree([Hashtable[]] $Platforms) {
  # Fail instead of leaving a partially-cleaned tree behind.
  function Clean-Path([string] $Path) {
    if (-not (Test-Path -LiteralPath $Path)) { return }
    try {
      [System.IO.Directory]::Delete("\\?\$Path", $true)
    } catch {
      throw "Failed to clean '$Path': $_"
    }
  }

  foreach ($Platform in (@($BuildPlatform, $HostPlatform) + $Platforms)) {
    Clean-Path "$BinaryCache\$($Platform.Triple)"
  }
  Clean-Path "$BinaryCache\1"
  Clean-Path "$BinaryCache\5"
  Clean-Path (Get-InstallDir $HostPlatform)
}

# Removes the SDKs and the Windows runtimes from the image, so that the SDKs
# that a toolchain is built against are installed from scratch.
function Remove-InstalledSDKs([Hashtable[]] $WindowsPlatforms) {
  Remove-Item -Force -Recurse ([IO.Path]::Combine((Get-InstallDir $HostPlatform), "Platforms")) -ErrorAction Ignore
  if ($HostPlatform.OS -eq [OS]::Windows) {
    (@($HostPlatform) + @($WindowsPlatforms)) | ForEach-Object {
      Remove-Item -Force -Recurse (Get-WindowsRuntimeInstallRoot $_) -ErrorAction Ignore
    }
  }
}
