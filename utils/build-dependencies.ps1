# Copyright 2026 Apple Inc. and the Swift project authors
#
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

<#
.SYNOPSIS
Downloads the tools needed to build and test the Swift toolchain on Windows.

.DESCRIPTION
Installs every dependency that build.ps1 may need into the artifact cache,
regardless of which components are going to be built or tested.

Each dependency is probed first (for example with `--version`). Only a
dependency that is missing or fails its probe is (re)installed, and it must
pass the probe afterwards.

The default values of the parameters must match the ones of build.ps1, which
passes its own values when it runs this script.

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

.PARAMETER SyftVersion
The version of syft to install.

.PARAMETER CMakeVersion
The version of CMake to install.

.PARAMETER DownloadRetryCount
The number of attempts to make when downloading a dependency before giving up.
Default: 3

.PARAMETER ArtifactLockTimeoutSeconds
The maximum time to wait for another process to release an artifact lock.
Default: 1800

.PARAMETER PythonVersion
The version of Python to install.

.PARAMETER HostArchName
The architecture where the toolchain will execute. Automatically detected from
system.  Valid values: AMD64, ARM64

.PARAMETER AndroidNDKVersion
The version number of the Android NDK to install. The Android NDK is not
installed when it is empty.
Default: ""

.EXAMPLE
PS> .\build-dependencies.ps1 -ArtifactCache C:\ArtifactCache
#>
[CmdletBinding(PositionalBinding = $false)]
param
(
  [System.IO.FileInfo] $ArtifactCache = "S:\ArtifactCache",

  [string] $PinnedBuild = "",
  [ValidatePattern("^([A-Fa-f0-9]{64}|)$")]
  [string] $PinnedSHA256 = "",
  [string] $PinnedVersion = "",

  [string] $SyftVersion = "1.40.0",
  [string] $CMakeVersion = "4.4.1",

  [ValidateRange(1, [int]::MaxValue)]
  [int] $DownloadRetryCount = 3,
  [ValidateRange(1, [int]::MaxValue)]
  [int] $ArtifactLockTimeoutSeconds = 1800,

  [ValidatePattern('^\d+(\.\d+)*$')]
  [string] $PythonVersion = "3.10.1",

  [ValidateSet("AMD64", "ARM64")]
  [string] $HostArchName = $(if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }),

  [ValidatePattern("^(r(?:[1-9]|[1-9][0-9])(?:[a-z])?(-beta[1-9])?|)$")]
  [string] $AndroidNDKVersion = ""
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version 3.0

. "$PSScriptRoot\build-dependencies-common.ps1"

$BuildArchName = if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }

if (($PinnedBuild -or $PinnedSHA256 -or $PinnedVersion) -and -not ($PinnedBuild -and $PinnedSHA256 -and $PinnedVersion)) {
  throw "If any of PinnedBuild, PinnedSHA256, or PinnedVersion is set, all three must be set."
}

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

$PythonExecutable = "$ArtifactCache\Python$BuildArchName-$PythonVersion\tools\python.exe"

function Write-Success([string] $Description) {
  $HeavyCheckMark = @{
    Object = [Char]0x2714
    ForegroundColor = 'DarkGreen'
    NoNewLine = $true
  }
  Write-Host @HeavyCheckMark
  Write-Host " $Description"
}

$WebClient = New-Object Net.WebClient

function DownloadAndVerify($URL, $Destination, $Hash) {
  if (Test-Path $Destination) {
    # Only reached when a dependency failed its probe, so the archive may
    # be the cause.
    if ((Get-FileHash -Path $Destination -Algorithm SHA256).Hash -eq $Hash) { return }
    Write-Warning "Removing '$Destination': SHA256 mismatch"
    Remove-Item -LiteralPath $Destination -Force
  }

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

function Test-Executable([string] $Executable, [string[]] $Arguments = @("--version")) {
  if (-not (Test-Path -LiteralPath $Executable -PathType Leaf)) { return $false }
  # Some tools print their version to stderr.
  $ErrorActionPreference = "Continue"
  try {
    & $Executable @Arguments 2>&1 | Out-Null
    return $LastExitCode -eq 0
  } catch {
    return $false
  }
}

# Runs $Probe and, if it fails, replaces whatever is at $InstallRoot by
# running $Install.
function Install-Dependency([string] $Description, [string] $InstallRoot,
                            [ScriptBlock] $Probe, [ScriptBlock] $Install) {
  if (-not (& $Probe)) {
    if (Test-Path -LiteralPath $InstallRoot) {
      Write-Warning "$Description in '$InstallRoot' is not working, reinstalling it."
      Remove-Item -LiteralPath $InstallRoot -Recurse -Force
    }
    & $Install
    if (-not (& $Probe)) {
      throw "$Description in '$InstallRoot' is not working after installing it."
    }
  }
  Write-Success $Description
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

function Install-Python([string] $ArchName, [bool] $EmbeddedPython = $false) {
  if (-not $KnownPythons.ContainsKey($PythonVersion)) {
    throw "Unknown python version: $PythonVersion"
  }
  $Python = $KnownPythons[$PythonVersion][$(if ($EmbeddedPython) { "${ArchName}_Embedded" } else { $ArchName })]
  $FileName = $(if ($EmbeddedPython) { "EmbeddedPython$ArchName-$PythonVersion" } else { "Python$ArchName-$PythonVersion" })
  $Executable = if ($EmbeddedPython) {
    "$ArtifactCache\$FileName\python.exe"
  } else {
    "$ArtifactCache\$FileName\tools\python.exe"
  }
  $Description = $(if ($EmbeddedPython) { "$ArchName embedded Python $PythonVersion" } else { "$ArchName Python $PythonVersion" })
  Install-Dependency $Description "$ArtifactCache\$FileName" {
    # The build machine cannot run the host's Python when cross compiling.
    if ($ArchName -eq $BuildArchName) {
      Test-Executable $Executable
    } else {
      Test-Path -LiteralPath $Executable -PathType Leaf
    }
  } {
    DownloadAndVerify $Python.URL "$ArtifactCache\$FileName.zip" $Python.SHA256
    Expand-ArtifactZip "$FileName.zip" $FileName
  }
}

function Test-PythonModuleInstalled([string] $ModuleName) {
  # Also check the dependencies so that caches populated before one was
  # pinned get repaired.
  $Modules = @($ModuleName) + $PythonModules[$ModuleName].Dependencies
  try {
    Invoke-Program -Silent $PythonExecutable -c "import importlib.util, sys; sys.exit(0 if all(importlib.util.find_spec(m) for m in sys.argv[1:]) else 1)" @Modules
    return $true
  } catch {
    return $false
  }
}

function Install-PythonModule([string] $ModuleName) {
  if (-not (Test-PythonModuleInstalled $ModuleName)) {
    $TempRequirementsTxt = New-TemporaryFile

    $Module = $PythonModules[$ModuleName]
    "$ModuleName==$($Module.Version) --hash=`"sha256:$($Module.SHA256[$BuildArchName])`"" | Out-File -FilePath $TempRequirementsTxt -Append -Encoding utf8
    foreach ($Dependency in $Module.Dependencies) {
      $DependencyModule = $PythonModules[$Dependency]
      "$Dependency==$($DependencyModule.Version) --hash=`"sha256:$($DependencyModule.SHA256[$BuildArchName])`"" | Out-File -FilePath $TempRequirementsTxt -Append -Encoding utf8
    }

    # Dependencies are pinned above; --require-hashes rejects anything else
    # pip would resolve on its own. Without --force-reinstall, pip skips a
    # module whose files are missing as long as its metadata is still there.
    Invoke-Program -OutNull $PythonExecutable '-I' -m pip install -r $TempRequirementsTxt --require-hashes --force-reinstall --disable-pip-version-check
    if (-not (Test-PythonModuleInstalled $ModuleName)) {
      throw "$ModuleName is not working after installing it."
    }
  }
  Write-Success "$ModuleName"
}

$Stopwatch = [Diagnostics.Stopwatch]::StartNew()
Write-Host "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies ..." -ForegroundColor Cyan
$ProgressPreference = "SilentlyContinue"

foreach ($ArchName in @($HostArchName, $BuildArchName) | Select-Object -Unique) {
  Install-Python $ArchName
  Install-Python $ArchName $true
}

Invoke-WithArtifactLock "Python$BuildArchName-$PythonVersion" {
  try {
    Invoke-Program -Silent $PythonExecutable -m pip
  } catch {
    Invoke-Program -OutNull $PythonExecutable '-I' -m ensurepip -U --default-pip
  }
  Write-Success "pip"

  Install-PythonModule "packaging"    # For building LLVM 18+
  Install-PythonModule "setuptools"   # Required for SWIG support
  Install-PythonModule "psutil"       # Required for testing LLDB
  Install-PythonModule "cryptography" # Required for testing LLDB
}

# WiX is needed both for packaging and for extracting the pinned toolchain
# installer that bootstraps toolchain builds.
$DotNetRuntimeInfo = Get-DotNetRuntime
$DotNetArtifact = "dotnet-runtime-$($DotNetRuntime.Version)-$($DotNetRuntimeInfo.RuntimeIdentifier)"
Install-Dependency ".NET Runtime $($DotNetRuntime.Version) ($($DotNetRuntimeInfo.RuntimeIdentifier))" (Get-DotNetRuntimeRoot) {
  Test-Executable (Get-DotNet) @("--list-runtimes")
} {
  DownloadAndVerify $DotNetRuntimeInfo.URL "$ArtifactCache\$DotNetArtifact.zip" $DotNetRuntimeInfo.SHA256
  Expand-ArtifactZip "$DotNetArtifact.zip" $DotNetArtifact
}

Install-Dependency "WiX $($WiX.Version)" "$ArtifactCache\WiX-$($WiX.Version)" {
  Invoke-WithDotNetRuntime {
    Test-Executable (Get-DotNet) @("$($WiX.Path)\wix.dll", "--version")
  }
} {
  DownloadAndVerify $WiX.URL "$ArtifactCache\WiX-$($WiX.Version).zip" $WiX.SHA256
  Expand-ArtifactZip "WiX-$($WiX.Version).zip" "WiX-$($WiX.Version)"
}

$ToolchainArtifact = "$ToolchainVersionIdentifier-$($BuildArchName.ToLowerInvariant())"
Invoke-WithArtifactLock "SwiftToolchainExtraction" {
  Install-Dependency "Swift Toolchain $PinnedVersion" "$ArtifactCache\toolchains\$ToolchainArtifact" {
    Test-Executable (Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath "swiftc.exe")
  } {
    DownloadAndVerify $PinnedBuild "$ArtifactCache\$PinnedToolchain.exe" $PinnedSHA256
    Extract-Toolchain "$PinnedToolchain.exe" -ToolchainName $ToolchainArtifact
  }
}

$CMake = Get-CMake
Install-Dependency "CMake $CMakeVersion" "$ArtifactCache\$($CMake.Artifact)" {
  Test-Executable $CMake.Path
} {
  DownloadAndVerify $CMake.URL "$ArtifactCache\$($CMake.FileName)" $CMake.SHA256
  Expand-ArtifactZip $CMake.FileName $CMake.Artifact
}

$Syft = Get-Syft
Install-Dependency "syft $SyftVersion" "$ArtifactCache\$($Syft.Artifact)" {
  Test-Executable $Syft.Path @("version")
} {
  DownloadAndVerify $Syft.URL "$ArtifactCache\$($Syft.Artifact).zip" $Syft.SHA256
  Expand-ArtifactZip "$($Syft.Artifact).zip" $Syft.Artifact
}

# The make tool isn't part of MSYS
$GnuWin32MakeURL = "https://downloads.sourceforge.net/project/ezwinports/make-4.4.1-without-guile-w32-bin.zip"
$GnuWin32MakeHash = "fb66a02b530f7466f6222ce53c0b602c5288e601547a034e4156a512dd895ee7"
Install-Dependency "GNUWin32 make 4.4.1" "$ArtifactCache\GnuWin32Make-4.4.1" {
  Test-Executable "$ArtifactCache\GnuWin32Make-4.4.1\bin\make.exe"
} {
  DownloadAndVerify $GnuWin32MakeURL "$ArtifactCache\GnuWin32Make-4.4.1.zip" $GnuWin32MakeHash
  Expand-ArtifactZip GnuWin32Make-4.4.1.zip GnuWin32Make-4.4.1
}

if ($AndroidNDKVersion) {
  $NDK = Get-AndroidNDK
  Install-Dependency "Android NDK $AndroidNDKVersion" (Get-AndroidNDKPath) {
    Test-Executable "$(Get-AndroidNDKPath)\toolchains\llvm\prebuilt\windows-x86_64\bin\clang.exe"
  } {
    DownloadAndVerify $NDK.URL "$ArtifactCache\android-ndk-$AndroidNDKVersion-windows.zip" $NDK.SHA256
    Expand-ArtifactZip "android-ndk-$AndroidNDKVersion-windows.zip" "android-ndk-$AndroidNDKVersion" "android-ndk-$AndroidNDKVersion"
  }
}

$FlexBisonArtifact = "win_flex_bison-$($WinFlexBison.Version)"
Install-Dependency "flex/bison $($WinFlexBison.Version)" "$ArtifactCache\$FlexBisonArtifact" {
  (Test-Executable (Get-FlexExecutable)) -and (Test-Executable (Get-BisonExecutable))
} {
  DownloadAndVerify $WinFlexBison.URL "$ArtifactCache\$FlexBisonArtifact.zip" $WinFlexBison.SHA256
  Expand-ArtifactZip "$FlexBisonArtifact.zip" $FlexBisonArtifact
}

Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies took $($Stopwatch.Elapsed)"
Write-Host ""
