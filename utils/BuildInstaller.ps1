# Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
# Copyright 2023 Tristan Labelle <tristan@thebrowser.company>

<#
.SYNOPSIS
Builds the installer of the Swift toolchain.

.DESCRIPTION
Patches the toolchain in the image with mimalloc, then builds the MSIs and the
installer bundle from the image that BuildToolchain.ps1 and BuildSDK.ps1
populated. Optionally generates the Software Bill of Materials, and copies the
artifacts to a staging directory for CI.

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

.PARAMETER Stage
The path to a directory where built msi's and the installer executable should be
staged (for CI). Leave empty for local development builds.

.PARAMETER ProductVersion
The product version to be used when building the installer. Supports semantic
version strings (e.g., "1.0.0"). Default: "0.0.0"

.PARAMETER ToolchainIdentifier
The toolchain version identifier for the toolchain being built.
Default: Uses TOOLCHAIN_VERSION environment variable or "$USERNAME.development"

.PARAMETER HostArchName
The architecture where the toolchain will execute. Valid values: AMD64, ARM64

.PARAMETER IncludeNoAsserts
Include the no-assert toolchain variant.

.PARAMETER WindowsSDKArchitectures
The architectures of the Windows SDK to include.

.PARAMETER AndroidSDKArchitectures
The architectures of the Android SDK to include.

.PARAMETER IncludeSBoM
Include Software Bill of Materials generation using syft. Used for compliance
tracking.

.PARAMETER DebugInfo
Include debug information in the builds.

.PARAMETER Summary
Display a build time summary at the end of the build.

.PARAMETER Trace
Write the build timings to build-timings-<name>.json in the binary cache.

.EXAMPLE
PS> .\BuildInstaller.ps1 -WindowsSDKArchitectures X64,X86,Arm64 -Stage S:\Stage
#>
[CmdletBinding(PositionalBinding = $false)]
param
(
  [System.IO.FileInfo] $SourceCache = "S:\SourceCache",
  [System.IO.FileInfo] $BinaryCache = "S:\BinaryCache",
  [Alias("ImageRoot")]
  [System.IO.FileInfo] $BuildRoot = "S:",
  [string] $Stage = "",

  [string] $ProductVersion = "0.0.0",
  [string] $ToolchainIdentifier = $(if ($env:TOOLCHAIN_VERSION) { $env:TOOLCHAIN_VERSION } else { "$env:USERNAME.development" }),
  [ValidateSet("AMD64", "ARM64")]
  [string] $HostArchName = $(if ($env:PROCESSOR_ARCHITEW6432) { $env:PROCESSOR_ARCHITEW6432 } else { $env:PROCESSOR_ARCHITECTURE }),
  [switch] $IncludeNoAsserts = $false,
  [string[]] $WindowsSDKArchitectures = @(),
  [string[]] $AndroidSDKArchitectures = @(),
  [switch] $IncludeSBoM = $false,

  [switch] $DebugInfo,
  [switch] $Summary,
  [switch] $Trace
)

# mimalloc builds with MSVC, which does not use the LLVM CAS.
$EnableCaching = $false

. "$PSScriptRoot\BuildSupport.ps1"

# Work around limitations of cmd passing in array arguments via powershell.exe -File
if ($AndroidSDKArchitectures.Length -eq 1) { $AndroidSDKArchitectures = $AndroidSDKArchitectures[0].Split(",") }

if ($WindowsSDKArchitectures.Length -eq 1) { $WindowsSDKArchitectures = $WindowsSDKArchitectures[0].Split(",") }

$AndroidSDKBuilds = @($AndroidSDKArchitectures | ForEach-Object {
  switch ($_) {
    "aarch64" { $KnownPlatforms["AndroidARM64"] }
    "armv7" { $KnownPlatforms["AndroidARMv7"] }
    "i686" { $KnownPlatforms["AndroidX86"] }
    "x86_64" { $KnownPlatforms["AndroidX64"] }
    default { throw "No Android platform for architecture $_" }
  }
})

$WindowsSDKBuilds = @($WindowsSDKArchitectures | ForEach-Object {
  switch ($_) {
    "X64" { $KnownPlatforms["WindowsX64"] }
    "X86" { $KnownPlatforms["WindowsX86"] }
    "Arm64" { $KnownPlatforms["WindowsArm64"] }
    default { throw "No Windows platform for architecture $_" }
  }
})

$PythonVersion = (Get-DependencyManifest).Python.Version
$WiX = (Get-DependencyManifest).WiX
$msbuild = "$VSInstallRoot\MSBuild\Current\Bin\$BuildArchName\MSBuild.exe"

function Get-DotNet() {
  return (Get-DependencyManifest).DotNet.Path
}

function Invoke-WithDotNetRuntime([scriptblock] $Body) {
  Invoke-IsolatingEnvVars {
    $env:DOTNET_ROOT = (Get-DependencyManifest).DotNet.Root
    $env:DOTNET_HOST_PATH = Get-DotNet
    $env:DOTNET_MULTILEVEL_LOOKUP = "0"

    & $Body
  }
}

function Build-WiXProject() {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position = 0, Mandatory = $true)]
    [string]$FileName,
    [Parameter(Mandatory = $true)]
    [hashtable]$Platform,
    [switch]$Bundle,
    [hashtable]$Properties = @{}
  )

  $ProductVersionArg = $ProductVersion
  if (-not $Bundle) {
    # WiX accepts a semantic version string for Bundles, but Packages still
    # require a purely numerical version number,
    # so trim any semantic versioning suffixes
    $ProductVersionArg = [regex]::Replace($ProductVersion, "[-+].*", "")
  }

  $Properties = $Properties.Clone()
  Add-KeyValueIfNew $Properties Configuration Release
  Add-KeyValueIfNew $Properties BaseOutputPath "$BinaryCache\$($Platform.Triple)\installer\"
  Add-KeyValueIfNew $Properties ProductArchitecture $Platform.Architecture.VSName
  Add-KeyValueIfNew $Properties ProductVersion $ProductVersionArg
  Add-KeyValueIfNew $Properties AcceptEula $WiX.EulaIdentifier

  $MSBuildArgs = @( "-noLogo", "-maxCpuCount", "-restore", "$SourceCache\swift-installer-scripts\platforms\Windows\$FileName" )
  foreach ($Property in $Properties.GetEnumerator()) {
    if ($Property.Value -is [string] -and $Property.Value.Contains(" ")) {
      $MSBuildArgs += "-p:$($Property.Key)=$($Property.Value.Replace('\', '\\'))"
    } else {
      $MSBuildArgs += "-p:$($Property.Key)=$($Property.Value)"
    }
  }
  $MSBuildArgs += "-binaryLogger:$BinaryCache\$($Platform.Triple)\msi\$($Platform.Architecture.VSName)-$([System.IO.Path]::GetFileNameWithoutExtension($FileName)).binlog"
  $MSBuildArgs += "-detailedSummary:False"

  Invoke-WithDotNetRuntime {
    Write-Host "$msbuild $MSBuildArgs"
    Invoke-Program $msbuild @MSBuildArgs
  }
}

# Reference: https://github.com/microsoft/mimalloc/tree/dev/bin#minject
function Build-mimalloc() {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position = 0, Mandatory = $true)]
    [hashtable]$Platform
  )

  $MimallocBinaryCache = "$BinaryCache\$($Platform.Triple)\mimalloc"
  # Match the architecture baselines in mimalloc's Visual Studio projects.
  # MI_OPT_ARCH may select a newer ARM architecture as mimalloc evolves.
  $MimallocArchitectureFlag = switch ($Platform.Architecture.ShortName) {
    "x64" { "/arch:AVX2" }
    "arm64" { "/arch:armv8.2" }
    default { throw "Unsupported mimalloc architecture '$($Platform.Architecture.ShortName)'" }
  }

  $ToolchainRoots = @($Platform.ToolchainInstallRoot)
  if ($IncludeNoAsserts) {
    $ToolchainRoots += $Platform.NoAssertsToolchainInstallRoot
  }

  foreach ($ToolchainRoot in $ToolchainRoots) {
    Build-CMakeProject `
      -Src "$SourceCache\mimalloc" `
      -Bin $MimallocBinaryCache `
      -InstallTo "$ToolchainRoot\usr" `
      -Platform $Platform `
      -CCompiler $Compilers.MSVC.C `
      -CXXCompiler $Compilers.MSVC.CXX `
      -Defines @{
        CMAKE_CXX_FLAGS = @($MimallocArchitectureFlag);
        MI_BUILD_OBJECT = "NO";
        MI_BUILD_SHARED = "YES";
        MI_BUILD_STATIC = "NO";
        MI_BUILD_TESTS = "NO";
        MI_NO_OPT_ARCH = "YES";
      }
  }
}

function Patch-mimalloc() {
  [CmdletBinding(PositionalBinding = $false)]
  param
  (
    [Parameter(Position = 0, Mandatory = $true)]
    [hashtable]$Platform
  )

  $BuildSuffix = if ($BuildPlatform -eq $KnownPlatforms["WindowsX64"]) { "" } else { "-arm64" }

  $Tools = @(
    "swift.exe",
    "swiftc.exe",
    "swift-api-digester.exe",
    "swift-autolink-extract.exe",
    "swift-driver.exe",
    "swift-frontend.exe",
    "swift-symbolgraph-extract.exe",
    "swift-synthesize-interface.exe",
    "clang.exe",
    "clang++.exe",
    "clang-cache.exe",
    "clang-cl.exe",
    "clang-cpp.exe",
    "lld.exe",
    "lld-link.exe",
    "ld.lld.exe",
    "ld64.lld.exe",
    "wasm-ld.exe"
  )
  $Binaries = $Tools | ForEach-Object {[IO.Path]::Combine($Platform.ToolchainInstallRoot, "usr", "bin", $_)}
  if ($IncludeNoAsserts) {
    $NoAssertBinaries = $Tools | ForEach-Object {[IO.Path]::Combine($Platform.NoAssertsToolchainInstallRoot, "usr", "bin", $_)}
    $Binaries = $Binaries + $NoAssertBinaries
  }

  $minject = "$SourceCache\mimalloc\bin\minject$BuildSuffix"
  # minject creates an intermediate path by extending the input filename.
  # Stage the input under a short name to avoid its pathname-length bug.
  $PatchBinary = [IO.Path]::Combine([IO.Path]::GetDirectoryName($minject), "binary.exe")

  try {
    foreach ($Binary in $Binaries) {
      $Name = [IO.Path]::GetFileName($Binary)

      Copy-Item -Force -LiteralPath $Binary -Destination $PatchBinary
      Invoke-Program $minject "-f" "-i" "$PatchBinary"
      Copy-Item -Force -LiteralPath $PatchBinary -Destination $Binary

      # Log the import table
      $LogFile = "$BinaryCache\$($Platform.Triple)\mimalloc\minject-log-$Name.txt"
      $ErrorFile = "$BinaryCache\$($Platform.Triple)\mimalloc\minject-log-$Name-error.txt"
      Invoke-Program "$minject" "-l" "$Binary" -OutFile $LogFile -ErrorFile $ErrorFile

      # Verify patching
      $Found = Select-String -Path $LogFile -Pattern "mimalloc"
      if (-not $Found) {
        Get-Content $ErrorFile
        throw "Failed to patch mimalloc for $Name"
      }
    }
  } finally {
    if (Test-Path -LiteralPath $PatchBinary) {
      Remove-Item -Force -LiteralPath $PatchBinary
    }
  }
}

function Build-Installer([Hashtable] $Platform) {
  # TODO(hjyamauchi) Re-enable the swift-inspect and swift-docc builds
  # when cross-compiling https://github.com/apple/swift/issues/71655
  $INCLUDE_SWIFT_DOCC = if ($IsCrossCompiling) { "False" } else { "True" }

  $Properties = @{
    BundleFlavor = "offline";
    ImageRoot = "$(Get-InstallDir $Platform)\";
    INCLUDE_SWIFT_DOCC = $INCLUDE_SWIFT_DOCC;
    SWIFT_DOCC_BUILD = "$(Get-ProjectBinaryCache $HostPlatform DocC)\release";
    SWIFT_DOCC_RENDER_ARTIFACT_ROOT = "${SourceCache}\swift-docc-render-artifact";
    PythonVersion = $PythonVersion
  }

  Invoke-IsolatingEnvVars {
    Invoke-VsDevShell $Platform
    # Avoid hard-coding the VC tools version number
    $VCRedistDir = (Get-ChildItem "${env:VCToolsRedistDir}\$($HostPlatform.Architecture.ShortName)" -Filter "Microsoft.VC*.CRT").FullName
    if ($VCRedistDir) {
      $Properties["VCRedistDir"] = "$VCRedistDir\"
    }
  }

  $Properties["Platforms"] = "`"$(
    @(
      if ($WindowsSDKBuilds) { "windows" }
      if ($AndroidSDKBuilds) { "android" }
    ) -Join ";"
  )`"";
  $Properties["AndroidArchitectures"] = "`"$(($AndroidSDKBuilds | ForEach-Object { $_.Architecture.LLVMName }) -Join ";")`""
  $Properties["WindowsArchitectures"] = "`"$(($WindowsSDKBuilds | ForEach-Object { $_.Architecture.LLVMName }) -Join ";")`""
  $Properties["ToolchainVariants"] = "`"asserts$(if ($IncludeNoAsserts) { ";noasserts" })`"";
  foreach ($Build in $WindowsSDKBuilds) {
    $Properties["WindowsRuntime$($Build.Architecture.ShortName.ToUpperInvariant())"] = Get-WindowsRuntimeInstallRoot $Build
  }

  Build-WiXProject bundle\installer.wixproj -Platform $Platform -Bundle -Properties $Properties
}

function Copy-BuildArtifactsToStage([Hashtable] $Platform) {
  # Save the installer binary log
  Copy-File "$BinaryCache\$($Platform.Triple)\msi\$($Platform.Architecture.VSName)-$([System.IO.Path]::GetFileNameWithoutExtension("bundle\installer.wixproj")).binlog" $Stage
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\*.cab" $Stage
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\*.msi" $Stage
  # Needed by the swift.org code-signing pipeline, which re-links the bundle
  # from pre-built artifacts without rebuilding project references.
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\baf.dll" $Stage
  foreach ($Build in $WindowsSDKBuilds) {
    Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Build.Architecture.VSName)\*.msm" $Stage
  }
  Copy-File "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\installer.exe" $Stage
  # Extract installer engine to ease code-signing on swift.org CI
  New-Item -ItemType Directory -Path "$BinaryCache\$($Platform.Triple)\installer\$($Platform.Architecture.VSName)" -ErrorAction Ignore | Out-Null
  Invoke-WithDotNetRuntime {
    Invoke-Program (Get-DotNet) "$($WiX.Path)\wix.dll" -- burn detach -acceptEula $WiX.EulaIdentifier "$BinaryCache\$($Platform.Triple)\installer\Release\$($Platform.Architecture.VSName)\installer.exe" -engine "$Stage\installer-engine.exe" -intermediateFolder "$BinaryCache\$($Platform.Triple)\installer\$($Platform.Architecture.VSName)\"
  }
}

#-------------------------------------------------------------------

try {

Invoke-BuildStep Build-mimalloc $HostPlatform
Invoke-BuildStep Patch-mimalloc $HostPlatform
Invoke-BuildStep Build-Installer $HostPlatform

if ($Stage) {
  Copy-BuildArtifactsToStage $HostPlatform
}

if ($IncludeSBoM) {
  Invoke-IsolatingEnvVars {
    $env:SYFT_FILE_METADATA_SELECTION = "all"
    $env:SYFT_FILE_CONTENT_GLOBS = "**\*.h"
    $env:SYFT_FILE_METADATA_DIGESTS = "sha256"
    Invoke-Program (Get-DependencyManifest).Syft -- `
        --base-path $BinaryCache `
        --source-name Swift `
        --source-version $ProductVersion `
        -o spdx-json=$ToolchainIdentifier-sbom.spdx.json `
        -o syft-json=$ToolchainIdentifier-sbom.syft.json `
        -o cyclonedx-xml=$ToolchainIdentifier-sbom.cyclone.xml `
        -o syft-table `
        dir:$(Get-InstallDir $HostPlatform)

    if ($Stage) {
      Copy-File $ToolchainIdentifier-sbom.spdx.json $Stage
      Copy-File $ToolchainIdentifier-sbom.syft.json $Stage
      Copy-File $ToolchainIdentifier-sbom.cyclone.xml $Stage
    }
  }
}

} catch {
  Write-BuildError $_
  exit 1
} finally {
  Write-BuildTimings "BuildInstaller"
}
