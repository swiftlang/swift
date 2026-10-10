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
build.ps1 dot-sources this script to define Get-Dependencies.

Each dependency is probed first (for example with `--version`). Only a
dependency that is missing or fails its probe is (re)installed, and it must
pass the probe afterwards.

Running this script directly forwards its arguments to
`build.ps1 -DependenciesOnly`, which populates the artifact cache and exits
without building anything.

.EXAMPLE
PS> .\build-dependencies.ps1 -ArtifactCache C:\ArtifactCache -Test lldb,lldb-swift -Android
#>

if ($MyInvocation.InvocationName -ne ".") {
  & "$PSScriptRoot\build.ps1" -DependenciesOnly @args
  exit $LastExitCode
}

function Get-Dependencies {
  Record-OperationTime $BuildPlatform "Get-Dependencies" {
    function Write-Success([string] $Description) {
      $HeavyCheckMark = @{
        Object = [Char]0x2714
        ForegroundColor = 'DarkGreen'
        NoNewLine = $true
      }
      Write-Host @HeavyCheckMark
      Write-Host " $Description"
    }

    $Stopwatch = [Diagnostics.Stopwatch]::StartNew()
    Write-Host "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies ..." -ForegroundColor Cyan
    $ProgressPreference = "SilentlyContinue"

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

    function Invoke-WithArtifactLock([string] $Name, [ScriptBlock] $ScriptBlock) {
      $LockRoot = Join-Path -Path $ArtifactCache -ChildPath ".locks"
      New-Item -ItemType Directory -Path $LockRoot -ErrorAction Ignore | Out-Null
      $LockPath = Join-Path -Path $LockRoot -ChildPath "$Name.lock"

      $Lock = $null
      $Stopwatch = [Diagnostics.Stopwatch]::StartNew()
      while (-not $Lock) {
        try {
          $Lock = [IO.File]::Open($LockPath, [IO.FileMode]::OpenOrCreate,
                                  [IO.FileAccess]::ReadWrite, [IO.FileShare]::None)
        } catch [IO.IOException] {
          $ErrorCode = $_.Exception.HResult -band 0xffff
          if ($ErrorCode -notin 32, 33) { throw }
          if ($Stopwatch.Elapsed.TotalSeconds -ge $ArtifactLockTimeoutSeconds) {
            throw "Timed out after $ArtifactLockTimeoutSeconds seconds waiting for artifact lock '$LockPath'"
          }
          Start-Sleep -Milliseconds 100
        }
      }
      $Stopwatch.Stop()

      try {
        & $ScriptBlock
      } finally {
        $Lock.Dispose()
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

    if ($IncludeSBoM) {
      $syft = Get-Syft
      Install-Dependency "syft $SyftVersion" "$ArtifactCache\$($syft.Artifact)" {
        Test-Executable $syft.Path @("version")
      } {
        DownloadAndVerify $syft.URL "$ArtifactCache\$($syft.Artifact).zip" $syft.SHA256
        Expand-ArtifactZip "$($syft.Artifact).zip" $syft.Artifact
      }
    }

    function Get-KnownPython([string] $ArchName, [bool] $EmbeddedPython = $false) {
      if (-not $KnownPythons.ContainsKey($PythonVersion)) {
        throw "Unknown python version: $PythonVersion"
      }
      $Key = $(if ($EmbeddedPython) { "${ArchName}_Embedded" } else { $ArchName })
      return $KnownPythons[$PythonVersion][$Key]
    }

    function Install-Python([string] $ArchName, [bool] $EmbeddedPython = $false) {
      $Python = Get-KnownPython $ArchName $EmbeddedPython
      $FileName = $(if ($EmbeddedPython) { "EmbeddedPython$ArchName-$PythonVersion" } else { "Python$ArchName-$PythonVersion" })
      $PythonExecutable = if ($EmbeddedPython) {
        "$ArtifactCache\$FileName\python.exe"
      } else {
        "$ArtifactCache\$FileName\tools\python.exe"
      }
      $Description = $(if ($EmbeddedPython) { "$ArchName embedded Python $PythonVersion" } else { "$ArchName Python $PythonVersion" })
      Install-Dependency $Description "$ArtifactCache\$FileName" {
        # The build machine cannot run the host's Python when cross compiling.
        if ($ArchName -eq $BuildArchName) {
          Test-Executable $PythonExecutable
        } else {
          Test-Path -LiteralPath $PythonExecutable -PathType Leaf
        }
      } {
        DownloadAndVerify $Python.URL "$ArtifactCache\$FileName.zip" $Python.SHA256
        Expand-ArtifactZip "$FileName.zip" $FileName
      }
    }

    function Install-PIPIfNeeded {
      try {
        Invoke-Program -Silent "$(Get-PythonExecutable)" -m pip
      } catch {
        Invoke-Program -OutNull "$(Get-PythonExecutable)" '-I' -m ensurepip -U --default-pip
      } finally {
        Write-Success "pip"
      }
    }

    function Test-PythonModuleInstalled([string] $ModuleName) {
      # Also check the dependencies so that caches populated before one was
      # pinned get repaired.
      $Modules = @($ModuleName) + $PythonModules[$ModuleName].Dependencies
      try {
        Invoke-Program -Silent "$(Get-PythonExecutable)" -c "import importlib.util, sys; sys.exit(0 if all(importlib.util.find_spec(m) for m in sys.argv[1:]) else 1)" @Modules
        return $true
      } catch {
        return $false
      }
    }

    function Install-PythonModule([string] $ModuleName) {
      if (Test-PythonModuleInstalled $ModuleName) {
        # Write-Output "$ModuleName already installed."
        return
      }

      $TempRequirementsTxt = New-TemporaryFile
      $ArchName = $BuildPlatform.Architecture.CMakeName

      $Module = $PythonModules[$ModuleName]
      "$ModuleName==$($Module.Version) --hash=`"sha256:$($Module.SHA256[$ArchName])`"" | Out-File -FilePath $TempRequirementsTxt -Append -Encoding utf8
      foreach ($Dependency in $Module.Dependencies) {
        $DependencyModule = $PythonModules[$Dependency]
        "$Dependency==$($DependencyModule.Version) --hash=`"sha256:$($DependencyModule.SHA256[$ArchName])`"" | Out-File -FilePath $TempRequirementsTxt -Append -Encoding utf8
      }

      # Dependencies are pinned above; --require-hashes rejects anything else
      # pip would resolve on its own.
      Invoke-Program -OutNull "$(Get-PythonExecutable)" '-I' -m pip install -r $TempRequirementsTxt --require-hashes --disable-pip-version-check

      Write-Success "$ModuleName"
    }

    function Install-PythonModules {
      Install-PIPIfNeeded
      Install-PythonModule "packaging"  # For building LLVM 18+
      Install-PythonModule "setuptools" # Required for SWIG support
      if ($Test -contains "lldb" -or $Test -contains "lldb-swift") {
        Install-PythonModule "psutil"       # Required for testing LLDB
        Install-PythonModule "cryptography" # Required for testing LLDB
      }
    }

    # Ensure Python modules that are required as host build tools
    Install-Python $HostArchName
    Install-Python $HostArchName $true
    if ($IsCrossCompiling) {
      Install-Python $BuildArchName
      Install-Python $BuildArchName $true
    }
    Invoke-WithArtifactLock (Split-Path -Leaf (Get-PythonPath $BuildPlatform)) {
      Install-PythonModules
    }

    # WiX is needed both for packaging and for extracting the pinned toolchain
    # installer that bootstraps toolchain builds.
    if ($Toolchain -or $Package) {
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
    }

    if ($Test -contains "lldb" -or $Test -contains "lldb-swift") {
      # The make tool isn't part of MSYS
      $GnuWin32MakeURL = "https://downloads.sourceforge.net/project/ezwinports/make-4.4.1-without-guile-w32-bin.zip"
      $GnuWin32MakeHash = "fb66a02b530f7466f6222ce53c0b602c5288e601547a034e4156a512dd895ee7"
      Install-Dependency "GNUWin32 make 4.4.1" "$ArtifactCache\GnuWin32Make-4.4.1" {
        Test-Executable "$ArtifactCache\GnuWin32Make-4.4.1\bin\make.exe"
      } {
        DownloadAndVerify $GnuWin32MakeURL "$ArtifactCache\GnuWin32Make-4.4.1.zip" $GnuWin32MakeHash
        Expand-ArtifactZip GnuWin32Make-4.4.1.zip GnuWin32Make-4.4.1
      }
    }

    if (-not $Toolchain) { return }

    $ToolchainArtifact = "$ToolchainVersionIdentifier-$($BuildArchName.ToLowerInvariant())"
    Invoke-WithArtifactLock "SwiftToolchainExtraction" {
      Install-Dependency "Swift Toolchain $PinnedVersion" "$ArtifactCache\toolchains\$ToolchainArtifact" {
        Test-Executable (Join-Path -Path (Get-PinnedToolchainToolsDir) -ChildPath "swiftc.exe")
      } {
        DownloadAndVerify $PinnedBuild "$ArtifactCache\$PinnedToolchain.exe" $PinnedSHA256
        Extract-Toolchain "$PinnedToolchain.exe" -ToolchainName $ToolchainArtifact
      }
    }

    # Install CMake.
    $CMake = Get-CMake
    Install-Dependency "CMake $CMakeVersion" "$ArtifactCache\$($CMake.Artifact)" {
      Test-Executable $CMake.Path
    } {
      DownloadAndVerify $CMake.URL "$ArtifactCache\$($CMake.FileName)" $CMake.SHA256
      Expand-ArtifactZip $CMake.FileName $CMake.Artifact
    }

    if ($Android) {
      $NDK = Get-AndroidNDK
      Install-Dependency "Android NDK $AndroidNDKVersion" (Get-AndroidNDKPath) {
        Test-Executable "$(Get-AndroidNDKPath)\toolchains\llvm\prebuilt\windows-x86_64\bin\clang.exe"
      } {
        DownloadAndVerify $NDK.URL "$ArtifactCache\android-ndk-$AndroidNDKVersion-windows.zip" $NDK.SHA256
        Expand-ArtifactZip "android-ndk-$AndroidNDKVersion-windows.zip" "android-ndk-$AndroidNDKVersion" "android-ndk-$AndroidNDKVersion"
      }
    }

    if ($IncludeDS2) {
      $Artifact = "win_flex_bison-$($WinFlexBison.Version)"
      Install-Dependency "flex/bison $($WinFlexBison.Version)" "$ArtifactCache\$Artifact" {
        (Test-Executable (Get-FlexExecutable)) -and (Test-Executable (Get-BisonExecutable))
      } {
        DownloadAndVerify $WinFlexBison.URL "$ArtifactCache\$Artifact.zip" $WinFlexBison.SHA256
        Expand-ArtifactZip "$Artifact.zip" $Artifact
      }
    }

    if ($WinSDKVersion) {
      try {
        # Check whether VsDevShell can already resolve the requested Windows SDK Version
        Invoke-IsolatingEnvVars { Invoke-VsDevShell $HostPlatform }
      } catch {
        Write-Output "Windows SDK $WinSDKVersion not found. Downloading from nuget.org ..."
        if (-not (Get-Command nuget.exe -ErrorAction Ignore)) {
          throw "nuget.exe is needed to download Windows SDK $WinSDKVersion but is not on PATH."
        }
        Invoke-WithArtifactLock "WindowsSDK-$WinSDKVersion" {
          Invoke-Program nuget install Microsoft.Windows.SDK.CPP -Version $WinSDKVersion -OutputDirectory $NugetRoot

          # Set to script scope so Invoke-VsDevShell can read it.
          $script:CustomWinSDKRoot = "$NugetRoot\Microsoft.Windows.SDK.CPP.$WinSDKVersion\c"

          # Install each required architecture package and move files under the base /lib directory.
          $Builds = $WindowsSDKBuilds.Clone()
          if (-not ($HostPlatform -in $Builds)) {
            $Builds += $HostPlatform
          }

          foreach ($Build in $Builds) {
            Invoke-Program nuget install Microsoft.Windows.SDK.CPP.$($Build.Architecture.ShortName) -Version $WinSDKVersion -OutputDirectory $NugetRoot
            Copy-Directory "$NugetRoot\Microsoft.Windows.SDK.CPP.$($Build.Architecture.ShortName).$WinSDKVersion\c\*" "$CustomWinSDKRoot\lib\$WinSDKVersion"
          }
        }
      }
    }

    Write-Host -ForegroundColor Cyan "[$([DateTime]::Now.ToString("yyyy-MM-dd HH:mm:ss"))] Get-Dependencies took $($Stopwatch.Elapsed)"
    Write-Host ""
  }
}
