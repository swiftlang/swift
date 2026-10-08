# Copyright 2026 Apple Inc. and the Swift project authors
#
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# Definitions of the tools needed to build and test the Swift toolchain on
# Windows, shared by build.ps1 and build-dependencies.ps1.
#
# The dot-sourcing script must define $ArtifactCache before dot-sourcing this
# file. The functions also read $ArtifactLockTimeoutSeconds, $AndroidNDKVersion,
# $BuildArchName, $CMakeVersion, $PinnedVersion, $SyftVersion and
# $ToolchainVersionIdentifier.

$DefaultPinned = @{
  AMD64 = @{
    PinnedBuild = "https://download.swift.org/swift-6.4.x-branch/windows10/swift-6.4.x-DEVELOPMENT-SNAPSHOT-2026-08-01-a/swift-6.4.x-DEVELOPMENT-SNAPSHOT-2026-08-01-a-windows10.exe";
    PinnedSHA256 = "C287DD533A65A73D657B1B9F2305BE50552F89B46199A0F6162A287DEE547149";
    PinnedVersion = "6.4.0";
  };
  ARM64 = @{
    PinnedBuild = "https://download.swift.org/swift-6.4.x-branch/windows10-arm64/swift-6.4.x-DEVELOPMENT-SNAPSHOT-2026-08-01-a/swift-6.4.x-DEVELOPMENT-SNAPSHOT-2026-08-01-a-windows10-arm64.exe"
    PinnedSHA256 = "8C9F35E37DA08CC598E0CBC7821880D2282E4D02633DD78ECBB248370A5C9985";
    PinnedVersion = "6.4.0";
  };
}

$WiX = @{
  Version = "7.0.0";
  EulaIdentifier = "wix7";
  URL = "https://www.nuget.org/api/v2/package/wix/7.0.0";
  SHA256 = "7f992e57c356dcbda2ea961bf3b348e1bd7d31d96795be4ac391e04cf140536d";
  Path = [IO.Path]::Combine("$ArtifactCache\WiX-7.0.0", "tools", "net8.0", "any");
}

$DotNetRuntime = @{
  Version = "8.0.27";
  Runtimes = @{
    AMD64 = @{
      RuntimeIdentifier = "win-x64";
      URL = "https://builds.dotnet.microsoft.com/dotnet/Runtime/8.0.27/dotnet-runtime-8.0.27-win-x64.zip";
      SHA256 = "0708AEAB018AB9C2BFAAC248AD2F3A2A1046913D96CB9A0A904A107C4CE4A813";
    };
    ARM64 = @{
      RuntimeIdentifier = "win-arm64";
      URL = "https://builds.dotnet.microsoft.com/dotnet/Runtime/8.0.27/dotnet-runtime-8.0.27-win-arm64.zip";
      SHA256 = "6FFBD29E58B71AB1EE7DD02741C420AF172D27A3963098D57F6FE8887FC7BEBF";
    };
  };
}

$KnownPythons = @{
  "3.9.10" = @{
    AMD64 = @{
      URL = "https://www.nuget.org/api/v2/package/python/3.9.10";
      SHA256 = "ac43b491e9488ac926ed31c5594f0c9409a21ecbaf99dc7a93f8c7b24cf85867";
    };
    ARM64 = @{
      URL = "https://www.nuget.org/api/v2/package/pythonarm64/3.9.10";
      SHA256 = "429ada77e7f30e4bd8ff22953a1f35f98b2728e84c9b1d006712561785641f69";
    };
  };
  "3.10.1" = @{
    AMD64 = @{
      URL = "https://www.nuget.org/api/v2/package/python/3.10.1";
      SHA256 = "987a0e446d68900f58297bc47dc7a235ee4640a49dace58bc9f573797d3a8b33";
    };
    AMD64_Embedded = @{
      URL = "https://www.python.org/ftp/python/3.10.1/python-3.10.1-embed-amd64.zip";
      SHA256 = "502670dcdff0083847abf6a33f30be666594e7e5201cd6fccd4a523b577403de";
    };
    ARM64 = @{
      URL = "https://www.nuget.org/api/v2/package/pythonarm64/3.10.1";
      SHA256 = "16becfccedf1269ff0b8695a13c64fac2102a524d66cecf69a8f9229a43b10d3";
    };
    ARM64_Embedded = @{
      URL = "https://www.python.org/ftp/python/3.10.1/python-3.10.1-embed-arm64.zip";
      SHA256 = "1f9e215fe4e8f22a8e8fba1859efb1426437044fb3103ce85794630e3b511bc2";
    };
  };
}

$PythonModules = @{
  # One SHA256 per architecture. Most modules are pinned to an architecture
  # independent source distribution and have the same hashes.
  "packaging" = @{
    Version = "24.1";
    SHA256 = @{
      AMD64 = "026ed72c8ed3fcce5bf8950572258698927fd1dbda10a5e981cdf0ac37f4f002";
      ARM64 = "026ed72c8ed3fcce5bf8950572258698927fd1dbda10a5e981cdf0ac37f4f002";
    };
    Dependencies = @();
  };
  "setuptools" = @{
    Version = "75.1.0";
    SHA256 = @{
      AMD64 = "d59a21b17a275fb872a9c3dae73963160ae079f1049ed956880cd7c09b120538";
      ARM64 = "d59a21b17a275fb872a9c3dae73963160ae079f1049ed956880cd7c09b120538";
    };
    Dependencies = @();
  };
  "psutil" = @{
    Version = "6.1.0";
    SHA256 = @{
      AMD64 = "a8fb3752b491d246034fa4d279ff076501588ce8cbcdbb62c32fd7a377d996be";
      ARM64 = "353815f59a7f64cdaca1c0307ee13558a0512f6db064e92fe833784f08539c7a";
    };
    Dependencies = @();
  };
  "cryptography" = @{
    Version = "46.0.3";
    SHA256 = @{
      AMD64 = "416260257577718c05135c55958b674000baef9a1c7d9e8f306ec60d71db850f";
      ARM64 = "d89c3468de4cdc4f08a57e214384d0471911a3830fcdaf7a8cc587e42a866372";
    };
    Dependencies = @("cffi", "pycparser", "typing_extensions");
  };
  "cffi" = @{
    Version = "2.0.0";
    # There is no cp310 win_arm64 wheel; ARM64 builds from the sdist.
    SHA256 = @{
      AMD64 = "b18a3ed7d5b3bd8d9ef7a8cb226502c6bf8308df1525e1cc676c3680e7176739";
      ARM64 = "44d1b5909021139fe36001ae048dbdde8214afa20200eda0f64c068cac5d5529";
    };
    Dependencies = @();
  };
  "pycparser" = @{
    Version = "2.23";
    SHA256 = @{
      AMD64 = "e5c6e8d3fbad53479cab09ac03729e0a9faf2bee3db8208a550daf5af81a5934";
      ARM64 = "e5c6e8d3fbad53479cab09ac03729e0a9faf2bee3db8208a550daf5af81a5934";
    };
    Dependencies = @();
  };
  "typing_extensions" = @{
    Version = "4.15.0";
    SHA256 = @{
      AMD64 = "f0fa19c6845758ab08074a0cfa8b7aecb71c999ca73d62883bc25cc018c4e548";
      ARM64 = "f0fa19c6845758ab08074a0cfa8b7aecb71c999ca73d62883bc25cc018c4e548";
    };
    Dependencies = @();
  };
  "argparse" = @{
    Version = "1.4.0";
    SHA256 = @{
      AMD64 = "c31647edb69fd3d465a847ea3157d37bed1f95f19760b11a47aa91c04b666314";
      ARM64 = "c31647edb69fd3d465a847ea3157d37bed1f95f19760b11a47aa91c04b666314";
    };
    Dependencies = @();
  };
  "six" = @{
    Version = "1.17.0";
    SHA256 = @{
      AMD64 = "4721f391ed90541fddacab5acf947aa0d3dc7d27b2e1e8eda2be8970586c3274";
      ARM64 = "4721f391ed90541fddacab5acf947aa0d3dc7d27b2e1e8eda2be8970586c3274";
    };
    Dependencies = @();
  };
  "traceback2" = @{
    Version = "1.4.0";
    SHA256 = @{
      AMD64 = "8253cebec4b19094d67cc5ed5af99bf1dba1285292226e98a31929f87a5d6b23";
      ARM64 = "8253cebec4b19094d67cc5ed5af99bf1dba1285292226e98a31929f87a5d6b23";
    };
    Dependencies = @();
  };
  "linecache2" = @{
    Version = "1.0.0";
    SHA256 = @{
      AMD64 = "e78be9c0a0dfcbac712fe04fbf92b96cddae80b1b842f24248214c8496f006ef";
      ARM64 = "e78be9c0a0dfcbac712fe04fbf92b96cddae80b1b842f24248214c8496f006ef";
    };
    Dependencies = @();
  };
}

$KnownNDKs = @{
  r27d = @{
    URL = "https://dl.google.com/android/repository/android-ndk-r27d-windows.zip"
    SHA256 = "82094f53e66a76b6a9ec4fc35a5076091a92de3b91d13c5d4a7cfdb226304c59"
    ClangVersion = 18
  }
  r28c = @{
    URL = "https://dl.google.com/android/repository/android-ndk-r28c-windows.zip"
    SHA256 = "6bec98ac2354d8a919760889a1a41d020132e5e8cfa1b1fe51610a72c36a466b"
    ClangVersion = 19
  }
  r30 = @{
    URL = "https://dl.google.com/android/repository/android-ndk-r30-windows.zip"
    SHA256 = "b830098aaf18b67a42eb831c404e15e5f2990a474f054ac145b0bc957ac6d729"
    ClangVersion = 21
  }
}

$WinFlexBison = @{
  Version = "2.5.25"
  URL = "https://github.com/lexxmark/winflexbison/releases/download/v2.5.25/win_flex_bison-2.5.25.zip"
  SHA256 = "8D324B62BE33604B2C45AD1DD34AB93D722534448F55A16CA7292DE32B6AC135"
}

$KnownSyft = @{
  "1.29.1" = @{
    AMD64 = @{
      Artifact = "syft-1.29.1-windows-amd64"
      URL = "https://github.com/anchore/syft/releases/download/v1.29.1/syft_1.29.1_windows_amd64.zip"
      SHA256 = "3C67CD9AF40CDCC7FFCE041C8349B4A77F33810184820C05DF23440C8E0AA1D7"
      Path = [IO.Path]::Combine("$ArtifactCache\syft-1.29.1-windows-amd64", "syft.exe")
    }
  };
  "1.40.0" = @{
    AMD64 = @{
      Artifact = "syft-1.40.0-windows-amd64"
      URL = "https://github.com/anchore/syft/releases/download/v1.40.0/syft_1.40.0_windows_amd64.zip"
      SHA256 = "3F4021EC098B4BCBAF19BBA7028CF7704FEF12936970778CEC3C6D669B740E6D"
      Path = [IO.Path]::Combine("$ArtifactCache\syft-1.40.0-windows-amd64", "syft.exe")
    };
    ARM64 = @{
      Artifact = "syft-1.40.0-windows-arm64"
      URL = "https://github.com/anchore/syft/releases/download/v1.40.0/syft_1.40.0_windows_arm64.zip"
      SHA256 = "CE7129DBCC39809542C9BC5032B179131DFEE72C68C5B3741E3270A3D9ED46E4"
      Path = [IO.Path]::Combine("$ArtifactCache\syft-1.40.0-windows-arm64", "syft.exe")
    };
  }
}

$KnownCMakes = @{
  "4.4.1" = @{
    AMD64 = @{
      Artifact = "cmake-4.4.1-windows-amd64"
      URL = "https://github.com/Kitware/CMake/releases/download/v4.4.1/cmake-4.4.1-windows-x86_64.zip"
      SHA256 = "091919E1CDE162B69D2D5E0F3B1F5670C973E72133F78126FBB18042947D6F19"
      FileName = "cmake-4.4.1-windows-x86_64.zip"
      CMakeRoot = [IO.Path]::Combine("$ArtifactCache", "cmake-4.4.1-windows-amd64", "cmake-4.4.1-windows-x86_64", "share", "cmake-4.4")
      Path = [IO.Path]::Combine("$ArtifactCache", "cmake-4.4.1-windows-amd64", "cmake-4.4.1-windows-x86_64", "bin", "cmake.exe")
    };
    ARM64 = @{
      Artifact = "cmake-4.4.1-windows-arm64"
      URL = "https://github.com/Kitware/CMake/releases/download/v4.4.1/cmake-4.4.1-windows-arm64.zip"
      SHA256 = "DC59D9F377F891B8DA42EDE22F53717034A9D093092FCEAF6297FEEEC6AFBA29"
      FileName = "cmake-4.4.1-windows-arm64.zip"
      CMakeRoot = [IO.Path]::Combine("$ArtifactCache", "cmake-4.4.1-windows-arm64", "cmake-4.4.1-windows-arm64", "share", "cmake-4.4")
      Path = [IO.Path]::Combine("$ArtifactCache", "cmake-4.4.1-windows-arm64", "cmake-4.4.1-windows-arm64", "bin", "cmake.exe")
    };
  }
}

function Get-AndroidNDK {
  $NDK = $KnownNDKs[$AndroidNDKVersion]
  if (-not $NDK) { throw "Unsupported Android NDK version" }
  return $NDK
}

function Get-AndroidNDKPath {
  return Join-Path -Path $ArtifactCache -ChildPath "android-ndk-$AndroidNDKVersion"
}

function Get-FlexExecutable {
  return Join-Path -Path $ArtifactCache -ChildPath "win_flex_bison-$($WinFlexBison.Version)\win_flex.exe"
}

function Get-BisonExecutable {
  return Join-Path -Path $ArtifactCache -ChildPath "win_flex_bison-$($WinFlexBison.Version)\win_bison.exe"
}

function Get-Syft {
  return $KnownSyft[$SyftVersion][$BuildArchName]
}

function Get-CMake {
  return $KnownCMakes[$CMakeVersion][$BuildArchName]
}

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

function Get-DotNetRuntime() {
  if (-not $DotNetRuntime.Runtimes.ContainsKey($BuildArchName)) {
    throw "Unsupported .NET runtime host architecture '$BuildArchName'"
  }

  return $DotNetRuntime.Runtimes[$BuildArchName]
}

function Get-DotNetRuntimeRoot() {
  $Runtime = Get-DotNetRuntime
  return [IO.Path]::Combine("$ArtifactCache", "dotnet-runtime-$($DotNetRuntime.Version)-$($Runtime.RuntimeIdentifier)")
}

function Get-DotNet() {
  return [IO.Path]::Combine((Get-DotNetRuntimeRoot), "dotnet.exe")
}

function Invoke-WithDotNetRuntime([scriptblock] $Body) {
  Invoke-IsolatingEnvVars {
    $DotNetRuntimeRoot = Get-DotNetRuntimeRoot
    $env:DOTNET_ROOT = $DotNetRuntimeRoot
    $env:DOTNET_HOST_PATH = Get-DotNet
    $env:DOTNET_MULTILEVEL_LOOKUP = "0"

    & $Body
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

function Get-PinnedToolchainToolsDir() {
  $ToolchainArtifact = "$ToolchainVersionIdentifier-$($BuildArchName.ToLowerInvariant())"
  return [IO.Path]::Combine("$ArtifactCache\toolchains", $ToolchainArtifact,
    "LocalApp", "Programs", "Swift", "Toolchains", "$PinnedVersion+Asserts",
    "usr", "bin")
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

