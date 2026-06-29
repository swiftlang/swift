# Copyright 2026 Apple Inc. and the Swift project authors
#
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

<#
.SYNOPSIS
Publishes the PDB files produced by a Swift Windows toolchain build into a
Microsoft SymStore layout and compresses the layout into a .zip archive.

.DESCRIPTION
Walks each input directory recursively for .pdb files, adds them to the
specified symbol store using symstore.exe, and writes a .zip containing the
resulting store. The zip can be expanded directly into an object store that
serves a Microsoft-compatible debug symbols endpoint.

This script is expected to be invoked after build.ps1 has installed the
toolchain and runtime files to the image root. It does not modify the install
tree.

.PARAMETER Search
One or more directories to search recursively for .pdb files.

.PARAMETER SymbolStore
Path to the symbol store directory to populate. Created if missing.

.PARAMETER Destination
Path to the .zip file to produce. Any existing file at this path is replaced.

.PARAMETER Product
Product name recorded in the symbol store transaction.
Default: 'swift'.

.PARAMETER Version
Optional version string recorded in the symbol store transaction.

.PARAMETER Comment
Optional comment recorded in the symbol store transaction.

.EXAMPLE
CreateSymStore.ps1 -Search "S:\Program Files\Swift" `
                   -SymbolStore "S:\b\symstore" `
                   -Destination "S:\b\artifacts\swift-windows-symbols.zip" `
                   -Version "6.3.0"
#>
[CmdletBinding(PositionalBinding = $false)]
param(
    [Parameter(Mandatory)]
    [string[]] $Search,

    [Parameter(Mandatory)]
    [string] $SymbolStore,

    [Parameter(Mandatory)]
    [string] $Destination,

    [string] $Product = "swift",

    [string] $Version,

    [string] $Comment
)

Set-StrictMode -Version 3.0
$ErrorActionPreference = "Stop"

$script:SymStoreStaging = $null

function Install-SymStoreExe {
    $script:SymStoreStaging = Join-Path ([System.IO.Path]::GetTempPath()) "swift-symstore-$([System.Guid]::NewGuid())"
    $LayoutDir = Join-Path $script:SymStoreStaging "layout"
    $ExtractDir = Join-Path $script:SymStoreStaging "extract"
    $InstallerPath = Join-Path $script:SymStoreStaging "winsdksetup.exe"

    $InstallerUrl = "https://go.microsoft.com/fwlink/?linkid=2361309"
    $InstallerHash = "D1A79958227B05FD0C554DB022FD718048B33B9EB79FAE70CA918A7A6F172627"

    New-Item -ItemType Directory -Path $script:SymStoreStaging -Force | Out-Null

    Write-Host "Downloading Windows SDK installer..."
    Invoke-WebRequest -Uri $InstallerUrl -OutFile $InstallerPath -UseBasicParsing
    $SHA256 = Get-FileHash -Path $InstallerPath -Algorithm SHA256
    if ($SHA256.Hash -ne $InstallerHash) {
        throw "SHA256 mismatch ($($SHA256.Hash) vs $InstallerHash)"
    }

    Write-Host "Laying out Windows SDK installers..."
    $Process = Start-Process -FilePath $InstallerPath `
        -ArgumentList "/features", "OptionId.WindowsDesktopDebuggers", "/layout", "`"$LayoutDir`"", "/q", "/norestart" `
        -Wait -PassThru
    if ($Process.ExitCode -ne 0) {
        throw "Windows SDK layout failed with exit code $($Process.ExitCode)"
    }

    $MsiPath = Join-Path $LayoutDir "Installers\X64 Debuggers And Tools-x64_en-us.msi"
    if (-not (Test-Path -LiteralPath $MsiPath)) {
        throw "Debugging Tools MSI not found at $MsiPath"
    }

    Write-Host "Extracting Debugging Tools for Windows..."
    $Process = Start-Process -FilePath "msiexec.exe" `
        -ArgumentList "/a", "`"$MsiPath`"", "/qn", "TARGETDIR=`"$ExtractDir`"" `
        -Wait -PassThru
    if ($Process.ExitCode -ne 0) {
        throw "msiexec admin install failed with exit code $($Process.ExitCode)"
    }

    $SymStorePath = Join-Path $ExtractDir "Windows Kits\10\Debuggers\x64\symstore.exe"
    if (-not (Test-Path -LiteralPath $SymStorePath)) {
        throw "symstore.exe was not found at $SymStorePath after extraction."
    }
    return $SymStorePath
}

function Get-SymStoreExe {
    $OnPath = Get-Command symstore.exe -ErrorAction SilentlyContinue
    if ($OnPath) { return $OnPath.Source }
    return Install-SymStoreExe
}

try {
    $SymStore = Get-SymStoreExe
    Write-Host "Using symstore: $SymStore"

    if (-not (Test-Path -LiteralPath $SymbolStore)) {
        New-Item -ItemType Directory -Path $SymbolStore -Force | Out-Null
    }

    $Common = @("add", "/r", "/o", "/s", $SymbolStore, "/t", $Product)
    if ($Version) { $Common += @("/v", $Version) }
    if ($Comment) { $Common += @("/c", $Comment) }

    $Added = 0
    foreach ($Path in $Search) {
        if (-not (Test-Path -LiteralPath $Path)) {
            Write-Warning "Skipping missing search path: $Path"
            continue
        }
        Write-Host "Publishing PDBs from $Path"
        & $SymStore @Common "/f" (Join-Path $Path "*.pdb")
        if ($LASTEXITCODE -ne 0) {
            throw "symstore.exe failed with exit code $LASTEXITCODE for $Path"
        }
        $Added++
    }

    if ($Added -eq 0) {
        throw "No input paths were processed; nothing to publish."
    }

    $DestinationDir = Split-Path -Parent $Destination
    if ($DestinationDir -and -not (Test-Path -LiteralPath $DestinationDir)) {
        New-Item -ItemType Directory -Path $DestinationDir -Force | Out-Null
    }
    if (Test-Path -LiteralPath $Destination) {
        Remove-Item -LiteralPath $Destination -Force
    }

    Write-Host "Compressing symbol store to $Destination"
    Compress-Archive -Path (Join-Path $SymbolStore "*") `
        -DestinationPath $Destination `
        -CompressionLevel Optimal

    Write-Host "Wrote $Destination"
} finally {
    if ($script:SymStoreStaging -and (Test-Path -LiteralPath $script:SymStoreStaging)) {
        Remove-Item -LiteralPath $script:SymStoreStaging -Recurse -Force -ErrorAction SilentlyContinue
    }
}
