# Copyright 2026 Apple Inc. and the Swift project authors
#
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

<#
.SYNOPSIS
Builds the Swift SDK of one Windows architecture.

.DESCRIPTION
build.ps1 runs this script for each architecture in -WindowsSDKArchitectures.
The SDK is built with the Stage1 compilers that build.ps1 has built, and with
the same settings as the rest of the build.

.PARAMETER ParametersPath
The file in which build.ps1 saved its parameters.

.PARAMETER Architecture
The LLVM name of the architecture to build the SDK for, for example aarch64.

.PARAMETER SDKIdentifier
The name of the SDK to build into, for example Windows.
#>

[CmdletBinding(PositionalBinding = $false)]
param
(
  [Parameter(Mandatory = $true)]
  [string] $ParametersPath,
  [Parameter(Mandatory = $true)]
  [string] $Architecture,
  [Parameter(Mandatory = $true)]
  [string] $SDKIdentifier
)

$SDKBuild = @{ Architecture = $Architecture; Identifier = $SDKIdentifier }

# Recreate the parameters of build.ps1, which the shared definitions use.
$BuildParameterTypes = (Get-Command "$PSScriptRoot\build.ps1").Parameters
foreach ($Parameter in (Import-Clixml -Path $ParametersPath).GetEnumerator()) {
  $Type = $BuildParameterTypes[$Parameter.Key].ParameterType
  $Value = $Parameter.Value
  if ($null -eq $Value -and $Type.IsArray) { $Value = @() }
  Set-Variable -Name $Parameter.Key -Value ($Value -as $Type)
}

. "$PSScriptRoot\build-common.ps1"

try {
  Get-Dependencies

  $Platform = $WindowsSDKBuilds | Where-Object { $_.Architecture.LLVMName -eq $SDKBuild.Architecture }
  if (-not $Platform) {
    throw "Unknown Windows SDK architecture '$($SDKBuild.Architecture)'."
  }

  Build-SDKDependencies @($Platform) -Compilers $Compilers.Stage1

  if ($Platform.LinkModes.contains("dynamic")) {
    Invoke-BuildStep Build-SDK $Platform -Context @{
      SDKIdentifier        = $SDKBuild.Identifier;
      Variant              = "Dynamic";
      Compilers            = $Compilers.Stage1;
      Static               = $false;
      BuildFoundation      = $true;
      SupplementalRuntimes = $SDKSupplementalRuntimes;
    }
  }

  if ($Platform.LinkModes.contains("static")) {
    Invoke-BuildStep Build-SDK $Platform -Context @{
      SDKIdentifier        = $SDKBuild.Identifier;
      Variant              = "Static";
      Compilers            = $Compilers.Stage1;
      Static               = $true;
      BuildFoundation      = $true;
      SupplementalRuntimes = $SDKSupplementalRuntimes;
    }
  }

  $SDKROOT = Get-SwiftSDK -OS Windows -Identifier $SDKBuild.Identifier
  ConvertTo-ThickLayout -Platform $Platform -Resources "${SDKROOT}\usr\lib\swift\windows"        -Filter @("*.lib")
  ConvertTo-ThickLayout -Platform $Platform -Resources "${SDKROOT}\usr\lib\swift_static\windows" -Filter @("*.lib")
} catch {
  Write-Host -ForegroundColor Red "Error: $_"
  Write-Host -ForegroundColor Red $_.ScriptStackTrace
  exit 1
} finally {
  if ($Summary) {
    Write-Summary
  }
}
