<#
.SYNOPSIS
Builds the Swift toolchain like build.ps1, running the independent phases in
parallel.

.DESCRIPTION
Takes the same parameters as build.ps1. The Windows SDKs of the non-host
architectures build alongside the toolchain, and the package tests run
alongside the compiler tests.
#>
[CmdletBinding(PositionalBinding = $false)]
param
(
  # Runs one build branch started by Start-BuildBranch.
  [string] $BranchPath = ""
)

DynamicParam {
  $Parameters = [Management.Automation.RuntimeDefinedParameterDictionary]::new()
  foreach ($Parameter in (Get-Command "$PSScriptRoot\build.ps1").Parameters.Values) {
    if ([Management.Automation.Cmdlet]::CommonParameters.Contains($Parameter.Name) -or
        [Management.Automation.Cmdlet]::OptionalCommonParameters.Contains($Parameter.Name)) {
      continue
    }
    $Attributes = [Collections.ObjectModel.Collection[Attribute]]::new()
    $Attributes.Add([Management.Automation.ParameterAttribute]::new())
    if ($Parameter.Aliases.Count -gt 0) {
      $Attributes.Add([Management.Automation.AliasAttribute]::new([string[]] $Parameter.Aliases))
    }
    $Parameters.Add($Parameter.Name, [Management.Automation.RuntimeDefinedParameter]::new(
      $Parameter.Name, $Parameter.ParameterType, $Attributes))
  }
  return $Parameters
}

end {

if ($BranchPath) {
  $BranchSpec = Import-Clixml -Path $BranchPath
  $BuildParameters = $BranchSpec.Parameters
} else {
  # Plain values, so that the build branches can deserialize them.
  $BuildParameters = @{}
  foreach ($Entry in $PSBoundParameters.GetEnumerator()) {
    if ($Entry.Key -eq "BranchPath") { continue }
    $Value = $Entry.Value
    if ($Value -is [IO.FileSystemInfo]) { $Value = $Value.ToString() }
    if ($Value -is [switch]) { $Value = $Value.IsPresent }
    $BuildParameters[$Entry.Key] = $Value
  }
}

# Bind the parameters with build.ps1's param block to get its defaults and validation.
$BuildParamBlock = (Get-Command "$PSScriptRoot\build.ps1").ScriptBlock.Ast.ParamBlock
$BuildParameterNames = @($BuildParamBlock.Parameters | ForEach-Object { $_.Name.VariablePath.UserPath })
$BindBuildParameters = [ScriptBlock]::Create(
  "$($BuildParamBlock.Extent.Text)`n@{ $(($BuildParameterNames | ForEach-Object { "'$_' = `$$_" }) -join '; ') }")
$BoundValues = & $BindBuildParameters @BuildParameters
foreach ($Name in $BuildParameterNames) {
  Set-Variable -Name $Name -Value $BoundValues[$Name]
}

. "$PSScriptRoot\build-common.ps1"

# These tests don't use the outputs of the compiler tests.
$PackageTests = @("foundation", "llbuild", "swiftpm", "swift-format", "sourcekit-lsp")

$BuildBranches = @()

# Runs $Block in a child build-multi.ps1 at below normal priority, so that the
# main branch keeps the CPU. Its output is printed by Wait-BuildBranches.
function Start-BuildBranch([string] $Name, [ScriptBlock] $Block, [object[]] $Arguments = @()) {
  $Log = "$BinaryCache\build-multi.$PID.$($script:BuildBranches.Count)"
  @{ Parameters = $BuildParameters; Block = $Block.ToString(); Arguments = $Arguments } |
    Export-Clixml -Path "$Log.xml"
  $Process = Start-Process -PassThru -NoNewWindow -FilePath (Get-Process -Id $PID).Path `
    -ArgumentList "-NoProfile -ExecutionPolicy $(Get-ExecutionPolicy) -File `"$PSCommandPath`" -BranchPath `"$Log.xml`"" `
    -RedirectStandardOutput "$Log.out" -RedirectStandardError "$Log.err"
  # Windows PowerShell only reports the exit code of a process whose handle was
  # opened before it exited.
  $null = $Process.Handle
  $Process.PriorityClass = [Diagnostics.ProcessPriorityClass]::BelowNormal
  $Branch = @{ Name = $Name; Process = $Process; Log = $Log }
  $script:BuildBranches += $Branch
  return $Branch
}

function Write-BuildBranchOutput([Hashtable] $Branch) {
  Write-Host -ForegroundColor Cyan "----- Output of $($Branch.Name) -----"
  Get-Content -ErrorAction Ignore "$($Branch.Log).out", "$($Branch.Log).err" | Write-Host
  Remove-Item -ErrorAction Ignore "$($Branch.Log).*"
}

function Wait-BuildBranches([Hashtable[]] $Branches) {
  if ($Branches.Count -eq 0) { return }
  Record-OperationTime $HostPlatform "Wait-BuildBranches" {
    foreach ($Branch in $Branches) {
      $Branch.Process.WaitForExit()
    }
  }
  foreach ($Branch in $Branches) {
    Write-BuildBranchOutput $Branch
  }
  foreach ($Branch in $Branches) {
    if ($Branch.Process.ExitCode -ne 0) {
      throw "$($Branch.Name) failed with exit code $($Branch.Process.ExitCode)."
    }
  }
}

#-------------------------------------------------------------------

if ($BranchPath) {
  try {
    Get-Dependencies
    $BranchBlock = [ScriptBlock]::Create($BranchSpec.Block)
    $BranchArguments = [object[]] @($BranchSpec.Arguments)
    & $BranchBlock @BranchArguments
  } catch {
    Write-BuildError $_
    exit 1
  } finally {
    if ($Summary) {
      Write-Summary
    }
  }
  exit 0
}

try {

Get-Dependencies

if ($DependenciesOnly) {
  exit 0;
}

if ($Clean) {
  Clear-BuildTree
}

if ($Toolchain) {
  Build-Stage1Toolchain
}

# The SDKs only need the Stage1 compilers. Each architecture installs into its
# own SDK, since their installs overwrite each other, and is merged below.
$WindowsSDKBranches = @()
if ($Windows) {
  Sync-Runtimes
  $WindowsSDKBranches = @($WindowsSDKBuilds | Where-Object { $_ -ne $HostPlatform } | ForEach-Object {
    Start-BuildBranch "the Windows $($_.Architecture.LLVMName) SDK" {
      param([string] $Architecture)
      $Platform = $WindowsSDKBuilds | Where-Object { $_.Architecture.LLVMName -eq $Architecture }
      Build-SDKDependencies @($Platform) -Compilers $Compilers.Stage1
      Build-WindowsSDK $Platform -SDKIdentifier "Windows-$Architecture"
    } $_.Architecture.LLVMName
  })
}

if ($Toolchain) {
  Build-HostSDK
  Build-Stage2Toolchain
}

if ($IncludeDS2) {
  Invoke-BuildStep Build-RegsGen2 $BuildPlatform
}

if ($Windows) {
  if ($WindowsSDKBuilds -contains $HostPlatform) {
    Build-SDKDependencies @($HostPlatform) -Compilers $Compilers.Stage1
    Build-WindowsSDK $HostPlatform
  }
  Wait-BuildBranches $WindowsSDKBranches
  foreach ($Build in $WindowsSDKBuilds) {
    if ($Build -eq $HostPlatform) { continue }
    $ArchitectureSDK = Get-SwiftSDK -OS Windows -Identifier "Windows-$($Build.Architecture.LLVMName)"
    Copy-Directory "$ArchitectureSDK\*" (Get-SwiftSDK -OS Windows)
    Remove-Item -Force -Recurse $ArchitectureSDK
  }
  Install-WindowsSDK
}

if ($Android) {
  Build-AndroidSDKs
}

if ($Package) {
  Invoke-BuildStep Build-mimalloc $HostPlatform
  Invoke-BuildStep Patch-mimalloc $HostPlatform
  Invoke-BuildStep Build-Installer $HostPlatform
}

if ($Stage) {
  Copy-BuildArtifactsToStage $HostPlatform
}

if (-not $IsCrossCompiling) {
  Stage-TestToolchain $Test
  $TestBranches = @()
  if ($Test | Where-Object { $PackageTests -contains $_ }) {
    $TestBranches = @(Start-BuildBranch "the package tests" {
      Invoke-Tests @($Test | Where-Object { $PackageTests -contains $_ })
    })
  }
  Invoke-Tests @($Test | Where-Object { $PackageTests -notcontains $_ })
  Wait-BuildBranches $TestBranches
}

if ($IncludeSBoM) {
  Write-SBoM
}

} catch {
  foreach ($Branch in $BuildBranches) {
    if (-not $Branch.Process.HasExited) {
      & taskkill /T /F /PID $Branch.Process.Id | Out-Null
      $Branch.Process.WaitForExit()
    }
    if (Test-Path "$($Branch.Log).out") {
      Write-BuildBranchOutput $Branch
    }
  }
  Write-BuildError $_
  exit 1
} finally {
  if ($Summary) {
    Write-Summary
  }
}

}
