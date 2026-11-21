# build.ps1 is kept for existing invocations. It takes the parameters of
# BuildSequential.ps1, which builds the toolchain with SetupDependencies.ps1,
# BuildToolchain.ps1, BuildSDK.ps1, BuildInstaller.ps1 and TestToolchain.ps1,
# and runs it with them.
[CmdletBinding(PositionalBinding = $false)]
param()
dynamicparam {
  $Parameters = New-Object System.Management.Automation.RuntimeDefinedParameterDictionary
  foreach ($Parameter in (Get-Command "$PSScriptRoot\BuildSequential.ps1").Parameters.Values) {
    if ([System.Management.Automation.PSCmdlet]::CommonParameters.Contains($Parameter.Name)) { continue }
    $Parameters.Add($Parameter.Name, (New-Object System.Management.Automation.RuntimeDefinedParameter($Parameter.Name, $Parameter.ParameterType, $Parameter.Attributes)))
  }
  return $Parameters
}
end {
  & "$PSScriptRoot\BuildSequential.ps1" @PSBoundParameters
  exit $LastExitCode
}
