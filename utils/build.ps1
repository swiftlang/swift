# build.ps1 is kept for existing invocations. It forwards its arguments to
# BuildSequential.ps1, which builds the toolchain with SetupDependencies.ps1,
# BuildToolchain.ps1, BuildSDK.ps1, BuildInstaller.ps1 and TestToolchain.ps1.
& "$PSScriptRoot\BuildSequential.ps1" @args
exit $LastExitCode
