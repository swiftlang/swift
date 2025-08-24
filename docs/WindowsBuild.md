# Building the Swift toolchain on Windows

The following is a step by step guide on how to build the Swift toolchain on x86/x64/ARM64 Windows.

## Installing the dependencies

### Visual Studio Build Tools

Visual Studio Build Tools 2017 or newer is needed to build Swift on Windows. The free Community edition is sufficient to build the Swift toolchain. We will use the 2022 Community Edition.

In a PowerShell shell, run the following command to install the required build tools.

```powershell
curl.exe -sOL https://aka.ms/vs/17/release/vs_BuildTools.exe
./vs_BuildTools.exe `
  --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 `
  --add Microsoft.VisualStudio.Component.VC.Tools.ARM64 `
  --add Microsoft.VisualStudio.Component.VC.14.44.17.14.ARM64 `
  --add Microsoft.VisualStudio.Component.VC.14.44.17.14.x86.x64 `
  --add Microsoft.VisualStudio.Component.VC.ATL `
  --add Microsoft.VisualStudio.Component.VC.ATL.ARM64 `
  --add Microsoft.VisualStudio.Component.Windows10SDK `
  --add Microsoft.VisualStudio.Component.Windows11SDK.22621 `
  --add Microsoft.VisualStudio.Component.VC.CMake.Project `
  --add Microsoft.NetCore.Component.Runtime.9.0 `
Remove-Item ./vs_BuildTools.exe
```

The following [link](https://docs.microsoft.com/visualstudio/install/workload-component-id-vs-build-tools?view=vs-2022) helps in finding the component name given its ID for Visual Studio 2022.

> [!NOTE]
> You only need the x64/x86 components when building for x64/x86 and vice versa. You need everything if you are cross-compiling.

### Other dependencies

In a PowerShell shell, run the following commands to install the rest of the required tools.

```powershell
winget install --id Git.Git --accept-package-agreements --accept-source-agreements
winget install --id Python.Python.3.13 --accept-package-agreements --accept-source-agreements
winget install --id Kitware.CMake --version 3.29.2 --accept-package-agreements --accept-source-agreements
winget install --id Ninja-build.Ninja --version 1.13.1 --accept-package-agreements --accept-source-agreements
```

> [!NOTE]
> If Git and Python 3.7+ are already installed on your system, you don’t need to reinstall them here (i.e ignore the first 2 commands).

## Windows configuration

### Symbolic link creation permissions

Symbolic links are used in some of the swift repositories. You need to grant permission to your user to create symbolic links in order to properly clone the repositories:

Using `gpedit.msc` add your user to the list of trusted user in `Computer Configuration\Windows Settings\Security Settings\Local Policies\User Rights Assignment\Create Symbolic links`.

In a PowerShell shell, run the following command to enable Git symbolic links and to disable automatic line endings conversion to support all the test suite:

```powershell
git config --global --add core.symlink true
git config --global --add core.autocrlf false
```

> [!IMPORTANT]
> Please make sure to consider the security risks listed [here](https://docs.microsoft.com/en-us/windows/security/threat-protection/security-policy-settings/create-symbolic-links).

## Cloning the repositories

- Open a regular (non admin, non Visual Studio) Powershell shell.
- Create a directory named S at the root of the C:\ drive: `mkdir C:\S`.
- Navigate to that directory: `cd C:\S`.
- Clone the Swift repository: `git clone https://github.com/swiftlang/swift.git`.
- Switch to the branch you want to build: `git -C swift checkout release/6.2`.
- Clone the rest of the repositories: `.\swift\utils\update-checkout --scheme release/6.2 --reset-to-remote --skip-repository swift`.

## Starting the build

- Set the `SKIP_UPDATE_CHECKOUT` environment variable to `true` to avoid cloning the repositories again: `$env:SKIP_UPDATE_CHECKOUT="true"`.
- Start the build: `.\swift\utils\build-windows-toolchain.bat`. This will build the toolchain, its installer and run all the test suite.
