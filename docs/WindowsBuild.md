# Building the Swift toolchain on Windows

The following is a step by step guide on how to build the Swift toolchain on x86/x64/ARM64 Windows.

## Installing the dependencies

### Visual Studio Build Tools

Visual Studio 2022 or newer is needed to build Swift on Windows. The free Community edition is sufficient to build the Swift toolchain. We will use the 2022 Community Edition.

In a CMD shell, run the following command to install the required build tools.

```cmd
curl.exe -sOL https://aka.ms/vs/17/release/vs_community.exe
vs_community.exe ^
  --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 ^
  --add Microsoft.VisualStudio.Component.VC.Tools.ARM64 ^
  --add Microsoft.VisualStudio.Component.VC.14.44.17.14.ARM64 ^
  --add Microsoft.VisualStudio.Component.VC.14.44.17.14.x86.x64 ^
  --add Microsoft.VisualStudio.Component.VC.ATL ^
  --add Microsoft.VisualStudio.Component.VC.ATL.ARM64 ^
  --add Microsoft.VisualStudio.Component.Windows10SDK ^
  --add Microsoft.VisualStudio.Component.Windows11SDK.22621 ^
  --add Microsoft.VisualStudio.Component.VC.CMake.Project ^
del /q vs_community.exe
```

The following [link](https://docs.microsoft.com/visualstudio/install/workload-component-id-vs-build-tools?view=vs-2022) helps in finding the component name given its ID for Visual Studio 2022.

### Other dependencies

In a CMD shell, run the following commands to install the rest of the required tools.

```cmd
winget install --id "Git.Git" --accept-package-agreements --accept-source-agreements
winget install --id "Python.Python.3.13" --accept-package-agreements --accept-source-agreements
winget install --id "Kitware.CMake" --version 3.29.2 --accept-package-agreements --accept-source-agreements
winget install --id "Ninja-build.Ninja" --version 1.13.1 --accept-package-agreements --accept-source-agreements
winget install --id "Microsoft.DotNet.Runtime.9" --architecture x64 --accept-package-agreements --accept-source-agreements
```

## Windows configuration

### Symbolic link creation permissions

Symbolic links are used in some of the swift repositories. You need to grant permission to your user to create symbolic links in order to properly clone the repositories:

Using `gpedit.msc` add your user to the list of trusted user in ^Computer Configuration\Windows Settings\Security Settings\Local Policies\User Rights Assignment\Create Symbolic links`.

In a CMD shell, run the following commands to enable Git symbolic links and to disable automatic line endings conversion to support all the test suite:

```cmd
git config --global --add core.symlinks true
git config --global --add core.autocrlf false
```

> [!IMPORTANT]
> Please make sure to consider the security risks listed [here](https://docs.microsoft.com/en-us/windows/security/threat-protection/security-policy-settings/create-symbolic-links).

## Environment Setup

The build has to be performed from a drive named `S:`. You can associate any directory to a virtual drive `S:` with the `subst` command. The following command maps the `C:\toolchain` directory.

```cmd
subst S: C:\toolchain
```

You can make this permanent by adding the entry to the Windows Registry. To do so, execute the following command from an Administrator PowerShell:

```ps1
Set-ItemProperty -Path "HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager\DOS Devices" -Name "S:" -Value "\??\C:\toolchain"
```

## Cloning the repositories

- Open a regular (non admin, non Visual Studio) CMD shell.
- Navigate to the root of the `S:` drive: `S:`.
- Create a directory named `SourceCache` and navigate to it: `mkdir SourceCache && cd SourceCache`.
- Clone the Swift repository: `git clone https://github.com/swiftlang/swift.git`.
- Switch to the branch you want to build: `git -C swift checkout release/6.2`.
- Clone the rest of the repositories: `.\swift\utils\update-checkout --clone --scheme release/6.2 --reset-to-remote --skip-repository swift`.

## Starting the build

- Open a regular (non admin, non Visual Studio) CMD shell.
- Run `S:\SourceCache\swift\utils\build.cmd -Windows`. This will build the toolchain, its installer and run all the test suite.
- (Optional): The available flags for `build.cmd` are documented in `swift/utils/build.ps1`.
