# escape=`

# Windows container image with the dependencies needed to build and test the
# Swift toolchain with utils/build.ps1 (or utils/build-windows-toolchain.bat).
#
# Build it from the root of the swift checkout:
#
#   docker build -m 8GB -t swift-windows-ci `
#     -f utils\windows-docker\Dockerfile utils
#
# The default base image targets Windows Server 2019 hosts. Pick another one
# with --build-arg BASE_IMAGE=mcr.microsoft.com/windows/servercore:ltsc2022.
# With process isolation (the default on Windows Server), the image should
# match the host's Windows build, including the patch level: pass
# --build-arg BASE_IMAGE=mcr.microsoft.com/windows/servercore:10.0.<build>.<ubr>
# (build-windows-toolchain.bat does this) or use --isolation=hyperv.
#
# The artifacts that build.ps1 downloads (pinned Swift toolchain, CMake,
# Python, WiX, ...) are installed in C:\ArtifactCache, which is exported as
# ARTIFACT_CACHE. Pass it to build.ps1 with -ArtifactCache so that builds in
# the container do not download anything.
#
# Behind a proxy, add --build-arg HTTPS_PROXY=http://<host>:<port>.

ARG BASE_IMAGE=mcr.microsoft.com/windows/servercore:ltsc2019
FROM ${BASE_IMAGE}

SHELL ["powershell.exe", "-NoLogo", "-NoProfile", "-ExecutionPolicy", "Bypass", "-Command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue';"]

# Windows components (.NET, the Visual Studio installer, certificate
# revocation checks) read the WinINet and WinHTTP proxy settings, not
# HTTPS_PROXY. The settings are removed at the end of the build.
RUN if (-not $env:HTTPS_PROXY) { exit 0 }; `
    $Server = ([Uri]$env:HTTPS_PROXY).Authority; `
    netsh.exe winhttp set proxy $Server '<local>' | Out-Null; `
    $Bypass = '<local>'; `
    $Settings = [byte[]](@(0x46, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0) + `
      [BitConverter]::GetBytes($Server.Length) + [Text.Encoding]::ASCII.GetBytes($Server) + `
      [BitConverter]::GetBytes($Bypass.Length) + [Text.Encoding]::ASCII.GetBytes($Bypass) + `
      @(0) * 36); `
    foreach ($Root in 'HKCU:', 'HKLM:') { `
      $Key = $Root + '\Software\Microsoft\Windows\CurrentVersion\Internet Settings\Connections'; `
      New-Item $Key -Force | Out-Null; `
      Set-ItemProperty $Key DefaultConnectionSettings $Settings `
    }

# Visual Studio 2026 Build Tools. VC.Tools.* install the latest MSVC toolset,
# which is deliberately not pinned. For Visual Studio 2022, pass
# --build-arg VS_BUILDTOOLS_URL=https://aka.ms/vs/17/release/vs_buildtools.exe.
ARG VS_BUILDTOOLS_URL=https://aka.ms/vs/18/stable/vs_buildtools.exe
COPY windows-docker/install-vs-buildtools.ps1 C:/Temp/
RUN & C:\Temp\install-vs-buildtools.ps1 -URL $env:VS_BUILDTOOLS_URL -InstallPath C:\BuildTools -Components @( `
      'Microsoft.VisualStudio.Component.VC.Tools.x86.x64', `
      'Microsoft.VisualStudio.Component.VC.Tools.ARM64', `
      'Microsoft.VisualStudio.Component.VC.ATL', `
      'Microsoft.VisualStudio.Component.VC.ATL.ARM64', `
      'Microsoft.VisualStudio.Component.Windows11SDK.26100', `
      'Microsoft.VisualStudio.Component.VC.CMake.Project'); `
    Remove-Item C:\Temp\install-vs-buildtools.ps1; `
    regsvr32.exe /s 'C:\BuildTools\DIA SDK\bin\amd64\msdia140.dll'

# Git for Windows. build.ps1 also puts its usr\bin on PATH for the Unix tools
# that tests use.
ARG GIT_URL=https://github.com/git-for-windows/git/releases/download/v2.56.0.windows.2/Git-2.56.0.2-64-bit.exe
ARG GIT_SHA256=52188F917B378F00C70EC136BCF090005F30D44FBC4EBA0BCE759CC6592D60F6
RUN Invoke-WebRequest $env:GIT_URL -OutFile C:\git.exe -UseBasicParsing; `
    if ((Get-FileHash C:\git.exe).Hash -ne $env:GIT_SHA256) { throw ('SHA256 mismatch for ' + $env:GIT_URL) }; `
    $Process = Start-Process C:\git.exe -Wait -PassThru -ArgumentList '/VERYSILENT', '/NORESTART', '/NOCANCEL', '/SP-', '/SUPPRESSMSGBOXES'; `
    if ($Process.ExitCode -ne 0) { throw ('Git installer exited with code ' + $Process.ExitCode) }; `
    Remove-Item C:\git.exe; `
    $Git = 'C:\Program Files\Git\cmd\git.exe'; `
    & $Git config --system core.symlinks true; `
    & $Git config --system core.autocrlf false; `
    & $Git config --system core.longpaths true; `
    & $Git config --system safe.directory '*'

# Python for update-checkout. build.ps1 uses its own Python from the artifact
# cache.
ARG PYTHON_URL=https://www.python.org/ftp/python/3.13.16/python-3.13.16-amd64.exe
ARG PYTHON_SHA256=FB4F9F5D438B2396DA0086DC70B935C530CB578E37ADC6D354F7AD2037FEE83B
RUN Invoke-WebRequest $env:PYTHON_URL -OutFile C:\python.exe -UseBasicParsing; `
    if ((Get-FileHash C:\python.exe).Hash -ne $env:PYTHON_SHA256) { throw ('SHA256 mismatch for ' + $env:PYTHON_URL) }; `
    $Process = Start-Process C:\python.exe -Wait -PassThru -ArgumentList '/quiet', 'InstallAllUsers=1', 'PrependPath=1', 'Include_test=0'; `
    if ($Process.ExitCode -ne 0) { throw ('Python installer exited with code ' + $Process.ExitCode) }; `
    Remove-Item C:\python.exe

# nuget.exe, which build.ps1 uses when -WinSDKVersion names an SDK that is not
# installed.
ARG NUGET_URL=https://dist.nuget.org/win-x86-commandline/v7.9.0/nuget.exe
ARG NUGET_SHA256=992D70CAC5B06C38EFEC91806CABA64CDCC07E6D963A0959DBBBAF264D33B800
RUN New-Item -ItemType Directory C:\Tools | Out-Null; `
    Invoke-WebRequest $env:NUGET_URL -OutFile C:\Tools\nuget.exe -UseBasicParsing; `
    if ((Get-FileHash C:\Tools\nuget.exe).Hash -ne $env:NUGET_SHA256) { throw ('SHA256 mismatch for ' + $env:NUGET_URL) }; `
    [Environment]::SetEnvironmentVariable('PATH', [Environment]::GetEnvironmentVariable('PATH', 'Machine') + ';C:\Tools', 'Machine')

RUN New-ItemProperty -Path HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem -Name LongPathsEnabled -Value 1 -PropertyType DWORD -Force | Out-Null

# Install build.ps1's dependencies. The archives are deleted afterwards, which
# works because build-dependencies.ps1 only installs what fails its probe; the
# second run checks that nothing needs to be installed again.
#
# Docker commits a Windows layer by exporting and re-importing every file in it,
# which takes tens of minutes for a layer of several GB. The default arguments
# therefore only install what build-windows-toolchain.bat uses; add -Android
# (Android NDK) or -IncludeDS2 (flex/bison) for builds that need them.
ARG BUILD_DEPENDENCIES_ARGS="-Test lld,lldb,lldb-swift,swift,dispatch,foundation,xctest,swift-format,sourcekit-lsp -IncludeSBoM"
COPY build.ps1 build-dependencies.ps1 C:/Temp/swift/utils/
RUN $Command = '& C:\Temp\swift\utils\build-dependencies.ps1 -ArtifactCache C:\ArtifactCache -BinaryCache C:\Temp\BinaryCache ' + $env:BUILD_DEPENDENCIES_ARGS; `
    Invoke-Expression $Command; `
    if ($LastExitCode -ne 0) { exit $LastExitCode }; `
    Remove-Item C:\ArtifactCache\* -Include *.zip, *.exe -Force; `
    Invoke-Expression $Command; `
    if ($LastExitCode -ne 0) { exit $LastExitCode }; `
    $Downloaded = @(Get-ChildItem C:\ArtifactCache -File); `
    if ($Downloaded) { throw ('Dependencies were installed again: ' + ($Downloaded.Name -join ', ')) }; `
    Remove-Item C:\Temp -Recurse -Force; `
    Write-Host ('[{0:yyyy-MM-dd HH:mm:ss}] Dependencies installed. Committing the layer, which takes a while.' -f (Get-Date))

# Remove the proxy settings, which only exist if HTTPS_PROXY is set. A failed
# Remove-ItemProperty fails the step even with -ErrorAction Ignore, because
# powershell.exe -Command exits with 1 when the last command failed.
RUN if (-not $env:HTTPS_PROXY) { exit 0 }; `
    netsh.exe winhttp reset proxy | Out-Null; `
    foreach ($Root in 'HKCU:', 'HKLM:') { `
      Remove-ItemProperty ($Root + '\Software\Microsoft\Windows\CurrentVersion\Internet Settings\Connections') DefaultConnectionSettings `
    }

ENV ARTIFACT_CACHE=C:\ArtifactCache
