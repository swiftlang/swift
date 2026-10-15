# With process isolation, the base image must match the host's Windows build:
# build-windows-toolchain.bat passes --build-arg BASE_IMAGE=mcr.microsoft.com/windows/servercore:10.0.<build>.<ubr>.
ARG BASE_IMAGE=mcr.microsoft.com/windows/servercore:ltsc2022
FROM ${BASE_IMAGE} AS windows

LABEL maintainer="Swift Infrastructure <swift-infrastructure@forums.swift.org>"
LABEL description="Docker Container for the Swift programming language"

SHELL ["powershell", "-Command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue';"]

ENV PYTHONIOENCODING UTF-8
ENV PYTHONUTF8 1

# Windows components (.NET, the Visual Studio installer, certificate
# revocation checks) read the WinINet and WinHTTP proxy settings, not
# HTTPS_PROXY. Pass --build-arg HTTPS_PROXY=http://<host>:<port> to set them.
RUN if (-not ${env:HTTPS_PROXY}) { exit 0 };                                    \
    $Server = ([Uri]${env:HTTPS_PROXY}).Authority;                              \
    netsh.exe winhttp set proxy $Server '<local>' | Out-Null;                   \
    $Bypass = '<local>';                                                        \
    $Settings = [byte[]](@(0x46, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0) +             \
      [BitConverter]::GetBytes($Server.Length) +                                \
      [Text.Encoding]::ASCII.GetBytes($Server) +                                \
      [BitConverter]::GetBytes($Bypass.Length) +                                \
      [Text.Encoding]::ASCII.GetBytes($Bypass) +                                \
      @(0) * 36);                                                               \
    foreach ($Root in 'HKCU:', 'HKLM:') {                                       \
      $Key = $Root + '\Software\Microsoft\Windows\CurrentVersion\Internet Settings\Connections'; \
      New-Item $Key -Force | Out-Null;                                          \
      Set-ItemProperty $Key DefaultConnectionSettings $Settings;                \
    }

# Enable Developer Mode.
RUN Write-Host -NoNewLine "Enabling Developer Mode";                            \
    $Process =                                                                  \
        Start-Process reg.exe -Wait -PassThru -NoNewWindow -ArgumentList @(     \
          'add',                                                                \
          'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock', \
          '/f',                                                                 \
          '/t', 'REG_DWORD',                                                    \
          '/v', 'AllowDevelopmentWithoutDevLicense',                            \
          '/d', '1'                                                             \
        );                                                                      \
    if ($Process.ExitCode -eq 0) {                                              \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Process.ExitCode);                              \
      exit 1;                                                                   \
    }

# Enable Long Paths
# RUN Write-Host -NoNewLine "Enabling Long Paths";                                \
#     $Process =                                                                  \
#         Start-Process reg.exe -Wait -PassThru -NoNewWindow -ArgumentList @(     \
#           'add',                                                                \
#           'HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\FileSystem',     \
#           '/f',                                                                 \
#           '/t', 'REG_DWORD',                                                    \
#           '/v', 'LongPathsEnabled',                                             \
#           '/d' '1'                                                              \
#         );                                                                      \
#     if ($Process.ExitCode -eq 0) {                                              \
#       Write-Host '✓';                                                           \
#     } else {                                                                    \
#       Write-Host ('✘ ({0})' -f $Process.ExitCode);                              \
#       exit 1;                                                                   \
#     }

# Install Git.
# See: git-[version]-[bit].exe /SAVEINF=git.inf and /?
ARG GIT=https://github.com/git-for-windows/git/releases/download/v2.56.0.windows.2/Git-2.56.0.2-64-bit.exe
ARG GIT_SHA256=52188F917B378F00C70EC136BCF090005F30D44FBC4EBA0BCE759CC6592D60F6
RUN Write-Host -NoNewLine ('Downloading {0} ... ' -f ${env:GIT});               \
    Invoke-WebRequest -Uri ${env:GIT} -OutFile git.exe;                         \
    Write-Host '✓';                                                             \
    Write-Host -NoNewLine ('Verifying SHA256 ({0}) ... ' -f ${env:GIT_SHA256}); \
    $Hash = Get-FileHash git.exe -Algorithm sha256;                             \
    if ($Hash.Hash -eq ${env:GIT_SHA256}) {                                     \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Hash.Hash);                                     \
      exit 1;                                                                   \
    }                                                                           \
    Write-Host -NoNewLine 'Installing git ... ';                                \
    $Process =                                                                  \
        Start-Process git.exe -Wait -PassThru -NoNewWindow -ArgumentList @(     \
          '/SP-',                                                               \
          '/VERYSILENT',                                                        \
          '/SUPPRESSMSGBOXES',                                                  \
          '/NOCANCEL',                                                          \
          '/NORESTART',                                                         \
          '/CLOSEAPPLICATIONS',                                                 \
          '/FORCECLOSEAPPLICATIONS',                                            \
          '/NOICONS',                                                           \
          '/COMPONENTS="gitlfs"',                                               \
          '/EditorOption=VIM',                                                  \
          '/PathOption=Cmd',                                                    \
          '/SSHOption=OpenSSH',                                                 \
          '/CURLOption=WinSSL',                                                 \
          '/UseCredentialManager=Enabled',                                      \
          '/EnableSymlinks=Enabled',                                            \
          '/EnableFSMonitor=Enabled'                                            \
        );                                                                      \
    if ($Process.ExitCode -eq 0) {                                              \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Process.ExitCode);                              \
      exit 1;                                                                   \
    }                                                                           \
    Remove-Item -Force git.exe;                                                 \
    $Git = 'C:\Program Files\Git\cmd\git.exe';                                  \
    & $Git config --system core.symlinks true;                                  \
    & $Git config --system core.autocrlf false;                                 \
    & $Git config --system core.longpaths true;                                 \
    & $Git config --system safe.directory '*';                                  \
    Remove-Item -ErrorAction SilentlyContinue -Force -Recurse ${env:TEMP}\*

# Install Python
ARG PYTHON=https://www.python.org/ftp/python/3.10.11/python-3.10.11-amd64.exe
ARG PYTHON_SHA256=D8DEDE5005564B408BA50317108B765ED9C3C510342A598F9FD42681CBE0648B
RUN Write-Host -NoNewLine ('Downloading {0} ... ' -f ${env:PYTHON});            \
    Invoke-WebRequest -Uri ${env:PYTHON} -OutFile python-amd64.exe;             \
    Write-Host '✓';                                                             \
    Write-Host -NoNewLine ('Verifying SHA256 ({0}) ... ' -f ${env:PYTHON_SHA256}); \
    $Hash = Get-FileHash python-amd64.exe -Algorithm sha256;                    \
    if ($Hash.Hash -eq ${env:PYTHON_SHA256}) {                                  \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Hash.Hash);                                     \
      exit 1;                                                                   \
    }                                                                           \
    Write-Host -NoNewLine 'Installing Python ... ';                             \
    $Process =                                                                  \
        Start-Process python-amd64.exe -Wait -PassThru -NoNewWindow -ArgumentList @( \
           'AssociateFiles=0',                                                  \
           'Include_doc=0',                                                     \
           'Include_debug=0',                                                   \
           'Include_dev=1',                                                     \
           'Include_exe=1',                                                     \
           'Include_lib=1',                                                     \
           'Include_tcltk=0',                                                   \
           'Include_test=0',                                                    \
           'InstallAllUsers=1',                                                 \
           'InstallLauncherAllUsers=0',                                         \
           'PrependPath=1',                                                     \
           '/quiet'                                                             \
         );                                                                     \
    if ($Process.ExitCode -eq 0) {                                              \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Process.ExitCode);                              \
      exit 1;                                                                   \
    }                                                                           \
    Remove-Item -Force python-amd64.exe;                                        \
    Remove-Item -ErrorAction SilentlyContinue -Force -Recurse ${env:TEMP}\*

# Install Visual Studio 2022 Build Tools
# Fixed-version bootstrapper for 17.14.42 (17.14.37806.7), see
# https://learn.microsoft.com/en-us/visualstudio/releases/2022/release-history
ARG VSB=https://download.visualstudio.microsoft.com/download/pr/c95450be-0a45-4779-b142-33f567ccfd6e/68978bb28916156802e60e04c5ee50b94348d802fe151a3ca24433f27984187b/vs_BuildTools.exe
ARG VSB_SHA256=68978BB28916156802E60E04C5EE50B94348D802FE151A3CA24433F27984187B
RUN Write-Host -NoNewLine ('Downloading {0} ... ' -f ${env:VSB});               \
    Invoke-WebRequest -Uri ${env:VSB} -OutFile vs_buildtools.exe -UseBasicParsing -TimeoutSec 300; \
    Write-Host '✓';                                                             \
    Write-Host -NoNewLine ('Verifying SHA256 ({0}) ... ' -f ${env:VSB_SHA256}); \
    $Hash = Get-FileHash vs_buildtools.exe -Algorithm sha256;                   \
    if ($Hash.Hash -eq ${env:VSB_SHA256}) {                                     \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Hash.Hash);                                     \
      exit 1;                                                                   \
    }                                                                           \
    Write-Host -NoNewLine 'Installing Visual Studio Build Tools ... ';          \
    $Start = Get-Date;                                                          \
    $Process =                                                                  \
        Start-Process vs_buildtools.exe -Wait -PassThru -NoNewWindow -ArgumentList @( \
          '--quiet',                                                            \
          '--wait',                                                             \
          '--norestart',                                                        \
          '--nocache',                                                          \
          '--add', 'Microsoft.VisualStudio.Component.Windows11SDK.26100',       \
          '--add', 'Microsoft.VisualStudio.Component.VC.Tools.x86.x64',         \
          '--add', 'Microsoft.VisualStudio.Component.VC.Tools.ARM64',           \
          '--add', 'Microsoft.VisualStudio.Component.VC.ATL',                   \
          '--add', 'Microsoft.VisualStudio.Component.VC.ATL.ARM64',             \
          '--add', 'Microsoft.NetCore.Component.SDK'                            \
        );                                                                      \
    $VSWhere = Join-Path ${env:ProgramFiles(x86)} 'Microsoft Visual Studio\Installer\vswhere.exe'; \
    $VSInstallRoot =                                                            \
        & $VSWhere -nologo -products * -property installationPath               \
            -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64;        \
    if (($Process.ExitCode -eq 0 -or $Process.ExitCode -eq 3010) -and $VSInstallRoot) { \
      Write-Host ('✓ ({0})' -f $Process.ExitCode);                              \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Process.ExitCode);                              \
      foreach ($Log in Get-ChildItem ${env:TEMP} -Recurse -Filter 'dd_*_errors.log' | Sort-Object LastWriteTime) { \
        Write-Host ('===== {0}' -f $Log.FullName);                              \
        Get-Content $Log.FullName | Write-Host;                                 \
      }                                                                         \
      Write-Host '===== Application event log errors';                          \
      Get-WinEvent -ErrorAction SilentlyContinue -FilterHashtable @{ LogName = 'Application'; Level = 2; StartTime = $Start } | \
        Format-List TimeCreated, ProviderName, Message | Out-String | Write-Host; \
      exit 1;                                                                   \
    }                                                                           \
    Write-Host -NoNewLine 'Registering msdia140.dll ... ';                      \
    $Process =                                                                  \
        Start-Process regsvr32.exe -Wait -PassThru -NoNewWindow -ArgumentList @( \
          '/s',                                                                 \
          ('{0}{1}\DIA SDK\bin\amd64\msdia140.dll{0}' -f [char]34, $VSInstallRoot) \
        );                                                                      \
    if ($Process.ExitCode -eq 0) {                                              \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Process.ExitCode);                              \
      exit 1;                                                                   \
    }                                                                           \
    Remove-Item -Force vs_buildtools.exe;                                       \
    Remove-Item -ErrorAction SilentlyContinue -Force -Recurse (Join-Path ${env:ProgramData} 'Package Cache'); \
    Remove-Item -ErrorAction SilentlyContinue -Force -Recurse ${env:TEMP}\*

# Install Ninja, which build.ps1 otherwise takes from the Visual Studio CMake
# component. build.ps1 downloads its own CMake.
ARG NINJA=https://github.com/ninja-build/ninja/releases/download/v1.13.2/ninja-win.zip
ARG NINJA_SHA256=07FC8261B42B20E71D1720B39068C2E14FFCEE6396B76FB7A795FB460B78DC65
RUN New-Item -ItemType Directory -Force C:\Tools | Out-Null;                    \
    Write-Host -NoNewLine ('Downloading {0} ... ' -f ${env:NINJA});             \
    Invoke-WebRequest -Uri ${env:NINJA} -OutFile ninja-win.zip;                 \
    Write-Host '✓';                                                             \
    Write-Host -NoNewLine ('Verifying SHA256 ({0}) ... ' -f ${env:NINJA_SHA256}); \
    $Hash = Get-FileHash ninja-win.zip -Algorithm sha256;                       \
    if ($Hash.Hash -eq ${env:NINJA_SHA256}) {                                   \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Hash.Hash);                                     \
      exit 1;                                                                   \
    }                                                                           \
    Expand-Archive -Path ninja-win.zip -DestinationPath C:\Tools;               \
    Remove-Item -Force ninja-win.zip

# Install nuget.exe, used by build.ps1 when -WinSDKVersion names an SDK that is
# not installed.
ARG NUGET=https://dist.nuget.org/win-x86-commandline/v7.9.0/nuget.exe
ARG NUGET_SHA256=992D70CAC5B06C38EFEC91806CABA64CDCC07E6D963A0959DBBBAF264D33B800
RUN New-Item -ItemType Directory -Force C:\Tools | Out-Null;                    \
    Write-Host -NoNewLine ('Downloading {0} ... ' -f ${env:NUGET});             \
    Invoke-WebRequest -Uri ${env:NUGET} -OutFile C:\Tools\nuget.exe;            \
    Write-Host '✓';                                                             \
    Write-Host -NoNewLine ('Verifying SHA256 ({0}) ... ' -f ${env:NUGET_SHA256}); \
    $Hash = Get-FileHash C:\Tools\nuget.exe -Algorithm sha256;                  \
    if ($Hash.Hash -eq ${env:NUGET_SHA256}) {                                   \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Hash.Hash);                                     \
      exit 1;                                                                   \
    }                                                                           \
    [Environment]::SetEnvironmentVariable('PATH', [Environment]::GetEnvironmentVariable('PATH', 'Machine') + ';C:\Tools', 'Machine')

# Default to powershell
# FIXME: we need to grant ContainerUser the SeCreateSymbolicLinkPrivilege
# privilege so that it can create symbolic links.
# USER ContainerUser
CMD ["powershell.exe", "-nologo", "-ExecutionPolicy", "Bypass"]
