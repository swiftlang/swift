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
ARG PYTHON=https://www.python.org/ftp/python/3.13.16/python-3.13.16-amd64.exe
ARG PYTHON_SHA256=FB4F9F5D438B2396DA0086DC70B935C530CB578E37ADC6D354F7AD2037FEE83B
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

# Install .NET Framework 4.8, which the Visual Studio installer needs.
# Windows Server 2019 images only have 4.7.2, and the Visual Studio installer
# would install 4.8 itself, which requires a reboot (3010). Installing it in its
# own layer takes care of the reboot.
ARG NDP48=https://download.visualstudio.microsoft.com/download/pr/2d6bb6b2-226a-4baa-bdec-798822606ff1/8494001c276a4b96804cde7829c04d7f/ndp48-x86-x64-allos-enu.exe
ARG NDP48_SHA256=68C9986A8DCC0214D909AA1F31BEE9FB5461BB839EDCA996A75B08DDFFC1483F
RUN $Release = (Get-ItemProperty 'HKLM:\SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full').Release; \
    if ($Release -ge 528040) {                                                  \
      Write-Host ('.NET Framework 4.8 or later is installed ({0})' -f $Release); \
      exit 0;                                                                   \
    }                                                                           \
    Write-Host -NoNewLine ('Downloading {0} ... ' -f ${env:NDP48});             \
    Invoke-WebRequest -Uri ${env:NDP48} -OutFile ndp48.exe -UseBasicParsing;    \
    Write-Host '✓';                                                             \
    Write-Host -NoNewLine ('Verifying SHA256 ({0}) ... ' -f ${env:NDP48_SHA256}); \
    $Hash = Get-FileHash ndp48.exe -Algorithm sha256;                           \
    if ($Hash.Hash -eq ${env:NDP48_SHA256}) {                                   \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Hash.Hash);                                     \
      exit 1;                                                                   \
    }                                                                           \
    Write-Host -NoNewLine 'Installing .NET Framework 4.8 ... ';                 \
    $Process =                                                                  \
        Start-Process ndp48.exe -Wait -PassThru -NoNewWindow -ArgumentList @(   \
          '/q',                                                                 \
          '/norestart'                                                          \
        );                                                                      \
    if ($Process.ExitCode -eq 0 -or $Process.ExitCode -eq 3010) {               \
      Write-Host ('✓ ({0})' -f $Process.ExitCode);                              \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Process.ExitCode);                              \
      exit 1;                                                                   \
    }                                                                           \
    Remove-Item -Force ndp48.exe;                                               \
    Remove-Item -ErrorAction SilentlyContinue -Force -Recurse ${env:TEMP}\*

# Install Visual Studio 2022 Build Tools
# Fixed-version bootstrapper for 17.14.42 (17.14.37806.7), see
# https://learn.microsoft.com/en-us/visualstudio/releases/2022/release-history
# The Visual Studio 2026 installer fails with 0x8007007E (module not found) on
# Windows Server 2019 base images.
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
    $Process =                                                                  \
        Start-Process vs_buildtools.exe -Wait -PassThru -NoNewWindow -ArgumentList @( \
          '--quiet',                                                            \
          '--wait',                                                             \
          '--norestart',                                                        \
          '--nocache',                                                          \
          '--installPath', 'C:\BuildTools',                                     \
          '--add', 'Microsoft.VisualStudio.Component.Windows11SDK.26100',       \
          '--add', 'Microsoft.VisualStudio.Component.VC.Tools.x86.x64',         \
          '--add', 'Microsoft.VisualStudio.Component.VC.Tools.ARM64',           \
          '--add', 'Microsoft.VisualStudio.Component.VC.ATL',                   \
          '--add', 'Microsoft.VisualStudio.Component.VC.ATL.ARM64',             \
          '--add', 'Microsoft.VisualStudio.Component.VC.CMake.Project',         \
          '--add', 'Microsoft.NetCore.Component.SDK'                            \
        );                                                                      \
    $DIA = 'C:\BuildTools\DIA SDK\bin\amd64\msdia140.dll';                      \
    $Installed = ($Process.ExitCode -eq 0 -or $Process.ExitCode -eq 3010) -and  \
                 (Test-Path C:\BuildTools\VC\Tools\MSVC\*\bin\Hostx64\x64\cl.exe) -and \
                 (Test-Path $DIA);                                              \
    if ($Installed) {                                                           \
      Write-Host ('✓ ({0})' -f $Process.ExitCode);                              \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Process.ExitCode);                              \
      foreach ($Log in Get-ChildItem ${env:TEMP} -Filter 'dd_*.log' | Sort-Object LastWriteTime) { \
        if ($Log.Name -match '_errors\.log$|^dd_bootstrapper|^dd_client|^dd_setup_\d+\.log$') { \
          Write-Host ('===== {0} (last 40 lines)' -f $Log.Name);                \
          Get-Content $Log.FullName -Tail 40 | Write-Host;                      \
        }                                                                       \
      }                                                                         \
      exit 1;                                                                   \
    }                                                                           \
    Write-Host -NoNewLine 'Registering msdia140.dll ... ';                      \
    $Process =                                                                  \
        Start-Process regsvr32.exe -Wait -PassThru -NoNewWindow -ArgumentList @( \
          '/s',                                                                 \
          ('"{0}"' -f $DIA)                                                     \
        );                                                                      \
    if ($Process.ExitCode -eq 0) {                                              \
      Write-Host '✓';                                                           \
    } else {                                                                    \
      Write-Host ('✘ ({0})' -f $Process.ExitCode);                              \
      exit 1;                                                                   \
    }                                                                           \
    Remove-Item -Force vs_buildtools.exe;                                       \
    Remove-Item -ErrorAction SilentlyContinue -Force -Recurse "${env:ProgramData}\Package Cache"; \
    Remove-Item -ErrorAction SilentlyContinue -Force -Recurse ${env:TEMP}\*

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
