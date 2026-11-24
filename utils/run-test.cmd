@echo off
setlocal

rem Prefer the Python that build.ps1 downloads into the artifact cache, falling
rem back to whatever "python" is on the PATH.  The cache location can be
rem overridden with SWIFT_ARTIFACT_CACHE, to match build.ps1's -ArtifactCache.
if not defined SWIFT_ARTIFACT_CACHE set "SWIFT_ARTIFACT_CACHE=S:\ArtifactCache"

set "PYTHON=python"
for /d %%D in ("%SWIFT_ARTIFACT_CACHE%\Python%PROCESSOR_ARCHITECTURE%-*") do (
    if exist "%%D\tools\python.exe" set "PYTHON=%%D\tools\python.exe"
)

rem The tests need the Visual Studio environment (LIB, INCLUDE, etc.) to link,
rem just as build.ps1 sets one up with Invoke-VsDevShell.
if defined VSCMD_VER goto have_vs
set "VSARCH=amd64"
if /i "%PROCESSOR_ARCHITECTURE%"=="ARM64" set "VSARCH=arm64"
set "VSWHERE=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
for /f "usebackq delims=" %%I in (`"%VSWHERE%" -nologo -latest -products * -all -prerelease -property installationPath`) do set "VSINSTALL=%%I"
if not defined VSINSTALL (
    echo run-test: could not find a Visual Studio installation 1>&2
    exit /b 1
)
call "%VSINSTALL%\Common7\Tools\VsDevCmd.bat" -no_logo -host_arch=%VSARCH% -arch=%VSARCH% || exit /b 1
:have_vs

rem Like build.ps1, don't let an installed toolchain's SDK leak into the build.
set "SDKROOT="

"%PYTHON%" "%~dp0\run-test" %*
exit /b %ERRORLEVEL%
