:: build-windows-toolchain.bat
::
:: This source file is part of the Swift.org open source project
::
:: Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
:: Licensed under Apache License v2.0 with Runtime Library Exception
::
:: See https://swift.org/LICENSE.txt for license information
:: See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

setlocal enableextensions enabledelayedexpansion
path %PATH%;%PYTHON_HOME%

:: Identify the SourceRoot
:: Normalize the SourceRoot to make it easier to read the output.
cd %~dp0\..\..
set SourceRoot=%CD%

:: Identify the BuildRoot
set BuildRoot=%SourceRoot%\build

:: Identify the PackageRoot
set PackageRoot=%BuildRoot%\package
md %PackageRoot%

:: Setup temporary directories
md %BuildRoot%\tmp
set TEMP=%BuildRoot%\tmp
set TMP=%BuildRoot%\tmp
set TMPDIR=%BuildRoot%\tmp

set NINJA_STATUS=[%%f/%%t][%%p][%%es] 

:: Build the -Test argument, if any
set TestArg=-Test swift,dispatch,foundation,xctest,
for %%I in (%SKIP_TESTS%) do (call set TestArg=%%TestArg:%%I,=%%)
if "%TestArg:~-1%"=="," (set TestArg=%TestArg:~0,-1%) else (set TestArg= )

:: Build the -SkipPackaging argument, if any
set SkipPackagingArg=-SkipPackaging
if not "%SKIP_PACKAGING%"=="1" set "SkipPackagingArg= "

call :CloneDependencies || (exit /b)
call :CloneRepositories || (exit /b)

powershell.exe -ExecutionPolicy RemoteSigned -Command %~dp0\build.ps1 ^
  -SourceCache %SourceRoot% ^
  -BinaryCache %BuildRoot% ^
  -BuildType %CMAKE_BUILD_TYPE% ^
  %SkipPackagingArg% ^
  %TestArg% ^
  -Stage "%BuildRoot%\package"

:: Clean up the module cache
rd /s /q %LocalAppData%\clang\ModuleCache

goto :end
endlocal

:CloneRepositories
setlocal enableextensions enabledelayedexpansion

if defined SKIP_UPDATE_CHECKOUT goto :eof

if defined REPO_SCHEME set "args=--scheme %REPO_SCHEME%"

:: Always enable symbolic links
git config --global core.symlink true

:: Ensure that we have the files in the original line endings, the swift tests
:: depend on this being the case.
rem git -C "%SourceRoot%\swift" config --local core.autocrlf input
rem git -C "%SourceRoot%\swift" checkout-index --force --all

set "args=%args% --skip-repository swift"
set "args=%args% --skip-repository ninja"
set "args=%args% --skip-repository icu"
set "args=%args% --skip-repository swift-integration-tests"
set "args=%args% --skip-repository swift-stress-tester"
set "args=%args% --skip-repository swift-xcode-playground-support"

call "%SourceRoot%\swift\utils\update-checkout.cmd" %args% --clone --skip-history --github-comment "%ghprbCommentBody%"

goto :eof
endlocal

:CloneDependencies
setlocal enableextensions enabledelayedexpansion

:: Always enable symbolic links
git config --global core.symlink true

:: FIXME(compnerd) avoid the fresh clone
rd /s /q zlib libxml2 sqlite icu curl

git clone --quiet --no-tags --depth 1 --branch v1.2.11 https://github.com/madler/zlib
git clone --quiet --no-tags --depth 1 --branch v2.9.12 https://github.com/gnome/libxml2
git clone --quiet --no-tags --depth 1 --branch version-3.36.0 https://github.com/sqlite/sqlite
git clone --quiet --no-tags --depth 1 --branch maint/maint-69 https://github.com/unicode-org/icu
git clone --quiet --no-tags --depth 1 --branch curl-7_77_0 https://github.com/curl/curl

goto :eof
endlocal

:end
