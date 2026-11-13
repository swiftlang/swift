@echo off
setlocal
rem Runs tests, given by path or by test suite, with TestToolchain.ps1.
if "%1"=="/?" (
    powershell.exe -Command "& Get-Help -Detailed ""%~dp0\TestToolchain.ps1"""
    exit /b
)

powershell.exe -ExecutionPolicy RemoteSigned -File "%~dp0\TestToolchain.ps1" %*
exit /b %ERRORLEVEL%
