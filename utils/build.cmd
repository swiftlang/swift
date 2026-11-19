@echo off
setlocal
if "%1"=="/?" (
    powershell.exe -Command "& Get-Help -Detailed ""%~dp0\BuildSequential.ps1"""
    exit /b
)

powershell.exe -ExecutionPolicy RemoteSigned -File "%~dp0\BuildSequential.ps1" %*
endlocal
