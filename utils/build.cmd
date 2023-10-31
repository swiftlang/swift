@echo off
setlocal
if "%1"=="/?" (
    powershell.exe -Command "& Get-Help -Detailed .\build.ps1"
    exit /b
)

powershell.exe -ExecutionPolicy RemoteSigned -File "%~dp0\build.ps1" %*
endlocal
