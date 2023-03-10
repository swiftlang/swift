@echo off
setlocal
powershell.exe -ExecutionPolicy RemoteSigned -File "%~dp0\build.ps1" %*
endlocal
