@ echo off
IF NOT DEFINED PYTHON SET PYTHON=python
"%PYTHON%" "%~dp0\mock-distcc" %*
