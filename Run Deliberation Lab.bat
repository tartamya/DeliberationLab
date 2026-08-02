@echo off
setlocal enableextensions
title Deliberation Lab
cd /d "%~dp0"

rem --- Dedicated temp dir so R never trips over a Windows temp cleaner ---
set "TMPDIR=C:\Rtemp"
if not exist "%TMPDIR%" mkdir "%TMPDIR%" >nul 2>&1

rem --- Find Rscript.exe (your installed R) ---
set "RSCRIPT=C:\Program Files\R\R-4.4.3\bin\x64\Rscript.exe"
if not exist "%RSCRIPT%" (
  for /f "delims=" %%R in ('where Rscript 2^>nul') do set "RSCRIPT=%%R"
)
if not exist "%RSCRIPT%" (
  echo.
  echo   ERROR: Could not find Rscript.exe.
  echo   Install R from https://cran.r-project.org/ , or edit this file
  echo   and set RSCRIPT to the full path of your Rscript.exe.
  echo.
  pause
  exit /b 1
)

echo.
echo   ============================================================
echo      DELIBERATION LAB  -  starting up
echo   ============================================================
echo.
echo   First load takes ~10-20 seconds; a browser tab opens by itself.
echo   KEEP THIS WINDOW OPEN.  Close it (or Ctrl+C) to stop the app.
echo.

"%RSCRIPT%" --vanilla "%~dp0run_local.R"

echo.
echo   The app has stopped. You can close this window.
pause
