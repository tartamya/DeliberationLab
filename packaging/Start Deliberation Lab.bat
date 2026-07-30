@echo off
setlocal
title Deliberation Lab
cd /d "%~dp0"

echo(
echo   ============================================================
echo      DELIBERATION LAB
echo      Multi-LLM Deliberation Laboratory
echo   ============================================================
echo(
echo   Starting up. The first launch takes about 10-20 seconds
echo   while the engine loads - your web browser will open by itself.
echo(
echo   KEEP THIS WINDOW OPEN while you use the app.
echo   Close this window (or press Ctrl+C) to stop the app.
echo(

rem --- locate Rscript.exe inside the bundled R-Portable (layout-agnostic) ---
set "RSCRIPT="
for /r "%~dp0R-Portable" %%R in (Rscript.exe) do if not defined RSCRIPT set "RSCRIPT=%%R"
if not defined RSCRIPT (
  echo   ERROR: Could not find Rscript.exe under the R-Portable folder.
  echo   Make sure R-Portable is extracted next to this file.
  echo(
  pause
  exit /b 1
)

"%RSCRIPT%" "%~dp0launch.R"

echo(
echo   The app has stopped. You can close this window.
pause
