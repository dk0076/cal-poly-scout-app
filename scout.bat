@echo off
rem ---------------------------------------------------------------------------
rem  Cal Poly Scout App. Double-click this.
rem
rem  R's installer does not put Rscript on the PATH, so typing "Rscript run.R"
rem  at a prompt fails on a machine where R is plainly installed. This finds it
rem  rather than assuming: the registry key the installer writes first, then
rem  the usual folder, then the PATH in case someone added it.
rem
rem  Pass a port to use a different one:  scout.bat 8080
rem ---------------------------------------------------------------------------
setlocal
cd /d "%~dp0"

set "RSCRIPT="

for /f "skip=2 tokens=2,*" %%A in ('reg query "HKLM\SOFTWARE\R-core\R" /v InstallPath 2^>nul') do set "RHOME=%%B"
if defined RHOME if exist "%RHOME%\bin\Rscript.exe" set "RSCRIPT=%RHOME%\bin\Rscript.exe"

if not defined RSCRIPT for /d %%D in ("%ProgramFiles%\R\R-*") do (
  if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
)

if not defined RSCRIPT (
  where Rscript >nul 2>&1 && set "RSCRIPT=Rscript"
)

if not defined RSCRIPT (
  echo.
  echo   Could not find R on this computer.
  echo   Install it from cran.r-project.org, then double-click this again.
  echo.
  pause
  exit /b 1
)

"%RSCRIPT%" run.R %*

echo.
echo   The app has stopped. This window can be closed.
pause
