@echo oN
rem
rem $Id: build.bat $
rem

:BUILD_IDE

   cls

   pushd "%~dp0"
   set HG_START_DP_IDE_BUILD_BAT=%CD%
   popd

   set HG_CLEAN=
   if /I not "%1" == "/C" goto ROOT
   shift
   set HG_CLEAN=/C
   set HG_ROOT=

:ROOT

   if not "%HG_ROOT%" == "" goto TEST
   pushd "%HG_START_DP_IDE_BUILD_BAT%\.."
   set HG_ROOT=%CD%
   popd

:TEST

   if /I "%1"=="HM30" ( shift & goto CALL30 )
   if /I "%1"=="HM32" ( shift & goto CALL32 )
   if /I "%1"=="HM34" ( shift & goto CALL34 )

:NOVERSION

   if exist "%HG_ROOT%\BuildApp30.bat" goto CALL30
   if exist "%HG_ROOT%\BuildApp32.bat" goto CALL32
   if exist "%HG_ROOT%\BuildApp34.bat" goto CALL34

:VERSIONNEEDED
   echo Syntax:
   echo    To build with Harbour 3.0 and MinGW
   echo       build [/C] HM30 [options]
   echo   To build with Harbour 3.2 and MinGW
   echo       build [/C] HM32 [options]
   echo   To build with Harbour 3.4 and MinGW
   echo       build [/C] HM34 [options]
   echo.
   goto END

:CALL30

   call "%HG_ROOT%\BuildApp.bat" %HG_CLEAN% HM30 mgide %1 %2 %3 %4 %5 %6 %7 %8 %9
   goto END

:CALL32

   call "%HG_ROOT%\BuildApp.bat" %HG_CLEAN% HM32 mgide  %1 %2 %3 %4 %5 %6 %7 %8 %9
   goto END

:CALL34
   call "%HG_ROOT%\BuildApp.bat" %HG_CLEAN% HM34 mgide  %1 %2 %3 %4 %5 %6 %7 %8 %9
   goto END

:END

   set HG_CLEAN=
   set HG_START_DP_IDE_BUILD_BAT=
