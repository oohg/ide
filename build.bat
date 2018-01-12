@echo off
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

   if /I "%1"=="HB30" ( shift & goto CALL30 )
   if /I "%1"=="HB32" ( shift & goto CALL32 )

:NOVERSION

   if exist "%HG_ROOT%\BuildApp30.bat" goto CONTINUE
   if exist "%HG_ROOT%\BuildApp32.bat" goto CALL32
   echo File %HG_ROOT%\BuildApp30.bat not found !!!
   echo File %HG_ROOT%\BuildApp32.bat not found !!!
   echo.
   goto END

:CONTINUE

   if not exist "%HG_ROOT%\BuildApp32.bat" goto CALL30
   echo Syntax:
   echo    To build with Harbour 3.0
   echo       build [/C] HB30 [options]
   echo   To build with Harbour 3.2
   echo       build [/C] HB32 [options]
   echo.
   goto END

:CALL30

   call "%HG_ROOT%\BuildApp.bat" %HG_CLEAN% HB30 mgide %1 %2 %3 %4 %5 %6 %7 %8 %9
   goto END

:CALL32

   call "%HG_ROOT%\BuildApp.bat" %HG_CLEAN% HB32 mgide  %1 %2 %3 %4 %5 %6 %7 %8 %9
   goto END

:END

   set HG_CLEAN=
   set HG_START_DP_IDE_BUILD_BAT=
