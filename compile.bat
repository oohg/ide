@echo off
rem
rem $Id: compile.bat $
rem

:COMPILE_IDE

   pushd "%~dp0"
   set HG_START_DP_IDE_COMPILE_BAT=%CD%
   popd

   if /I not "%1%" == "/NOCLS" cls
   if /I "%1%" == "/NOCLS" shift

   if not exist mgide.rc goto ERROR1

   if /I not "%1" == "/C" goto ROOT
   shift
   set HG_ROOT=
   set HG_HRB=
   set HG_BCC=
   set LIB_GUI=
   set LIB_HRB=
   set BIN_HRB=

:ROOT

   if not "%HG_ROOT%" == "" goto CONTINUE
   pushd "%HG_START_DP_IDE_COMPILE_BAT%\.."
   set HG_ROOT=%CD%
   popd

:CONTINUE

   if "%HG_HRB%"==""   set HG_HRB=%HG_ROOT%\xhbcc
   if "%HG_BCC%"==""   set HG_BCC=c:\Borland\BCC55
   if "%LIB_GUI%"==""  set LIB_GUI=lib\xhb\bcc
   if "%LIB_HRB%"==""  set LIB_HRB=lib
   if "%BIN_HRB%"==""  set BIN_HRB=bin

:CLEAN_EXE

   if exist oide.exe del oide.exe
   if exist oide.exe goto ERROR2

:RESOURCES

   echo BRC32: Compiling resources...
   md auxdir
   cd auxdir
   xcopy "%HG_ROOT%\resources\*.*" /q > nul
   md imgs
   cd imgs
   xcopy ..\..\imgs\*.* /q > nul
   cd ..
   copy /b oohg_bcc.rc + ..\mgide.rc oide.rc /y > nul
   "%HG_BCC%\bin\brc32.exe" -r oide.rc > nul
   copy oide.res .. /y > nul
   cd ..
   rd auxdir /s /q

:COMPILE

   set HG_DEFXHB=
   if /I not "%1"=="/H" set HG_DEFXHB=-D__XHARBOUR__
   echo Harbour: Compiling sources...
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\mgide    -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n1 -w3 -gc0 -es2 -q0
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\dbucvc   -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n1 -w3 -gc0 -es2 -q0
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\formedit -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n1 -w3 -gc0 -es2 -q0
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\menued   -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n1 -w3 -gc0 -es2 -q0
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\toolbed  -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n1 -w3 -gc0 -es2 -q0
   echo BCC32: Compiling...
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; %HG_DEFXHB% mgide.c    > nul
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; %HG_DEFXHB% dbucvc.c   > nul
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; %HG_DEFXHB% formedit.c > nul
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; %HG_DEFXHB% menued.c   > nul
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; %HG_DEFXHB% toolbed.c  > nul

:LINK

   echo ILINK32: Linking... oide.exe
   echo c0w32.obj + > b32.bc
   echo mgide.obj dbucvc.obj formedit.obj menued.obj toolbed.obj, + >> b32.bc
   echo oide.exe, + >> b32.bc
   echo oide.map, + >> b32.bc
   echo %HG_ROOT%\%LIB_GUI%\oohg.lib + >> b32.bc
   for %%a in ( rtl vm gtgui lang codepage macro rdd dbfdbt dbfntx dbfcdx dbffpt common debug pp ct ) do if exist %HG_HRB%\%LIB_HRB%\%%a.lib echo %HG_HRB%\%LIB_HRB%\%%a.lib + >> b32.bc
   for %%a in ( hbrtl hbvm hblang hbcpage hbmacro hbrdd rddntx rddcdx rddfpt hbcommon hbdebug hbpp hbct hbwin ) do if exist %HG_HRB%\%LIB_HRB%\%%a.lib echo %HG_HRB%\%LIB_HRB%\%%a.lib + >> b32.bc
   for %%a in ( hbsix tip hsx pcrepos libmisc hboleaut dll socket zlib ) do if exist %HG_HRB%\%LIB_HRB%\%%a.lib echo %HG_HRB%\%LIB_HRB%\%%a.lib + >> b32.bc
   for %%a in ( bostaurus hbprinter miniprint ) do if exist %HG_ROOT%\%LIB_GUI%\%%a.lib echo %HG_ROOT%\%LIB_GUI%\%%a.lib + >> b32.bc
   echo cw32.lib + >> b32.bc
   echo msimg32.lib + >> b32.bc
   echo import32.lib, , + >> b32.bc
   echo oide.res + >> b32.bc

   "%HG_BCC%\bin\ilink32.exe" -Gn -Tpe -aa -L%HG_BCC%\lib;%HG_BCC%\lib\psdk; @b32.bc
   if exist oide.exe goto OK
   echo Build finished with ERROR !!!
   goto CLEAN

:ERROR1

   echo This file must be executed from IDE folder !!!
   goto END

:ERROR2

   echo COMPILE ERROR: Is oide.exe running ?
   goto END

:OK

   echo Build finished OK !!!

:CLEAN

   for %%a in (*.tds)  do del %%a
   for %%a in (*.c)    do del %%a
   for %%a in (*.map)  do del %%a
   for %%a in (*.obj)  do del %%a
   for %%a in (b32.bc) do del %%a
   for %%a in (*.res)  do del %%a
   set HG_DEFXHB=

:END

   set HG_START_DP_IDE_COMPILE_BAT=
   echo.
