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

   if not "%HG_ROOT%" == "" goto TEST
   pushd "%HG_START_DP_IDE_COMPILE_BAT%\.."
   set HG_ROOT=%CD%
   popd

:TEST

   if /I "%1"=="XB" ( shift & goto CALLXB )
   if /I "%1"=="XM" ( shift & goto CALLXM )

:DETECT_XB

   if not exist "%HG_ROOT%\compileXB.bat" goto DETECT_XM
   if exist "%HG_ROOT%\compileXM.bat" goto SYNTAX
   goto COMPILE_XB

:DETECT_XM

   if exist "%HG_ROOT%\compileXM.bat" goto COMPILE_XM

:SYNTAX

   echo Syntax:
   echo   To build with xHarbour and BCC
   echo       compile [/C] XB file [options]
   echo   To build with xHarbour and MinGW
   echo       compile [/C] XM file [options]
   echo.
   goto END

:COMPILE_XB

   if "%HG_HRB%"   == "" set HG_HRB=%HG_ROOT%\xhbcc
   if "%HG_BCC%"   == "" set HG_BCC=%HG_CCOMP%
   if "%HG_BCC%"   == "" set HG_BCC=c:\Borland\BCC55
   if "%HG_CCOMP%" == "" set HG_CCOMP=%HG_BCC%
   if "%LIB_GUI%"  == "" set LIB_GUI=lib\xhb\bcc
   if "%LIB_HRB%"  == "" set LIB_HRB=lib
   if "%BIN_HRB%"  == "" set BIN_HRB=bin

   if exist oide.exe del oide.exe
   if exist oide.exe goto ERROR2

   echo xHarbour: Compiling sources...
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\mgide    -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n -w3 -gc0 -es2 -q0
   if errorlevel 1 goto ERROR3
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\dbucvc   -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n -w3 -gc0 -es2 -q0
   if errorlevel 1 goto ERROR3
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\formedit -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n -w3 -gc0 -es2 -q0
   if errorlevel 1 goto ERROR3
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\menued   -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n -w3 -gc0 -es2 -q0
   if errorlevel 1 goto ERROR3
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\toolbed  -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n -w3 -gc0 -es2 -q0
   if errorlevel 1 goto ERROR3

   echo BCC32: Compiling sources...
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -tWM -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; mgide.c    > nul
   if errorlevel 1 goto ERROR3
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -tWM -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; dbucvc.c   > nul
   if errorlevel 1 goto ERROR3
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -tWM -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; formedit.c > nul
   if errorlevel 1 goto ERROR3
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -tWM -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; menued.c   > nul
   if errorlevel 1 goto ERROR3
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -tWM -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; toolbed.c  > nul
   if errorlevel 1 goto ERROR3

   echo BRC32: Compiling resources...
   copy /b %HG_ROOT%\resources\oohg_bcc.rc + mgide.rc _temp.rc /y > nul
   %HG_BCC%\bin\brc32.exe -r -i%HG_ROOT%\resources _temp.rc > nul
   if errorlevel 1 goto ERROR3

   echo ILINK32: Linking...
   echo c0w32.obj + > b32.bc
   echo mgide.obj dbucvc.obj formedit.obj menued.obj toolbed.obj, + >> b32.bc
   echo oide.exe, + >> b32.bc
   echo oide.map, + >> b32.bc
   echo %HG_ROOT%\%LIB_GUI%\oohg.lib + >> b32.bc
   for %%a in ( gtgui rtl vmmt rdd common hbsix dbffpt dbfntx macro ct lang pcrepos ) do if exist %HG_HRB%\%LIB_HRB%\%%a.lib echo %HG_HRB%\%LIB_HRB%\%%a.lib + >> b32.bc
   echo %HG_ROOT%\%LIB_GUI%\bostaurus.lib + >> b32.bc
   echo %HG_ROOT%\%LIB_GUI%\hbprinter.lib + >> b32.bc
   echo %HG_ROOT%\%LIB_GUI%\miniprint.lib + >> b32.bc
   for %%a in ( cw32mt import32 user32 winspool gdi32 comctl32 comdlg32 shell32 ole32 oleaut32 uuid mpr wsock32 ws2_32 mapi32 winmm vfw32 msimg32 iphlpapi ) do echo %%a.lib + >> b32.bc
   echo , , + >> b32.bc
   echo _temp.res + >> b32.bc
   "%HG_BCC%\bin\ilink32.exe" -Gn -Tpe -aa -L%HG_BCC%\lib;%HG_BCC%\lib\psdk; @b32.bc > nul

   if exist oide.exe goto OK_XB
   echo Build finished with ERROR !!!
   goto CLEAN_XB

:ERROR1

   echo This file must be executed from IDE folder !!!
   goto END

:ERROR2

   echo COMPILE ERROR: Is oide.exe running ?
   goto END

:ERROR3
   echo Build finished with ERROR !!!
   goto END

:OK_XB

   echo Build finished OK !!!

:CLEAN_XB

   for %%a in ( *.tds )   do del %%a > nul
   for %%a in ( *.c )     do del %%a > nul
   for %%a in ( *.map )   do del %%a > nul
   for %%a in ( *.obj )   do del %%a > nul
   for %%a in ( b32.bc )  do del %%a > nul
   for %%a in ( _temp.* ) do del %%a > nul
   goto END

:COMPILE_XM

   if "%HG_HRB%"   == "" set HG_HRB=%HG_ROOT%\xhmingw
   if "%HG_MINGW%" == "" set HG_MINGW=%HG_CCOMP%
   if "%HG_MINGW%" == "" set HG_MINGW=%HG_HRB%\comp\mingw
   if "%HG_CCOMP%" == "" set HG_CCOMP=%HG_MINGW%
   if "%LIB_GUI%"  == "" set LIB_GUI=lib\xhb\mingw
   if "%LIB_HRB%"  == "" set LIB_HRB=lib
   if "%BIN_HRB%"  == "" set BIN_HRB=bin

   if exist oide.exe del oide.exe
   if exist oide.exe goto ERROR2

   set "HG_PATH=%PATH%"
   set "PATH=%HG_MINGW%\bin;%HG_HRB%\%BIN_HRB%"

   echo xHarbour: Compiling sources...
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\mgide    -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n -w3 -gc0 -es2 -q0
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\dbucvc   -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n -w3 -gc0 -es2 -q0
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\formedit -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n -w3 -gc0 -es2 -q0
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\menued   -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n -w3 -gc0 -es2 -q0
   "%HG_HRB%\%BIN_HRB%\harbour.exe" prgs\toolbed  -i%HG_HRB%\include;%HG_ROOT%\include;fmgs -n -w3 -gc0 -es2 -q0

   echo GCC: Compiling...
   gcc -I. -I%HG_HRB%\include -I%HG_ROOT%\include -Wall -c mgide.c    -o mgide.o
   gcc -I. -I%HG_HRB%\include -I%HG_ROOT%\include -Wall -c dbucvc.c   -o dbucvc.o
   gcc -I. -I%HG_HRB%\include -I%HG_ROOT%\include -Wall -c formedit.c -o formedit.o
   gcc -I. -I%HG_HRB%\include -I%HG_ROOT%\include -Wall -c menued.c   -o menued.o
   gcc -I. -I%HG_HRB%\include -I%HG_ROOT%\include -Wall -c toolbed.c  -o toolbed.o

   echo WindRes: Compiling resource file...
   echo #define oohgpath %HG_ROOT%\RESOURCES > _oohg_resconfig.h
   copy /b %HG_ROOT%\resources\ooHG.rc + mgide.rc _temp.rc > nul
   windres -i _temp.rc -o _temp.o

   echo GCC: Linking...
   set HG_OBJS=%HG_HRB%\%LIB_HRB%\mainwin.o mgide.o dbucvc.o formedit.o menued.o toolbed.o _temp.o
   set HG_LIBS=-lbostaurus -lhbprinter -lminiprint -looHG
   set HG_CFLAGS=-Wall -mwindows -static -static-libgcc
   set HG_XLIBS=-lcodepage -lcommon -lct -ldbfcdx -ldbffpt -ldbfntx -ldebug -lgtgui -lgtwin -lhbsix -lhsx -llang -lmacro -lpcrepos -lpp -lrdd -lrtl -lvmmt
   set HG_WLIBS=-luser32 -lwinspool -lgdi32 -lcomctl32 -lcomdlg32 -lole32 -loleaut32 -luuid -lmpr -lwsock32 -lws2_32 -lmapi32 -lwinmm -lvfw32 -lmsimg32 -liphlpapi
   set HG_SEARCH=-L. -L%HG_MINGW%\lib -L%HG_HRB%\%LIB_HRB% -L%HG_ROOT%\%LIB_GUI%
   gcc -o oide.exe %HG_OBJS% %HG_CFLAGS% %HG_SEARCH% -Wl,--start-group %HG_XLIBS% %HG_LIBS% %HG_WLIBS% -Wl,--end-group

   if exist oide.exe goto OK_XM
   echo Build finished with ERROR !!!
   goto CLEAN_XM

:OK_XM

   echo Build finished OK !!!

:CLEAN_XM

   if exist _temp.o del _temp.o
   if exist _temp.rc del _temp.rc
   if exist _oohg_resconfig.h del _oohg_resconfig.h
   del mgide.o
   del mgide.c
   del dbucvc.o
   del dbucvc.c
   del formedit.o
   del formedit.c
   del menued.o
   del menued.c
   del toolbed.o
   del toolbed.c
   set "PATH=%HG_PATH%"
   set HG_PATH=
   set HG_OBJS=
   set HG_LIBS=
   set HG_CFLAGS=
   set HG_XLIBS=
   set HG_WLIBS=
   set HG_SEARCH=
   goto END

:END

   set HG_START_DP_IDE_COMPILE_BAT=
   echo.
