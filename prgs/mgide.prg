/*
 * $Id: mgide.prg $
 */
/*
 * OOHG source code:
 * OOHG IDE+ form generator
 *
 * Copyright 2014-2022 Fernando Yurisich <fyurisich@oohg.org> and contributors of
 * the Object Oriented (x)Harbour GUI (aka OOHG) Project, https://oohg.github.io
 *
 * Portions of this project are based upon:
 *    "Harbour Minigui IDE"
 *       Copyright 2002-2014 Ciro Vargas Clemow, <cvc@oohg.org>
 *    "Harbour MiniGUI Extended Edition Library"
 *       Copyright 2005-2020 MiniGUI Team, http://hmgextended.com
 *    "Harbour GUI framework for Win32"
 *       Copyright 2001 Alexander S.Kresin, <alex@kresin.ru>
 *       Copyright 2001 Antonio Linares, <alinares@fivetech.com>
 *    "Harbour MiniGUI"
 *       Copyright 2002-2016 Roberto Lopez, <mail.box.hmg@gmail.com>
 *    "Harbour Project"
 *       Copyright 1999-2022 Contributors, https://harbour.github.io/
 */
/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file LICENSE.txt. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1335, USA (or download from http://www.gnu.org/licenses/).
 *
 * As a special exception, the ooHG Project gives permission for
 * additional uses of the text contained in its release of ooHG.
 *
 * The exception is that, if you link the ooHG libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the ooHG library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the ooHG
 * Project under the name ooHG. If you copy code from other
 * ooHG Project or Free Software Foundation releases into a copy of
 * ooHG, as the General Public License permits, the exception does
 * not apply to the code that you add in this way. To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for ooHG, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */


#include "oohg.ch"
#include "hbclass.ch"
#include "common.ch"
#include "i_windefs.ch"

#define APP_F_OPTIONS         i18n( 'F1 Help    F5 Build    F6 Build / Run    F7 Run    F8 Debug    ' )
#define APP_FULL_NAME         ( "OOHG IDE+ v." + SubStr( __DATE__, 3, 2 ) + "." + Right( __DATE__, 4 ) )
#define CR                    Chr( 13 )
#define HTAB                  Chr( 9 )
#define LF                    Chr( 10 )
#define NUL                   Chr( 0 )
#define BKSLSH                "\"
#define UpperNIL( cValue )    iif( Upper( AllTrim( cValue ) ) == "NIL", "NIL", cValue )
#define NOTEMPTY( cValue )    ( ! Empty( cValue ) .AND. UpperNIL( cValue ) != "NIL" )
#define NOTZERO( cValue )     ( ! Empty( cValue ) .AND. UpperNIL( cValue ) != "NIL" .AND. UpperNIL( cValue ) != "0" )

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION Main( rtl )
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL myIde

   SetOneArrayItemPerLine( .T. )

   SetAppHotKey( VK_F9, 0, { || _OOHG_CallDump( "IDE Dump", InputBox( "Enter (F)ile, (S)creen or (B)oth:", 'OOHG IDE+' ) ) } )
   SetAppHotKey( VK_F10, 0, { || _OOHG_CallDump() } )
   SetAppHotKey( VK_F11, 0, { || AutoMsgBox( &( InputBox( "Variable to inspect:", 'OOHG IDE+' ) ) ) } )

   IF rtl # NIL
      rtl := Upper( rtl )
      IF rtl == "RTL"
         SET GLOBALRTL ON
         rtl := NIL
      ENDIF
   ENDIF

   myIde := THMI()
   myIde:NewIde( rtl )
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   DATA aClrDefs                  INIT {}
   DATA aClrXtrs                  INIT {}
   DATA aEditors                  INIT {}
   DATA aLineR                    INIT {}
   DATA aOriginalFont             INIT Array( 8 )
   DATA aPositions                INIT { {0, 0}, {120, 0}, {120, GetDeskTopRealWidth() - 380} }
   DATA aSystemColor              INIT { 215, 231, 244 }
   DATA aSystemColorAux           INIT  {}
   DATA cBCCFolder                INIT ''
   DATA cBCCFolderB               INIT ''
   DATA cBCCFolderI               INIT ''
   DATA cBCCFolderL               INIT ''
   DATA cExe                      INIT ''
   DATA cExtEditor                INIT ''
   DATA cFile                     INIT ''
   DATA cFormDefFontColor         INIT ''
   DATA cFormDefFontName          INIT ''
   DATA cGuiHbBCC                 INIT ''
   DATA cGuiHbBCCI                INIT ''
   DATA cGuiHbBCCL                INIT ''
   DATA cGuiHbBCCR                INIT ''
   DATA cGuiHbMinGW               INIT ''
   DATA cGuiHbMinGWI              INIT ''
   DATA cGuiHbMinGWL              INIT ''
   DATA cGuiHbMinGWR              INIT ''
   DATA cGuiHbPelles              INIT ''
   DATA cGuiHbPellesI             INIT ''
   DATA cGuiHbPellesL             INIT ''
   DATA cGuiHbPellesR             INIT ''
   DATA cGuixHbBCC                INIT ''
   DATA cGuixHbBCCI               INIT ''
   DATA cGuixHbBCCL               INIT ''
   DATA cGuixHbBCCR               INIT ''
   DATA cGuixHbMinGW              INIT ''
   DATA cGuixHbMinGWI             INIT ''
   DATA cGuixHbMinGWL             INIT ''
   DATA cGuixHbMinGWR             INIT ''
   DATA cGuixHbPelles             INIT ''
   DATA cGuixHbPellesI            INIT ''
   DATA cGuixHbPellesL            INIT ''
   DATA cGuixHbPellesR            INIT ''
   DATA cHbBCCFolder              INIT ''
   DATA cHbBCCFolderB             INIT ''
   DATA cHbBCCFolderI             INIT ''
   DATA cHbBCCFolderL             INIT ''
   DATA cHbMinGWFolder            INIT ''
   DATA cHbMinGWFolderB           INIT ''
   DATA cHbMinGWFolderI           INIT ''
   DATA cHbMinGWFolderL           INIT ''
   DATA cHbPellFolder             INIT ''
   DATA cHbPellFolderB            INIT ''
   DATA cHbPellFolderI            INIT ''
   DATA cHbPellFolderL            INIT ''
   DATA cIDE_Folder               INIT ''
   DATA cItemFile                 INIT ''
   DATA cLibCC                    INIT ''
   DATA cLibXH                    INIT ''
   DATA cMakeTool                 INIT ''
   DATA cMinGWFolder              INIT ''
   DATA cMinGWFolderB             INIT ''
   DATA cMinGWFolderI             INIT ''
   DATA cMinGWFolderL             INIT ''
   DATA cOutFile                  INIT ''
   DATA cPellFolder               INIT ''
   DATA cPellFolderB              INIT ''
   DATA cPellFolderI              INIT ''
   DATA cPellFolderL              INIT ''
   DATA cProjectName              INIT ''
   DATA cProjFolder               INIT ''
   DATA cText                     INIT ''
   DATA cxHbBCCFolder             INIT ''
   DATA cxHbBCCFolderB            INIT ''
   DATA cxHbBCCFolderI            INIT ''
   DATA cxHbBCCFolderL            INIT ''
   DATA cxHbMinGWFolder           INIT ''
   DATA cxHbMinGWFolderB          INIT ''
   DATA cxHbMinGWFolderI          INIT ''
   DATA cxHbMinGWFolderL          INIT ''
   DATA cxHbPellFolder            INIT ''
   DATA cxHbPellFolderB           INIT ''
   DATA cxHbPellFolderI           INIT ''
   DATA cxHbPellFolderL           INIT ''
   DATA Form_Edit                 INIT NIL
   DATA Form_Prefer               INIT NIL
   DATA Form_Splash               INIT NIL
   DATA Form_Tree                 INIT NIL
   DATA Form_Wait                 INIT NIL
   DATA lCloseOnFormExit          INIT .F.
   DATA lCtrlSizes                INIT .T.
   DATA lFormDefFontBold          INIT .F.
   DATA lFormDefFontItalic        INIT .F.
   DATA lFormDefFontStrikeOut     INIT .F.
   DATA lFormDefFontUnderLine     INIT .F.
   DATA lHideTT                   INIT .F.
   DATA lMultiDelete              INIT .F.
   DATA lPsave                    INIT .T.
   DATA lSave                     INIT .T.
   DATA lSaveDefaultValues        INIT .T.
   DATA lSavePosOnDrag            INIT .T.
   DATA lSaveSizeonRed            INIT .T.
   DATA lSnap                     INIT .F.
   DATA MainHeight                INIT NIL
   DATA nActiveEditor             INIT 0
   DATA nCaretPos                 INIT 0
   DATA nColBorder                INIT 50
   DATA nCompilerC                INIT 2
   DATA nCompxBase                INIT 1
   DATA nDPIh                     INIT 0
   DATA nDPIw                     INIT 0
   DATA nFormDefFontCharSet       INIT 0
   DATA nFormDefFontSize          INIT 0
   DATA nLabelHeight              INIT 0
   DATA nLineSkip                 INIT 5
   DATA nPosText                  INIT 0
   DATA nPxJump                   INIT 10
   DATA nPxMove                   INIT 5
   DATA nPxSize                   INIT 1
   DATA nRowBorder                INIT 50
   DATA nStdVertGap               INIT 24
   DATA nSyntax                   INIT 1
   DATA nTabSize                  INIT 8
   DATA nTBuild                   INIT 1
   DATA nTextBoxHeight            INIT 0

   METHOD About
   METHOD AjustaFrame
   METHOD Analizar
   METHOD BuildWithHarbourAndBCC
   METHOD BuildWithHarbourAndMinGW
   METHOD BuildWithHarbourAndPellesC
   METHOD BuildWithxHarbourAndBCC
   METHOD BuildWithxHarbourAndMinGW
   METHOD BuildWithxHarbourAndPellesC
   METHOD CleanR
   METHOD ColorToStr
   METHOD CompileOptions
   METHOD DatabaseView
   METHOD DataMan
   METHOD DeleteItem
   METHOD EditColors
   METHOD EditorExit
   METHOD Exit
   METHOD Form_Prefer_OnChange
   METHOD GetPreferredFont
   METHOD GoLine
   METHOD InitializeProject
   METHOD LeaLogR
   METHOD LeaStrR
   METHOD LoadClrDefs
   METHOD LookChanges
   METHOD ModifyItem
   METHOD myInputWindow
   METHOD NewCH
   METHOD NewFMG
   METHOD NewIde
   METHOD NewPRG
   METHOD NewProject
   METHOD NewRC
   METHOD NewRPT
   METHOD NextSearch
   METHOD OkPrefer
   METHOD OpenFile
   METHOD OpenProject
   METHOD PosXY
   METHOD Preferences
   METHOD PrintIt
   METHOD ReadINI
   METHOD Reforma
   METHOD Report_Edit
   METHOD RunP
   METHOD SaveAndExit
   METHOD SaveFile
   METHOD SaveINI
   METHOD SaveProject
   METHOD SearchItem
   METHOD SearchText
   METHOD SearchType
   METHOD SplashDelay
   METHOD StrToColor
   METHOD TxtSearch
   METHOD ViewErrors
   METHOD ViewSource
ENDCLASS

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD NewIde( cParameter ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL nPos, nRed, nGreen, nBlue, lIsProject := .F., pmgFolder, nEsquema, cvcx, cvcy, cName, cExt

   // Preserve OOHG's default font before changing it
   ::aOriginalFont[1] := ::cFormDefFontName      := _OOHG_DefaultFontName
   ::aOriginalFont[2] := ::nFormDefFontSize      := _OOHG_DefaultFontSize
   ::aOriginalFont[3] := ::lFormDefFontBold      := _OOHG_DefaultFontBold
   ::aOriginalFont[4] := ::lFormDefFontItalic    := _OOHG_DefaultFontItalic
   ::aOriginalFont[5] := ::cFormDefFontColor     := ::ColorToStr( _OOHG_DefaultFontColor )
   ::aOriginalFont[6] := ::lFormDefFontUnderLine := _OOHG_DefaultFontUnderLine
   ::aOriginalFont[7] := ::lFormDefFontStrikeOut := _OOHG_DefaultFontStrikeOut
   ::aOriginalFont[8] := ::nFormDefFontCharSet   := _OOHG_DefaultFontCharSet

   SET CENTURY ON
   SET EXACT ON
   SET FONT TO "Verdana", 9
   SET INTERACTIVECLOSE OFF
   SET NAVIGATION EXTENDED
   SET BROWSESYNC ON
   SET TOOLTIPSTYLE BALLOON

   ::cProjFolder := GetCurrentFolder()
   ::cIDE_Folder := GetStartupFolder()

   ::nDPIw := ( PixelsPerInchX() / 96 )
   ::nDPIh := ( PixelsPerInchY() / 96 )

   ::MainHeight := Int( 50 * ::nDPIh ) + GetTitleHeight() + GetBorderHeight()

   IF cParameter # NIL
      nPos := At( ".", cParameter )
      IF nPos > 0
         cName := SubStr( cParameter, 1, nPos - 1 )
         IF ! Empty( cName )
            cExt := SubStr( cParameter, nPos + 1, 3 )
            IF Lower( cExt ) == "ch"
               cParameter := cName + "." + cExt
            ELSEIF Lower( cExt ) == "fmg"
               cParameter := cName + "." + cExt
            ELSEIF Lower( cExt ) == "pmg"
               lIsProject := .T.
               cParameter := cName + "." + cExt
            ELSEIF Lower( cExt ) == "prg"
               cParameter := cName + "." + cExt
            ELSEIF Lower( cExt ) == "rc"
               cParameter := cName + "." + cExt
            ELSEIF Lower( cExt ) == "rpt"
               cParameter := cName + "." + cExt
            ENDIF
         ENDIF
      ELSE
         cParameter := cParameter + ".fmg"
      ENDIF
   ENDIF

   nEsquema := 4        // COLOR_MENU, Menu background color
   nRed     := GetRed( GetSysColor( nEsquema ) )
   nGreen   := GetGreen( GetSysColor( nEsquema ) )
   nBlue    := GetBlue( GetSysColor( nEsquema ) )
   ::aSystemColorAux := &( '{' + Str( nRed, 3 ) + ',' + Str( nGreen, 3 ) + ',' + Str( nBlue, 3 ) + '}' )

   cvcx := GetDesktopWidth()
   cvcy := GetDesktopHeight()

   IF cvcx < 800 .OR. cvcy < 600
      MsgInfo( i18n( 'Best viewed with 800x600 or higher resolution.' ), 'OOHG IDE+' )
   ENDIF

   DEFINE WINDOW Form_Tree OBJ ::Form_Tree ;
      AT 0, 0 ;
      WIDTH 584 ;
      HEIGHT 308 ;
      TITLE APP_FULL_NAME ;
      MAIN ;
      ICON 'IDE_EDIT' ;
      ON SIZE ::AjustaFrame() ;
      ON INTERACTIVECLOSE iif( MsgYesNo( i18n( 'Exit program?' ), 'OOHG IDE+' ), ::Exit(), .F. ) ;
      BACKCOLOR ::aSystemColor

      DEFINE STATUSBAR
         STATUSITEM APP_FULL_NAME WIDTH GetTextWidth( 0, APP_FULL_NAME, ::Form_Tree:StatusBar:FontHandle )
         STATUSITEM APP_F_OPTIONS WIDTH GetTextWidth( 0, APP_F_OPTIONS, ::Form_Tree:StatusBar:FontHandle ) + 25
      END STATUSBAR

      DEFINE MAIN MENU
         POPUP i18n( '&File' )
            ITEM i18n( '&New Project' )          IMAGE 'IDE_NEW'     ACTION ::NewProject()
            ITEM i18n( '&Open Project' )         IMAGE 'IDE_OPENPRJ' ACTION ::OpenProject()
            ITEM i18n( '&Save Project' )         IMAGE 'IDE_SAVE'    ACTION ::SaveProject()
            SEPARATOR
            ITEM i18n( '&Preferences' )          IMAGE 'IDE_CONFIG'  ACTION ::Preferences()
            SEPARATOR
            ITEM i18n( '&Exit' )                 IMAGE 'IDE_EXIT'    ACTION ::Exit()
         END POPUP
         POPUP i18n( 'Pro&ject' )
            POPUP i18n( 'Add Item' ) NAME 'Add'  IMAGE 'IDE_NEWITEM'
               ITEM 'FMG'                                            ACTION ::NewFMG( InputBox( "FMG", i18n( 'Add FMG module' ) ) )
               ITEM 'PRG'                                            ACTION ::NewPRG( InputBox( "PRG", i18n( 'Add PRG Module' ) ) )
               ITEM 'CH'                                             ACTION ::NewCH( InputBox( "CH", i18n( 'Add CH Module' ) ) )
               ITEM 'RPT'                                            ACTION ::NewRPT( InputBox( "RPT", i18n( 'Add RPT Module' ) ) )
               ITEM 'RC'                                             ACTION ::NewRC( InputBox( "RC", i18n( 'Add RC Module' ) ) )
            END POPUP
            SEPARATOR
            ITEM i18n( "Modify Item" )           IMAGE 'IDE_MOD'     ACTION ::Analizar()
            SEPARATOR
            ITEM i18n( 'Remove Item' )           IMAGE 'IDE_DEL'     ACTION ::DeleteItem()
            SEPARATOR
            ITEM i18n( 'View / Print Item' )     IMAGE 'IDE_PRINT'   ACTION ::PrintIt()
         END POPUP
         POPUP i18n( 'Build / Run / Debug' )
            ITEM i18n( 'Build Project' )         IMAGE 'IDE_BUILD'   ACTION ::CompileOptions( 1 )
            ITEM i18n( 'Build and Run Project' ) IMAGE 'IDE_B_R'     ACTION ::CompileOptions( 2 )
            ITEM i18n( 'Run Project' )           IMAGE 'IDE_RUN'     ACTION ::CompileOptions( 3 )
            ITEM i18n( 'Debug Project' )         IMAGE 'IDE_DEBUG'   ACTION ::CompileOptions( 4 )
         END POPUP
         POPUP i18n( 'Tools' )
            ITEM i18n( 'Global Search Text' )    IMAGE 'IDE_FIND'    ACTION ::SearchText()
            ITEM i18n( 'Quick Browse' )          IMAGE 'IDE_BROWSE'  ACTION ::DatabaseView()
            ITEM i18n( 'Data Manager' )          IMAGE 'IDE_DM'      ACTION ::DataMan()
         END POPUP
         POPUP i18n( '&Help' )
            ITEM i18n( 'ooHG Syntax Help' )      IMAGE 'IDE_OOHG'    ACTION _Execute( GetActiveWindow(), NIL, ::cIDE_Folder + "\oohg.chm", NIL, NIL, 5 )
            ITEM i18n( '&About' )                IMAGE 'IDE_OIDE'    ACTION ::About()
         END POPUP
      END MENU

      ON KEY F1 ACTION Help_F1( "PROJECT", Self )
      ON KEY F5 ACTION ::CompileOptions( 1 )
      ON KEY F6 ACTION ::CompileOptions( 2 )
      ON KEY F7 ACTION ::CompileOptions( 3 )
      ON KEY F8 ACTION ::CompileOptions( 4 )

      @ 65, 30 FRAME frame_tree WIDTH ( cvcx - 30 ) HEIGHT ( cvcy - 65 )

      DEFINE TREE Tree_1 ;
         AT 90, 50 ;
         WIDTH 200 ;
         HEIGHT ( cvcy -290 ) ;
         VALUE 1 ;
         TOOLTIP { || iif( ::lHideTT, NIL, i18n( 'Double click to modify an item.' ) ) } ;
         ON DBLCLICK ::Analizar() ;
         ON ENTER ::Analizar() ;
         NODEIMAGES { "IDE_CL_FL", "IDE_OP_FL" } ;
         ITEMIMAGES { "IDE_DOC", "IDE_DOC_FL" }

         NODE "Project" IMAGES { "IDE_DOC" }
            TREEITEM "FMG"
            TREEITEM "PRG"
            TREEITEM "CH"
            TREEITEM "RPT"
            TREEITEM "RC"
         END NODE
      END TREE

      DEFINE SPLITBOX
         DEFINE TOOLBAR 0 ;
            BUTTONSIZE 16, 16 ;
            FLAT

            BUTTON Button_13 ;
               TOOLTIP i18n( 'Exit' ) ;
               PICTURE 'IDE_EXIT' ;
               ACTION iif( MsgYesNo( i18n( "Exit program?" ), 'OOHG IDE+' ), ::Exit(), NIL ) ;
               AUTOSIZE

            BUTTON Button_1b ;
               TOOLTIP i18n( 'Open ...' ) ;
               PICTURE 'IDE_OPENPRJ' ;
               ACTION ::OpenProject() ;
               AUTOSIZE

            BUTTON Button_01 ;
               TOOLTIP i18n( 'Save ...' ) ;
               PICTURE 'IDE_SAVE' ;
               ACTION ::SaveProject() ;
               AUTOSIZE ;
               SEPARATOR

            BUTTON Button_1 ;
               TOOLTIP i18n( 'Add ...' ) ;
               PICTURE 'IDE_NEWITEM' ;
               ACTION ::NewFMG( InputBox( "FMG", i18n( 'Add FMG module' ) ) ) ;
               DROPDOWN ;
               AUTOSIZE

            BUTTON Button_6 ;
               TOOLTIP i18n( 'Modify item' ) ;
               PICTURE 'IDE_MOD' ;
               ACTION ::Analizar() ;
               AUTOSIZE

            BUTTON Button_07 ;
               TOOLTIP i18n( 'Remove item' ) ;
               PICTURE 'IDE_DEL' ;
               ACTION ::DeleteItem() ;
               AUTOSIZE

            BUTTON Button_7a ;
               TOOLTIP i18n( 'View / Print item' ) ;
               PICTURE 'IDE_PRINT' ;
               ACTION ::PrintIt() ;
               AUTOSIZE ;
               SEPARATOR

            BUTTON Button_09 ;
               TOOLTIP i18n( 'Build project' ) ;
               PICTURE 'IDE_BUILD' ;
               ACTION ::CompileOptions( 1 )

            BUTTON Button_10 ;
               TOOLTIP i18n( 'Build and run project' ) ;
               PICTURE 'IDE_B_R' ;
               ACTION ::CompileOptions( 2 )

            BUTTON Button_11 ;
               TOOLTIP i18n( 'Run project' ) ;
               PICTURE 'IDE_RUN' ;
               ACTION ::CompileOptions( 3 ) ;
               DROPDOWN ;
               AUTOSIZE ;
               SEPARATOR

            BUTTON Button_8 ;
               TOOLTIP i18n( 'Global search' ) ;
               PICTURE 'IDE_FIND' ;
               ACTION ::SearchText() ;
               AUTOSIZE

            BUTTON Button_qb ;
               TOOLTIP i18n( 'Quick browse' ) ;
               PICTURE 'IDE_BROWSE' ;
               ACTION ::databaseview() ;
               AUTOSIZE

            BUTTON Button_12 ;
               TOOLTIP i18n( 'Data manager' ) ;
               PICTURE 'IDE_DM' ;
               ACTION ::DataMan() ;
               AUTOSIZE
         END TOOLBAR

         DEFINE DROPDOWN MENU BUTTON Button_1
            ITEM 'FMG' ACTION ::NewFMG( InputBox( "FMG", i18n( 'Add FMG module' ) ) )
            ITEM 'PRG' ACTION ::NewPRG( InputBox( "PRG", i18n( 'Add PRG Module' ) ) )
            ITEM 'CH'  ACTION ::NewCH( InputBox( "CH", i18n( 'Add CH Module' ) ) )
            ITEM 'RPT' ACTION ::NewRPT( InputBox( "RPT", i18n( 'Add RPT Module' ) ) )
            ITEM 'RC'  ACTION ::NewRC( InputBox( "RC", i18n( 'Add RC Module' ) ) )
         END MENU

         DEFINE DROPDOWN MENU BUTTON Button_11
            ITEM i18n( 'Run' )   IMAGE 'IDE_B_R'   ACTION ::CompileOptions( 3 )
            ITEM i18n( 'Debug' ) IMAGE 'IDE_DEBUG' ACTION ::CompileOptions( 4 )
         END MENU
      END SPLITBOX

      @ 135, 280 IMAGE image_front ;
         PICTURE 'IDE_BACKIMG' ;
         WIDTH 420 ;
         HEIGHT 219
   END WINDOW

   CENTER WINDOW Form_Tree

   DEFINE WINDOW Form_Splash OBJ ::Form_Splash ;
      AT 0, 0 ;
      WIDTH 584 ;
      HEIGHT 308 ;
      TITLE '' ;
      MODAL ;
      TOPMOST ;
      NOCAPTION ;
      ON INIT ::SplashDelay()

      @ 0, 0 IMAGE image_splash ;
         PICTURE 'IDE_SPLASH' ;
         WIDTH 584 ;
         HEIGHT 308
   END WINDOW

   CENTER WINDOW Form_Splash

   // Default values from exe startup folder
   ::ReadINI( ::cIDE_Folder + '\hmi.ini' )

   // Load color definitions
   ::LoadClrDefs()

   DEFINE WINDOW Form_Wait OBJ ::Form_Wait  ;
      AT 10, 10 ;
      WIDTH 180 ;
      HEIGHT 100 ;
      TITLE i18n( "Information" ) ;
      CHILD ;
      NOSYSMENU ;
      NOCAPTION ;
      NOSHOW ;
      BACKCOLOR ::aSystemColor

      @ 35, 15 LABEL hmi_label_101 VALUE '              '  AUTOSIZE SIZE 14
   END WINDOW

   CENTER WINDOW Form_Wait

   IF lIsProject
      // Project
      pmgFolder := OnlyFolder( cParameter )
      IF ! Empty( pmgFolder )
         ::cProjFolder := pmgFolder
         DirChange( pmgFolder )
      ENDIF
      ::InitializeProject( cParameter )
      ::Form_Tree:Add:Enabled := .T.
      ::Form_Tree:Button_1:Enabled := .T.
      ACTIVATE WINDOW Form_Tree, Form_Wait, Form_Splash
   ELSEIF ! Empty( cParameter )
      // Other files (ch, fmg, prg, rc, rpt)
      pmgFolder := OnlyFolder( cParameter )
      IF ! Empty( pmgFolder )
         ::cProjFolder := pmgFolder
         DirChange( pmgFolder )
      ENDIF
      ::lCloseOnFormExit := .T.
      ::Form_Tree:Add:Enabled := .F.
      ::Form_Tree:Button_1:Enabled := .F.
      ::Form_Tree:Hide()
      ACTIVATE WINDOW Form_Tree, Form_Wait, Form_Splash NOWAIT
      ::cFile := cParameter
      ::Analizar( cParameter )
      ::cFile := ""
   ELSE
      // None
      ::Form_Tree:Add:Enabled := .F.
      ::Form_Tree:Button_1:Enabled := .F.
      ACTIVATE WINDOW Form_Tree, Form_Wait, Form_Splash
   ENDIF
RETURN Self

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD AjustaFrame() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL aInfo := Array( 4 )

   GetClientRect( ::Form_Tree:hWnd, aInfo )

   ::Form_Tree:frame_tree:Width  := aInfo[ 3 ] - 65
   ::Form_Tree:frame_tree:Height := aInfo[ 4 ] - 120
   ::Form_Tree:Tree_1:Height     := ::Form_Tree:frame_tree:Height - 50

   IF ( ::Form_Tree:frame_tree:Width < ( ::Form_Tree:image_front:Width + 270 ) ) .OR. ( ::Form_Tree:frame_tree:Height < ( ::Form_Tree:image_front:Height + 80 ) )
      ::Form_Tree:image_front:Hide()
   ELSE
      ::Form_Tree:image_front:Show()
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION BorraTemp( cFolder )
/*--------------------------------------------------------------------------------------------------------------------------------*/
   hb_idleSleep( 1 )
   IF File( cFolder + "OBJ\nul" )
      ZapDirectory( cFolder + "OBJ" + NUL )
   ENDIF
   DELETE FILE ( cFolder + '_aux.rc' )
   DELETE FILE ( cFolder + '_build.bat' )
   DELETE FILE ( cFolder + '_oohg_resconfig.h' )
   DELETE FILE ( cFolder + '_temp.bc' )
   DELETE FILE ( cFolder + '_temp.rc' )
   DELETE FILE ( cFolder + 'b32.bc' )
   DELETE FILE ( cFolder + 'comp.bat' )
   DELETE FILE ( cFolder + 'error.txt' )
   DELETE FILE ( cFolder + 'makefile.gcc' )
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION BorraObj()
/*--------------------------------------------------------------------------------------------------------------------------------*/
   LOCAL aOBJFilesB[aDir( 'OBJ\*.OBJ' )]
   LOCAL aCFiles[aDir( 'OBJ\*.C' )]
   LOCAL aOFiles[aDir( 'OBJ\*.O' )]
   LOCAL aRESFiles[aDir( 'OBJ\*.RES' )]
   LOCAL aMAPFiles[aDir( '*.MAP' )]
   LOCAL aTDSFiles[aDir( '*.TDS' )]
   LOCAL i

   aDir( 'OBJ\*.OBJ', aOBJFilesB )
   aDir( 'OBJ\*.C', aCFiles )
   aDir( 'OBJ\*.O', aOFiles )
   aDir( 'OBJ\*.RES', aRESFiles )
   aDir( '*.MAP', aMAPFiles )
   aDir( '*.TDS', aTDSFiles )

   For i := 1 To Len( aOBJFilesB )
      DELETE FILE ( 'OBJ\' +  aOBJFilesB[i] )
   NEXT i
   For i := 1 To Len( aCFiles )
      DELETE FILE ( 'OBJ\' + aCFiles[i] )
   NEXT i
   For i := 1 To Len( aOFiles )
      DELETE FILE ( 'OBJ\' + aOFiles[i] )
   NEXT i
   For i := 1 To Len( aRESFiles )
      DELETE FILE ( 'OBJ\' + aRESFiles[i] )
   NEXT i
   For i := 1 To Len( aMAPFiles )
      DELETE FILE ( aMAPFiles[i] )
   NEXT i
   For i := 1 To Len( aTDSFiles )
      DELETE FILE ( aTDSFiles[i] )
   NEXT i

   DirRemove( 'OBJ' )
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Analizar( cItem ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL cGroup, lWait, nPos, oEditor, cExt

   IF HB_ISSTRING( cItem )
      nPos := At( ".", cItem )
      cExt := Lower( SubStr( cItem, nPos + 1, 3 ) )
      DO CASE
      CASE cExt == "ch"
         cGroup := "CH"
      CASE cExt == "fmg"
         cGroup := "FMG"
      CASE cExt == "prg"
         cGroup := "PRG"
      CASE cExt == "rc"
         cGroup := "RC"
      CASE cExt == "rpt"
         cGroup := "RPT"
      OTHERWISE
         RETURN NIL
      ENDCASE
      lWait := .T.
   ELSEIF ::Form_Tree:Tree_1:Value > 0
      cItem := ::Form_Tree:Tree_1:Item( ::Form_Tree:Tree_1:Value )
      cGroup := ::SearchType( ::Form_Tree:Tree_1:Value )
      DO CASE
      CASE cGroup == "CH"
         cExt := 'ch'
      CASE cGroup == "FMG"
         cExt := 'fmg'
      CASE cGroup == "PRG"
         cExt := 'prg'
      CASE cGroup == "RC"
         cExt := 'rc'
      CASE cGroup == "RPT"
         cExt := 'rpt'
      OTHERWISE
         RETURN NIL
      ENDCASE
      cItem := Lower( cItem ) + "." + cExt
      lWait := .F.
   ELSE
      RETURN NIL
   ENDIF

   IF cItem == "project" .OR. cItem == "ch" .OR. cItem == "fmg" .OR. cItem == "prg" .OR. cItem == "rc" .OR. cItem == "rpt"
      RETURN NIL
   ENDIF

   IF cGroup == "FMG"
      IF Len( ::aEditors ) > 0                // TODO: more than one form at the same time
         MsgStop( i18n( "Sorry, the IDE can't -yet- edit more than one FMG module at a time." ), 'OOHG IDE+' )
      ELSE
         IF aScan( ::aEditors, { |x| Lower( x:cForm ) == cItem + '.fmg' } ) > 0
            MsgStop( i18n( 'FMG is already open.' ), 'OOHG IDE+' )
         ELSE
            ::Form_Tree:button_07:enabled := .F.
            ::Form_Tree:button_09:enabled := .F.
            ::Form_Tree:button_10:enabled := .F.
            ::Form_Tree:button_11:enabled := .F.

            oEditor := TFormEditor()
            aAdd( ::aEditors, oEditor )
            ::nActiveEditor := Len( ::aEditors )
            oEditor:EditForm( Self, cItem, ::nActiveEditor, lWait )
         ENDIF
      ENDIF
   ELSEIF cGroup == "RPT"
      ::Report_Edit( cItem )
   ELSEIF cGroup == "PRG" .OR. cGroup == "CH" .OR. cGroup == "RC"
      ::ModifyItem( cItem, cGroup )
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD EditColors() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL cFile, cText, nLineCount, i, lCreate := .T.

   cFile := ::cIDE_Folder + "\i_clrdef.ch"
   IF File( cFile )
      cText := MemoRead( cFile )
      nLineCount := MLCount( cText )
      FOR i := 1 TO nLineCount
         IF ! Empty( AllTrim( StrTran( MemoLine( cText, 1200, i ), HTAB, " " ) ) )
            lCreate := .F.
         ENDIF
      NEXT i
   ENDIF
   IF lCreate .AND. ! MsgYesNo( i18n( "The file containing the color definitions could not be found." ) + CRLF + i18n( "Create new file and edit?" ), 'OOHG IDE+' )
      RETURN NIL
   ENDIF
   cText := "/*" + CRLF + ;
            " * Color definitions" + CRLF + ;
            " * Generated by ooHG IDE Plus v.18.0521" + CRLF + ;
            " * Visit us at https://oohg.github.io" + CRLF + ;
            " *" + CRLF + ;
            " * Add, at the end of this file, the color constants that you want the IDE" + CRLF + ;
            " * to recognize in addition to the ones defined at MINIGUI's 'i_color.ch'." + CRLF + ;
            " * Constants may be specified using #define or #xtranslate directives." + CRLF + ;
            " * Constants specified using #define can be deleted using #undef directive." + CRLF + ;
            " * The following (x)Harbour Preprocessor's rules apply:" + CRLF + ;
            " *    #define is case-sensitive and #xtranslate isn't." + CRLF + ;
            " *    #define takes precedence over #xtranslate." + CRLF + ;
            " *    last directive overwrites previous ones." + CRLF + ;
            " * e.g." + CRLF + ;
            " * These are diferent colors:" + CRLF + ;
            " *    #define MY_COLOR { 215, 231, 244 }" + CRLF + ;
            " *    #define my_COLOR { 100, 101, 102 }" + CRLF + ;
            " * These are diferent colors but, at FMG's save time, { 215, 231, 244 }" + CRLF + ;
            " * is always translated to 'MY_COLOR':" + CRLF + ;
            " *    #define MY_COLOR { 215, 231, 244 }" + CRLF + ;
            " *    #define my_COLOR { 215, 231, 244 }" + CRLF + ;
            " * These are the same color and the first directive will be ignored:" + CRLF + ;
            " *    #define MY_COLOR => { 215, 231, 244 }" + CRLF + ;
            " *    #define MY_COLOR => { 100, 101, 102 }" + CRLF + ;
            " * These are the same color and the first directive will be ignored:" + CRLF + ;
            " *    #xtranslate MY_COLOR => { 215, 231, 244 }" + CRLF + ;
            " *    #xtranslate my_COLOR => { 100, 101, 102 }" + CRLF + ;
            " * At FMG's load time 'MY_COLOR' is translated to { 215, 231, 244 } and" + CRLF + ;
            " * 'my_COLOR' and other variations except 'MY_COLOR' to { 100, 101, 102 }," + CRLF + ;
            " * at FMG's save time { 215, 231, 244 } is translated to 'MY_COLOR' and" + CRLF + ;
            " * { 100, 101, 102 } to 'my_COLOR':" + CRLF + ;
            " *    #define MY_COLOR => { 215, 231, 244 }" + CRLF + ;
            " *    #xtranslate my_COLOR => { 100, 101, 102 }" + CRLF + ;
            " * At FMG's load time 'MY_COLOR' and all it's variations are translated" + CRLF + ;
            " * to { 215, 231, 244 }. At FMG's save time { 215, 231, 244 } is always" + CRLF + ;
            " * translated to 'MY_COLOR'." + CRLF + ;
            " *    #define MY_COLOR => { 215, 231, 244 }" + CRLF + ;
            " *    #xtranslate my_COLOR => { 215, 231, 244 }" + CRLF + ;
            " */" + CRLF + ;
            CRLF + ;
            "#define IDE_COLOR { 215, 231, 244 }" + CRLF
   hb_MemoWrit( cFile, cText )
   ::OpenFile( cFile )
   ::LoadClrDefs()

RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD LoadClrDefs() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL cMiniGuiFolder, cMiniGuiFolderI, cFile, cText, nLineCount, i, cLine, cColor, cArray, aColor, nPos

   DO CASE
   CASE ( ::nCompxBase == 1 .AND. ::nCompilerC == 1 )  // Harbour-MinGW
      cMiniGuiFolder  := ::cGUIHbMinGW
      cMiniGuiFolderI := ::cGUIHbMinGWI
   CASE ( ::nCompxBase == 1 .AND. ::nCompilerC == 2 )  // Harbour-BCC
      cMiniGuiFolder  := ::cGuiHbBCC
      cMiniGuiFolderI := ::cGuiHbBCCI
   CASE ( ::nCompxBase == 1 .AND. ::nCompilerC == 3 )  // Harbour-PellesC
      cMiniGuiFolder  := ::cGuixHbPelles
      cMiniGuiFolderI := ::cGuixHbPellesI
   CASE ( ::nCompxBase == 2 .AND. ::nCompilerC == 1 )  // xHarbour-MinGW
      cMiniGuiFolder  := ::cGUIxHbMinGW
      cMiniGuiFolderI := ::cGUIxHbMinGWI
   CASE ( ::nCompxBase == 2 .AND. ::nCompilerC == 2 )  // xHarbour-BCC
      cMiniGuiFolder  := ::cGuixHbBCC
      cMiniGuiFolderI := ::cGuixHbBCCI
   CASE ( ::nCompxBase == 2 .AND. ::nCompilerC == 3 )  // xHarbour-PellesC
      cMiniGuiFolder  := ::cGuixHbPelles
      cMiniGuiFolderI := ::cGuixHbPellesI
   OTHERWISE
      cMiniGuiFolder  := ""
      cMiniGuiFolderI := ""
   ENDCASE

   cText := ""
   IF ! Empty( cMiniGuiFolderI )
      cFile := cMiniGuiFolder + '\' + cMiniGuiFolderI + "\i_color.ch"
      IF File( cFile )
         cText := MemoRead( cFile )
      ENDIF
   ENDIF
   cFile := ::cIDE_Folder + "\i_clrdef.ch"
   IF File( cFile )
      IF ! Empty( cText )
         cText += CRLF
      ENDIF
      cText += MemoRead( cFile )
   ENDIF

   IF ! Empty( cText )
      nLineCount := MLCount( cText )
      /* We need two arrays because #define is case-sensitive and #xtranslate isn't */
      FOR i := 1 TO nLineCount
         cLine := LTrim( MemoLine( cText, 1200, i ) )
         /* Skip one-line comments */
         IF Left( cLine, 1 ) == "*"
            LOOP
         ELSEIF Left( cLine, 2 ) == "//"
            LOOP
         ELSEIF Left( cLine, 2 ) == "&&"
            LOOP
         ENDIF
         /* Skip multi-line comments */
         IF Left( cLine, 2 ) == "/*"
            DO WHILE i <= nLineCount .AND. ( nPos := At( "*/", cLine ) ) == 0
               i ++
               cLine := LTrim( MemoLine( cText, 1200, i ) )
            ENDDO
            IF i > nLineCount
               EXIT
            ENDIF
            cLine := LTrim( SubStr( cLine, nPos + 2 ) )
         ENDIF
         /* Process directives */
         IF Left( cLine, 1 ) == "#"
            cLine := LTrim( SubStr( cLine, 2 ) )
            IF Upper( Left( cLine, 5 ) ) == "UNDEF"
               cLine := LTrim( SubStr( cLine, 6 ) )
               IF ( nPos := At( " ", cLine ) ) > 0
                  cColor := Left( cLine, nPos - 1 )
               ELSE
                  cColor := cLine
               ENDIF
               IF IsAlpha( cColor )
                  IF ( nPos := AScan( ::aClrDefs, {|cd| cd[1] == cColor } ) ) > 0
                     ADel( ::aClrDefs, nPos )
                     ASize( ::aClrDefs, Len( ::aClrDefs ) - 1 )
                  ENDIF
               ENDIF
            ELSEIF Upper( Left( cLine, 6 ) ) == "DEFINE"
               cLine := LTrim( SubStr( cLine, 7 ) )
               IF ( nPos := At( " ", cLine ) ) > 0
                  cColor := SubStr( cLine, 1, nPos - 1 )
                  IF IsAlpha( cColor )
                     cArray := AllTrim( SubStr( cLine, nPos + 1 ) )
                     IF IsValidColorArray( cArray )
                        /* It's valid, add or replace previous */
                        aColor := &( cArray )
                        IF ( nPos := AScan( ::aClrDefs, {|cd| cd[1] == cColor } ) ) > 0
                           ::aClrDefs[nPos, 2] := aColor
                        ELSE
                           AAdd( ::aClrDefs, { cColor, aColor } )
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ELSEIF Upper( Left( cLine, 10 ) ) == "XTRANSLATE"
               cLine := LTrim( SubStr( cLine, 11 ) )
               IF ( nPos := At( " ", cLine ) ) > 0
                  cColor := SubStr( cLine, 1, nPos - 1 )
                  IF IsAlpha( cColor )
                     cLine := LTrim( SubStr( cLine, nPos + 1 ) )
                     IF Left( cLine, 2 ) == "=>"
                        cArray := AllTrim( SubStr( cLine, 3 ) )
                        IF IsValidColorArray( cArray )
                           /* It's valid, add or replace previous */
                           aColor := &( cArray )
                           IF ( nPos := AScan( ::aClrXtrs, {|cx| Upper( cx[1] ) == Upper( cColor ) } ) )  > 0
                              ::aClrXtrs[nPos] := { cColor, aColor }
                           ELSE
                              AAdd( ::aClrXtrs, { cColor, aColor } )
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      NEXT i
   ENDIF

RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION IsValidColorArray( cColor )
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL aColor, uRet

   IF "A" == Type( cColor )
      aColor := &( cColor )
      uRet := Len( aColor ) == 3 .AND. ;
              HB_ISNUMERIC( aColor[1] ) .AND. aColor[1] >= 0 .AND. aColor[1] <= 255 .AND. ;
              HB_ISNUMERIC( aColor[2] ) .AND. aColor[2] >= 0 .AND. aColor[2] <= 255 .AND. ;
              HB_ISNUMERIC( aColor[3] ) .AND. aColor[3] >= 0 .AND. aColor[3] <= 255
   ELSE
      uRet := .F.
   ENDIF
RETURN uRet

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD StrToColor( cColor ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL uRet, nPos

   IF IsValidColorArray( cColor )
      uRet := &( cColor )
   ELSEIF Type( cColor ) == "N"
      uRet := Val( cColor )
   ELSE
      /* #define take precedence over #xtranslate, #define is case-sensitive and #xtranslate isn't */
      IF ( nPos := AScan( ::aClrDefs, {|cd| cd[1] == cColor } ) )  > 0
         uRet := ::aClrDefs[nPos, 2]
      ELSEIF ( nPos := AScan( ::aClrXtrs, {|cx| Upper( cx[1] ) == Upper( cColor ) } ) )  > 0
         uRet := ::aClrXtrs[nPos, 2]
      ELSE
         uRet := NIL
      ENDIF
   ENDIF

RETURN uRet

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ColorToStr( aColor ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL uRet

   IF aColor == NIL
      uRet := "NIL"
   ELSE
      uRet := "{ " + LTrim( Str( aColor[1] ) ) + ", " + LTrim( Str( aColor[2] ) ) + ", " + LTrim( Str( aColor[3] ) ) + " }"
   ENDIF

RETURN uRet

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ReadINI( cFile ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL lSnap := 0, nPos := 0, lHideTT := 0, cColor := "", cFontName := "", nFontSize := 0, lBold := 2, lItalic := 2, lUnderline := 2
LOCAL lSaveDefaultValues := 1, lSavePosOnDrag := 1, lSaveSizeonRed := 1, lMultiDelete := 0, lStrikeOut := 2

   IF Left( cFile, 1 ) == "\"
      cFile := SubStr( cFile, 2 )
   ENDIF

   IF ! File( cFile )
      hb_MemoWrit( cFile, '[PROJECT]' )
   ENDIF

   BEGIN INI FILE cFile
      // PROJECT
      GET ::cOutFile              SECTION 'PROJECT'     ENTRY "OUTFILE"       DEFAULT ''
      // EDITOR
      GET ::cExtEditor            SECTION 'EDITOR'      ENTRY "EXTERNAL"      DEFAULT ''
      GET ::nTabSize              SECTION "EDITOR"      ENTRY "TABSIZE"       DEFAULT 8
      IF ::nTabSize < 1 .OR. ::nTabSize > 99
         ::nTabSize := 8
      ENDIF
      // FORM'S FONT
      GET cFontName               SECTION "FORMFONT"    ENTRY "FONT"          DEFAULT ''
      IF ! Empty( cFontName ) .AND. cFontName # 'NIL'
         ::cFormDefFontName := cFontName
      ENDIF
      GET nFontSize               SECTION "FORMFONT"    ENTRY "SIZE"          DEFAULT 0
      IF nFontSize > 0
         ::nFormDefFontSize := nFontSize
      ENDIF
      GET lBold                   SECTION 'FORMFONT'    ENTRY "BOLD"          DEFAULT 2
      IF lBold == 0
         ::lFormDefFontBold := .F.
      ELSEIF lBold == 1
         ::lFormDefFontBold := .T.
      ENDIF
      GET lItalic                 SECTION 'FORMFONT'    ENTRY "ITALIC"        DEFAULT 2
      IF lItalic == 0
         ::lFormDefFontItalic := .F.
      ELSEIF lItalic == 1
         ::lFormDefFontItalic := .T.
      ENDIF
      GET cColor                  SECTION "FORMFONT"    ENTRY "COLOR"         DEFAULT ''
      IF ::StrToColor( cColor ) # NIL
         ::cFormDefFontColor := cColor
      ENDIF
      GET lUnderline              SECTION 'FORMFONT'    ENTRY "UNDERLINE"     DEFAULT 2
      IF lUnderline == 0
         ::lFormDefFontUnderLine := .F.
      ELSEIF lUnderline == 1
         ::lFormDefFontUnderLine := .T.
      ENDIF
      GET lStrikeOut              SECTION 'FORMFONT'    ENTRY "STRIKEOUT"     DEFAULT 2
      IF lStrikeOut == 0
         ::lFormDefFontStrikeOut := .F.
      ELSEIF lItalic == 1
         ::lFormDefFontStrikeOut := .T.
      ENDIF
      GET ::nFormDefFontCharSet   SECTION "FORMFONT"    ENTRY "CHARSET"       DEFAULT ::nFormDefFontCharSet
      // FORM'S METRICS
      GET ::nColBorder            SECTION "FORMMETRICS" ENTRY "COLBORDER"     DEFAULT 50
      GET ::nRowBorder            SECTION "FORMMETRICS" ENTRY "ROWBORDER"     DEFAULT 50
      GET ::nLabelHeight          SECTION "FORMMETRICS" ENTRY "LABELHEIGHT"   DEFAULT 0
      IF ::nLabelHeight < 0
         ::nLabelHeight := 0
      ENDIF
      GET ::nTextBoxHeight        SECTION "FORMMETRICS" ENTRY "TEXTBOXHEIGHT" DEFAULT 0
      IF ::nTextBoxHeight < 0
         ::nTextBoxHeight := 0
      ENDIF
      GET ::nStdVertGap           SECTION "FORMMETRICS" ENTRY "STDVERTGAP"    DEFAULT 24
      IF ::nStdVertGap < 1
         ::nStdVertGap := 24
      ENDIF
      GET ::nPxMove               SECTION "FORMMETRICS" ENTRY "PXMOVE"        DEFAULT 5
      IF ::nPxMove < 1 .OR. ::nPxMove > 99
         ::nPxMove := 5
      ENDIF
      GET ::nPxJump               SECTION "FORMMETRICS" ENTRY "PXJUMP"        DEFAULT 10
      IF ::nPxJump < 1 .OR. ::nPxJump > 99
         ::nPxJump := 10
      ENDIF
      GET ::nPxSize               SECTION "FORMMETRICS" ENTRY "PXSIZE"        DEFAULT 1
      IF ::nPxSize < 1 .OR. ::nPxSize > 99
         ::nPxSize := 1
      ENDIF
      // OOHG
      GET ::cGuiHbMinGW           SECTION 'GUILIB'      ENTRY "GUIHBMINGW"    DEFAULT 'c:\oohg'
      GET ::cGuiHbBCC             SECTION 'GUILIB'      ENTRY "GUIHBBCC"      DEFAULT 'c:\oohg'
      GET ::cGuiHbPelles          SECTION 'GUILIB'      ENTRY "GUIHBPELL"     DEFAULT 'c:\oohg'
      GET ::cGuixHbMinGW          SECTION 'GUILIB'      ENTRY "GUIXHBMINGW"   DEFAULT 'c:\oohg'
      GET ::cGuixHbBCC            SECTION 'GUILIB'      ENTRY "GUIXHBBCC"     DEFAULT 'c:\oohg'
      GET ::cGuixHbPelles         SECTION 'GUILIB'      ENTRY "GUIXHBPELL"    DEFAULT 'c:\oohg'
      GET ::cGuiHbBCCI            SECTION 'GUIHBBCC'    ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cGuiHbBCCL            SECTION 'GUIHBBCC'    ENTRY "LIB"           DEFAULT 'lib\hb\bcc'
      GET ::cGuiHbBCCR            SECTION 'GUIHBBCC'    ENTRY "RESOURCES"     DEFAULT 'resources'
      GET ::cGuiHbMinGWI          SECTION 'GUIHBMINGW'  ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cGuiHbMinGWL          SECTION 'GUIHBMINGW'  ENTRY "LIB"           DEFAULT 'lib\hb\mingw'
      GET ::cGuiHbMinGWR          SECTION 'GUIHBMINGW'  ENTRY "RESOURCES"     DEFAULT 'resources'
      GET ::cGuiHbPellesI         SECTION 'GUIHBPELL'   ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cGuiHbPellesL         SECTION 'GUIHBPELL'   ENTRY "LIB"           DEFAULT 'lib\hb\pcc'
      GET ::cGuiHbPellesR         SECTION 'GUIHBPELL'   ENTRY "RESOURCES"     DEFAULT 'resources'
      GET ::cGuixHbBCCI           SECTION 'GUIXHBBCC'   ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cGuixHbBCCL           SECTION 'GUIXHBBCC'   ENTRY "LIB"           DEFAULT 'lib\xhb\bcc'
      GET ::cGuixHbBCCR           SECTION 'GUIXHBBCC'   ENTRY "RESOURCES"     DEFAULT 'resources'
      GET ::cGuixHbMinGWI         SECTION 'GUIXHBMINGW' ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cGuixHbMinGWL         SECTION 'GUIXHBMINGW' ENTRY "LIB"           DEFAULT 'lib\xhb\mingw'
      GET ::cGuixHbMinGWR         SECTION 'GUIXHBMINGW' ENTRY "RESOURCES"     DEFAULT 'resources'
      GET ::cGuixHbPellesI        SECTION 'GUIXHBPELL'  ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cGuixHbPellesL        SECTION 'GUIXHBPELL'  ENTRY "LIB"           DEFAULT 'lib\xhb\pcc'
      GET ::cGuixHbPellesR        SECTION 'GUIXHBPELL'  ENTRY "RESOURCES"     DEFAULT 'resources'
      // HARBOUR
      GET ::cHbMinGWFolder        SECTION 'HARBOUR'     ENTRY "HBMINGW"       DEFAULT 'c:\harbourm'
      GET ::cHbBCCFolder          SECTION 'HARBOUR'     ENTRY "HBBCC"         DEFAULT 'c:\harbourb'
      GET ::cHbPellFolder         SECTION 'HARBOUR'     ENTRY "HBPELLES"      DEFAULT 'c:\harbourp'
      GET ::cHbMinGWFolderB       SECTION 'HBMINGW'     ENTRY "BIN"           DEFAULT 'bin'
      GET ::cHbMinGWFolderI       SECTION 'HBMINGW'     ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cHbMinGWFolderL       SECTION 'HBMINGW'     ENTRY "LIB"           DEFAULT 'lib'
      GET ::cHbBCCFolderB         SECTION 'HBBCC'       ENTRY "BIN"           DEFAULT 'bin'
      GET ::cHbBCCFolderI         SECTION 'HBBCC'       ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cHbBCCFolderL         SECTION 'HBBCC'       ENTRY "LIB"           DEFAULT 'lib'
      GET ::cHbPellFolderB        SECTION 'HBPELLES'    ENTRY "BIN"           DEFAULT 'bin'
      GET ::cHbPellFolderI        SECTION 'HBPELLES'    ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cHbPellFolderL        SECTION 'HBPELLES'    ENTRY "LIB"           DEFAULT 'lib'
      // XHARBOUR
      GET ::cxHbMinGWFolder       SECTION 'HARBOUR'     ENTRY "XHBMINGW"      DEFAULT 'c:\xharbourm'
      GET ::cxHbBCCFolder         SECTION 'HARBOUR'     ENTRY "XHBBCC"        DEFAULT 'c:\xharbourb'
      GET ::cxHbPellFolder        SECTION 'HARBOUR'     ENTRY "XHBPELLES"     DEFAULT 'c:\xharbourp'
      GET ::cxHbMinGWFolderB      SECTION 'XHBMINGW'    ENTRY "BIN"           DEFAULT 'bin'
      GET ::cxHbMinGWFolderI      SECTION 'XHBMINGW'    ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cxHbMinGWFolderL      SECTION 'XHBMINGW'    ENTRY "LIB"           DEFAULT 'lib'
      GET ::cxHbBCCFolderB        SECTION 'XHBBCC'      ENTRY "BIN"           DEFAULT 'bin'
      GET ::cxHbBCCFolderI        SECTION 'XHBBCC'      ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cxHbBCCFolderL        SECTION 'XHBBCC'      ENTRY "LIB"           DEFAULT 'lib'
      GET ::cxHbPellFolderB       SECTION 'XHBPELLES'   ENTRY "BIN"           DEFAULT 'bin'
      GET ::cxHbPellFolderI       SECTION 'XHBPELLES'   ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cxHbPellFolderL       SECTION 'XHBPELLES'   ENTRY "LIB"           DEFAULT 'lib'
      // C COMPILER
      GET ::cMinGWFolder          SECTION 'COMPILER'    ENTRY "MINGWFOLDER"   DEFAULT 'c:\MinGW'
      GET ::cMinGWFolderB         SECTION 'MINGW'       ENTRY "BIN"           DEFAULT 'bin'
      GET ::cMinGWFolderI         SECTION 'MINGW'       ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cMinGWFolderL         SECTION 'MINGW'       ENTRY "LIB"           DEFAULT 'lib'
      GET ::cBCCFolder            SECTION 'COMPILER'    ENTRY "BCCFOLDER"     DEFAULT 'c:\Borland\BCC55'
      GET ::cBCCFolderB           SECTION 'BCC'         ENTRY "BIN"           DEFAULT 'bin'
      GET ::cBCCFolderI           SECTION 'BCC'         ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cBCCFolderL           SECTION 'BCC'         ENTRY "LIB"           DEFAULT 'lib'
      GET ::cPellFolder           SECTION 'COMPILER'    ENTRY "PELLESFOLDER"  DEFAULT 'c:\PellesC'
      GET ::cPellFolderB          SECTION 'PELLES'      ENTRY "BIN"           DEFAULT 'bin'
      GET ::cPellFolderI          SECTION 'PELLES'      ENTRY "INCLUDE"       DEFAULT 'include'
      GET ::cPellFolderL          SECTION 'PELLES'      ENTRY "LIB"           DEFAULT 'lib'
      // MODE
      GET ::nCompxBase            SECTION 'WHATCOMP'    ENTRY "XBASECOMP"     DEFAULT 1  // 1 Harbour  2 xHarbour
      GET ::nCompilerC            SECTION 'WHATCOMP'    ENTRY "CCOMPILER"     DEFAULT 1  // 1 MinGW    2 BCC   3 Pelles C
      // POSITION
      GET nPos                    SECTION 'POSITION'    ENTRY "FORM_MAIN_ROW" DEFAULT ::aPositions[1, 1]
      IF HB_ISNUMERIC( nPos ) .AND. nPos >= 0
         ::aPositions[1, 1] := nPos
      ENDIF
      GET nPos                    SECTION 'POSITION'    ENTRY "FORM_MAIN_COL" DEFAULT ::aPositions[1, 2]
      IF HB_ISNUMERIC( nPos ) .AND. nPos >= 0
         ::aPositions[1, 2] := nPos
      ENDIF
      GET nPos                    SECTION 'POSITION'    ENTRY "CVCCNTRLS_ROW" DEFAULT ::aPositions[2, 1]
      IF HB_ISNUMERIC( nPos ) .AND. nPos >= 0
         ::aPositions[2, 1] := nPos
      ENDIF
      GET nPos                    SECTION 'POSITION'    ENTRY "CVCCNTRLS_COL" DEFAULT ::aPositions[2, 2]
      IF HB_ISNUMERIC( nPos ) .AND. nPos >= 0
         ::aPositions[2, 2] := nPos
      ENDIF
      GET nPos                    SECTION 'POSITION'    ENTRY "FORM_LIST_ROW" DEFAULT ::aPositions[3, 1]
      IF HB_ISNUMERIC( nPos ) .AND. nPos >= 0
         ::aPositions[3, 1] := nPos
      ENDIF
      GET nPos                    SECTION 'POSITION'    ENTRY "FORM_LIST_COL" DEFAULT ::aPositions[3, 2]
      IF HB_ISNUMERIC( nPos ) .AND. nPos >= 0
         ::aPositions[3, 2] := nPos
      ENDIF
      // IDE
      GET ::nLineSkip             SECTION 'SETTINGS'    ENTRY "LINESKIP"      DEFAULT 5
      GET ::nTBuild               SECTION 'SETTINGS'    ENTRY "BUILD"         DEFAULT 2  // 1=Compile.bat 2=Own Make 3=HbMk2
      IF ::nTBuild < 1 .OR. ( ::nTBuild > 2 .AND. ::nCompxBase == 2 ) .OR. ( ::nTBuild > 3 .AND. ::nCompilerC == 1 )
         ::nTBuild := 2
      ENDIF
      GET lSaveDefaultValues      SECTION 'SETTINGS'    ENTRY "SAVEDEFAULTS"  DEFAULT 1
      ::lSaveDefaultValues := ( lSaveDefaultValues == 1 )
      GET lSavePosOnDrag          SECTION 'SETTINGS'    ENTRY "SAVEPOSONDRAG" DEFAULT 1
      ::lSavePosOnDrag := ( lSavePosOnDrag == 1 )
      GET lSaveSizeonRed          SECTION 'SETTINGS'    ENTRY "SAVESIZEONRED" DEFAULT 1
      ::lSaveSizeonRed := ( lSaveSizeonRed == 1 )
      GET lMultiDelete            SECTION 'SETTINGS'    ENTRY "MULTIDELETE"   DEFAULT 0
      ::lMultiDelete := ( lMultiDelete == 1 )
      GET lSnap                   SECTION 'SETTINGS'    ENTRY "SNAP"          DEFAULT 0
      ::lSnap := ( lSnap == 1 )
      GET ::cLibCC                SECTION 'SETTINGS'    ENTRY "ADDLIBCC"      DEFAULT ''
      GET ::cLibXH                SECTION 'SETTINGS'    ENTRY "ADDLIBXH"      DEFAULT ''
      GET ::cMakeTool             SECTION 'SETTINGS'    ENTRY "MAKETOOL"      DEFAULT ''
      GET ::nSyntax               SECTION 'SETTINGS'    ENTRY "SYNTAX"        DEFAULT 1
      GET lHideTT                 SECTION 'SETTINGS'    ENTRY "HIDETT"        DEFAULT 0
      ::lHideTT := ( lHideTT == 1 )
   END INI
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD SaveINI( cFile ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/

   IF Left( cFile, 1 ) == "\"
      cFile := SubStr( cFile, 2 )
   ENDIF

   BEGIN INI FILE cFile
      // PROJECT
      SET SECTION 'PROJECT'     ENTRY "PROJFOLDER"    TO ::cProjFolder
      SET SECTION 'PROJECT'     ENTRY "OUTFILE"       TO ::cOutFile
      // EDITOR
      SET SECTION "EDITOR"      ENTRY "EXTERNAL"      TO ::cExtEditor
      SET SECTION "EDITOR"      ENTRY "TABSIZE"       TO LTrim( Str( ::nTabSize, 2, 0 ) )
      // FORM'S FONT
      SET SECTION "FORMFONT"    ENTRY "FONT"          TO ::cFormDefFontName
      SET SECTION "FORMFONT"    ENTRY "SIZE"          TO LTrim( Str( ::nFormDefFontSize, 2, 0 ) )
      SET SECTION "FORMFONT"    ENTRY "BOLD"          TO iif( ::lFormDefFontBold, "1", "0" )
      SET SECTION "FORMFONT"    ENTRY "ITALIC"        TO iif( ::lFormDefFontItalic, "1", "0" )
      SET SECTION "FORMFONT"    ENTRY "COLOR"         TO ::cFormDefFontColor
      SET SECTION "FORMFONT"    ENTRY "UNDERLINE"     TO iif( ::lFormDefFontUnderLine, "1", "0" )
      SET SECTION "FORMFONT"    ENTRY "STRIKEOUT"     TO iif( ::lFormDefFontStrikeOut, "1", "0" )
      SET SECTION "FORMFONT"    ENTRY "CHARSET"       TO LTrim( Str( ::nFormDefFontCharSet ) )
      // FORM'S METRICS
      SET SECTION "FORMMETRICS" ENTRY "COLBORDER"     TO LTrim( Str( ::nColBorder, 6, 0) )
      SET SECTION "FORMMETRICS" ENTRY "ROWBORDER"     TO LTrim( Str( ::nRowBorder, 6, 0) )
      SET SECTION "FORMMETRICS" ENTRY "LABELHEIGHT"   TO LTrim( Str( ::nLabelHeight, 2, 0 ) )
      SET SECTION "FORMMETRICS" ENTRY "TEXTBOXHEIGHT" TO LTrim( Str( ::nTextBoxHeight, 2, 0 ) )
      SET SECTION "FORMMETRICS" ENTRY "STDVERTGAP"    TO LTrim( Str( ::nStdVertGap, 3, 0 ) )
      SET SECTION "FORMMETRICS" ENTRY "PXMOVE"        TO LTrim( Str( ::nPxMove, 2, 0 ) )
      SET SECTION "FORMMETRICS" ENTRY "PXJUMP"        TO LTrim( Str( ::nPxJump, 2, 0 ) )
      SET SECTION "FORMMETRICS" ENTRY "PXSIZE"        TO LTrim( Str( ::nPxSize, 2, 0 ) )
      // OOHG
      SET SECTION 'GUILIB'      ENTRY "GUIHBMINGW"    TO ::cGuiHbMinGW
      SET SECTION 'GUILIB'      ENTRY "GUIHBBCC"      TO ::cGuiHbBCC
      SET SECTION 'GUILIB'      ENTRY "GUIHBPELL"     TO ::cGuiHbPelles
      SET SECTION 'GUILIB'      ENTRY "GUIXHBMINGW"   TO ::cGuixHbMinGW
      SET SECTION 'GUILIB'      ENTRY "GUIXHBBCC"     TO ::cGuixHbBCC
      SET SECTION 'GUILIB'      ENTRY "GUIXHBPELL"    TO ::cGuixHbPelles
      SET SECTION 'GUIHBBCC'    ENTRY "INCLUDE"       TO ::cGuiHbBCCI
      SET SECTION 'GUIHBBCC'    ENTRY "LIB"           TO ::cGuiHbBCCL
      SET SECTION 'GUIHBBCC'    ENTRY "RESOURCES"     TO ::cGuiHbBCCR
      SET SECTION 'GUIHBMINGW'  ENTRY "INCLUDE"       TO ::cGuiHbMinGWI
      SET SECTION 'GUIHBMINGW'  ENTRY "LIB"           TO ::cGuiHbMinGWL
      SET SECTION 'GUIHBMINGW'  ENTRY "RESOURCES"     TO ::cGuiHbMinGWR
      SET SECTION 'GUIHBPELL'   ENTRY "INCLUDE"       TO ::cGuiHbPellesI
      SET SECTION 'GUIHBPELL'   ENTRY "LIB"           TO ::cGuiHbPellesL
      SET SECTION 'GUIHBPELL'   ENTRY "RESOURCES"     TO ::cGuiHbPellesR
      SET SECTION 'GUIXHBBCC'   ENTRY "INCLUDE"       TO ::cGuixHbBCCI
      SET SECTION 'GUIXHBBCC'   ENTRY "LIB"           TO ::cGuixHbBCCL
      SET SECTION 'GUIXHBBCC'   ENTRY "RESOURCES"     TO ::cGuixHbBCCR
      SET SECTION 'GUIXHBMINGW' ENTRY "INCLUDE"       TO ::cGuixHbMinGWI
      SET SECTION 'GUIXHBMINGW' ENTRY "LIB"           TO ::cGuixHbMinGWL
      SET SECTION 'GUIXHBMINGW' ENTRY "RESOURCES"     TO ::cGuixHbMinGWR
      SET SECTION 'GUIXHBPELL'  ENTRY "INCLUDE"       TO ::cGuixHbPellesI
      SET SECTION 'GUIXHBPELL'  ENTRY "LIB"           TO ::cGuixHbPellesL
      SET SECTION 'GUIXHBPELL'  ENTRY "RESOURCES"     TO ::cGuixHbPellesR
                     // HARBOUR
      SET SECTION 'HARBOUR'     ENTRY "HBMINGW"       TO ::cHbMinGWFolder
      SET SECTION 'HARBOUR'     ENTRY "HBBCC"         TO ::cHbBCCFolder
      SET SECTION 'HARBOUR'     ENTRY "HBPELLES"      TO ::cHbPellFolder
      SET SECTION 'HBMINGW'     ENTRY "BIN"           TO ::cHbMinGWFolderB
      SET SECTION 'HBMINGW'     ENTRY "INCLUDE"       TO ::cHbMinGWFolderI
      SET SECTION 'HBMINGW'     ENTRY "LIB"           TO ::cHbMinGWFolderL
      SET SECTION 'HBBCC'       ENTRY "BIN"           TO ::cHbBCCFolderB
      SET SECTION 'HBBCC'       ENTRY "INCLUDE"       TO ::cHbBCCFolderI
      SET SECTION 'HBBCC'       ENTRY "LIB"           TO ::cHbBCCFolderL
      SET SECTION 'HBPELLES'    ENTRY "BIN"           TO ::cHbPellFolderB
      SET SECTION 'HBPELLES'    ENTRY "INCLUDE"       TO ::cHbPellFolderI
      SET SECTION 'HBPELLES'    ENTRY "LIB"           TO ::cHbPellFolderL
      // XHARBOUR
      SET SECTION 'HARBOUR'     ENTRY "XHBMINGW"      TO ::cxHbMinGWFolder
      SET SECTION 'HARBOUR'     ENTRY "XHBBCC"        TO ::cxHbBCCFolder
      SET SECTION 'HARBOUR'     ENTRY "XHBPELLES"     TO ::cxHbPellFolder
      SET SECTION 'XHBMINGW'    ENTRY "BIN"           TO ::cxHbMinGWFolderB
      SET SECTION 'XHBMINGW'    ENTRY "INCLUDE"       TO ::cxHbMinGWFolderI
      SET SECTION 'XHBMINGW'    ENTRY "LIB"           TO ::cxHbMinGWFolderL
      SET SECTION 'XHBBCC'      ENTRY "BIN"           TO ::cxHbBCCFolderB
      SET SECTION 'XHBBCC'      ENTRY "INCLUDE"       TO ::cxHbBCCFolderI
      SET SECTION 'XHBBCC'      ENTRY "LIB"           TO ::cxHbBCCFolderL
      SET SECTION 'XHBPELLES'   ENTRY "BIN"           TO ::cxHbPellFolderB
      SET SECTION 'XHBPELLES'   ENTRY "INCLUDE"       TO ::cxHbPellFolderI
      SET SECTION 'XHBPELLES'   ENTRY "LIB"           TO ::cxHbPellFolderL
      // C COMPILER
      SET SECTION 'COMPILER'    ENTRY "MINGWFOLDER"   TO ::cMinGWFolder
      SET SECTION 'COMPILER'    ENTRY "BCCFOLDER"     TO ::cBCCFolder
      SET SECTION 'COMPILER'    ENTRY "PELLESFOLDER"  TO ::cPellFolder
      SET SECTION 'MINGW'       ENTRY "BIN"           TO ::cMinGWFolderB
      SET SECTION 'MINGW'       ENTRY "INCLUDE"       TO ::cMinGWFolderI
      SET SECTION 'MINGW'       ENTRY "LIB"           TO ::cMinGWFolderL
      SET SECTION 'BCC'         ENTRY "BIN"           TO ::cBCCFolderB
      SET SECTION 'BCC'         ENTRY "INCLUDE"       TO ::cBCCFolderI
      SET SECTION 'BCC'         ENTRY "LIB"           TO ::cBCCFolderL
      SET SECTION 'PELLES'      ENTRY "BIN"           TO ::cPellFolderB
      SET SECTION 'PELLES'      ENTRY "INCLUDE"       TO ::cPellFolderI
      SET SECTION 'PELLES'      ENTRY "LIB"           TO ::cPellFolderL
      // MODE
      SET SECTION 'WHATCOMP'    ENTRY "XBASECOMP"     TO LTrim( Str( ::nCompxBase, 1, 0 ) )
      SET SECTION 'WHATCOMP'    ENTRY "CCOMPILER"     TO LTrim( Str( ::nCompilerC, 1, 0 ) )
      // POSITION
      SET SECTION 'POSITION'    ENTRY "FORM_MAIN_ROW" TO LTrim( Str( ::aPositions[1, 1], 6, 0 ) )
      SET SECTION 'POSITION'    ENTRY "FORM_MAIN_COL" TO LTrim( Str( ::aPositions[1, 2], 6, 0 ) )
      SET SECTION 'POSITION'    ENTRY "CVCCNTRLS_ROW" TO LTrim( Str( ::aPositions[2, 1], 6, 0 ) )
      SET SECTION 'POSITION'    ENTRY "CVCCNTRLS_COL" TO LTrim( Str( ::aPositions[2, 2], 6, 0 ) )
      SET SECTION 'POSITION'    ENTRY "FORM_LIST_ROW" TO LTrim( Str( ::aPositions[3, 1], 6, 0 ) )
      SET SECTION 'POSITION'    ENTRY "FORM_LIST_COL" TO LTrim( Str( ::aPositions[3, 2], 6, 0 ) )
      // OTHER
      SET SECTION "SETTINGS"    ENTRY "ADDLIBCC"      TO ::cLibCC
      SET SECTION "SETTINGS"    ENTRY "ADDLIBXH"      TO ::cLibXH
      SET SECTION "SETTINGS"    ENTRY "MAKETOOL"      TO ::cMakeTool
      SET SECTION 'SETTINGS'    ENTRY "LINESKIP"      TO LTrim( Str( ::nLineSkip, 2, 0 ) )
      IF ::nTBuild < 1 .OR. ( ::nTBuild > 2 .AND. ::nCompxBase == 2 ) .OR. ( ::nTBuild > 3 .AND. ::nCompilerC == 1 )
         ::nTBuild := 2
      ENDIF
      SET SECTION "SETTINGS"    ENTRY "BUILD"         TO LTrim( Str( ::nTBuild, 1, 0 ) )
      SET SECTION "SETTINGS"    ENTRY "MULTIDELETE"   TO iif( ::lMultiDelete, "1", "0" )
      SET SECTION "SETTINGS"    ENTRY "SAVEDEFAULTS"  TO iif( ::lSaveDefaultValues, "1", "0" )
      SET SECTION "SETTINGS"    ENTRY "SAVEPOSONDRAG" TO iif( ::lSavePosOnDrag, "1", "0" )
      SET SECTION "SETTINGS"    ENTRY "SAVESIZEONRED" TO iif( ::lSaveSizeonRed, "1", "0" )
      SET SECTION "SETTINGS"    ENTRY "SNAP"          TO iif( ::lSnap, "1", "0" )
      SET SECTION "SETTINGS"    ENTRY "SYNTAX"        TO LTrim( Str( ::nSyntax, 1, 0 ) )
      SET SECTION "SETTINGS"    ENTRY "HIDETT"        TO iif( ::lHideTT, "1", "0" )
   END INI
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Exit() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   IF IsWindowDefined( Form_Edit )
      ::SaveAndExit()
   ENDIF

   DO WHILE Len( ::aEditors ) > 0
      aTail( ::aEditors ):Exit()
      ::nActiveEditor := Len( ::aEditors )
   ENDDO

   IF ! ::lPsave
      IF MsgYesNo( i18n( 'Save the changes made to the project?' ), 'OOHG IDE+' )
         ::SaveProject()
      ENDIF
   ENDIF

   IF IsWindowActive( Form_Tree )
      ::Form_Tree:Release()
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD PrintIt() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL cItem, cParent, cArch

   cItem := ::Form_Tree:Tree_1:Item( ::Form_Tree:Tree_1:Value )
   cParent := ::SearchType( ::Form_Tree:Tree_1:Value )
   IF cParent == "PRG" .AND. cItem # "PRG"
      cArch := MemoRead( cItem + '.prg' )
   ELSE
      IF cParent == "FMG" .AND. cItem # "FMG"
         cArch := MemoRead( cItem + '.fmg' )
      ELSE
         IF cParent == "CH" .AND. cItem # "CH"
            cArch := MemoRead( cItem + '.ch' )
         ELSE
            IF cParent == "RPT" .AND. cItem # "RPT"
               cArch := MemoRead( cItem + '.rpt' )
            ELSE
               IF cParent == "RC" .AND. cItem # "RC"
                  cArch := MemoRead( cItem + '.rc' )
               ELSE
                  MsgInfo( i18n( "This item can't be printed." ), 'OOHG IDE+' )
                  RETURN NIL
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   ::ViewSource( cArch )
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD CompileOptions( nOpt ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/

   DO CASE
   CASE nOpt == 1   // Make only
      IF ::nCompxBase == 1 .AND. ::nCompilerC == 1     // Harbour-MinGW
         ::BuildWithHarbourAndMinGW( 0 )
      ENDIF
      IF ::nCompxBase == 1 .AND. ::nCompilerC == 2     // Harbour-BCC
         ::BuildWithHarbourAndBCC( 0 )
      ENDIF
      IF ::nCompxBase == 1 .AND. ::nCompilerC == 3     // Harbour-PellesC
         ::BuildWithHarbourAndPellesC( 0 )
      ENDIF

      IF ::nCompxBase == 2 .AND. ::nCompilerC == 1     // xHarbour-MinGW
         ::BuildWithxHarbourAndMinGW( 0 )
      ENDIF
      IF ::nCompxBase == 2 .AND. ::nCompilerC == 2     // xHarbour-BCC
         ::BuildWithxHarbourAndBCC(0 )
      ENDIF
      IF ::nCompxBase == 2 .AND. ::nCompilerC == 3     // xHarbour-PellesC
         ::BuildWithxHarbourAndPellesC( 0 )
      ENDIF
   CASE nOpt == 2   // Make and Run
        IF ::nCompxBase == 1 .AND. ::nCompilerC == 1   // Harbour-MinGW
           ::BuildWithHarbourAndMinGW( 1 )
        ENDIF
        IF ::nCompxBase == 1 .AND. ::nCompilerC == 2   // Harbour-BCC
           ::BuildWithHarbourAndBCC( 1 )
        ENDIF
        IF ::nCompxBase == 1 .AND. ::nCompilerC == 3   // Harbour-PellesC
           ::BuildWithHarbourAndPellesC( 1 )
        ENDIF

        IF ::nCompxBase == 2 .AND. ::nCompilerC == 1   // xHarbour-MinGW
           ::BuildWithxHarbourAndMinGW( 1 )
        ENDIF
        IF ::nCompxBase == 2 .AND. ::nCompilerC == 2   // xHarbour-BCC
           ::BuildWithxHarbourAndBCC( 1 )
        ENDIF
        IF ::nCompxBase == 2 .AND. ::nCompilerC == 3   // xHarbour-PellesC
           ::BuildWithxHarbourAndPellesC( 1 )
        ENDIF
   CASE nOpt == 3   // Run only
        ::RunP()
   CASE nOpt == 4   // Debug
        IF ::nCompxBase == 1 .AND. ::nCompilerC == 1   // Harbour-MinGW
           ::BuildWithHarbourAndMinGW( 2 )
        ENDIF
        IF ::nCompxBase == 1 .AND. ::nCompilerC == 2   // Harbour-BCC
           ::BuildWithHarbourAndBCC( 2 )
        ENDIF
        IF ::nCompxBase == 1 .AND. ::nCompilerC == 3   // Harbour-PellesC
           ::BuildWithHarbourAndPellesC( 2 )
        ENDIF

        IF ::nCompxBase == 2 .AND. ::nCompilerC == 1   // xHarbour-MinGW
           ::BuildWithxHarbourAndMinGW( 2 )
        ENDIF
        IF ::nCompxBase == 2 .AND. ::nCompilerC == 2   // xHarbour-BCC
           ::BuildWithxHarbourAndBCC( 2 )
        ENDIF
        IF ::nCompxBase == 2 .AND. ::nCompilerC == 3   // xHarbour-PellesC
           ::BuildWithxHarbourAndPellesC( 2 )
        ENDIF
   ENDCASE
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD RunP() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL cRun

   ::Form_Tree:button_09:Enabled := .F.
   ::Form_Tree:button_10:Enabled := .F.
   ::Form_Tree:button_11:Enabled := .F.

   IF Empty( ::cOutFile )
      cRun := ::cExe
   ELSE
      cRun := ::cOutFile
   ENDIF

   IF File( cRun )
      EXECUTE FILE cRun
   ELSE
      MsgStop( i18n( 'The EXE file could not be found.' ) + CRLF + i18n( 'You must build the project before running it.' ), 'OOHG IDE+' )
   ENDIF

   ::Form_Tree:button_09:Enabled := .T.
   ::Form_Tree:button_10:Enabled := .T.
   ::Form_Tree:button_11:Enabled := .T.
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION PrintItem( cArch )
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL oPrint, nCount, wPage, i

   IF ! HB_IsString ( cArch )
      RETURN NIL
   ENDIF
   oPrint := TPrint( "HBPRINTER" )
   oPrint:Init()
   oPrint:SelPrinter( .T., .T. )
   IF oPrint:lPrError
      MsgStop( i18n( 'Error detected while printing.' ), 'OOHG IDE+' )
      oPrint:Release()
      RETURN NIL
   ENDIF
   oPrint:BeginDoc()
   oPrint:SetPreviewSize( 1 )
   oPrint:BeginPage()
   oPrint:SetCPL( 120 )

   nCount := 1
   oPrint:PrintData( nCount, 0, Replicate( '-', 90 ) )
   wpage := 1
   FOR i := 1 TO MLCount( cArch )
      nCount ++
      oPrint:PrintData( nCount, 0, AllTrim( MemoLine( cArch, 500, i ) ) )
      IF nCount > 60
         nCount ++
         nCount ++
         oPrint:PrintData( nCount, 0, 'Page # ' + Str( wPage, 3 ) )
         nCount ++
         oPrint:PrintData( nCount, 0, Replicate( '-', 90 ) )
         oPrint:EndPage()
         oPrint:BeginPage()
         nCount := 1
         wPage ++
      ENDIF
   NEXT i
   nCount ++
   nCount ++
   oPrint:PrintData( nCount, 0, 'Page # ' + Str( wPage, 3 ) )
   nCount ++
   oPrint:PrintData( nCount, 0, Replicate( '-', 90 ) )
   nCount ++
   oPrint:PrintData( nCount, 0, 'End print ' )
   oPrint:EndPage()
   oPrint:EndDoc()
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD About() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL oForm

   SET INTERACTIVECLOSE ON

   DEFINE WINDOW about_form OBJ oForm ;
      AT 0,0 ;
      WIDTH 520 ;
      HEIGHT 240 ;
      CLIENTAREA ;
      TITLE "About " + APP_FULL_NAME ;
      ICON "IDE_EDIT" ;
      MODAL NOSIZE ;
      BACKCOLOR ::aSystemColor

      @ 20,20 LABEL lbl_1 ;
         VALUE "Created by Ciro Vargas Clemov" ;
         AUTOSIZE ;
         HEIGHT 24

      @ 45,20 LABEL lbl_2 ;
         VALUE "(c) 2002-2014" ;
         AUTOSIZE ;
         HEIGHT 24

      @ 80,20 LABEL lbl_3 ;
         VALUE DQM( "Dedicated to my dear sons: Ciro Andr�s, Santiago and Esteban." ) ;
         AUTOSIZE

      @ 10,330 IMAGE img_Photo ;
         PICTURE "IDE_CVCPHOTO" ;
         WIDTH 83 ;
         HEIGHT 59

      @ 130,20 LABEL lbl_4 ;
         VALUE "Based on the works of Roberto L�pez (MiniGUI's creator)." ;
         AUTOSIZE

      @ 160,20 HYPERLINK hlk_1 ;
         VALUE "Adapted and maintained by OOHG Development Team, (c) 2014-" + LTrim( Str( Year( Date() ) ) ) ;
         ADDRESS 'https://oohg.github.io/' ;
         HEIGHT 24 ;
         AUTOSIZE ;
         HANDCURSOR

      @ 200,20 LABEL lbl_5 ;
         VALUE "Built with " + OOHGVersion() + " " + hb_Compiler() + " " + Version() ;
         AUTOSIZE

      oForm:ClientWidth := oForm:lbl_5:width + 40
      oForm:img_Photo:col := oForm:ClientWidth - oForm:img_Photo:width - 20

      ON KEY ESCAPE ACTION oForm:Release()
   END WINDOW

   CENTER WINDOW about_form
   ACTIVATE WINDOW about_form
   SET INTERACTIVECLOSE OFF
RETURN NIL


/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD DataMan() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   IF ! IsWindowDefined( _dbu )
      DatabaseView1( Self )
   ELSE
      MsgInfo( i18n( 'Data manager is already running.' ), 'OOHG IDE+' )
   ENDIF
   ::Form_Tree:Maximize()
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD SplashDelay() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL iTime

   CursorWait()
   iTime := Seconds()
   DO WHILE ( Seconds() - iTime ) < 1
   ENDDO
   ::Form_Splash:Release()
   ::Form_Tree:Maximize()
   ::Form_Tree:SetFocus()
   ::Form_Tree:BringToTop()
   CursorArrow()
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Preferences() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL Folder
LOCAL aFont := { ::cFormDefFontName, ;
                 ::nFormDefFontSize, ;
                 ::lFormDefFontBold, ;
                 ::lFormDefFontItalic, ;
                 ::StrToColor( ::cFormDefFontColor ), ;
                 ::lFormDefFontUnderLine, ;
                 ::lFormDefFontStrikeOut, ;
                 ::nFormDefFontCharSet }

   _OOHG_TempWindowName := "form_prefer"
   DECLARE WINDOW form_prefer
   #include "form_prefer.fmg"

   ::Form_Prefer := GetFormObject( "Form_prefer" )

   ::Form_Prefer:Backcolor          := ::aSystemColor
   ::Form_Prefer:Title              := i18n( "Preferences from file " ) + ::cProjFolder + '\hmi.ini'
   ::Form_Prefer:text_3:value       := ::cProjFolder
   ::Form_Prefer:text_4:value       := ::cOutFile
   ::Form_Prefer:text_12:value      := ::cGuiHbMinGW
   ::Form_Prefer:text_12_2:value    := ::cGuiHbMinGWI
   ::Form_Prefer:text_12_3:value    := ::cGuiHbMinGWL
   ::Form_Prefer:text_12_4:value    := ::cGuiHbMinGWR
   ::Form_Prefer:text_9:value       := ::cGuiHbBCC
   ::Form_Prefer:text_9_2:value     := ::cGuiHbBCCI
   ::Form_Prefer:text_9_3:value     := ::cGuiHbBCCL
   ::Form_Prefer:text_9_4:value     := ::cGuiHbBCCR
   ::Form_Prefer:text_11:value      := ::cGuiHbPelles
   ::Form_Prefer:text_11_2:value    := ::cGuiHbPellesI
   ::Form_Prefer:text_11_3:value    := ::cGuiHbPellesL
   ::Form_Prefer:text_11_4:value    := ::cGuiHbPellesR
   ::Form_Prefer:text_16:value      := ::cGuixHbMinGW
   ::Form_Prefer:text_16_2:value    := ::cGuixHbMinGWI
   ::Form_Prefer:text_16_3:value    := ::cGuixHbMinGWL
   ::Form_Prefer:text_16_4:value    := ::cGuixHbMinGWR
   ::Form_Prefer:text_17:value      := ::cGuixHbBCC
   ::Form_Prefer:text_17_2:value    := ::cGuixHbBCCI
   ::Form_Prefer:text_17_3:value    := ::cGuixHbBCCL
   ::Form_Prefer:text_17_4:value    := ::cGuixHbBCCR
   ::Form_Prefer:text_18:value      := ::cGuixHbPelles
   ::Form_Prefer:text_18_2:value    := ::cGuixHbPellesI
   ::Form_Prefer:text_18_3:value    := ::cGuixHbPellesL
   ::Form_Prefer:text_18_4:value    := ::cGuixHbPellesR
   ::Form_Prefer:text_8:value       := ::cHbMinGWFolder
   ::Form_Prefer:text_2:value       := ::cHbBCCFolder
   ::Form_Prefer:text_7:value       := ::cHbPellFolder
   ::Form_Prefer:text_8_2:value     := ::cHbMinGWFolderB
   ::Form_Prefer:text_2_2:value     := ::cHbBCCFolderB
   ::Form_Prefer:text_7_2:value     := ::cHbPellFolderB
   ::Form_Prefer:text_8_3:value     := ::cHbMinGWFolderI
   ::Form_Prefer:text_2_3:value     := ::cHbBCCFolderI
   ::Form_Prefer:text_7_3:value     := ::cHbPellFolderI
   ::Form_Prefer:text_8_4:value     := ::cHbMinGWFolderL
   ::Form_Prefer:text_2_4:value     := ::cHbBCCFolderL
   ::Form_Prefer:text_7_4:value     := ::cHbPellFolderL
   ::Form_Prefer:text_13:value      := ::cxHbMinGWFolder
   ::Form_Prefer:text_14:value      := ::cxHbBCCFolder
   ::Form_Prefer:text_15:value      := ::cxHbPellFolder
   ::Form_Prefer:text_13_2:value    := ::cxHbMinGWFolderB
   ::Form_Prefer:text_14_2:value    := ::cxHbBCCFolderB
   ::Form_Prefer:text_15_2:value    := ::cxHbPellFolderB
   ::Form_Prefer:text_13_3:value    := ::cxHbMinGWFolderI
   ::Form_Prefer:text_14_3:value    := ::cxHbBCCFolderI
   ::Form_Prefer:text_15_3:value    := ::cxHbPellFolderI
   ::Form_Prefer:text_13_4:value    := ::cxHbMinGWFolderL
   ::Form_Prefer:text_14_4:value    := ::cxHbBCCFolderL
   ::Form_Prefer:text_15_4:value    := ::cxHbPellFolderL
   ::Form_Prefer:text_libXH:value   := ::cLibXH
   ::Form_Prefer:text_10:value      := ::cMinGWFolder
   ::Form_Prefer:text_10_2:value    := ::cMinGWFolderB
   ::Form_Prefer:text_10_3:value    := ::cMinGWFolderI
   ::Form_Prefer:text_10_4:value    := ::cMinGWFolderL
   ::Form_Prefer:text_5:value       := ::cBCCFolder
   ::Form_Prefer:text_5_2:value     := ::cBCCFolderB
   ::Form_Prefer:text_5_3:value     := ::cBCCFolderI
   ::Form_Prefer:text_5_4:value     := ::cBCCFolderL
   ::Form_Prefer:text_6:value       := ::cPellFolder
   ::Form_Prefer:text_6_2:value     := ::cPellFolderB
   ::Form_Prefer:text_6_3:value     := ::cPellFolderI
   ::Form_Prefer:text_6_4:value     := ::cPellFolderL
   ::Form_Prefer:radiogroup_1:value := ::nCompxBase
   ::Form_Prefer:radiogroup_2:value := ::nCompilerC
   ::Form_Prefer:text_19:value      := ::nLabelHeight
   ::Form_Prefer:text_21:value      := ::nTextBoxHeight
   ::Form_Prefer:text_22:value      := ::nStdVertGap
   ::Form_Prefer:text_23:value      := ::nPxMove
   ::Form_Prefer:text_34:value      := ::nPxJump
   ::Form_Prefer:text_24:value      := ::nPxSize
   ::Form_Prefer:text_25:value      := ::nTabSize
   ::Form_Prefer:text_1:value       := ::cExtEditor
   ::Form_Prefer:text_font:value    := ::cFormDefFontName + " " + ;
                                       LTrim( Str( ::nFormDefFontSize, 2, 0 ) ) + ;
                                       iif( ::lFormDefFontBold, " Bold", "" ) + ;
                                       iif( ::lFormDefFontItalic, " Italic", "" ) + ;
                                       iif( ::lFormDefFontUnderLine, " Underline", "" ) + ;
                                       iif( ::lFormDefFontStrikeOut, " Strikeout", "" ) + ;
                                       ", Color " + ::cFormDefFontColor + ;
                                       ", CharSet " + LTrim( Str( ::nFormDefFontCharSet, 3, 0 ) )
   IF ::Form_Prefer:radiogroup_1:value == 1
      // Harbour
      ::Form_Prefer:radiogroup_4:value := ::nTBuild
   ELSE
      // xHarbour
      ::Form_Prefer:radiogroup_3:value := ::nTBuild
   ENDIF
   ::Form_Prefer:text_make:value     := ::cMakeTool
   ::Form_Prefer:text_libCC:value    := ::cLibCC
   ::Form_Prefer:chk_HideTT:value    := ::lHideTT
   ::Form_Prefer:chk_Snap:value      := ::lSnap
   ::Form_Prefer:combo_26:value      := ::nSyntax
   ::Form_Prefer:text_31:value       := ::nColBorder
   ::Form_Prefer:text_32:value       := ::nRowBorder
   ::Form_Prefer:text_33:value       := ::nLineSkip
   ::Form_Prefer:chk_SaveDefs:value  := ::lSaveDefaultValues
   ::Form_Prefer:chk_SaveDrag:value  := ::lSavePosOnDrag
   ::Form_Prefer:chk_SaveRedim:value := ::lSaveSizeonRed
   ::Form_Prefer:chk_CtrlSizes:value := ::lCtrlSizes
   ::Form_Prefer:chk_MultiDel:value  := ::lMultiDelete
   ::Form_Prefer:tab_1:value         := 1

   ACTIVATE WINDOW Form_Prefer

RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Form_Prefer_OnChange()
/*--------------------------------------------------------------------------------------------------------------------------------*/

   IF ::Form_Prefer:radiogroup_1:value == 1
      // Harbour
      IF ::Form_Prefer:radiogroup_4:value < 1 .OR. ::Form_Prefer:radiogroup_4:value > 3
         IF ::Form_Prefer:radiogroup_3:value < 1 .OR. ::Form_Prefer:radiogroup_3:value > 2
            ::Form_Prefer:radiogroup_4:value := 2
         ELSE
            ::Form_Prefer:radiogroup_4:value := ::Form_Prefer:radiogroup_3:value
         ENDIF
      ENDIF
      ::Form_Prefer:radiogroup_4:visible := .T.
      ::Form_Prefer:radiogroup_3:visible := .F.
   ELSE
      // xHarbour
      IF ::Form_Prefer:radiogroup_3:value < 1 .OR. ::Form_Prefer:radiogroup_3:value > 2
         IF ::Form_Prefer:radiogroup_4:value < 1 .OR. ::Form_Prefer:radiogroup_4:value > 2
            ::Form_Prefer:radiogroup_3:value := 2
         ELSE
            ::Form_Prefer:radiogroup_3:value := ::Form_Prefer:radiogroup_4:value
         ENDIF
      ENDIF
      ::Form_Prefer:radiogroup_3:visible := .T.
      ::Form_Prefer:radiogroup_4:visible := .F.
   ENDIF

                                        // Harbour                                    MinGW
   ::Form_Prefer:label_6:visible     := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_6_1:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_8:visible      := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:button_11:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_6_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_8_2:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_6_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_8_3:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_6_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_8_4:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )

                                        // Harbour                                    BCC
   ::Form_Prefer:label_5:visible     := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_5_1:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_2:visible      := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:button_3:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_5_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_2_2:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_5_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_2_3:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_5_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_2_4:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )

                                        // Harbour                                    Pelles
   ::Form_Prefer:label_4:visible     := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_4_1:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_7:visible      := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:button_2:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_4_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_7_2:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_4_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_7_3:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_4_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_7_4:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )

                                        // xHarbour                                   MinGW
   ::Form_Prefer:label_14:visible    := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_14_1:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_13:visible     := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:button_8:visible    := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_14_2:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_13_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_14_3:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_13_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_14_4:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_13_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )

                                        // xHarbour                                   BCC
   ::Form_Prefer:label_15:visible    := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_15_1:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_14:visible     := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:button_12:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_15_2:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_14_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_15_3:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_14_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_15_4:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_14_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )

                                        // xHarbour                                   Pelles
   ::Form_Prefer:label_16:visible    := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_16_1:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_15:visible     := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:button_13:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_16_2:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_15_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_16_3:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_15_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_16_4:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_15_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )

                                        // MinGW
   ::Form_Prefer:label_8:visible     := ( ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_8_1:visible   := ( ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_10:visible     := ( ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:button_10:visible   := ( ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_8_2:visible   := ( ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_10_2:visible   := ( ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_8_3:visible   := ( ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_10_3:visible   := ( ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_8_4:visible   := ( ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_10_4:visible   := ( ::Form_Prefer:radiogroup_2:value == 1 )

                                        // BCC
   ::Form_Prefer:label_11:visible    := ( ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_11_1:visible  := ( ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_5:visible      := ( ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:button_4:visible    := ( ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_11_2:visible  := ( ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_5_2:visible    := ( ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_11_3:visible  := ( ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_5_3:visible    := ( ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_11_4:visible  := ( ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_5_4:visible    := ( ::Form_Prefer:radiogroup_2:value == 2 )

                                        // Pelles
   ::Form_Prefer:label_13:visible    := ( ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_13_1:visible  := ( ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_6:visible      := ( ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:button_5:visible    := ( ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_13_2:visible  := ( ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_6_2:visible    := ( ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_13_3:visible  := ( ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_6_3:visible    := ( ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_13_4:visible  := ( ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_6_4:visible    := ( ::Form_Prefer:radiogroup_2:value == 3 )

                                        // Harbour                                    MinGW
   ::Form_Prefer:label_10:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_10_1:visible  := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_12:visible     := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:button_9:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_10_2:visible  := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_12_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_10_3:visible  := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_12_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_10_4:visible  := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_12_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 1 )

                                        // Harbour                                    BCC
   ::Form_Prefer:label_7:visible     := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_7_1:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_9:visible      := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:button_6:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_7_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_9_2:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_7_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_9_3:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_7_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_9_4:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 2 )

                                        // Harbour                                    Pelles
   ::Form_Prefer:label_9:visible     := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_9_1:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_11:visible     := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:button_7:visible    := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_9_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_11_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_9_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_11_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_9_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_11_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 1 .AND. ::Form_Prefer:radiogroup_2:value == 3 )

                                        // xHarbour                                   MinGW
   ::Form_Prefer:label_17:visible    := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_17_1:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_16:visible     := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:button_14:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_17_2:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_16_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_17_3:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_16_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:label_17_4:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )
   ::Form_Prefer:text_16_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 1 )

                                        // xHarbour                                   BCC
   ::Form_Prefer:label_18:visible    := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_18_1:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_17:visible     := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:button_15:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_18_2:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_17_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_18_3:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_17_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:label_18_4:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )
   ::Form_Prefer:text_17_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 2 )

                                        // xHarbour                                   Pelles
   ::Form_Prefer:label_19:visible    := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_19_1:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_18:visible     := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:button_16:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_19_2:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_18_2:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_19_3:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_18_3:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:label_19_4:visible  := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )
   ::Form_Prefer:text_18_4:visible   := ( ::Form_Prefer:radiogroup_1:value == 2 .AND. ::Form_Prefer:radiogroup_2:value == 3 )

RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD GetPreferredFont( aFont )
/*--------------------------------------------------------------------------------------------------------------------------------*/
   aFont := GetFont( aFont[1], aFont[2], aFont[3], aFont[4], aFont[5], aFont[6], aFont[7], aFont[8] )
   IF Empty( aFont[1] )
      aFont[1] := ::aOriginalFont[1]
   ENDIF
   IF ! aFont[2] > 0
      aFont[2] := ::aOriginalFont[2]
   ENDIF
   IF ! HB_ISLOGICAL( aFont[3] )
      aFont[3] := ::aOriginalFont[3]
   ENDIF
   IF ! HB_ISLOGICAL( aFont[4] )
      aFont[4] := ::aOriginalFont[4]
   ENDIF
   IF Empty( aFont[5] ) .OR. ! HB_ISARRAY( aFont[5] ) .OR. Len( aFont[5] ) < 3 .OR. ;
      ! HB_ISNUMERIC( aFont[5, 1] ) .OR. aFont[5, 1] < 0 .OR. aFont[5, 1] > 255 .OR. ;
      ! HB_ISNUMERIC( aFont[5, 2] ) .OR. aFont[5, 2] < 0 .OR. aFont[5, 2] > 255 .OR. ;
      ! HB_ISNUMERIC( aFont[5, 3] ) .OR. aFont[5, 3] < 0 .OR. aFont[5, 3] > 255
      aFont[5] := ::StrToColor( ::aOriginalFont[5] )
   ENDIF
   IF ! HB_ISLOGICAL( aFont[6] )
      aFont[6] := ::aOriginalFont[6]
   ENDIF
   IF ! HB_ISLOGICAL( aFont[7] )
      aFont[7] := ::aOriginalFont[7]
   ENDIF
   IF ! HB_ISNUMERIC( aFont[8] )
      aFont[8] := ::aOriginalFont[8]
   ENDIF
   ::Form_prefer:text_font:value := ;
      aFont[1] + " " + ;
      LTrim( Str( aFont[2] ) ) + ;
      iif( aFont[3], " Bold", "" ) + ;
      iif( aFont[4], " Italic", "" ) + ;
      iif( aFont[6], " Underline", "" ) + ;
      iif( aFont[7], " Strikeout", "" ) + ;
      ", Color " + ::ColorToStr( aFont[5] ) + ;
      ", CharSet " + LTrim( Str( aFont[8], 3, 0 ) )
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD SearchText() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   LOCAL cTextSearch, i, nItems,cInput, cOutput, cItem, j

   cTextSearch := inputbox( i18n( 'Text' ), i18n( 'Search text' ) )
   IF len( cTextSearch ) == 0
      RETURN NIL
   ENDIF
   CursorWait()
   ::Form_Wait:hmi_label_101:value := i18n( 'Searching ...' )
   ::Form_Wait:Show()
   nItems := ::Form_Tree:Tree_1:ItemCount
   cOutput := ''
   FOR i:= 1 TO nItems
      cItem := ::Form_Tree:Tree_1:Item( i )
      IF ::SearchType( i ) == "RC" .AND. ! cItem == "RC"
         IF file( cItem + '.rc' )
            cInput := MemoRead( cItem + '.rc' )
            FOR j := 1 TO MLCount( cInput )
               IF At( Upper( cTextSearch ), Upper( Trim( MemoLine( cInput, 500, j ) ) ) ) > 0
                  cOutput += cItem + '  ==> ' + "RC" + '  Line ' + Str( j, 6 ) + CRLF
               ENDIF
            NEXT j
         ENDIF
      ELSEIF ::SearchType( i ) == "CH" .AND. ! cItem == "CH"
         IF File( cItem + '.ch')
            cInput := MemoRead( cItem + '.ch' )
            FOR j := 1 TO MLCount( cInput )
               IF At( Upper( cTextSearch ), Upper( Trim( MemoLine( cInput, 500, j ) ) ) ) > 0
                  cOutput += cItem + '  ==> ' + "CH" + '  Line ' + Str( j, 6 ) + CRLF
               ENDIF
            NEXT j
         ENDIF
      ELSEIF ::SearchType( i ) == "PRG" .AND. ! cItem == "PRG"
         IF file(citem+'.prg')
            cInput:= MemoRead( cItem + '.prg' )
            FOR j := 1 TO MLCount(cInput)
               IF At( Upper( cTextSearch ), Upper( Trim( MemoLine( cInput, 500, j ) ) ) ) > 0
                  cOutput += cItem + '  ==> ' + "PRG" + '  Line ' + Str( j, 6 ) + CRLF
               ENDIF
            NEXT j
         ENDIF
      ELSEIF ::SearchType( i ) == "FMG" .AND. ! cItem == "FMG"
         IF File( citem + '.fmg' )
            cInput := MemoRead( cItem + '.fmg' )
            FOR j := 1 TO MLCount( cInput )
               IF At( Upper( cTextSearch ), Upper( Trim( MemoLine( cInput, 500, j ) ) ) ) > 0
                  cOutput += cItem + '  ==> ' + "FMG" + '  Line ' + Str( j, 6 ) + CRLF
               ENDIF
            NEXT j
         ENDIF
      ELSEIF ::SearchType( i ) == "RPT" .AND. ! cItem == "RPT"
         IF File( citem + '.rpt' )
            cInput := MemoRead( cItem + '.rpt' )
            FOR j := 1 TO MLCount( cInput )
               IF At( Upper( cTextSearch ), Upper( Trim( MemoLine( cInput, 500,j ) ) ) ) > 0
                  cOutput += cItem + '  ==> ' + "RPT" + '  Line ' + Str( j, 6 ) + CRLF
               ENDIF
            NEXT j
         ENDIF
      ENDIF
   NEXT i

   ::Form_Wait:Hide()
   CursorArrow()
   IF coutput == ''
      MsgInfo( i18n( 'Text not found.' ), 'OOHG IDE+' )
   ELSE
      MsgInfo( i18n( "Text found in: " ) + CRLF + coutput, 'OOHG IDE+' )
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD OkPrefer( aFont ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/

   ::cOutFile           := AllTrim( ::Form_Prefer:text_4:Value )
   ::cExtEditor         := AllTrim( ::Form_Prefer:text_1:Value )
   ::cGuiHbMinGW        := ::Form_Prefer:text_12:Value
   ::cGuiHbBCC          := ::Form_Prefer:text_9:Value
   ::cGuiHbBCCI         := ::Form_Prefer:text_9_2:value
   ::cGuiHbBCCL         := ::Form_Prefer:text_9_3:value
   ::cGuiHbBCCR         := ::Form_Prefer:text_9_4:value
   ::cGuiHbMinGWI       := ::Form_Prefer:text_12_2:value
   ::cGuiHbMinGWL       := ::Form_Prefer:text_12_3:value
   ::cGuiHbMinGWR       := ::Form_Prefer:text_12_4:value
   ::cGuiHbPelles       := ::Form_Prefer:text_11:Value
   ::cGuiHbPellesI      := ::Form_Prefer:text_11_2:value
   ::cGuiHbPellesL      := ::Form_Prefer:text_11_3:value
   ::cGuiHbPellesR      := ::Form_Prefer:text_11_4:value
   ::cGuixHbBCC         := ::Form_Prefer:text_17:Value
   ::cGuixHbBCCI        := ::Form_Prefer:text_17_2:value
   ::cGuixHbBCCL        := ::Form_Prefer:text_17_3:value
   ::cGuixHbBCCR        := ::Form_Prefer:text_17_4:value
   ::cGuixHbMinGW       := ::Form_Prefer:text_16:Value
   ::cGuixHbMinGWI      := ::Form_Prefer:text_16_2:value
   ::cGuixHbMinGWL      := ::Form_Prefer:text_16_3:value
   ::cGuixHbMinGWR      := ::Form_Prefer:text_16_4:value
   ::cGuixHbPelles      := ::Form_Prefer:text_18:Value
   ::cGuixHbPellesI     := ::Form_Prefer:text_18_2:value
   ::cGuixHbPellesL     := ::Form_Prefer:text_18_3:value
   ::cGuixHbPellesR     := ::Form_Prefer:text_18_4:value
   ::cHbMinGWFolder     := ::Form_Prefer:text_8:Value
   ::cHbBCCFolder       := ::Form_Prefer:text_2:Value
   ::cHbPellFolder      := ::Form_Prefer:text_7:Value
   ::cHbMinGWFolderB    := ::Form_Prefer:text_8_2:Value
   ::cHbBCCFolderB      := ::Form_Prefer:text_2_2:Value
   ::cHbPellFolderB     := ::Form_Prefer:text_7_2:Value
   ::cHbMinGWFolderI    := ::Form_Prefer:text_8_3:Value
   ::cHbBCCFolderI      := ::Form_Prefer:text_2_3:Value
   ::cHbPellFolderI     := ::Form_Prefer:text_7_3:Value
   ::cHbMinGWFolderL    := ::Form_Prefer:text_8_4:Value
   ::cHbBCCFolderL      := ::Form_Prefer:text_2_4:Value
   ::cHbPellFolderL     := ::Form_Prefer:text_7_4:Value
   ::cxHbMinGWFolder    := ::Form_Prefer:text_13:Value
   ::cxHbBCCFolder      := ::Form_Prefer:text_14:Value
   ::cxHbPellFolder     := ::Form_Prefer:text_15:Value
   ::cxHbMinGWFolderB   := ::Form_Prefer:text_13_2:Value
   ::cxHbBCCFolderB     := ::Form_Prefer:text_14_2:Value
   ::cxHbPellFolderB    := ::Form_Prefer:text_15_2:Value
   ::cxHbMinGWFolderI   := ::Form_Prefer:text_13_3:Value
   ::cxHbBCCFolderI     := ::Form_Prefer:text_14_3:Value
   ::cxHbPellFolderI    := ::Form_Prefer:text_15_3:Value
   ::cxHbMinGWFolderL   := ::Form_Prefer:text_13_4:Value
   ::cxHbBCCFolderL     := ::Form_Prefer:text_14_4:Value
   ::cxHbPellFolderL    := ::Form_Prefer:text_15_4:Value
   ::cMinGWFolder       := ::Form_Prefer:text_10:Value
   ::cMinGWFolderB      := ::Form_Prefer:text_10_2:Value
   ::cMinGWFolderI      := ::Form_Prefer:text_10_3:Value
   ::cMinGWFolderL      := ::Form_Prefer:text_10_4:Value
   ::cBCCFolder         := ::Form_Prefer:text_5:Value
   ::cBCCFolderB        := ::Form_Prefer:text_5_3:Value
   ::cBCCFolderI        := ::Form_Prefer:text_5_3:Value
   ::cBCCFolderL        := ::Form_Prefer:text_5_4:Value
   ::cPellFolder        := ::Form_Prefer:text_6:Value
   ::cPellFolderB       := ::Form_Prefer:text_6_2:Value
   ::cPellFolderI       := ::Form_Prefer:text_6_3:Value
   ::cPellFolderL       := ::Form_Prefer:text_6_4:Value
   ::nCompxBase         := ::Form_Prefer:radiogroup_1:Value
   ::nCompilerC         := ::Form_Prefer:radiogroup_2:Value
   IF ::Form_Prefer:radiogroup_1:value == 1
      // Harbour
      ::nTBuild := ::Form_Prefer:radiogroup_4:value
   ELSE
      // xHarbour
      ::nTBuild := ::Form_Prefer:radiogroup_3:Value
   ENDIF
   ::lHideTT            := ::Form_Prefer:chk_HideTT:Value
   ::lSnap              := ::Form_Prefer:chk_Snap:Value
   ::cLibCC             := AllTrim( ::Form_Prefer:text_libCC:Value )
   ::cLibXH             := AllTrim( ::Form_Prefer:text_libXH:Value )
   ::cMakeTool          := AllTrim( ::Form_Prefer:text_make:Value )
   IF Empty( aFont[1] )
      ::cFormDefFontName      := ::aOriginalFont[1]
      ::nFormDefFontSize      := ::aOriginalFont[2]
      ::lFormDefFontBold      := ::aOriginalFont[3]
      ::lFormDefFontItalic    := ::aOriginalFont[4]
      ::cFormDefFontColor     := ::aOriginalFont[5]
      ::lFormDefFontUnderLine := ::aOriginalFont[6]
      ::lFormDefFontStrikeOut := ::aOriginalFont[7]
      ::nFormDefFontCharSet   := ::aOriginalFont[8]
   ELSE
      ::cFormDefFontName      := aFont[1]
      ::nFormDefFontSize      := iif( aFont[2] > 0, Int( aFont[2] ), ::aOriginalFont[2] )
      ::lFormDefFontBold      := aFont[3]
      ::lFormDefFontItalic    := aFont[4]
      ::cFormDefFontColor     := iif( Empty( aFont[5] ) .OR. ! HB_ISARRAY( aFont[5] ) .OR. Len( aFont[5] ) < 3 .OR. ;
                                      aFont[5, 1] == NIL .OR. aFont[5, 2] == NIL .OR. aFont[5, 3] == NIL, ;
                                      ::aOriginalFont[5], ::ColorToStr( aFont[5] ) )
      ::lFormDefFontUnderLine := aFont[6]
      ::lFormDefFontStrikeOut := aFont[7]
      ::nFormDefFontCharSet   := iif( aFont[8] >= 0, aFont[8], 0 )
   ENDIF
   ::nLabelHeight       := ::Form_Prefer:text_19:Value
   ::nTextBoxHeight     := ::Form_Prefer:text_21:Value
   ::nStdVertGap        := ::Form_Prefer:text_22:Value
   ::nPxMove            := ::Form_Prefer:text_23:Value
   ::nPxJump            := ::Form_Prefer:text_34:Value
   ::nPxSize            := ::Form_Prefer:text_24:Value
   ::nTabSize           := ::Form_Prefer:text_25:Value
   ::nSyntax            := ::Form_Prefer:combo_26:Value
   IF ::nSyntax == 2
      MsgStop( i18n( 'Alternative syntax is not yet implemented.' ), 'OOHG IDE+' )
      ::nSyntax := 1
   ENDIF
   ::nColBorder         := ::Form_Prefer:text_31:value
   IF ::nColBorder < 5
      MsgStop( i18n( 'The right exclusion border was set to 5 px.' ), 'OOHG IDE+' )
      ::nColBorder := 5
   ENDIF
   ::nRowBorder         := ::Form_Prefer:text_32:value
   IF ::nRowBorder < 5
      MsgStop( i18n( 'The bottom exclusion border was set to 5 px.' ), 'OOHG IDE+' )
      ::nRowBorder := 5
   ENDIF
   ::nLineSkip          := ::Form_Prefer:text_33:value
   IF ::nLineSkip < 1
      MsgStop( i18n( 'The number of lines skipped was set to 1.' ), 'OOHG IDE+' )
      ::nLineSkip := 1
   ENDIF
   ::lSaveDefaultValues := ::Form_Prefer:chk_SaveDefs:value
   ::lSavePosOnDrag     := ::Form_Prefer:chk_SaveDrag:value
   ::lSaveSizeonRed     := ::Form_Prefer:chk_SaveRedim:value
   ::lCtrlSizes         := ::Form_Prefer:chk_CtrlSizes:value
   ::lMultiDelete       := ::Form_Prefer:chk_MultiDel:value

   ::Form_Prefer:Release()

   IF Empty( ::cProjectName )
      ::SaveINI( ::cIDE_Folder + '\hmi.ini' )
   ELSE
      ::SaveINI( ::cProjFolder + '\hmi.ini' )
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD BuildWithHarbourAndMinGW( nOption ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   LOCAL aPrgFiles
   LOCAL aRcFiles
   LOCAL cCompFolder  := ::cMinGWFolder + '\'
   LOCAL cCompFolderB := cCompFolder + ::cMinGWFolderB
   LOCAL cCompFolderI := cCompFolder + ::cMinGWFolderI
   LOCAL cCompFolderL := cCompFolder + ::cMinGWFolderL
   LOCAL cDosComm
   LOCAL cError
   LOCAL cError1
   LOCAL cFile
   LOCAL cHarbourFolder  := ::cHbMinGWFolder + '\'
   LOCAL cHarbourFolderB := cHarbourFolder + ::cHbMinGWFolderB
   LOCAL cHarbourFolderI := cHarbourFolder + ::cHbMinGWFolderI
   LOCAL cHarbourFolderL := cHarbourFolder + ::cHbMinGWFolderL
   LOCAL cHBP
   LOCAL cHBMK2
   LOCAL cLine
   LOCAL cMemo
   LOCAL cMiniGuiFolder  := ::cGUIHbMinGW + '\'
   LOCAL cMiniGuiFolderI := cMiniGuiFolder + ::cGUIHbMinGWI
   LOCAL cMiniGuiFolderL := cMiniGuiFolder + ::cGUIHbMinGWL
   LOCAL cMiniGuiFolderR := cMiniGuiFolder + ::cGUIHbMinGWR
   LOCAL cOut
   LOCAL cPrgName
   LOCAL cFolder := ::cProjFolder + '\'
   LOCAL i
   LOCAL nItems
   LOCAL nLineCount
   LOCAL nPrgFiles

   ::Form_Tree:button_09:Enabled := .F.
   ::Form_Tree:button_10:Enabled := .F.
   ::Form_Tree:button_11:Enabled := .F.
   CursorWait()
   ::Form_Wait:hmi_label_101:Value := i18n( 'Building ...' )
   ::Form_Wait:Show()

   BEGIN SEQUENCE
      // Check folders
      IF Empty( ::cProjectName )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'You must save the project before building it.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cCompFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The MinGW folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ::nTBuild == 2 .AND. ! File( ::cMakeTool ) .AND. ! IsFileInPath( ::cMakeTool )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Make tool " + ::cMakeTool + " was not found." ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cMiniGuiFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The ooHG-Hb-MinGW folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderI + "\OOHG.CH" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's INCLUDE subfolder error: can't find " ) + cMiniGuiFolderI + "\OOHG.CH", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderL + "\LIBOOHG.A" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's LIB subfolder error: can't find " ) + cMiniGuiFolderL + "\LIBOOHG.A", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderR + "\OOHG.RC" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's RESOURCES subfolder error: can't find " ) + cMiniGuiFolderR + "\OOHG.RC", 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cHarbourFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The Harbour-MinGW folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderB + "\HARBOUR.EXE" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Harbour's BIN subfolder error: can't find " ) + cHarbourFolderB + "\HARBOUR.EXE", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderI + "\HBAPI.H" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Harbour's INCLUDE subfolder error: can't find " ) + cHarbourFolderI + "\HBAPI.H", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderL + "\LIBHBRTL.A" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Harbour's LIB subfolder error: can't find " ) + cHarbourFolderL + "\LIBHBRTL.A", 'OOHG IDE+' )
         Break
      ENDIF

      // Prepare to build
      SetCurrentFolder( cFolder )
      BorraTemp( cFolder )

      DO CASE
      CASE ::nTBuild == 3    // HBMK2
         // Check for hbp file
         cHBP := StrTran( AllTrim( DelExt( DelPath( ::cProjectName ) ) ), " ", "_" ) + '.hbp'
         IF ! File( cHBP )
            ::Form_Wait:Hide()
            MsgInfo( cHBP + i18n( " was not found in the project's folder." ), 'OOHG IDE+' )
            Break
         ENDIF
         // Check for hbmk2.exe file
         IF File( 'hbmk2.exe' ) .OR. IsFileInPath( 'hbmk2.exe' )
            cHBMK2 := 'hbmk2.exe'
         ELSEIF File( cHarbourFolderB + "\hbmk2.exe" )
            cHBMK2 := cHarbourFolderB + "\hbmk2.exe"
         ELSE
            ::Form_Wait:Hide()
            MsgInfo( i18n( "Can't find: " ) + "HBMK2.EXE", 'OOHG IDE+' )
            Break
         ENDIF
         // Get EXE filename (search for -o<outname> or name of first prg)
         cMemo := MemoRead( cHBP )
         nLineCount := MLCount( cMemo )
         ::cExe := ""
         FOR i := 1 TO nLineCount
            cLine := AllTrim( MemoLine( cMemo, 1200, i ) )
            IF Upper( Left( cLine, 2 ) ) == "-O"
               ::cExe := SubStr( cLine, 3 )
               EXIT
            ENDIF
         NEXT i
         IF Empty( ::cExe )
            FOR i := 1 TO nLineCount
               cLine := AllTrim( MemoLine( cMemo, 1200, i ) )
               IF Upper( Right( cLine, 4 ) ) == ".PRG"
                  ::cExe := Left( cLine, Len( cLine ) - 4 )
                  EXIT
               ENDIF
            NEXT i
         ENDIF
         IF Empty( ::cExe )
            IF Empty( ::cOutFile )
               MsgStop( i18n( 'EXE filename can not be determined.' ) + CRLF + i18n( 'Change the .hbp file or set "Output Name" field at "File/Preferences" menu.' ), 'OOHG IDE+' )
               Break
            ELSE
               IF ! Upper( Right( ::cOutFile, 4 ) ) == ".EXE"
                  ::cOutFile += ".EXE"
               ENDIF
            ENDIF
         ELSE
            IF ! Upper( Right( ::cExe, 4 ) ) == ".EXE"
               ::cExe += ".EXE"
            ENDIF
         ENDIF
         // Delete previous EXE
         IF File( ::cExe )
            DELETE FILE ( ::cExe )
         ENDIF
         IF File( ::cOutFile )
            DELETE FILE ( ::cOutFile )
         ENDIF
         IF File( ::cExe ) .or. File( ::cOutFile )
            ::Form_Wait:Hide()
            MsgInfo( i18n( 'Is EXE running?' ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build batch to launch HBMK2 utility
         cOut := ''
         cOut += '@echo off' + CRLF
         cOut += 'set "HG_TPATH=%PATH%"' + CRLF
         cOut += 'set "PATH=' + cHarbourFolderB + ";" + cCompFolderB + ';%PATH%"' + CRLF
         cOut += cHBMK2 + ' ' + cHBP + ' > error.txt 2>&1' + CRLF
         cOut += 'if errorlevel 1 echo ERROR BUILDING ' + cHBP + ' >> error.txt' + CRLF
         cOut += 'set "PATH=%HG_TPATH%"' + CRLF
         cOut += 'set HG_TPATH=' + CRLF
         hb_MemoWrit( '_build.bat', cOut )
         // Compile and link
         EXECUTE FILE '_build.bat' WAIT HIDE

      CASE ::nTBuild == 2    // Own Make
         // Build list of source files
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         aRcFiles := {}
         FOR i := 1 TO nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ELSEIF ::SearchType( ::SearchItem( cFile, "RC" ) ) == "RC" .AND. ! cFile == "RC"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aRcFiles, cFile ) == 0
                  aAdd( aRcFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         ::cExe := aPrgFiles[1] + ".EXE"
         // Build make script
         // Variables
         cOut := ''
         cOut += 'PATH          = ' + cCompFolderB + CRLF
         cOut += 'PROJECTFOLDER = ' + DelSlash( cFolder ) + CRLF
         cOut += 'APP_NAME      = ' + ::cExe + CRLF
         cOut += 'OBJ_DIR       = ' + cFolder + 'OBJ' + CRLF
         cOut += 'OBJECTS       = '
         FOR i := 1 TO nPrgFiles
            cOut += '\' + CRLF + '$(OBJ_DIR)\' + aPrgFiles[i] + '.o '
         NEXT i
         cOut += '\' + CRLF + '$(OBJ_DIR)\_temp.o'
         cOut += CRLF
         cOut += 'LINK_EXE      = GCC.EXE' + CRLF
         cOut += 'LINK_FLAGS    = -Wall -mwindows -O3 -Wl,--allow-multiple-definition' + CRLF
         cOut += 'LINK_SEARCH   = -L' + DelSlash( cFolder ) + ;
                                ' -L' + cCompFolderL + ;
                                ' -L' + cHarbourFolderL + ;
                                ' -L' + cMiniGUIFolderL + CRLF
         cOut += 'LINK_LIBS     = -Wl,--start-group -loohg -lhbprinter -lminiprint -lbostaurus -lgtgui -lgtwin ' + ;
                                  '-lhbsix -lhbvm -lhbrdd -lhbmacro -lhbmemio -lhbpp -lhbrtl -lhbzebra -lhbziparc ' + ;
                                  '-lhblang -lhbcommon -lhbnulrdd -lrddntx -lrddcdx -lrddfpt -lhbct -lhbmisc -lrddsql -lsddodbc ' + ;
                                  '-lodbc32 -lhbwin -lhbcpage -lhbmzip -lminizip -lhbzlib -lhbtip -lhbpcre -luser32 -lwinspool -lcomctl32 ' + ;
                                  '-lcomdlg32 -lgdi32 -lole32 -loleaut32 -luuid -lwinmm -lvfw32 -lwsock32 -lws2_32 -lmsimg32 ' + ;
                                  iif( nOption == 2, '-lgtwin ', '' ) + ;
                                  iif( ! Empty( ::cLibXH ), ::cLibXH + ' ', '' ) + ;
                                  iif( ! Empty( ::cLibCC ), ::cLibCC + ' ', '' ) + ;
                                  '-Wl,--end-group' + CRLF
         cOut += 'CC_EXE        = GCC.EXE' + CRLF
         cOut += 'CC_FLAGS      = -Wall -mwindows -O3' + CRLF
         cOut += 'CC_SEARCH     = -I' + DelSlash( cFolder ) + ;
                                ' -I' + cCompFolderI + ;
                                ' -I' + cHarbourFolderI + ;
                                ' -I' + cMiniGUIFolderI + CRLF
         cOut += 'HRB_EXE       = ' + cHarbourFolderB + '\HARBOUR.EXE' + CRLF
         cOut += 'HRB_FLAGS     = -n -q ' + iif( nOption == 2, "-b ", "" ) + CRLF
         cOut += 'HRB_SEARCH    = -i' + DelSlash( cFolder ) + ;
                                ' -i' + cHarbourFolderI + ;
                                ' -i' + cMiniGUIFolderI + CRLF
         cOut += 'RC_COMP       = WINDRES.EXE' + CRLF
         cOut += CRLF
         // Rule for .exe building
         cOut += '$(APP_NAME) : $(OBJECTS)' + CRLF
         cOut += HTAB + '$(LINK_EXE) $(LINK_FLAGS) -o$(APP_NAME) $^ $(LINK_SEARCH) $(LINK_LIBS)' + CRLF
         cOut += CRLF
         // Rule for .c compiling
         FOR i := 1 TO nPrgFiles
            cOut += '$(OBJ_DIR)\' + aPrgFiles[i] + '.o : $(OBJ_DIR)\' + aPrgFiles[i] + '.c' + CRLF
            cOut += HTAB + '$(CC_EXE) $(CC_FLAGS) $(CC_SEARCH) -c $^ -o $@' + CRLF
            cOut += HTAB + '@echo #' + CRLF
            cOut += CRLF
         NEXT i
         // Rule for .prg compiling
         FOR i := 1 TO nPrgFiles
            cOut += '$(OBJ_DIR)\' + aPrgFiles[i] + '.c : $(PROJECTFOLDER)\' + aPrgFiles[i] + '.prg' + CRLF
            cOut += HTAB + '$(HRB_EXE) $^ $(HRB_FLAGS) $(HRB_SEARCH) -o$@' + CRLF
            cOut += HTAB + '@echo #' + CRLF
            cOut += CRLF
         NEXT i
         // Rule for .rc compiling
         cOut += '$(OBJ_DIR)\_temp.o : $(PROJECTFOLDER)\_temp.rc' + CRLF
         cOut += HTAB + '$(RC_COMP) -I $(INCLUDE_DIR) -i $^ -o $@' + CRLF
         cOut += HTAB + '@echo #' + CRLF
         hb_MemoWrit( 'makefile.gcc', cOut )
         // Delete previous EXE
         IF File( ::cExe )
            DELETE FILE ( ::cExe )
         ENDIF
         IF File( ::cOutFile )
            DELETE FILE ( ::cOutFile )
         ENDIF
         IF File( ::cExe ) .or. File( ::cOutFile )
            ::Form_Wait:Hide()
            MsgInfo( i18n( 'Is EXE running?' ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build batch to create RC temp file and launch make utility
         cOut := ''
         cOut += '@echo off' + CRLF
         cOut += 'echo #define oohgpath ' + cMiniGUIFolderR + ' > ' + cFolder + '_oohg_resconfig.h' + CRLF
         cOut += 'echo. > "' + cMiniGUIFolderR + '\filler"' + CRLF
         cOut += 'copy /b "' + cMiniGUIFolderR + '\oohg.rc" _temp.rc > NUL' + CRLF
         FOR i := 1 TO Len( aRcFiles )
            cOut += 'copy /b "' + cMiniGUIFolderR + '\filler" + _temp.rc _aux.rc > NUL' + CRLF
            cOut += 'copy /b "' + aRcFiles[ i ] + '.rc" + _aux.rc _temp.rc > NUL' + CRLF
         NEXT i
         cOut += ::cMakeTool + ' -f makefile.gcc > error.txt 2>&1' + CRLF
         hb_MemoWrit( '_build.bat', cOut )
         // Create temp folder for objects
         CreateFolder( cFolder + 'OBJ' )
         // Compile and link
         EXECUTE FILE '_build.bat' WAIT HIDE

      CASE ::nTBuild == 1 // Compile.bat
         // Check for compile file
         IF ! File( 'compile.bat' ) .AND. ! IsFileInPath( 'compile.bat' )
            ::Form_Wait:Hide()
            MsgInfo( i18n( "Copy OOHG's COMPILE.BAT to the project's folder or add OOHG root folder to PATH." ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build auxiliary source file
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         FOR i := 1 TO nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile + '.PRG' ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  // Ignore duplicated files
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         cOut := ''
         FOR i := 1 TO nPrgFiles
            cOut += "#include '" + aPrgFiles[i] + "'" + CRLF + CRLF
         NEXT i
         cPrgName := TempFile()   // Unique file name without extension
         hb_MemoWrit( cPrgName + '.prg', cOut )
         // Delete previous EXE
         ::cExe := StrTran( AllTrim( DelExt( DelPath( ::cProjectName ) ) ), " ", "_" ) + '.EXE'
         IF File( ::cExe )
            DELETE FILE ( ::cExe )
         ENDIF
         IF File( ::cOutFile )
            DELETE FILE ( ::cOutFile )
         ENDIF
         IF File( ::cExe ) .OR. File( ::cOutFile )
            ::Form_Wait:Hide()
            MsgInfo( i18n( 'Is EXE running?' ), 'OOHG IDE+' )
            Break
         ENDIF
         // Compile and link
         cDosComm := 'CMD.EXE /c compile ' + cPrgName + ' /nr /l' + iif( nOption == 2, " /d", "" )
         EXECUTE FILE cDosComm WAIT HIDE
         DELETE FILE ( cPrgName + '.prg' )
         IF File( cPrgName + '.EXE' )
            RENAME ( cPrgName + '.EXE' ) TO ( ::cExe )
            IF ! File( ::cExe )
               MsgInfo( i18n( "EXE file can't be renamed as " + ::cExe ) + CRLF + i18n( 'Current name is ' + cPrgName + '.EXE' ), 'OOHG IDE+' )
            ENDIF
         ENDIF
      ENDCASE

      // Check for errors
      cError := hb_OEMToANSI( MemoRead( 'error.txt' ) ) // TODO: check hb_OEMToANSI
      cError1 := Upper( cError )
      IF At( ' ERROR ', cError1 ) > 0 .OR. At( ' ERROR:', cError1 ) > 0 .OR. At( 'LD RETURNED 1 EXIT STATUS', cError1 ) > 0
         ::Form_Wait:Hide()
         ::ViewErrors( cError )
         Break
      ELSEIF ! File( ::cExe )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Error building " ) + DQM( ::cExe ), 'OOHG IDE+' )
         Break
      ENDIF

      // Rename or move
      cOut := Upper( AllTrim( ::cOutFile ) )
      IF ! Empty( ::cOutFile )
         IF Right( cOut, 4 ) != ".EXE"
            cOut += ".EXE"
         ENDIF
         ::cOutFile := cOut
         IF ! ::cExe == ::cOutFile
            cDosComm := 'CMD.EXE /c move /y ' + ::cExe + " " + cOut
            EXECUTE FILE cDosComm WAIT HIDE
            IF ! File( cOut )
               ::Form_Wait:Hide()
               MsgStop( i18n( "Can't move or rename EXE file." ), 'OOHG IDE+' )
               Break
            ENDIF
            ::cExe := cOut
         ENDIF
      ENDIF

      // Cleanup
      BorraTemp( cFolder )
      ::Form_Wait:Hide()
      IF nOption == 0
         MsgInfo( i18n( 'Project builded.' ), 'OOHG IDE+' )
      ELSEIF nOption == 1 .or. nOption == 2
         ::RunP()
      ENDIF
   END SEQUENCE

   CursorArrow()
   ::Form_Tree:button_09:Enabled := .T.
   ::Form_Tree:button_10:Enabled := .T.
   ::Form_Tree:button_11:Enabled := .T.
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD BuildWithxHarbourAndMinGW( nOption ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   LOCAL aPrgFiles
   LOCAL aRcFiles
   LOCAL cCompFolder  := ::cMinGWFolder + '\'
   LOCAL cCompFolderB := cCompFolder + ::cMinGWFolderB
   LOCAL cCompFolderI := cCompFolder + ::cMinGWFolderI
   LOCAL cCompFolderL := cCompFolder + ::cMinGWFolderL
   LOCAL cDosComm
   LOCAL cError
   LOCAL cError1
   LOCAL cFile
   LOCAL cHarbourFolder  := ::cxHbMinGWFolder + '\'
   LOCAL cHarbourFolderB := cHarbourFolder + ::cxHbMinGWFolderB
   LOCAL cHarbourFolderI := cHarbourFolder + ::cxHbMinGWFolderI
   LOCAL cHarbourFolderL := cHarbourFolder + ::cxHbMinGWFolderL
   LOCAL cMiniGuiFolder  := ::cGUIxHbMinGW + '\'
   LOCAL cMiniGuiFolderI := cMiniGuiFolder + ::cGUIxHbMinGWI
   LOCAL cMiniGuiFolderL := cMiniGuiFolder + ::cGUIxHbMinGWL
   LOCAL cMiniGuiFolderR := cMiniGuiFolder + ::cGUIxHbMinGWR
   LOCAL cOut
   LOCAL cPrgName
   LOCAL cFolder := ::cProjFolder + '\'
   LOCAL i
   LOCAL nItems
   LOCAL nPrgFiles

   ::Form_Tree:button_09:Enabled := .F.
   ::Form_Tree:button_10:Enabled := .F.
   ::Form_Tree:button_11:Enabled := .F.
   CursorWait()
   ::Form_Wait:hmi_label_101:Value := i18n( 'Compiling ...' )
   ::Form_Wait:Show()

   BEGIN SEQUENCE
      // Check folders
      IF Empty( ::cProjectName )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'You must save the project before building it.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cCompFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The MinGW folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ::nTBuild == 2 .AND. ! File( ::cMakeTool ) .AND. ! IsFileInPath( ::cMakeTool )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Make tool " + ::cMakeTool + " was not found." ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cMiniGuiFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The ooHG-xHb-MinGW folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderI + "\OOHG.CH" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's INCLUDE subfolder error: can't find " ) + cMiniGuiFolderI + "\OOHG.CH", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderL + "\LIBOOHG.A" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's LIB subfolder error: can't find " ) + cMiniGuiFolderL + "\LIBOOHG.A", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderR + "\OOHG.RC" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's RESOURCES subfolder error: can't find " ) + cMiniGuiFolderR + "\OOHG.RC", 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cHarbourFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The xHarbour-MinGW folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderB + "\HARBOUR.EXE" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "xHarbour's BIN subfolder error: can't find " ) + cHarbourFolderB + "\HARBOUR.EXE", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderI + "\HBAPI.H" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "xHarbour's INCLUDE subfolder error: can't find " ) + cHarbourFolderI + "\HBAPI.H", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderL + "\LIBRTL.A" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "xHarbour's LIB subfolder error: can't find " ) + cHarbourFolderL + "\LIBRTL.A", 'OOHG IDE+' )
         Break
      ENDIF

      // Prepare to build
      SetCurrentFolder( cFolder )
      BorraTemp( cFolder )
      IF File( ::cExe )
         DELETE FILE ( ::cExe )
      ENDIF
      IF File( ::cOutFile )
         DELETE FILE ( ::cOutFile )
      ENDIF
      IF File( ::cExe ) .or. File( ::cOutFile )
         ::Form_Wait:Hide()
         MsgInfo( i18n( 'Is EXE running?' ), 'OOHG IDE+' )
         Break
      ENDIF
      ::cExe := cPrgName + '.exe'
      IF File( ::cExe )
         DELETE FILE ( ::cExe )
      ENDIF
      IF File( ::cExe )
         ::Form_Wait:Hide()
         MsgInfo( i18n( 'Is EXE running?' ), 'OOHG IDE+' )
         Break
      ENDIF

      DO CASE
      CASE ::nTBuild == 2    // Own Make
         // Build list of source files
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         aRcFiles := {}
         For i := 1 To nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ELSEIF ::SearchType( ::SearchItem( cFile, "RC" ) ) == "RC" .AND. ! cFile == "RC"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aRcFiles, cFile ) == 0
                  aAdd( aRcFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build make script
         // Variables
         cOut := ''
         cOut += 'PATH          = ' + cCompFolderB + CRLF
         cOut += 'PROJECTFOLDER = ' + DelSlash( cFolder ) + CRLF
         cOut += 'APP_NAME      = ' + ::cExe + CRLF
         cOut += 'OBJ_DIR       = ' + cFolder + 'OBJ' + CRLF
         cOut += 'OBJECTS       = '
         FOR i := 1 TO nPrgFiles
            cOut += '\' + CRLF + '$(OBJ_DIR)\' + aPrgFiles[i] + '.o '
         NEXT i
         cOut += '\' + CRLF + '$(OBJ_DIR)\_temp.o'
         cOut += CRLF
         cOut += 'LINK_EXE      = GCC.EXE' + CRLF
         cOut += 'LINK_FLAGS    = -Wall -mwindows -O3 -Wl,--allow-multiple-definition' + CRLF
         cOut += 'LINK_SEARCH   = -L' + DelSlash( cFolder ) + ;
                                ' -L' + cCompFolderL + ;
                                ' -L' + cHarbourFolderL + ;
                                ' -L' + cMiniGUIFolderL + CRLF
         cOut += 'LINK_LIBS     = -Wl,--start-group -looHG -lhbprinter -lminiprint -lbostaurus -lgtgui -lgtwin ' + ;
                                  '-lhbsix -lhbvm -lhbrdd -lhbmacro -lhbmemio -lhbpp -lhbrtl -lhbzebra -lhbziparc ' + ;
                                  '-lhblang -lhbcommon -lhbnulrdd -lrddntx -lrddcdx -lrddfpt -lhbct -lhbmisc -lrddsql -lsddodbc ' + ;
                                  '-lodbc32 -lhbwin -lhbcpage -lhbmzip -lminizip -lhbzlib -lhbtip -lhbpcre -luser32 -lwinspool -lcomctl32 ' + ;
                                  '-lcomdlg32 -lgdi32 -lole32 -loleaut32 -luuid -lwinmm -lvfw32 -lwsock32 -lws2_32 -lmsimg32 ' + ;
                                  iif( nOption == 2, '-lgtwin ', '' ) + ;
                                  iif( ! Empty( ::cLibXH ), ::cLibXH + ' ', '' ) + ;
                                  iif( ! Empty( ::cLibCC ), ::cLibCC + ' ', '' ) + ;
                                  '-Wl,--end-group' + CRLF
         cOut += 'CC_EXE        = GCC.EXE' + CRLF
         cOut += 'CC_FLAGS      = -Wall -mwindows -O3' + CRLF
         cOut += 'CC_SEARCH     = -I' + DelSlash( cFolder ) + ;
                                ' -I' + cCompFolderI + ;
                                ' -I' + cHarbourFolderI + ;
                                ' -I' + cMiniGUIFolderI + CRLF
         cOut += 'HRB_EXE       = ' + cHarbourFolderB + '\HARBOUR.EXE' + CRLF
         cOut += 'HRB_FLAGS     = -n -q ' + iif( nOption == 2, "-b ", "" ) + CRLF
         cOut += 'HRB_SEARCH    = -i' + DelSlash( cFolder ) + ;
                                ' -i' + cHarbourFolderI + ;
                                ' -i' + cMiniGUIFolderI + CRLF
         cOut += 'RC_COMP       = WINDRES.EXE' + CRLF
         cOut += CRLF
         // Rule for .exe building
         cOut += '$(APP_NAME) : $(OBJECTS)' + CRLF
         cOut += HTAB + '$(LINK_EXE) $(LINK_FLAGS) -o$(APP_NAME) $^ $(LINK_SEARCH) $(LINK_LIBS)' + CRLF
         cOut += CRLF
         // Rule for .c compiling
         For i := 1 To nPrgFiles
            cOut += '$(OBJ_DIR)\' + aPrgFiles[i] + '.o : $(OBJ_DIR)\' + aPrgFiles[i] + '.c' + CRLF
            cOut += HTAB + '$(CC_EXE) $(CC_FLAGS) $(CC_SEARCH) -c $^ -o $@' + CRLF
            cOut += HTAB + '@echo #' + CRLF
            cOut += CRLF
         NEXT i
         // Rule for .prg compiling
         For i := 1 To nPrgFiles
            cOut += '$(OBJ_DIR)\' + aPrgFiles[i] + '.c : $(PROJECTFOLDER)\' + aPrgFiles[i] + '.prg' + CRLF
            cOut += HTAB + '$(HRB_EXE) $^ $(HRB_FLAGS) $(HRB_SEARCH) -o$@' + CRLF
            cOut += HTAB + '@echo #' + CRLF
            cOut += CRLF
         NEXT i
         // Rule for .rc compiling
         cOut += '$(OBJ_DIR)\_temp.o : $(PROJECTFOLDER)\_temp.rc' + CRLF
         cOut += HTAB + '$(RC_COMP) -I $(INCLUDE_DIR) -i $^ -o $@' + CRLF
         cOut += HTAB + '@echo #' + CRLF
         hb_MemoWrit( 'makefile.gcc', cOut )
         // Build batch to create RC temp file and launch make utility
         cOut := ''
         cOut += '@echo off' + CRLF
         cOut += 'echo #define oohgpath ' + cMiniGUIFolderR + ' > ' + cFolder + '_oohg_resconfig.h' + CRLF
         cOut += 'echo. > "' + cMiniGUIFolderR + '\filler"' + CRLF
         cOut += 'copy /b "' + cMiniGUIFolderR + '\oohg.rc" _temp.rc > NUL' + CRLF
         FOR i := 1 TO Len( aRcFiles )
            cOut += 'copy /b "' + cMiniGUIFolderR + '\filler" + _temp.rc _aux.rc > NUL' + CRLF
            cOut += 'copy /b "' + aRcFiles[ i ] + '.rc" + _aux.rc _temp.rc > NUL' + CRLF
         NEXT i
         cOut += ::cMakeTool + ' -f makefile.gcc > error.txt 2>&1' + CRLF
         hb_MemoWrit( '_build.bat', cOut )
         // Create temp folder for objects
         CreateFolder( cFolder + 'OBJ' )
         // Compile and link
         EXECUTE FILE '_build.bat' WAIT HIDE
      CASE ::nTBuild == 1 // Compile.bat
         // Check for compile file
         IF ! File( 'compile.bat' ) .AND. ! IsFileInPath( 'compile.bat' )
            ::Form_Wait:Hide()
            MsgInfo( i18n( "Copy OOHG's COMPILE.BAT to the project's folder or add OOHG root folder to PATH." ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build auxiliary source file
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         For i := 1 To nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile + '.PRG' ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         cOut := ''
         For i := 1 To nPrgFiles
            cOut += "#include '" + aPrgFiles[i] + "'" + CRLF + CRLF
         NEXT i
         cPrgName := StrTran( AllTrim( DelExt( DelPath( ::cProjectName ) ) ), " ", "_" )
         hb_MemoWrit( cPrgName + '.prg', cOut )

         // Compile and link
         cDosComm := 'CMD.EXE /c compile ' + cPrgName + ' /nr /l' + iif( nOption == 2, " /d", "" )
         EXECUTE FILE cDosComm WAIT HIDE
         DELETE FILE ( cPrgName + '.prg' )
      ENDCASE

      // Check for errors
      cError := MemoRead( 'error.txt' )
      cError1 := Upper( cError )
      IF At( ' ERROR ', cError1 ) > 0 .OR. At( ' FATAL ', cError1 ) > 0 .OR. At( 'LD RETURNED 1 EXIT STATUS', cError1 ) > 0
         ::Form_Wait:Hide()
         ::ViewErrors( cError )
         Break
      ELSEIF ! File( ::cExe )
         ::Form_Wait:Hide()
         MsgStop( i18n( "File is missing: " ) + DQM( ::cExe ), 'OOHG IDE+' )
         Break
      ENDIF

      // Rename or move
      cOut := Upper( AllTrim( ::cOutFile ) )
      IF ! Empty( ::cOutFile )
         IF Right( cOut, 4 ) != ".EXE"
            cOut += ".EXE"
         ENDIF
         ::cOutFile := cOut
         cDosComm := 'CMD.EXE /c move /y ' + ::cExe + " " + cOut
         EXECUTE FILE cDosComm WAIT HIDE
         IF ! File( cOut )
            ::Form_Wait:Hide()
            MsgStop( i18n( "Can't move or rename EXE file." ), 'OOHG IDE+' )
            Break
         ENDIF
         ::cExe := cOut
      ENDIF

      // Cleanup
      BorraTemp( cFolder )
      ::Form_Wait:Hide()
      IF nOption == 0
         MsgInfo( i18n( 'Project builded.' ), 'OOHG IDE+' )
      ELSEIF nOption == 1 .or. nOption == 2
         EXECUTE FILE ::cExe
      ENDIF
   END SEQUENCE

   CursorArrow()
   ::Form_Tree:button_09:Enabled := .T.
   ::Form_Tree:button_10:Enabled := .T.
   ::Form_Tree:button_11:Enabled := .T.
RETURN NIL

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._-._.-._.-._.-._.-._.-._.-._
*                 COMPILING WITH BORLAND C AND HARBOUR
*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._-._.-._.-._.-._.-._.-._.-._

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD BuildWithHarbourAndBCC( nOption ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   LOCAL aPrgFiles
   LOCAL aRcFiles
   LOCAL cCompFolder  := ::cBCCFolder + '\'
   LOCAL cCompFolderB := cCompFolder + ::cBCCFolderB
   LOCAL cCompFolderI := cCompFolder + ::cBCCFolderI
   LOCAL cCompFolderL := cCompFolder + ::cBCCFolderL
   LOCAL cDosComm
   LOCAL cError
   LOCAL cError1
   LOCAL cFile
   LOCAL cHarbourFolder  := ::cHbBCCFolder + '\'
   LOCAL cHarbourFolderB := cHarbourFolder + ::cHbBCCFolderB
   LOCAL cHarbourFolderI := cHarbourFolder + ::cHbBCCFolderI
   LOCAL cHarbourFolderL := cHarbourFolder + ::cHbBCCFolderL
   LOCAL cHBP
   LOCAL cHBMK2
   LOCAL cMiniGuiFolder  := ::cGuiHbBCC + '\'
   LOCAL cMiniGuiFolderI := cMiniGuiFolder + ::cGUIHbBCCI
   LOCAL cMiniGuiFolderL := cMiniGuiFolder + ::cGUIHbBCCL
   LOCAL cMiniGuiFolderR := cMiniGuiFolder + ::cGUIHbBCCR
   LOCAL cOut
   LOCAL cPrgName
   LOCAL cFolder := ::cProjFolder + '\'
   LOCAL i
   LOCAL nItems
   LOCAL nPrgFiles

   ::Form_Tree:button_09:Enabled := .F.
   ::Form_Tree:button_10:Enabled := .F.
   ::Form_Tree:button_11:Enabled := .F.
   CursorWait()
   ::Form_Wait:hmi_label_101:Value := i18n( 'Compiling ...' )
   ::Form_Wait:Show()

   BEGIN SEQUENCE
      // Check folders
      IF Empty( ::cProjectName )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'You must save the project before building it.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cCompFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The BCC folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ::nTBuild == 2 .AND. ! File( ::cMakeTool ) .AND. ! IsFileInPath( ::cMakeTool )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Make tool " + ::cMakeTool + " was not found." ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cMiniGuiFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The ooHG-Hb-BCC folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderI + "\OOHG.CH" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's INCLUDE subfolder error: can't find " ) + cMiniGuiFolderI + "\OOHG.CH", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderL + "\OOHG.LIB" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's LIB subfolder error: can't find " ) + cMiniGuiFolderL + "\OOHG.LIB", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderR + "\OOHG_BCC.RC" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's RESOURCES subfolder error: can't find " ) + cMiniGuiFolderR + "\OOHG_BCC.RC", 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cHarbourFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The Harbour-Borland C folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderB + "\HARBOUR.EXE" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Harbour's BIN subfolder error: can't find " ) + cHarbourFolderB + "\HARBOUR.EXE", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderI + "\HBAPI.H" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Harbour's INCLUDE subfolder error: can't find " ) + cHarbourFolderI + "\HBAPI.H", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderL + "\HBRTL.LIB" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Harbour's LIB subfolder error: can't find " ) + cHarbourFolderL + "\HBRTL.LIB", 'OOHG IDE+' )
         Break
      ENDIF

      // Prepare to build
      SetCurrentFolder( cFolder )
      BorraTemp( cFolder )
      cPrgName := StrTran( AllTrim( DelExt( DelPath( ::cProjectName ) ) ), " ", "_" )
      ::cExe := cPrgName + '.exe'
      IF File( ::cExe )
         DELETE FILE ( ::cExe )
      ENDIF
      IF File( ::cExe )
         ::Form_Wait:Hide()
         MsgInfo( i18n( 'Is EXE running?' ), 'OOHG IDE+' )
         Break
      ENDIF

      DO CASE
      CASE ::nTBuild == 3    // HBMK2
         // Check for hbp file
         cHBP := cPrgName + '.hbp'
         IF ! File( cHBP )
            ::Form_Wait:Hide()
            MsgInfo( cHBP + i18n( " was not found in the project's folder." ), 'OOHG IDE+' )
            Break
         ENDIF
         // Check for hbmk2.exe file
         IF File( 'hbmk2.exe' ) .OR. IsFileInPath( 'hbmk2.exe' )
            cHBMK2 := 'hbmk2.exe'
         ELSEIF File( cHarbourFolderB + "\hbmk2.exe" )
            cHBMK2 := cHarbourFolderB + "\hbmk2.exe"
         ELSE
            ::Form_Wait:Hide()
            MsgInfo( i18n( "Can't find: " ) + "HBMK2.EXE", 'OOHG IDE+' )
            Break
         ENDIF
         // Build batch to launch HBMK2 utility
         cOut := ''
         cOut += '@echo off' + CRLF
         cOut += 'set "HG_TPATH=%PATH%"' + CRLF
         cOut += 'set "PATH=' + cHarbourFolderB + ";" + cCompFolderB + ';%PATH%"' + CRLF
         cOut += cHBMK2 + ' ' + cHBP + ' > error.txt 2>&1' + CRLF
         cOut += 'if errorlevel 1 echo ERROR BUILDING ' + cHBP + ' >> error.txt' + CRLF
         cOut += 'set "PATH=%HG_TPATH%"' + CRLF
         cOut += 'set HG_TPATH=' + CRLF
         hb_MemoWrit( '_build.bat', cOut )
         // Compile and link
         EXECUTE FILE '_build.bat' WAIT HIDE
      CASE ::nTBuild == 2    // Own Make
         // Build list of source files
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         aRcFiles := {}
         For i := 1 To nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ELSEIF ::SearchType( ::SearchItem( cFile, "RC" ) ) == "RC" .AND. ! cFile == "RC"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aRcFiles, cFile ) == 0
                  aAdd( aRcFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build make script
         // Variables
         cOut := ''
         cOut += 'PROJECTFOLDER = ' + DelSlash( cFolder ) + CRLF
         cOut += 'APP_NAME      = ' + ::cExe + CRLF
         cOut += 'OBJ_DIR       = ' + cFolder + 'OBJ' + CRLF
         cOut += 'OBJECTS       = '
         For i := 1 To nPrgFiles
            cOut += '\' + CRLF + '$(OBJ_DIR)\' + aPrgFiles[i] + '.obj '
         NEXT i
         cOut += CRLF
         cOut += 'RESFILES      = $(OBJ_DIR)\_temp.res '
         cOut += 'LINK_EXE      = ' + cCompFolderB + '\ILINK32.EXE' + CRLF
         cOut += 'LINK_FLAGS    = -Gn -Tpe -x' + iif( nOption == 2, "-ap", "-aa" ) + CRLF
         cOut += 'LINK_SEARCH   = -L' + DelSlash( cFolder ) + ;
                                ' -L' + cCompFolderL + ;
                                ' -L' + cHarbourFolderL + ;
                                ' -L' + cMiniGUIFolderL + CRLF
         cOut += 'LINK_LIBS     = '
         cOut += '\' + CRLF + cMiniGuiFolderL + '\oohg.lib'
         cOut += '\' + CRLF + cMiniGuiFolderL + '\hbprinter.lib'
         cOut += '\' + CRLF + cMiniGuiFolderL + '\miniprint.lib'
         cOut += '\' + CRLF + cMiniGuiFolderL + '\bostaurus.lib'
         IF nOption == 2
            cOut += '\' + CRLF + cHarbourFolderL + '\gtwin.lib'
         ENDIF
         cOut += '\' + CRLF + cHarbourFolderL + '\gtgui.lib'
         For Each i In { "ace32.lib", ;
                         "codepage.lib", ;
                         "common.lib", ;
                         "ct.lib", ;
                         "dbfcdx.lib", ;
                         "dbfdbt.lib", ;
                         "dbffpt.lib", ;
                         "dbfntx.lib", ;
                         "debug.lib", ;
                         "dll.lib", ;
                         "hbcommon.lib", ;
                         "hbcpage.lib", ;
                         "hbct.lib", ;
                         "hbdebug.lib", ;
                         "hbhsx.lib", ;
                         "hblang.lib", ;
                         "hbmacro.lib", ;
                         "hboleaut.lib", ;
                         "hbpp.lib", ;
                         "hbrdd.lib", ;
                         "hbrtl.lib", ;
                         "hbsix.lib", ;
                         "hbvm.lib", ;
                         "hbwin.lib", ;
                         "hsx.lib", ;
                         "lang.lib", ;
                         "libmisc.lib", ;
                         "libmysqldll.lib", ;
                         "macro.lib", ;
                         "mysql.lib", ;
                         "odbc32.lib", ;
                         "pcrepos.lib", ;
                         "pp.lib", ;
                         "rdd.lib", ;
                         "rddads.lib", ;
                         "rddcdx.lib", ;
                         "rddfpt.lib", ;
                         "rddntx.lib", ;
                         "rtl.lib", ;
                         "tip.lib", ;
                         "vm.lib", ;
                         "ziparchive.lib", ;
                         "zlib1.lib" }
            IF File( cHarbourFolderL + '\' + i )
               cOut += '\' + CRLF + cHarbourFolderL + '\' + i
            ENDIF
         NEXT
         IF ! Empty( ::cLibXH )
            cOut += '\' + CRLF + ::cLibXH
         ENDIF
         cOut += CRLF
         cOut += 'CC_EXE        = ' + cCompFolderB + '\BCC32.EXE' + CRLF
         cOut += 'CC_FLAGS      = -c -O2 -tW -M' + CRLF
         cOut += 'CC_SEARCH     = -I' + DelSlash( cFolder ) + ';' + ;
                                        cCompFolderI + ';' + ;
                                        cHarbourFolderI + ';' + ;
                                        cMiniGUIFolderI + ';' + ;
                                 '-L' + cCompFolderL + ';' + cCompFolderL + '\PSDK;' + CRLF
         cOut += 'HRB_EXE       = ' + cHarbourFolderB + '\HARBOUR.EXE' + CRLF
         cOut += 'HRB_FLAGS     = -n -q ' + iif( nOption == 2, "-b ", "" ) + CRLF
         cOut += 'HRB_SEARCH    = -i' + DelSlash( cFolder ) + ;
                                ' -i' + cHarbourFolderI + ;
                                ' -i' + cMiniGUIFolderI + CRLF
         cOut += 'RC_COMP       = ' + cCompFolderB + '\BRC32.EXE' + CRLF
         cOut += CRLF
         // Rule for .exe building
         cOut += '$(APP_NAME) : $(OBJECTS) $(RESFILES)' + CRLF
         cOut += HTAB + '$(LINK_EXE) $(LINK_SEARCH) $(LINK_FLAGS) c0w32.obj $(OBJECTS),$(APP_NAME),,$(LINK_LIBS) cw32.lib import32.lib msimg32.lib' + ;
                        iif( ! Empty( ::cLibCC ), ' ' + ::cLibCC, '' ) +',,$(RESFILES)' + CRLF
         cOut += HTAB + '@echo.' + CRLF
         cOut += CRLF
         // Rule for .c compiling
         For i := 1 To nPrgFiles
            cOut += '$(OBJ_DIR)\' + aPrgFiles[i] + '.obj : $(OBJ_DIR)\' + aPrgFiles[i] + '.c' + CRLF
            cOut += HTAB + '$(CC_EXE) $(CC_FLAGS) $(CC_SEARCH) -o$@ $**' + CRLF
            cOut += HTAB + '@echo.' + CRLF
            cOut += CRLF
         NEXT i
         // Rule for .prg compiling
         For i := 1 To nPrgFiles
            cOut += '$(OBJ_DIR)\' + aPrgFiles[i] + '.c : $(PROJECTFOLDER)\' + aPrgFiles[i] + '.prg' + CRLF
            cOut += HTAB + '$(HRB_EXE) $(HRB_FLAGS) $(HRB_SEARCH) $** -o$@' + CRLF
            cOut += HTAB + '@echo.' + CRLF
            cOut += CRLF
         NEXT i
         // Rule for _temp.rc compiling
         cOut += '$(OBJ_DIR)\_temp.res : $(PROJECTFOLDER)\_temp.rc' + CRLF
         cOut += HTAB + '$(RC_COMP) -r -fo$@ $**' + CRLF
         cOut += HTAB + '@echo.' + CRLF
         // Write make script
         hb_MemoWrit( '_temp.bc', cOut )
         // Build batch to create RC temp file and launch make utility
         cOut := ''
         cOut += '@echo off' + CRLF
         cOut += 'echo #define oohgpath ' + cMiniGUIFolderR + ' > ' + cFolder + '_oohg_resconfig.h' + CRLF
         cOut += 'echo. > "' + cMiniGUIFolderR + '\filler"' + CRLF
         cOut += 'copy /b "' + cMiniGUIFolderR + '\oohg.rc" _temp.rc > NUL' + CRLF
         FOR i := 1 TO Len( aRcFiles )
            cOut += 'copy /b "' + cMiniGUIFolderR + '\filler" + _temp.rc _aux.rc > NUL' + CRLF
            cOut += 'copy /b "' + aRcFiles[ i ] + '.rc" + _aux.rc _temp.rc > NUL' + CRLF
         NEXT i
         cOut += ::cMakeTool + ' /f' + cFolder + '_temp.bc > ' + cFolder + 'error.txt' + CRLF
         hb_MemoWrit( '_build.bat', cOut )
         // Create temp folder for objects
         CreateFolder( cFolder + 'OBJ' )
         // Compile and link
         EXECUTE FILE '_build.bat' WAIT HIDE
      CASE ::nTBuild == 1 // Compile.bat
         // Check for compile file
         IF ! File( 'compile.bat' ) .AND. ! IsFileInPath( 'compile.bat' )
            ::Form_Wait:Hide()
            MsgInfo( i18n( "Copy OOHG's COMPILE.BAT to the project's folder or add OOHG root folder to PATH." ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build auxiliary source file
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         For i := 1 To nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile + '.PRG' ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         cOut := ''
         For i := 1 To nPrgFiles
            cOut += "#include '" + aPrgFiles[i] + "'" + CRLF + CRLF
         NEXT i
         hb_MemoWrit( cPrgName + '.prg', cOut )
         // Compile and link
         cDosComm := 'CMD.EXE /c compile ' + cPrgName + ' /nr /l' + iif( nOption == 2, " /d", "" )
         EXECUTE FILE cDosComm WAIT HIDE
         DELETE FILE (  cPrgName + '.prg' )
      ENDCASE

      // Check for errors
      cError := MemoRead( 'error.txt' )
      cError1 := Upper( cError )
      IF At( ' ERROR ', cError1 ) > 0 .OR. At( ' FATAL ', cError1 ) > 0 .OR. At( 'LD RETURNED 1 EXIT STATUS', cError1 ) > 0
         ::Form_Wait:Hide()
         ::ViewErrors( cError )
         Break
      ELSEIF ! File( ::cExe )
         ::Form_Wait:Hide()
         MsgStop( i18n( "File is missing: " ) + DQM( ::cExe ), 'OOHG IDE+' )
         Break
      ENDIF

      // Rename or move
      cOut := Upper( AllTrim( ::cOutFile ) )
      IF ! Empty( ::cOutFile )
         IF Right( cOut, 4 ) != ".EXE"
            cOut += ".EXE"
         ENDIF
         ::cOutFile := cOut
         cDosComm := 'CMD.EXE /c move /y ' + ::cExe + " " + cOut
         EXECUTE FILE cDosComm WAIT HIDE
         IF ! File( cOut )
            ::Form_Wait:Hide()
            MsgStop( i18n( "Can't move or rename EXE file." ), 'OOHG IDE+' )
            Break
         ENDIF
         ::cExe := cOut
      ENDIF

      // Cleanup
      BorraTemp( cFolder )
      ::Form_Wait:Hide()
      IF nOption == 0
         MsgInfo( i18n( 'Project builded.' ), 'OOHG IDE+' )
      ELSEIF nOption == 1 .or. nOption == 2
         EXECUTE FILE ::cExe
      ENDIF
   END SEQUENCE

   CursorArrow()
   ::Form_Tree:button_09:Enabled := .T.
   ::Form_Tree:button_10:Enabled := .T.
   ::Form_Tree:button_11:Enabled := .T.
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD BuildWithxHarbourAndBCC( nOption ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   LOCAL aPrgFiles
   LOCAL aRcFiles
   LOCAL cCompFolder  := ::cBCCFolder + '\'
   LOCAL cCompFolderB := cCompFolder + ::cBCCFolderB
   LOCAL cCompFolderI := cCompFolder + ::cBCCFolderI
   LOCAL cCompFolderL := cCompFolder + ::cBCCFolderL
   LOCAL cDosComm
   LOCAL cError
   LOCAL cError1
   LOCAL cFile
   LOCAL cHarbourFolder := ::cxHbBCCFolder + '\'
   LOCAL cHarbourFolderB := cHarbourFolder + ::cxHbBCCFolderB
   LOCAL cHarbourFolderI := cHarbourFolder + ::cxHbBCCFolderI
   LOCAL cHarbourFolderL := cHarbourFolder + ::cxHbBCCFolderL
   LOCAL cMiniGuiFolder  := ::cGuixHbBCC + '\'
   LOCAL cMiniGuiFolderI := cMiniGuiFolder + ::cGUIxHbBCCI
   LOCAL cMiniGuiFolderL := cMiniGuiFolder + ::cGUIxHbBCCL
   LOCAL cMiniGuiFolderR := cMiniGuiFolder + ::cGUIxHbBCCR
   LOCAL cOut
   LOCAL cPrgName
   LOCAL cFolder := ::cProjFolder + '\'
   LOCAL i
   LOCAL nItems
   LOCAL nPrgFiles

   ::Form_Tree:button_09:Enabled := .F.
   ::Form_Tree:button_10:Enabled := .F.
   ::Form_Tree:button_11:Enabled := .F.
   CursorWait()
   ::Form_Wait:hmi_label_101:Value := i18n( 'Compiling ...' )
   ::Form_Wait:Show()

   BEGIN SEQUENCE
      // Check folders
      IF Empty( ::cProjectName )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'You must save the project before building it.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cCompFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The BCC folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ::nTBuild == 2 .AND. ! File( ::cMakeTool ) .AND. ! IsFileInPath( ::cMakeTool )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Make tool " + ::cMakeTool + " was not found." ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cMiniGuiFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The ooHG-xHb-BCC folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderI + "\OOHG.CH" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's INCLUDE subfolder error: can't find " ) + cMiniGuiFolderI + "\OOHG.CH", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderL + "\OOHG.LIB" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's LIB subfolder error: can't find " ) + cMiniGuiFolderL + "\OOHG.LIB", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderR + "\OOHG_BCC.RC" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's RESOURCES subfolder error: can't find " ) + cMiniGuiFolderR + "\OOHG_BCC.RC", 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cHarbourFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The xHarbour-Borland C folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderB + "\HARBOUR.EXE" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "xHarbour's BIN subfolder error: can't find " ) + cHarbourFolderB + "\HARBOUR.EXE", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderI + "\HBAPI.H" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "xHarbour's INCLUDE subfolder error: can't find " ) + cHarbourFolderI + "\HBAPI.H", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderL + "\RTL.LIB" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "xHarbour's LIB subfolder error: can't find " ) + cHarbourFolderL + "\RTL.LIB", 'OOHG IDE+' )
         Break
      ENDIF

      // Prepare to build
      SetCurrentFolder( cFolder )
      BorraTemp( cFolder )
      cPrgName := StrTran( AllTrim( DelExt( DelPath( ::cProjectName ) ) ), " ", "_" )
      ::cExe := cPrgName + '.exe'
      IF File( ::cExe )
         DELETE FILE ( ::cExe )
      ENDIF
      IF File( ::cExe )
         ::Form_Wait:Hide()
         MsgInfo( i18n( 'Is EXE running?' ), 'OOHG IDE+' )
         Break
      ENDIF

      DO CASE
      CASE ::nTBuild == 2    // Own Make
         // Build list of source files
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         aRcFiles := {}
         For i := 1 To nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ELSEIF ::SearchType( ::SearchItem( cFile, "RC" ) ) == "RC" .AND. ! cFile == "RC"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aRcFiles, cFile ) == 0
                  aAdd( aRcFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build make script
         // Variables
         cOut := ''
         cOut += 'PROJECTFOLDER = ' + DelSlash( cFolder ) + CRLF
         cOut += 'APP_NAME      = ' + ::cExe + CRLF
         cOut += 'TDS_NAME      = ' + cPrgName + '.tds' + CRLF
         cOut += 'OBJ_DIR       = ' + cFolder + 'OBJ' + CRLF
         cOut += 'OBJECTS       = '
         For i := 1 To nPrgFiles
            cOut += '\' + CRLF + '$(OBJ_DIR)\' + aPrgFiles[i] + '.obj '
         NEXT i
         cOut += CRLF
         cOut += 'RESFILES      = $(OBJ_DIR)\_temp.res '
         cOut += 'LINK_EXE      = ' + cCompFolderB + '\ILINK32.EXE' + CRLF
         cOut += 'LINK_FLAGS    = -Gn -Tpe -x ' + iif( nOption == 2, "-ap", "-aa" ) + CRLF
         cOut += 'LINK_SEARCH   = -L' + DelSlash( cFolder ) + ;
                                ' -L' + cCompFolderL + ;
                                ' -L' + cHarbourFolderL + ;
                                ' -L' + cMiniGUIFolderL + CRLF
         cOut += 'LINK_LIBS     = '
         cOut += '\' + CRLF + cMiniGuiFolderL + '\oohg.lib'
         cOut += '\' + CRLF + cMiniGuiFolderL + '\hbprinter.lib'
         cOut += '\' + CRLF + cMiniGuiFolderL + '\miniprint.lib'
         cOut += '\' + CRLF + cMiniGuiFolderL + '\bostaurus.lib'
         IF nOption == 2
            cOut += '\' + CRLF + cHarbourFolderL + '\gtwin.lib'
         ENDIF
         cOut += '\' + CRLF + cHarbourFolderL + '\gtgui.lib'
         For Each i In { "ace32.lib", ;
                         "codepage.lib", ;
                         "common.lib", ;
                         "ct.lib", ;
                         "dbfcdx.lib", ;
                         "dbfdbt.lib", ;
                         "dbffpt.lib", ;
                         "dbfntx.lib", ;
                         "debug.lib", ;
                         "dll.lib", ;
                         "hbcommon.lib", ;
                         "hbcpage.lib", ;
                         "hbct.lib", ;
                         "hbdebug.lib", ;
                         "hbhsx.lib", ;
                         "hblang.lib", ;
                         "hbmacro.lib", ;
                         "hboleaut.lib", ;
                         "hbpp.lib", ;
                         "hbrdd.lib", ;
                         "hbrtl.lib", ;
                         "hbsix.lib", ;
                         "hbvm.lib", ;
                         "hbwin.lib", ;
                         "hsx.lib", ;
                         "lang.lib", ;
                         "libmisc.lib", ;
                         "libmysqldll.lib", ;
                         "macro.lib", ;
                         "mysql.lib", ;
                         "odbc32.lib", ;
                         "pcrepos.lib", ;
                         "pp.lib", ;
                         "rdd.lib", ;
                         "rddads.lib", ;
                         "rddcdx.lib", ;
                         "rddfpt.lib", ;
                         "rddntx.lib", ;
                         "rtl.lib", ;
                         "tip.lib", ;
                         "vm.lib", ;
                         "ziparchive.lib", ;
                         "zlib1.lib" }
            IF File( cHarbourFolderL + '\' + i )
               cOut += '\' + CRLF + cHarbourFolderL + '\' + i
            ENDIF
         NEXT
         IF ! Empty( ::cLibXH )
            cOut += '\' + CRLF + ::cLibXH
         ENDIF
         cOut += CRLF
         cOut += 'CC_EXE        = ' + cCompFolderB + '\BCC32.EXE' + CRLF
         cOut += 'CC_FLAGS      = -c -O2 -tW -M' + CRLF
         cOut += 'CC_SEARCH     = -I' + DelSlash( cFolder ) + ';' + ;
                                        cCompFolderI + ';' + ;
                                        cHarbourFolderI + ';' + ;
                                        cMiniGUIFolderI + ';' + ;
                                 '-L' + cCompFolderL + ';' + cCompFolderL + '\PSDK;' + CRLF
         cOut += 'HRB_EXE       = ' + cHarbourFolderB + '\HARBOUR.EXE' + CRLF
         cOut += 'HRB_FLAGS     = -n -q ' + iif( nOption == 2, "-b ", "" ) + CRLF
         cOut += 'HRB_SEARCH    = -i' + DelSlash( cFolder ) + ;
                                ' -i' + cHarbourFolderI + ;
                                ' -i' + cMiniGUIFolderI + CRLF
         cOut += 'RC_COMP       = ' + cCompFolderB + '\BRC32.EXE' + CRLF
         cOut += CRLF
         // Rule for .exe building
         cOut += '$(APP_NAME) : $(OBJECTS) $(RESFILES)' + CRLF
         cOut += HTAB + '$(LINK_EXE) $(LINK_SEARCH) $(LINK_FLAGS) c0w32.obj $(OBJECTS),$(APP_NAME),,$(LINK_LIBS) cw32.lib import32.lib msimg32.lib' + ;
                        iif( ! Empty( ::cLibCC ), ' ' + ::cLibCC, '' ) +',,$(RESFILES)' + CRLF
         cOut += HTAB + '@del $(TDS_NAME)' + CRLF
         cOut += HTAB + '@echo.' + CRLF
         cOut += CRLF
         // Rule for .c compiling
         For i := 1 To nPrgFiles
            cOut += '$(OBJ_DIR)\' + aPrgFiles[i] + '.obj : $(OBJ_DIR)\' + aPrgFiles[i] + '.c' + CRLF
            cOut += HTAB + '$(CC_EXE) $(CC_FLAGS) $(CC_SEARCH) -o$@ $**' + CRLF
            cOut += HTAB + '@echo.' + CRLF
            cOut += CRLF
         NEXT i
         // Rule for .prg compiling
         For i := 1 To nPrgFiles
            cOut += '$(OBJ_DIR)\' + aPrgFiles[i] + '.c : $(PROJECTFOLDER)\' + aPrgFiles[i] + '.prg' + CRLF
            cOut += HTAB + '$(HRB_EXE) $(HRB_FLAGS) $(HRB_SEARCH) $** -o$@' + CRLF
            cOut += HTAB + '@echo.' + CRLF
            cOut += CRLF
         NEXT i
         // Rule for _temp.rc compiling
         cOut += '$(OBJ_DIR)\_temp.res : $(PROJECTFOLDER)\_temp.rc' + CRLF
         cOut += HTAB + '$(RC_COMP) -r -fo$@ $**' + CRLF
         cOut += HTAB + '@echo.' + CRLF
         // Write make script
         hb_MemoWrit( '_temp.bc', cOut )
         // Build batch to create RC temp file and launch make utility
         cOut := ''
         cOut += '@echo off' + CRLF
         cOut += 'echo #define oohgpath ' + cMiniGUIFolderR + ' > ' + cFolder + '_oohg_resconfig.h' + CRLF
         cOut += 'echo. > "' + cMiniGUIFolderR + '\filler"' + CRLF
         cOut += 'copy /b "' + cMiniGUIFolderR + '\oohg.rc" _temp.rc > NUL' + CRLF
         FOR i := 1 TO Len( aRcFiles )
            cOut += 'copy /b "' + cMiniGUIFolderR + '\filler" + _temp.rc _aux.rc > NUL' + CRLF
            cOut += 'copy /b "' + aRcFiles[ i ] + '.rc" + _aux.rc _temp.rc > NUL' + CRLF
         NEXT i
         cOut += ::cMakeTool + ' /f' + cFolder + '_temp.bc > ' + cFolder + 'error.txt' + CRLF
         hb_MemoWrit( '_build.bat', cOut )
         // Create temp folder for objects
         CreateFolder( cFolder + 'OBJ' )
         // Compile and link
         EXECUTE FILE '_build.bat' WAIT HIDE
      CASE ::nTBuild == 1 // Compile.bat
         // Check for compile file
         IF ! File( 'compile.bat' ) .AND. ! IsFileInPath( 'compile.bat' )
            ::Form_Wait:Hide()
            MsgInfo( i18n( "Copy OOHG's COMPILE.BAT to the project's folder or add OOHG root folder to PATH." ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build auxiliary source file
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         For i := 1 To nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile + '.PRG' ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         cOut := ''
         For i := 1 To nPrgFiles
            cOut += "#include '" + aPrgFiles[i] + "'" + CRLF + CRLF
         NEXT i
         hb_MemoWrit( cPrgName + '.prg', cOut )
         // Compile and link
         cDosComm := 'CMD.EXE /c compile ' + cPrgName + ' /nr /l' + iif( nOption == 2, " /d", "" )
         EXECUTE FILE cDosComm WAIT HIDE
         DELETE FILE ( cPrgName + '.prg' )
      ENDCASE

      // Check for errors
      cError := MemoRead( 'error.txt' )
      cError1 := Upper( cError )
      IF At( ' ERROR ', cError1 ) > 0 .OR. At( ' FATAL ', cError1 ) > 0 .OR. At( 'LD RETURNED 1 EXIT STATUS', cError1 ) > 0
         ::Form_Wait:Hide()
         ::ViewErrors( cError )
         Break
      ELSEIF ! File( ::cExe )
         ::Form_Wait:Hide()
         MsgStop( i18n( "File is missing: " ) + DQM( ::cExe ), 'OOHG IDE+' )
         Break
      ENDIF

      // Rename or move
      cOut := Upper( AllTrim( ::cOutFile ) )
      IF ! Empty( ::cOutFile )
         IF Right( cOut, 4 ) != ".EXE"
            cOut += ".EXE"
         ENDIF
         ::cOutFile := cOut
         cDosComm := 'CMD.EXE /c move /y ' + ::cExe + " " + cOut
         EXECUTE FILE cDosComm WAIT HIDE
         IF ! File( cOut )
            ::Form_Wait:Hide()
            MsgStop( i18n( "Can't move or rename EXE file." ), 'OOHG IDE+' )
            Break
         ENDIF
         ::cExe := cOut
      ENDIF

      // Cleanup
      BorraTemp( cFolder )
      ::Form_Wait:Hide()
      IF nOption == 0
         MsgInfo( i18n( 'Project builded.' ), 'OOHG IDE+' )
      ELSEIF nOption == 1 .or. nOption == 2
         EXECUTE FILE ::cExe
      ENDIF
   END SEQUENCE

   CursorArrow()
   ::Form_Tree:button_09:Enabled := .T.
   ::Form_Tree:button_10:Enabled := .T.
   ::Form_Tree:button_11:Enabled := .T.
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD BuildWithHarbourAndPellesC(nOption) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   LOCAL aPrgFiles
   LOCAL aRcFiles
   LOCAL cCompFolder  := ::cPellFolder + '\'
   LOCAL cCompFolderB := cCompFolder + ::cPellFolderB
   LOCAL cCompFolderI := cCompFolder + ::cPellFolderI
   LOCAL cCompFolderL := cCompFolder + ::cPellFolderL
   LOCAL cDosComm
   LOCAL cError
   LOCAL cError1
   LOCAL cFile
   LOCAL cHarbourFolder  := ::cHbPellFolder + '\'
   LOCAL cHarbourFolderB := cHarbourFolder + ::cHbPellFolderB
   LOCAL cHarbourFolderI := cHarbourFolder + ::cHbPellFolderI
   LOCAL cHarbourFolderL := cHarbourFolder + ::cHbPellFolderL
   LOCAL cHBP
   LOCAL cHBMK2
   LOCAL cMiniGuiFolder := ::cGuiHbPelles + '\'
   LOCAL cMiniGuiFolderI := cMiniGuiFolder + ::cGuiHbPellesI
   LOCAL cMiniGuiFolderL := cMiniGuiFolder + ::cGuiHbPellesL
   LOCAL cMiniGuiFolderR := cMiniGuiFolder + ::cGuiHbPellesR
   LOCAL cOut
   LOCAL cPrgName
   LOCAL cFolder := ::cProjFolder + '\'
   LOCAL i
   LOCAL nItems
   LOCAL nPrgFiles

   ::Form_Tree:button_09:Enabled := .F.
   ::Form_Tree:button_10:Enabled := .F.
   ::Form_Tree:button_11:Enabled := .F.
   CursorWait()
   ::Form_Wait:hmi_label_101:Value := i18n( 'Compiling ...' )
   ::Form_Wait:Show()

   BEGIN SEQUENCE
      // Check folders
      IF Empty( ::cProjectName )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'You must save the project before building it.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cCompFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The Pelles C folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ::nTBuild == 2 .AND. ! File( ::cMakeTool ) .AND. ! IsFileInPath( ::cMakeTool )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Make tool " + ::cMakeTool + " was not found." ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cMiniGuiFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The ooHG-Hb-Pelles C folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderI + "\OOHG.CH" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's INCLUDE subfolder error: can't find " ) + cMiniGuiFolderI + "\OOHG.CH", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderL + "\OOHG.LIB" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's LIB subfolder error: can't find " ) + cMiniGuiFolderL + "\OOHG.LIB", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderR + "\OOHG_PELLES.RC" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's RESOURCES subfolder error: can't find " ) + cMiniGuiFolderR + "\OOHG_PELLES.RC", 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cHarbourFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The Harbour-Pelles C folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderB + "\HARBOUR.EXE" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Harbour's BIN subfolder error: can't find " ) + cHarbourFolderB + "\HARBOUR.EXE", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderI + "\HBAPI.H" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Harbour's INCLUDE subfolder error: can't find " ) + cHarbourFolderI + "\HBAPI.H", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderL + "\HBRTL.LIB" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Harbour's LIB subfolder error: can't find " ) + cHarbourFolderL + "\HBRTL.LIB", 'OOHG IDE+' )
         Break
      ENDIF

      // Prepare to build
      SetCurrentFolder( cFolder )
      BorraTemp( cFolder )
      cPrgName := StrTran( AllTrim( DelExt( DelPath( ::cProjectName ) ) ), " ", "_" )
      ::cExe := cPrgName + '.exe'
      IF File( ::cExe )
         DELETE FILE ( ::cExe )
      ENDIF
      IF File( ::cExe )
         ::Form_Wait:Hide()
         MsgInfo( i18n( 'Is EXE running?' ), 'OOHG IDE+' )
         Break
      ENDIF

      DO CASE
      CASE ::nTBuild == 3    // HBMK2
         // Check for hbp file
         cHBP := cPrgName + '.hbp'
         IF ! File( cHBP )
            ::Form_Wait:Hide()
            MsgInfo( cHBP + i18n( " was not found in the project's folder." ), 'OOHG IDE+' )
            Break
         ENDIF
         // Check for hbmk2.exe file
         IF File( 'hbmk2.exe' ) .OR. IsFileInPath( 'hbmk2.exe' )
            cHBMK2 := 'hbmk2.exe'
         ELSEIF File( cHarbourFolderB + "\hbmk2.exe" )
            cHBMK2 := cHarbourFolderB + "\hbmk2.exe"
         ELSE
            ::Form_Wait:Hide()
            MsgInfo( i18n( "Can't find: " ) + "HBMK2.EXE", 'OOHG IDE+' )
            Break
         ENDIF
         // Build batch to launch HBMK2 utility
         cOut := ''
         cOut += '@echo off' + CRLF
         cOut += 'set HG_TPATH=%PATH%' + CRLF
         cOut += 'set "PATH=' + cHarbourFolderB + ";" + cCompFolderB + ';%PATH%"' + CRLF
         cOut += cHBMK2 + ' ' + cHBP + ' > error.txt 2>&1' + CRLF
         cOut += 'if errorlevel 1 echo ERROR BUILDING ' + cHBP + ' >> error.txt' + CRLF
         cOut += 'set "PATH=%HG_TPATH%"' + CRLF
         cOut += 'set HG_TPATH=' + CRLF
         hb_MemoWrit( '_build.bat', cOut )
         // Compile and link
         EXECUTE FILE '_build.bat' WAIT HIDE
      CASE ::nTBuild == 2    // Own Make
         // Build list of source files
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         aRcFiles := {}
         For i := 1 To nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ELSEIF ::SearchType( ::SearchItem( cFile, "RC" ) ) == "RC" .AND. ! cFile == "RC"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aRcFiles, cFile ) == 0
                  aAdd( aRcFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build make script
         cOut := ''
         cOut += 'HARBOUR_EXE = ' + cHarbourFolderB + '\HARBOUR.EXE' + CRLF
         cOut += 'CC = ' + cCompFolderB + '\POCC.EXE' + CRLF
         cOut += 'ILINK_EXE = ' + cCompFolderB + '\POLINK.EXE' + CRLF
         cOut += 'BRC_EXE = ' + cCompFolderB + '\PORC.EXE' + CRLF
         cOut += 'APP_NAME = ' + ::cExe + CRLF
         cOut += 'INCLUDE_DIR = ' + cHarbourFolderI + ';' + cMiniGuiFolderI + ';' + DelSlash( cFolder ) + CRLF
         cOut += 'INCLUDE_C_DIR = ' + cHarbourFolderI + ' -I' + cMiniGuiFolderI + ' -I' + DelSlash( cFolder ) + ' -I' + cCompFolderI + ' -I' + cCompFolderI + '\WIN' + CRLF
         cOut += 'CC_LIB_DIR = ' + cCompFolderL + CRLF
         cOut += 'HRB_LIB_DIR = ' + cHarbourFolderL + CRLF
         cOut += 'OBJ_DIR = ' + cFolder + 'OBJ' + CRLF
         cOut += 'C_DIR = ' + cFolder + 'OBJ' + CRLF
         cOut += 'USER_FLAGS =' + CRLF
         cOut += 'HARBOUR_FLAGS = /i$(INCLUDE_DIR) /n /q0 ' + iif( nOption == 2, "/b ", "" ) + '$(USER_FLAGS)' + CRLF
         cOut += 'COBJFLAGS = /Ze /Zx /Go /Tx86-coff /D__WIN32__ ' + '-I$(INCLUDE_C_DIR)' + CRLF
         cOut += CRLF
         cOut += '$(APP_NAME) : $(OBJ_DIR)\' + aPrgFiles[1] + '.obj'
         For i := 2 To nPrgFiles
            cOut += ' \' + CRLF
            cOut += '   $(OBJ_DIR)\' + aPrgFiles[i] + '.obj'
         NEXT i
         cOut += CRLF
         cOut += '   $(BRC_EXE) /fo _temp.res _temp.rc' + CRLF
         For i := 1 To nPrgFiles
            cOut += '   echo $(OBJ_DIR)\' + aPrgFiles[i] + '.obj + >' + iif( i > 1, '>', '' ) + ' b32.bc' + CRLF
         NEXT i
         cOut += '   echo /OUT:$(APP_NAME) >> b32.bc' + CRLF
         cOut += '   echo /FORCE:MULTIPLE >> b32.bc' + CRLF
         cOut += '   echo /LIBPATH:$(CC_LIB_DIR) >> b32.bc' + CRLF
         cOut += '   echo /LIBPATH:$(CC_LIB_DIR)\WIN >> b32.bc' + CRLF
         cOut += '   echo ' + cMiniGuiFolderL + '\oohg.lib >> b32.bc' + CRLF
         cOut += '   echo ' + cMiniGuiFolderL + '\hbprinter.lib >> b32.bc' + CRLF
         cOut += '   echo ' + cMiniGuiFolderL + '\miniprint.lib >> b32.bc' + CRLF
         cOut += '   echo ' + cMiniGuiFolderL + '\bostaurus.lib >> b32.bc' + CRLF
         IF nOption == 2
            cOut += '   echo $(HRB_LIB_DIR)\gtwin.lib >> b32.bc' + CRLF
         ENDIF
         cOut += '   echo $(HRB_LIB_DIR)\gtgui.lib >> b32.bc' + CRLF
         For Each i In { "ace32.lib", ;
                         "codepage.lib", ;
                         "common.lib", ;
                         "ct.lib", ;
                         "dbfcdx.lib", ;
                         "dbfdbt.lib", ;
                         "dbffpt.lib", ;
                         "dbfntx.lib", ;
                         "debug.lib", ;
                         "dll.lib", ;
                         "hbcommon.lib", ;
                         "hbcpage.lib", ;
                         "hbct.lib", ;
                         "hbdebug.lib", ;
                         "hbhsx.lib", ;
                         "hblang.lib", ;
                         "hbmacro.lib", ;
                         "hbmisc.lib", ;
                         "hbmzip.lib", ;
                         "hboleaut.lib", ;
                         "hbpcre.lib", ;
                         "hbpp.lib", ;
                         "hbrdd.lib", ;
                         "hbrtl.lib", ;
                         "hbsix.lib", ;
                         "hbtip.lib", ;
                         "hbvm.lib", ;
                         "hbwin.lib", ;
                         "hbzlib.lib", ;
                         "hsx.lib", ;
                         "lang.lib", ;
                         "libmisc.lib", ;
                         "libmysqldll.lib", ;
                         "macro.lib", ;
                         "minizip.lib", ;
                         "mysql.lib", ;
                         "odbc32.lib", ;
                         "pcrepos.lib", ;
                         "pp.lib", ;
                         "rdd.lib", ;
                         "rddads.lib", ;
                         "rddcdx.lib", ;
                         "rddfpt.lib", ;
                         "rddntx.lib", ;
                         "rtl.lib", ;
                         "tip.lib", ;
                         "vm.lib", ;
                         "ziparchive.lib", ;
                         "zlib1.lib" }
            IF File( cHarbourFolderL + '\' + i )
               cOut += '   echo $(HRB_LIB_DIR)\' + i + ' >> b32.bc' + CRLF
            ENDIF
         NEXT
         IF ! Empty( ::cLibXH )
            cOut += '   echo ' + ::cLibXH + ' >> b32.bc' + CRLF
         ENDIF
         cOut += '   echo $(CC_LIB_DIR)\crt.lib >> b32.bc' + CRLF
         For Each i In { "winmm.lib", ;
                         "kernel32.lib", ;
                         "user32.lib", ;
                         "ws2_32.lib", ;
                         "iphlpapi", ;
                         "advapi32.lib", ;
                         "gdi32.lib", ;
                         "winspool.lib", ;
                         "ole32.lib", ;
                         "uuid.lib", ;
                         "oleaut32.lib", ;
                         "mpr.lib", ;
                         "comdlg32.lib", ;
                         "comctl32.lib", ;
                         "olepro32.lib", ;
                         "shell32.lib", ;
                         "vfw32.lib", ;
                         "wsock32.lib" }
            IF File( cCompFolderL + '\' + i )
               cOut += '   echo $(CC_LIB_DIR)\' + i + ' >> b32.bc' + CRLF
            ENDIF
         NEXT
         IF ! Empty( ::cLibCC )
            cOut += '   echo ' + ::cLibCC + ' >> b32.bc' + CRLF
         ENDIF
         cOut += '   echo _temp.res >> b32.bc' + CRLF
         cOut += '   $(ILINK_EXE)  /SUBSYSTEM:' + iif( nOption == 2, "CONSOLE", "WINDOWS" ) + ' @b32.bc' + CRLF
         cOut += CRLF
         For i := 1 To nPrgFiles
            cOut += CRLF
            cOut += '$(C_DIR)\' + aPrgFiles[i] + '.c : ' + cFolder + aPrgFiles[i] + '.prg' + CRLF
            cOut += '   $(HARBOUR_EXE) $(HARBOUR_FLAGS) $** -o$@'  + CRLF
            cOut += CRLF
            cOut += '$(OBJ_DIR)\' + aPrgFiles[i] + '.obj : $(C_DIR)\' + aPrgFiles[i] + '.c' + CRLF
            cOut += '   $(CC) $(COBJFLAGS) -Fo$@ $**' + CRLF
         NEXT i
         hb_MemoWrit( '_temp.bc', cOut )
         // Build batch to create RC temp file and launch make utility
         cOut := ''
         cOut += '@echo off' + CRLF
         cOut += 'echo #define oohgpath ' + cMiniGUIFolderR + ' > ' + cFolder + '_oohg_resconfig.h' + CRLF
         cOut += 'echo. > "' + cMiniGUIFolderR + '\filler"' + CRLF
         cOut += 'copy /b "' + cMiniGUIFolderR + '\oohg.rc" _temp.rc > NUL' + CRLF
         FOR i := 1 TO Len( aRcFiles )
            cOut += 'copy /b "' + cMiniGUIFolderR + '\filler" + _temp.rc _aux.rc > NUL' + CRLF
            cOut += 'copy /b "' + aRcFiles[ i ] + '.rc" + _aux.rc _temp.rc > NUL' + CRLF
         NEXT i
         cOut += ::cMakeTool + ' /f' + cFolder + '_temp.bc > ' + cFolder + 'error.txt' + CRLF
         hb_MemoWrit( '_build.bat', cOut )
         // Create folder for objects
         CreateFolder( cFolder + 'OBJ' )
         // Build
         EXECUTE FILE '_build.bat' WAIT HIDE
   CASE ::nTBuild == 1 // Compile.bat
         // Check for compile file
         IF ! File( 'compile.bat' ) .AND. ! IsFileInPath( 'compile.bat' )
            ::Form_Wait:Hide()
            MsgInfo( i18n( "Copy OOHG's COMPILE.BAT to the project's folder or add OOHG root folder to PATH." ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build auxiliary source file
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         For i := 1 To nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile + '.PRG' ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         cOut := ''
         For i := 1 To nPrgFiles
            cOut += "#include '" + aPrgFiles[i] + "'" + CRLF + CRLF
         NEXT i
         hb_MemoWrit( cPrgName + '.prg', cOut )
         // Compile and link
         cDosComm := 'CMD.EXE /c compile ' + cPrgName + ' /nr /l' + iif( nOption == 2, " /d", "" )
         EXECUTE FILE cDosComm WAIT HIDE
         DELETE FILE ( cPrgName + '.prg' )
      ENDCASE

      // Check for errors
      cError := MemoRead( 'error.txt' )
      cError1 := Upper( cError )
      IF At( ' ERROR ', cError1 ) > 0 .OR. At( ' FATAL ', cError1 ) > 0 .OR. At( 'LD RETURNED 1 EXIT STATUS', cError1 ) > 0
         ::Form_Wait:Hide()
         ::ViewErrors( cError )
         Break
      ELSEIF ! File( ::cExe )
         ::Form_Wait:Hide()
         MsgStop( i18n( "File is missing: " ) + DQM( ::cExe ), 'OOHG IDE+' )
         Break
      ENDIF

      // Rename or move
      cOut := Upper( AllTrim( ::cOutFile ) )
      IF ! Empty( ::cOutFile )
         IF Right( cOut, 4 ) != ".EXE"
            cOut += ".EXE"
         ENDIF
         ::cOutFile := cOut
         cDosComm := 'CMD.EXE /c move /y ' + ::cExe + " " + cOut
         EXECUTE FILE cDosComm WAIT HIDE
         IF ! File( cOut )
            ::Form_Wait:Hide()
            MsgStop( i18n( "Can't move or rename EXE file." ), 'OOHG IDE+' )
            Break
         ENDIF
         ::cExe := cOut
      ENDIF

      // Cleanup
      BorraTemp( cFolder )
      ::Form_Wait:Hide()
      IF nOption == 0
         MsgInfo( i18n( 'Project builded.' ), 'OOHG IDE+' )
      ELSEIF nOption == 1 .or. nOption == 2
         EXECUTE FILE ::cExe
      ENDIF
   END SEQUENCE

   CursorArrow()
   ::Form_Tree:button_09:Enabled := .T.
   ::Form_Tree:button_10:Enabled := .T.
   ::Form_Tree:button_11:Enabled := .T.
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD BuildWithxHarbourAndPellesC( nOption ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   LOCAL aPrgFiles
   LOCAL aRcFiles
   LOCAL cCompFolder  := ::cPellFolder + '\'
   LOCAL cCompFolderB := cCompFolder + ::cPellFolderB
   LOCAL cCompFolderI := cCompFolder + ::cPellFolderI
   LOCAL cCompFolderL := cCompFolder + ::cPellFolderL
   LOCAL cDosComm
   LOCAL cError
   LOCAL cError1
   LOCAL cFile
   LOCAL cHarbourFolder  := ::cxHbPellFolder + '\'
   LOCAL cHarbourFolderB := cHarbourFolder + ::cxHbPellFolderB
   LOCAL cHarbourFolderI := cHarbourFolder + ::cxHbPellFolderI
   LOCAL cHarbourFolderL := cHarbourFolder + ::cxHbPellFolderL
   LOCAL cMiniGuiFolder  := ::cGuixHbPelles + '\'
   LOCAL cMiniGuiFolderI := cMiniGuiFolder + ::cGuixHbPellesI
   LOCAL cMiniGuiFolderL := cMiniGuiFolder + ::cGuixHbPellesL
   LOCAL cMiniGuiFolderR := cMiniGuiFolder + ::cGuixHbPellesR
   LOCAL cOut
   LOCAL cPrgName
   LOCAL cFolder := ::cProjFolder + '\'
   LOCAL i
   LOCAL nItems
   LOCAL nPrgFiles

   ::Form_Tree:button_09:Enabled := .F.
   ::Form_Tree:button_10:Enabled := .F.
   ::Form_Tree:button_11:Enabled := .F.
   CursorWait()
   ::Form_Wait:hmi_label_101:Value := i18n( 'Compiling ...' )
   ::Form_Wait:Show()

   BEGIN SEQUENCE
      // Check folders
      IF Empty( ::cProjectName )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'You must save the project before building it.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cCompFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The Pelles C folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ::nTBuild == 2 .AND. ! File( ::cMakeTool ) .AND. ! IsFileInPath( ::cMakeTool )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Make tool " + ::cMakeTool + " was not found." ), 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cMiniGuiFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The ooHG-xHb-Pelles C folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderI + "\OOHG.CH" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's INCLUDE subfolder error: can't find " ) + cMiniGuiFolderI + "\OOHG.CH", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderL + "\OOHG.LIB" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's LIB subfolder error: can't find " ) + cMiniGuiFolderL + "\OOHG.LIB", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cMiniGuiFolderR + "\OOHG_PELLES.RC" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "OOHG's RESOURCES subfolder error: can't find " ) + cMiniGuiFolderR + "\OOHG_PELLES.RC", 'OOHG IDE+' )
         Break
      ENDIF

      IF Empty( cHarbourFolder )
         ::Form_Wait:Hide()
         MsgStop( i18n( 'The xHarbour-Pelles C folder must be specified to build a project.' ), 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderB + "\HARBOUR.EXE" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "xHarbour's BIN subfolder error: can't find " ) + cHarbourFolderB + "\HARBOUR.EXE", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderI + "\HBAPI.H" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "xHarbour's INCLUDE subfolder error: can't find " ) + cHarbourFolderI + "\HBAPI.H", 'OOHG IDE+' )
         Break
      ENDIF

      IF ! File( cHarbourFolderL + "\RTL.LIB" )
         ::Form_Wait:Hide()
         MsgStop( i18n( "xHarbour's LIB subfolder error: can't find " ) + cHarbourFolderL + "\RTL.LIB", 'OOHG IDE+' )
         Break
      ENDIF

      // Prepare to build
      SetCurrentFolder( cFolder )
      BorraTemp( cFolder )
      cPrgName := StrTran( AllTrim( DelExt( DelPath( ::cProjectName ) ) ), " ", "_" )
      ::cExe := cPrgName + '.exe'
      IF File( ::cExe )
         DELETE FILE ( ::cExe )
      ENDIF
      IF File( ::cExe )
         ::Form_Wait:Hide()
         MsgInfo( i18n( 'Is EXE running?' ), 'OOHG IDE+' )
         Break
      ENDIF

      DO CASE
      CASE ::nTBuild == 2    // Own Make
         // Build list of source files
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         aRcFiles := {}
         For i := 1 To nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ELSEIF ::SearchType( ::SearchItem( cFile, "RC" ) ) == "RC" .AND. ! cFile == "RC"
               cFile := Upper( AllTrim( cFile ) )
               IF aScan( aRcFiles, cFile ) == 0
                  aAdd( aRcFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build make script
         cOut := ''
         cOut += 'HARBOUR_EXE = ' + cHarbourFolderB + '\HARBOUR.EXE' + CRLF
         cOut += 'CC = ' + cCompFolderB + '\POCC.EXE' + CRLF
         cOut += 'ILINK_EXE = ' + cCompFolderB + '\POLINK.EXE' + CRLF
         cOut += 'BRC_EXE = ' + cCompFolderB + '\PORC.EXE' + CRLF
         cOut += 'APP_NAME = ' + ::cExe + CRLF
         cOut += 'INCLUDE_DIR = ' + cHarbourFolderI + ';' + cMiniGuiFolderI + ';' + DelSlash( cFolder ) + CRLF
         cOut += 'INCLUDE_C_DIR = ' + cHarbourFolderI + ' -I' + cMiniGuiFolderI + ' -I' + DelSlash( cFolder ) + ' -I' + cCompFolderI + ' -I' + cCompFolderI + '\WIN' + CRLF
         cOut += 'CC_LIB_DIR = ' + cCompFolderL + CRLF
         cOut += 'HRB_LIB_DIR = ' + cHarbourFolderL + CRLF
         cOut += 'OBJ_DIR = ' + cFolder + 'OBJ' + CRLF
         cOut += 'C_DIR = ' + cFolder + 'OBJ' + CRLF
         cOut += 'USER_FLAGS =' + CRLF
         cOut += 'HARBOUR_FLAGS = /i$(INCLUDE_DIR) /n /q0 ' + iif( nOption == 2, "/b ", "" ) + '$(USER_FLAGS)' + CRLF
         cOut += 'COBJFLAGS = /Ze /Zx /Go /Tx86-coff /D__WIN32__ ' + '-I$(INCLUDE_C_DIR)' + CRLF
         cOut += CRLF
         cOut += '$(APP_NAME) : $(OBJ_DIR)\' + aPrgFiles[1] + '.obj'
         For i := 2 To nPrgFiles
            cOut += ' \' + CRLF
            cOut += '   $(OBJ_DIR)\' + aPrgFiles[i] + '.obj'
         NEXT i
         cOut += CRLF
         cOut += '   $(BRC_EXE) /fo _temp.res _temp.rc' + CRLF
         For i := 1 To nPrgFiles
            cOut += '   echo $(OBJ_DIR)\' + aPrgFiles[i] + '.obj + >' + iif( i > 1, '>', '' ) + ' b32.bc' + CRLF
         NEXT i
         cOut += '   echo /OUT:$(APP_NAME) >> b32.bc' + CRLF
         cOut += '   echo /FORCE:MULTIPLE >> b32.bc' + CRLF
         cOut += '   echo /LIBPATH:$(CC_LIB_DIR) >> b32.bc' + CRLF
         cOut += '   echo /LIBPATH:$(CC_LIB_DIR)\WIN >> b32.bc' + CRLF
         cOut += '   echo ' + cMiniGuiFolderL + '\oohg.lib >> b32.bc' + CRLF
         cOut += '   echo ' + cMiniGuiFolderL + '\hbprinter.lib >> b32.bc' + CRLF
         cOut += '   echo ' + cMiniGuiFolderL + '\miniprint.lib >> b32.bc' + CRLF
         cOut += '   echo ' + cMiniGuiFolderL + '\bostaurus.lib >> b32.bc' + CRLF
         IF nOption == 2
            cOut += '   echo $(HRB_LIB_DIR)\gtwin.lib >> b32.bc' + CRLF
         ENDIF
         cOut += '   echo $(HRB_LIB_DIR)\gtgui.lib >> b32.bc' + CRLF
         For Each i In { "ace32.lib", ;
                         "codepage.lib", ;
                         "common.lib", ;
                         "ct.lib", ;
                         "dbfcdx.lib", ;
                         "dbfdbt.lib", ;
                         "dbffpt.lib", ;
                         "dbfntx.lib", ;
                         "debug.lib", ;
                         "dll.lib", ;
                         "hbcommon.lib", ;
                         "hbcpage.lib", ;
                         "hbct.lib", ;
                         "hbdebug.lib", ;
                         "hbhsx.lib", ;
                         "hblang.lib", ;
                         "hbmacro.lib", ;
                         "hboleaut.lib", ;
                         "hbpp.lib", ;
                         "hbrdd.lib", ;
                         "hbrtl.lib", ;
                         "hbsix.lib", ;
                         "hbvm.lib", ;
                         "hbwin.lib", ;
                         "hsx.lib", ;
                         "lang.lib", ;
                         "libmisc.lib", ;
                         "libmysqldll.lib", ;
                         "macro.lib", ;
                         "mysql.lib", ;
                         "odbc32.lib", ;
                         "pcrepos.lib", ;
                         "pp.lib", ;
                         "rdd.lib", ;
                         "rddads.lib", ;
                         "rddcdx.lib", ;
                         "rddfpt.lib", ;
                         "rddntx.lib", ;
                         "rtl.lib", ;
                         "tip.lib", ;
                         "vm.lib", ;
                         "ziparchive.lib", ;
                         "zlib1.lib" }
            IF File( cHarbourFolderL + '\' + i )
               cOut += '   echo $(HRB_LIB_DIR)\' + i + ' >> b32.bc' + CRLF
            ENDIF
         NEXT
         IF ! Empty( ::cLibXH )
            cOut += '   echo ' + ::cLibXH + ' >> b32.bc' + CRLF
         ENDIF
         cOut += '   echo $(CC_LIB_DIR)\crt.lib >> b32.bc' + CRLF
         For Each i In { "winmm.lib", ;
                         "kernel32.lib", ;
                         "user32.lib", ;
                         "ws2_32.lib", ;
                         "iphlpapi", ;
                         "advapi32.lib", ;
                         "gdi32.lib", ;
                         "winspool.lib", ;
                         "ole32.lib", ;
                         "uuid.lib", ;
                         "oleaut32.lib", ;
                         "mpr.lib", ;
                         "comdlg32.lib", ;
                         "comctl32.lib", ;
                         "olepro32.lib", ;
                         "shell32.lib", ;
                         "vfw32.lib", ;
                         "wsock32.lib" }
            IF File( cCompFolderL + '\' + i )
               cOut += '   echo $(CC_LIB_DIR)\' + i + ' >> b32.bc' + CRLF
            ENDIF
         NEXT
         IF ! Empty( ::cLibCC )
            cOut += '   echo ' + ::cLibCC + ' >> b32.bc' + CRLF
         ENDIF
         cOut += '   echo _temp.res >> b32.bc' + CRLF
         cOut += '   $(ILINK_EXE)  /SUBSYSTEM:' + iif( nOption == 2, "CONSOLE", "WINDOWS" ) + ' @b32.bc' + CRLF
         cOut += CRLF
         For i := 1 To nPrgFiles
            cOut += CRLF
            cOut += '$(C_DIR)\' + aPrgFiles[i] + '.c : ' + cFolder + aPrgFiles[i] + '.prg' + CRLF
            cOut += '   $(HARBOUR_EXE) $(HARBOUR_FLAGS) $** -o$@'  + CRLF
            cOut += CRLF
            cOut += '$(OBJ_DIR)\' + aPrgFiles[i] + '.obj : $(C_DIR)\' + aPrgFiles[i] + '.c' + CRLF
            cOut += '   $(CC) $(COBJFLAGS) -Fo$@ $**' + CRLF
         NEXT i
         hb_MemoWrit( '_temp.bc', cOut )
         // Build batch to create RC temp file and launch make utility
         cOut := ''
         cOut += '@echo off' + CRLF
         cOut += 'echo #define oohgpath ' + cMiniGUIFolderR + ' > ' + cFolder + '_oohg_resconfig.h' + CRLF
         cOut += 'echo. > "' + cMiniGUIFolderR + '\filler"' + CRLF
         cOut += 'copy /b "' + cMiniGUIFolderR + '\oohg.rc" _temp.rc > NUL' + CRLF
         FOR i := 1 TO Len( aRcFiles )
            cOut += 'copy /b "' + cMiniGUIFolderR + '\filler" + _temp.rc _aux.rc > NUL' + CRLF
            cOut += 'copy /b "' + aRcFiles[ i ] + '.rc" + _aux.rc _temp.rc > NUL' + CRLF
         NEXT i
         cOut += ::cMakeTool + ' /f' + cFolder + '_temp.bc > ' + cFolder + 'error.txt' + CRLF
         hb_MemoWrit( '_build.bat', cOut )
         // Create folder for objects
         CreateFolder( cFolder + 'OBJ' )
         // Build
         EXECUTE FILE '_build.bat' WAIT HIDE
   CASE ::nTBuild == 1 // Compile.bat
         // Check for compile file
         IF ! File( 'compile.bat' ) .AND. ! IsFileInPath( 'compile.bat' )
            ::Form_Wait:Hide()
            MsgInfo( i18n( "Copy OOHG's COMPILE.BAT to the project's folder or add OOHG root folder to PATH." ), 'OOHG IDE+' )
            Break
         ENDIF
         // Build auxiliary source file
         nItems := ::Form_Tree:Tree_1:ItemCount
         aPrgFiles := {}
         For i := 1 To nItems
            cFile := ::Form_Tree:Tree_1:Item( i )
            IF ::SearchType( ::SearchItem( cFile, "PRG" ) ) == "PRG" .AND. ! cFile == "PRG"
               cFile := Upper( AllTrim( cFile + '.PRG' ) )
               IF aScan( aPrgFiles, cFile ) == 0
                  aAdd( aPrgFiles, cFile )
               ENDIF
            ENDIF
         NEXT i
         nPrgFiles := Len( aPrgFiles )
         IF nPrgFiles == 0
            ::Form_Wait:Hide()
            MsgStop( i18n( 'Project has no .PRG files.' ), 'OOHG IDE+' )
            Break
         ENDIF
         cOut := ''
         For i := 1 To nPrgFiles
            cOut += "#include '" + aPrgFiles[i] + "'" + CRLF + CRLF
         NEXT i
         hb_MemoWrit( cPrgName + '.prg', cOut )
         // Compile and link
         cDosComm := 'CMD.EXE /c compile ' + cPrgName + ' /nr /l' + iif( nOption == 2, " /d", "" )
         EXECUTE FILE cDosComm WAIT HIDE
         DELETE FILE ( cPrgName + '.prg' )
      ENDCASE

      // Check for errors
      cError := MemoRead( 'error.txt' )
      cError1 := Upper( cError )
      IF At( ' ERROR ', cError1 ) > 0 .OR. At( ' FATAL ', cError1 ) > 0 .OR. At( 'LD RETURNED 1 EXIT STATUS', cError1 ) > 0
         ::Form_Wait:Hide()
         ::ViewErrors( cError )
         Break
      ELSEIF ! File( ::cExe )
         ::Form_Wait:Hide()
         MsgStop( i18n( "File is missing." ) + DQM( ::cExe ), 'OOHG IDE+' )
         Break
      ENDIF

      // Rename or move
      cOut := Upper( AllTrim( ::cOutFile ) )
      IF ! Empty( ::cOutFile )
         IF Right( cOut, 4 ) != ".EXE"
            cOut += ".EXE"
         ENDIF
         ::cOutFile := cOut
         cDosComm := 'CMD.EXE /c move /y ' + ::cExe + " " + cOut
         EXECUTE FILE cDosComm WAIT HIDE
         IF ! File( cOut )
            ::Form_Wait:Hide()
            MsgStop( i18n( "Can't move or rename EXE file." ), 'OOHG IDE+' )
            Break
         ENDIF
         ::cExe := cOut
      ENDIF

      // Cleanup
      BorraTemp( cFolder )
      ::Form_Wait:Hide()
      IF nOption == 0
         MsgInfo( i18n( 'Project builded.' ), 'OOHG IDE+' )
      ELSEIF nOption == 1 .or. nOption == 2
         EXECUTE FILE ::cExe
      ENDIF
   END SEQUENCE

   CursorArrow()
   ::Form_Tree:button_09:Enabled := .T.
   ::Form_Tree:button_10:Enabled := .T.
   ::Form_Tree:button_11:Enabled := .T.
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ViewErrors( wr ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   LOCAL Form_Errors, oEdit, oButt

   IF HB_IsString( wr )
      DEFINE WINDOW Form_Errors OBJ Form_Errors ;
         AT 10, 10 ;
         CLIENTAREA WIDTH 650 HEIGHT 480 ;
         TITLE 'Error Report' ;
         ICON 'IDE_EDIT' ;
         MODAL ;
         BACKCOLOR ::aSystemColor ;
         ON INIT ( oButt:Col := Form_Errors:ClientWidth - 40, ;
                   oEdit:Width := Form_Errors:ClientWidth - 45, ;
                   oEdit:Height := Form_Errors:ClientHeight ) ;
         ON SIZE ( oButt:Col := Form_Errors:ClientWidth - 40, ;
                   oEdit:Width := Form_Errors:ClientWidth - 45, ;
                   oEdit:Height := Form_Errors:ClientHeight ) ;

         @ 0, 0 EDITBOX Edit_1 ;
            OBJ oEdit ;
            WIDTH 590 ;
            HEIGHT 445 ;
            VALUE wr ;
            READONLY ;
            FONT 'FixedSys' ;
            SIZE 10 ;
            BACKCOLOR {255, 255, 235}

         @ 10, 595 Button Butt_1 ;
            OBJ oButt ;
            CAPTION 'Exit' ;
            ACTION Form_Errors:Release() ;
            WIDTH 35 ;
            FLAT
      END WINDOW

      CENTER WINDOW Form_Errors
      ACTIVATE WINDOW Form_Errors
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ViewSource( wr ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   IF ! HB_IsString( wr )
      RETURN NIL
   ENDIF
   SET INTERACTIVECLOSE ON

   DEFINE WINDOW c_source ;
      AT 10,10 ;
      WIDTH 625 ;
      HEIGHT 460 ;
      TITLE 'Source code' ;
      ICON 'IDE_EDIT' ;
      MODAL ;
      BACKCOLOR ::aSystemColor

      @ 0,0 EDITBOX edit_1 ;
         WIDTH 573 ;
         HEIGHT 425 ;
         VALUE WR ;
         READONLY ;
         FONT 'FixedSys' ;
         SIZE 10 ;
         BACKCOLOR { 255, 255, 235 }

      @ 10,575 Button _exiterr ;
         CAPTION 'Exit' ;
         ACTION ThisWindow.Release() ;
         WIDTH 35

      @ 50,575 Button _prints ;
         CAPTION 'Print' ;
         ACTION PrintItem( wr ) ;
         WIDTH 35

     ON KEY ESCAPE ACTION ThisWindow.Release()
   END WINDOW

   CENTER WINDOW c_source
   ACTIVATE WINDOW c_source
   SET INTERACTIVECLOSE OFF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD NewProject() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   IF Len( ::aEditors ) > 0
      MsgStop( i18n( "You can't open a new project until the FMG being edited is closed." ), 'OOHG IDE+' )
      RETURN NIL
   ENDIF

   IF ! ::lPsave
      IF MsgYesNo( i18n( 'Save changes made to current project?' ), 'OOHG IDE+' )
         ::SaveProject()
      ENDIF
   ENDIF

   ::Form_Tree:Tree_1:DeleteAllItems()
   ::Form_Tree:Tree_1:AddItem( "Project", 0 )
   ::Form_Tree:Tree_1:AddItem( "FMG", 1 )
   ::Form_Tree:Tree_1:AddItem( "PRG", 1 )
   ::Form_Tree:Tree_1:AddItem( "CH", 1 )
   ::Form_Tree:Tree_1:AddItem( "RPT", 1 )
   ::Form_Tree:Tree_1:AddItem( "RC", 1 )
   ::Form_Tree:Tree_1:Value := 1
   ::Form_Tree:Title := APP_FULL_NAME
   ::Form_Tree:StatusBar:Item( 1, ::Form_Tree:Title )
   ::lPsave := .T.
   ::cProjectName := ''
   ::Form_Tree:Add:Enabled := .F.
   ::Form_Tree:Button_1:Enabled := .F.

   // Default values from exe startup folder
   ::ReadINI( ::cIDE_Folder + '\hmi.ini' )
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD OpenProject() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL pmgFolder, cName

   IF Len( ::aEditors ) > 0
      MsgStop( i18n( 'You can�t open another project until the FMG being edited is closed.' ), 'OOHG IDE+' )
      RETURN NIL
   ENDIF

   cName := GetFile( { { i18n( 'OOHG IDE+ project files *.pmg' ), '*.pmg' } }, 'OOHG IDE+ - ' + i18n( 'Open Project'), "", .F., .F. )
   IF Len( cName ) > 0
      pmgFolder := OnlyFolder( cName )
      IF ! Empty( pmgFolder )
         ::cProjFolder := pmgFolder
         DirChange( pmgFolder )
      ENDIF
      ::InitializeProject( cName )
      ::Form_Tree:Add:Enabled := .T.
      ::Form_Tree:Button_1:Enabled := .T.
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD InitializeProject( cName ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL aLine[0], nCount, cItems, sw, i, cFile

   ::cProjectName := cName

   // From project folder
   ::ReadINI( ::cProjFolder + "\hmi.ini" )

   cItems := MemoRead( ::cProjectName )
   nCount := MLCount( cItems )

   ::Form_Tree:Title := APP_FULL_NAME + i18n( ' - Project: ' ) + ::cProjectName
   ::Form_Tree:Tree_1:DeleteAllItems()
   ::Form_Tree:Tree_1:AddItem( "Project", 0 )
   ::Form_Tree:Tree_1:AddItem( "FMG", 1 )
   ::Form_Tree:Tree_1:AddItem( "PRG", 1 )
   ::Form_Tree:Tree_1:AddItem( "CH", 1 )
   ::Form_Tree:Tree_1:AddItem( "RPT", 1 )
   ::Form_Tree:Tree_1:AddItem( "RC", 1 )
   ::Form_Tree:StatusBar:Item( 1, ::Form_Tree:Title )

   sw := 0
   FOR i := 1 TO nCount
      aAdd( aLine, RTrim( MemoLine( cItems, NIL, i ) ) )
      aLine[i] := StrTran( aLine[i], LF, "" )
      aLine[i] := StrTran( aLine[i], CR, "" )
      aLine[i] := RTrim( aLine[i] )

     cFile := Upper( aLine[i] )
      DO CASE
      CASE cFile == "PROJECT" .OR. cFile == "[PROJECT]"
      CASE cFile == "FORM MODULE" .OR. cFile == "[FMG]"
         sw := 1
      CASE cFile == "PRG MODULE" .OR. cFile == "[PRG]"
         sw := 2
      CASE cFile == "CH MODULE" .OR. cFile == "[CH]"
         sw := 3
      CASE cFile == "RPT MODULE" .OR. cFile == "[RPT]"
         sw := 4
      CASE cFile == "RC MODULE" .OR. cFile == "[RC]"
         sw := 5
      OTHERWISE
         IF sw == 1
            ::NewFMG( aLine[i] )
         ENDIF
         IF sw == 2
            ::NewPRG( aLine[i] )
         ENDIF
         IF sw == 3
            ::NewCH( aLine[i] )
         ENDIF
         IF sw == 4
            ::NewRPT( aLine[i] )
         ENDIF
         IF sw == 5
            ::NewRC( aLine[i] )
         ENDIF
      ENDCASE
   NEXT i

   ::Form_Tree:Tree_1:Value := 1
   ::Form_Tree:Tree_1:Expand( 1 )
   ::lPsave := .T.
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD SaveProject() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL Output, nItems, i, cItem, nPos, cExt, lRet, cName, aPrgFiles, cPrg

   IF Empty( ::cProjectName ) .AND. ! Empty( ::cFile )
      nPos := At( ".", ::cFile )
      cExt := Lower( SubStr( ::cFile, nPos + 1, 3 ) )
      cName := SubStr( ::cFile, 1, nPos - 1 )
      DO CASE
      CASE cExt == "ch"
         lRet := ::NewCH( cName )
      CASE cExt == "fmg"
         lRet := ::NewFMG( cName )
      CASE cExt == "prg"
         lRet := ::NewPRG( cName )
      CASE cExt == "rc"
         lRet := ::NewRC( cName )
      CASE cExt == "rpt"
         lRet := ::NewRPT( cName )
      OTHERWISE
         RETURN NIL
      ENDCASE
      IF ! lRet
         RETURN NIL
      ENDIF
      ::cFile := ""
   ENDIF

   Output := ''
   nItems := ::Form_Tree:Tree_1:ItemCount
   FOR i := 1 TO nItems
      cItem := ::Form_Tree:Tree_1:Item( i )
      DO CASE
      CASE Upper( cItem ) == "PROJECT"
         cItem := "[PROJECT]"
      CASE cItem == "FMG"
         cItem := "[FMG]"
      CASE cItem == "PRG"
         cItem := "[PRG]"
      CASE cItem == "CH"
         cItem := "[CH]"
      CASE cItem == "RPT"
         cItem := "[RPT]"
      CASE cItem == "RC"
         cItem := "[RC]"
      ENDCASE
      Output += cItem + CRLF
   NEXT i

   IF Empty( ::cProjectName )
      ::cProjectName := PutFile( { { i18n( 'OOHG IDE+ project files *.pmg' ), '*.pmg' } }, 'OOHG IDE+ - ' + i18n( 'Save Project' ) )
      IF Upper( Right( ::cProjectName, 4 ) ) != '.PMG'
         ::cProjectName += '.pmg'
      ENDIF
      IF Upper( ::cProjectName ) == '.PMG'
         ::cProjectName := ''
      ENDIF
   ENDIF

   ::cProjFolder := OnlyFolder( ::cProjectName )
   ::Form_Tree:Title := APP_FULL_NAME + i18n( ' - Project: ' ) + ::cProjectName
   ::Form_Tree:StatusBar:Item( 1, ::Form_Tree:Title )

   IF Empty( ::cProjectName )
      MsgStop( i18n( 'Project was not saved because it has no name.' ), 'OOHG IDE+' )
   ELSE
      nItems := ::Form_Tree:Tree_1:ItemCount
      aPrgFiles := {}
      FOR i := 1 TO nItems
         cPrg := ::Form_Tree:Tree_1:Item( i )
         IF ::SearchType( ::SearchItem( cPrg, "PRG" ) ) == "PRG" .AND. ! cPrg == "PRG"
            cPrg := Upper( AllTrim( cPrg + '.PRG' ) )
            IF aScan( aPrgFiles, cPrg ) == 0
               aAdd( aPrgFiles, cPrg )
            ENDIF
         ENDIF
      NEXT i
      IF ! Empty( ::cFile ) .AND. aScan( aPrgFiles, Upper( ::cFile ) ) # 0
         MsgStop( i18n( "Project was not saved because its name is equal to one of its PRG files." ), 'OOHG IDE+' )
         ::cProjectName := ''
      ELSE
         ::Form_Tree:Add:Enabled := .T.
         ::Form_Tree:Button_1:Enabled := .T.
         hb_MemoWrit( ::cProjectName, Output )
         ::SaveINI( ::cProjFolder + '\hmi.ini' )
         ::lPsave := .T.
         MsgInfo( i18n( 'Project was saved.' ), 'OOHG IDE+' )
      ENDIF
   ENDIF

RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD NewFMG( cForm ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL nValue, lRet := .F.

   IF Val( cForm ) > 0
      MsgStop( i18n( "The FMG's name must begin with a letter." ), 'OOHG IDE+' )
   ELSEIF Len( cForm ) > 0
      IF At( '.', cForm ) # 0
         MsgStop( i18n( 'The name must not contain a dot (.) in it.' ), 'OOHG IDE+' )
      ELSEIF ::SearchType( ::SearchItem( cForm, "FMG" ) ) == "FMG"
         MsgStop( i18n( 'This name is not allowed.' ), 'OOHG IDE+' )
      ELSE
         nValue := ::SearchItem( "FMG", "FMG" )
         ::Form_Tree:Tree_1:AddItem( cForm, nValue )
         ::lPsave := .F.
         lRet := .T.
      ENDIF
   ENDIF
RETURN lRet

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD NewPRG( cPrg ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL nValue, lRet := .F.

   IF Val( cPrg ) > 0
      MsgStop( i18n( "The PRG's name must begin with a letter." ), 'OOHG IDE+' )
   ELSEIF Len( cPrg ) > 0
      IF At( '.', cPrg ) # 0
         MsgStop( i18n( "The PRG's name must not contain a dot (.) in it." ), 'OOHG IDE+' )
      ELSEIF ::SearchType( ::SearchItem( cPrg, "PRG" ) ) == "PRG"
         MsgStop( i18n( 'This name is not allowed.' ), 'OOHG IDE+' )
      ELSEIF ::SearchItem( cPrg, "PRG" ) # 0
         MsgStop( i18n( 'Another PRG with the same name is already part of the project.' ), 'OOHG IDE+' )
      ELSEIF Upper( cPrg ) == Upper( AllTrim( DelExt( ::cProjectName ) ) )
         MsgStop( i18n( "The PRG's name must not be equal to the project's name." ), 'OOHG IDE+' )
      ELSE
         nValue := ::SearchItem( "PRG", "PRG" )
         ::Form_Tree:Tree_1:AddItem( cPrg, nValue )
         ::lPsave := .F.
         lRet := .T.
      ENDIF
   ENDIF
RETURN lRet

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD NewCH( cCH ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL nValue, lRet := .F.

   IF Val( cCH ) > 0
      MsgStop( i18n( "The CH's name must begin with a letter." ), 'OOHG IDE+' )
   ELSEIF Len( cCH ) > 0
      IF At( '.', cCH ) # 0
         MsgStop( i18n( "The CH's name must not contain a dot (.) in it." ), 'OOHG IDE+' )
      ELSEIF ::SearchType( ::SearchItem( cCH, "CH" ) ) == "CH"
         MsgStop( i18n( 'This name is not allowed.' ), 'OOHG IDE+' )
      ELSEIF ::SearchItem( cCH, "CH" ) # 0
         MsgStop( i18n( 'Another CH with the same name is already part of the project.' ), 'OOHG IDE+' )
      ELSE
         nValue := ::SearchItem( "CH", "CH" )
         ::Form_Tree:Tree_1:AddItem( cCH, nValue )
         ::lPsave := .F.
         lRet := .T.
      ENDIF
   ENDIF
RETURN lRet

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD NewRC( cRC ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL nValue, lRet := .F.

   IF Val( cRC ) > 0
      MsgStop( i18n( "The RC's name must begin with a letter." ), 'OOHG IDE+' )
   ELSEIF Len( cRC ) > 0
      IF At( '.', cRC ) # 0
         MsgStop( i18n( "The RC's name must not contain a dot (.) in it." ), 'OOHG IDE+' )
      ELSEIF ::SearchType( ::SearchItem( cRC, "RC" ) ) == "RC"
         MsgStop( i18n( 'This name is not allowed.' ), 'OOHG IDE+' )
      ELSEIF ::SearchItem( cRC, "RC" ) # 0
         MsgStop( i18n( 'Another RC with the same name is already part of the project.' ), 'OOHG IDE+' )
      ELSE
         nValue := ::SearchItem( "RC", "RC" )
         ::Form_Tree:Tree_1:AddItem( cRC, nValue )
         ::lPsave := .F.
         lRet := .T.
      ENDIF
   ENDIF
RETURN lRet

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD NewRPT( cRpt ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL nValue, lRet := .F.

   IF Val( cRpt ) > 0
      MsgStop( i18n( "The RPT's name must begin with a letter." ), 'OOHG IDE+' )
   ELSEIF Len( cRpt ) > 0
      IF At( '.', cRpt ) # 0
         MsgStop( i18n( "The RPT's name must not contain a dot (.) in it." ), 'OOHG IDE+' )
      ELSEIF ::SearchType( ::SearchItem( cRpt, "RPT" ) ) == "RPT"
         MsgStop( i18n( 'This name is not allowed.' ), 'OOHG IDE+' )
      ELSEIF ::SearchItem( cRpt, "RPT" ) # 0
         MsgStop( i18n( 'Another RPT with the same name is already part of the project.' ), 'OOHG IDE+' )
      ELSE
         nValue := ::SearchItem( "RPT", "RPT" )
         ::Form_Tree:Tree_1:AddItem( cRpt, nValue )
         ::lPsave := .F.
         lRet := .T.
      ENDIF
   ENDIF
RETURN lRet

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD DeleteItem() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL cItem

   cItem := ::Form_Tree:Tree_1:Item( ::Form_Tree:Tree_1:Value )
   IF cItem == "FMG" .OR. cItem == "PRG" .OR. cItem == "Project" .OR. cItem == "CH" .OR. cItem == "RPT" .OR. cItem == "RC"
      MsgStop( i18n( "This item can't be deleted." ), 'OOHG IDE+' )
      RETURN NIL
   ENDIF

   IF MsgYesNo( i18n( "Select [Yes] to confirm the removal of item " ) + cItem + ".", "OOHG IDE+" )
      ::Form_Tree:Tree_1:DeleteItem( ::Form_Tree:Tree_1:Value )
      ::lPsave := .F.
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD SearchItem( cNameItem, cGroup ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL nItems, i, cItem, sw := 0

   nItems := ::Form_Tree:Tree_1:ItemCount
   FOR i := 1 TO nItems
      cItem := ::Form_Tree:Tree_1:Item( i )
      IF cItem == cGroup
         sw := 1
      ENDIF
      IF sw == 1
         IF Upper( cItem ) == Upper( cNameItem )
            RETURN i
         ENDIF
      ENDIF
   NEXT i
RETURN 0

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD SearchType( nValue ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL i

   IF HB_ISNUMERIC( nValue ) .AND. nValue > 0 .AND. nValue <= ::Form_Tree:Tree_1:ItemCount
      FOR i := nValue TO 1 STEP -1
          IF ::Form_Tree:Tree_1:Item( i ) == "FMG"
             RETURN "FMG"
          ENDIF
          IF ::Form_Tree:Tree_1:Item( i ) == "PRG"
             RETURN "PRG"
          ENDIF
          IF ::Form_Tree:Tree_1:Item( i ) == "CH"
             RETURN "CH"
          ENDIF
          IF ::Form_Tree:Tree_1:Item( i ) == "RPT"
             RETURN "RPT"
          ENDIF
          IF ::Form_Tree:Tree_1:Item( i ) == "RC"
             RETURN "RC"
          ENDIF
      NEXT i
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ModifyItem( cParameter, cParent ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL Output, cItem := SubStr( cParameter, 1, At( ".", cParameter ) - 1 )

   IF cParent == "PRG"
      IF File( cParameter )
         ::OpenFile( cParameter )
      ELSE
         Output := '/*        IDE: OOHG IDE+' + CRLF
         Output += ' *     Project: ' + ::cProjectName + CRLF
         Output += ' *        Item: ' + cParameter + CRLF
         Output += ' * Description: ' + CRLF
         Output += ' *      Author: ' + CRLF
         Output += ' *        Date: ' + DtoC( Date() ) + CRLF
         Output += ' */' + CRLF + CRLF

         Output += "#include 'oohg.ch'" + CRLF + CRLF
         Output += "*------------------------------------------------------*" + CRLF
         IF ::SearchItem( cItem, cParent ) == ( ::SearchItem( cParent, cParent ) + 1 )
            Output += 'FUNCTION Main()' + CRLF
         ELSE
            Output += 'FUNCTION ' + cItem + '()' + CRLF
         ENDIF
         Output += "*------------------------------------------------------*" + CRLF + CRLF
         Output += 'RETURN NIL' + CRLF + CRLF
         hb_MemoWrit( cParameter, Output )
         ::OpenFile( cParameter )
      ENDIF
   ELSEIF cParent == "CH"
      IF File( cParameter )
         ::OpenFile( cParameter )
      ELSE
         Output := '/*        IDE: OOHG IDE+' + CRLF
         Output += ' *     Project: ' + ::cProjectName + CRLF
         Output += ' *        Item: ' + cParameter + CRLF
         Output += ' * Description:' + CRLF
         Output += ' *      Author:' + CRLF
         Output += ' *        Date: ' + DtoC( Date() ) + CRLF
         Output += ' */' + CRLF + CRLF
         Output += '#' + CRLF
         hb_MemoWrit( cParameter, Output )
         ::OpenFile( cParameter )
      ENDIF
   ELSEIF cParent == "RC"
      IF File( cParameter )
         ::OpenFile( cParameter )
      ELSE
         Output:='//         IDE: OOHG IDE+' + CRLF
         Output+='//     Project: ' + ::cProjectName + CRLF
         Output+='//        Item: ' + cParameter + CRLF
         Output+='// Description:' + CRLF
         Output+='//      Author:' + CRLF
         Output+='//        Date: ' + DToC( Date() ) + CRLF
         Output+='// Name    Format   Filename' + CRLF
         Output+='// MYBMP   BITMAP   res\Next.bmp' + CRLF
         Output+='// Last line of this file must end with a CRLF' + CRLF
         hb_MemoWrit( cParameter, Output )
         ::OpenFile( cParameter )
      ENDIF
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD EditorExit( aPositions, nEditorIndex ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   ::aPositions := aClone( aPositions )
   ::SaveINI( ::cProjFolder + '\hmi.ini' )

   _OOHG_DeleteArrayItem( ::aEditors, nEditorIndex )

   IF ::lCloseOnFormExit .AND. Len( ::aEditors ) == 0
      RELEASE WINDOW ALL
   ELSE
      ::Form_Tree:button_07:Enabled := .T.
      ::Form_Tree:button_09:Enabled := .T.
      ::Form_Tree:button_10:Enabled := .T.
      ::Form_Tree:button_11:Enabled := .T.
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD SaveFile() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   IF AllTrim( ::Form_Edit:edit_1:Value ) == ''
      IF File( ::cItemFile )
         DELETE FILE ( ::cItemFile )
      ENDIF
      ::lSave := .T.
   ELSE
      IF hb_MemoWrit( ::cItemFile, AllTrim( ::Form_Edit:edit_1:Value ) )
         ::lSave := .T.
      ELSE
         MsgStop( i18n( 'Error writing ' ) + ::cItemFile + '.', 'OOHG IDE+' )
      ENDIF
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD OpenFile( cFile ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL cOutput, nwidth, nheight, wq, nRAt, cRun, ll, i, cTextedit, nInterval

   CursorWait()
   ::lSave := .T.
   ::nPosText := 0
   ::cText := ''
   ::Form_Wait:Show()
   ::Form_Wait:hmi_label_101:Value := i18n( 'Loading ...' )
   ::cItemFile := cFile

   IF Len( AllTrim( ::cExtEditor ) ) == 0
      cTextEdit := MemoRead( cFile )
      cTextEdit := StrTran( cTextEdit, HTAB, ::nTabSize )
      cOutput := ''
      FOR i := 1 TO MLCount( cTextEdit )
          cOutput := cOutput + RTrim( MemoLine( cTextEdit, 500, i ) ) + CRLF
      NEXT i
      cTextEdit := RTrim( cOutput )
      DO WHILE .T.
         wq := SubStr( cOutput, Len( cTextEdit ) - 1, 1 )
         IF wq == CR .OR. wq = LF
            cTextEdit := Left( cTextEdit, Len( cTextEdit ) - 1 )
         ELSE
            cTextEdit := Left( cTextEdit, Len( cTextEdit ) - 1 ) + CRLF
            EXIT
         ENDIF
      ENDDO

      IF IsWindowDefined( Form_Edit )
         ::Form_Wait:Hide()
         MsgStop( i18n( "Sorry, the IDE can't edit more than one file at a time." ), 'OOHG IDE+' )
         RETURN NIL
      ENDIF

      nWidth := ::Form_Tree:Width - ( ::Form_Tree:Width / 3.5 )
      nHeight := ::Form_Tree:Height - 160

      DEFINE WINDOW Form_Edit OBJ ::Form_Edit ;
         AT 109, 80 ;
         WIDTH nWidth ;
         HEIGHT nHeight ;
         TITLE APP_FULL_NAME + i18n( ' - Project: ' ) + cFile ;
         ICON 'IDE_EDIT' ;
         CHILD ;
         BACKCOLOR ::aSystemColor ;
         ON SIZE { || ::Form_Edit:Edit_1:Width := ::Form_Edit:Width - 15, ::Form_Edit:Edit_1:Height := ::Form_Edit:Height - 90 }

         @ 30, 2 RICHEDITBOX edit_1 ;
            WIDTH ::Form_Edit:Width - 15 ;
            HEIGHT ::Form_Edit:Height - 90 ;
            VALUE cTextEdit ;
            FONT "Courier New" ;
            SIZE 10 ;
            BACKCOLOR {255, 255, 235} ;
            MAXLENGTH 256000 ;
            ON CHANGE ::lSave := .F. ;
            ON GOTFOCUS ::PosXY()

         IF Len( ::Form_Edit:edit_1:Value ) > 100000
            MsgInfo( i18n( 'You should use another program editor.' ), 'OOHG IDE+' )
         ENDIF

         IF Len( ::Form_Edit:edit_1:Value ) > 250000
            MsgStop( i18n( 'You must use another program editor.' ), 'OOHG IDE+' )
            RETURN NIL
         ENDIF

         ll := MLCount( ::Form_Edit:edit_1:Value )
         IF ll <= 800
            nInterval := 1000
         ELSE
            nInterval := Int( ( ( ( ll - 800 ) / 800 ) + 1 ) * 2000 )
         ENDIF

         DEFINE TIMER Timit INTERVAL nInterval ACTION ::LookChanges()

         DEFINE SPLITBOX
            DEFINE TOOLBAR 0 BUTTONSIZE 20, 20 FLAT
               BUTTON button_2 TOOLTIP i18n( 'Exit (Esc)' )        PICTURE 'IDE_EXIT'  ACTION ::SaveAndExit()
               BUTTON button_1 TOOLTIP i18n( 'Save (F2)' )         PICTURE 'IDE_SAVE'  ACTION ::SaveFile()
               BUTTON button_3 TOOLTIP i18n( 'Find (Ctrl-F)' )     PICTURE 'IDE_FIND'  ACTION ::TxtSearch()
               BUTTON button_4 TOOLTIP i18n( 'Next (F3)' )         PICTURE 'IDE_NEXT'  ACTION ::NextSearch()
               BUTTON button_5 TOOLTIP i18n( 'Go (Ctrl-G)' )       PICTURE 'IDE_GO'    ACTION ::GoLine()
               nRAt := RAt( '.prg', cFile )
               IF nRAt > 0
               BUTTON button_6 TOOLTIP i18n( 'Reformat (Ctrl-R)' ) PICTURE 'IDE_REFOR' ACTION ::Reforma( ::Form_Edit:edit_1:Value )
               ENDIF
            END TOOLBAR
         END SPLITBOX

         ON KEY F2     OF Form_Edit ACTION ::SaveFile()
         ON KEY F3     OF Form_Edit ACTION ::NextSearch()
         ON KEY CTRL+F OF Form_Edit ACTION ::TxtSearch()
         ON KEY CTRL+G OF Form_Edit ACTION ::GoLine()
         ON KEY ESCAPE OF Form_Edit ACTION ::SaveAndExit()
         IF nRAt > 0
         ON KEY CTRL+R OF Form_Edit ACTION ::Reforma( ::Form_Edit:edit_1:Value )
         ENDIF

         DEFINE STATUSBAR
            STATUSITEM " Lin:     Col:     Caret:     " WIDTH 20
            KEYBOARD
            DATE WIDTH 100
            CLOCK WIDTH 90
         END STATUSBAR

         DEFINE CONTEXT MENU
            MENUITEM i18n( 'Cut' )        ACTION Send_Cut()
            MENUITEM i18n( 'Copy' )       ACTION Send_Copy()
            MENUITEM i18n( 'Paste' )      ACTION Send_Paste()
            MENUITEM i18n( 'Delete' )     ACTION _PushKey( 32 )
            SEPARATOR
            MENUITEM i18n( 'Select all' ) ACTION Send_SelectAll()
         END MENU
      END WINDOW

      CENTER WINDOW Form_Edit
      ::Form_Wait:Hide()
      CursorArrow()
      ACTIVATE WINDOW Form_Edit
   ELSE
      cRun := ::cExtEditor + ' ' + cFile

      ::Form_Wait:Hide()
      CursorArrow()
      EXECUTE FILE cRun WAIT
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Reforma( cContenido ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL ntab := 0
LOCAL lcero := 0
LOCAL coutput := ''
LOCAL swclase := 0
LOCAL cantlin := ''
LOCAL swcase := 0
LOCAL swc := 0
LOCAL i, clineaorig, clinea, cllinea, cdeslin, clinea1
LOCAL largo

   ::Form_Wait:hmi_label_101:value := i18n( 'Reformating ...' )
   ::Form_Wait:Show()

   ccontenido := StrTran( ccontenido, HTAB, ::TabSize )
   largo:=mlcount(ccontenido)
   for i := 1 to largo
       IF i > 0
          cantlin:=ltrim(rtrim(memoline(ccontenido,500,i-1)))
       ENDIF
       IF i < largo
          cdeslin:=ltrim(rtrim(memoline(ccontenido,500,i+1)))
       ENDIF
       clineaorig:=memoline(ccontenido,500,i)
       clinea1:=rtrim(clineaorig)
       clinea:=ltrim(rtrim(clineaorig))
       cllinea:=upper(clinea)
       DO CASE
          CASE substr(cllinea,1,4) == 'CASE' .OR. substr(cllinea,1,9) == 'OTHERWISE'
             IF swcase=0
                coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
                ntab:=ntab+3
                swcase:=-1
             ELSE
                IF swcase=-1
                   ntab:=ntab-3
                   coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
                   swcase:=1
                   ntab:=ntab+3
                ELSE
                   ntab:=ntab-3
                   coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
                   swcase=-1
                   ntab:=ntab+3
                ENDIF
             ENDIF
          CASE substr(cllinea,1,9)='DO WHILE '
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,17)='#PRAGMA BEGINDUMP'
             coutput:=coutput+replicate(' ',ntab)+clineaorig+CRLF
             swc:=1
          CASE substr(cllinea,1,15)='#PRAGMA ENDDUMP'
             coutput:=coutput+replicate(' ',ntab)+clineaorig+CRLF
             swc:=0
          CASE substr(cllinea,1,9)='BEGIN INI'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,7)='END INI'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,9)='FUNCTION '
             IF substr(cantlin,1,2) # '*-'
                coutput:=coutput+CRLF
                coutput:=coutput+'*-------------------------'+CRLF
             ENDIF
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             IF substr(cdeslin,1,2) # '*-'
                coutput:=coutput+'*-------------------------'+CRLF
             ENDIF
          CASE substr(cllinea,1,16)='STATIC FUNCTION '
             IF substr(cantlin,1,2) # '*-'
                coutput:=coutput+CRLF
                coutput:=coutput+'*-------------------------'+CRLF
             ENDIF
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             IF substr(cdeslin,1,2) # '*-'
                coutput:=coutput+'*-------------------------'+CRLF
             ENDIF
          CASE substr(cllinea,1,10)='PROCEDURE '
             IF substr(cantlin,1,2) # '*-'
                coutput:=coutput+CRLF
                coutput:=coutput+'*-------------------------'+CRLF
             ENDIF
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             IF substr(cdeslin,1,2) # '*-'
                coutput:=coutput+'*-------------------------'+CRLF
             ENDIF
          CASE substr(cllinea,1,9)='METHOD '
             IF swclase=0 .AND. substr(cantlin,1,2) # '*-'
                coutput:=coutput+CRLF
                coutput:=coutput+'*-------------------------'+CRLF
             ENDIF
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             IF swclase=0 .AND. substr(cdeslin,1,2) # '*-'
                coutput:=coutput+'*-------------------------'+CRLF
             ENDIF
          CASE substr(cllinea,1,5)='CLASS'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
             swclase:=1
          CASE substr(cllinea,1,7)='DO CASE'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
             swcase:=0
          CASE substr(cllinea,1,7)='ENDCASE'
             ntab:=ntab-6
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF

          CASE substr(cllinea,1,8)='ENDCLASS'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             swclase:=0
          CASE substr(cllinea,1,3)='IF '
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,5)='ENDIF' .or. substr(cllinea,1,6)='END IF'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,5)='ENDDO'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,6)='ELSEIF'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3

          CASE substr(cllinea,1,4)='ELSE'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,4)='FOR '
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+4
          CASE substr(cllinea,1,4)='NEXT'
             ntab:=ntab-4
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,14)='DEFINE WINDOW '
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3

          CASE substr(cllinea,1,15)='DEFINE SPLITBOX'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,10)='END WINDOW'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,12)='END SPLITBOX'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,16)='DEFINE STATUSBAR'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,13)='END STATUSBAR'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF

          CASE substr(cllinea,1,16)='DEFINE MAIN MENU'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,8)='END MENU'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF

          CASE substr(cllinea,1,5)='POPUP'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,9)='END POPUP'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF

          CASE substr(cllinea,1,11)='DEFINE TREE'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,8)='END TREE'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,14)='DEFINE TOOLBAR'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,11)='END TOOLBAR'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,20)='DEFINE DROPDOWN MENU'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,14)='DEFINE CONTEXT'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,12)='DEFINE LABEL'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,9)='END LABEL'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,14)='DEFINE TEXTBOX'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,11)='END TEXTBOX'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,14)='DEFINE EDITBOX'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,11)='END EDITBOX'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,13)='DEFINE BUTTON'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,10)='END BUTTON'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,15)='DEFINE CHECKBOX'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,12)='END CHECKBOX'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,14)='DEFINE LISTBOX'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,11)='END LISTBOX'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,15)='DEFINE COMBOBOX'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,12)='END COMBOBOX'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,18)='DEFINE CHECKBUTTON'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,15)='END CHECKBUTTON'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,11)='DEFINE GRID'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,8)='END GRID'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,13)='DEFINE SLIDER'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,10)='END SLIDER'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,14)='DEFINE SPINNER'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,11)='END SPINNER'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,12)='DEFINE IMAGE'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,9)='END IMAGE'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,17)='DEFINE DATEPICKER'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,14)='END DATEPICKER'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,13)='DEFINE BROWSE'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,10)='END BROWSE'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,17)='DEFINE RADIOGROUP'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,14)='END RADIOGROUP'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,12)='DEFINE FRAME'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,9)='END FRAME'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,10)='DEFINE TAB'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,7)='END TAB'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,17)='DEFINE ANIMATEBOX'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,14)='END ANIMATEBOX'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             //    CASE substr(cllinea,1,5)='PAGE '
             //         coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             //         ntab:=ntab+3
             //    CASE substr(cllinea,1,8)='END PAGE'
             //         ntab:=ntab-3
             //         coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,16)='DEFINE HYPERLINK'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,13)='END HYPERLINK'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,20)='DEFINE MONTHCALENDAR'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,17)='END MONTHCALENDAR'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,18)='DEFINE PROGRESSBAR'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,15)='END PROGRESSBAR'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,18)='DEFINE RICHEDITBOX'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,15)='END RICHEDITBOX'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,13)='DEFINE PLAYER'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,10)='END PLAYER'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          CASE substr(cllinea,1,16)='DEFINE IPADDRESS'
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
             ntab:=ntab+3
          CASE substr(cllinea,1,13)='END IPADDRESS'
             ntab:=ntab-3
             coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
          otherwise
             IF len(clinea) > 0
                IF swc=0
                   coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
                ELSE
                   coutput:=coutput+replicate(' ',ntab)+clinea1+CRLF
                ENDIF
                lcero:=0
             ELSE
                lcero++
                IF lcero < 10
                   coutput:=coutput+replicate(' ',ntab)+clinea+CRLF
                ENDIF
             ENDIF
       ENDCASE
   NEXT i
   ::Form_Edit:edit_1:Value := cOutput
   ::Form_Wait:Hide()
   ::Form_Edit:edit_1:SetFocus()
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD GoLine() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL i, nCount, nPos, nLine, cText

   nCount := MLCount( ::Form_Edit:edit_1:Value )
   nPos   := 0
   nLine  := Val( InputBox( i18n( 'Go to line:' ), i18n( 'Question' ) ) )
   IF nLine > nCount
      nLine := nCount
   ENDIF
   cText := ::Form_Edit:edit_1:Value
   ::Form_Edit:edit_1:SetFocus()
   FOR i := 1 TO nCount
       nPos := nPos + Len( RTrim( ( MemoLine( cText, 500, i ) ) ) )
       IF i == nLine
          ::Form_Edit:edit_1:SetFocus()
          ::Form_Edit:edit_1:CaretPos := nPos + ( i * 2 ) - i + 1 - 2 - Len( Trim( ( MemoLine( cText, 500, i ) ) ) )
          EXIT
       ENDIF
   NEXT i
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD LookChanges() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   IF ::Form_Edit:edit_1:CaretPos # ::nCaretPos
      ::PosXY()
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD PosXY() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL i, texto, long
LOCAL nCP := ::Form_Edit:edit_1:CaretPos, npos := 0, nposx := 0, nposy
   texto:=::Form_Edit:edit_1:value
   long:=mlcount(texto)
   ::nCaretPos := nCP
   nposy:=0
   for i:=1 to long
       npos:=npos+len(rtrim(( memoline(texto,500,i)   )))
       IF npos > ( nCP -(i-1) )
          nposx:=len((rtrim((memoline(texto,500,i)))))-(npos-(nCP-(i-1)))+1
          nposy:=i
          IF nposx=0
             nposy --
             nposx:=len((rtrim((memoline(texto,500,nposy)))))+1
          ENDIF
          exit
       ENDIF
    NEXT i
    ::Form_Edit:StatusBar:Item(1, ' Lin' + padr( str( nposy, 4), 4) + ' Col' + PADR( str( nposx, 4), 4) + ' Car' + padr( str( nCP, 4), 4) )
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD TxtSearch() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   ::nPosText := 0
   ::cText := AllTrim( InputBox( i18n( 'Text' ), i18n( 'Search' ) ) )
   IF Len( ::cText ) > 0
      ::NextSearch()
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD NextSearch() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL texto
   texto := StrTran( ::Form_Edit:edit_1:value, CR, "" )
   ::nPosText := myAt( Upper( ::cText ), Upper( texto ), ::nPosText + Len( ::cText) )
   IF ::nPosText > 0
      ::Form_Edit:edit_1:setfocus()
      ::Form_Edit:edit_1:CaretPos := ::nPosText-1
   ELSE
      ::Form_Edit:edit_1:SetFocus()
      MsgInfo( i18n( 'No more matches found.' ), 'OOHG IDE+' )
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION myAt( cBusca, cTexto, nInicio )
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL i,nposluna
nposluna:=0
for i:= ninicio to len(cTexto)
    IF upper(substr(cTexto,i,len(cbusca)))=upper(cbusca)
       nposluna:=i
       exit
    ENDIF
NEXT i
RETURN nposluna

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD SaveAndExit() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   IF ! ::lSave
      IF MsgYesNo( i18n( 'File not saved, save it now?' ), 'OOHG IDE+' )
         ::SaveFile()
      ENDIF
   ENDIF
   ::Form_Edit:Release()
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD DatabaseView() CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL curfol, curdrv, cFile, nPos, i, j

   curfol := CurDir()
   curdrv := CurDrive() + ':\'
   cFile := GetFile( { { i18n( 'dbf files *.dbf' ), '*.dbf' } }, 'OOHG IDE+ - ' + i18n( 'Open DBF file'), NIL, .F., .F. )
   IF Len( cFile ) > 0
      nPos := at( ".", cFile )
      cFile := Left( cFile, nPos - 1 )
      j := 0
      FOR i := 1 TO Len( cFile )
          IF SubStr( cFile, i, 1 ) == '\'
             j := i
          ENDIF
      NEXT i
      cFile := SubStr( cFile, j + 1, Len( cFile ) )
      USE ( cFile ) NEW
      SET INTERACTIVECLOSE ON
      EDIT EXTENDED WORKAREA ( cFile ) TITLE i18n( 'Browsing of ... ' ) + cFile
      SET INTERACTIVECLOSE OFF
      ( cFile )->( dbCloseArea() )
   ENDIF
   DirChange( curdrv + curfol )
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD myInputWindow( cTitle, aLabels, aValues, aFormats, aStat0, aStat1, aStat2 ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL l, aResult, i, _iw, ControlRow, cLblName, cCtrlName, oWin, lChange := .F.

   DEFAULT aStat0 TO " "

   SET INTERACTIVECLOSE ON
   l := Len( aLabels )
   aResult := Array( l )

   DEFINE WINDOW _inputwindow OBJ _iw ;
      TITLE cTitle ;
      WIDTH Int( 720 * ::nDPIw ) ;
      HEIGHT Int( 720 * ::nDPIw ) ;
      MODAL ;
      NOSIZE ;
      ICON "IDE_EDIT" ;
      BACKCOLOR ::aSystemColor ;
      ON INTERACTIVECLOSE iif( lChange, MsgYesNo( i18n( "Close without saving?" ), "OOHG IDE+" ), .T. )

      DEFINE STATUSBAR
         STATUSITEM aStat0
         IF HB_ISARRAY( aStat1 )
         STATUSITEM aStat1[1]                   WIDTH Int( 115 * ::nDPIw ) ACTION Eval( aStat1[2] )                             TOOLTIP aStat1[3]
         ENDIF
         IF HB_ISARRAY( aStat2 )
         STATUSITEM aStat2[1]                   WIDTH Int( 115 * ::nDPIw ) ACTION Eval( aStat2[2] )                             TOOLTIP aStat2[3] FLAT
         ENDIF
         STATUSITEM i18n( "Ok              ." ) WIDTH Int( 115 * ::nDPIw ) ACTION _myInputWindowOk( _iw, aResult, oWin )        TOOLTIP i18n( "Save changes." )
         STATUSITEM i18n( "Cancel          ." ) WIDTH Int( 115 * ::nDPIw ) ACTION _myInputWindowCancel( _iw, aResult, lChange ) TOOLTIP i18n( "Discard changes." ) RAISED
      END STATUSBAR

      DEFINE WINDOW Int_1 OBJ oWin ;
         AT 0, 0 ;
         INTERNAL ;
         WIDTH _iw:ClientWidth ;
         HEIGHT _iw:ClientHeight ;
         VIRTUAL HEIGHT _iw:ClientHeight ;
         BACKCOLOR ::aSystemColor

         ControlRow := Int( 10 * ::nDPIh )

         FOR i := 1 TO l
            cLblName  := "Label_" + AllTrim(Str( i ))
            cCtrlName := "Control_" + AllTrim(Str( i ))

            @ ControlRow + Int( 3 * ::nDPIh ), 10 LABEL &cLblName VALUE aLabels[i] AUTOSIZE

            DO CASE
            CASE ValType( aValues[i] ) == "L"
               @ ControlRow, Int( 180 * ::nDPIw ) CHECKBOX &cCtrlName CAPTION "" VALUE aValues[i] ON CHANGE lChange := .T. NOFOCUSRECT
               ControlRow := ControlRow + Int( 30 * ::nDPIh )
            CASE ValType( aValues[i] ) == "D"
               @ ControlRow, Int( 180 * ::nDPIw ) DATEPICKER &cCtrlName VALUE aValues[i] WIDTH Int( 420 * ::nDPIw ) ON CHANGE lChange := .T.
               ControlRow := ControlRow + Int( 26 * ::nDPIh )
            CASE ValType( aValues[i] ) == "N"
               IF ValType( aFormats[i] ) == "A"
                  @ ControlRow, Int( 180 * ::nDPIw ) COMBOBOX &cCtrlName ITEMS aFormats[i] VALUE aValues[i] WIDTH Int( 420 * ::nDPIw ) FONT "Courier new" SIZE 9 ON CHANGE lChange := .T.
                  ControlRow := ControlRow + Int( 26 * ::nDPIh )
               ELSEIF  ValType( aFormats[i] ) == "C"
                  IF AT ( ".", aFormats[i] ) > 0
                     @ ControlRow, Int( 180 * ::nDPIw ) TEXTBOX &cCtrlName VALUE aValues[i] WIDTH Int( 120 * ::nDPIw ) HEIGHT Int( 24 * ::nDPIw ) FONT "Courier new" SIZE 9 NUMERIC INPUTMASK aFormats[i] RIGHTALIGN ON CHANGE lChange := .T.
                     ControlRow := ControlRow + Int( 26 * ::nDPIh )
                  ELSE
                     @ ControlRow, Int( 180 * ::nDPIw ) TEXTBOX &cCtrlName VALUE aValues[i] WIDTH Int( 120 * ::nDPIw ) HEIGHT Int( 24 * ::nDPIw ) FONT "Courier new" SIZE 9 NUMERIC INPUTMASK aFormats[i] RIGHTALIGN ON CHANGE lChange := .T.
                     ControlRow := ControlRow + Int( 26 * ::nDPIh )
                  ENDIF
               ELSE
                  ControlRow := ControlRow + Int( 26 * ::nDPIh )
               ENDIF
            CASE ValType( aValues[i] ) == "C"
               IF ValType( aFormats[i] ) == "N"
                  IF  aFormats[i] <= 32
                     @ ControlRow, Int( 180 * ::nDPIw ) TEXTBOX &cCtrlName VALUE aValues[i] WIDTH Int( 270 * ::nDPIw ) HEIGHT Int( 24 * ::nDPIw ) FONT "Courier new" SIZE 9 MAXLENGTH aFormats[i] ON CHANGE lChange := .T.
                     ControlRow := ControlRow + Int( 26 * ::nDPIh )
                  ELSE
                     @ ControlRow, Int( 180 * ::nDPIw ) EDITBOX &cCtrlName WIDTH Int( 420 * ::nDPIw ) HEIGHT Int( 40 * ::nDPIh ) VALUE aValues[i] FONT "Courier new" SIZE 9 MAXLENGTH aFormats[i] NOVSCROLL ON CHANGE lChange := .T.
                     ControlRow := ControlRow + Int( 42 * ::nDPIh )
                  ENDIF
               ELSEIF ValType( aFormats[i] ) == "C" .AND. aFormats[i] == "M"
                  @ ControlRow, Int( 180 * ::nDPIw ) EDITBOX &cCtrlName WIDTH Int( 420 * ::nDPIw ) HEIGHT Int( 90 * ::nDPIh ) VALUE aValues[i] FONT "Courier new" SIZE 9 ON CHANGE lChange := .T.
                  ControlRow := ControlRow + Int( 92 * ::nDPIh )
               ELSEIF aFormats[i] == NIL
                  @ ControlRow, Int( 180 * ::nDPIw ) LABEL &cCtrlName VALUE aValues[i] FONT "Courier new" SIZE 9
                  ControlRow := ControlRow + Int( 26 * ::nDPIh )
               ELSE
                  ControlRow := ControlRow + Int( 26 * ::nDPIh )
               ENDIF
            CASE ValType( aValues[i] ) == "M"
               @ ControlRow, Int( 180 * ::nDPIw ) EDITBOX &cCtrlName WIDTH Int( 420 * ::nDPIw ) HEIGHT Int( 90 * ::nDPIh ) VALUE aValues[i] FONT "Courier new" SIZE 9 ON CHANGE lChange := .T.
               ControlRow := ControlRow + Int( 92 * ::nDPIh )
            OTHERWISE
               ControlRow := ControlRow + Int( 26 * ::nDPIh )
            ENDCASE
         NEXT i

      END WINDOW

      oWin:VScrollBar:nLineSkip := ::nLineSkip

      ON KEY ESCAPE OF _inputwindow ACTION _myInputWindowCancel( _iw, aResult, lChange )
   END WINDOW

   oWin:ClientHeight := ControlRow

   _iw:Width := Int( 720 * ::nDPIw )
   _iw:Height := Int( Min( GetDesktopRealHeight() - ::MainHeight, ControlRow - _iw:StatusBar:ClientHeightUsed() + GetTitleHeight() + GetBorderHeight() * 2 ) )

   oWin:Width := _iw:ClientWidth
   oWin:Height := _iw:ClientHeight + _iw:StatusBar:ClientHeightUsed()
   oWin:VirtualHeight := ControlRow

   CENTER WINDOW _InputWindow
   ACTIVATE WINDOW _InputWindow

   SET INTERACTIVECLOSE OFF
RETURN aResult

/*--------------------------------------------------------------------------------------------------------------------------------*/
STATIC FUNCTION _myInputWindowOk( oInputWindow, aResult, oWin )
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL i, l, o

   l := Len( aResult )
   FOR i := 1 TO l
      o := oWin:Control( "Control_" + AllTrim( Str( i ) ) )
      IF HB_ISOBJECT( o )
         aResult[ i ] := o:Value
      ENDIF
   NEXT i
   oInputWindow:Release()
RETURN .T.

/*--------------------------------------------------------------------------------------------------------------------------------*/
STATIC FUNCTION _myInputWindowCancel( oInputWindow, aResult, lChange )
/*--------------------------------------------------------------------------------------------------------------------------------*/
   IF ! lChange .OR. MsgYesNo( i18n( "Close without saving?" ), "OOHG IDE+" )
      aFill( aResult, NIL )
      oInputWindow:Release()
   ENDIF
RETURN .T.

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION DelExt( cFileName )
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL nAt, cBase

   nAt := RAt( ".", cFileName )
   IF nAt > 0
      cBase := Left( cFileName, nAt - 1 )
   ELSE
      cBase := cFileName
   ENDIF
RETURN cBase

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION DelPath( cFileName )
/*--------------------------------------------------------------------------------------------------------------------------------*/
RETURN SubStr( cFileName, RAt( '\', cFileName ) + 1 )

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION AddSlash(cInFolder)
/*--------------------------------------------------------------------------------------------------------------------------------*/
  LOCAL cOutFolder := AllTrim(cInFolder)

  IF RIGHT(cOutfolder, 1) != '\'
    cOutFolder += '\'
  ENDIF
RETURN cOutFolder

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION DelSlash( cInFolder )
/*--------------------------------------------------------------------------------------------------------------------------------*/
  LOCAL cOutFolder := AllTrim( cInFolder )

  IF Right( cOutfolder, 1 ) == '\'
     cOutFolder := Left( cOutFolder, Len( cOutFolder ) - 1 )
  ENDIF
RETURN cOutFolder

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION OnlyFolder( cFile )
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL nPos, cFolder := ""

   IF Len( cFile ) > 0
      nPos := RAt( '\', cFile )
      IF nPos > 1
         cFolder := Left( cFile, nPos - 1 )
      ENDIF
   ENDIF
RETURN cFolder

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION IsFileInPath( cFileName )
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL cDir, cName, cExt

   hb_FNameSplit( cFileName, @cDir, @cName, @cExt )

   For Each cDir In hb_ATokens( GetEnv( "PATH" ), hb_osPathListSeparator(), .T., .T. )
      IF Left( cDir, 1 ) == '"' .AND. Right( cDir, 1 ) == '"'
         cDir := SubStr( cDir, 2, Len( cDir ) - 2 )
      ENDIF
      IF ! Empty( cDir )
         IF ! Right( cDir, 1 ) == "\"
            cDir += "\"
         ENDIF
         IF File( cDir + cFileName )
            RETURN .T.
         ENDIF
      ENDIF
   NEXT
RETURN .F.

/*--------------------------------------------------------------------------------------------------------------------------------*/
FUNCTION Help_F1( c_p, myIde )
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL wr

   // TODO: check and translate

   DO CASE
   CASE c_p == 'PROJECT'
      wr := CRLF
      wr := wr + "CREATING PROJECTS" + CRLF
      wr := wr + "Select 'New Project' from menu File, and you'll have a new project" + CRLF
      wr := wr + "with basic elements and a main PRG." + CRLF + CRLF

      wr := wr + "CHANGING PREFERENCES" + CRLF
      wr := wr + "Select 'Preferences' from menu File." + CRLF
      wr := wr + "You can change Project, xBase compiler, C compiler, OOHG, Build Type, " + CRLF
      wr := wr + "Make Tool and IDE+ related options." + CRLF + CRLF

      wr := wr + "SAVING PROJECTS" + CRLF
      wr := wr + "Select 'Save Project' from File menu." + CRLF + CRLF

      wr := wr + "ADDING ITEMS TO PROJECT TREE" + CRLF
      wr := wr + "Add form, prg, ch, rpt and rc modules." + CRLF
      wr := wr + "Select 'ADD ...' toolbar button or it's dropdown menu options." + CRLF + CRLF

      wr := wr + "MODIFY ITEMS" + CRLF
      wr := wr + "Simply double click over the item to modify it, or" + CRLF
      wr := wr + "select 'Modify item' toolbar button after selecting the item." + CRLF + CRLF

      wr := wr + "REMOVE ITEMS." + CRLF
      wr := wr + "Select 'Remove Item' toolbar button." + CRLF + CRLF

      wr := wr + "VIEW and PRINT ITEMS" + CRLF
      wr := wr + "Select 'View/Print Item' toolbar button." + CRLF + CRLF

      wr := wr + "BUILDING, RUNING and DEBUGGING" + CRLF
      wr := wr + "Select 'Build Project' toolbar button to build." + CRLF
      wr := wr + "Select 'Build and Run Project' toolbar button to build and run." + CRLF
      wr := wr + "Select 'Run Project' toolbar button to run." + CRLF
      wr := wr + "Select 'Debug Project' from 'Run Project' button's dropdown menu options." + CRLF + CRLF

      wr := wr + "SEARCHING TEXT" + CRLF
      wr := wr + "Select 'Global Search Text' toolbar button to search text all across the project." + CRLF + CRLF

      wr := wr + "QUICK BROWSING" + CRLF
      wr := wr + "Select 'Quick Browse' toolbar button to quick browse a DBF file." + CRLF + CRLF

      wr := wr + "DATA MANAGEMENT" + CRLF
      wr := wr + "Select 'Data Manager' toolbar button to create a new DBF file or to edit," + CRLF
      wr := wr + "change the structure, browse or zap and existing one." + CRLF + CRLF

   CASE c_p == 'FORMEDIT'
      wr := CRLF
      wr := wr + "EDITING FORMS AND CONTROLS" + CRLF + CRLF
      wr := wr + "FORM OPTIONS" + CRLF + CRLF
      wr := wr + "Menu Builder." + CRLF
      wr := wr + "Select dropedownmenu in form toolbar Menus button the apropiate option" + CRLF
      wr := wr + "Can build MAIN, CONTEXT or NOTIFY menus." + CRLF + CRLF
      wr := wr + "Form Properties." + CRLF
      wr := wr + "Select toolbar Properties button." + CRLF + CRLF
      wr := wr + "Form Events." + CRLF
      wr := wr + "Select toolbar Events button." + CRLF + CRLF
      wr := wr + "Form Font/Color and backcolor" + CRLF
      wr := wr + "Select toolbar Font/Color button." + CRLF + CRLF
      wr := wr + "Control Order (tab order)." + CRLF
      wr := wr + "Select toolbar Order button and move controls Up/down." + CRLF + CRLF
      wr := wr + "Toolbar builder." + CRLF
      wr := wr + "Select toolbar 'Toolbar' button." + CRLF + CRLF
      wr := wr + "Statusbar Builder." + CRLF
      wr := wr + "In order to use statusbar must be ON." + CRLF + CRLF + CRLF

      wr := wr + "CONTROL OPTIONS." + CRLF + CRLF
      wr := wr + "Adding controls." + CRLF
      wr := wr + "Select control type on left toolbar with mouse, and click over design form." + CRLF + CRLF

      wr := wr + "Change control properties in 2 ways." + CRLF
      wr := wr + "1) Selecting control with mouse, and push the toolbar control  button Properties" + CRLF
      wr := wr + "2) Selecting control with mouse, and select properties on context menu" + CRLF + CRLF

      wr := wr + "Change control events in 2 ways." + CRLF
      wr := wr + "1) Selecting control with mouse, and push the toolbar control  button Events" + CRLF
      wr := wr + "2) Selecting control with mouse, and select Events on context menu" + CRLF + CRLF

      wr := wr + "Change Font/Color and Backcolor in 2 ways." + CRLF
      wr := wr + "1) Selecting control with mouse, and push the toolbar control button Font/Colors" + CRLF
      wr := wr + "2) Selecting control with mouse, and select Font/Color on context menu" + CRLF + CRLF

      wr := wr + "Move controls in 4 ways." + CRLF
      wr := wr + "1) Selecting control with mouse, and push the toolbar button Interactive move" + CRLF
      wr := wr + "2) Selecting control with mouse, and select Interactive move on context menu" + CRLF
      wr := wr + "3) Selecting control with mouse, and push the toolbar button Manual move/size" + CRLF
      wr := wr + "4) Drag upper left corner" + CRLF
      wr := wr + "When use 1,2 or 3 option can move control with mouse or keyboard" + CRLF + CRLF

      wr := wr + "Resize controls in 4 ways." + CRLF
      wr := wr + "1) Selecting control with mouse, and push the toolbar button Interactive size" + CRLF
      wr := wr + "2) Selecting control with mouse, and select Interactive size on context menu" + CRLF
      wr := wr + "3) Selecting control with mouse, and push the toolbar button Manual move/size" + CRLF
      wr := wr + "4) Drag lower right corner" + CRLF
      wr := wr + "When use 1,2 or 3 option can resize control with mouse or keyboard" + CRLF + CRLF

      wr := wr + "Delete controls in 3 ways." + CRLF
      wr := wr + "1) Selecting control with mouse, and push the toolbar button delete" + CRLF
      wr := wr + "2) Selecting control with mouse, and select delete on context menu" + CRLF
      wr := wr + "3) Selecting control with mouse, and press the delete key" + CRLF + CRLF
   ENDCASE

   SET INTERACTIVECLOSE ON
   DEFINE WINDOW FAyuda ;
      AT 10, 10 ;
      WIDTH 620 HEIGHT 460 ;
      TITLE 'Help' ;
      ICON 'IDE_EDIT' ;
      MODAL ;
      BACKCOLOR myIde:aSystemColor

      ON KEY ESCAPE OF FAyuda ACTION FAyuda.Release()

      @ 0, 0 EDITBOX EDIT_1 ;
         WIDTH 613 ;
         HEIGHT 435 ;
         VALUE wr ;
         READONLY

   END WINDOW

   CENTER WINDOW FAyuda
   ACTIVATE WINDOW FAyuda
   SET INTERACTIVECLOSE OFF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Report_Edit( cFileRep ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL nCount, i
LOCAL cTitle := ''
LOCAL cHeaders := '{},{}'
LOCAL cFields := '{}'
LOCAL cWidths := '{}'
LOCAL cTotals := ''
LOCAL cFormats := ''
LOCAL cAlias := ''
LOCAL cLPP := '50'
LOCAL cCPL := '80'
LOCAL cLMargin := '0'
LOCAL cPaperSize := 'DMPAPER_LETTER'
LOCAL lNoProp := .F.
LOCAL lDos := .F.
LOCAL lPreview := .F.
LOCAL lSelect := .F.
LOCAL cGraphic := "'     ' at 0,0 to 0,0"
LOCAL lMul := .F.
LOCAL cGrpBy := ''
LOCAL cHdrGrp := ''
LOCAL lLandscape := .F.
LOCAL lGroupEject := .F.
LOCAL cHeading := ''
LOCAL cRes := ''
LOCAL cBin := ''
LOCAL cDuplex := ''
LOCAL lCollate := .F.
LOCAL cCopies := ''
LOCAL lColor := .F.
LOCAL cScale := ''
LOCAL cLength := ''
LOCAL cWidth := ''
LOCAL cName := ''
LOCAL cTMargin := ''
LOCAL lNoStamp := .F.
LOCAL Output
LOCAL aLabels
LOCAL aInitValues
LOCAL aFormats
LOCAL aResults
LOCAL cReport
LOCAL cLine

   ::aLineR := {}
   IF File( cFileRep )
      cReport := MemoRead( cFileRep )
      cReport := StrTran( cReport, HTAB, " " )
      nCount := MLCount( cReport )
      FOR i := 1 TO nCount
         /* The leading and trailing spaces are necessary to properly find key words */
         cLine := AllTrim( MemoLine( cReport, 1200, i ) )
         IF Left( cLine, 7 ) == "DEFINE " .AND. Left( LTrim( SubStr( cLine, 8 ) ), 7 ) == "REPORT "
            cLine := "DO REPORT ;"
         ELSEIF Right( cLine, 1 ) # ";"
            cLine += " ;"
         ELSEIF ! Right( cLine, 2 ) == " ;"
            cLine := Left( cLine, Len( cLine ) - 1 ) + " ;"
         ENDIF
         AAdd( ::aLineR, " " + cLine + " " )
      NEXT i
      cTitle      := ::LeaStrR( 'TITLE', '' )
      cHeaders    := ::LeaStrR( 'HEADERS', '{},{}' )
      cFields     := ::LeaStrR( 'FIELDS', '{}' )
      cWidths     := ::LeaStrR( 'WIDTHS', '{}' )
      cTotals     := ::LeaStrR( 'TOTALS', '{}' )
      cFormats    := ::LeaStrR( 'NFORMATS', '{}' )
      cAlias      := ::LeaStrR( 'WORKAREA', '' )
      cLPP        := ::LeaStrR( 'LPP', '55' )
      cCPL        := ::LeaStrR( 'CPL', '80' )
      cLMargin    := ::LeaStrR( 'LMARGIN', '' )
      cPaperSize  := ::LeaStrR( 'PAPERSIZE', '' )
      lNoProp     := ::LeaLogR( 'NOFIXED', .F. )
      lDos        := ::LeaLogR( 'DOSMODE', .F. )
      lPreview    := ::LeaLogR( 'PREVIEW', .F. )
      lSelect     := ::LeaLogR( 'SELECT', .F. )
      cGraphic    := ::LeaStrR( 'IMAGE', '')
      lMul        := ::LeaLogR( 'MULTIPLE', .F. )
      cGrpBy      := ::LeaStrR( 'GROUPED BY', '' )
      cHdrGrp     := ::LeaStrR( 'HEADRGRP', '' )
      lLandscape  := ::LeaLogR( 'LANDSCAPE', .F. )
      lGroupEject := ::LeaLogR( 'GROUPEJECT', .F. )
      cHeading    := ::LeaStrR( 'HEADING', '' )
      cRes        := ::LeaStrR( 'QUALITY', '' )
      cBin        := ::LeaStrR( 'DEFAULTSOURCE', '' )
      cDuplex     := ::LeaStrR( 'DUPLEX', '' )
      lCollate    := ::LeaLogR( 'COLLATE', .F. )
      cCopies     := ::LeaStrR( 'COPIES', '' )
      lColor      := ::LeaLogR( 'COLOR', .F. )
      cScale      := ::LeaStrR( 'SCALE', '' )
      cLength     := ::LeaStrR( 'PAPERLENGTH', '' )
      cWidth      := ::LeaStrR( 'PAPERWIDTH', '' )
      cName       := ::LeaStrR( 'NAME', '' )
      cTMargin    := ::LeaStrR( 'TMARGIN', '' )
      lNoStamp    := ::LeaLogR( 'NODATETIMESTAMP', .F. )
   ENDIF

   aLabels     := { 'Title', 'Headers', 'Fields', 'Widths ', 'Totals', 'NFormats', 'Workarea', 'LPP', 'CPL', 'Left margin', 'Paper size', 'No fixed', 'Dos mode', 'Preview', 'Select', 'Image / at - to', 'Multiple', 'Grouped by', 'Group header', 'Landscape', 'Group eject', 'Heading', 'Quality', 'Default source', 'Duplex', 'Collate', 'Copies', 'Color', 'Scale', 'Paper Length', 'Paper Width', 'Name', 'Top margin', 'No Stamp' }
   aInitValues := { cTitle,  cHeaders,  cFields,  cWidths,   cTotals,  cFormats,   cAlias,     cLPP,  cCPL,  cLMargin,      cPaperSize,   lNoProp,    lDos,       lPreview,  lSelect,  cGraphic,          lMul,       cGrpBy,       cHdrGrp,        lLandscape,  lGroupEject,   cHeading,  cRes,      cBin,             cDuplex,  lCollate,  cCopies,  lColor,  cScale,  cLength,        cWidth,        cName,  cTMargin,     lNoStamp   }
   aFormats    := { 1000,    1000,      1000,     1000,      1000,     1000,       1000,       1000,  1000,  1000,          1000,         .F.,        .F.,        .T.,       .F.,      1000,              .F.,        1000,         1000,           .F.,         .T.,           1000,      1000,      1000,             1000,     .F.,       1000,     .F.,     1000,    1000,           1000,          1000,   1000,         .F.        }
   aResults    := ::myInputWindow( "Report parameters of " + cFileRep, aLabels, aInitValues, aFormats )
   IF aResults[1] == NIL
      RETURN NIL
   ENDIF

   Output := 'DO REPORT'
   Output += ' ;' + CRLF + "   TITLE " + aResults[01]
   Output += ' ;' + CRLF + "   HEADERS " + aResults[02]
   Output += ' ;' + CRLF + "   FIELDS " + aResults[03]
   Output += ' ;' + CRLF + "   WIDTHS " + aResults[04]
   IF NOTEMPTY( aResults[05] )
      Output += ' ;' + CRLF + "   TOTALS " + aResults[05]
   ENDIF
   IF NOTEMPTY( aResults[06] )
      Output += ' ;' + CRLF + "   NFORMATS " + aResults[06]
   ENDIF
   Output += ' ;' + CRLF + "   WORKAREA " + aResults[07]
   IF NOTZERO( aResults[08] )
      Output += ' ;' + CRLF + "   LPP " + aResults[08]
   ENDIF
   IF NOTZERO( aResults[09] )
      Output += ' ;' + CRLF + "   CPL " + aResults[09]
   ENDIF
   IF NOTEMPTY( aResults[10] )
      Output += ' ;' + CRLF + "   LMARGIN " + aResults[10]
   ENDIF
   IF NOTZERO( aResults[11] )
      Output += ' ;' + CRLF + "   PAPERSIZE " + aResults[11]
   ENDIF
   IF aResults[12]
      Output += ' ;' + CRLF + "   NOFIXED"
   ENDIF
   IF aResults[13]
      Output += ' ;' + CRLF + "   DOSMODE"
   ENDIF
   IF aResults[14]
      Output += ' ;' + CRLF + "   PREVIEW"
   ENDIF
   IF aResults[15]
      Output += ' ;' + CRLF + "   SELECT"
   ENDIF
   IF NOTEMPTY( aResults[16] )
      Output += ' ;' + CRLF + "   IMAGE " + aResults[16]
   ENDIF
   IF aResults[17]
      Output += ' ;' + CRLF + "   MULTIPLE"
   ENDIF
   IF NOTEMPTY( aResults[18] )
      Output += ' ;' + CRLF + "   GROUPED BY " + aResults[18]
   ENDIF
   IF NOTEMPTY( aResults[19] )
      Output += ' ;' + CRLF + "   HEADRGRP " + aResults[19]
   ENDIF
   IF aResults[20]
      Output += ' ;' + CRLF + "   LANDSCAPE"
   ENDIF
   IF aResults[21]
      Output += ' ;' + CRLF + "   GROUPEJECT"
   ENDIF
   IF NOTEMPTY( aResults[22] )
      Output += ' ;' + CRLF + "   HEADING " + aResults[22]
   ENDIF
   IF NOTEMPTY( aResults[23] )
      Output += ' ;' + CRLF + "   QUALITY " + aResults[23]
   ENDIF
   IF NOTEMPTY( aResults[24] )
      Output += ' ;' + CRLF + "   DEFAULTSOURCE " + aResults[24]
   ENDIF
   IF NOTEMPTY( aResults[25] )
      Output += ' ;' + CRLF + "   DUPLEX " + aResults[25]
   ENDIF
   IF aResults[26]
      Output += ' ;' + CRLF + "   COLLATE"
   ENDIF
   IF NOTEMPTY( aResults[27] )
      Output += ' ;' + CRLF + "   COPIES " + aResults[27]
   ENDIF
   IF aResults[28]
      Output += ' ;' + CRLF + "   COLOR"
   ENDIF
   IF NOTEMPTY( aResults[29] )
      Output += ' ;' + CRLF + "   SCALE " + aResults[29]
   ENDIF
   IF NOTEMPTY( aResults[30] )
      Output += ' ;' + CRLF + "   PAPERLENGTH " + aResults[30]
   ENDIF
   IF NOTEMPTY( aResults[31] )
      Output += ' ;' + CRLF + "   PAPERWIDTH " + aResults[31]
   ENDIF
   IF NOTEMPTY( aResults[32] )
      Output += ' ;' + CRLF + "   NAME " + aResults[32]
   ENDIF
   IF NOTEMPTY( aResults[33] )
      Output += ' ;' + CRLF + "   TMARGIN " + aResults[33]
   ENDIF
   IF aResults[34]
      Output += ' ;' + CRLF + "   NODATETIMESTAMP"
   ENDIF
   Output += CRLF + CRLF
   IF hb_MemoWrit( cFileRep, Output )
      MsgInfo( i18n( 'Report saved.' ), 'OOHG IDE+' )
   ELSE
      MsgInfo( i18n( 'Error saving report.' ), 'OOHG IDE+' )
   ENDIF
RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD LeaStrR( cPropmet, cDefault ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL i, sw, cFValue, nPos

   sw := 0
   FOR i := 1 TO Len( ::aLineR )
      IF ! At( " DO REPORT ", Upper( ::aLineR[i] ) ) == 0
         sw := 1
      ELSE
         IF sw == 1
            nPos := At( Upper( cPropmet ) + ' ', Upper( ::aLineR[i] ) )
            IF Empty( ::aLineR[i] )
               RETURN cDefault
            ENDIF
            IF nPos > 0
               cFValue := SubStr( ::aLineR[i], nPos + Len( cPropmet ), Len( ::aLineR[i] ) )
               cFValue := AllTrim( cFValue )
               IF Right( cFValue, 1 ) == ';'
                  cFValue := SubStr( cFValue, 1, Len( cFValue ) - 1 )
               ELSE
                  cFValue := SubStr( cFValue, 1, Len( cFValue ) )
               ENDIF
               RETURN AllTrim( cFValue )
            ENDIF
         ENDIF
      ENDIF
   NEXT i
RETURN cDefault

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD LeaLogR( cPropmet, cDefault ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL i, sw := 0

   FOR i := 1 TO Len( ::aLineR )
      IF At( " DO REPORT ", Upper( ::aLineR[i] ) ) # 0
         sw := 1
      ELSE
         IF sw == 1
            IF At( Upper( cPropmet ) + ' ', Upper( ::aLineR[i] ) ) > 0
               RETURN .T.
            ENDIF
            IF Empty( ::aLineR[i] )
               RETURN cDefault
            ENDIF
         ENDIF
      ENDIF
   NEXT i
RETURN cDefault

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD CleanR( cFValue ) CLASS THMI
/*--------------------------------------------------------------------------------------------------------------------------------*/
   cFValue  :=  StrTran( cFValue, '"', '' )
   cFValue  :=  StrTran( cFValue, "'", "" )
RETURN cFValue

/*--------------------------------------------------------------------------------------------------------------------------------*/
#pragma BEGINDUMP
/*--------------------------------------------------------------------------------------------------------------------------------*/

#include <windows.h>
#include <winuser.h>
#include "hbapi.h"

#define VK1_A 65
#define VK1_C 67
#define VK1_V 86
#define VK1_X 88

/* select all - ctrl-a */
HB_FUNC( SEND_SELECTALL )
{
   keybd_event( (BYTE) VK_CONTROL, (BYTE) MapVirtualKey( (UINT) VK_CONTROL, 0), 0,               0 );
   keybd_event( (BYTE) VK1_A,      (BYTE) MapVirtualKey( (UINT) VK1_A,      0), 0,               0 );
   keybd_event( (BYTE) VK1_A,      (BYTE) MapVirtualKey( (UINT) VK1_A,      0), KEYEVENTF_KEYUP, 0 );
   keybd_event( (BYTE) VK_CONTROL, (BYTE) MapVirtualKey( (UINT) VK_CONTROL, 0), KEYEVENTF_KEYUP, 0 );
}

/* copy - ctrl-c */
HB_FUNC( SEND_COPY )
{
   keybd_event( (BYTE) VK_CONTROL, (BYTE) MapVirtualKey( (UINT) VK_CONTROL, 0), 0,               0 );
   keybd_event( (BYTE) VK1_C,      (BYTE) MapVirtualKey( (UINT) VK1_C,      0), 0,               0 );
   keybd_event( (BYTE) VK1_C,      (BYTE) MapVirtualKey( (UINT) VK1_C,      0), KEYEVENTF_KEYUP, 0 );
   keybd_event( (BYTE) VK_CONTROL, (BYTE) MapVirtualKey( (UINT) VK_CONTROL, 0), KEYEVENTF_KEYUP, 0 );
}

/* paste - ctrl-v */
HB_FUNC( SEND_PASTE )
{
   keybd_event( (BYTE) VK_CONTROL, (BYTE) MapVirtualKey( (UINT) VK_CONTROL, 0), 0,               0 );
   keybd_event( (BYTE) VK1_V,      (BYTE) MapVirtualKey( (UINT) VK1_V,      0), 0,               0 );
   keybd_event( (BYTE) VK1_V,      (BYTE) MapVirtualKey( (UINT) VK1_V,      0), KEYEVENTF_KEYUP, 0 );
   keybd_event( (BYTE) VK_CONTROL, (BYTE) MapVirtualKey( (UINT) VK_CONTROL, 0), KEYEVENTF_KEYUP, 0 );
}

/* cut - ctrl-x */
HB_FUNC( SEND_CUT )
{
   keybd_event( (BYTE) VK_CONTROL, (BYTE) MapVirtualKey( (UINT) VK_CONTROL, 0), 0,               0 );
   keybd_event( (BYTE) VK1_X,      (BYTE) MapVirtualKey( (UINT) VK1_X,      0), 0,               0 );
   keybd_event( (BYTE) VK1_X,      (BYTE) MapVirtualKey( (UINT) VK1_X,      0), KEYEVENTF_KEYUP, 0 );
   keybd_event( (BYTE) VK_CONTROL, (BYTE) MapVirtualKey( (UINT) VK_CONTROL, 0), KEYEVENTF_KEYUP, 0 );
}

HB_FUNC ( ZAPDIRECTORY )
{
   SHFILEOPSTRUCT sh;

   sh.hwnd = GetActiveWindow();
   sh.wFunc = FO_DELETE;
   sh.pFrom = hb_parc( 1 );
   sh.pTo = NULL;
   sh.fFlags = FOF_NOCONFIRMATION | FOF_SILENT;
   sh.hNameMappings = 0;
   sh.lpszProgressTitle = NULL;

   SHFileOperation( &sh );
}

#pragma ENDDUMP


/*--------------------------------------------------------------------------------------------------------------------------------*/
CLASS myTProgressBar FROM TProgressBar
/*--------------------------------------------------------------------------------------------------------------------------------*/
   METHOD Events
ENDCLASS

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Events( hWnd, nMsg, wParam, lParam ) CLASS myTProgressBar
/*--------------------------------------------------------------------------------------------------------------------------------*/
   IF nMsg == WM_LBUTTONUP
      ::DoEventMouseCoords( ::OnClick, "CLICK" )
   ENDIF
RETURN ::Super:Events( hWnd, nMsg, wParam, lParam )


/*--------------------------------------------------------------------------------------------------------------------------------*/
CLASS myTRadioGroup FROM TRadioGroup
/*--------------------------------------------------------------------------------------------------------------------------------*/
   METHOD DoChange
ENDCLASS

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD DoChange() CLASS myTRadioGroup
/*--------------------------------------------------------------------------------------------------------------------------------*/
   _OOHG_EVAL( ::OnRClick )
RETURN ::Super:DoChange()


/*--------------------------------------------------------------------------------------------------------------------------------*/
CLASS myTCombo FROM TCombo
/*--------------------------------------------------------------------------------------------------------------------------------*/
   DATA uDisplayTime              INIT 0

   METHOD Events_Command
ENDCLASS

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Events_Command( wParam  ) CLASS myTCombo
/*--------------------------------------------------------------------------------------------------------------------------------*/
LOCAL Hi_wParam := HIWORD( wParam  )

   IF Hi_wParam == CBN_DROPDOWN
      ::uDisplayTime := hb_MilliSeconds()

   ELSEIF Hi_wParam == CBN_CLOSEUP
      IF hb_MilliSeconds() < ::uDisplayTime + GetDoubleClickTime()
         ::DoEvent( ::OnDblClick, "DBLCLICK" )
      ENDIF
   ENDIF
RETURN ::Super:Events_Command( wParam  )

/*
 * EOF
 */
