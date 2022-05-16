/*
 * $Id: toolbed.prg $
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


#include "dbstruct.ch"
#include "oohg.ch"
#include "hbclass.ch"

#define UpperNIL( cValue )  iif( Upper( AllTrim( cValue ) ) == "NIL", "NIL", cValue )
#define EDIT_ABORTED        .T.
#define EDIT_SAVED          .F.
#define NOTEMPTY( cValue )  ( ! Empty( cValue ) .AND. UpperNIL( cValue ) != "NIL" )

/*--------------------------------------------------------------------------------------------------------------------------------*/
CLASS TMyToolBarEditor

   DATA aToolBars                 INIT {}
   DATA lChanged                  INIT .F.
   DATA nLast                     INIT 0
   DATA oEditor                   INIT NIL
   DATA oSplitBox                 INIT NIL

   METHOD AddToolBar
   METHOD Count                   BLOCK { |Self| Len( ::aToolBars ) }
   METHOD CreateToolBars
   METHOD DelToolBar
   METHOD Edit
   METHOD EditToolBar
   METHOD FmgOutput
   METHOD LoadToolBars
   METHOD MoveToolBarDown
   METHOD MoveToolBarUp
   METHOD New                     CONSTRUCTOR
   METHOD Release

   ENDCLASS

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD AddToolBar( cName ) CLASS TMyToolBarEditor

   LOCAL i, oTB

   DEFAULT cName TO "toolbar_" + LTrim( Str( ++ ::nLast ) )
   i := 1
   DO WHILE i <= ::Count
      oTB := ::aToolBars[ i ]
      IF Upper( oTB:Name ) == Upper( cName )
         cName := "toolbar_" + LTrim( Str( ++ ::nLast ) )
         i := 1
      ELSE
         i ++
      ENDIF
   ENDDO
   oTB := TMyToolBar():New( cName, ::oEditor )
   AAdd( ::aToolBars, oTB )

   RETURN oTB

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD DelToolBar( i ) CLASS TMyToolBarEditor

   IF i > 0 .AND. i <= ::Count
      ::aToolBars[ i ]:Release()
      ADel( ::aToolBars, i )
      ASize( ::aToolBars, ::Count - 1 )
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Edit() CLASS TMyToolBarEditor

   LOCAL oLst, oDel, oEdit

   SET INTERACTIVECLOSE ON
   LOAD WINDOW myTBSel
   ON KEY ESCAPE OF myTBSel ACTION myTBSel.Release()
   ACTIVATE WINDOW myTBSel
   SET INTERACTIVECLOSE OFF
   ::CreateToolBars()
   ::oEditor:oDesignForm:SetFocus()

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD CreateToolBars() CLASS TMyToolBarEditor

   LOCAL oTB

   IF HB_ISOBJECT( ::oSplitBox )
      FOR EACH oTB IN ::aToolBars
         IF HB_ISOBJECT( oTB:oTBCtrl )
            oTB:oTBCtrl:Release()
         ENDIF
      NEXT
      ::oSplitBox:Release()
   ENDIF
   IF Len( ::aToolBars ) > 1
      DEFINE SPLITBOX OF ( ::oEditor:oDesignForm:Name ) OBJ ::oSplitBox
   ENDIF
   FOR EACH oTB IN ::aToolBars
      oTB:CreateCtrl()
   NEXT
   IF Len( ::aToolBars ) > 1
      END SPLITBOX
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD EditToolBar( i ) CLASS TMyToolBarEditor

   LOCAL lRet

   IF i > 0 .AND. i <= ::Count
      IF ( lRet := ::aToolBars[ i ]:Edit() ) == EDIT_SAVED
         ::lChanged := .T.
      ELSE
         ::aToolBars[ i ]:ReadFromFMG()
      ENDIF
   ELSE
      lRet := EDIT_ABORTED
   ENDIF

   RETURN lRet

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD FmgOutput( nSpacing ) CLASS TMyToolBarEditor

   LOCAL oTB, cOutput := ""

   IF Len( ::aToolBars ) > 1
      cOutput += "DEFINE SPLITBOX" + CRLF
   ENDIF
   FOR EACH oTB IN ::aToolBars
      cOutput += oTB:FmgOutput( nSpacing )
   NEXT
   FOR EACH oTB IN ::aToolBars
      oTB:CreateCtrl()
   NEXT
   IF Len( ::aToolBars ) > 1
      cOutput += "END SPLITBOX" + CRLF + CRLF
   ENDIF

   RETURN cOutput

/*--------------------------------------------------------------------------------------------------------------------------------*/
 METHOD LoadToolBars() CLASS TMyToolBarEditor

   LOCAL i := 1, cName, oTB

   ::aToolBars := {}
   ::lChanged := .F.
   DO WHILE i <= Len( ::oEditor:aLine )
      IF At( "DEFINE TOOLBAR ", Upper( LTrim( ::oEditor:aLine[ i ] ) ) ) == 1
         cName := ::oEditor:ReadCtrlName( i )
         IF ! Empty( cName )
            oTB := ::AddToolBar( cName )
            oTB:ReadFromFMG()
         ENDIF
      ENDIF
      i ++
   ENDDO
   ::CreateToolBars()

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD MoveToolBarDown( i ) CLASS TMyToolBarEditor

   LOCAL oTB

   IF i > 0 .AND. i < ::Count
      oTB := ::aToolBars[ i ]
      ADel( ::aToolBars, i )
      i ++
      AIns( ::aToolBars, i )
      ::aToolBars[ i ] := oTB
      ::lChanged := .T.
   ENDIF

   RETURN i

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD MoveToolBarUp( i ) CLASS TMyToolBarEditor

   LOCAL oTB

   IF i > 1 .AND. i <= ::Count
      oTB := ::aToolBars[ i ]
      ADel( ::aToolBars, i )
      i --
      AIns( ::aToolBars, i )
      ::aToolBars[ i ] := oTB
      ::lChanged := .T.
   ENDIF

   RETURN i

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD New( oEditor ) CLASS TMyToolBarEditor

   ::oEditor := oEditor

   RETURN Self

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Release() CLASS TMyToolBarEditor

   LOCAL oTB

   FOR EACH oTB IN ::aToolBars
      oTB:Release()
   NEXT
   ::aToolBars := {}
   ::oEditor := NIL

   RETURN NIL


/*--------------------------------------------------------------------------------------------------------------------------------*/
CLASS TMyToolBar

   DATA aButtons                  INIT {}
   DATA aData                     INIT {}
   DATA cAction                   INIT ""
   DATA cCaption                  INIT ""
   DATA cFontName                 INIT ""
   DATA cFontNameFrm              INIT ""
   DATA cFontSizeFrm              INIT ""
   DATA cObj                      INIT ""
   DATA cSubClass                 INIT ""
   DATA cToolTip                  INIT ""
   DATA FormEdit                  INIT NIL
   DATA lBold                     INIT .F.
   DATA lBorder                   INIT .T.
   DATA lBottom                   INIT .F.
   DATA lBreak                    INIT .F.
   DATA lEditResult               INIT EDIT_ABORTED
   DATA lFlat                     INIT .F.
   DATA lItalic                   INIT .F.
   DATA lNoBreak                  INIT .F.
   DATA lNoTabStop                INIT .F.
   DATA lOwnTT                    INIT .F.
   DATA lRightText                INIT .F.
   DATA lRTL                      INIT .F.
   DATA lStrikeout                INIT .F.
   DATA lUnderline                INIT .F.
   DATA lVertical                 INIT .F.
   DATA Name                      INIT ""
   DATA nColorB                   INIT 0
   DATA nColorG                   INIT 0
   DATA nColorR                   INIT 0
   DATA nFontSize                 INIT 0
   DATA nHeight                   INIT 65
   DATA nLastBtn                  INIT 0
   DATA nToolBarSize              INIT 0
   DATA nWidth                    INIT 65
   DATA oEditor                   INIT NIL
   DATA oTBCtrl                   INIT NIL

   METHOD AddBtn
   METHOD CreateCtrl
   METHOD DeleteBtn
   METHOD Discard
   METHOD Edit
   METHOD EditDropDownButton
   METHOD FmgOutput
   METHOD InsertBtn
   METHOD MoveDown
   METHOD MoveUp
   METHOD New                     CONSTRUCTOR
   METHOD OnEditInit
   METHOD OnGridChange
   METHOD PreProcessDefine
   METHOD ReadFromFMG
   METHOD ReadToolBarLogicalData
   METHOD ReadToolBarStringData
   METHOD Release
   METHOD Save
   METHOD SetFont
   METHOD WriteAction
   METHOD WriteAutosize
   METHOD WriteCaption
   METHOD WriteCheck
   METHOD WriteGroup
   METHOD WriteName
   METHOD WriteObj
   METHOD WritePicture
   METHOD WriteSeparator
   METHOD WriteSubClass
   METHOD WriteToolTip
   METHOD WriteWhole

   ENDCLASS

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD AddBtn() CLASS TMyToolBar

   LOCAL oBut, nNew

   ::nLastBtn ++
   oBut := TMyTBBtn():New( ::Name + "_button_" + LTrim( Str( ::nLastBtn ) ), ::oEditor )
   AAdd( ::aButtons, oBut )

   nNew := ::FormEdit:grd_Buttons:ItemCount + 1
   ::FormEdit:grd_Buttons:InsertItem( nNew, { oBut:Name, "", "", .F., .F., "", .F., .F., "", "", .F., .F., "" } )
   ::FormEdit:grd_Buttons:Value := nNew
   ::FormEdit:grd_Buttons:SetFocus()

   IF Len( ::aButtons ) > 1
      ::FormEdit:btn_Up:Enabled := .T.
      ::FormEdit:btn_Down:Enabled := .T.
   ELSE
      ::FormEdit:btn_Insert:Enabled := .T.
      ::FormEdit:btn_Delete:Enabled := .T.
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD CreateCtrl() CLASS TMyToolBar

   LOCAL i

   IF HB_ISOBJECT( ::oTBCtrl )
      ::oTBCtrl:Release()
   ENDIF

   ::oTBCtrl := TToolBar():Define( 0, ::oEditor:oDesignForm, 0, 0, ::nWidth, ::nHeight, ;
                                   ::oEditor:StrToValueCtrl( ::cCaption, "C" ), NIL, ::cFontName, ::nFontSize, ;
                                   ::oEditor:StrToValueCtrl( ::cToolTip, "C" ), ::lFlat, ::lBottom, ::lRightText, ;
                                   ::lBreak, ::lBold, ::lItalic, ::lUnderline, ::lStrikeout, ::lBorder, ::lRTL, ;
                                   ::lNoTabStop, ::lVertical, ::lOwnTT, ::nToolBarSize )

   FOR i := 1 TO Len( ::aButtons )
       ::aButtons[ i ]:CreateCtrl()
   NEXT i

   _EndToolBar( ! ::lNoBreak )

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD DeleteBtn() CLASS TMyToolBar

   LOCAL nLen, nBut

   nBut := ::FormEdit:grd_Buttons:Value
   nLen := Len( ::aButtons )
   IF nBut > 0 .AND. nBut <= nLen
      ADel( ::aButtons, nBut )
      ASize( ::aButtons, nLen - 1 )

      ::FormEdit:grd_Buttons:DeleteItem( nBut )
      ::FormEdit:grd_Buttons:Value := Min( nBut, ::FormEdit:grd_Buttons:ItemCount )
      ::FormEdit:grd_Buttons:SetFocus()

      ::OnGridChange()

      IF Len( ::aButtons ) > 0
         IF Len( ::aButtons ) == 1
            ::FormEdit:btn_Up:Enabled := .F.
            ::FormEdit:btn_Down:Enabled := .F.
         ENDIF
      ELSE
         ::FormEdit:btn_Insert:Enabled := .F.
         ::FormEdit:btn_Delete:Enabled := .F.
         ::FormEdit:btn_Up:Enabled     := .F.
         ::FormEdit:btn_Down:Enabled   := .F.
      ENDIF
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Discard() CLASS TMyToolBar

   IF MsgYesNo( i18n( "Changes will be discarded, are you sure?" ), "OOHG IDE+" )
      ::FormEdit:Release()
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Edit() CLASS TMyToolBar

   SET INTERACTIVECLOSE ON
   LOAD WINDOW myToolBarEd
   ON KEY ESCAPE OF myToolBarEd ACTION ::Discard()
   ACTIVATE WINDOW myToolBarEd
   SET INTERACTIVECLOSE OFF
   ::oEditor:oDesignForm:SetFocus()

   RETURN ::lEditResult

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD FmgOutput( nSpacing ) CLASS TMyToolBar

   LOCAL cOutput := "", oBut, oMenu

   cOutput += Space( nSpacing ) + "DEFINE TOOLBAR " + AllTrim( ::Name ) + " ;" + CRLF
   cOutput += Space( nSpacing * 2 ) + "BUTTONSIZE " + LTrim( Str( ::nWidth ) ) + ", " + LTrim( Str( ::nHeight ) )
   cOutput += iif( ::nToolBarSize > 0, " ;" + CRLF + Space( nSpacing * 2 ) + "TOOLBARSIZE " + LTrim( Str( ::nToolBarSize ) ), "" )
   cOutput += iif( NOTEMPTY( ::cObj ), " ;" + CRLF + Space( nSpacing * 2 ) + "OBJ " + AllTrim( ::cObj ), "" )
   cOutput += iif( NOTEMPTY( ::cFontNameFrm ), " ;" + CRLF + Space( nSpacing * 2 ) + "FONT " + AllTrim( ::cFontNameFrm ), "" )
   cOutput += iif( NOTEMPTY( ::cFontSizeFrm ), " ;" + CRLF + Space( nSpacing * 2 ) + "SIZE " + AllTrim( ::cFontSizeFrm ), "" )
   cOutput += iif( ::lBold, " ;" + CRLF + Space( nSpacing * 2 ) + "BOLD ", "" )
   cOutput += iif( ::lItalic, " ;" + CRLF + Space( nSpacing * 2 ) + "ITALIC ", "" )
   cOutput += iif( ::lUnderline, " ;" + CRLF + Space( nSpacing * 2 ) + "UNDERLINE ", "" )
   cOutput += iif( ::lStrikeout, " ;" + CRLF + Space( nSpacing * 2 ) + "STRIKEOUT ", "" )
   cOutput += iif( NOTEMPTY( ::cToolTip ), " ;" + CRLF + Space( nSpacing * 2 ) + "TOOLTIP " + AllTrim( ::cToolTip ), "" )
   cOutput += iif( ::lOwnTT, " ;" + CRLF + Space( nSpacing * 2 ) + "OWNTOOLTIP ", "" )
   cOutput += iif( ::lFlat, " ;" + CRLF + Space( nSpacing * 2 ) + "FLAT ", "" )
   cOutput += iif( ::lBottom, " ;" + CRLF + Space( nSpacing * 2 ) + "BOTTOM ", "" )
   cOutput += iif( ::lRightText, " ;" + CRLF + Space( nSpacing * 2 ) + "RIGHTTEXT ", "" )
   cOutput += iif( ::lBorder, " ;" + CRLF + Space( nSpacing * 2 ) + "BORDER ", "" )
   cOutput += iif( ::lVertical, " ;" + CRLF + Space( nSpacing * 2 ) + "VERTICAL ", "" )
   cOutput += iif( ::lBreak, " ;" + CRLF + Space( nSpacing * 2 ) + "BREAK ", "" )
   cOutput += iif( ::lRTL, " ;" + CRLF + Space( nSpacing * 2 ) + "RTL ", "" )
   cOutput += iif( ::lNoTabStop, " ;" + CRLF + Space( nSpacing * 2 ) + "NOTABSTOP ", "" )
   cOutput += iif( NOTEMPTY( ::cCaption ), " ;" + CRLF + Space( nSpacing * 2 ) + "CAPTION " + AllTrim( ::cCaption ), "" )
   cOutput += iif( NOTEMPTY( ::cAction ), " ;" + CRLF + Space( nSpacing * 2 ) + "ACTION " + AllTrim( ::cAction ), "" )
   cOutput += iif( NOTEMPTY( ::cSubClass ), " ;" + CRLF + Space( nSpacing * 2 ) + "SUBCLASS " + AllTrim( ::cSubClass ), "" )
   cOutput += CRLF + CRLF

   FOR EACH oBut IN ::aButtons
      cOutput += oBut:FmgOutput( nSpacing )
   NEXT

   cOutput += Space( nSpacing ) + "END TOOLBAR "
   cOutput += iif( ::lNoBreak, " ;" + CRLF + Space( nSpacing * 2 ) + "NOBREAK ", "" )
   cOutput += CRLF + CRLF

   /* TODO: DROP DOWN MENU */
   oMenu := TMyMenuEditor()
   oMenu:oEditor := ::oEditor
   oMenu:nType := 4
   FOR EACH oBut IN ::aButtons
      IF File( ::oEditor:cFName + "." + oBut:Name + ".mnd" )
         IF oBut:lDrop .OR. oBut:lWhole
            cOutput += oMenu:FmgOutput( NIL, NIL, nSpacing, oBut:Name )
         ELSE
            ERASE ( ::oEditor:cFName + "." + oBut:Name + ".mnd" )
         ENDIF
      ENDIF
   NEXT

   RETURN cOutput

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD InsertBtn() CLASS TMyToolBar

   LOCAL nBut, nLen, oBut

   nBut := ::FormEdit:grd_Buttons:Value
   nLen := Len( ::aButtons )
   IF nBut > 0 .AND. nBut <= nLen
      ASize( ::aButtons, nLen + 1 )
      AIns( ::aButtons, nBut )
      ::nLastBtn ++

      oBut := TMyTBBtn():New( ::Name + "_button_" + LTrim( Str( ::nLastBtn ) ), ::oEditor )
      ::aButtons[ nBut ] := oBut

      ::FormEdit:grd_Buttons:InsertItem( nBut, { oBut:Name, "", "", .F., .F., "", .F., .F., "", "", .F., .F., "" } )
      ::FormEdit:grd_Buttons:Value := Min( nBut, ::FormEdit:grd_Buttons:ItemCount )
      ::FormEdit:grd_Buttons:SetFocus()

      ::OnGridChange()

      IF Len( ::aButtons ) > 0
         IF Len( ::aButtons ) == 1
            ::FormEdit:btn_Up:Enabled := .F.
            ::FormEdit:btn_Down:Enabled := .F.
         ENDIF
      ELSE
         ::FormEdit:btn_Insert:Enabled := .F.
         ::FormEdit:btn_Delete:Enabled := .F.
      ENDIF
   ENDIF

   ::FormEdit:grd_Buttons:SetFocus()

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD MoveDown() CLASS TMyToolBar

   IF ::FormEdit:grd_Buttons:Value < 1
      IF ::FormEdit:grd_Buttons:ItemCount > 0
         ::FormEdit:grd_Buttons:Value := 1
      ENDIF
   ELSE
      IF ::FormEdit:grd_Buttons:Value < ::FormEdit:grd_Buttons:ItemCount
         ::FormEdit:grd_Buttons:Value := ::FormEdit:grd_Buttons:Value + 1
      ENDIF
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD MoveUp() CLASS TMyToolBar

   IF ::FormEdit:grd_Buttons:Value > 1
      ::FormEdit:grd_Buttons:Value := ::FormEdit:grd_Buttons:Value - 1
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD New( cName, oEditor ) CLASS TMyToolBar

   ::Name    := cName
   ::oEditor := oEditor

   RETURN Self

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD OnEditInit() CLASS TMyToolBar

   LOCAL oBut

   ::FormEdit:txt_Name:Value     := ::Name
   ::FormEdit:txt_Width:Value    := ::nWidth
   ::FormEdit:txt_Height:Value   := ::nHeight
   ::FormEdit:txt_ToolTip:Value  := ::cToolTip
   ::FormEdit:chk_OwnTT:Value    := ::lOwnTT
   ::FormEdit:chk_Flat:Value     := ::lFlat
   ::FormEdit:chk_Bottom:Value   := ::lBottom
   ::FormEdit:chk_Right:Value    := ::lRightText
   ::FormEdit:chk_Border:Value   := ::lBorder
   ::FormEdit:chk_Vert:Value     := ::lVertical
   ::FormEdit:chk_RTL:Value      := ::lRTL
   ::FormEdit:chk_NoTab:Value    := ::lNoTabStop
   ::FormEdit:chk_Break:Value    := ::lBreak
   ::FormEdit:chk_NoBreak:Value  := ::lNoBreak
   ::FormEdit:txt_Obj:Value      := ::cObj
   ::FormEdit:txt_Caption:Value  := ::cCaption
   ::FormEdit:txt_Action:Value   := ::cAction
   ::FormEdit:txt_SubClass:Value := ::cSubClass
   ::FormEdit:txt_TBSize:Value   := ::nToolBarSize

   FOR EACH oBut IN ::aButtons
      WITH OBJECT oBut
         ::FormEdit:grd_Buttons:AddItem( { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   NEXT

   IF Len( ::aButtons ) > 0
      ::FormEdit:grd_Buttons:Value := 1
      IF Len( ::aButtons ) == 1
         ::FormEdit:btn_Up:Enabled := .F.
         ::FormEdit:btn_Down:Enabled := .F.
      ENDIF
   ELSE
      ::FormEdit:btn_Insert:Enabled := .F.
      ::FormEdit:btn_Delete:Enabled := .F.
      ::FormEdit:btn_Up:Enabled     := .F.
      ::FormEdit:btn_Down:Enabled   := .F.
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD OnGridChange() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      ::FormEdit:txt_ItCaption:Value   := oBut:cCaption
      ::FormEdit:txt_ItName:Value      := oBut:Name
      ::FormEdit:txt_ItAction:Value    := oBut:cAction
      ::FormEdit:txt_ItPicture:Value   := oBut:cPicture
      ::FormEdit:txt_ItToolTip:Value   := oBut:cToolTip
      ::FormEdit:txt_ItObj:Value       := oBut:cObj
      ::FormEdit:txt_ItSubClass:Value  := oBut:cSubClass
      ::FormEdit:chk_ItCheck:Value     := oBut:lCheck
      ::FormEdit:chk_ItAutosize:Value  := oBut:lAutosize
      ::FormEdit:chk_ItSeparator:Value := oBut:lSeparator
      ::FormEdit:chk_ItGroup:Value     := oBut:lGroup
      ::FormEdit:chk_ItWhole:Value     := oBut:lWhole
   ELSE
      ::FormEdit:txt_ItCaption:Value   := ""
      ::FormEdit:txt_ItName:Value      := ""
      ::FormEdit:txt_ItAction:Value    := ""
      ::FormEdit:txt_ItPicture:Value   := ""
      ::FormEdit:txt_ItToolTip:Value   := ""
      ::FormEdit:txt_ItObj:Value       := ""
      ::FormEdit:txt_ItSubClass:Value  := ""
      ::FormEdit:chk_ItCheck:Value     := .F.
      ::FormEdit:chk_ItAutosize:Value  := .F.
      ::FormEdit:chk_ItSeparator:Value := .F.
      ::FormEdit:chk_ItGroup:Value     := .F.
      ::FormEdit:chk_ItWhole:Value     := .F.
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD PreProcessDefine() CLASS TMyToolBar

   LOCAL nFrom, nTo, i, cData := "", j, c, k
   LOCAL aTokens0 := { "BOLD", "ITALIC", "UNDERLINE", "STRIKEOUT", "OWNTOOLTIP", "FLAT", "BOTTOM", "RIGHTTEXT", "BORDER", "VERTICAL", "BREAK", "RTL", "NOTABSTOP" }
   LOCAL aTokens1 := { "BUTTONSIZE", "OBJ", "FONT", "SIZE", "TOOLTIP", "CAPTION", "ACTION", "SUBCLASS", "TOOLBARSIZE" }

   /* Concatenate lines */
   IF ( i := AScan( ::oEditor:aControlW, Lower( ::Name ) ) ) > 0
      nFrom := ::oEditor:aSpeed[ i ]
      nTo := ::oEditor:aNumber[ i ]
      FOR i := nFrom TO nTo
         IF Empty( ::oEditor:aLine[ i ] )
            EXIT
         ENDIF
         cData += ::oEditor:aLine[ i ]
      NEXT i
      cData := RTrim( cData )
      /* Convert to tokens */
      ::aData := hb_ATokens( cData, " ", .T., .F. )
      /* Add ; at the end to make parsing easier */
      IF ATail( ::aData ) # ";"
         AAdd( ::aData, ";" )
      ENDIF
      /* This routine locates the toolbar's clauses and concatenates all the
         subsequent tokens until the next clause or a semicolon is found */
      i := 1
      DO WHILE i < Len( ::aData )
         DO CASE
         CASE AScan( aTokens0, ::aData[i] ) > 0
            /* 'logical' properties */
            i ++
            j := i
            DO WHILE ::aData[j] # ";" .AND. ! AScan( aTokens0, ::aData[j] ) > 0 .AND. ! AScan( aTokens1, ::aData[j] ) > 0
               j ++
            ENDDO
            FOR k := j - 1 TO i STEP -1
               ADel( ::aData, k )
               ASize( ::aData, Len( ::aData ) - 1 )
            NEXT
            IF ::aData[i] # ";"
               ASize( ::aData, Len( ::aData ) + 1 )
               AIns( ::aData, i )
               ::aData[i] := ";"
            ENDIF
            i ++
         CASE AScan( aTokens1, ::aData[i] ) > 0
            /* Properties with 1 parameter */
            i ++
            j := i
            c := ""
            DO WHILE ::aData[j] # ";" .AND. ! AScan( aTokens0, ::aData[j] ) > 0 .AND. ! AScan( aTokens1, ::aData[j] ) > 0
               c += ( ::aData[j] + " " )
               j ++
            ENDDO
            c := RTrim( c )
            IF ! Empty( c )
               ::aData[i] := c
               i ++
            ENDIF
            FOR k := j - 1 TO i STEP -1
               ADel( ::aData, k )
               ASize( ::aData, Len( ::aData ) - 1 )
            NEXT
            IF ::aData[i] # ";"
               ASize( ::aData, Len( ::aData ) + 1 )
               AIns( ::aData, i )
               ::aData[i] := ";"
            ENDIF
            i ++
         OTHERWISE
            ADel( ::aData, i )
            ASize( ::aData, Len( ::aData ) - 1 )
         ENDCASE
      ENDDO
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ReadFromFMG() CLASS TMyToolBar

   LOCAL i, cLine, cName, cSize, nPos, nVal, oBut

   IF ( i := AScan( ::oEditor:aControlW, Lower( ::Name ) ) ) > 0
      ::PreProcessDefine()

      ::cObj         := ::ReadToolBarStringData( "OBJ", ::cObj )
      ::cCaption     := ::ReadToolBarStringData( "CAPTION", ::cCaption )
      ::cCaption     := ::ReadToolBarStringData( "GRIPPERTEXT", ::cCaption )
      ::cAction      := ::ReadToolBarStringData( "ACTION", ::cAction )
      ::cFontNameFrm := ::ReadToolBarStringData( "FONT", ::cFontNameFrm )
      ::cFontNameFrm := ::ReadToolBarStringData( "FONTNAME", ::cFontNameFrm )
      ::cFontName    := ::oEditor:StrToValueCtrl( ::cFontNameFrm, "C", "" )
      ::cFontSizeFrm := ::ReadToolBarStringData( "SIZE", ::cFontSizeFrm )
      ::cFontSizeFrm := ::ReadToolBarStringData( "FONTSIZE", ::cFontSizeFrm )
      ::nFontSize    := ::oEditor:StrToValueCtrl( ::cFontSizeFrm, "N", 0, 1, 9999 )
      ::nToolBarSize := ::oEditor:StrToValueCtrl( ::ReadToolBarStringData( "TOOLBARSIZE", "" ), "N", 0, 1, 9999 )
      ::lBold        := ::ReadToolBarLogicalData( "BOLD", ::lBold )
      ::lBold        := ::ReadToolBarLogicalData( "FONTBOLD", ::lBold )
      ::lItalic      := ::ReadToolBarLogicalData( "ITALIC", ::lItalic )
      ::lItalic      := ::ReadToolBarLogicalData( "FONTITALIC", ::lItalic )
      ::lUnderline   := ::ReadToolBarLogicalData( "UNDERLINE", ::lUnderline )
      ::lUnderline   := ::ReadToolBarLogicalData( "FONTUNDERLINE", ::lUnderline )
      ::lStrikeout   := ::ReadToolBarLogicalData( "STRIKEOUT", ::lStrikeout )
      ::lStrikeout   := ::ReadToolBarLogicalData( "FONTSTRIKEOUT", ::lStrikeout )
      ::cToolTip     := ::ReadToolBarStringData( "TOOLTIP", ::cToolTip )
      ::lOwnTT       := ::ReadToolBarLogicalData( "OWNTOOLTIP", ::lFlat )
      ::lFlat        := ::ReadToolBarLogicalData( "FLAT", ::lFlat )
      ::lBottom      := ::ReadToolBarLogicalData( "BOTTOM", ::lBottom )
      ::lVertical    := ::ReadToolBarLogicalData( "VERTICAL", ::lVertical )
      ::lRightText   := ::ReadToolBarLogicalData( "RIGHTTEXT", ::lRightText )
      ::lBorder      := ::ReadToolBarLogicalData( "BORDER", ::lBorder )
      ::lBreak       := ::ReadToolBarLogicalData( "BREAK", ::lBreak )
      ::lRTL         := ::ReadToolBarLogicalData( "RTL", ::lRTL )
      ::cSubClass    := ::ReadToolBarStringData( "SUBCLASS", ::cSubClass )
      ::lNoTabStop   := ::ReadToolBarLogicalData( "NOTABSTOP", ::lNoTabStop )

      cSize := ::ReadToolBarStringData( "BUTTONSIZE", "" )
      IF ( nPos := At( ",", cSize ) ) > 0
         IF ( nVal := Val( SubStr( cSize, 1, nPos - 1 ) ) ) > 0
            ::nWidth := nVal
         ENDIF
         IF ( nVal := Val( SubStr( cSize, nPos + 1 ) ) ) > 0
            ::nHeight := nVal
         ENDIF
      ELSE
         IF ( nVal := Val( cSize ) ) > 0
           ::nWidth := nVal
         ENDIF
      ENDIF
      /*
      aFontColor := UpperNIL( ::ReadToolBarStringData( "FONTCOLOR", "NIL" ) )
      IF "A" == Type( aFontColor )
         ::nColorR := aFontColor[ 1 ]
         ::nColorG := aFontColor[ 2 ]
         ::nColorB := aFontColor[ 3 ]
      ELSEIF Type( aFontColor ) == "N"
         ::nColorR := GetRed( aFontColor )
         ::nColorG := GetGreen( aFontColor )
         ::nColorB := GetBlue( aFontColor )
      ENDIF
      */

      ::aButtons := {}
      DO WHILE i < Len( ::oEditor:aLine )
         i ++
         cLine := Upper( LTrim( ::oEditor:aLine[ i ] ) )
         IF At( "DEFINE TBBUTTON ", cLine ) == 1
            cName := ::oEditor:ReadCtrlName( i )
            IF ! Empty( cName )
               oBut := TMyTBBtn():New( cName, ::oEditor )
               AAdd( ::aButtons, oBut )
               oBut:ReadFromFMG()
            ENDIF
         ELSEIF At( "END TOOLBAR", cLine ) == 1 .OR. At( "END WINDOW", cLine ) == 1
            ::lNoBreak := ( AllTrim( SubStr( cLine, 12 ) ) == "NOBREAK" )
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ReadToolBarLogicalData( cProp, lDefault ) CLASS TMyToolBar

   LOCAL i

   FOR EACH i IN ::aData
      IF Upper( i ) == cProp
         RETURN .T.
      ENDIF
   NEXT i

   RETURN lDefault

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ReadToolBarStringData( cProp, cDefault ) CLASS TMyToolBar

   LOCAL nLen, i, c2, c1

   IF ( i := At( " ", cProp ) ) > 0
      c1 := SubStr( cProp, 1, i - 1 )
      c2 := SubStr( cProp, i + 1 )
      nLen := Len( ::aData ) - 2
      FOR i := 1 TO nLen
         IF Upper( ::aData[ i ] ) == c1
            IF Upper( ::aData[ i + 1 ] ) == c2
               RETURN ::aData[ i + 2 ]
            ENDIF
         ENDIF
      NEXT i
   ELSE
      nLen := Len( ::aData ) - 1
      FOR i := 1 TO nLen
         IF Upper( ::aData[ i ] ) == cProp
            RETURN ::aData[ i + 1 ]
         ENDIF
      NEXT i
   ENDIF

   RETURN cDefault

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Release() CLASS TMyToolBar

   ::aButtons := {}
   ::oEditor := NIL
   IF HB_ISOBJECT( ::oTBCtrl )
      ::oTBCtrl:Release()
      ::oTBCtrl := NIL
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD Save() CLASS TMyToolBar

   DO CASE
   CASE Empty( ::Name )
      MsgStop( i18n( "ToolBar must have a name." ), "OOHG IDE+" )
   CASE ::nWidth <= 0
      MsgStop( i18n( "Width must be greater than 0." ), "OOHG IDE+" )
   CASE ::nHeight <= 0
      MsgStop( i18n( "Height must be greater than 0." ), "OOHG IDE+" )
   OTHERWISE
      ::lEditResult := EDIT_SAVED      
      ::FormEdit:Release()
   ENDCASE

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD SetFont() CLASS TMyToolBar

   LOCAL aFont, cColor

   cColor := "{" + Str( ::nColorR, 3 ) + "," + Str( ::nColorG, 3 ) + "," + Str( ::nColorB, 3 ) + "}"
   aFont := GetFont( ::cFontName, ::nFontSize, ::lBold, ::lItalic, &cColor, ::lUnderline, ::lStrikeout, 0 )
   IF aFont[ 1 ] == ""
      RETURN NIL
   ENDIF
   ::cFontName    := aFont[ 1 ]
   ::cFontNameFrm := StrToStr( aFont[1] )
   ::nFontSize    := aFont[ 2 ]
   ::cFontSizeFrm := LTrim( Str( aFont[2] ) )
   ::lBold        := aFont[ 3 ]
   ::lItalic      := aFont[ 4 ]
   ::nColorR      := aFont[ 5, 1 ]
   ::nColorG      := aFont[ 5, 2 ]
   ::nColorB      := aFont[ 5, 3 ]
   ::lUnderline   := aFont[ 6 ]
   ::lStrikeout   := aFont[ 7 ]

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteAction() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         :cAction := ::FormEdit:txt_ItAction:Value
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteAutosize() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         :lAutosize := ::FormEdit:chk_ItAutosize:Value
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteCaption() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         :cCaption := ::FormEdit:txt_ItCaption:Value
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteCheck() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         :lCheck := ::FormEdit:chk_ItCheck:Value
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteGroup() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         :lGroup := ::FormEdit:chk_ItGroup:Value
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteName() CLASS TMyToolBar

   LOCAL i, oBut, cNewName

   IF ::FormEdit:grd_Buttons:Value > 0
      cNewName := Upper( AllTrim( ::FormEdit:txt_ItName:Value ) )

      FOR i := 1 to Len( ::aButtons )
         IF i # ::FormEdit:grd_Buttons:Value
            oBut := ::aButtons[ i ]
            IF Upper( oBut:Name ) == cNewName
               MsgStop( i18n( "Another button has the same name." ), "OOHG IDE+" )
               RETURN NIL
            ENDIF
         ENDIF
      NEXT i

      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         IF Empty( cNewName )
            MsgStop( i18n( "Button must have a name." ), "OOHG IDE+" )
            ::FormEdit:txt_ItName:Value := :Name
            RETURN NIL
         ENDIF

         :Name := AllTrim( ::FormEdit:txt_ItName:Value )
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteObj() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         :cObj := ::FormEdit:txt_ItObj:Value
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WritePicture() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         :cPicture := ::FormEdit:txt_ItPicture:Value
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteSeparator() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         :lSeparator := ::FormEdit:chk_ItSeparator:Value
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteSubClass() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         :cSubClass := ::FormEdit:txt_ItSubClass:Value
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteToolTip() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         :cToolTip := ::FormEdit:txt_ItToolTip:Value
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD WriteWhole() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         IF :lDrop
            :lWhole := ::FormEdit:chk_ItWhole:Value
            ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
         ELSE
            MsgStop( i18n( "Button has no dropdown menu." ), "OOHG IDE+" )
         ENDIF
      END WITH
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD EditDropDownButton() CLASS TMyToolBar

   LOCAL oBut

   IF ::FormEdit:grd_Buttons:Value > 0
      oBut := ::aButtons[ ::FormEdit:grd_Buttons:Value ]

      WITH OBJECT oBut
         TMyMenuEditor():Edit( ::oEditor, 4, :Name )
         :lDrop := File( ::oEditor:cFName + "." + :Name + ".mnd" )
         IF ! :lDrop
            :lWhole := .F.
         ENDIF
         ::FormEdit:grd_Buttons:Item( ::FormEdit:grd_Buttons:Value, { :Name, :cCaption, :cAction, :lCheck, :lAutosize, :cPicture, :lSeparator, :lGroup, :cTooltip, :cObj, :lDrop, :lWhole, :cSubClass } )
      END WITH
   ENDIF

   RETURN NIL


/*--------------------------------------------------------------------------------------------------------------------------------*/
CLASS TMyTBBtn

   DATA aData                     INIT {}
   DATA cAction                   INIT ""
   DATA cCaption                  INIT ""
   DATA cObj                      INIT ""
   DATA cPicture                  INIT ""
   DATA cToolTip                  INIT ""
   DATA cSubClass                 INIT ""
   DATA lAutosize                 INIT .F.
   DATA lCheck                    INIT .F.
   DATA lDrop                     INIT .F.
   DATA lGroup                    INIT .F.
   DATA lSeparator                INIT .F.
   DATA lWhole                    INIT .F.
   DATA Name                      INIT ""
   DATA oEditor                   INIT NIL

   METHOD CreateCtrl
   METHOD FmgOutput
   METHOD New                     CONSTRUCTOR
   METHOD PreProcessDefine                                                      
   METHOD ReadFromFMG                                                           
   METHOD ReadTBBtnLogicalData                                                  
   METHOD ReadTBBtnStringData                                                   

   ENDCLASS

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD FmgOutput( nSpacing ) CLASS TMyTBBtn

   LOCAL cOutput := ""

   cOutput += Space( nSpacing * 2 ) + "BUTTON " + AllTrim( ::Name )
   cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "CAPTION " + AllTrim( ::cCaption )
   IF NOTEMPTY( ::cPicture )
     cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "PICTURE " + AllTrim( ::cPicture )
   ENDIF
   IF NOTEMPTY( ::cAction )
      cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "ACTION " + AllTrim( ::cAction )
   ENDIF
   IF ::lSeparator
      cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "SEPARATOR "
   ENDIF
   IF ::lAutosize
      cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "AUTOSIZE "
   ENDIF
   IF ::lCheck
      cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "CHECK "
   ENDIF
   IF ::lGroup
      cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "GROUP "
   ENDIF
   IF ::lWhole
      cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "WHOLEDROPDOWN "
   ELSEIF ::lDrop
      cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "DROPDOWN "
   ENDIF
   IF NOTEMPTY( ::cToolTip )
      cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "TOOLTIP " + AllTrim( ::cToolTip )
   ENDIF
   IF NOTEMPTY( ::cObj )
      cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "OBJ " + AllTrim( ::cObj )
   ENDIF
   IF NOTEMPTY( ::cSubClass )
      cOutput += " ;" + CRLF + Space( nSpacing * 3 ) + "SUBCLASS " + AllTrim( ::cSubClass )
   ENDIF
   cOutput += CRLF + CRLF

   RETURN cOutPut

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD New( cName, oEditor ) CLASS TMyTBBtn

   ::Name := cName
   ::oEditor := oEditor

   RETURN Self

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD PreProcessDefine() CLASS TMyTBBtn

   LOCAL nFrom, nTo, i, cData := "", j, c, k
   LOCAL aTokens0 := { "SEPARATOR", "AUTOSIZE", "DROPDOWN", "WHOLEDROPDOWN", "CHECK", "GROUP" }
   LOCAL aTokens1 := { "OBJ", "CAPTION", "PICTURE", "TOOLTIP", "ACTION", "ONCLICK", "SUBCLASS" }

   /* Concatenate lines */
   IF ( i := AScan( ::oEditor:aControlW, Lower( ::Name ) ) ) > 0
      nFrom := ::oEditor:aSpeed[ i ]
      nTo := ::oEditor:aNumber[ i ]
      FOR i := nFrom TO nTo
         IF Empty( ::oEditor:aLine[ i ] )
            EXIT
         ENDIF
         cData += ::oEditor:aLine[ i ]
      NEXT i
      cData := RTrim( cData )
      /* Convert to tokens */
      ::aData := hb_ATokens( cData, " ", .T., .F. )
      /* add ; at the end to make parsing easier */
      IF ATail( ::aData ) # ";"
         AAdd( ::aData, ";" )
      ENDIF
      /* This routine locates the button's clauses and concatenates all the
         subsequent tokens until the next clause or a semicolon is found */
      i := 1
      DO WHILE i < Len( ::aData )
         DO CASE
         CASE AScan( aTokens0, Upper( ::aData[i] ) ) > 0
            /* 'logical' properties */
            i ++
            j := i
            DO WHILE ::aData[j] # ";" .AND. ! AScan( aTokens0, Upper( ::aData[j] ) ) > 0 .AND. ! AScan( aTokens1, Upper( ::aData[j] ) ) > 0
               j ++
            ENDDO
            FOR k := j - 1 TO i STEP -1
               ADel( ::aData, k )
               ASize( ::aData, Len( ::aData ) - 1 )
            NEXT
            IF ::aData[i] # ";"
               ASize( ::aData, Len( ::aData ) + 1 )
               AIns( ::aData, i )
               ::aData[i] := ";"
            ENDIF
            i ++
         CASE AScan( aTokens1, Upper( ::aData[i] ) ) > 0
            /* Properties with 1 parameter */
            i ++
            j := i
            c := ""
            DO WHILE ::aData[j] # ";" .AND. ! AScan( aTokens0, Upper( ::aData[j] ) ) > 0 .AND. ! AScan( aTokens1, Upper( ::aData[j] ) ) > 0
               c += ( ::aData[j] + " " )
               j ++
            ENDDO
            c := RTrim( c )
            IF ! Empty( c )
               ::aData[i] := c
               i ++
            ENDIF
            FOR k := j - 1 TO i STEP -1
               ADel( ::aData, k )
               ASize( ::aData, Len( ::aData ) - 1 )
            NEXT
            IF ::aData[i] # ";"
               ASize( ::aData, Len( ::aData ) + 1 )
               AIns( ::aData, i )
               ::aData[i] := ";"
            ENDIF
            i ++
         CASE ::aData[i] == "ON"
            /* Action */
            i ++
            IF ::aData[i] == "CLICK"
               i ++
               j := i
               c := ""
               DO WHILE ::aData[j] # ";" .AND. ! AScan( aTokens0, Upper( ::aData[j] ) ) > 0 .AND. ! AScan( aTokens1, Upper( ::aData[j] ) ) > 0
                  c += ( AllTrim( ::aData[j] ) + " " )
                  j ++
               ENDDO
               c := RTrim( c )
               IF ! Empty( c )
                  ::aData[i] := c
                  i ++
               ENDIF
            ELSE
               j := i
               DO WHILE ::aData[j] # ";" .AND. ! AScan( aTokens0, Upper( ::aData[j] ) ) > 0 .AND. ! AScan( aTokens1, Upper( ::aData[j] ) ) > 0
                  j ++
               ENDDO
            ENDIF
            FOR k := j - 1 TO i STEP -1
               ADel( ::aData, k )
               ASize( ::aData, Len( ::aData ) - 1 )
            NEXT
            IF ::aData[i] # ";"
               ASize( ::aData, Len( ::aData ) + 1 )
               AIns( ::aData, i )
               ::aData[i] := ";"
            ENDIF
            i ++
         OTHERWISE
            ADel( ::aData, i )
            ASize( ::aData, Len( ::aData ) - 1 )
         ENDCASE
      ENDDO
   ENDIF

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ReadFromFMG() CLASS TMyTBBtn

   ::PreProcessDefine()

   ::cObj       := ::ReadTBBtnStringData( "OBJ", "" )
   ::cCaption   := ::ReadTBBtnStringData( "CAPTION", "" )
   ::cPicture   := ::ReadTBBtnStringData( "PICTURE", "" )
   ::cAction    := ::ReadTBBtnStringData( "ACTION", "" )
   ::cAction    := ::ReadTBBtnStringData( "ON CLICK", ::cAction )
   ::cAction    := ::ReadTBBtnStringData( "ONCLICK", ::cAction )
   ::cToolTip   := ::ReadTBBtnStringData( "TOOLTIP", "" )
   ::lSeparator := ::ReadTBBtnLogicalData( "SEPARATOR", .F. )
   ::lAutoSize  := ::ReadTBBtnLogicalData( "AUTOSIZE", .F. )
   ::lCheck     := ::ReadTBBtnLogicalData( "CHECK", .F. )
   ::lGroup     := ::ReadTBBtnLogicalData( "GROUP", .F. )
   ::lWhole     := ::ReadTBBtnLogicalData( "WHOLEDROPDOWN", .F. )
   ::lDrop      := ::lWhole .OR. ::ReadTBBtnLogicalData( "DROPDOWN", .F. )
   ::cSubClass  := ::ReadTBBtnStringData( "SUBCLASS", "" )

   RETURN NIL

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ReadTBBtnStringData( cProp, cDefault ) CLASS TMyTBBtn

   LOCAL nLen, i, c2, c1

   IF ( i := At( " ", cProp ) ) > 0
      c1 := SubStr( cProp, 1, i - 1 )
      c2 := SubStr( cProp, i + 1 )
      nLen := Len( ::aData ) - 2
      FOR i := 1 TO nLen
         IF Upper( ::aData[ i ] ) == c1
            IF Upper( ::aData[ i + 1 ] ) == c2
               RETURN ::aData[ i + 2 ]
            ENDIF
         ENDIF
      NEXT i
   ELSE
      nLen := Len( ::aData ) - 1
      FOR i := 1 TO nLen
         IF Upper( ::aData[ i ] ) == cProp
            RETURN ::aData[ i + 1 ]
         ENDIF
      NEXT i
   ENDIF

   RETURN cDefault

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD ReadTBBtnLogicalData( cProp, lDefault ) CLASS TMyTBBtn

   LOCAL i

   FOR EACH i IN ::aData
      IF Upper( i ) == cProp
         RETURN .T.
      ENDIF
   NEXT i

   RETURN lDefault

/*--------------------------------------------------------------------------------------------------------------------------------*/
METHOD CreateCtrl() CLASS TMyTBBtn

   LOCAL cPicture, oBut

   IF ! Empty( cPicture := ::oEditor:StrToValueCtrl( ::cPicture, "C", NIL ) )
      IF ! File( cPicture )
         cPicture := "ZZZ_AAAOOHG"
      ENDIF
   ENDIF

   oBut := TToolButton():Define( 0, 0, 0, ::oEditor:StrToValueCtrl( ::cCaption, "C" ), NIL, NIL, NIL, ;
                                 cPicture, ::oEditor:StrToValueCtrl( ::cToolTip, "C" ), NIL, NIL, .F., ;
                                 ::lSeparator, ::lAutosize, ::lCheck, ::lGroup, ::lDrop, ::lWhole )

   IF ::lDrop .OR. ::lWhole
      /* DROPDOWN MENU */
      // TODO: load from FMG file
      TMyMenuEditor():CreateMenuFromFile( ::oEditor, 4, ::Name, oBut )
   ENDIF

   RETURN NIL

/*
 * EOF
 */
