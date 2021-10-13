/*
* KODIAK
* (C) 2020 Edilson Mendes Nascimento <edilson.mendes.nascimento@gmail.com>
*/

#include "box.ch"
#include "inkey.ch"
#include "main.ch"

/***
*
*	Console()
*
*	Exibe a janela com as conexoes ativas
*
*/
PROCEDURE Console()

local oWindow
local oBrowse, oColumn
local oScrollBar, oTmpButton
local nKey          := 0
local nMenuItem     := 1
local lSair         := pFALSE

local nTmp
local nPointer
local nLastKeyType  := hb_MilliSeconds()
local nRefresh      := 1000              /* um segundo como defaul */
local aConexoes     := { { "", "" } }
local nRow          := 1


    begin sequence

        // Cria o Objeto Windows
        oWindow := WindowsNew()
        oWindow:Init( ,,,, B_SINGLE + " ", SystemFormColor() )
        oWindow:Top    := INT( SystemMaxRow() / 2 ) - 15
        oWindow:Left   := INT( SystemMaxCol() / 2 ) - 34
        oWindow:Bottom := INT( SystemMaxRow() / 2 ) + 15
        oWindow:Right  := INT( SystemMaxCol() / 2 ) + 34
        oWindow:Border := B_SINGLE + " "
        oWindow:Color  := SystemFormColor()
        oWindow:Header := " Conexoes "
        oWindow:Open()

        // Desenha a Linha de Botoes
        hb_DispBox( oWindow:Bottom- 2, oWindow:Left+ 1, ;
                    oWindow:Bottom- 2, oWindow:Right- 1, B_SINGLE, SystemFormColor() )


        // Exibe o Browse com as Dezenas Selecionadas
        oBrowse               := TBrowseNew( oWindow:Top+ 2, oWindow:Left+ 1,    ;
                                            oWindow:Bottom- 3, oWindow:Right- 1 )
        oBrowse:skipBlock     := {|x,k| ;
                                        k := IIF(ABS(x) >= IIF( x >= 0,                            ;
                                                            LEN( aConexoes ) - nRow, nRow - 1),    ;
                                                IIF(x >= 0, LEN( aConexoes ) - nRow,1 - nRow), x ) ;
                                                , nRow += k, k                                     ;
                                        }
        oBrowse:goTopBlock    := { || nRow := 1 }
        oBrowse:goBottomBlock := { || nRow := LEN( aConexoes ) }
        oBrowse:colorSpec     := SiBrowseColor()
        oBrowse:headSep       := CHR(205)
        oBrowse:colSep        := CHR(179)
        oBrowse:Cargo         := {}

        oColumn            := TBColumnNew( "TESTE1", { || aConexoes[ nRow ][ 1] } )
        oColumn:width      := 2
        oBrowse:addColumn( oColumn )

        oColumn            := TBColumnNew( "TESTE2", { || aConexoes[ nRow ][ 2] } )
        oColumn:width      := 2
        oBrowse:addColumn( oColumn )

        // Realiza a Montagem da Barra de Rolagem
        oScrollBar           := ScrollBar( oWindow:Top+ 3, oWindow:Bottom- 3, oWindow:Right )
        oScrollBar:colorSpec := SiScrollBar()
        oScrollBar:display()


        // Desenha os botoes da tela
        oTmpButton           := PushButton( oWindow:Bottom- 1, oWindow:Left+ 2, " &Marcar " )
        oTmpButton:sBlock    := { || Nil }
        oTmpButton:Style     := ""
        oTmpButton:ColorSpec := SiPushButton()
        AADD( oBrowse:Cargo, { oTmpButton, Upper( SubStr( oTmpButton:Caption, At("&", oTmpButton:Caption )+ 1, 1 ) ) } )

        oTmpButton           := PushButton( oWindow:Bottom- 1, oWindow:Left+11, " &Desmarcar " )
        oTmpButton:sBlock    := { || Nil }
        oTmpButton:Style     := ""
        oTmpButton:ColorSpec := SiPushButton()
        AADD( oBrowse:Cargo, { oTmpButton, Upper( SubStr( oTmpButton:Caption, At("&", oTmpButton:Caption )+ 1, 1 ) ) } )

        oTmpButton           := PushButton( oWindow:Bottom- 1, oWindow:Left+23, " &Sair " )
        oTmpButton:sBlock    := { || lSair := pTRUE }
        oTmpButton:Style     := ""
        oTmpButton:ColorSpec := SiPushButton()
        AADD( oBrowse:Cargo, { oTmpButton, Upper( SubStr( oTmpButton:Caption, At("&", oTmpButton:Caption )+ 1, 1 ) ) } )

        AEval( oBrowse:Cargo, { |xItem| xItem[1]:Display() } )
        oBrowse:Cargo[ nMenuItem ][1]:SetFocus()


        while .not. lSair

            oBrowse:forceStable()

            oScrollBar:current := nRow * ( 100 / LEN( aConexoes ) )
            oScrollBar:update()

            nKey := Inkey( (nRefresh / 1000), INKEY_ALL )

            if oBrowse:stable .and. nKey > 0

                nLastKeyType := hb_MilliSeconds()
                nRefresh     := 1000

                do case
                    case ( nPointer := AScan( pBRW_KEYS, { |x| x[1] == nKey } ) ) > 0
                        Eval( pBRW_KEYS[ nPointer ][2], oBrowse )

                    case nKey == K_TAB
                        oBrowse:Right()

                    case nKey == K_SH_TAB
                        oBrowse:Left()

                    case ( nPointer := AScan( oBrowse:Cargo, { |xKey| xKey[2] == Upper( Chr( nKey ) ) } ) ) > 0
                        if oBrowse:Cargo[ nMenuItem ][1]:HasFocus
                            oBrowse:Cargo[ nMenuItem ][1]:KillFocus()
                        endif
                        nMenuItem := nPointer
                        oBrowse:Cargo[ nMenuItem ][1]:SetFocus()

                    case nKey == K_LEFT
                        if oBrowse:Cargo[ nMenuItem ][1]:HasFocus
                            oBrowse:Cargo[ nMenuItem ][1]:KillFocus()
                        endif
                        if --nMenuItem < 1
                            nMenuItem := 1
                        endif
                        oBrowse:Cargo[ nMenuItem ][1]:SetFocus()

                    case nKey == K_RIGHT
                        if oBrowse:Cargo[ nMenuItem ][1]:HasFocus
                            oBrowse:Cargo[ nMenuItem ][1]:KillFocus()
                        endif
                        if ++nMenuItem > LEN( oBrowse:Cargo )
                            nMenuItem := LEN( oBrowse:Cargo )
                        endif
                        oBrowse:Cargo[ nMenuItem ][1]:SetFocus()

                    case nKey == K_ENTER .or. nKey == K_LBUTTONDOWN
                        oBrowse:Cargo[ nMenuItem ][1]:Select()
                        oBrowse:refreshAll()

                endcase

            else
                nTmp := int( ( ( hb_MilliSeconds() - nLastKeyType ) / 1000 ) / 60 )
                if nTmp > 720
                    nRefresh := 60000 /* um minuto a cada 12 horas */
                elseif nTmp > 60
                    nRefresh := 30000
                elseif nTmp > 15
                    nRefresh := 10000
                elseif nTmp > 1
                    nRefresh := 3000
                elseif nTmp > 0
                    nRefresh := 2000
                endif
            endif

        enddo

    always
        // Fecha o Objeto Windows
        oWindow:Close()
    end sequence

return
