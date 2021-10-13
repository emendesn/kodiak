/*
* KODIAK
* (C) 2020 Edilson Mendes Nascimento <edilson.mendes.nascimento@gmail.com>
*/

#include "inkey.ch"
#include "button.ch"
#include "box.ch"
#include "main.ch"


static aKodiakSystem
static oHosts


********************************

PROCEDURE Main()

local nPorta := 8090
local cModo  := "NONE"

	begin sequence

altd()

		oHosts = WebSocketServer():New( nPorta, cModo, , {|| Com( ) }  )
		oHosts:cErrorLog    := DToS( date() ) + '.log '
		oHosts:lBackground  := pTRUE
		oHosts:cBindAndress := '192.168.1.34'
		oHosts:Activate()

		// Salva a tela Principal
		PUSHScreen()

		// Guarda as dimensoes da tela
		pSCR_ROWTOP       := 0
		pSCR_COLTOP       := 0
		pSCR_ROWBOTTOM    := MAXROW()
		pSCR_COLBOTTOM    := MAXCOL()


		// Define as Cores do Sistema
		If ISCOLOR()
			pCLR_MESSAGE        := "B/W,R/W"                                      && Message no Rodape
			pCLR_BACKGROUND     := "B/W,R/W"                                      && Fundo de tela
			pCLR_PUSHBUTTON     := "W+/R,I/R,I/R,GR+/R"                           && Cores dos Botoes
			pCLR_MENU           := "N/BG,W+/N,GR+/BG,GR+/N,N+/BG,N/BG"            && Menu
			pCLR_FORM           := "W+/B"                                         && Formularios
			pCLR_LABEL          := "W+/B,B/W"                                     && Label de campos
			pCLR_SCROLLBAR      := "W+/B,GR+/B"                                   && Cores da Barra de Rolagem Vertical
			pCLR_FIELDGET       := "B/W,R/W,W+/B,GR+/BG"                          && Campos de Entrada de dados
			pCLR_FIELDLISTBOX   := "GR+/W,B/W,W/GR+,R/W,n+/w,w+/b,w/b,b/w"        && Campos de ListBox
			pCLR_FIELDGRADIOBOX := "W+/B,W+/B,GR+/B"                              && Cores Radio Group
			pCLR_FIELDBRADIOBOX := "W+/B,GR+/B,W+/B,GR+/B,W+/B,GR+/B,GR+/B"       && Campos Rario Button
			pCLR_FIELDCHECKBOX  := "B/W,GR+/B,W+/B,GR+/BG"                        && Campos de Entrada de dados
			pCLR_BROWSE         := "W+/B,B/W,R+/B,GR+/B,BG+/B,R+/B,W+/GB,B/W"     && Cores do Browse

		Else
			pCLR_MESSAGE        := "B/W,R/W"
			pCLR_BACKGROUND     := "B/W,R/W"
			pCLR_PUSHBUTTON     := "W/N,N/W,N/W,W+/N"
			pCLR_MENU           := "b/w,gr+/rb,r/w,g/rb,n+/w,w+/b"
			pCLR_FORM           := "W/N"
			pCLR_LABEL          := "W/N,N/W"
			pCLR_SCROLLBAR      := "B/W,R/W,W+/B,GR+/BG"
			pCLR_FIELDGET       := "B/W,R/W,B/BG,GR+/BG"
			pCLR_FIELDLISTBOX   := "b/w,gr+/rb,r/w,g/rb,n+/w,w+/b,w/b,b/w"
			pCLR_FIELDGRADIOBOX := "N/W"
			pCLR_FIELDBRADIOBOX := "W+/B,W+/B,GR+/B"
			pCLR_FIELDCHECKBOX  := "B/W,R/W,W+/B,GR+/BG"
			pCLR_BROWSE         := "B/BG,B/W"

		EndIf

		// Cria o Menu
		pMNU_MAINMENU := MenuCreate()

		pSCR_BANNER   := { |cName| ;
							DispBegin(),;
							hb_DispBox( pSCR_ROWTOP, pSCR_COLTOP, pSCR_ROWTOP+ 4, pSCR_COLBOTTOM, B_SINGLE + " ", SystemLabelColor() ),             ;
							hb_DispOutAt( pSCR_ROWTOP+ 1, pSCR_COLTOP+ 1,    "Edilson Mendes Nascimento", SystemLabelColor() ),                     ;
							hb_DispOutAt( pSCR_ROWTOP+ 1, pSCR_COLBOTTOM-30, PADL( TRANSFORM( DATE(), "@E 99/99/9999"), 30 ), SystemLabelColor() ), ;
							hb_DispOutAt( pSCR_ROWTOP+ 2, pSCR_COLTOP+ 1,    "Kodiak(tm)", SystemLabelColor() ),                                    ;
							hb_DispOutAt( pSCR_ROWTOP+ 2, pSCR_COLBOTTOM-30, PADL( "Versao 1.0a", 30 ), SystemLabelColor() ),                      ;
							hb_DispOutAt( pSCR_ROWTOP+ 3, pSCR_COLTOP+ 1,    PADC( cName, pSCR_COLBOTTOM -2 ), SystemLabelColor() ),                ;
							DispEnd() }

		EVAL( pSCR_BANNER, "Menu Principal" )

		WHILE MenuModal( pMNU_MAINMENU, 1, pSCR_ROWBOTTOM, pSCR_COLTOP, pSCR_COLBOTTOM, pCLR_MESSAGE ) <> 999
		ENDDO
		
	always
		// Restaura a tela Principal
		PopScreen()
	end sequence

return


/***
*
*	MenuCreate()
*
*	Realiza a montagem do Menu Principal.
*
*/
STATIC FUNCTION MenuCreate()

local oTopBar
local oSistema
LOCAL oItem
		
		
	oTopBar := TopBar( pSCR_ROWTOP+ 5, pSCR_COLTOP, pSCR_COLBOTTOM )
	oTopBar:ColorSpec := pCLR_MENU

		// Create a new popup menu named FILE and add it to the TopBar object
		oSistema := PopUp()
		oSistema:ColorSpec := pCLR_MENU
		oTopBar:AddItem( MenuItem ( "&Sistema", oSistema ) )

			oItem := MenuItem( "St&art         Ctr-A", {|| Nil }, K_CTRL_A, "Inicia o servico" )
			oItem:Enabled := pTRUE
			oSistema:AddItem( oItem  )

			oItem := MenuItem( "St&op          Ctr-O", {|| Nil }, K_CTRL_O, "Para o servico" )
			oItem:Enabled := pTRUE
			oSistema:AddItem( oItem  )

			oSistema:AddItem( MenuItem( MENU_SEPARATOR ) )			  

			oItem := MenuItem( "Conso&le       Ctr-L", {|| Console() }, K_CTRL_L, "Janela de console" )
			oItem:Enabled := pTRUE
			oSistema:AddItem( oItem  )

			// Separador do Menu
			oSistema:AddItem( MenuItem( MENU_SEPARATOR ) )

			oItem := MenuItem( "&Sair"                ,{|| pTRUE }, K_ALT_F4, "Finaliza a Aplicacao", 999)
			oSistema:AddItem( oItem)

return( oTopBar )
	


/***
*
*	InitSystem()
*
*	Funcao Executada no momento da inicializacao do sistema.
*
*/
INIT PROCEDURE InitSystem()

	// Definir o Idioma Portugues
/*	REQUEST HB_LANG_PT
	REQUEST HB_CODEPAGE_PT850
	hb_langSelect("PT")
	HB_SETCODEPAGE("PT850") */

    // Definir o Idioma Portugues
    REQUEST HB_LANG_PT
    REQUEST HB_CODEPAGE_UTF8
    REQUEST HB_CODEPAGE_PT850

    hb_langSelect("PT")
    HB_SETCODEPAGE("PT850")


	SetMode( 33, 100 )

	// Inicializa as Variaveis do Sistema
	pInitKodiak

return

/***
*
*	ExistSystem()
*
*	Funcao Executada no momento da Finalizacao do systema.
*
*/
EXIT PROCEDURE ExitSystem()

	DBCloseAll()	

return


/***
*
*	SystemLabelColor() --> String
*
*	Realizado o retorno da cor utilizada na exibicao dos Labels.
*
*/
FUNCTION SystemLabelColor()
return( pCLR_LABEL )

/***
*
*	SystemFormColor() --> String
*
*	Funcao responsavel em realizar o retorno da cor utilizada no Form.
*
*/
FUNCTION SystemFormColor()
return( pCLR_FORM )

/***
*
*	SiBrowseColor() --> String
*
*	Retorna a cor utilizada nos campos de RadioBox.
*
*/
FUNCTION SiBrowseColor()
return( pCLR_BROWSE )

/***
*
*	SiPushButton() --> String
*
*	Funcao responsavel em realizar o retorno da cor utilizada no PushButtom.
*
*/
FUNCTION SiPushButton()
return( pCLR_PUSHBUTTON )

/***
*
*	SiScrollBar() --> String
*
*	Funcao responsavel em realizar o retorno da cor na Barra de Rolagem Vertical.
*
*/
FUNCTION SiScrollBar()
return( pCLR_SCROLLBAR )


/***
*
*	SystemMaxRow() --> Int
*
*	Realiza o retorno do numero de linha maxima na tela.
*
*/
FUNCTION SystemMaxRow()
return( pSCR_ROWBOTTOM  )

/***
*
*	SystemMaxCol() --> Int
*
*	Realiza o retorno do numero de coluna maxima na tela.
*
*/
FUNCTION SystemMaxCol()
return( pSCR_COLBOTTOM  )

/* #pragma BEGINDUMP

int hb_fsProcessIsRunning( HB_HANDLE hProcess, DWORD dwTimeOut )
{
    BOOL fRunning = FALSE;

    HB_TRACE( HB_TR_DEBUG, ("hb_fsProcessIsRunning(%p, %d)", (void *) (HB_PTRDIFF) hProcess, dwTimeOut));

    {
        DWORD dwResult;
        HANDLE hProc = (HANDLE) hb_fsGetOsHandle( hProcess );

        if( hProc) {
            hb_vmLock();
            dwResult = WaithForSinglebject( hProc, dwTimeOut);
            if( dwResult != WAIT_OBJECT_0)
            fsRuning = TRUE;
            hb_vmLock();
        } else
        hb_fsSetError(( UNSORT) FS_ERROR );
    }
    return fsRuning;
}

HB_FUNC( HB_PROCESSISRUNNING )
{
    HB_FHANDLE hProcess = hb_numToHandle( hb_parnint(1));

    if( hProcess != 0 && hProcess != FS_ERROR && ( hb_pcount() > 2 || ISNUM(2)))
    hb_retl( hb_processValue( hProcess, ( hb_pcount() > 2 ? 0 : hb_parnint(2))));
    else
    hb_errRT_BASE_Substr( EG_ARG, 4001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}
#pragma ENDDUMP
*/


FUNCTION COM()

	local nPos

	for nPos:= 1 to 100
		DispOutAt( 10, 10, Str( nPos ) )
	next

Return( Nil )