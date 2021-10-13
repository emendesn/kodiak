/*
* KODIAK
* (C) 2020 Edilson Mendes Nascimento <edilson.mendes.nascimento@gmail.com>
*/

#include "common.ch"
#include "main.ch"

/***
*
* PushScreen( <nTop>, <nLeft>, <nBottom>, <nRight>, <lPopScreen> ) --> cHa
*
*	Processa os parametros passados na linha de comando.
*
*/
FUNCTION PushScreen( nTop, nLeft, nBottom, nRight, lPopScreen )

LOCAL xRetValue
LOCAL cScreen

STATIC aPushScreen

	DEFAULT nTop       TO 0,        ;
            nLeft      TO 0,        ;
            nBottom    TO MAXROW(), ;
            nRight     TO MAXCOL(), ;
            lPopScreen TO pFALSE
	
	If lPopScreen
		xRetValue   := ATAIL( aPushScreen )
		aPushScreen := ASIZE( aPushScreen, LEN( aPushScreen ) -1)
	Else
		DEFAULT aPushScreen TO {}
		xRetValue := pTRUE
		cScreen   := TRANSFORM( nTop, "99")
		cScreen   += TRANSFORM( nLeft, "99")
		cScreen   += TRANSFORM( nBottom, "99")
		cScreen   += TRANSFORM( nRight, "99")
		cScreen   += SAVESCREEN( nTop, nLeft, nBottom, nRight )
		AADD( aPushScreen, cScreen )
	EndIf

return( xRetValue )


/***
*
*	Classe Para Manipulacao de tela() --> NIL
*
*	Cria a tabela de Erros do sistema.
*
*/	
FUNCTION PopScreen

LOCAL cScreen   := PushScreen( ,,,, pTRUE)
LOCAL nTop
LOCAL nLeft
LOCAL nBottom
LOCAL nRight
LOCAL lRetValue := pFALSE
	
	If ISCHARACTER( cScreen )
		lRetValue  := pTRUE
		nTop    := VAL( SUBSTR( cScreen, 1, 2) )
		nLeft   := VAL( SUBSTR( cScreen, 3, 2) )
		nBottom := VAL( SUBSTR( cScreen, 5, 2) )
		nRight  := VAL( SUBSTR( cScreen, 7, 2) )
		RESTSCREEN( nTop, nLeft, nBottom, nRight, SUBSTR( cScreen, 9 ) )
	EndIf

return( lRetValue )
