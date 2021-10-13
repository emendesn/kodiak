/*
* KODIAK
* (C) 2020 Edilson Mendes Nascimento <edilson.mendes.nascimento@gmail.com>
*/

/***
*
*	Classes para Manipulacao de Janelas.
*
*/

#INCLUDE "hbclass.ch"
#INCLUDE "common.ch"
#INCLUDE "set.ch"
#INCLUDE "box.ch"

/***
*
*	WindowsNew
*
*	Objeto para manipulacao de telas 
*
*	Objetos:
*            :Top    - Define a linha inicial para o posicionamento da janela
*            :Left   - Define a coluna inicial para o posicionamento da janela
*            :Bottom - Define a linha final para o posicionamento da janela
*            :Right  - Define a coluna final para o posicionamento da janela
*            :Border - Define o formato da borda da janela
*            :Color  - Define a cor da janela
*            :Header - Define a mensagem apresentada no topo da janela
*
*	Metodos:
*            :Open()  - Cria o objeto
*            :Open()  - Exibe na tela o objeto criado
*            :Close() - Fecha o objeto criado
*
*/
CLASS WindowsNew
	HIDDEN:
		DATA cScreen AS CHARACTER
	
	EXPORTED:
		DATA Top     AS NUMERIC
		DATA Left    AS NUMERIC
		DATA Bottom  AS NUMERIC
		DATA Right   AS NUMERIC
		DATA Border  AS CHARACTER
		DATA Color   AS CHARACTER
		DATA Header  AS CHARACTER

	METHOD New( nTop, nLeft, nBottom, nRight, cBorder, cColor, cHeader )
	METHOD Open()
	METHOD Close()
	
END CLASS


/***
*
*	WindowsNew -> New( <nTop>, <nLeft>, <nBottom>, <nRight>, <cBorder>, <cColor>, <cHeader> )
*
*	Metodo utilizado para a criacao do objeto na tela
*
*	Parametors:
*            <nTop>    - Define a linha inicial para o posicionamento da janela
*            <nLeft>   - Define a coluna inicial para o posicionamento da janela
*            <nBottom> - Define a linha final para o posicionamento da janela
*            <nRight>  - Define a coluna final para o posicionamento da janela
*            <nBorder> - Define o formato da borda da janela
*            <nColor>  - Define a cor da janela
*            <nHeader> - Define a mensagem apresentada no topo da janela
*
*
*/
METHOD New( nTop, nLeft, nBottom, nRight, cBorder, cColor, cHeader ) CLASS WindowsNew
	
	::Top    := IIF( .NOT. ISNIL( nTop ), nTop,  0 )
	::Left   := IIF( .NOT. ISNIL( nLeft ), nLeft, 0 )
	::Bottom := IIF( .NOT. ISNIL( nBottom ), nBottom, MAXROW() )
	::Right  := IIF( .NOT. ISNIL( nRight ), nRight, MAXCOL() )
	::Border := IIF( .NOT. ISNIL( cBorder ), cBorder, B_SINGLE )	
	::Color  := IIF( .NOT. ISNIL( cColor ), cColor, SET( _SET_COLOR ) )
	::Header := IIF( .NOT. ISNIL( cHeader ), cHeader, "" )
		
RETURN QSelf()


/***
*
*	WindowsNew -> Open()
*
*	Metodo utilizado para a criacao do objeto na tela
*
*
*/
METHOD Open() CLASS WindowsNew
		
LOCAL nRow := ROW()
LOCAL nCol := COL()

	// Salva a regiao corrente
	::cScreen := SAVESCREEN( ::Top, ::Left, ::Bottom+ 2, ::Right+ 2)
	
	// Cria a Janela
	hb_DispBox( ::Top, ::Left, ::Bottom, ::Right, ::Border, ::Color )
	
	// Cria o efeito de sombra na Janela
	DBGSHADOW( ::Top, ::Left, ::Bottom, ::Right )
	
	// Exibe o titulo da Janela
	IF .NOT. ISNIL( ::Header )
		@ ::Top, ::Left+ 1 SAY ::Header COLOR ::Color
	ENDIF
	
	SETPOS( nRow, nCol)
		
RETURN QSelf()


/***
*
*	WindowsNew -> Close()
*
*	Metodo utilizado para a fechar o objeto na tela e restaurar a area
*
*
*/
METHOD Close() CLASS WindowsNew

	// Restaura a regiao da Janela
	IF .NOT. ISNIL( ::cScreen )
		RESTSCREEN( ::Top, ::Left, ::Bottom+ 2, ::Right+ 2, ::cScreen )
	ENDIF
		
RETURN QSelf()