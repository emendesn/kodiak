/*******
*
*  amfws-stub.prg by Aleksander Czajczynski
*
*  amfws-stub.prg - Basic routines for communications using AMF
*                   over WebSocket
*
********/

#include "amfio.ch"
#include "hbsocket.ch"
#include "common.ch"
#include "hboo.ch"
#include "hbthread.ch"

#define MAGIC_KEY "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
#define CRLF chr(13)+chr(10)
#define MAX_MESSAGE_LEN 131070

#ifdef HB_UNICODE
   #xtranslate Chr( <exp> ) => HB_BChar( <exp> )
   #xtranslate Asc( <exp> ) => HB_BCode( <exp> )
   #xtranslate Len( <exp> ) => HB_BLen( <exp> )
   #xtranslate SubStr( <exp> ) => HB_BSubStr( <exp> )
   #xtranslate Left( <exp> ) => HB_BLeft( <exp> )
#endif

STATIC s_lExit := .F.

FUNCTION AMFWS_MTSERVER( nPort, cIfAddr, hAnonFuncs, lUserControl, symConvIn, symConvOut, lObjectExt )
   LOCAL pListenSocket

   IF !Empty( lObjectExt )
      IF HB_IsLogical( symConvIn ) .AND. symConvIn
         symConvIn := @AMFIO_ObjConvIn()
      ENDIF
      IF HB_IsLogical( symConvOut ) .AND. symConvOut
         symConvOut := @AMFIO_ObjConvOut()
      ENDIF
   ELSE
      IF !HB_IsSymbol( symConvIn )
         symConvIn := NIL
      ENDIF
      IF !HB_IsSymbol( symConvOut )
         symConvOut := NIL
      ENDIF
   ENDIF

   IF hb_mtvm()
      
      s_lExit := .F.
      
      pListenSocket := hb_socketOpen()
      IF ! hb_socketBind( pListenSocket, { HB_SOCKET_AF_INET, cIfAddr, nPort } )
         ? "bind() error", hb_socketGetError()
         RETURN -1
      ELSEIF ! hb_socketListen( pListenSocket )
         ? "listen() error", hb_socketGetError()
         RETURN -1
      ENDIF 

      hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_PUBLIC, @AMFWS_SRVLOOP(), pListenSocket, @AMFWS_SERVER(), hAnonFuncs, lUserControl, symConvIn, symConvOut, lObjectExt ) )

   ENDIF

   RETURN pListenSocket

FUNCTION AMFWS_SRVLOOP( pListenSocket, symServer, ... )
   LOCAL pConnectionSocket
   LOCAL aRemote := {}

   DO WHILE .T.

      pConnectionSocket := hb_socketAccept( pListenSocket, @aRemote, 50 )
      
      IF ! EMPTY( pConnectionSocket )
         IF s_lExit == .T.
            hb_socketShutdown( pConnectionSocket )
            hb_socketClose( pConnectionSocket )
            RETURN 0
         ENDIF
         hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_PUBLIC, symServer, pConnectionSocket, ... ) )
      ELSEIF s_lExit == .T.
         RETURN 0
      ENDIF 
   ENDDO 
   RETURN 0

PROCEDURE AMFWS_SERVER( pConnectionSocket, hAnonFuncs, lUserControl, symConvIn, symConvOut, lObjectExt, symFuncRedir )
   LOCAL cBuf := Space( 4096 )
   LOCAL hLPP, cPacket := Space( 4096 )
   LOCAL lHandShake := .F.
   LOCAL nLen, nPos, nByte
   LOCAL lFin, nOpCode, cPayLoad, cMasked, lMask, cMask, nMaskPos, c
   LOCAL nPayLoadLen
   LOCAL cResponse

   cPacket := ""

   DO WHILE !lHandShake

      IF ( nLen := hb_socketRecv( pConnectionSocket, @cBuf, 4096, 0, 1000 ) ) > 0
         IF Len( cPacket ) + nLen > 4096
            // HTTP request is going to be larger than 4096 bytes
            // we better disconnect such client
            s_lExit := .T.  
         ELSE
            cPacket += Left( cBuf, nLen )
            IF ( nPos := RAt( CRLF + CRLF, cPacket ) ) > 0
               IF AMFWS_HandShake( Left( cPacket, nPos - 1 ), pConnectionSocket )
                  cPacket := SubStr( cPacket, nPos + 4 )                  
                  EXIT // handshake ok, advancing to stream loop
               ELSE
                  cPacket := SubStr( cPacket, nPos + 4 ) // we didn't recive a WebSocket request, wait for another one
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF s_lExit
         RETURN  
      ENDIF

   ENDDO

   AMFIO_INIT( hAnonFuncs, lUserControl, symFuncRedir )

   DO WHILE .T.

      IF !ReadUpto( 2, pConnectionSocket, @cBuf, @cPacket )
         EXIT
      ENDIF

      nOpCode := Asc( Left( cPacket, 1 ) )      
      lFin := HB_BitTest( nOpCode, 7 )
      IF !lFin
         // message fragmenting not yet supported, we have to create
         // one another outer loop
         ? "fragmented messages not yet supported"
         EXIT
      ENDIF
      // RSV1, 6
      // RSV2, 5
      // RSV3, 4
      nOpCode := HB_BitAnd( nOpCode, 0x0F )

      nPayLoadLen := Asc( SubStr( cPacket, 2, 1 ) )
      cPacket := SubStr( cPacket, 3 )

      lMask := HB_BitTest( nPayLoadLen, 7 )
      nPayLoadLen := HB_BitReset( nPayLoadLen, 7 )
      
      IF nPayLoadLen == 126 // 16-bit UINT
         IF !ReadUpto( 2, pConnectionSocket, @cBuf, @cPacket )
            EXIT
         ENDIF
         nPayLoadLen := BEBin2U( Left( cPacket, 2 ) )
         cPacket := SubStr( cPacket, 3 )
      ELSEIF nPayLoadLen == 127 // 64-bit UINT
         IF !ReadUpto( 8, pConnectionSocket, @cBuf, @cPacket )
            EXIT
         ENDIF
         nPayLoadLen := BEBin2U( Left( cPacket, 8 ) )
         cPacket := SubStr( cPacket, 9 )
      ENDIF

      IF nPayLoadLen > MAX_MESSAGE_LEN // message is too large for us, discard this client
         EXIT
      ENDIF

      IF lMask

         IF !ReadUpto( 4, pConnectionSocket, @cBuf, @cPacket )
            EXIT
         ENDIF

         cMask := Left( cPacket, 4 )
         cPacket := SubStr( cPacket, 5 )

      ENDIF

      IF !ReadUpto( nPayLoadLen, pConnectionSocket, @cBuf, @cPacket )
         EXIT
      ENDIF

      cPayload := Left( cPacket, nPayLoadLen )
      cPacket := SubStr( cPacket, nPayLoadLen + 1 )

      IF lMask
         cMasked := cPayload
         cPayload := ""
         nMaskPos := 1
         FOR EACH c IN cMasked
             cPayLoad += Chr( HB_BitXOR( Asc( SubStr( cMask, nMaskPos++, 1 ) ), Asc( c ) ) )
             IF nMaskPos == 5
                nMaskPos := 1
             ENDIF
         NEXT
      ENDIF

      ? "we received", cPayload

      cResponse := AMFIO_HandleRequest( @cPayLoad, symConvIn, symConvOut, lObjectExt, symFuncRedir )

      IF cResponse # NIL
           HB_SocketSend( pConnectionSocket, AMFWS_Response( cResponse ) )
      ENDIF

      IF s_lExit
         EXIT
      ENDIF

   ENDDO
   
   AMFIO_ThreadSendBlock( NIL )
   
   hb_socketShutdown( pConnectionSocket )
   hb_socketClose( pConnectionSocket )

   RETURN

STATIC FUNCTION ReadUpto( n, pConnectionSocket, cBuf, cPacket )
   LOCAL nLen
   DO WHILE Len( cPacket ) < n .AND. !s_lExit
      IF ( nLen := hb_socketRecv( pConnectionSocket, @cBuf, n, 0, 1000 ) ) > 0
          cPacket += Left( cBuf, nLen )
      ENDIF
   ENDDO
   IF s_lExit
      RETURN .F.
   ENDIF
   RETURN .T.

FUNCTION AMFWS_Response( cResponse )
   LOCAL nOpCode, nLen, cPayLoadLen

   nOpCode := 0x01 // 0x01 is a text frame
   nOpCode := HB_BitSet( nOpCode, 7 ) // set FIN frame marker

   nLen := Len( cResponse )
   IF nLen <= 125           
      // HB_BitSet( nPayLoadLen, 7 ) // set this if you want string masking
      cPayLoadLen := Chr( nLen )
   ELSEIF nLen >= 126 .and. nLen <= 65535
     // Chr( HB_BitSet( 126, 7 ) ) // if string masking
     cPayLoadLen := Chr( 126 ) // 16-bit UINT
     cPayLoadLen += BEW2Bin( nLen )
   ELSE
     // Chr( HB_BitSet( 127, 7 ) ) // if string masking
     cPayLoadLen := Chr( 127 ) // 64-bit UINT
     cPayLoadLen += BEU2Bin( nLen )
   ENDIF

   RETURN Chr( nOpCode ) + cPayLoadLen + cResponse

// Based on a code shared by Antonio Linares and Daniel Garcia-Gil
STATIC FUNCTION AMFWS_HandShake( cBuffer, pConnectionSocket )
   LOCAL nLen, cContext, cKey, cSend
      
   cContext := AMFWS_GetContext( cBuffer, "Sec-WebSocket-Key" )
   IF !Empty( cContext )
      cKey := HB_Base64Encode( HB_SHA1( cContext + MAGIC_KEY, .T. ) ) // + "." add something to check that the handshake gets wrong
   
      cSend := "HTTP/1.1 101 Switching Protocols" + CRLF + ;
               "Upgrade: websocket" + CRLF + ;
               "Connection: Upgrade" + CRLF + ;
               "Sec-WebSocket-Accept: " + cKey + CRLF + CRLF   

      nLen := HB_SocketSend( pConnectionSocket, cSend )

      IF nLen > 0
         RETURN .T.
      ENDIF
   ENDIF
   
   RETURN .F.

// Based on a code shared by Antonio Linares and Daniel Garcia-Gil
STATIC FUNCTION AMFWS_GetContext( cData, cContext )
  LOCAL nLen := Len( cContext )
  LOCAL cValue := ""
  LOCAL aLines
  LOCAL aSubLine
  LOCAL cRow
  
  aLines := hb_ATokens( cData, CRLF )
  FOR EACH cRow IN aLines
     IF cContext $ cRow
        aSubLine := hb_ATokens( cRow, ":" )
        cValue := AllTrim( aSubLine[ 2 ] )
        EXIT
     ENDIF
  NEXT

RETURN cValue
   
FUNCTION AMFWS_SHUTDOWN()
   return s_lExit := .T.

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbapiitm.h"

HB_FUNC( BEBIN2U )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );   

   if( pItem )
   {
      HB_SIZE nLen = hb_itemGetCLen( pItem );
      if( nLen )
      {
         const char * pszString = hb_itemGetCPtr( pItem );
         if( nLen >= 5 )
         {             
            hb_retnint( HB_GET_BE_UINT64( pszString ) );
         }
         if( nLen >= 3 )
            hb_retnint( HB_GET_BE_UINT32( pszString ) );
         else
            hb_retnint( HB_GET_BE_UINT16( pszString ) );
      }
   }
   else
      hb_retnint( 0 );
}

HB_FUNC( BEU2BIN )
{
   char szResult[ 4 ];
   HB_U32 uiValue = ( HB_U32 ) hb_parnint( 1 );
   HB_PUT_BE_UINT32( szResult, uiValue );
   hb_retclen( szResult, 4 );
}

HB_FUNC( BEW2BIN )
{
   char szResult[ 2 ];
   HB_U16 uiValue = ( HB_U16 ) hb_parni( 1 );
   HB_PUT_BE_UINT16( szResult, uiValue );
   hb_retclen( szResult, 2 );
}

#pragma ENDDUMP
