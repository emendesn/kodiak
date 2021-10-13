#include "hbclass.ch"
#include "hbsocket.ch"
#include "fileio.ch"
#include "common.ch"


#define CRLF chr(13)+chr(10)

//-----------------------------------------//

CLASS HBSocket

   DATA   cBindAddress INIT "0.0.0.0"

   DATA bDebug
   DATA bOnAccept
   DATA bOnClose
   DATA bOnListen
   DATA bOnRead
   DATA bOnWrite
   DATA bOnProccess
   DATA bOnKillClient
 
   DATA cBuffer
   DATA cLogFile
   DATA cError
 
   DATA hClients
   DATA hMutexUser

   DATA nClientId
   DATA nPort

   DATA lDebug
   DATA lExit        

   DATA pSocket
   DATA oPP

   METHOD New()
   METHOD End()


   METHOD KillClient( oClient )
   
   METHOD Listen()

   METHOD NewId() INLINE ::nClientId++
   
   METHOD OnAccept( oClient )
   METHOD OnClose() VIRTUAL
   METHOD OnRead( oClient )
   
   METHOD SendData( cText ) 
   
   HIDDEN:
   
   METHOD Debug( cText ) 


ENDCLASS

//-----------------------------------------//

METHOD New( nPort, oPP ) CLASS HBSocket

   DEFAULT nPort TO 8080
   
   ::nClientId = 1
   ::nPort     = nPort
   ::hClients  = hb_Hash()
altd()
   ::lExit     = .F.
   ::lDebug    = .F. 
   
   ::oPP       = oPP
   

RETURN Self

//-----------------------------------------//

METHOD End() CLASS HBSocket

   LOCAL oClient
   
   for each oClient in ::hClients
      ::Debug( "CLIENT CLOSED", oClient:hSocket, oClient:nID )
      ::KillClient( oClient )
   next
   
   if ::pSocket != NIL
      //HBSocketClose( ::pSocket )
   endif
   
   if ::bOnClose != NIL 
      Eval( ::bOnClose, Self )
   endif      

RETURN nil

//-----------------------------------------//

METHOD Debug( ... ) CLASS HBSocket
   
   local aParams := hb_aParams()

   IF ::lDebug 
   
      IF hb_IsBlock( ::bDebug )
         Eval( ::bDebug, aParams )
      ELSE
         AEval( aParams, {| u | QOut( u ) } )
      ENDIF
      
   ENDIF

RETURN NIL

//------------------------------------------------------//

METHOD KillClient( oClient ) CLASS HBSocket
   
   local nID 
   
   IF hb_IsBlock( ::bOnKillClient )
      Eval( ::bOnKillClient, oClient )
   ENDIF
   
   nID = oClient:nID   
   oClient:End()
   
   hb_mutexLock( ::hMutexUser )   
   hb_HDEL( ::hClients, nID )
   hb_mutexUnlock( ::hMutexUser )


return nil

//-----------------------------------------//

METHOD Listen() CLASS HBSocket

   ::pSocket     = HB_SocketOpen( )
   ::hMutexUser  = HB_MutexCreate()   

   IF ! HB_SocketBind( ::pSocket, { HB_SOCKET_AF_INET, ::cBindAddress, ::nPort } )
      QOut( ::cError :=  "Bind error " + hb_ntos( HB_SocketGetError() ) )
      HB_SocketClose( ::pSocket )
      RETURN .F.
   ENDIF

   IF ! HB_SocketListen( ::pSocket )
      QOut( ::cError :=  "Listen error " + hb_ntos( HB_SocketGetError() ) )
      HB_SocketClose( ::pSocket )
      RETURN .F.
   ENDIF
      
   if hb_IsBlock( ::bOnListen )
      Eval( ::bOnListen, Self )
   endif
   ::Debug( "LISTEN" )

   hb_ThreadStart( {|| ::OnAccept() } )
altd()
   DO WHILE ! ::lExit
   

      inkey( 0.1 )  

      if ::bOnProccess != nil 
         ::lExit = eval( ::bOnProccess, Self )
      else 
         ::lExit := LastKey() == 27
      endif
      
               
   ENDDO    

   ::End()
   
RETURN .T.

//-----------------------------------------//

METHOD OnAccept() CLASS HBSocket

   local pClientSocket
   local oClient
   
   ::Debug( "ONACCEPT" )
      
   do while ! ::lExit

      if ! Empty( pClientSocket := HB_SocketAccept( ::pSocket,, 500 ) )
         ::Debug( "ACCEPTED", pClientSocket )
         hb_mutexLock( ::hMutexUser )
         ::NewId()
         oClient = HBSocketClient():New( Self )
         oClient:nID = ::nClientId
         oClient:hSocket = pClientSocket
         HB_SOCKETSETSNDBUFSIZE( oClient:hSocket, 4096*2 )
         HB_SOCKETSETRCVBUFSIZE( oClient:hSocket, 4096*2 )
         hb_HSET( ::hClients, ::nClientId, oClient )
         hb_mutexUnlock( ::hMutexUser )
         if ::bOnAccept != NIL
            Eval( ::bOnAccept, Self, oClient )
         endif         
         hb_ThreadStart( {| oClient | ::OnRead( oClient ) }, oClient )         
         oClient:TimerConnection()
      elseif ! ::lExit
         //? "Catched error ",  hb_ntos( HBSocketGetError() )
         //EXIT
      endif
   enddo
   
RETURN nil

//------------------------------------------------------//

METHOD OnRead( oClient ) CLASS HBSocket

   local lMyExit    := .F.
   local cData, oError
   local nLength  := 0
   local nRetry   := 0
   local lActive  := .T.
   local cBuffer
   
   oClient:pThread = hb_ThreadSelf()
   
   do while ! lMyExit 

      cBuffer = Space( 4096 )   
      BEGIN SEQUENCE
         if oClient:hSocket != NIL
            if ( nLength := HB_SocketRecv( oClient:hSocket, @cBuffer, 4096*2, 0, 1000 ) ) > 0
               oClient:cBuffer = AllTrim( cBuffer )
               oClient:nBufferLength = Len( oClient:cBuffer )
            endif         
         else 
            lMyExit = .T.
         endif 
      RECOVER USING oError
         ::Debug( oError:Description )
         lMyExit := .t.
      ENDSEQUENCE

      if lMyExit
         EXIT
      endif
      
      if nLength > 0
         ::Debug( "ONREAD", oClient:hSocket, "<" + oClient:cBuffer + ">", Len( oClient:cBuffer ) )
      endif

      if nLength == 0
         lMyExit = .T.         
      elseif nLength > 1      
         oClient:RestartTimerConnection()   
         if ::bOnRead != NIL
            Eval( ::bOnRead, Self, oClient )
         else 
            HBSocket_CallBack_Onread( Self, oClient )
         endif
      endif

   enddo  
   
   ::Debug( "CLIENT LISTEN FINISHED", oClient:hSocket )   
   
   ::KillClient( oClient )


RETURN nil

//-----------------------------------------//

METHOD SendData( oClient, cSend ) CLASS HBSocket

   local nLen 

   ::Debug( "SENDING...", cSend )
   
   DO WHILE Len( cSend ) > 0
      IF ( nLen := HB_SocketSend( oClient:hSocket, @cSend ) ) == - 1
         EXIT
      ELSEIF nLen > 0
         cSend = SubStr( cSend, nLen + 1 )     
      ENDIF
   ENDDO
   
   
RETURN nLen   

//-----------------------------------------//
//-----------------------------------------//

CLASS HBSocketClient

   DATA hSocket
   DATA nID
   DATA Cargo
   DATA oServer
   DATA cBuffer
   DATA cOutPut
   DATA nTimeOut      //time in seconds
   DATA nCurrentTime
   DATA bOnClose
   DATA nBufferLength
   DATA pThread
   
   METHOD New( oServer )
   
   METHOD End() 
   
   METHOD CloseConnection()   INLINE ::oServer:KillClient( Self )
   
   METHOD RestartTimerConnection() INLINE ::nCurrentTime := Seconds() + ::nTimeOut
   
   METHOD SendData( cSend ) INLINE ::oServer:SendData( Self, cSend )
   
   METHOD TimerConnection( nTimeOut ) 
   METHOD TimeOut()

ENDCLASS

//-----------------------------------------//

METHOD New( oSrv ) CLASS  HBSocketClient

   ::oServer  = oSrv
   ::nTimeOut = 1200000
   ::nBufferLength = 0

RETURN Self

//------------------------------------------------------//

METHOD End() CLASS HBSocketClient 

   IF hb_IsBlock( ::bOnClose )
      Eval( ::bOnClose, Self )
   ENDIF
   
   ::hSocket = NIL

RETURN NIL

//------------------------------------------------------//

METHOD TimerConnection( nTimeOut ) CLASS HBSocketClient
   
   DEFAULT nTimeOut TO ::nTimeOut
   
   IF nTimeOut > 0
      ::nTimeOut = nTimeOut
      
      ::nCurrentTime = Seconds() + ::nTimeOut
      
      hb_ThreadStart( {|| ::TimeOut() } , self )
   ENDIF

return nil

//----------------------------------------------------------//

METHOD TimeOut() CLASS HBSocketClient

   DO WHILE ::nCurrentTime > seconds() //.AND. o:lTimer
      hb_idlesleep( 1 )
   enddo

   ::CloseConnection()
   
RETURN NIL

//-----------------------------------------//

FUNCTION LogFile( cFileName, aInfo )

   local hFile, cLine := DToC( Date() ) + " " + Time() + ": ", n
   
   cFileName = hb_dirBase() + cFileName
   
   for n = 1 to Len( aInfo )
      cLine += uValToChar( aInfo[ n ] ) + Chr( 9 )
   next
   cLine += CRLF

   if ! File( cFileName )
      FClose( FCreate( cFileName ) )
   endif

   if( ( hFile := FOpen( cFileName, FO_WRITE ) ) != -1 )
      FSeek( hFile, 0, FS_END )
      FWrite( hFile, cLine, Len( cLine ) )
      FClose( hFile )
   endif

RETURN NIL

//---------------------------------------------------------------------------//

static function uValToChar( uVal )

   local cType := ValType( uVal )

   do case
      case cType == "C" .or. cType == "M"
           return uVal

      case cType == "D"
           #ifdef __XHARBOUR__
              if HasTimePart( uVal )
                 return If( Year( uVal ) == 0, TToC( uVal, 2 ), TToC( uVal ) )
              endif
           #endif
           return DToC( uVal )

      #ifdef __HARBOUR__
         #ifndef __XHARBOUR__
            case cType == "T"
               return If( Year( uVal ) == 0, HB_TToC( uVal, '', Set( _SET_TIMEFORMAT ) ), HB_TToC( uVal ) )
         #endif
      #endif

      case cType == "L"
           return If( uVal, ".T.", ".F." )

      case cType == "N"
           return TStr( uVal )

      case cType == "B"
           return "{|| ... }"

      case cType == "A"
           return "{...}"

      case cType == "O"
           return If( __ObjHasData( uVal, "cClassName" ), uVal:cClassName, uVal:ClassName() )

      case cType == "H"
           return "{=>}"

      case cType == "P"
           #ifdef __XHARBOUR__
              return "0x" + NumToHex( uVal )
           #else
              return "0x" + hb_NumToHex( uVal )
           #endif

      otherwise

           return ""
   endcase

return nil

//---------------------------------------------------------------------------//

static function TStr( n )
return AllTrim( Str( n ) )

//---------------------------------------------------------------------------//

#pragma BEGINDUMP
#include "hbapi.h"

void * pfnCallBack_OnRead;

void set_callback_Onread( void * p )
{
	void * pfnCallBack_OnRead = p;   
}

HB_FUNC( SET_CALLBACK_ONREAD )
{
   set_callback_Onread( ( void * ) hb_parptr( 1 ) );   
}

HB_FUNC( HBSOCKET_CALLBACK_ONREAD )
{
   PHB_ITEM pServer = hb_param( 1, HB_IT_OBJECT );
   PHB_ITEM pClient = hb_param( 2, HB_IT_OBJECT );
   
   if( pfnCallBack_OnRead )
   {
      void (*pfn)( PHB_ITEM, PHB_ITEM );
  	  pfn = pfnCallBack_OnRead;
  	  pfn( pServer, pClient );      	
   }
   
}

#pragma ENDDUMP
