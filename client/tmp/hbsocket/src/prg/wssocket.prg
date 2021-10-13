//wssocket.prg

#include "h5.ch"
                
#define MAGIC_KEY "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
#define TYPE_CONT    0x0
#define TYPE_TEXT    0x1
#define TYPE_BINA    0x2
#define TYPE_CLOSE   0x8
#define TYPE_PING    0x9
#define TYPE_PONG    0xA
#define WS_MAXLENGTH 2^63

#ifdef __PLATFORM__LINUX
#define PID_FILE "pid.pid"
#define SIGHUP   1
#define SIGTERM 15
#define SIGKILL  9
#else
#define _SERVICE_NAME "hbwsocket"
#define _SERVICE_DESCRIBE "Web Socket"
#endif

#define PORT_FILE hb_dirBase() + "port.txt"

//MESSAGES
#define H5_STARTED       0x1

static oHost
static lWorking := .F.

FUNCTION H5_GetHost()
RETURN oHost

CLASS WebSocketServer FROM HBSocket

   DATA bOnCloseClient
   DATA bOnStarted 
   DATA bOnNewClient
   
   DATA hMainWnd 
   DATA hMutexWork
   DATA hObjects INIT hb_Hash()
   
   DATA lBackground
   DATA lStarted
   
   METHOD New( nPort, cMode )
   
   METHOD HandleEvent( hEvents )
   METHOD HandShake( oClient )
   
   METHOD KillBack()
   
   METHOD NewClient( oClient ) 
   METHOD OnReadProcess( oClient )
   METHOD OnClose( oClient )
   
   METHOD Activate()
#ifdef __PLATFORM__LINUX
   METHOD SignalHandler( nSignal )
#endif   

   METHOD SetObject( o ) INLINE ::hObjects[ o:cID ] := o
   

ENDCLASS

//-----------------------------------------//

METHOD New( cPort, cMode ) CLASS WebSocketServer

   LOCAL nError
   LOCAL cMsg
   LOCAL nProcess
   
   oHost      = Self
   ::hMainWnd = hb_Hash()
   ::hMutexWork = hb_MutexCreate()
   
   ::lStarted    = .F.
   ::lBackground = .T.
   
   IF ! File( PORT_FILE ) .OR. ! Empty( cPort )
      DEFAULT cPort TO "2000"   
      MemoWrit( PORT_FILE, AllTrim( cPort ) )
   ENDIF

   ::nPort = Val( cPort )
   
#ifdef __PLATFORM__LINUX
   DEFAULT cMode TO "start"
   cls
   if cMode == "START"
     if File( PID_FILE )
        QOut(  "daemon is already running" + CRLF )
        return nil
     else
        MemoWrit( PID_FILE, AllTrim( Str( getpid() ) ) )
     endif
   elseif cMode == "STOP"
     if File( PID_FILE )
        nProcess = Val( MemoRead( PID_FILE ) ) + 1
        hb_tracelog( "finishing daemon", nProcess )
        KillTerm( nProcess )
        return nil
     endif
   elseif Upper( cMode ) == "NONE"
      ::lBackground = .F.
   else
     QOut(  CRLF + "FiveTech daemon. Syntax:" + CRLF )
     QOut(  "   ./daemon start" + CRLF )
     QOut(  "   ./daemon stop" + CRLF + CRLF  )
     return nil
   endif   

   IF ::lBackground

   SetSignalsHandler( Self )

   if Fork() > 0
     return nil  // Parent process ends and child process continues
   endif

   umask( 0 ) // In order to write to any files (including logs) created by the daemon

   SetSignalAlarm()

   QOut(  "daemon starts" + CRLF )
   ::Activate()
   
   ENDIF
   // Close standard files descriptors
   // CloseStandardFiles()
#endif

#ifdef __PLATFORM__WINDOWS
   DEFAULT cMode TO "S" /* NOTE: Must be the default action */
   SWITCH Upper( cMode )
   CASE "INSTALL"

      IF win_serviceInstall( _SERVICE_NAME, _SERVICE_DESCRIBE )
         ? "Service has been successfully installed"
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Error installing service: " + hb_ntos( nError ) + " " + cMsg
      ENDIf
      EXIT

   CASE "UNINSTALL"

      IF win_serviceDelete( _SERVICE_NAME )
         ? "Service has been deleted"
         ferase( PORT_FILE )
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Error deleting service: " + hb_ntos( nError ) + " " + cMsg
      ENDIf
      EXIT
   
   CASE "NONE"
      /* NOTE: Used run like a app */      
      ::lBackground = .F.
//      SrvMain()
      EXIT
   CASE "S"
      /* NOTE: Used when starting up as service.
               Do not invoke the executable manually with this option */

      IF win_serviceStart( _SERVICE_NAME, {|| ::Activate() } )
         ? "Service has started OK"
      ELSE
         nError := wapi_GetLastError()
         cMsg := Space( 128 )
         wapi_FormatMessage( ,,,, @cMsg )
         ? "Service has had some problems: " + hb_ntos( nError ) + " " + cMsg
      ENDIF    
   OTHERWISE
      ? "Invalid parameter"
      EXIT

   ENDSWITCH
#endif

RETURN Self

//-----------------------------------------//

METHOD KillBack() CLASS WebSocketServer
   local nProcess
   
   IF ::lBackground
#ifdef __PLATFORM__LINUX
   QOut(  CRLF + "daemon ends" + CRLF )
   nProcess = Val( MemoRead( PID_FILE ) ) + 1
   FErase( PID_FILE )
   KillKill( nProcess )
#endif   
#ifdef __PLATFORM__WINDOWS
    win_serviceSetExitCode( 0 )
    win_serviceStop()
#endif
   ENDIF
   
RETURN NIL

//-----------------------------------------//

METHOD Activate() CLASS WebSocketServer
   
   local oError

   if ::lStarted
      RETURN NIL 
   endif
   
   ::lStarted = .T.
   
   BEGIN SEQUENCE
   altd()
  //Super:new( ::nPort )
  ::super:new( ::nPort )
   
   if ::lBackground
#ifdef  __PLATFORM__WINDOWS
      ::bOnProccess = {|| ! ( win_serviceGetStatus() == WIN_SERVICE_RUNNING ) }
#endif    
   else 
      cls
      ? "Press <ESC> to exit!!!"
altd()
   endif
   ::lDebug    = .T.
   ::bDebug    = {| aParam | LogFile( "debug.txt", aParam ) }
   ::bOnRead   = {| oSrv, oClient | ::OnReadProcess( oClient ) }
   ::bOnAccept = {| oSrv, oClient | ::NewClient( oClient )  }
   ::bOnKillClient = {| oClient | ::OnClose( oClient ) }
   ::Listen()
   ::End()
   RECOVER USING oError
      LogFile( "error.log", { oError:description } )
   ENDSEQUENCE

   ::KillBack()
   
   ::lStarted = .F.
   
RETURN NIL   

//-----------------------------------------//

METHOD HandleEvent( hEvents, oClient ) CLASS WebSocketServer

   local nEvent 
   local wParam 
   local lParam 
   local cParam 
   
   IF hEvents != NIL 
      nEvent := hEvents[ "message" ]
      wParam := hEvents[ "wParam"]
      lParam := hEvents[ "lParam" ]
      cParam := hEvents[ "cParam" ]
      

      SWITCH nEvent
         CASE H5_STARTED /*STARTED*/
            IF hb_isBlock( ::bOnStarted )
               Eval( ::bOnStarted, oClient )
            ENDIF
            EXIT
         CASE 0x2 //CLICK ON MENU ITEM      
            H5_GetMainMenu()[ lParam ]:HandleEvent( nEvent, wParam, lParam )
            EXIT
         CASE 0x3 //MOUSE MOVE
            ::hObjects[ lParam ]:OnMouseMove( H5_HighWord( wParam ), H5_LowWord( wParam ) )         
            EXIT         
      ENDSWITCH
      
   ELSE 
      ? "ERROR"
   ENDIF
   
RETURN NIL

//-----------------------------------------//

METHOD HandShake(  oClient ) CLASS WebSocketServer
   local nLen, cBuffer, cContext, cKey, cSend
   
   cBuffer := oClient:cBuffer

   cContext = GetContext( cBuffer, "Sec-WebSocket-Key" )
   cKey     = hb_Base64Encode( hb_sha1( cContext + MAGIC_KEY, .T. ) ) // + "." add something to check that the handshake gets wrong
   
   cSend = "HTTP/1.1 101 Switching Protocols" + CRLF + ;
           "Upgrade: websocket" + CRLF + ;
           "Connection: Upgrade" + CRLF + ;
           "Sec-WebSocket-Accept: " + cKey + CRLF + CRLF   

   nLen = ::SendData( oClient, cSend )

   IF nLen > 0
      oClient:lHandShake = .T.          
   ENDIF

   
RETURN NIL 

//-----------------------------------------//

METHOD NewClient( oClient ) CLASS WebSocketServer

   __objAddData( oClient, "lHandShake" ) 
   __objAddData( oClient, "aBuffer" ) 
   __objAddData( oClient, "oFrame" ) 
   __objAddData( oClient, "oMainWnd" ) 
   __objModMethod( oClient, "SendData", @Send() )
   __objAddMethod( oClient, "SendFrame", @SendFrame() )
   __objAddMethod( oClient, "ReadFromBuffer", @ReadFromBuffer() )
   
   oClient:lHandShake = .F.
   oClient:nTimeOut   = 0
   oClient:aBuffer    = {}
   
   IF hb_isBlock( ::bOnNewClient )
      Eval( ::bOnNewClient, oClient )
   ENDIF

RETURN NIL

//-----------------------------------------//

METHOD OnClose( oClient ) CLASS WebSocketServer
   
   IF hb_isBlock( ::bOnCloseClient )
      Eval( ::bOnCloseClient, oClient )
   ENDIF

   IF oClient:oMainWnd != NIL
      oClient:oMainWnd:End()
   ENDIF

RETURN NIL

//-----------------------------------------//

METHOD OnReadProcess( oClient ) CLASS WebSocketServer
   
   local cMask, cData, n, cMsg:="", oError
   local nMaskPos := 1, cAnswer
   local oFrame
   local hJSon, nJSon
   
   //handshake
   IF ! oClient:lHandShake
      ::HandShake( oClient ) 
   ELSE 
      IF  !lWorking
      hb_mutexLock( ::hMutexWork )   
      lWorking = .t.
      hb_mutexUnlock( ::hMutexWork )   
            
      oClient:aBuffer = WS_UnPack( oClient:cBuffer )
      IF oClient:oFrame == NIL
         oFrame = WebSocketFrame():New()
         oClient:oFrame = oFrame
      ELSE 
         oFrame = oClient:oFrame
      ENDIF
      oFrame:Read( oClient )
      IF oFrame:lCompleted
         nJSon = hb_jsonDecode( WS_Pack( oFrame:aData ), @hJSon )
         IF hJSon != NIL
            ::HandleEvent( hJSon, oClient )
         ENDIF
         oClient:oFrame = NIL
      ENDIF
      hb_mutexLock( ::hMutexWork )   
      lWorking = .F.
      hb_mutexUnlock( ::hMutexWork )         
      
      ENDIF
            
   ENDIF

RETURN NIL

#ifdef __PLATFORM__LINUX
METHOD SignalHandler( nSignal ) CLASS WebSocketServer

  DO CASE 
     CASE nSignal == SIGHUP
          QOut(  "Received SIGHUP signal" + CRLF  )
     CASE nSignal == SIGKILL .or. nSignal == SIGTERM
          QOut( "to exit..." + CRLF )
          ::lExit = .T. 
     OTHERWISE
          QOut(  "Unhandled SIGNAL " + AllTrim( Str( nSignal ) ) + CRLF )
  ENDCASE

RETURN NIL

#endif


//-----------------------------------------//

STATIC FUNCTION ReadFromBuffer( nFrom, nBytes )

   local Self := QSelf()
   local aResult
   
   IF nFrom > ::nBufferLength
      RETURN aResult
   ENDIF
   
   IF nBytes > ::nBufferLength
      nBytes = ::nBufferLength
   ENDIF
   
   aResult = Array_Slice( ::aBuffer, nFrom, nBytes )
   
   nFrom += nBytes

RETURN aResult

//-----------------------------------------//

STATIC FUNCTION Send( cData, nType ) 
   
   local aData    := {}
   local nLenData := Len( cData )
   local nFramesNeeded
   local n
   local oFrame
   local Self := QSelf()
   
   
   DEFAULT nType TO TYPE_TEXT

   /* If the message type is text, the data is a string that must be transformed into a
    * byte array. */
   IF nType == TYPE_TEXT
   
       /* If there is data, split per character */
       IF nLenData > 0
          FOR n = 1 TO nLenData
             AAdd( aData, Asc( SubStr( cData, n, 1 ) ) )
          NEXT
       ENDIF
       
   ENDIF
   
   /* Determine how many frames will be needing to send this message */
   nFramesNeeded = Int( nLenData / WS_MAXLENGTH ) + 1
   
   /* Send the date frame by frame */
   FOR n = 0 TO nFramesNeeded - 1 
       /* The first parameter is the message type. The actual message type must only be set
        * for the first frame, all following frames must have the continuation frame-type set
        * The second parameter is true if this is the final frame. False otherwise
        * The final parameter indicates whether the data should be masked. The protocol only states
        * that data send from the client MUST be masked. Data that is send by the server shouldn't be
        * masked. (Point 4.3)
        *
        */
       oFrame = WebSocketFrame():New( If( n != 0, 0, nType ), nFramesNeeded == n + 1, .T. );
   
       /* Get the piece of the message that should be send in the current frame. */
       IF nLenData > 0
           oFrame:setData( array_slice(aData, WS_MAXLENGTH * n, WS_MAXLENGTH ) )
       ENDIF
   
       /* And use the sendFrame method to actually send the frame to the client. */
       ::sendFrame( oFrame )
       /*
       if (!$this -> sendFrame($pFrame)) {
           $this -> close(false);
           return false;
       }
       */
   NEXT
   
   //::SendData( oClient, cSend )

RETURN NIL

/**
* This method takes objects that implement the Frameable interface
* and will send the stream it gets from Frameable :: asByteStream()
* to the client. This stream has to be a complete frame.
*
* @param Frameable $pFrame The frame that has to be send.
* @return boolean True on succes, false otherwise
*/

STATIC FUNCTION sendFrame( oFrame )
   
   local Self := QSelf()
   
RETURN ::oServer:SendData( Self, oFrame:PrepareToSend() )


//-----------------------------------------//

CLASS WebSocketFrame

   DATA aMaskingKey
   DATA aData

   DATA lFinal 
   DATA nRsv   
   DATA nType  
   DATA lMasked
   DATA lCompleted
   DATA lReadFirst
   DATA lLengthRead
   DATA nMaxLengthOut
   DATA nPayLoadLength

   METHOD New( )

   METHOD Read( oClient ) 
   METHOD setData( aData )
   METHOD PrepareToSend() 
  
   HIDDEN:   
   METHOD asByteStream()
   
   METHOD generateMaskingKey()
   
   METHOD isControlFrame() INLINE ::nType == TYPE_CLOSE .OR. ::nType == TYPE_PING .OR. ::nType == TYPE_PONG

   METHOD Mask()
   
   METHOD RePack( aBytes )   
   
   METHOD unMask() 
   
   METHOD unpack( nValue, nBytes )
   
   
   
ENDCLASS

//-----------------------------------------//

METHOD New( nType, lFinal, lMasked, nRsv )

   DEFAULT nType   TO 0x0
   DEFAULT lFinal  TO .T.
   DEFAULT lMasked TO .T.
   DEFAULT nRsv    TO 0x0
   

   ::lFinal         = lFinal 
   ::nRsv           = nRsv   
   ::nType          = nType  
   ::lMasked        = lMasked
   ::nPayLoadLength = 0
   ::nMaxLengthOut  = WS_MAXLENGTH
   ::aMaskingKey    = {}
   ::aData          = {}
   ::lReadFirst     = .F.
   ::lCompleted     = .F.
   ::lLengthRead    = .F.
        
   /* Control frames may not be fragmented */
   IF ::isControlFrame()
      ::lFinal = .T.
   ENDIF

RETURN Self

//-----------------------------------------//
/**
* Part of the Frameable interface. This method will when called return a byte array
* containing a valid frame. It will mask the frame if the masking bit is set.
*/
METHOD asByteStream() CLASS WebSocketFrame
   /* The frame will be stored in this array as bytes */   
   local aBytes := {}
   local nByte

    IF ::nPayLoadLength != Len( ::aData )
       RETURN .F.
    ENDIF
    
    /* The first byte... */
    nByte = 0x0
    
    /* If this frame is the final frame of a message, set the MSB to 1 */
    IF ::lFinal 
       nByte = hb_BitOR( nByte, 0x80 )
    ENDIF
    
    /* The next three bytes of the first byte are the bits reservers for extensions */    
    nByte = hb_BitOR( nByte, hb_BitShift( ::nRsv, 4 ) )
    
    /* The final 4 bits of the first byte contains the frame opcode or type */
    nByte = hb_BitOR( nByte, ::nType )
    
    /* The first byte is fully parsed, store it in the result byte array */
    AAdd( aBytes, nByte )
    
    /* The first bit of the second byte is the masking bit. If the frame is masked,
     * set it to 1 and set it to 0 otherwise. */
    IF ::lMasked
        nByte = 0x80
    ELSE
        nByte = 0x0
    ENDIF
    
    /*
     * The next bytes are a bit more complicated, if the payload length is less then
     * 126 bytes, the length will be in the next 7 bits. If the payload length is larger
     * then 125 but smaller then OxFFFF the next 7 bits should be 126 and the actual length
     * is in the next 16 bits. If the payload length is larger then 0xFFFF then the 7 bits
     * should be 127 and the actual length is in the next 64 bits. The length is a handled
     * as a unsigned integer.
     */
    IF ::nPayLoadLength < 126
        nByte = hb_BitOR( nByte, ::nPayLoadLength )
        AAdd( aBytes, nByte )
    ELSEIF ::nPayLoadLength <= 0xFFFF
        nByte = hb_BitOR( nByte, 0x7E )
        AAdd( aBytes, nByte )
        Array_Merge( aBytes, ::UnPack( ::nPayLoadLength, 2 ) )
    ELSEIF ::nPayLoadLength <= WS_MAXLENGTH
        nByte = hb_BitOR( nByte, 0x7F )
        AAdd( aBytes, nByte )
        Array_Merge( aBytes, ::UnPack( ::nPayLoadLength, 8 ) )
    ELSE
        RETURN .F.
    ENDIF

    /* If the masking key is set, mask the frame */
    IF ::lMasked
        /* Generate a random masking key if non already exists */
        IF Len( ::aMaskingKey ) != 4 
            ::aMaskingKey = ::generateMaskingKey()
        ENDIF
        
        /* Add the masking key to the frame */
        Array_Merge( aBytes, ::aMaskingKey )
        
        /* mask the data */
        ::Mask()
        
    ENDIF
    
    /* Add the data to the resulting byte stream (the frame) if there is any data*/
    IF ::nPayLoadLength != 0
       Array_Merge( aBytes, ::aData )
    ENDIF

    /* Return byte array */
    
RETURN aBytes


/**
* Generates a random 4 byte maskink key for XOR encryption
*
* @return array Newly generated masking key
*/
METHOD generateMaskingKey() CLASS WebSocketFrame
   local aMaskingKey := {}
   local i 
    
   FOR i = 1 TO 4
       AAdd( aMaskingKey,  hb_random( 0, 0xFF ) )
   NEXT 
       
RETURN aMaskingKey

/**
 * This method will mask the payload data using the masking key.
 * If no masking key is set, a random key will be generated.
 */
METHOD Mask() CLASS WebSocketFrame

   local aMaskingKey
   local i 
   /* Do we have a valid masking key? */
   IF Len( ::aMaskingKey) == 4
       /* If so, use that key */
       aMaskingKey = ::aMaskingKey
   ELSE
       /* If not, generate one */
       aMaskingKey = ::generateMaskingKey()
   ENDIF
 
   /* Mask using XOR encryption */
   FOR i = 0 TO Len( ::aData ) - 1
      ::aData[ i + 1 ] = hb_BitXOR( ::aData[ i + 1 ], aMaskingKey[ ( i%4 ) + 1 ] )
   NEXT
   
RETURN NIL

//-----------------------------------------//

METHOD PrepareToSend() CLASS WebSocketFrame
   local cStream, aBytes
   
 /* Array containing the bytes that make up the frame in correct order */
   aBytes = ::asByteStream()

   /* Pack that array into a string (byte-string) */
   
   cStream := WS_Pack( aBytes )
   
   /* And send this to the client */
   
RETURN cStream

//-----------------------------------------//

/* If we haven't read the first 2 bytes and the read-buffer is smaller
 * then 2 bytes, wait for the next round and hope the bytes will be
 * available then. */
 
METHOD Read( oClient ) CLASS WebSocketFrame

   local nLastPos := 1
   local aBytes
   
   IF ! ::lReadFirst
   /* Extract the values from the bytes using bit-wise operations.
    * Point 4.2 of the protocol goes more into details as to how
    * WebSocket Frames are formed */
      aBytes   = oClient:ReadFromBuffer( @nLastPos, 2 )
       ::lFinal             = hb_BitAnd( 0x80, aBytes[ 1 ] ) == 0x80 
       ::nRsv               = hb_BitShift( hb_BitAnd( 0x70, aBytes[ 1 ] ), -4 )
       ::nType              = hb_BitAnd( 0xF, aBytes[ 1 ] ) 
       ::lMasked            = hb_BitAnd( 0x80, aBytes[ 2 ] )  == 0x80
       ::nPayLoadLength     = hb_BitAnd( 0x7F, aBytes[ 2 ] )       
       ::lReadFirst         = .T.
   ENDIF
   /* If the payloadlength in the header is 126, the actual payload length
    * is in the next two bytes. So read those. If the value in the header
    * is 127, the actual payloadlength is in the next 8 bytes. */
   IF ::nPayLoadLength == 126 .AND. ! ::lLengthRead
      IF oClient:nBufferLength >= 2
         ::nPayLoadLength = ::RePack( oClient:ReadFromBuffer( @nLastPos, 2 ) )
         ::lLengthRead = .T.
      ENDIF
   ELSEIF ::nPayLoadLength == 127 .AND. ! ::lLengthRead
      IF oClient:nBufferLength >= 8
          ::nPayLoadLength = ::RePack( oClient:ReadFromBuffer( @nLastPos, 8 ) )
          ::lLengthRead = .T.
      ENDIF
   ELSE 
      ::lLengthRead = .T.
   ENDIF

   /* If the payloadlength is larger then WebSocketFrame :: m_nMaxLengthIn we can't process it properly */
   IF ::nPayLoadLength > WS_MAXLENGTH
      RETURN .F.
   ENDIF
   
   /* The header contains a masking-bit. When this bit is set, the next 4 bytes are a 
    * masking key that is used to mask the payloaddata using XOR encryption. */
   IF ::lMasked .AND. Len( ::aMaskingKey ) == 0
      IF oClient:nBufferLength >= 4
         ::aMaskingKey = oClient:ReadFromBuffer( @nLastPos, 4 )
         // AEval( ::aMaskingKey, {| u | QOut( Chr( u ) ) } )
      ENDIF
   ENDIF
   /* If there is enough data in the read-buffer, read the data. */
   ::aData = Array_Merge( ::aData, oClient:ReadFromBuffer( @nLastPos, ::nPayLoadLength ) )
   IF Len( ::aData ) == ::nPayLoadLength
   
       /* If the masking bit is set, unmask the data using the masking-key we
        * read from the stream earlier. */
       IF ::lMasked
          ::unMask()
       ENDIF
       ::lCompleted = .T.
       ::lReadFirst = .F.
       
   ELSE /*TODO*/       
       /* The frame is not complete yet */
       ::lCompleted = .F.
   ENDIF
   
   /* Nothing went wrong! YEEY! */
           
RETURN .T.

//-----------------------------------------//

/**
 * Data that was read from a socket is divided into bytes. When we need
 * to repack 4 bytes back into a 32 bit integer we have to merge these
 * bytes and do some bit shifting.
 *
 * @param array $aBytes Byte array to repack
 * @return int The resulting integer
 */
METHOD RePack( aBytes ) CLASS WebSocketFrame

   local nResult := 0x0
   local i
   
   FOR i = 1  TO Len( aBytes )
       nResult = hb_BitShift( nResult, 8 )
       nResult += aBytes[ i ]
   NEXT
       
RETURN nResult;

   
   
//-----------------------------------------//
   
METHOD setData( aData ) CLASS WebSocketFrame

   local nlen := Len( aData )

   IF ::nMaxLengthOut < nlen
       /* Exceeding maximum frame length */
       RETURN .F.
   ENDIF
   
   /* Reset cache */

   ::aData = aData;
   
   ::nPayLoadLength = nlen

RETURN .T.

//-----------------------------------------//

/**
 * This method will unmask the payload data using the masking key
 *
 * @return true if unmasking was succesfull, false otherwise
 */
METHOD unMask() CLASS WebSocketFrame

   /* We need 4 bytes in the masking key */
   IF Len( ::aMaskingKey ) == 4
      /* Unmasking uses the same algorithm as masking */
      ::Mask()
      RETURN .T.
   ENDIF
    
RETURN .F.


/**
 * When we need to send integers larger then 8 bits we need to split them
 * into multiple bytes in order to send them. If you request more bytes then
 * are in the value, the singing bit will be used to form new bytes.
 *
 * @param int nValue Value that needs to be split into bytes
 * @param int nBytes Number of bytes in the resulting array
 * @return array The bytes array
 */
METHOD UnPack( nValue, nBytes ) CLASS WebSocketFrame
   
   local aResult
   local i
   
   aResult = {}
   
   FOR i = 1 TO nBytes
      AAdd( aResult, hb_BitAnd( nValue, 0xFF ) )
      nValue = hb_BitShift( nValue, -8 )
   NEXT
    
   aResult = array_reverse( aResult )
    
RETURN aResult


//-----------------------------------------//

STATIC FUNCTION Array_Slice( aArray, nPos, nLen )

   local n 
   local aSlice := {}
   
   DEFAULT nPos TO 1
   DEFAULT nLen TO Len( aArray )

   IF nPos <= 0 
      nPos = 1
   ENDIF
   
   IF nPos > Len( aArray ) 
      RETURN aSlice
   ENDIF   

   IF nPos + nLen > Len( aArray ) 
      nLen = Len( aArray ) - nPos + 1
   ENDIF      
   
   IF nLen <= 0 
      nLen = Len( aArray )
   ENDIF   

   IF nLen > Len( aArray ) 
      nLen = Len( aArray )
   ENDIF   
   
   FOR n = nPos TO nPos + nLen - 1
      AAdd( aSlice, aArray[ n ] )
   NEXT 

RETURN aSlice

//-----------------------------------------//

STATIC FUNCTION Array_Reverse( aArray )
   
   local nLen := Len( aArray )
   local nHalf := Int( nLen / 2 ) - 1
   local n
   local uOld
   
   FOR n = 0 TO nHalf
      uOld = aArray[ nLen - n ]
      aArray[ nLen - n ] = aArray[ n + 1 ]
      aArray[ n + 1 ] = uOld      
   NEXT

RETURN aArray

//-----------------------------------------//

STATIC FUNCTION Array_Merge( aSource, aMerge )
   
   local uData
   
   IF hb_IsArray( aSource ) .AND. hb_IsArray( aMerge )
   
      FOR EACH uData IN aMerge
         AAdd( aSource, uData )      
      NEXT
      
   ENDIF  
   

RETURN aSource

//-----------------------------------------//

STATIC FUNCTION GetContext( cData, cContext )

  local nLen := Len( cContext )
  local cValue := ""
  local aLines
  local aSubLine
  local cRow
  
  aLines := hb_ATokens( cData, CRLF )
  FOR each cRow IN aLines
     IF cContext $ cRow
        aSubLine = hb_ATokens( cRow, ":" )
        cValue = AllTrim( aSubLine[ 2 ] )
        EXIT
     ENDIF
  NEXT

RETURN cValue

//-----------------------------------------//

STATIC FUNCTION WS_UnPack( cData )
   local aResult := {}
   local i 
   
   FOR i = 1 TO Len( cData )
      AAdd( aResult, Asc( SubStr( cData, i, 1 ) ) )
   NEXT 

RETURN aResult

//-----------------------------------------//

STATIC FUNCTION WS_Pack( aBytes )

   local cStream := ""
   local nBit

   FOR EACH nBit IN aBytes 
      cStream += Chr( nBit )
   NEXT
   
RETURN cStream   

//-----------------------------------------//

FUNCTION H5_GetClient()

   local oClient, o
   local pThread := hb_ThreadSelf()
   
   FOR EACH o IN oHost:hClients
      oClient = o
      IF oClient:pThread == pThread         
         EXIT 
      ENDIF
   NEXT

RETURN oClient


//-----------------------------------------//

FUNCTION H5_HIGHWORD( a )
RETURN hb_BitShift( ( a - H5_LOWWORD( a ) ), -16 );

//-----------------------------------------//

FUNCTION H5_LOWWORD( a )
RETURN hb_BitAnd( a, ( hb_BitShift( 1, 16 ) -1 ) ) ;

//-----------------------------------------//

#ifdef __PLATFORM__LINUX

#pragma BEGINDUMP

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <syslog.h>
#include <string.h>
#include <signal.h>

#include <hbapi.h>
#include <hbvm.h>

static PHB_ITEM pSelf;

HB_FUNC( FORK )
{
 hb_retnl( fork() );
}

HB_FUNC( UMASK )
{
  umask( hb_parnl( 1 ) );
}

HB_FUNC( EXIT )
{
  exit( EXIT_SUCCESS );
}

HB_FUNC( GETPPID )
{
  hb_retnl( getppid() );
}

HB_FUNC( GETPID )
{
  hb_retnl( getpid() );
}

HB_FUNC( KILLTERM )
{
  kill( hb_parnl( 1 ), SIGTERM );
}

HB_FUNC( KILLKILL )
{
  kill( hb_parnl( 1 ), SIGKILL );
}

HB_FUNC( CLOSESTANDARDFILES )
{
  close( STDIN_FILENO );
  close( STDOUT_FILENO );
  close( STDERR_FILENO );
}

void CatchAlarm( int sig )
{
 HB_SYMBOL_UNUSED( sig );
}

void SignalHandler( int sig )
{
  hb_vmPushSymbol( hb_dynsymGetSymbol( "SIGNALHANDLER" ) );
  hb_vmPush( pSelf );
  hb_vmPushLong( sig );
  hb_vmFunction( 1 );
}

HB_FUNC( SETSIGNALALARM )
{
 signal( SIGALRM, CatchAlarm );
}

HB_FUNC( SETSIGNALSHANDLER )
{
  pSelf = hb_param( 1, HB_IT_OBJECT );
  signal( SIGHUP, SignalHandler );
  signal( SIGTERM, SignalHandler );
  signal( SIGINT, SignalHandler );
  signal( SIGQUIT, SignalHandler );
  signal( SIGKILL, SignalHandler );
}

HB_FUNC( ALARM )
{
 alarm( hb_parnl( 1 ) );
}

HB_FUNC( SLEEP )
{
 sleep( hb_parnl( 1 ) );
}

HB_FUNC( SYSLOG )
{
  syslog( hb_parnl( 1 ), hb_parc( 2 ), hb_parc( 3 ) );
}

HB_FUNC( TEST )
{
  hb_retnl( LOG_WARNING );
}

#pragma ENDDUMP

#endif 

//#include "hbsocket.prg"
