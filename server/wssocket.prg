/*
* KODIAK
* (C) 2020 Edilson Mendes Nascimento <edilson.mendes.nascimento@gmail.com>
*/

#ifdef __PLATFORM_WINDOWS
    #include 'hbwin.ch'
#endif

#include 'hbclass.ch'
#include 'common.ch'
#include 'main.ch'

#define pCRLF             Chr(13) + Chr(10)
#define pH5_MAX_ID        8

#define pMAGIC_KEY        "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
#define pTYPE_CONT        0x0
#define pTYPE_TEXT        0x1
#define pTYPE_BINA        0x2
#define pTYPE_CLOSE       0x8
#define pTYPE_PING        0x9
#define pTYPE_PONG        0xA
#define pWS_MAXLENGTH     2^63

#define pMSG_CONTROL      0x400

#ifdef __PLATAFORM__LINUX
    #define pPID_FILE        'pid.pid'
    #define pSIGHUP           1
    #define pSIGTERM         15
    #define pSIGKILL          9
#else
    #define _SERVICE_NAME     'hbwsocket'
    #define _SERVICE_DESCRIBE 'Web Socket'
#endif

#define pPORT_FILE        hb_DirBase() + 'port.txt'


// Message
#define pH5_STARTED       0x1


// Temporizador para o Metodo PingPongControl
#define pTMR_PINGPONG     5



static oHost
static lWorking  := pFALSE


function H5_GetHost()
    return oHost


/***
*
*	CLASS WebSocketServer
*
*	Exibe a janela com as conexoes ativas
*
*/
CLASS WebSocketServer FROM hBSocket

    EXPORTED:
        DATA aMaskingKey    AS ARRAY
        Data aData          AS ARRAY

        DATA bOnCloseClient AS BLOCK
        DATA bOnStarted     AS BLOCK
        DATA bOnNewClient   AS BLOCK
        DATA bOnReadProcess AS BLOCK
        DATA bOnInit        AS BLOCK

        DATA hMainWnd       AS USUAL
        DATA hMutexWork     AS USUAL
        DATA hObjects       AS USUAL   INIT hb_Hash()

        DATA lBackground    AS LOGICAL
        DATA lStarted       AS LOGICAL

    METHOD New( nPort, cMode, bOnInit, bOnStarted )

    METHOD HandleEvent( hEvent, oClient )
    METHOD HandShake( oClient )
    METHOD HybiEncode( cData, lBinary )
    METHOD HybiDecode( cData, oClient )

    METHOD KillBack()

    METHOD generateMaskingKey()
    METHOD Mask()
    METHOD UnMask()

    METHOD NewClient( oCLient )
    METHOD OnReadProcess( oClient )
    METHOD OnClose( oClient )

    METHOD Activate()
    //#ifdef __PLATAFORM__LINUX
    //    METHOD SignalHandler( nSignal )
    //#endif

    METHOD StartControl( oClient )
    METHOD SetObject( o )      INLINE ::hObjects[ o:cID ] := 0

END CLASS


METHOD New( nPort, cMode, bOnInit, bOnStarted) CLASS WebSocketServer

local nError
local cMsg
local nProcess
local nFork


    DEFAULT nPort      to 8080
    DEFAULT cMode      to 'start'
    DEFAULT bOnInit    to { || Nil }
    DEFAULT bOnStarted to { || Nil }


    oHost         := Self

    ::nPort       := nPort

    ::hMainWnd    := hb_Hash()
    ::hMutexWork  := hb_mutexCreate()

    ::lStarted    := pFALSE
    ::lBackground := pTRUE

    ::bOnInit     := bOnInit
    ::bOnStarted  := bOnStarted

    if .not. File( pPORT_FILE ) .or. nPort == 0
        MemoWrit( pPORT_FILE, AllTrim( Str( nPort ) ) )
    endif

    #ifdef __PLATAFORM__LINUX
        DEFAULT cMode to 'start'
        cMode := Upper( cMode )
        cls
        if cMode == 'START'
            if File( pPID_FILE )
                QQOut( 'daemon is already runing' + pCRLF)
                return Nil
            else
                MemoWrit( pPID_FILE, AllTrim( Str( GetPid() ) ) )
            endif
        elseif cMode == 'STOP'
            if File( pPID_FILE )
                nProcess := Val( MemoRead( pPID_FILE ) ) + 1
                hb_traceLog( 'finishing daemon', nProcess )
                KillTerm( nProcess )
                return Nil
            endif
        elseif cMode == 'NONE'
            ::lBackground := pFALSE
        else
            QOut( pCRLF + 'daemon syntax: ' + pCRLF )
            QOut( '   ./daemon start' + pCRLF )
            QOut( '   ./daemon stop' + pCRLF + pCRLF )
            return Nil
        endif

        if ::lBackground

            SetSignalsHandler()
            if ( nFork := Fork() ) > 0
                return Nil  // Parent process ends and child process continues
            endif
            UnMask( 0 )  // in order to write to any  files (include Logs) create by the daemon

            SetSignalAlarm()

            QOut( 'daemon start' + pCRLF)
        endif

        // close start file descriptors
        // CloseStandardFiles()
    #endif

    #ifdef __PLATFORM_WINDOWS
        DEFAULT cMode TO 'S'  // NOTE: Must be the default action

        switch Upper( cMode )
            case 'INSTALL'
                if win_serviceInstall( _SERVICE_NAME, _SERVICE_DESCRIBE)
                    ? 'Service has been successfully installed'
                else
                    nError := wapi_GetLastError()
                    cMsg   := Space( 128 )
                    wapi_FormatMessage( ,,,, @cMsg)
                    ? 'Error installing service: ' + hb_ntos( nError ) + ' ' cMsg
                endif
                exit

            case 'UNINSTALL'
                if win_serviceDelete( _SERVICE_NAME )
                    ? 'Service has been deleted'
                    FErase( pPORT_FILE )
                else
                    nError := wapi_GetLastError()
                    cMsg   := Space(128)
                    wapi_FormatMessage( ,,,, @cMsg )
                    ? 'Error deleting service: ' + hb_ntos( nError ) + ' ' + cMsg
                endif
                exit

            case 'STOP'
                if win_serviceStoped( _SERVICE_NAME )
                    ? 'Service has Stoped'
                else
                    nError := wapi_GetLastError()
                    cMsg   := Space(128)
                    wapi_FormatMessage( ,,,, @cMsg )
                    ? 'Service has had some problems:' + hb_ntos( nError ) + '' + cMsg
                endif
                exit

            case 'START'
                if win_serviceInitiate( _SERVICE_NAME )
                    ? 'Service has had started ok'
                else
                    nError := wapi_GetLastError()
                    cMsg   := Space(128)
                    wapi_FormatMessage( ,,,, @cMsg )
                    ? 'Service has had some problems: ' + hb_ntos( nError ) + ' ' cMsg
                endif
                exit

            case 'NONE'
                // NOTE:  used run like a app
                ::lBackground := pFALSE
                // SrvMain()
                exit

            case'S'
                /* NONE: Used when starting up as service. 
                    Do not invoke the executable manually with this option */

                if win_serviceStart( _SERVICE_ANAME, { || :: Activate() },, 2/* WIN_SERVICE_AUTO_START */ )
                    ? 'Service has started ok'
                else
                    nErro := wapi_GetLastError()
                    cMsg  := Space(128)
                    wapi_FormatMessage( ,,,,@cMsg )
                    ? 'Service has had some problems: ' + hb_ntos( nErro ) + ' ' + cMsg
                endif

            otherwise
                ? 'Invalid parameters'
                exit

        end switch
    #endif

  //  ::Activate()

return Self


METHOD PROCEDURE KillBack() CLASS WebSocketServer

local nProcess

    if ::lBackground
        #ifdef __PLATAFORM__LINUX
            QOut( pCRLF + 'daemons ends' + pCRLF )
            nProcess := Val( MemoRead( pPID_FILE ) )+ 1
            FErase( pPID_FILE )
            KillKill( nProcess )
        #endif

        #ifdef __PLATFORM_WINDOWS
            win_serviceSetExitCode( 0 )
            win_serviceStop()
        #endif
    endif

return


METHOD PROCEDURE Activate() CLASS WebSocketServer

local oError

    if .not. ::lStarted

        ::lStarted := pTRUE

        begin sequence // with { |oErr| UErrorHandler( oErr, Self ) }

            ::New( ::nPort )

            if ::lBackground
                #ifdef __PLATFORM_WINDOWS
                    ::bOnProccess := { || .not. ( win_serviceGetStatus() == WIN_SERVICE STATUS )}
                #endif
            else
                cls
                ? 'Press <ESC> to exit !!!'
            endif

            ::lDebug        := pTRUE
            ::bDebug        := { |aParam| LogFile( 'debug_socket.txt', aParam ) }
            ::bOnRead       := { |oSrv, oClient| ::OnReadProcess( oClient ) }
            ::bOnAccept     := { |oSrv, oClient| ::NewClient( oClient ) }
            ::bOnKillClient := { |oClient| ::OnClose( oClient ) }

            if HB_ISBLOCK( ::bOnInit )
                Eval( ::bOnInit, Self )
            endif

            ::Listen()
            ::End()

        recover using oError
            // LogFile( "error.log", { oError:description })
        end sequence

        ::KillBack()

        ::lStarted := pFALSE

    endif

return


METHOD PROCEDURE HandleEvent( hEvent, oClient) CLASS WebSocketServer

local nEvent
local wParam
local lParam
local cParam


    if hEvent != Nil
        nEvent := hEvent[ 'message' ]
        wParam := hEvent[ 'wParam' ]
        lParam := hEvent[ 'lParam' ]
        cParam := hEvent[ 'cParam' ]

        switch nEvent
            case pH5_STARTED  /* STARTED */
                if HB_ISBLOCK( ::bOnStarted )
                    Eval( ::bOnStarted, oClient )
                endif
                exit

            case 0x2   // CLICK ON MENU ITEM
                // h5_GetMainMenu()[ lParam ]:HandleEvent( nEvent, wParam, lParam )
                exit

            case 0x3   // MOUSE MOVE
                ::hObjects[ lParam ]:OnMouseMove( H5_HighWord( wParam ), H5_LowWord( wParam ) )
                exit

        end switch

    else
        ? 'Error'
    endif

return


METHOD PROCEDURE HandShake( oClient ) CLASS WebSocketServer

//local nLen
local cBuffer
local cContext
local cKey
local cSend


    cBuffer  := oClient:cBuffer

    cContext := GetConText( cBuffer, 'Sec-WebSocket-Key')
    cKey     := hb_base64Encode( hb_sha1( cContext + pMAGIC_KEY, pTRUE ) ) // + '.' and something to check that the handshake gets wrong

    cSend    := 'HTTP/1.1 101 Switching Protocols' + pCRLF + ;
                'Upgrade: websocket' +  pCRLF + ;
                'Connection Upgrade' + pCRLF + ;
                'Sec-WebSocket-Accept: ' + cKey + pCRLF + pCRLF

    // if ( nLen := ::SendData( oClient, cSend ) ) > 0
    if ::SendData( oClient, cSend ) > 0
        oClient:lHandShake := pTRUE
    endif

return


METHOD PROCEDURE NewClient( oClient ) CLASS WebSocketServer

    __objAddData( oClient, 'lHandShake' )
    __objAddData( oClient, 'nPingPongControl' )
    __objAddData( oClient, 'nDataLength ')
    __objAddData( oClient, 'oMainWnd' )

    __objAddMethod( oClient, 'SendData', @Send() )
    __objAddMethod( oClient, 'PingPongControl', @PingPongControl() )
    __objAddMethod( oClient, 'doPing', @doPing() )
    __objAddMethod( oClient, 'doPong', @doPong() )

    oClient:lHandShake := pFALSE
    oClient:nTimeOut   := 0

    //::StartControl( oClient )
    if HB_ISBLOCK( ::bOnNewClient )
        Eval( ::bOnNewClient, oClient )
    endif

return


METHOD PROCEDURE StartControl( oClient ) CLASS WebSocketServer

    oClient:nPingPongControl := Seconds() + pTMR_PINGPONG

    if oClient:hSocket != Nil
        hb_threadStart( { |nTime| oClient:PingPongControl( nTime ) }, pTMR_PINGPONG )
    endif

return


METHOD PROCEDURE OnClose( oClient ) CLASS WebSocketServer

    if HB_ISBLOCK( ::bOnCloseClient )
        Eval( ::bOnCloseClient, oClient )
    endif

    oClient := Nil

return


METHOD OnReadProcess( oClient ) CLASS WebSocketServer

local lRetValue   := pTRUE
local cDecodeData //:= ''
local cBuffer     := oClient:cBuffer
//local nLenMsg
local hJson
local nJson

    if .not. oClient:lHandShake
        ::lHandShake( oClient )
    else
        if hb_At( Chr(129), cBuffer ) > 0 .or. ;
            hb_At( Chr( 1 ), cBuffer ) > 0 .or. ;
            hb_At( Chr(130), cBuffer ) > 0
            cDecodeData := ::HybiDecode( cBuffer, oClient )
        elseif hb_At( Chr(137), cBuffer) > 0
            cDecodeData := Chr( 0x8A ) + ::HybiDecode( cBuffer, oClient )
            ::oServer:SendData( oClient, cDecodeData )
            oClient:End()
            lRetValue := pFALSE
        else
            ::Debug( 'Data incorrectly frame. Dropping connection' )
            oClient:End()
            lRetValue := pFALSE
        endif

        nJson := hb_jsonDecode( cDecodeData, @hJson )
        if hJson != Nil
            ::HandleEvent( hJson, oClient)
        endif

        if HB_ISBLOCK( ::bOnReadProcess )
            Eval( ::bOnReadProcess, cDecodeData, oClient )
        endif
    endif

return lRetValue



METHOD UnMask() CLASS WebSocketServer

local cUnMask

    // We need 4 bytesin the masking key
    if len( ::aMaskingKey ) == 4
        // UnMasking uses the same algorithm as masking
        cUnMask := ::Mask()
    endif

return cUnMask


METHOD Mask() CLASS WebSocketServer

local aMaskingKey
local nPos
local cMask := ''


    // Do we have a valid masking key ?
    if Len( ::aMaskingKey ) == 4
        // if not, use that key
        aMaskingKey := ::aMaskingKey
    else
        // if not, generate one
        aMaskingKey := ::generateMaskingKey()
    endif

    // Mask using XOR encryption
    for nPos := 0 to Len( ::aData ) - 1
        cMask += Chr( hb_bitXor( Asc( ::aData[ nPos + 1] ), Asc( aMaskingKey[ ( nPos%4 ) + 1 ] ) ) )
    next

return cMask


METHOD generateMaskingKey() CLASS WebSocketServer

local aMaskingKey := {}
local nPos

    for nPos := 1 to 4
        aadd( aMaskingKey, Int( hb_Random( 0, 0xFF ) ) )
    next

return aMaskingKey


METHOD HybiDecode( cData, oClient ) CLASS WebSocketServer

local byte        := cData 
local nDataLength
local cMask       // := ''
local Coded_Data  // := ''
local cDecodeData // := ''
local secondByte  := Asc( SubStr( byte, 2, 1) )
local lMasked     := hb_bitAnd( 0x80, secondByte ) == 0x80


    nDataLength := hb_bitAnd( 0x7F, secondByte )

    if lMasked
        if nDataLength == 126
            cMask      := SubStr( byte, 5, 4)
            Coded_Data := SubStr( byte, 9 )
        elseif nDataLength == 127
            cMask      := SubStr( byte, 11, 4 )
            Coded_Data := SubStr( byte, 15 )
        else
            cMask      := SubStr( byte, 3, 4 )
            Coded_Data := SubStr( byte, 7 )
        endif

        ::aMaskingKey := SToA( cMask )
        ::aData       := SToA( Coded_Data )
        cDecodeData   := ::UnMask()

    else
        if nDataLength == 126
            cDecodeData := SubStr( byte, 5 )
        elseif nDataLength == 127
            cDecodeData := SubStr( byte, 11 )
        else
            cDecodeData := SubStr( byte, 3 )
        endif
    endif

    oClient:nDataLength := nDataLength

return cDecodeData


METHOD HybiEncode( cData, lBinary ) CLASS WebSocketServer

local aDataBuffer   // := {}
local nSendLenght   := Len( cData )
local nRawBytesSend := nSendLenght + 2
local cPacket       := ''
local lo
local hi
local c


    if nSendLenght > 0xFFFF
        // 64 bits
        aDataBuffer      := Array( 10 )
        aDataBuffer[ 2 ] := 127
        lo := hb_bitOr( nSendLenght, 0 )
        hi := ( nSendLenght - lo ) / 0x100000000
        aDataBuffer[ 3] := hb_bitAnd( hb_bitShift( hi, -24), 0xFF )
        aDataBuffer[ 4] := hb_bitAnd( hb_bitShift( hi, -16), 0xFF )
        aDataBuffer[ 5] := hb_bitAnd( hb_bitShift( hi, -8), 0xFF )
        aDataBuffer[ 6] := hb_bitAnd( hi, 0xFF )

        aDataBuffer[ 7] := hb_bitAnd( hb_bitShift( lo, -24), 0xFF )
        aDataBuffer[ 8] := hb_bitAnd( hb_bitShift( lo, -16), 0xFF )
        aDataBuffer[ 9] := hb_bitAnd( hb_bitShift( lo, -8), 0xFF )
        aDataBuffer[10] := hb_bitAnd( lo, 0xFF )
        nRawBytesSend += 8
    elseif nSendLenght > 0xFF
        // 16 bits
        aDataBuffer     := Array( 4 )
        aDataBuffer[ 2] := 126
        aDataBuffer[ 3] := hb_bitAnd( hb_bitShift( nSendLenght, -8), 0xFF )
        aDataBuffer[ 6] := hb_bitAnd( nSendLenght, 0xFF )
        nRawBytesSend += 2
    else
        aDataBuffer := Array( 2 )
        aDataBuffer[ 2] := nSendLenght
    endif

    aDataBuffer[1] := ( 128 + iif( lBinary, 2, 1 ) ) 

    for each c in aDataBuffer
        cPacket += Chr( c )
    next

    for each c in cData
        cPacket += c
    next

return cPacket


#ifdef __PLATAFORM__LINUX

/* METHOD PROCEDURE SignalHandler( nSignal ) CLASS WebSocketServer

    do case
        case nSignal == pSIGHUP
            QOut( 'Received SIGHUP signal' + pCRLF )

        case nSignal == pSIGKILL .or. nSignal == pSIGTERM
            QOut( 'to exit...' + pCRLF )
            ::lExit := pTRUE

        otherwise
            QOut( 'Unhandled SIGNAL ' + AllTrim( Str( nSignal ) ) + pCRLF )

    endcase

return */

PROCEDURE SignalHandler( nSignal )

local cOut := ''

    do case
        case nSignal == pSIGHUP
            cOut := 'Received SIGHUP signal' + pCRLF

        case nSignal == pSIGKILL .or. nSignal == pSIGTERM
            cOut := 'Received SIGTERM signal' + pCRLF
            FErase( pPID_FILE )

        otherwise
            cOut := 'Unhandled SIGNAL ' + AllTrim( Str( nSignal ) ) + pCRLF
            
    endcase

return

#endif


STATIC PROCEDURE doPing()

local Self := QSelf()

    ::oServer:Debug( 'HACIENDO PING' )
    ::SendData( , pTYPE_PING )

return


STATIC PROCEDURE doPong()

local Self := QSelf()


    ::oServer:Debug( 'HACIENDO PONG' )
    ::SendData( , pTYPE_PONG )

return


STATIC PROCEDURE PingPongControl( nTimeOut )

//local nSec
local Self := QSelf()


    DEFAULT nTimeOut TO 1

    //while ::nPingPongControl > ( nSec := Seconds() ) .and. ::hSocket != Nil
    while ::nPingPongControl > Seconds() .and. ::hSocket != Nil
        hb_idleSleep( nTimeOut )
    enddo

    ::doPing()
    ::oServer:StartControl( Self )

return


STATIC PROCEDURE Send( cData )

local Self       := QSelf()
local encodeData := ::oServer:HybiEncode( cData, pFALSE )

    ::oServer:SendData( Self, encodeData )

return


STATIC FUNCTION GetConText( cData, cContext )

// local nLen     := Len( cContext )
local cValue   := ''
local aLines
local aSubLine
local cRow


    aLines := hb_ATokens( cData, pCRLF )
    for each cRow in aLines
        if cContext $ cRow
            aSubLine := hb_ATokens( cRow, pCRLF )
            cValue := AllTrim( aSubLine[2] )
            exit
        endif
    next

return cValue


FUNCTION H5_GetClient()

local oClient
local o
local pThread := hb_threadSelf()

    for each o in oHost:hClients
        oClient := o
        if oClient:pThread == pThread
            exit
        endif
    next

return oClient


FUNCTION H5_HighWord( a )
    return hb_bitShift( ( a - H5_LowWord( a ) ), -16 )


FUNCTION H5_LowWord( a )
    return hb_bitAnd( a, ( hb_bitShift( 1, 16 ) -1 ) )


#pragma BEGINDUMP
#if defined( HB_OS_WIN )
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
    PHB_ITEM Self;
#endif

#include <hbapi.h>
#include <hbvm.h>
#include <hbapiitm.h>
#include <hbapierr.h>

#if defined( HB_OS_WIN )
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
        exit( EXIT_SUCESS );
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

    HB_FUNC( CLOSESTANDARFILES )
    {
        close( STDIN_FILENO );
        close( STDOUT_FILENO );
        close( STDERR_FILENO );
    }

    void CatchAlarm( int sig )
    {
        HB_SYMBOL_UNUSED( sid );
    }


    HB_FUNC( SETSIGNALALARM )
    {
        signal( SIGALARM, CatchAlarm );
    }


    HB_FUNC( SETSIGNALSHANDLER )
    {
        signal( SIGHUP, SignalHandler );
        signal( SIGTERM, SignalHandler );
        sginal( SIGINT, SignalHandler );
        sginal( SIGQUIT, SignalHandler );
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
        syslog( hb_parnl( 1 ), hbparc( 2 ), hb_parc( 3 ) );
    }


    HB_FUNC( TEST )
    {
        hb_retnl( LOG_WARNING );
    }

#endif

HB_FUNC( STOA )
{
    const char * cText = hb_parc( 1 );
    if ( HB_ISCHAR( 1 ) )
    {
        HB_SIZE nTextLength = hb_parclen( 1 );
        PHB_ITEM pOut = hb_itemArrayNew( nTextLength );
        HB_SIZE nPos;

        for ( nPos = 0; nPos < nTextLength; nPos++ )
            hb_arraySetCL( pOut, nPos+ 1, cText++, 1);

        hb_itemReturnRelease( pOut );
    } else
        hb_errRT_BASE_SubstR( EG_ARG, 1108, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#pragma ENDDUMP
    