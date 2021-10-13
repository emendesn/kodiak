/*
* KODIAK
* (C) 2020 Edilson Mendes Nascimento <edilson.mendes.nascimento@gmail.com>
*/

#include "hbclass.ch"
#include "hbsocket.ch"
#include "fileio.ch"
#include "common.ch"
#include "error.ch"
#include "inkey.ch"
#include "hbver.ch"
#include "main.ch"


#define pCR_LF           Chr(13) + Chr(10)
#define pBUFFER_SIZE     4096


/***
*
*	CLASS hBSocket
*
*	Exibe a janela com as conexoes ativas
*
*/
CLASS hBSocket

    EXPORTED:
        DATA cBindAndress  AS CHARACTER INIT '0.0.0.0'

        DATA bDebug        AS BLOCK
        DATA bOnAccept     AS BLOCK
        DATA bOnClose      AS BLOCK
        DATA bOnListen     AS BLOCK
        DATA bOnRead       AS BLOCK
        DATA bOnWrite      AS BLOCK
        DATA bOnProccess   AS BLOCK
        DATA bOnKillClient AS BLOCK

        DATA cBuffer       AS CHARACTER
        DATA cLogFile      AS CHARACTER
        DATA cErrorLog     AS CHARACTER
        DATA cError        AS CHARACTER

        DATA hClients
        DATA hMutexUser    AS USUAL
        DATA hMutexServer  AS USUAL

        DATA nClientId     AS NUMERIC
        DATA nPort         AS NUMERIC

        DATA lDebug        AS LOGICAL
        DATA lExit         AS LOGICAL

        DATA pSocket
        DATA oPP           AS OBJECT

    METHOD New( nPort, oPP )
    METHOD End()

    METHOD KillClient( oClient )

    METHOD Listen()

    METHOD NewId()                     INLINE ::nClientId++

    METHOD OnAccept()
    METHOD OnClose()                   VIRTUAL
    METHOD OnRead( oClient )

    METHOD SendData( oCLient, cSend )

// HIDDEN:

    METHOD Debug( ... )

END CLASS


METHOD New( nPort, oPP ) CLASS hBSocket

    DEFAULT nPort TO 8080

    ::nClientId := 1
    ::nPort     := nPort
    ::hClients  := hb_Hash()
    ::cErrorLog := "error.log"

    ::lExit     := pFALSE
    ::lDebug    := pFALSE

    ::oPP       := oPP

return Self


METHOD PROCEDURE End() CLASS hBSocket

local oClient
local hClone := hb_HClone( ::hClients )

    for each oClient in hClone
        ::Debug( 'CLIENT CLOSED', oClient:hSocket, oClient:nID )
        ::KillClient( oClient )
    next

    if ::pSocket != Nil
        hb_socketClose( ::pSocket )
        ::pSocket := Nil
    endif

    if ::bOnClose != Nil
        Eval( ::bOnClose, Self )
    endif

return


METHOD PROCEDURE Debug( ... ) CLASS hBSocket

local aParams := hb_AParams()

    if ::lDebug

        if HB_ISBLOCK( ::bDebug )
            eval( ::bDebug, aParams )
        else
            AEval( aParams, { |u| QOut( u ) } )
        endif

    endif

return


METHOD PROCEDURE KillClient( oClient ) CLASS hBSocket

local nID

    if oClient:hSocket != Nil
        if HB_ISBLOCK( ::bOnKillClient )
            Eval( ::bOnKillClient, oClient )
        endif

        ::Debug( "KILLCLIENT", oClient:hSocket )

        nID := oClient:nID

        //oClient:End()
        //oClient := Nil

        hb_mutexLock( ::hMutexUser )
        hb_HDel( ::hClients, nID )
        hb_mutexUnlock( ::hMutexUser )
    endif

return


METHOD Listen() CLASS hBSocket

local lRetValue := pFALSE

    ::pSocket      := hb_socketOpen()
    ::hMutexUser   := hb_mutexCreate()
    ::hMutexServer := hb_mutexCreate()

    if hb_socketBind( ::pSocket, { HB_SOCKET_AF_INET, ::cBindAndress, ::nPort } )

        if hb_socketListen( ::pSocket )

            if HB_ISBLOCK( ::bOnListen )
                Eval( ::bOnListen, Self )
            endif

            ::Debug( "LISTEN")

            hb_threadStart( { || ::OnAccept() } )

            while .not. ::lExit

                Inkey( 0.5 )

                if ::bOnProccess != Nil
                    ::lExit := Eval( ::bOnProccess, Self)
                else
                    ::lExit := ( LastKey() == K_ESC )
                endif

            enddo

            ::End()
            lRetValue := pTRUE

        else
            QOut( ::cError := "Listen Error " + hb_ntos( hb_socketGetError() ) )
            hb_socketClose( ::pSocket )
        endif

    else
        QOut( ::cError := "Bind Error " + hb_ntos( hb_socketGetError() ) )
        hb_socketClose( ::pSocket )
    endif
    
return lRetValue


METHOD PROCEDURE OnAccept() CLASS hBSocket

local pClientSocket
local oClient

    ::Debug( "ONACCEPT" )

    while .not. ::lExit

        if .not. Empty( pClientSocket := hb_socketAccept( ::pSocket,, 500 ) )
            ::Debug( "ACCEPTED", pClientSocket )
            hb_mutexLock( ::hMutexUser )
            ::NewId()
            oClient         := HBSocketClient():New( Self )
            oClient:nID     := ::nClientId
            oClient:hSocket := pClientSocket
            hb_socketSetSndBufSize( oClient:hSocket, pBUFFER_SIZE )
            hb_socketSetRcvBufSize( oClient:hSocket, pBUFFER_SIZE )
            hb_HSet( ::hClients, ::nClientId, oClient)
            hb_mutexUnlock( ::hMutexUser )

            if ::bOnAccept != Nil
                Eval( ::bOnAccept, Self, oClient)
            endif

            hb_threadStart( { |xClient| ::OnRead( xClient ), xClient := Nil, hb_gcAll( pTRUE ) }, oClient )
            //hb_mutexNotify( Self:hMutexServer, pClientSocket )

        elseif .not. ::lExit
            // ? "Catched error :", hb_ntos( HBSocketGetError() )

        endif

    enddo

return


METHOD PROCEDURE OnRead( oClient ) CLASS hBSocket

local lMyExit := pFALSE
// local cData
local oError
local nLength := 0
//local nRetry  := 0
//local lActive := pTRUE
local cBuffer
//local hSocket

    ErrorBlock( { |o| UErrorHandler( o, Self ) } )

    oClient:pThread := hb_threadSelf()

    //hb_mutexSubscribe( Self:hMutexServer,, @hSocket)
    //if hSocket != Nil
    //    oClient:hSocket := hSocket
        oClient:TimerConnection()
        ::Debug( "CLIENT LISTEN START", oClient:hSocket )

        while .not. lMyExit .and. oClient:hSocket != Nil

            cBuffer := Space( pBUFFER_SIZE )
            begin sequence with { |oErr| UErrorHandler( oErr, Self) }
                if oClient:hSocket != Nil
                    if ( nLength := hb_socketRecv( oClient:hSocket, @cBuffer, pBUFFER_SIZE, 0, 100 ) ) > 0
                        hb_idleSleep( 0.1 )
                        oClient:cBuffer := RTrim( cBuffer )
                        oClient:nBufferLength := len( Trim( oClient:cBuffer  ) )
                    endif
                else
                    lMyExit := pTRUE
                endif
            recover using oError
                // ::Debug( oError:Description )
                lMyExit := pTRUE
            end sequence

            if lMyExit
                exit
            endif

            if nLength > 0
                ::Debug( 'ONREAD', oClient:hSocket, '<' + AllTrim( oClient:cBuffer ) + '>', Len( AllTrim( oClient:cBuffer ) ) )
            endif

            if nLength == 0
                lMyExit := pTRUE
            elseif nLength > 1
                oClient:RestartTimerConnection()
                if ::bOnRead != Nil
                    Eval( ::bOnRead, Self, oClient )
                endif
            endif

        enddo

        ::Debug( "CLIENT LISTEN FINISHED", oClient:hSocket )

    //else
    //    LogFile( ::cErrorLog, { "ERROR EN MUTEXSERVER"})

    //endif
    oClient:End()
    hb_threadJoin( oClient:pThread)

    //::KillClient( oClient )

return


METHOD SendData( oCLient, cSend ) CLASS hBSocket

local nLen

    ::Debug( "SENDING...", oclient:hSocket, cSend)

    while Len( cSend ) > 0 .and. oClient:hSocket != Nil

        if oClient:hSocket != Nil
            if ( nLen := hb_socketSend( oCLient:hSocket, @cSend ) ) == -1
                exit
            elseif nLen > 0
                cSend := SubStr( cSend, nLen+ 1)
            endif
        endif

    enddo

    if ::bOnWrite != Nil
        Eval( ::bOnWrite, self, oCLient, cSend )
    endif

return nLen



/***
*
*	CLASS hBSocketClient
*
*	Exibe a janela com as conexoes ativas
*
*/
CLASS hBSocketClient

    EXPORTED:
        DATA hSocket       AS USUAL
        DATA nID           AS NUMERIC
        DATA Cargo
        DATA oServer       AS OBJECT
        DATA cBuffer       AS CHARACTER
        DATA cOutPut       AS CHARACTER
        DATA nTimeOut      AS NUMERIC
        DATA nCurrentTime  AS NUMERIC
        DATA bOnClose      AS BLOCK
        DATA nBufferLength AS NUMERIC
        DATA pThread
        DATA lTimerOn      AS LOGICAL

    METHOD New( oServer )

    METHOD End()

    METHOD CloseConnection()           INLINE ::oServer:KillClient( Self )

    METHOD RestartTimerConnection()    INLINE ::nCurrentTime := Seconds() + ::nTimeOut

    METHOD SendData( cSend )           INLINE ::oServer:SendData( Self, cSend )
    
    METHOD TimerConnection( nTimeOut )
    METHOD TimeOut()

END CLASS


METHOD New( oServer ) CLASS hBSocketClient

    ::oServer       := oServer
    ::nTimeOut      := 0
    ::nBufferLength := 0
    ::lTimerOn      := pTRUE

return Self


METHOD PROCEDURE End() CLASS hBSocketClient

    if ::hSocket != Nil
        ::lTimerOn := pFALSE

        if HB_ISBLOCK( ::bOnClose )
            Eval( ::bOnClose, Self)
        endif

        ::oServer:Debug( "CLIENT END", ::hSocket)
        ::CloseConnection()

        if ::hSocket != Nil
            hb_socketShutdown( ::hSocket )
            hb_socketClose( ::hSocket )
            ::hSocket := Nil
        endif

    endif

return


METHOD PROCEDURE TimerConnection( nTimeOut ) CLASS hBSocketClient

    DEFAULT nTimeOut TO ::nTimeOut

    if nTimeOut > 0
        ::lTimerOn := pTRUE
        ::nTimeOut := nTimeOut

        ::nCurrentTime := Seconds() + ::nTimeOut

        hb_threadStart( { || ::TimeOut() }, Self )
    endif

return


METHOD PROCEDURE TimeOut() CLASS hBSocketClient

    while ::nCurrentTime > Seconds() .and. ::lTimerOn .and. ::hSocket != Nil
        hb_idleSleep( 1 )
    enddo

    if ::lTimerOn .and. ::hSocket != Nil
        ::oServer:Debug( "TIME OUT", ::hSocket)
        ::End()
        // ::CloseConnection()
    endif
    ::oServer:Debug( "SALIDA, " + uValToChar( ::hSocket ) + ", ", ::lTimerOn )

return


PROCEDURE LogFile( cFileName, aInfo )

local hFile
local cLine := DToC( date() ) + " " + Time() + ": "
local nPos

    cFileName := hb_DirBase() + cFileName

    for nPos := 1 to Len( aInfo )
        cLine += uValToChar( aInfo[ nPos ] ) + Chr( 9 )
    next
    cLine += pCR_LF

    if .not. File( cFileName )
        FClose( FCreate( cFileName ) )
    endif

    if ( ( hFile := FOpen( cFileName, FO_WRITE ) ) != -1 )
        FSeek( hFile, 0, FS_END )
        FWrite( hFile, cline, Len( cLine ) )
        FClose( hFile )
    endif

return


STATIC FUNCTION uValToChar( uVal )

local cRetValue
local cType     := ValType( uVal )

    do case
        case cType == "C" .or. cType == "M"
            cRetValue := uVal

        case cType == "D"
            #ifdef __XHARBOUR__
                if HasTimePart( uVal )
                    cRetValue := iif( Year( uVal ) == 0, TToC( uVal, 2 ), TToC( uVal ) )
                endif
            #endif
            cRetValue := DToC( uVal )

    #ifdef __HARBOUR__
        #ifndef __XHARBOUR__
            case cType == "T"
                cRetValue := iif( Year( uVal ) == 0, hb_TToC( uVal, '', Set( _SET_TIMEFORMAT ) ), hb_TToC( uVal ) )
        #endif
    #endif

        case cType == "L"
            cRetValue := iif( uVal, ".T.", ".F." )

        case cType == "N"
            cRetValue := hb_TSToStr( uVal )

        case cType == "B"
            cRetValue := "{ || ... }"

        case cType == "A"
            cRetValue := "{ ... }"

        case cType == "O"
            cRetValue := iif( __objHasData( uVal, "cClassName"), uVal:cClassName, uVal:ClassName() )

        case cType == "H"
            cRetValue := "{=>}"

        case cType == "P"
            #ifdef __XHARBOUR__
                cRetValue := "0x" + NumToHex( uVal )
            #else
                cRetValue := "0x" + hb_NumToHex( uVal )
            #endif

        otherwise
            cRetValue := " "

    end case

return cRetValue


FUNCTION UErrorHandler( oErr, oServer )

local lRetValue

    if     oErr:genCode == EG_ZERODIV ; lRetValue := 0
    elseif oErr:genCode == EG_LOCK ;    lRetValue := pTRUE
    elseif ( oErr:genCode == EG_OPEN .and. oErr:genCode == 32 .or. ;
            oErr:genCode == EG_APPENDLOCK ) .and. oErr:canDefault
            NetErr( pTRUE )
            lRetValue := pFALSE
    endif

    LogFile( oServer:cErrorLog, { GetErrorDesc( oErr ) } )

    if oErr != Nil
        Break( oErr )
    endif

return lRetValue


STATIC FUNCTION GetErrorDesc( oErr )

local cRetValue
local nI
local cI
local aPar
local xI
local nPos


    cRetValue := 'ERRORLOG ============================================================' + hb_eol() + ;
                'Error: ' + oErr:subsystem + '/' + ErrDescCode( oErr:genCode ) + '(' + hb_ntos( oErr:genCode ) + ') ' + hb_ntos( oErr:subcode ) + hb_eol()
    if .not. Empty( oErr:filename )
        cRetValue += 'File: ' + oErr:filename + hb_eol()
    endif

    if .not. Empty( oErr:descrition )
        cRetValue += 'Description: ' + oErr:descrition + hb_eol()
    endif

    if .not. Empty( oErr:operation )
        cRetValue += 'Operation: ' + oErr:operation + hb_eol()
    endif

    if .not. Empty( oErr:osCode )
        cRetValue += 'OS error: ' + hb_ntos( oErr:osCode ) + hb_eol()
    endif

    if HB_ISARRAY( oErr:args )
        cRetValue += 'Arguments:' + hb_eol()
        AEval( oErr:args, { |x| cRetValue += Str( x, 5) + ': ' + hb_CStr( x ) + hb_eol() })
    endif
    cRetValue += hb_eol()

    cRetValue += 'Stack:' + hb_eol()
    nI := 2

    #if 0
        while .not. Empty( ProcName( ++nI ) )
            cRetValue += '  ' + ProcName( nI ) + '(' + hb_ntos( ProcLine( nI ) ) + ')' + hb_eol()
        enddo
    #else
        while .not. Empty( ProcName( ++nI ) )
            cI := '  ' + ProcName( nI ) + '(' + hb_ntos( ProcLine( nI ) ) + ')'
            cI := PadR( cI, Max( 32, Len( cI )+ 1 ) )
            cI += '('
            aPar := __dbgVMParLList( nI )
            for nPos := 1 to Len( aPar )
                cI += cvt2str( aPar[ nPos] )
                if nPos < Len( aPar )
                    cI += ', '
                endif
            next
            cI += ')'

            nPos := Len( aPar )
            while .not. ( ValType( xI := __dbgVMVarLGet( nI, ++nPos ) ) == 'S')
                cI += ', ' + cvt2str( xI )
            enddo

            xI := Nil
        enddo
    #endif

    cRetValue += hb_eol()

    cRetValue += 'Executable: ' + hb_ProgName() + hb_eol()
    cRetValue += 'Version: ' + hb_eol()
    cRetValue += ' OS: ' + OS() + hb_eol()
    cRetValue += ' Harbour build Date: ' + hb_Version( HB_VERSION_BUILD_DATE_STR ) + hb_eol()
    cRetValue += ' Harbour Revisao: ' + hb_Version( HB_VERSION_RELEASE  ) + hb_eol()
    cRetValue += ' Harbour Status: ' + hb_Version( HB_VERSION_STATUS ) + hb_eol()
    cRetValue += hb_eol()

    if oErr:genCode != EG_MEM
        cRetValue += 'Database Areas: ' + hb_eol()
        cRetValue += ' Current: ' + hb_ntos( Select() ) + ' ' + Alias() + hb_eol()

        begin sequence with { |o| Break( o ) }
            if Used()
                cRetValue += ' Filter: ' + dbFilter() + hb_eol()
                cRetValue += ' Relation: ' + dbRelation() + hb_eol()
                cRetValue += 'Index Expression: ' + ordKey( ordSetFocus() ) + hb_eol()
                cRetValue += hb_eol()

                begin sequence with { |o| Break( o ) }
                    for nI := 1 to FCount()
                        cRetValue += Str( nI, 6) + ' ' + PadR( FieldName( nI ), 14 ) + ': ' + hb_ValToExp( FieldGet( nI ) ) + hb_eol()
                    next
                recover
                    cRetValue += 'Error reading database fields !!!' + hb_eol()
                end sequence
                cRetValue += hb_eol()
            endif
        recover
            cRetValue += 'Erro accessing current workarea !!!' + hb_eol()
        end sequence

        for nI := 1 to 250
            begin sequence with { |o| Break( o ) }
                if Used()
                    dbSelectArea( nI )
                    cRetValue += Str( nI, 6 ) + ' ' + rddName() + ' ' + PadR( Alias(), 15) + Str( RecNo() ) + '/' + Str( LastRec() ) + ;
                                iif( Empty( ordSetFocus() ), '', ' Index ' + ordSetFocus() + '(' + hb_ntos( ordNumber() ) + ')' ) + hb_eol() 
                    dbCloseArea()
                endif
            recover
                cRetValue += '!!! Error accessing workarea number: ' + Str( nI, 4) + '!!!' + hb_eol()
            end sequence
        next
        cRetValue += hb_eol()
    endif

return cRetValue


STATIC FUNCTION ErrDescCode( nCode )

local cRetValue

    if nCode > 0 .and. nCode <= 41
        cRetvalue := {  "ARG"     , "BOUND"    , "STROVERFLOW", "NUMOVERFLOW", "ZERODIV" , "NUMERR"     , "SYNTAX"  , "COMPLEXITY" , ; //  1,  2,  3,  4,  5,  6,  7,  8
                        NIL       , NIL        , "MEM"        , "NOFUNC"     , "NOMETHOD", "NOVAR"      , "NOALIAS" , "NOVARMETHOD", ; //  9, 10, 11, 12, 13, 14, 15, 16
                        "BADALIAS", "DUPALIAS" , NIL          , "CREATE"     , "OPEN"    , "CLOSE"      , "READ"    , "WRITE"      , ; // 17, 18, 19, 20, 21, 22, 23, 24
                        "PRINT"   , NIL        , NIL          , NIL          , NIL       , "UNSUPPORTED", "LIMIT"   , "CORRUPTION" , ; // 25, 26 - 29, 30, 31, 32
                        "DATATYPE", "DATAWIDTH", "NOTABLE"    , "NOORDER"    , "SHARED"  , "UNLOCKED"   , "READONLY", "APPENDLOCK" , ; // 33, 34, 35, 36, 37, 38, 39, 40
                        "LOCK"    }[nCode]
    endif

return iif( cRetValue == Nil, '', 'EG_' + cRetValue )


STATIC FUNCTION cvt2str( xI, lLong )

local cValType := ValType( xI )
local cRetValue
local cI
local xJ

    lLong := .not. Empty( lLong )
    if cValType == 'U'
        cRetvalue := iif( lLong, '[U]:Nil', 'Nil')

    elseif cValType == 'N'
        cRetvalue := iif( lLong, '[U]' + Str( xI), hb_ntos( xI ) )

    elseif cRetValue == 'CM'
        if Len( xI ) <= 200
            cRetValue := iif( lLong, '[' + cValType + hb_ntos( len( xI ) ) + ']:', '' ) + '"' + xI + '"'
        else
            cRetValue := iif( lLong, '[' + cValType + hb_ntos( Len( xI ) ) + ']:', '' ) + '"' + Left( xI, 100 ) + '"...'
        endif

    elseif cValType == 'A'
        cRetValue := '[A' + hb_ntos( Len( xI ) ) + ']'

    elseif cValType == 'H'
        cRetValue := '[H' + hb_ntos( Len( xI ) ) + ']'

    elseif cValType == 'O'
        cI := ''
        if __objHasMsg( xI, 'ID')
            xJ := xI:ID
            if .not. HB_ISOBJECT( xJ )
                cI += ', ID=' + cvt2str( xJ )
            endif
        endif
        if __objHasMsg( xI, 'nID' )
            xJ := xI:nID
            if .not. HB_ISOBJECT( xJ )
                cI += ', NID=' + cvt2str( xJ )
            endif
        endif
        if __objHasMsg( xI, 'xValue' )
            xJ := xI:xValue
            if .not. HB_ISOBJECT( xJ )
                cI += ', XVALUE=' + cvt2str( xJ )
            endif
        endif
        cRetValue := '[O:' + xI:ClassName + cI + ']'
    
    elseif cValType == 'D'
        cRetValue := iif( lLong, '[D]:', '') + DToC( xI )

    elseif cValType == 'L'
        cRetValue := iif( lLong, '[L]:', '') + iif( xI, '.T.', '.F.')

    elseif cValType == 'P'
        cRetValue := iif( llong, '[P]:', '') + '0p' + hb_NumToHex( xI )

    else
        cRetValue := '[' + cValType + ']'
    endif

return cRetValue


/* STATIC FUNCTION TStr( n )
    return AllTrim( Str( n ) ) */



#pragma BEGINDUMP
#if defined( HB_OS_WIN )

    #include 'hbapi.h'
    #include 'hbwapi.h'
    #include 'hbvm.h'
    #include 'hpapiim.h'

    void * pfnCallBack_OnRead;

    void set_callback_OnRead( void * p )
    {
        void * pfnCallBack_OnRead = p;
    }


    HB_FUNC( SET_CALLBACK_ONREAD )
    {
        set_callback_OnRead( ( void * ) hb_parptr( 1 ) );
    }


    HB_FUNC( HSOCKET_CALLBACKONREAD )
    {
        PHB_ITEM pserver = hb_param( 1, HB_IT_OBJECT );
        PHB_ITEM pclient = hb_param( 2, HB_IT_OBJECT );

        if ( pfnCallBack_OnRead )
        {
            void ( *pfn )( PHB_ITEM, PHB_ITEM );
                pfn = pfnCallBack_OnRead;
                pfn( pserver, pclient );
        }
    }



    HB_FUNC( WIN_SERVICEINITIATE )
    {
        HB_BOOL pRetVal = HB_FALSE;
        #if ! define( HB_OS_WIN_CE )
            // Open a handle to the SCM
            SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );

            if ( schSCM )
            {
                // Open a Handle to the service
                void * hServiceName;

                SC_HANDLE schSrv = OpenService( schSCM, HB_PARSTRDEF( 1, &hServiceName, NULL ), GENERIC_EXECUTE );
                if ( schSrv )
                {
                    if ( ! ( bRetVal = StartService( schSCM, 0 , NULL ) ) )
                        hbwapi_SetLastError( GetLastError() );

                } else
                        hbwapi_SetLastError( GetLastError() );

                hb_strfree( hServiceName );

                CloseServiceHandle( schSrv );
                CloseServiceHandle( schSCM );

            } else
                hbwapi_SetLastError( GetLastError() );

        #endif

        hb_retl( bRetVal );

    }


    HB_FUNC( WIN_SERVICESTOP )
    {
        HB_BOOL bRetVal = HB_FALSE;

        #if !defined( HB_OS_WIN_CE )

            // Open a handle to the SCM
            SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );

            if ( schSCM )
            {
                void * hServiceName;

                // Open a handle to the service
                SC_HANDLE schSrv = OpenService( schSCM, HB_PARSTRDEF( 1, &hServiceName, NULL ), GENERIC_EXECUTE );
                if ( schSrv )
                {
                    // Send the STOP control request to the service
                    SERVICE_STATUS status;
                    ControlService( schSrv, SERVICE_CONTROL_STOP, &status);
                    CloseServiceHandle( schSrv );
                    CloseServiceHandle( schSCM );

                    if ( ( bRetVal = statis.dwCurrentState != SERVICE_STOP ) )
                        hbwapi_SetLastError( GetLastError() );

                    hb_strfree( hServiceName );

                } else
                    hbwapi_SetLastError( GetLastError() );

            } else 
                hbwapi_SetLastError( GetLastError() );
        #endif
        hb_retl( bRetVal );
    }

#endif
#pragma ENDDUMP