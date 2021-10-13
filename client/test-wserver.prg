#include "hbclass.ch"

#if __HARBOUR__ >= 0x30200
 REQUEST HB_CODEPAGE_UTF8EX 
#endif

STATIC s_hFuncs, s_aClientsWithTimer, s_pMutex

PROCEDURE Main( cPort )
   LOCAL pSocket
   LOCAL nPrompt
   LOCAL nPort
   LOCAL n

#if __HARBOUR__ >= 0x30200
   HB_CdpSelect("UTF8EX")
#endif

   SetMode( 25, 80 )   
   IF cPort = NIL
      nPort := 2000
   ELSE
      nPort := Val( cPort )
   ENDIF

   s_aClientsWithTimer := { }

   ? pSocket := Listen( nPort )

   s_pMutex := HB_MutexCreate()

   IF !Empty( pSocket )
      DO WHILE .T.
         HB_MutexLock( s_pMutex )

         FOR n := 1 TO Len( s_aClientsWithTimer )

            BEGIN SEQUENCE WITH { |e| Break( e ) }

               IF Eval( s_aClientsWithTimer[ n ], Time() ) == 0 // -> nBytesSent

                  // if nothing's sent, client socket no longer available
                  // so let's remove entry from array

                  HB_ADel( s_aClientsWithTimer, n--, .T. )

               ENDIF

            RECOVER

               HB_ADel( s_aClientsWithTimer, n--, .T. )

            END SEQUENCE

         NEXT

//       AEval( s_aClientsWithTimer, { |bBlock,n| Eval( bBlock, Time() ) } )

         HB_MutexUnlock( s_pMutex )

         HB_IdleSleep( 1 )
      ENDDO
   ENDIF

FUNCTION Listen( nPort )
   RETURN AMFWS_MTServer( nPort, "0.0.0.0", s_hFuncs := { "ECHO" =>, "LISTFUNCS" =>, "TIMER" => } , .F., .T., .T., .T. )

FUNCTION Echo( x )
  RETURN x

FUNCTION ListFuncs()
  RETURN HB_HKeys( s_hFuncs )

PROCEDURE Timer()
  HB_MutexLock( s_pMutex )
  AAdd( s_aClientsWithTimer, AMFWS_CyclicResponseBlock() )
  HB_MutexUnlock( s_pMutex )
  RETURN Time()
