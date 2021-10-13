
#include "hbsocket.ch"

PROC main()
LOCAL hSrv, hConn, hPConn, cText
  IF ! HB_MTVM()
    ? "Not MT :("
  ENDIF
  hSrv := hb_socketOpen()
  hb_socketBind(hSrv, {HB_SOCKET_AF_INET, "0.0.0.0", 9876})
  hb_socketListen(hSrv)
  hb_threadDetach(hb_threadStart(@client_proc()))
  DO WHILE INKEY() != 27
    IF ! EMPTY( hConn := hb_socketAccept(hSrv,, 1) )
      hPConn := hb_psocketCreate(hConn)
      IF hb_psocketRecv(hPConn, @cText)
        DO WHILE ! hb_psocketSend(hPConn, "Hello, " + cText + "!")
        ENDDO
      ENDIF
      hb_socketShutdown(hConn)
      hb_socketClose(hConn)
    ENDIF
  ENDDO
  hb_socketClose(hSrv)
RETURN

  
PROC client_proc()
LOCAL nI, hConn, hPConn, cText
  FOR nI := 1 TO 5
    hConn := hb_socketOpen()
    IF hb_socketConnect(hConn, {HB_SOCKET_AF_INET, "127.0.0.1", 9876})
      hPConn := hb_psocketCreate(hConn)
      DO WHILE ! hb_psocketSend(hPConn, TIME())
      ENDDO
      IF hb_psocketRecv(hPConn, @cText)
        ? cText
      ENDIF
    ENDIF
    hb_socketClose(hConn)
    hb_idleSleep(2)
  NEXT
RETURN