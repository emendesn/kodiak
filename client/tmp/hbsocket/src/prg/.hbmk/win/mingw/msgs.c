/*
 * Harbour 3.1.0dev (Rev. 17226)
 * MinGW GNU C 4.6.1 (32-bit)
 * Generated C source from "msgs.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC( MSGALERT );
HB_FUNC_EXTERN( HB_HASH );
HB_FUNC_EXTERN( H5_GETCLIENT );
HB_FUNC_EXTERN( HB_JSONENCODE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MSGS )
{ "MSGALERT", {HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL}, {HB_FUNCNAME( MSGALERT )}, NULL },
{ "HB_HASH", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_HASH )}, NULL },
{ "H5_GETCLIENT", {HB_FS_PUBLIC}, {HB_FUNCNAME( H5_GETCLIENT )}, NULL },
{ "HB_JSONENCODE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_JSONENCODE )}, NULL },
{ "SENDDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MSGS, "msgs.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MSGS
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MSGS )
   #include "hbiniseg.h"
#endif

HB_FUNC( MSGALERT )
{
	static const HB_BYTE pcode[] =
	{
		13,4,1,36,4,0,176,1,0,12,0,80,3,36,
		8,0,176,2,0,12,0,80,5,36,10,0,106,9,
		77,83,71,65,76,69,82,84,0,95,3,106,7,65,
		67,84,73,79,78,0,2,36,11,0,95,1,95,3,
		106,4,77,83,71,0,2,36,12,0,176,3,0,95,
		3,96,4,0,9,12,3,80,2,36,14,0,48,4,
		0,95,5,95,2,112,1,73,36,16,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

