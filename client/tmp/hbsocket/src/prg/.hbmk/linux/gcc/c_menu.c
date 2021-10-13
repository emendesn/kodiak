/* C source generated by Harbour */

#include "hbvmpub.h"
#include "hbinit.h"

HB_FUNC( C_MENU );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( C_OBJECT );
HB_FUNC_STATIC( C_MENU_NEW );
HB_FUNC_STATIC( C_MENU_END );
HB_FUNC_STATIC( C_MENU_ADDMENU );
HB_FUNC_STATIC( C_MENU_ADDMENUITEM );
HB_FUNC_STATIC( C_MENU_HANDLEEVENT );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( HB_HASH );
HB_FUNC_EXTERN( HB_HDEL );
HB_FUNC( C_MENUITEM );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( HBOBJECT );
HB_FUNC_STATIC( C_MENUITEM_NEW );
HB_FUNC_STATIC( C_MENUITEM_ADDMENUITEM );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( STRZERO );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_STATIC( C_MENUITEM_MAKEPARENT );
HB_FUNC_STATIC( H5_GETLASTMENU );
HB_FUNC_EXTERN( ATAIL );
HB_FUNC( H5_ENDMENU );
HB_FUNC_STATIC( H5_GETLASTPARENT );
HB_FUNC( H5_ADDMENUITEM );
HB_FUNC( H5_BEGINMENU );
HB_FUNC_STATIC( H5_GETLASTITEM );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( HB_HVALUEAT );
HB_FUNC( H5_GETMAINMENU );
HB_FUNC_INITSTATICS();

HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_C_MENU )
{ "C_MENU", { HB_FS_PUBLIC | HB_FS_FIRST | HB_FS_LOCAL }, { HB_FUNCNAME( C_MENU ) }, NULL },
{ "__CLSLOCKDEF", { HB_FS_PUBLIC }, { HB_FUNCNAME( __CLSLOCKDEF ) }, NULL },
{ "NEW", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "HBCLASS", { HB_FS_PUBLIC }, { HB_FUNCNAME( HBCLASS ) }, NULL },
{ "C_OBJECT", { HB_FS_PUBLIC }, { HB_FUNCNAME( C_OBJECT ) }, NULL },
{ "ADDMULTICLSDATA", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "ADDMULTIDATA", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "ADDMETHOD", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "C_MENU_NEW", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( C_MENU_NEW ) }, NULL },
{ "C_MENU_END", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( C_MENU_END ) }, NULL },
{ "C_MENU_ADDMENU", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( C_MENU_ADDMENU ) }, NULL },
{ "C_MENU_ADDMENUITEM", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( C_MENU_ADDMENUITEM ) }, NULL },
{ "C_MENU_HANDLEEVENT", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( C_MENU_HANDLEEVENT ) }, NULL },
{ "CREATE", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "__CLSUNLOCKDEF", { HB_FS_PUBLIC }, { HB_FUNCNAME( __CLSUNLOCKDEF ) }, NULL },
{ "INSTANCE", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "__OBJHASMSG", { HB_FS_PUBLIC }, { HB_FUNCNAME( __OBJHASMSG ) }, NULL },
{ "INITCLASS", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "GETID", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "_HITEMS", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "HB_HASH", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_HASH ) }, NULL },
{ "_LCOMPLETED", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "_HJSON", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "HJSON", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "CID", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "_PTRITEMS", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "HB_HDEL", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_HDEL ) }, NULL },
{ "_CPROMPT", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "LCOMPLETED", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "C_MENUITEM", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( C_MENUITEM ) }, NULL },
{ "CPROMPT", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "HITEMS", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "NID", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "AADD", { HB_FS_PUBLIC }, { HB_FUNCNAME( AADD ) }, NULL },
{ "PTRITEMS", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "HB_ISBLOCK", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_ISBLOCK ) }, NULL },
{ "BACTION", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "EVAL", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "HBOBJECT", { HB_FS_PUBLIC }, { HB_FUNCNAME( HBOBJECT ) }, NULL },
{ "C_MENUITEM_NEW", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( C_MENUITEM_NEW ) }, NULL },
{ "C_MENUITEM_ADDMENUITEM", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( C_MENUITEM_ADDMENUITEM ) }, NULL },
{ "ADDINLINE", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "_CID", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "ALLTRIM", { HB_FS_PUBLIC }, { HB_FUNCNAME( ALLTRIM ) }, NULL },
{ "STRZERO", { HB_FS_PUBLIC }, { HB_FUNCNAME( STRZERO ) }, NULL },
{ "_NID", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "VALTYPE", { HB_FS_PUBLIC }, { HB_FUNCNAME( VALTYPE ) }, NULL },
{ "C_MENUITEM_MAKEPARENT", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( C_MENUITEM_MAKEPARENT ) }, NULL },
{ "_LCHECKED", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "_LENABLE", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "_LSELECTED", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "_OPARENT", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "_BACTION", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "_OROOT", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "H5_GETLASTMENU", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( H5_GETLASTMENU ) }, NULL },
{ "OROOT", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "ISKINDOF", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "OPARENT", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "ATAIL", { HB_FS_PUBLIC }, { HB_FUNCNAME( ATAIL ) }, NULL },
{ "H5_ENDMENU", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( H5_ENDMENU ) }, NULL },
{ "H5_GETLASTPARENT", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( H5_GETLASTPARENT ) }, NULL },
{ "H5_ADDMENUITEM", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( H5_ADDMENUITEM ) }, NULL },
{ "ADDMENUITEM", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "H5_BEGINMENU", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( H5_BEGINMENU ) }, NULL },
{ "H5_GETLASTITEM", { HB_FS_STATIC | HB_FS_LOCAL }, { HB_FUNCNAME( H5_GETLASTITEM ) }, NULL },
{ "MAKEPARENT", { HB_FS_PUBLIC | HB_FS_MESSAGE }, { NULL }, NULL },
{ "LEN", { HB_FS_PUBLIC }, { HB_FUNCNAME( LEN ) }, NULL },
{ "HB_HVALUEAT", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_HVALUEAT ) }, NULL },
{ "H5_GETMAINMENU", { HB_FS_PUBLIC | HB_FS_LOCAL }, { HB_FUNCNAME( H5_GETMAINMENU ) }, NULL },
{ "(_INITSTATICS00003)", { HB_FS_INITEXIT | HB_FS_LOCAL }, { hb_INITSTATICS }, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_C_MENU, "", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_C_MENU
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_C_MENU )
   #include "hbiniseg.h"
#endif

HB_FUNC( C_MENU )
{
	static const HB_BYTE pcode[] =
	{
		149,3,0,116,69,0,36,5,0,103,2,0,100,8,29,228,1,176,1,
		0,104,2,0,12,1,29,217,1,166,155,1,0,122,80,1,48,2,0,
		176,3,0,12,0,106,7,67,95,77,101,110,117,0,108,4,4,1,0,
		108,0,112,3,80,2,36,7,0,48,5,0,95,2,100,121,95,1,121,
		72,92,32,72,121,72,121,72,106,4,110,73,68,0,4,1,0,9,112,
		5,73,36,9,0,48,6,0,95,2,100,100,95,1,121,72,121,72,121,
		72,106,8,99,80,114,111,109,112,116,0,4,1,0,9,112,5,73,36,
		11,0,48,6,0,95,2,100,100,95,1,121,72,121,72,121,72,106,7,
		104,73,116,101,109,115,0,4,1,0,9,112,5,73,36,12,0,48,6,
		0,95,2,100,100,95,1,121,72,121,72,121,72,106,6,104,74,83,111,
		110,0,4,1,0,9,112,5,73,36,13,0,48,6,0,95,2,100,100,
		95,1,121,72,121,72,121,72,106,9,112,116,114,73,116,101,109,115,0,
		4,1,0,9,112,5,73,36,15,0,48,6,0,95,2,100,100,95,1,
		121,72,121,72,121,72,106,11,108,67,111,109,112,108,101,116,101,100,0,
		4,1,0,9,112,5,73,36,17,0,48,7,0,95,2,106,4,78,101,
		119,0,108,8,95,1,121,72,121,72,121,72,112,3,73,36,18,0,48,
		7,0,95,2,106,4,69,110,100,0,108,9,95,1,121,72,121,72,121,
		72,112,3,73,36,20,0,48,7,0,95,2,106,8,65,100,100,77,101,
		110,117,0,108,10,95,1,121,72,121,72,121,72,112,3,73,36,21,0,
		48,7,0,95,2,106,12,65,100,100,77,101,110,117,73,116,101,109,0,
		108,11,95,1,121,72,121,72,121,72,112,3,73,36,23,0,48,7,0,
		95,2,106,12,72,97,110,100,108,101,69,118,101,110,116,0,108,12,95,
		1,121,72,121,72,121,72,112,3,73,36,26,0,48,13,0,95,2,112,
		0,73,167,14,0,0,176,14,0,104,2,0,95,2,20,2,168,48,15,
		0,95,2,112,0,80,3,176,16,0,95,3,106,10,73,110,105,116,67,
		108,97,115,115,0,12,2,28,12,48,17,0,95,3,164,146,1,0,73,
		95,3,110,7,48,15,0,103,2,0,112,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( C_MENU_NEW )
{
	static const HB_BYTE pcode[] =
	{
		36,32,0,48,18,0,102,106,4,77,78,85,0,112,1,73,36,33,0,
		48,19,0,102,176,20,0,12,0,112,1,73,36,34,0,48,21,0,102,
		9,112,1,73,36,35,0,48,22,0,102,176,20,0,12,0,112,1,73,
		36,36,0,4,0,0,48,23,0,102,112,0,106,6,105,116,101,109,115,
		0,2,36,37,0,48,24,0,102,112,0,48,23,0,102,112,0,106,3,
		73,68,0,2,36,38,0,106,11,67,82,69,65,84,69,77,69,78,85,
		0,48,23,0,102,112,0,106,7,65,67,84,73,79,78,0,2,36,39,
		0,48,25,0,102,176,20,0,12,0,112,1,73,36,41,0,102,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( C_MENU_END )
{
	static const HB_BYTE pcode[] =
	{
		116,69,0,36,47,0,176,26,0,103,1,0,48,24,0,102,112,0,20,
		2,36,49,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( C_MENU_ADDMENU )
{
	static const HB_BYTE pcode[] =
	{
		13,1,1,36,57,0,48,2,0,176,0,0,12,0,112,0,80,2,36,
		58,0,48,27,0,95,2,95,1,112,1,73,36,60,0,95,2,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( C_MENU_ADDMENUITEM )
{
	static const HB_BYTE pcode[] =
	{
		13,2,2,36,69,0,48,28,0,102,112,0,31,119,36,70,0,48,2,
		0,176,29,0,12,0,102,95,1,95,2,112,3,80,3,36,72,0,106,
		5,116,101,120,116,0,48,30,0,95,3,112,0,106,3,105,100,0,48,
		24,0,95,3,112,0,177,2,0,80,4,36,73,0,95,3,48,31,0,
		102,112,0,48,32,0,95,3,112,0,2,36,75,0,176,33,0,48,23,
		0,102,112,0,106,6,105,116,101,109,115,0,1,95,4,20,2,36,76,
		0,95,3,48,34,0,102,112,0,48,24,0,95,3,112,0,2,36,80,
		0,95,3,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( C_MENU_HANDLEEVENT )
{
	static const HB_BYTE pcode[] =
	{
		13,1,3,36,88,0,25,53,36,90,0,48,34,0,102,112,0,95,2,
		1,80,4,36,91,0,176,35,0,48,36,0,95,4,112,0,12,1,28,
		20,36,92,0,48,37,0,48,36,0,95,4,112,0,95,4,112,1,73,
		25,14,95,1,133,1,0,97,2,0,0,0,25,195,36,98,0,100,110,
		7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( C_MENUITEM )
{
	static const HB_BYTE pcode[] =
	{
		149,3,0,116,69,0,36,101,0,103,3,0,100,8,29,255,2,176,1,
		0,104,3,0,12,1,29,244,2,166,182,2,0,122,80,1,48,2,0,
		176,3,0,12,0,106,11,67,95,77,101,110,117,73,116,101,109,0,108,
		38,4,1,0,108,29,112,3,80,2,36,103,0,48,5,0,95,2,100,
		121,95,1,121,72,92,32,72,121,72,121,72,106,4,110,73,68,0,4,
		1,0,9,112,5,73,36,105,0,48,6,0,95,2,100,100,95,1,121,
		72,121,72,121,72,106,8,98,65,99,116,105,111,110,0,4,1,0,9,
		112,5,73,36,107,0,48,6,0,95,2,100,100,95,1,121,72,121,72,
		121,72,106,4,99,73,68,0,4,1,0,9,112,5,73,36,109,0,48,
		6,0,95,2,100,100,95,1,121,72,121,72,121,72,106,8,99,80,114,
		111,109,112,116,0,4,1,0,9,112,5,73,36,111,0,48,6,0,95,
		2,100,100,95,1,121,72,121,72,121,72,106,7,104,73,116,101,109,115,
		0,4,1,0,9,112,5,73,36,112,0,48,6,0,95,2,100,100,95,
		1,121,72,121,72,121,72,106,6,104,74,83,111,110,0,4,1,0,9,
		112,5,73,36,114,0,48,6,0,95,2,100,100,95,1,121,72,121,72,
		121,72,106,9,108,67,104,101,99,107,101,100,0,4,1,0,9,112,5,
		73,36,115,0,48,6,0,95,2,100,100,95,1,121,72,121,72,121,72,
		106,8,108,69,110,97,98,108,101,0,4,1,0,9,112,5,73,36,116,
		0,48,6,0,95,2,100,100,95,1,121,72,121,72,121,72,106,10,108,
		83,101,108,101,99,116,101,100,0,4,1,0,9,112,5,73,36,117,0,
		48,6,0,95,2,100,100,95,1,121,72,121,72,121,72,106,11,108,67,
		111,109,112,108,101,116,101,100,0,4,1,0,9,112,5,73,36,119,0,
		48,6,0,95,2,100,100,95,1,121,72,121,72,121,72,106,8,111,80,
		97,114,101,110,116,0,4,1,0,9,112,5,73,36,120,0,48,6,0,
		95,2,100,100,95,1,121,72,121,72,121,72,106,6,111,82,111,111,116,
		0,4,1,0,9,112,5,73,36,122,0,48,7,0,95,2,106,4,78,
		101,119,0,108,39,95,1,121,72,121,72,121,72,112,3,73,36,124,0,
		48,7,0,95,2,106,12,65,100,100,77,101,110,117,73,116,101,109,0,
		108,40,95,1,121,72,121,72,121,72,112,3,73,36,126,0,48,41,0,
		95,2,106,6,71,101,116,73,100,0,89,50,0,1,0,0,0,48,42,
		0,95,1,106,4,73,84,77,0,176,43,0,176,44,0,48,45,0,95,
		1,21,48,32,0,163,0,112,0,23,112,1,92,8,12,2,12,1,72,
		112,1,6,95,1,121,72,121,72,121,72,112,3,73,36,128,0,48,41,
		0,95,2,106,9,105,115,80,97,114,101,110,116,0,89,25,0,1,0,
		0,0,176,46,0,48,31,0,95,1,112,0,12,1,106,2,72,0,8,
		6,95,1,121,72,121,72,121,72,112,3,73,36,130,0,48,7,0,95,
		2,106,11,77,97,107,101,80,97,114,101,110,116,0,108,47,95,1,121,
		72,121,72,121,72,112,3,73,36,133,0,48,13,0,95,2,112,0,73,
		167,14,0,0,176,14,0,104,3,0,95,2,20,2,168,48,15,0,95,
		2,112,0,80,3,176,16,0,95,3,106,10,73,110,105,116,67,108,97,
		115,115,0,12,2,28,12,48,17,0,95,3,164,146,1,0,73,95,3,
		110,7,48,15,0,103,3,0,112,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( C_MENUITEM_NEW )
{
	static const HB_BYTE pcode[] =
	{
		13,0,6,36,139,0,95,4,100,8,28,5,9,80,4,36,140,0,95,
		5,100,8,28,5,120,80,5,36,141,0,95,6,100,8,28,5,9,80,
		6,36,143,0,48,48,0,102,95,4,112,1,73,36,144,0,48,49,0,
		102,95,5,112,1,73,36,145,0,48,50,0,102,95,6,112,1,73,36,
		146,0,48,27,0,102,95,2,112,1,73,36,147,0,48,51,0,102,95,
		1,112,1,73,36,148,0,48,52,0,102,95,3,112,1,73,36,149,0,
		48,18,0,102,112,0,73,36,150,0,48,21,0,102,9,112,1,73,36,
		151,0,48,53,0,102,176,54,0,12,0,112,1,73,36,153,0,102,110,
		7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( C_MENUITEM_ADDMENUITEM )
{
	static const HB_BYTE pcode[] =
	{
		13,2,2,36,163,0,48,28,0,102,112,0,32,139,0,36,165,0,48,
		2,0,176,29,0,12,0,102,95,1,95,2,112,3,80,3,36,167,0,
		95,3,48,31,0,102,112,0,48,24,0,95,3,112,0,2,36,168,0,
		106,5,116,101,120,116,0,48,30,0,95,3,112,0,106,3,105,100,0,
		48,24,0,95,3,112,0,177,2,0,80,4,36,169,0,176,33,0,48,
		23,0,102,112,0,106,8,115,117,98,109,101,110,117,0,1,106,9,105,
		116,101,109,100,97,116,97,0,1,95,4,20,2,36,170,0,95,3,48,
		34,0,48,55,0,102,112,0,112,0,48,24,0,95,3,112,0,2,36,
		174,0,95,3,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( C_MENUITEM_MAKEPARENT )
{
	static const HB_BYTE pcode[] =
	{
		36,182,0,48,19,0,102,176,20,0,12,0,112,1,73,36,184,0,48,
		56,0,48,57,0,102,112,0,106,7,67,95,77,69,78,85,0,112,1,
		28,39,36,185,0,48,22,0,102,176,58,0,48,23,0,48,55,0,102,
		112,0,112,0,106,6,105,116,101,109,115,0,1,12,1,112,1,73,25,
		51,36,187,0,48,22,0,102,176,58,0,48,23,0,48,57,0,102,112,
		0,112,0,106,8,115,117,98,109,101,110,117,0,1,106,9,105,116,101,
		109,100,97,116,97,0,1,12,1,112,1,73,36,190,0,106,3,105,100,
		0,106,4,83,85,66,0,48,24,0,102,112,0,72,106,9,105,116,101,
		109,100,97,116,97,0,4,0,0,177,2,0,48,23,0,102,112,0,106,
		8,115,117,98,109,101,110,117,0,2,36,192,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( H5_ENDMENU )
{
	static const HB_BYTE pcode[] =
	{
		13,2,0,36,201,0,176,54,0,12,0,80,1,36,202,0,176,60,0,
		95,1,12,1,80,2,36,204,0,48,21,0,95,2,120,112,1,73,36,
		206,0,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( H5_ADDMENUITEM )
{
	static const HB_BYTE pcode[] =
	{
		13,3,3,36,214,0,176,54,0,12,0,80,4,36,215,0,176,60,0,
		95,4,12,1,80,5,36,217,0,95,1,100,8,28,6,95,5,80,1,
		36,219,0,48,62,0,95,1,95,2,95,3,112,2,80,6,36,221,0,
		95,6,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( H5_BEGINMENU )
{
	static const HB_BYTE pcode[] =
	{
		13,2,1,116,69,0,36,229,0,95,1,100,8,28,5,9,80,1,36,
		231,0,176,54,0,12,0,80,2,36,233,0,95,2,100,69,28,40,95,
		1,31,36,36,235,0,176,64,0,95,2,12,1,80,3,36,237,0,95,
		3,100,69,28,47,36,238,0,48,65,0,95,3,112,0,73,25,34,36,
		243,0,48,2,0,176,0,0,12,0,120,112,1,80,2,36,244,0,95,
		2,103,1,0,48,24,0,95,2,112,0,2,36,248,0,95,2,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( H5_GETLASTPARENT )
{
	static const HB_BYTE pcode[] =
	{
		13,2,1,36,0,1,176,46,0,48,31,0,95,1,112,0,12,1,106,
		2,72,0,8,28,89,48,28,0,95,1,112,0,31,80,36,1,1,176,
		66,0,48,31,0,95,1,112,0,12,1,80,3,36,3,1,95,3,121,
		15,28,45,36,4,1,176,67,0,48,31,0,95,1,112,0,176,66,0,
		48,31,0,95,1,112,0,12,1,12,2,80,2,36,5,1,176,60,0,
		95,2,12,1,80,2,25,23,36,7,1,95,1,80,2,25,14,36,10,
		1,48,57,0,95,1,112,0,80,2,36,13,1,95,2,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( H5_GETLASTITEM )
{
	static const HB_BYTE pcode[] =
	{
		13,2,1,36,21,1,176,46,0,48,31,0,95,1,112,0,12,1,106,
		2,72,0,8,28,80,36,22,1,176,66,0,48,31,0,95,1,112,0,
		12,1,80,3,36,23,1,95,3,121,15,28,45,36,24,1,176,67,0,
		48,31,0,95,1,112,0,176,66,0,48,31,0,95,1,112,0,12,1,
		12,2,80,2,36,25,1,176,64,0,95,2,12,1,80,2,25,18,36,
		27,1,95,1,80,2,25,9,36,30,1,95,1,80,2,36,33,1,95,
		2,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_STATIC( H5_GETLASTMENU )
{
	static const HB_BYTE pcode[] =
	{
		13,2,0,116,69,0,36,40,1,176,66,0,103,1,0,12,1,80,2,
		36,42,1,95,2,121,15,28,17,36,43,1,176,67,0,103,1,0,95,
		2,12,2,80,1,36,46,1,95,1,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( H5_GETMAINMENU )
{
	static const HB_BYTE pcode[] =
	{
		116,69,0,36,52,1,103,1,0,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC_INITSTATICS()
{
	static const HB_BYTE pcode[] =
	{
		117,69,0,3,0,116,69,0,177,0,0,82,1,0,7
	};

	hb_vmExecute( pcode, symbols );
}
