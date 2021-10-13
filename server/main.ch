/***
*
*  MAIN.CH
*
***/

/***
*
*  Constante Logiscas
*
***/
#define pTRUE                   .T.
#define pFALSE                  .F.


/***
*
*  Constante Variaveis Internas
*
***/
#xtranslate pCLR_MESSAGE        => aKodiakSystem\[ 1\]
#xtranslate pCLR_BACKGROUND     => aKodiakSystem\[ 2\]
#xtranslate pCLR_PUSHBUTTON     => aKodiakSystem\[ 3\]
#xtranslate pCLR_MENU           => aKodiakSystem\[ 4\]
#xtranslate pCLR_FORM           => aKodiakSystem\[ 5\]
#xtranslate pCLR_LABEL          => aKodiakSystem\[ 6\]
#xtranslate pCLR_SCROLLBAR      => aKodiakSystem\[ 7\]
#xtranslate pCLR_FIELDGET       => aKodiakSystem\[ 8\]
#xtranslate pCLR_FIELDLISTBOX   => aKodiakSystem\[ 9\]
#xtranslate pCLR_FIELDGRADIOBOX => aKodiakSystem\[10\]
#xtranslate pCLR_FIELDBRADIOBOX => aKodiakSystem\[11\]
#xtranslate pCLR_FIELDCHECKBOX  => aKodiakSystem\[12\]
#xtranslate pCLR_BROWSE         => aKodiakSystem\[13\]

#xtranslate pMNU_MAINMENU       => aKodiakSystem\[14\]

#xtranslate pSCR_ROWTOP         => aKodiakSystem\[15\]
#xtranslate pSCR_COLTOP         => aKodiakSystem\[16\]
#xtranslate pSCR_ROWBOTTOM      => aKodiakSystem\[17\]
#xtranslate pSCR_COLBOTTOM      => aKodiakSystem\[18\]
#xtranslate pSCR_BANNER         => aKodiakSystem\[19\]

#xtranslate pInitKodiak         => ( aKodiakSystem := ARRAY( 19 ) )


/***
*
*  Constante para os key do browse
*
***/
#define pBRW_KEYS         { { K_DOWN      , {|obj| obj:down()}     },;
                            { K_UP        , {|obj| obj:up()}       },;
                            { K_PGDN      , {|obj| obj:pageDown()} },;
                            { K_PGUP      , {|obj| obj:pageUp()}   },;
                            { K_CTRL_PGUP , {|obj| obj:goTop()}    },;
                            { K_CTRL_PGDN , {|obj| obj:goBottom()} },;
                            { K_HOME      , {|obj| obj:home()}     },;
                            { K_END       , {|obj| obj:end()}      },;
                            { K_CTRL_LEFT , {|obj| obj:panLeft()}  },;
                            { K_CTRL_RIGHT, {|obj| obj:panRight()} },;
                            { K_CTRL_HOME , {|obj| obj:panHome()}  },;
                            { K_CTRL_END  , {|obj| obj:panEnd()}   } }