
/*
 *  S H p a s s 1 . C  --  S2H compiler:  Pass 1 functions
 *
 *  03.01.2000 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# include   "shcommon.h"
# include   "y.tab.h"
/*-------------------------------------------------------------------------*/

static char linebuffer[maxINPLINELEN]; /* for temporary strings */

/*-------------------------------------------------------------------------*/
/* types of nodes: */
char  nodnameGND[]      = "0";  /* name of GND-node in Spice */
char  nodtypUNKNOWN[]   = "";
char  nodtypPLUS[]      = "+";
char  nodtypMINUS[]     = "-";
char  nodtypCTRLPLUS[]  = "ctr+";
char  nodtypCTRLMINUS[] = "ctr-";
char  nodtypBjtC[]      = "bjtC";
char  nodtypBjtB[]      = "bjtB";
char  nodtypBjtE[]      = "bjtE";
char  nodtypBjtS[]      = "bjtS";
char  nodtypFetD[]      = "fetD";
char  nodtypFetG[]      = "fetG";
char  nodtypFetS[]      = "fetS";
char  nodtypFetB[]      = "fetB";
char  nodtypSubCirc[]   = "parm";

/*-------------------------------------------------------------------------*/
/* counters: */
long  cntNodesMain = 0L; /* nodes in main circuit */
long  cntSubCirc   = 0L; /* subcircuits */
long  cntNodesTmp;       /* temporary nodes count for AddNodeInfo() */
long  cntModels    = 0L; /* models definitions counter */
long  renNodes     = startRENNODES; /* rename counter for nodes */
/* . . . */

/* pointers to lists: */
FileList  *listFiles     = NULL; /* list of input files names */
Node      *listNodesMain = NULL; /* list of nodes in main circuit */
Node      *tailNodesMain = NULL; /* tail of nodes list in main circuit */
Node      *listNodesTmp;         /* temporary list of nodes for AddNodeInfo() */
SubCirc   *listSubCirc   = NULL; /* list of subcircuits */
ModelList *listModels    = NULL; /* list of all models */
/* . . . */

/* counters of elements: */
long  cntA = 0L; /*-*/
long  cntB = 0L;
long  cntC = 0L;
long  cntD = 0L;
long  cntE = 0L;
long  cntF = 0L;
long  cntG = 0L;
long  cntH = 0L;
long  cntI = 0L;
long  cntJ = 0L;
long  cntK = 0L;
long  cntL = 0L;
long  cntM = 0L;
long  cntN = 0L; /*-*/
long  cntO = 0L;
long  cntP = 0L; /*-*/
long  cntQ = 0L;
long  cntR = 0L;
long  cntS = 0L;
long  cntT = 0L;
long  cntU = 0L;
long  cntV = 0L;
long  cntW = 0L;
long  cntX = 0L;
long  cntY = 0L; /*-*/
long  cntZ = 0L;

/* rename counters for elements:  */
int   renA = startRENCNT; /*-*/
int   renB = startRENCNT;
int   renC = startRENCNT;
int   renD = startRENCNT;
int   renE = startRENCNT;
int   renF = startRENCNT;
int   renG = startRENCNT;
int   renH = startRENCNT;
int   renI = startRENCNT;
int   renJ = startRENCNT;
int   renK = startRENCNT;
int   renL = startRENCNT;
int   renM = startRENCNT;
int   renN = startRENCNT; /*-*/
int   renO = startRENCNT;
int   renP = startRENCNT; /*-*/
int   renQ = startRENCNT;
int   renR = startRENCNT;
int   renS = startRENCNT;
int   renT = startRENCNT;
int   renU = startRENCNT;
int   renV = startRENCNT;
int   renW = startRENCNT;
int   renX = startRENCNT;
int   renY = startRENCNT; /*-*/
int   renZ = startRENCNT;

/* pointers to lists of elements: */
ElemList *listA = NULL; /*-*/
ElemList *listB = NULL;
ElemList *listC = NULL;
ElemList *listD = NULL;
ElemList *listE = NULL;
ElemList *listF = NULL;
ElemList *listG = NULL;
ElemList *listH = NULL;
ElemList *listI = NULL;
ElemList *listJ = NULL;
ElemList *listK = NULL;
ElemList *listL = NULL;
ElemList *listM = NULL;
ElemList *listN = NULL; /*-*/
ElemList *listO = NULL;
ElemList *listP = NULL; /*-*/
ElemList *listQ = NULL;
ElemList *listR = NULL;
ElemList *listS = NULL;
ElemList *listT = NULL;
ElemList *listU = NULL;
ElemList *listV = NULL;
ElemList *listW = NULL;
ElemList *listX = NULL;
ElemList *listY = NULL; /*-*/
ElemList *listZ = NULL;

/*=========================================================================*/
/* static functions: */
static void InitPass1(void);      /* initializations for pass 1 */
static void Pass1Statistic(void); /* print statistic of 1st pass */
static NodeItem *CreateNodeItem( char *nodetype, char *elemname );
    static Node *     /* returned pointer to tail = new node */
CreateNode(           /* function to create new network node */
    Node *listnodes,  /* list of nodes */
    Node *tailnodes,  /* tail of nodes */
    long cntnodes,    /* counter of nodes */
    char *nodename    /* node name */
);
static char *FindFileName( char *filename );
static char *CreateFileName( char *filename );
static void P1debug( char *msgfmt, ... ); /* print debug message */
/*-------------------------------------------------------------------------*/
    static void
PrintElemList(          /* debugging print elements list (backward) */
    ElemList *listElem, /* pointer to elements list */
    SubCirc  *circuit,  /* for 'circuit', if NULL - for all circuits */
    long     counter,   /* counter of elements */
    char     *type      /* type of elements - first letter of names */
);
    static ElemList *   /* returned pointer to element or NULL if not found */
FindElem(               /* function for find element in list by spice-name */
    ElemList *listElem, /* pointer to elements list */
    char     *elemname, /* spice-name of element */
    SubCirc  *parent    /* pointer to parent circuit or NULL if in main circuit */
);
    static int          /* returned updated rename counter renCntElem */
RenameElems(            /* function for rename elements in list */
    ElemList *listElem, /* pointer to elements list or it part */
    SubCirc  *circuit,  /* from wich subcirc elements */
    int      renCntElem /* rename counter */
);
    static ElemList *   /* returned pointer to element or NULL if not found */
FindHabElem(            /* function for find element in list by habala-name */
    ElemList *listElem, /* pointer to elements list */
    SubCirc  *circuit,  /* from wich subcirc element */
    ElemList *exclude,  /* exclude element */
    char     *hab_name  /* habala-name of element */
);
/*-------------------------------------------------------------------------*/
    static SubCirc *   /* returned pointer to new circuit or NULL/exit if fail */
CreateSubCirc(         /* function for create new circuit */
    SubCirc *parent,   /* pointer to parent circuit or NULL if in main circuit */
    char    *circname  /* circuit name name */
);
    static void
PrintSubCircList(      /* debug print subcircuits list */
    SubCirc *listcirc, /* subcircuits list */
    long    counter    /* counter of subcircuits */
);
/*-------------------------------------------------------------------------*/
static void DebugPrintAllModels(void); /* debugging print all models list */
    static void
PrintModelParmList(         /* print model parameters list */
    ModParmList *parameters /* pointer to parameters list */
);
/*=========================================================================*/
    int
Pass1(      /* 1st pass - Lex & YACC parsing of input */
    void
){
    int result;

    PassNo = 1;

    MsgConsole( msgPASSbegin, PassNo );
    MsgDebug( msgPASSbegin , PassNo );
    if( Flags[flagE].value ) MsgError( msgPASSbegin, PassNo );

    InitPass1();
    MsgDebug( msgAVAILMEM, coreleft() ); /**/
    result =  yyparse(); /* 1st pass */
    MsgDebug( msgAVAILMEM, coreleft() ); /**/

    /* for main circuit: */
    ClrNodesName( listNodesMain ); /* clear Habala-nodes names, '0' -> '0   ' */
    RenameNodes( listNodesMain );  /* rename nodes according to Habala rules */

    /* rename elements from main circuit (NULL) according to Habala rules: */
    RenameAllElems( NULL );

    if( Flags[flag1].value ){
        SHOWint( renC );
        SHOWint( renL );
        SHOWint( renR );
    }

 /* MsgConsole( msgPASSend, PassNo ); /* message in *.Y */
    MsgDebug( msgPASSend , PassNo );
    if( Flags[flagE].value ) MsgError( msgPASSend, PassNo );

    /* skip line counter: */
    MsgConsole( "\n" );

    /* pass no: */
    MsgDebug( msgPASS_, PassNo );
    MsgConsole( msgPASS_, PassNo );
    if( Flags[flagE].value ) MsgError( msgPASS_, PassNo );
    /* errors statistic: */
    if( cntErrors > 0 ){
        MsgDebug( msgTOTALERRORS, cntErrors );
        MsgConsole( msgTOTALERRORS, cntErrors );
        if( Flags[flagE].value ) MsgError( msgTOTALERRORS, cntErrors );
    } else {
        MsgDebug( msgNOERRORS, PassNo );
        MsgConsole( msgNOERRORS, PassNo );
        if( Flags[flagE].value ) MsgError( msgNOERRORS, PassNo );
    }
    /* warnings statistic: */
    if( cntWarnings1 > 0 ){
        MsgDebug( msgTOTALWARNS, cntWarnings1 );
        MsgConsole( msgTOTALWARNS, cntWarnings1 );
        if( Flags[flagE].value ) MsgError( msgTOTALWARNS, cntWarnings1 );
    } else {
        MsgDebug( msgNOWARNS );
        MsgConsole( msgNOWARNS );
        if( Flags[flagE].value ) MsgError( msgNOWARNS );
    }

    Pass1Statistic();

    /* debug print: */
    if( Flags[flag1].value ){

      DebugPrintOptions();   /* debugging print Habala-options */

      /* nodes in main circuit: */
      MsgDebug( msgTABLENODESMAIN );
      MsgDebug( msgCNTNODESMAIN, cntNodesMain );
      PrintNodesList( listNodesMain );

      /* all circuits: */
      PrintSubCircList( listSubCirc, cntSubCirc ); /* debug print subcircuits list */

      /* all models: */
      MsgDebug( msgCNTMODELS, cntModels );
      DebugPrintAllModels();

      /* all elements: */
      DebugPrintElemLists( NULL ); /* debugging print elements lists for all circ */
    }

    /* out nodes translations list for main circuit: */
    PrintNodesTransl( NULL/*MainCircuit*/, listNodesMain );

    OutHabSERVIS();      /* generate &SERVIS-statement */

    OutHabCIRCOM();      /* generate &CIRCOM-statement */

    return result;
}/*Pass1()*/
/*-------------------------------------------------------------------------*/
    static void
InitPass1(   /* initializations for pass 1 */
    void
){
    /* errors count: */
    cntErrors = 0;

    /* counters: */
    cntNodesMain = 0L;
    cntSubCirc   = 0L;
    cntModels    = 0L;

    /* print header to nodes translation file */
    OutNodeTr( msgHDRNODESFILE );

    /* print header to elements names translation file */
    OutElems( msgHDRELEMSFILE );

    /* counters of elements: */
    cntA = 0L; /*-*/
    cntB = 0L;
    cntC = 0L;
    cntD = 0L;
    cntE = 0L;
    cntF = 0L;
    cntG = 0L;
    cntH = 0L;
    cntI = 0L;
    cntJ = 0L;
    cntK = 0L;
    cntL = 0L;
    cntM = 0L;
    cntN = 0L; /*-*/
    cntO = 0L;
    cntP = 0L; /*-*/
    cntQ = 0L;
    cntR = 0L;
    cntS = 0L;
    cntT = 0L;
    cntU = 0L;
    cntV = 0L;
    cntW = 0L;
    cntX = 0L;
    cntY = 0L; /*-*/
    cntZ = 0L;

    /* rename counters for elements:  */
    renA = /*-*/
    renB =
    renC =
    renD =
    renE =
    renF =
    renG =
    renH =
    renI =
    renJ =
    renK =
    renL =
    renM =
    renN = /*-*/
    renO =
    renP = /*-*/
    renQ =
    renR =
    renS =
    renT =
    renU =
    renV =
    renW =
    renX =
    renY = /*-*/
    renZ = startRENCNT;

    /* init pointers: */
    listNodesMain = NULL; /* list of nodes in main circuit */
    tailNodesMain = NULL; /* tail of nodes list in main circuit */
    listFiles     = NULL; /* list of input files names */
    listSubCirc   = NULL; /* list of subcircuits */
    listModels    = NULL; /* list of all models */

    /* lists of elements: */
    listA = NULL; /*-*/
    listB = NULL;
    listC = NULL;
    listD = NULL;
    listE = NULL;
    listF = NULL;
    listG = NULL;
    listH = NULL;
    listI = NULL;
    listJ = NULL;
    listK = NULL;
    listL = NULL;
    listM = NULL;
    listN = NULL; /*-*/
    listO = NULL;
    listP = NULL; /*-*/
    listQ = NULL;
    listR = NULL;
    listS = NULL;
    listT = NULL;
    listU = NULL;
    listV = NULL;
    listW = NULL;
    listX = NULL;
    listY = NULL; /*-*/
    listZ = NULL;

}/*InitPass1()*/
/*-------------------------------------------------------------------------*/
    static void
Pass1Statistic(   /* show parse statistic */
    void
){

 /* print in debug-file - always: */
    MsgDebug( "\n" );
    MsgDebug( msgSTATHEADER );

    /* print numbers of: */
    MsgDebug( msgCNTNODESMAIN, cntNodesMain ); /* nodes */
    MsgDebug( msgCNTSUBCIRC, cntSubCirc );     /* subcircuits */
    MsgDebug( msgCNTMODELS, cntModels );       /* models */

    /* elements statistic: */
    if( cntA ) MsgDebug( msgCNTDEFSELEM, "A", cntA ); /*-*/
    if( cntB ) MsgDebug( msgCNTDEFSELEM, "B", cntB );
    if( cntC ) MsgDebug( msgCNTDEFSELEM, "C", cntC );
    if( cntD ) MsgDebug( msgCNTDEFSELEM, "D", cntD );
    if( cntE ) MsgDebug( msgCNTDEFSELEM, "E", cntE );
    if( cntF ) MsgDebug( msgCNTDEFSELEM, "F", cntF );
    if( cntG ) MsgDebug( msgCNTDEFSELEM, "G", cntG );
    if( cntH ) MsgDebug( msgCNTDEFSELEM, "H", cntH );
    if( cntI ) MsgDebug( msgCNTDEFSELEM, "I", cntI );
    if( cntJ ) MsgDebug( msgCNTDEFSELEM, "J", cntJ );
    if( cntK ) MsgDebug( msgCNTDEFSELEM, "K", cntK );
    if( cntL ) MsgDebug( msgCNTDEFSELEM, "L", cntL );
    if( cntM ) MsgDebug( msgCNTDEFSELEM, "M", cntM );
    if( cntN ) MsgDebug( msgCNTDEFSELEM, "N", cntN ); /*-*/
    if( cntO ) MsgDebug( msgCNTDEFSELEM, "O", cntO );
    if( cntP ) MsgDebug( msgCNTDEFSELEM, "P", cntP ); /*-*/
    if( cntQ ) MsgDebug( msgCNTDEFSELEM, "Q", cntQ );
    if( cntR ) MsgDebug( msgCNTDEFSELEM, "R", cntR );
    if( cntS ) MsgDebug( msgCNTDEFSELEM, "S", cntS );
    if( cntT ) MsgDebug( msgCNTDEFSELEM, "T", cntT );
    if( cntU ) MsgDebug( msgCNTDEFSELEM, "U", cntU );
    if( cntV ) MsgDebug( msgCNTDEFSELEM, "V", cntV );
    if( cntW ) MsgDebug( msgCNTDEFSELEM, "W", cntW );
 /* if( cntX ) MsgDebug( msgCNTDEFSELEM, "X", cntX ); /* => cntSubCirc */
    if( cntY ) MsgDebug( msgCNTDEFSELEM, "Y", cntY ); /*-*/
    if( cntZ ) MsgDebug( msgCNTDEFSELEM, "Z", cntZ );

    if( !Flags[flagS].value ) return; /* skip rest: */

 /* print to error file: */
    /*if( Flags[flagE].value )*/
    MsgError( msgSTATHEADER );

    /* print numbers of: */
    MsgError( msgCNTNODESMAIN, cntNodesMain ); /* nodes */
    MsgError( msgCNTSUBCIRC, cntSubCirc );     /* subcircuits */
    MsgError( msgCNTMODELS, cntModels );       /* models */

    /* elements statistic: */
    if( cntA ) MsgError( msgCNTDEFSELEM, "A", cntA ); /*-*/
    if( cntB ) MsgError( msgCNTDEFSELEM, "B", cntB );
    if( cntC ) MsgError( msgCNTDEFSELEM, "C", cntC );
    if( cntD ) MsgError( msgCNTDEFSELEM, "D", cntD );
    if( cntE ) MsgError( msgCNTDEFSELEM, "E", cntE );
    if( cntF ) MsgError( msgCNTDEFSELEM, "F", cntF );
    if( cntG ) MsgError( msgCNTDEFSELEM, "G", cntG );
    if( cntH ) MsgError( msgCNTDEFSELEM, "H", cntH );
    if( cntI ) MsgError( msgCNTDEFSELEM, "I", cntI );
    if( cntJ ) MsgError( msgCNTDEFSELEM, "J", cntJ );
    if( cntK ) MsgError( msgCNTDEFSELEM, "K", cntK );
    if( cntL ) MsgError( msgCNTDEFSELEM, "L", cntL );
    if( cntM ) MsgError( msgCNTDEFSELEM, "M", cntM );
    if( cntN ) MsgError( msgCNTDEFSELEM, "N", cntN ); /*-*/
    if( cntO ) MsgError( msgCNTDEFSELEM, "O", cntO );
    if( cntP ) MsgError( msgCNTDEFSELEM, "P", cntP ); /*-*/
    if( cntQ ) MsgError( msgCNTDEFSELEM, "Q", cntQ );
    if( cntR ) MsgError( msgCNTDEFSELEM, "R", cntR );
    if( cntS ) MsgError( msgCNTDEFSELEM, "S", cntS );
    if( cntT ) MsgError( msgCNTDEFSELEM, "T", cntT );
    if( cntU ) MsgError( msgCNTDEFSELEM, "U", cntU );
    if( cntV ) MsgError( msgCNTDEFSELEM, "V", cntV );
    if( cntW ) MsgError( msgCNTDEFSELEM, "W", cntW );
 /* if( cntX ) MsgError( msgCNTDEFSELEM, "X", cntX ); /* => cntSubCirc */
    if( cntY ) MsgError( msgCNTDEFSELEM, "Y", cntY ); /*-*/
    if( cntZ ) MsgError( msgCNTDEFSELEM, "Z", cntZ );

}/*Pass1Statistic()*/
/*=========================================================================*/
    void
ClrNodesName(       /* clear Habala-nodes names, for node '0' set H-name '0   ' */
    Node *listnodes /* list of nodes */
){
    int  i;
    Node *pn;
    for( pn = listnodes; pn != NULL; /**/pn = pn->nextnode ){
      for( i=0; i<lenHABNAME; i++ ) pn->newname[i] = '\0'; /* clear H-name */
      if( streq(pn->nodename, nodnameGND) ) pn->newname[0] = '0'; /* for node '0' */
    }
}/*ClrNodesName()*/
/*-------------------------------------------------------------------------*/
    void
RenameNodes(        /* rename nodes according to Habala rules */
    Node *listnodes /* list of nodes */
){
    int  i;
    char *lb;
    Node *pn;
  /*int  node0proc;*/

    if( listnodes == NULL ) return;

 /* MsgDebug( "before RenameNodes(): " );/**/
 /* SHOWlong( renNodes );/**/

    /* new version of RenameNodes(): */
    for( pn = listnodes; pn != NULL; pn = pn->nextnode )
        if( strlen(pn->newname) == 0 ){ /* rename only if newname is empty: */
            /* generate new name from rename counter: */
            sprintf( linebuffer, "%-4ld", renNodes );
            /* remove tail blanks: */
            lb = linebuffer;
            while( isdigit(*lb) ) lb++;
            *lb = '\0';
            strncpy( pn->newname, linebuffer, lenHABNAME );

            renNodes += incRENNODES;
            if( renNodes == stopRENNODES ) /* fatal: rename counter out of bound */
                MsgErrorFatal( exitRENNODESFAIL, msgRENNODESFAIL );
        }

# ifdef UseOldVerOfRenameNodes
    /* old version of RenameNodes(): */
  /*node0proc = FALSE;*/
    for( pn = listnodes; pn != NULL; /**/pn = pn->nextnode ){
        /* process node if ... */
        if(  streq(pn->nodename, nodnameGND) /*&& !node0proc*/ ){
            /* ... it is GND-node that not processed yet: */            

            /* sprintf( pn->newname, "%-4s", nodnameGND ); /* - set tail blanks */
                /* in pn->newname must not be tail blanks: */
            for( i=0; i<lenHABNAME; i++ ) pn->newname[i] = '\0';
            pn->newname[0] = '0';

        }else if( !streq(pn->nodename, nodnameGND) /*&& node0proc*/ ){
            /* ... it is nonGND-node after GND-node already processed - assign new name: */

            /* sprintf( pn->newname, "%-4ld", renNodes ); /* - set tail blanks */
                /* in pn->newname must not be tail blanks: */
            for( i=0; i<lenHABNAME; i++ ) pn->newname[i] = '\0'; /*clear*/

            /* generate new name from rename counter: */
            sprintf( linebuffer, "%-4ld", renNodes );
            /* remove tail blanks: */
            lb = linebuffer;
            while( isdigit(*lb) ) lb++;
            *lb = '\0';
            strncpy( pn->newname, linebuffer, lenHABNAME );

            renNodes += incRENNODES;
            if( renNodes == stopRENNODES ) /* fatal: rename counter out of bound */
                MsgErrorFatal( exitRENNODESFAIL, msgRENNODESFAIL );
        }
        /* after process at first GND-node restart loop again: */
/*      if( streq(pn->nodename, nodnameGND) && !node0proc ){
            node0proc = TRUE;
            pn = listnodes;    /* goto begin of list */        
/*      } else
            pn = pn->nextnode; /* goto next list item */
    }
# endif

 /* MsgDebug( "after RenameNodes(): " );/**/
 /* SHOWlong( renNodes );/**/

}/*RenameNodes()*/
/*-------------------------------------------------------------------------*/
    void
PrintNodesList(     /* debug print nodes list */
    Node *listnodes /* list of nodes */
){
    long     no;
    Node     *pn;
    NodeItem *el;
    int      node0printed; /* flag set TRUE if node '0' printed */

 /* SHOWptr(listnodes);/**/

    node0printed = FALSE;
    for( no = 1, pn = listnodes; pn != NULL;  ){

        /* print node information if ... */
        if(     /* ... it is GND-node that not printed yet: */
            (  streq(pn->nodename, nodnameGND) && !node0printed )
                /* ... it is nonGND-node after GND-node already printed: */
         || ( !streq(pn->nodename, nodnameGND) &&  node0printed )
        ){
            /* print node name and counter of references: */
            MsgDebug( msgNODENAMEREF, no++, pn->nodename, pn->newname, pn->cntref );

            /* print all items list: */
            for( el = pn->itemslist; el != NULL; el = el->nextitem ){
               if( el->nodetype != NULL ){
                      MsgDebug( msgNODETYPE, el->nodetype );
               } else MsgDebug( msgNODETYPEFREE );
               MsgDebug( msgNODEITEM, el->elemname, el->filename, el->linenum );
            }
        }

        /* after print at first GND-node restart loop again: */
        if( streq(pn->nodename, nodnameGND) && !node0printed ){
            node0printed = TRUE;
            pn = listnodes;    /* goto begin of list */        
        } else {
            pn = pn->nextnode; /* goto next list item */

            /* if list ended and node 0 not found - restart loop again: */
            if( pn == NULL && !node0printed ){
                node0printed = TRUE;
                pn = listnodes;    /* goto begin of list */
            }
        }
    }
}/*PrintNodesList()*/
/*-------------------------------------------------------------------------*/
    void
PrintNodesTransl(      /* print nodes translation */
    SubCirc *circ,     /* pointer to circuit, = NULL for main circuit */
    Node    *listnodes /* list of nodes */
){
    Node *pn;
 /* MsgDebug("PrintNodesTransl(%s)...\n", circ==NULL?msgMainCircuit:circ->circname);/**/
    for( pn = listnodes; pn != NULL; pn = pn->nextnode ){
        OutNodeTr( msgNODETRANSLINE, pn->newname, pn->nodename);
        OutCallPath( fileNodes ); /* print Xxxx-subcall path */
        OutNodeTr( "\t" );  
        OutFullCircName( fileNodes, circ );
        OutNodeTr( "\n" );  
    }
}/*PrintNodesTransl()*/
/*-------------------------------------------------------------------------*/
    void
PrintElemsTransl(      /* print element name translation line */
    char     *elname,  /* element name */
    SubCirc  *circ,    /* pointer to circuit, = NULL for main circuit */
    ElemList *ellist   /* pointer to elements list */
){
    ElemList *el;

 /* MsgDebug("PrintElemsTransl(%s)...\n", circ==NULL?msgMainCircuit:circ->circname);/**/

    /* find element 'elname' in circuit 'circ': */
    for( el = ellist; el != NULL; el = el->nextelem )
        if( streq( el->elemname, elname ) && ( el->parent == circ )
          ) goto prn_elem;
    return;

    /* print: */
prn_elem:
    OutElems( msgELEMTRANSLINE, el->newname, el->elemname);
    OutCallPath( fileElems ); /* print Xxxx-subcall path */
    OutElems( "\t" );  
    OutFullCircName( fileElems, el->parent ); /* circuit */
    OutElems( "\n" );  
}/*PrintElemsTransl()*/
/*-------------------------------------------------------------------------*/
    void
OutFullCircName(   /* print full circuit name */
    FILE    *ofil, /* file for out */
    SubCirc *circ  /* pointer to circuit, = NULL for main circuit */
){
#define NORECURSIVE /**/
#ifdef  NORECURSIVE
/* non-recursive version: */
    int  i;
    static char buf[maxINPLINELEN];
    static char result[maxINPLINELEN];
    SubCirc *pc = circ;

    for( i = 0; i < maxINPLINELEN; i++ ) result[i] = 0;
    for(;;){
        /* put to 'buf' last name + '/' */
        for( i = 0; i < maxINPLINELEN; i++ ) buf[i] = 0;
        if( pc != NULL ){
          if( strlen(buf) + strlen(pc->circname) > maxINPLINELEN )
            MsgErrorFatal( exitCIRCNAMETOOLONG, msgCIRCNAMETOOLONG, circ->circname );
          strcat( buf, pc->circname );
        }
        if( strlen(buf) + strlen(msgCircSeparator) > maxINPLINELEN )
            MsgErrorFatal( exitCIRCNAMETOOLONG, msgCIRCNAMETOOLONG, circ->circname );
        strcat( buf, msgCircSeparator );

        /* append to 'buf' saved 'result': */
        if( strlen(buf) + strlen(result) > maxINPLINELEN )
            MsgErrorFatal( exitCIRCNAMETOOLONG, msgCIRCNAMETOOLONG, circ->circname );
        strcat( buf, result );

        /* save 'result': */
        for( i = 0; i < maxINPLINELEN; i++ ) result[i] = 0;
        strncpy( result, buf, maxINPLINELEN );

        if( pc == NULL ) break;
        pc = pc->parent;
    }
    OutFile( ofil, result );
#else
/* recursive version: */
    if( circ != NULL ) OutFullCircName( ofil, circ->parent );
    if( circ != NULL ) OutFile( ofil, circ->circname );
    OutFile( ofil, msgCircSeparator );
    if( circ == NULL ) return;
#endif
}/*OutFullCircName()*/
/*-------------------------------------------------------------------------*/
    Node * /* returned pointer to tail (last node) or NULL if fail or -1 if not processing */
AddNodeInfo(          /* function to add information about network node */
    Node *listnodes,  /* list of nodes */
    Node *tailnodes,  /* tail of nodes (last element) */
    long cntnodes,    /* counter of nodes */
    char *nodename,   /* node name */
    char *nodetype,   /* type of node (optional) */
    char *elemname    /* network element name wich contain node */
){
    Node     *pn;
    Node     *tail = tailnodes;
    NodeItem *newitem, *itail;

    if( PassNo != 1 ) return (Node *)(-1); /* process nodes only in Pass 1 */

 /* MsgDebug("AddNodeInfo(node='%s' in elem='%s'): ",nodename,elemname); /**/
 /* SHOWptr(tailnodes); /**/

    pn = FindNode( listnodes, nodename );
    if( pn == NULL ){
        pn = CreateNode( listnodes, tailnodes, cntnodes, nodename ); 
        if( pn != NULL ) tail = pn;
        else return NULL;
    }
    /* add item to itemlist in node: */
    newitem = CreateNodeItem( nodetype, elemname );

    /* 1st item is head of items list: */
    if( pn->itemslist == NULL ) pn->itemslist = newitem;

    /* add to items list: */
    itail = pn->itemstail;
    if( itail != NULL ) itail->nextitem = newitem; /* for non1st items */
    pn->itemstail = newitem;

    pn->cntref++; /* counter of references to this node */

 /* MsgDebug("...AddNodeInfo(): "); /**/
 /* SHOWptr(tail); /**/

    return tail; /* pointer to tail */
}/*AddNodeInfo()*/
/*-------------------------------------------------------------------------*/
    static NodeItem * /* returned pointer to node item or NULL if fail */
CreateNodeItem(       /* function to create new node item */
    char *nodetype,   /* type of node (if optional = NULL) */
    char *elemname    /* network element name wich contain node */
){
    NodeItem *el;
    if( ( el = (NodeItem *)malloc( sizeof(NodeItem) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC1 );
    el->nodetype = nodetype; /* type of node (optional) */
    el->elemname = elemname; /* network element name wich contain node */

    /* source file name: */
    el->filename = FindFileName( nameFileInp );
    if( el->filename == NULL ) el->filename = CreateFileName( nameFileInp ); 
    if( el->filename == NULL ) return NULL;

    el->linenum  = yyTokenLine/*yylineno*/;  /* line number in this file */

    el->nextitem = NULL; /* pointer to next item */

    return el;
}/*CreateNodeItem()*/
/*-------------------------------------------------------------------------*/
    Node *          /* returned pointer to node or NULL if not found */
FindNode(	    /* function to search node in list */
    Node *listnodes,/* list of nodes */
    char *nodename  /* node name */
){
    Node *pn;
    for( pn = listnodes; pn != NULL; pn = pn->nextnode )
        if( streq(pn->nodename, nodename) ) return pn;
    return NULL;
}/*FindNode()*/
/*-------------------------------------------------------------------------*/
    char *          /* returned habala-name of node or NULL if not found */
GetHabNodeName(     /* function */
    Node *listnodes,/* list of nodes */
    char *spicename /* from spice-name of node */
){
    Node *pn;
    for( pn = listnodes; pn != NULL; pn = pn->nextnode )
        if( streq(pn->nodename, spicename) ) return pn->newname;
    return NULL;
}/*GetHabNodeName()*/
/*-------------------------------------------------------------------------*/
    static Node *     /* returned pointer to tail = new node */
CreateNode(           /* function to create new network node */
    Node *listnodes,  /* list of nodes */
    Node *tailnodes,  /* tail of nodes */
    long cntnodes,    /* counter of nodes */
    char *nodename    /* node name */
){                  /* ! use global Node *listNodesTmp - temporary list of nodes */
                    /* ! use global long cntNodesTmp - temporary counter of nodes */
    int  i;
    Node *pn;

    /* create node: */
    if( ( pn = (Node *)malloc( sizeof(Node) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC1 );
    pn->nodename  = nodename; /* node name */
    for( i = 0; i < 5; i++ ) pn->newname[i] = 0;
    pn->cntref    = 0;        /* counter of references to this node */
    pn->itemslist = NULL;     /* pointer to list of elements wich contain node */
    pn->itemstail = NULL;     /* pointer to tail of elements list */
    pn->nextnode  = NULL;     /* pointer to next node */

    /* 1st node is head of list: */
    if( listnodes == NULL ) listnodes = pn;

    /* add to list: */
    if( tailnodes != NULL ) tailnodes->nextnode = pn; /* for non1st nodes */
    tailnodes = pn;

    cntnodes++;

    /* return global vars: */
    listNodesTmp = listnodes;
    cntNodesTmp  = cntnodes;

    return tailnodes;
}/*CreateNode()*/
/*-------------------------------------------------------------------------*/
    static char *
FindFileName(
    char *filename
){
    FileList *fl;
    for( fl = listFiles; fl != NULL; fl = fl->nextfile )
        if( streq(fl->filename, filename) ) return fl->filename;
    return NULL;
}/*FindFileName()*/
/*-------------------------------------------------------------------------*/
    static char *
CreateFileName(
    char *filename
){
    FileList *fl;
    char     *nf;

    if( ( nf = (char *)malloc( strlen(filename) + 1 ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC1 );
    strcpy( nf, filename );
    nf[strlen(filename)] = '\0';
    if( ( fl = (FileList *)malloc( sizeof(FileList) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC1 );
    fl->filename = nf;

    /* add to list: */
    fl->nextfile = listFiles; /* pointer to next file */
    listFiles = fl;

    return fl->filename;
}/*FindFileName()*/
/*=========================================================================*/
    void
DebugPrintElemLists(    /* debugging print all elements lists */
    SubCirc  *circuit   /* for 'circuit', if NULL - for all circuits */
){
  if( Flags[flag1].value || Flags[flag2].value ){
    MsgDebug( msgELEMLIST4CIRC );
    OutFullCircName( fileDebug, circuit ); /* full circ name */
    MsgDebug( "\n" );
 /* PrintElemList( listA, circuit, cntA, "A" ); /*-*/
    PrintElemList( listB, circuit, cntB, "B" );
    PrintElemList( listC, circuit, cntC, "C" );
    PrintElemList( listD, circuit, cntD, "D" );
    PrintElemList( listE, circuit, cntE, "E" );
    PrintElemList( listF, circuit, cntF, "F" );
    PrintElemList( listG, circuit, cntG, "G" );
    PrintElemList( listH, circuit, cntH, "H" );
    PrintElemList( listI, circuit, cntI, "I" );
    PrintElemList( listJ, circuit, cntJ, "J" );
    PrintElemList( listK, circuit, cntK, "K" );
    PrintElemList( listL, circuit, cntL, "L" );
    PrintElemList( listM, circuit, cntM, "M" );
 /* PrintElemList( listN, circuit, cntN, "N" ); /*-*/
    PrintElemList( listO, circuit, cntO, "O" );
 /* PrintElemList( listP, circuit, cntP, "P" ); /*-*/
    PrintElemList( listQ, circuit, cntQ, "Q" );
    PrintElemList( listR, circuit, cntR, "R" );
    PrintElemList( listS, circuit, cntS, "S" );
    PrintElemList( listT, circuit, cntT, "T" );
    PrintElemList( listU, circuit, cntU, "U" );
    PrintElemList( listV, circuit, cntV, "V" );
    PrintElemList( listW, circuit, cntW, "W" );
    PrintElemList( listX, circuit, cntX, "X" );
 /* PrintElemList( listY, circuit, cntY, "Y" ); /*-*/
    PrintElemList( listZ, circuit, cntZ, "Z" );
  }
}/*DebugPrintElemLists()*/
/*-------------------------------------------------------------------------*/
    static void
PrintElemList(          /* debugging print elements list (backward) */
    ElemList *listElem, /* pointer to elements list */
    SubCirc  *circuit,  /* for 'circuit', if NULL - for all circuits */
    long     counter,   /* counter of elements */
    char     *type      /* type of elements - first letter of names */
){
    long     no  = 0L;
    ElemList *el = listElem;

    if( counter == 0 ){
      if( circuit == NULL ) MsgDebug( msgELEMLISTEMPTY, type );
    } else {
      if( circuit == NULL ) MsgDebug( msgELEMLISTPRN, type, counter );
      for( el = listElem; el != NULL; el = el->nextelem ){
        if( circuit == el->parent || circuit == NULL ){
          /* print old, new names, circuit, filename and lineno: */
          MsgDebug( msgELEMDEBPRN,
                        ++no,
                        el->elemname, el->newname,
                        el->filename, el->linenum,
                        (el->parent == NULL)? msgMainCircuit: el->parent->circname
                  );
          OutFullCircName( fileDebug, el->parent ); /* full circ name, ASB000414 */
          MsgDebug( "\n" );
        }
      }
    }
}/*PrintElemList()*/
/*-------------------------------------------------------------------------*/
    ElemList *          /* returned (updated if OK) pointer to elements list */
AddElem(                /* function for add new element to list */
    ElemList *listElem, /* pointer to elements list */
    long     *counter,  /* pointer to counter of elements */
    char     *elemname, /* element name */
    SubCirc  *parent    /* pointer to parent circuit or NULL if in main circuit */
){
    int      i;
    ElemList *el;

 /* MsgDebug("AddElem('%s')...\n",elemname); /**/
 /* SHOWptr(listElem); /**/

    /* list of elements fill only in 1st pass: */
    if( PassNo != 1 ) return listElem; /* exit without changes */

 /*
    SHOWstr(elemname);
    SHOWptr(parent);
    if( parent != NULL ){ SHOWstr(parent->circname); }
    else                  MsgDebug( "%s\n", msgMainCirc );
 /**/
    el = FindElem( listElem, elemname, parent );
 /* SHOWptr(el);/**/

    /* pass1: if element with such name exist in list - error: */
    if( el != NULL ) MsgCompile( msgELEMNAMEEXIST, elemname );

    /* create new element in list anywhere: */
    if( ( el = (ElemList *)malloc( sizeof(ElemList) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC1 );
 /* SHOWptr(el);/**/

    el->elemname = elemname; /* element name */
    el->parent = parent;     /* pointer to parent circuit */

    /* at first: new Habala-name[4 symbols+'\0'] = truncated element name: */
    for( i = 0; i < lenHABNAME0/*5*/; i++ ) el->newname[i] = 0;
    if( parent == NULL ){ /* assign new name only for elements from main circuit */
        el->newname[0] = toupper( elemname[0] );
        for( i = 1; i < lenHABNAME/*4*/; i++ ){
            if( i >= strlen(elemname) ) break;
            el->newname[i] = elemname[i];
        }
    }
    /* source file name: */
    el->filename = FindFileName( nameFileInp );
    if( el->filename == NULL )
        el->filename = CreateFileName( nameFileInp ); 
    if( el->filename == NULL ) return listElem; /* exit without changes */

    el->linenum  = yyTokenLine/*yylineno*/;  /* line number in this file */
    if( yyTokenPos == 0 ) el->linenum--;     /* line number of next line */

    el->nextelem = listElem; /* pointer to next item */
 /*
    SHOWstr(el->elemname);
    SHOWstr(el->newname);
    SHOWstr(el->filename);
    SHOWlong(el->linenum);
    SHOWptr(el->nextelem);
 */
    listElem = el; /* update list */
 /* SHOWptr(listElem); /**/

    (*counter)++; /* increase counter of elements */
    return listElem;

}/*AddElem()*/
/*-------------------------------------------------------------------------*/
    static ElemList *   /* returned pointer to element or NULL if not found */
FindElem(               /* function for find element in list by spice-name */
    ElemList *listElem, /* pointer to elements list */
    char     *elemname, /* spice-name of element */
    SubCirc  *parent    /* pointer to parent circuit or NULL if in main circuit */
){
    ElemList *el;

 /* MsgDebug( "FindElem('%s' in circuit '%s')...\n",
        elemname, (parent==NULL)? msgMainCircuit: parent->circname ); /**/

    for( el = listElem; el != NULL; el = el->nextelem )
        if( streq( elemname, el->elemname ) ){
        /*if( parent == NULL)          return el; /* in main circ *//* - error ! */
        /*else*/
            if( parent == el->parent ) return el; /* in any subcirc include main */
        }
    return NULL;
}/*FindElem()*/
/*-------------------------------------------------------------------------*/
    char *               /* returned habala-name of element or NULL if not found */
GetHabElemName(          /* function for convert spice- to habala-name of element */
    char     *spicename, /* spice-name of element */
    SubCirc  *circ,      /* pointer to circuit, = NULL for main circuit */
    ElemList *listElem   /* pointer to elements list */
){
    ElemList *el;
    for( el = listElem; el != NULL; el = el->nextelem )
        if( streq( el->elemname, spicename ) && ( el->parent == circ )
          ) return el->newname;
    return NULL;
}/*GetHabElemName()*/
/*-------------------------------------------------------------------------*/
    void
RenameAllElems(         /* rename all elements in circuit */
    SubCirc *circuit
){
    renA = RenameElems( listA, circuit, renA ); /*-*/
    renB = RenameElems( listB, circuit, renB );
    renC = RenameElems( listC, circuit, renC );
    renD = RenameElems( listD, circuit, renD );
    renE = RenameElems( listE, circuit, renE );
    renF = RenameElems( listF, circuit, renF );
    renG = RenameElems( listG, circuit, renG );
    renH = RenameElems( listH, circuit, renH );
 /* renI = RenameElems( listI, circuit, renI ); /* rename on generation, not here */
    renJ = RenameElems( listJ, circuit, renJ );
    renK = RenameElems( listK, circuit, renK );
    renL = RenameElems( listL, circuit, renL );
    renM = RenameElems( listM, circuit, renM );
    renN = RenameElems( listN, circuit, renN ); /*-*/
    renO = RenameElems( listO, circuit, renO );
    renP = RenameElems( listP, circuit, renP ); /*-*/
    renQ = RenameElems( listQ, circuit, renQ );
    renR = RenameElems( listR, circuit, renR );
    renS = RenameElems( listS, circuit, renS );
    renT = RenameElems( listT, circuit, renT );
    renU = RenameElems( listU, circuit, renU );
 /* renV = RenameElems( listV, circuit, renV ); /* rename on generation, not here */
    renW = RenameElems( listW, circuit, renW );
    renX = RenameElems( listX, circuit, renX );
    renY = RenameElems( listY, circuit, renY ); /*-*/
    renZ = RenameElems( listZ, circuit, renZ );
}/*RenameAllElems()*/
/*-------------------------------------------------------------------------*/
    static int          /* returned updated rename counter renCntElem */
RenameElems(            /* function for rename elements in list */
    ElemList *listElem, /* pointer to elements list or it part */
    SubCirc  *circuit,  /* from wich subcirc elements */
    int      renCntElem /* rename counter */
){
    int      i;
    ElemList *el    = listElem;

    if( listElem == NULL ) return renCntElem;

    for( el = listElem; el != NULL; el = el->nextelem )
      if( el->parent == circuit ){
        /*MsgDebug( "RenameElems(): original element name = %s\n", el->elemname );/**/

        if( circuit == NULL ){ /* only for main circ: try to save orig name: */
          /* if newname is free then assign it as truncated original name: */
          if( strlen(el->newname) == 0 ){
            el->newname[0] = toupper( el->elemname[0] );
            for( i = 1; i < lenHABNAME/*4*/; i++ ){
              if( i >= strlen(el->elemname) ) break;
              el->newname[i] = el->elemname[i];
            }
            /*MsgDebug( "RenameElems(): assign 1st new element name = %s\n", el->newname );/**/
          }
          /* find equal name in the all list, exclude current element: */
          if( FindHabElem( listElem, circuit, el, el->newname ) ){
            /* rename element: */
            sprintf( el->newname, "%c%03d", toupper(el->elemname[0]), renCntElem );
            /*MsgDebug( "RenameElems(): assign new unique element name = %s\n", el->newname );/**/
            renCntElem += incRENCNT; /* prepare to next rename */
            if( renCntElem == stopRENCNT ) /* fatal: rename counter out of bound */
                MsgErrorFatal( exitRENELEMFAIL, msgRENELEMFAIL );
          }
        }else{ /* element in subcircuit rename anywhere: */
            sprintf( el->newname, "%c%03d", toupper(el->elemname[0]), renCntElem );
            /*MsgDebug( "RenameElems(): assign new unique element name = %s\n", el->newname );/**/
            renCntElem += incRENCNT; /* prepare to next rename */
            if( renCntElem == stopRENCNT ) /* fatal: rename counter out of bound */
                MsgErrorFatal( exitRENELEMFAIL, msgRENELEMFAIL );
        }
        /*MsgDebug( "RenameElems(): result: new element name = %s\n", el->newname );/**/
      }
    return renCntElem;
}/*RenameElems()*/
/*-------------------------------------------------------------------------*/
    int                 /* returned updated rename counter renCntElem */
Rename1elem(            /* function for rename _single_ elements in list */
    char     *spicename,/* original name of element */
    ElemList *listElem, /* pointer to elements list or it part */
    SubCirc  *circuit,  /* from wich subcirc elements */
    int      renCntElem /* rename counter */
){
    int      i;
    ElemList *el    = listElem;

    if( listElem == NULL ) return renCntElem;

    for( el = listElem; el != NULL; el = el->nextelem )
      if( streq(spicename, el->elemname) && el->parent == circuit ){
/*      MsgDebug( "Rename1elem(): original element name = %s\n", el->elemname );/**/

        /* if( circuit == NULL ){...} /* for main circ: don't save orig name! */
        /* rename anywhere: */
        sprintf( el->newname, "%c%03d", toupper(el->elemname[0]), renCntElem );
        /*MsgDebug( "RenameElems(): assign new unique element name = %s\n", el->newname );/**/
        renCntElem += incRENCNT; /* prepare to next rename */
        if( renCntElem == stopRENCNT ) /* fatal: rename counter out of bound */
            MsgErrorFatal( exitRENELEMFAIL, msgRENELEMFAIL );

/*      MsgDebug( "Rename1elem(): result: new element name = %s\n", el->newname );/**/
        break;
      }
    return renCntElem;
}/*Rename1elem()*/
/*-------------------------------------------------------------------------*/
    static ElemList *   /* returned pointer to element or NULL if not found */
FindHabElem(            /* function for find element in list by habala-name */
    ElemList *listElem, /* pointer to elements list */
    SubCirc  *circuit,  /* from wich subcirc element */
    ElemList *exclude,  /* exclude element */
    char     *hab_name  /* habala-name of element */
){
    ElemList *el = listElem;

    while( el != NULL ){
      if( el->parent == circuit && el != exclude
          && streq( hab_name, el->newname ) )
              return el;
      el = el->nextelem;
    }
    return NULL;
}/*FindHabElem()*/
/*-------------------------------------------------------------------------*/
    void
HabELEMout(           /* generate Habala-operator '&ELEM' */
    char *HBname,     /* habala-name of element */
    char *HBnodelist, /* string-list of habala nodes */
    char *PAR,        /* PAR=... */
    char *IDOP,       /* IDOP=... */
    char *ISTR        /* ISTR=... */
){
    sprintf( linebuffer, " &ELEM NE='%-4s', KNOT=%s", HBname, HBnodelist );
                                    /* 4 = lenHABNAME */
    if( strlen(PAR) != 0 ){
        strcat( linebuffer, ", PAR=" );
        strcat( linebuffer, PAR );
    }
    if( strlen(IDOP) != 0 ){
        strcat( linebuffer, ", IDOP=" );
        strcat( linebuffer, IDOP );
    }
    if( strlen(ISTR) != 0 ){
        strcat( linebuffer, ", ISTR=" );
        strcat( linebuffer, ISTR );
    }
    strcat( linebuffer, " /\n" );
    OutTmp( linebuffer ); /* in tmp-file, not in obj-file! */
}/*HabELEMout()*/
/*=========================================================================*/
    int           /* returned TRUE if OK */
PushCircStack(    /* function for save current subcircuit states in stack */
    void
 /* SubCirc *circ /* pointer to current subcircuit = circDefCurrent */
){
    StackCirc *ptrTmp;

    if( ( ptrTmp = (StackCirc *)malloc(sizeof(StackCirc)) ) == NULL ){
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC1 );
     /* return FALSE; /**/
    }

    /* ASB000304-ASB000318: save subcirc states: */
 /* ptrTmp->issubcall    = isSubCall;      /**/
 /* ptrTmp->levelsubcall = levelSubCall;   /**/
    ptrTmp->circcall     = circCall;
 /* ptrTmp->issubdefbody = isSubDefBody;   /**/
 /* ptrTmp->levelsubdef  = levelSubDef;    /*ASB000312*/
    ptrTmp->circdefcurr  = circDefCurrent; /*ASB000309*/
 /* SHOWCIRCSTATE( "PushCircStack()" ); /**/

 /* ptrTmp->circ  = circ; /* pointer to subcircuit */
    ptrTmp->prev  = stackCircuits; /* previous element of stack = old head of list */
    stackCircuits = ptrTmp; /* new head of list = new element */

    return TRUE;
}/*PushCircStack()*/
/*-------------------------------------------------------------------------*/
    SubCirc * /* returned current circuit from stack, NULL if main (1st pushed) */
PopCircStack( /* function for restore circuit states from stack */
    void
){
 /* SubCirc   *circ; */
    StackCirc *ptrTmp;

    if( stackCircuits == NULL ) return NULL;

    /* ASB000304-ASB000318: restore subcirc states: */
 /* isSubCall      = stackCircuits->issubcall;    /**/
 /* levelSubCall   = stackCircuits->levelsubcall; /**/
    circCall       = stackCircuits->circcall;
 /* isSubDefBody   = stackCircuits->issubdefbody; /**/
 /* levelSubDef    = stackCircuits->levelsubdef;  /*ASB000312*/
    circDefCurrent = stackCircuits->circdefcurr;  /*ASB000309*/
 /* circ           = stackCircuits->circ; */

 /* SHOWCIRCSTATE( "PopCircStack()" ); /**/

    ptrTmp         = stackCircuits;
    stackCircuits  = stackCircuits->prev;
    free( ptrTmp ); /* free top of stack */

 /* return circ; */
    return circDefCurrent;
}/*PopCircStack()*/
/*=========================================================================*/
    SubCirc *          /* returned new pointer to circuits list */
AddSubCirc(            /* function for add to list info about circuit definition */
    SubCirc *circlist, /* circuits list */
    SubCirc *parent,   /* pointer to parent circuit or NULL if in main circuit */
    char    *circname  /* circuit name name */
){
    SubCirc *cr;

    if( PassNo != 1 ) return circlist; /* process ubcircuits only in Pass 1 */

    /* test if circuit with such name is already exist on this level: */
    if( FindSubCirc( circlist, parent, circname ) ){
        MsgCompile( msgDEFCIRCEXIST, circname,
            (parent==NULL)? msgMainCircuit: parent->circname );
        return circlist;
    }
    cr = CreateSubCirc( parent, circname );
    if( !cr ) return FALSE;

    /* add to head of list: */
    cr->nextcirc = circlist;

    return cr; /* new head of list */
}/*AddSubCirc()*/
/*-------------------------------------------------------------------------*/
    int                /* returned TRUE if OK or FALSE if not found */
AddSubCircEnd(         /* function for add to list info about end of circuit definition */
    SubCirc *circlist, /* circuits list */
    SubCirc *parent,   /* pointer to parent circuit or NULL if in main circuit */
    char    *circname  /* circuit name name */
){
    SubCirc *cr;

    if( PassNo != 1 ) return TRUE; /* process subcircuits only in Pass 1 */

    cr = FindSubCirc( circlist, parent, circname );
    if( !cr ) return FALSE;

    /* save for .ends filename & line#: */
    cr->endfname = FindFileName( nameFileInp ); /* source file name */
    if( cr->endfname == NULL ) cr->endfname = CreateFileName( nameFileInp ); 
    if( cr->endfname == NULL ) return FALSE;
    /* number of last line of subcircuit definition body: */
    cr->endline  = yyTokenLine/*yylineno*/;
    if( yyTokenPos == 0 ) (cr->endline)--; /* if parser jump to next line */

 /* MsgDebug("AddSubCircEnd('%s', %s:%ld)\n", circname, cr->endfname, cr->endline);/**/

    return TRUE;
}/*AddSubCircEnd()*/
/*-------------------------------------------------------------------------*/
    SubCirc *          /* returned pointer to find circuit or NULL if not found */
FindSubCirc(           /* function for find circ definition inside 'currcirc' */
    SubCirc *circlist, /* circuits list */
    SubCirc *currcirc, /* pointer to current circuit or NULL if in main circuit */
    char    *circname  /* circuit name */
){
    SubCirc *cr;
/*
    MsgDebug(" FindSubCirc('%s')\n",circname);
    SHOWptr(circlist); SHOWstr(circlist->circname);
    SHOWptr(currcirc); SHOWstr(currcirc->circname);
/**/
    for( cr = circlist; cr != NULL; cr = cr->nextcirc ){
/*
        MsgDebug(" (for) "); SHOWstr(cr->circname);
        MsgDebug(" (for) "); SHOWstr(cr->parent->circname);
/**/
        if( streq( circname, cr->circname ) ){
        /*  if( currcirc == NULL )
        /*                                 return cr; /* found in main circ */
        /*  else /* in subcirc 'currcirc': */
              if( currcirc == cr->parent ) return cr; /* found in 'currcirc' */
        }
    }
    return NULL;
}/*FindSubCirc()*/
/*-------------------------------------------------------------------------*/
    SubCirc *          /* returned pointer to find circuit or NULL if not found */
FindSubCircHigh(       /* function for find circdef inside 'currcirc' and parents */
    SubCirc *circlist, /* circuits list */
    SubCirc *currcirc, /* pointer to current circuit or NULL if in main circuit */
    char    *circname  /* circuit name */
){
    SubCirc *cr;

    for( cr = circlist; cr != NULL; cr = cr->nextcirc ){
      if( streq( circname, cr->circname ) ){
        /* if find equal name, test to belong found circuit */
        /* to 'currcirc' and its parent */
        for(;;){
          if( currcirc == cr->parent ){
            if( Flags[flagG].value  || Flags[flag1].value  || Flags[flag2].value ){
              MsgDebug( "FindSubCircHigh(): found circ def for '%s' in ", circname );
              OutFullCircName( fileDebug, currcirc );
              MsgDebug( "\n" ); 
            }
            return cr;
          }
          if( currcirc != NULL ) currcirc = currcirc->parent;
        }
      }
    }
    return NULL;
}/*FindSubCircHigh()*/
/*-------------------------------------------------------------------------*/
    static SubCirc *   /* returned pointer to new circuit or NULL/exit if fail */
CreateSubCirc(         /* function for create new circuit */
    SubCirc *parent,   /* pointer to parent circuit or NULL if in main circuit */
    char    *circname  /* circuit name name */
){
    SubCirc *newcirc;

    if( ( newcirc = (SubCirc *)malloc( sizeof(SubCirc) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC1 );

    newcirc->circname = circname; /* subcircuit name */
    newcirc->parent = parent;     /* pointer to parent circuit */

    /* init local nodes-parameters list: */
    newcirc->cntnodesparm = 0L;   /* counter of local nodes-parameters */
    newcirc->nodeparmlist = NULL; /* list of all local nodes-parameters */
    newcirc->nodeparmtail = NULL; /* tail of list of all local nodes-parameters */

    /* init local elements nodes list: */
    newcirc->cntnodes = 0L;      /* counter of local nodes */
    newcirc->circnodlist = NULL; /* list of all local nodes */
    newcirc->circnodtail = NULL; /* tail of list of all local nodes */

    /* save begin file&line: */
    newcirc->begfname = FindFileName( nameFileInp ); /* source file name */
    if( newcirc->begfname == NULL ) newcirc->begfname = CreateFileName( nameFileInp ); 
    if( newcirc->begfname == NULL ) return NULL;
    /* number of next line - begin of definition body: */
    newcirc->begline  = yyTokenLine/*yylineno*/;

    /* end file&line: */
    newcirc->endfname = NULL; /* not known yet */
    newcirc->endline  = 0L;   /* not known yet */

    newcirc->nextcirc = NULL; /* pointer to next circuit */

    cntSubCirc++;

    return newcirc;
}/*CreateSubCirc()*/
/*-------------------------------------------------------------------------*/
    static void
PrintSubCircList(      /* debug print subcircuits list */
    SubCirc *listcirc, /* subcircuits list */
    long    counter    /* counter of subcircuits */
){
    long no  = 0L;
    SubCirc *cr = listcirc;

    if( counter == 0 )
      MsgDebug( msgCIRCLISTEMPTY );
    else {
      MsgDebug( msgCIRCLISTPRN, counter );
      while( cr != NULL ){

        /* print name, parent name, (begin..end)filename&lineno: */
        MsgDebug( msgCIRCDEBPRN, cr, ++no, cr->circname,
                  (cr->parent == NULL)? msgMainCirc: cr->parent->circname,
                  cr->begfname, cr->begline, cr->endfname, cr->endline
                );
        MsgDebug( msgFULLCIRCNAME );
        OutFullCircName( fileDebug, cr );
        MsgDebug( "\n" );
        MsgDebug( msgFULLPARENTNAME );
        OutFullCircName( fileDebug, cr->parent );
        MsgDebug( "\n" );

        /* print local nodes-parameters list: */
        MsgDebug( msgCNTNODESPARM, cr->cntnodesparm );
/*      SHOWptr( cr->nodeparmlist ); /**/
        PrintNodesList( cr->nodeparmlist );

        /* print local elements nodes list: */
        MsgDebug( msgCNTNODESLOCAL, cr->cntnodes );
/*      SHOWptr( cr->circnodlist ); /**/
        PrintNodesList( cr->circnodlist );

        cr = cr->nextcirc;
      }
    }
}/*PrintSubCircList()*/
/*=========================================================================*/
    static void
DebugPrintAllModels(    /* debugging print all models list */
    void
){
    ModelList *lm;
    for( lm = listModels; lm != NULL; lm = lm->nextmodel ){
        MsgDebug( msgModelNameType, lm->modelname, lm->modeltype);
        OutFullCircName( fileDebug, lm->circuit );
        MsgDebug( "\n" );
        PrintModelParmList( lm->parameters );
    }
}/*DebugPrintAllModels()*/
/*-------------------------------------------------------------------------*/
    ModelList * /* returned pointer to model in list or NULL if not found */
FindModel(      /* function for find model in list */
    char    *modelname, /* model name */
    SubCirc *circuit    /* circuit contain model definition */
){
    ModelList *lm;

    for( lm = listModels; lm != NULL; lm = lm->nextmodel )
        if( streq( lm->modelname, modelname) && lm->circuit == circuit )
            return lm;

    return NULL;
}/*FindModel()*/
/*-------------------------------------------------------------------------*/
    ModelList * /* returned pointer to head of models list / NULL if exist / exit if fail */
AddToModelList( /* function for add model to list of models */
    char    *modelname, /* model name */
    char    *modeltype, /* model type */
    SubCirc *circuit    /* circuit contain model definition */
){
    ModelList *newmodel;

    if( FindModel( modelname, circuit ) != NULL ) return NULL;

 /* MsgDebug( "AddToModelList('%s','%s')...", modelname, modeltype );/**/

    if( ( newmodel = (ModelList *)malloc( sizeof(ModelList) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC1 );

    cntModels++;
    newmodel->modelname = modelname; /* model name */
    newmodel->modeltype = modeltype; /* model type */
    newmodel->circuit   = circuit;   /* circuit in wich define model */
    newmodel->parameters= NULL ;     /* pointer to model parameters list */

    newmodel->nextmodel = listModels;  /* pointer to next model - old head of models list */

    return newmodel;
}/*AddToModelList()*/
/*-------------------------------------------------------------------------*/
    int       /* returned TRUE if OK, exit if fail */
AddModelParm( /* function for add model parameter to model definition in list */
    ModelList *model,     /* model */
    char      *parmname,  /* parameter name */
    int       valueexist, /* = TRUE if parameter have value */
    double    value       /* parameter value */
){
    ModParmList *newparm;

    if( ( newparm = (ModParmList *)malloc( sizeof(ModParmList) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC1 );

    newparm->parmname   = parmname; /* parameter name */
    newparm->valueexist = valueexist; /* = TRUE if parameter have value */
    newparm->value      = value; /* parameter value */

    newparm->nextparm   = model->parameters; /* pointer to next parameter */
    model->parameters   = newparm;

    return TRUE;
}/*AddModelParm()*/
/*-------------------------------------------------------------------------*/
    int        /* returned TRUE if found, FALSE if not found */
FindModelParm( /* function for find model parameter of model definition in list */
    ModelList *model,    /* model */
    char      *parmname  /* parameter name */
){
    ModParmList *pl;
    for( pl = model->parameters; pl != NULL; pl = pl->nextparm )
        if( streq( pl->parmname, parmname ) ) return TRUE;
    return FALSE;
}/*FindModelParm()*/
/*-------------------------------------------------------------------------*/
    int        /* returned TRUE if found, FALSE if not found */
GetModelParm(  /* function for get model parameter */
    ModelList *model,     /* model */
    char      *parmname,  /* parameter name, case ignored(!) */
    double    *parmvalue, /* pointer to parameter value */
    int       *existval   /* = TRUE if parameter have value */
){
    ModParmList *pl;

    for( pl = model->parameters; pl != NULL; pl = pl->nextparm )
        if( strEQ( pl->parmname, parmname ) ){
            *parmvalue = pl->value;       /* value */
            *existval  = pl->valueexist;  /* = TRUE if parameter have value */
            return TRUE;
        }
    return FALSE;
}/*GetModelParm()*/
/*-------------------------------------------------------------------------*/
    static void
PrintModelParmList(         /* print model parameters list */
    ModParmList *parameters /* pointer to parameters list */
){
    ModParmList *pl;

    for( pl = parameters; pl != NULL; pl = pl->nextparm ){
        MsgDebug( msgModParm );
        /* parameter name: */
        if( strlen(pl->parmname) != 0 )
            MsgDebug( msgModParmName, pl->parmname );
        /* parameter value: */
        if( pl->valueexist )
            MsgDebug( msgModParmVal, pl->value );
        MsgDebug( "\n" );
    }
}/*PrintModelParmList()*/
/*-------------------------------------------------------------------------*/
    ModelList *     /* returned pointer to subcirc or NULL if not found */
UpFindModel( /* model find function in current circ 'circDefCurrent' and higher */
    char *modelname /* model name */
){
    ModelList *model;
    SubCirc   *circ;

    if( Flags[flag2].value )
      MsgDebug("UpFindModel('%s'): ...\n", modelname );/**/

    /* find model in current circuit and higher: */
    for( circ = circDefCurrent; ; circ = circ->parent ){
      if( Flags[flag2].value ){
        MsgDebug( "Try to find model in circuit " );
        OutFullCircName( fileDebug, circ);
        MsgDebug( "...\n" );
      }
      model = FindModel( modelname, circ );
      if( model != NULL ){
        break;
      }
      if( circ == NULL ) break;
    }

    if( model != NULL ){
      if( Flags[flag2].value ){
        MsgDebug( "\tModel '%s' found in circuit ", modelname );
        OutFullCircName( fileDebug, circ);
        MsgDebug( "\n" );
      }
      return model;
    }else{
        MsgDebug( "\tModel not found in current and all higher circuits\n" );
    }

    return model;
}/*UpFindModel()*/
/*-------------------------------------------------------------------------*/
    void
OutHabCIRCOM(        /* generate &CIRCOM-statement */
    void
){
    Out( " &CIRCOM /\n" );
}/*OutHabCIRCOM()*/
/*=========================================================================*/
    static  void
P1debug(     /* print debug message */
    char    *msgfmt,    /* Don't use '\n' in message */
            ...
){
    va_list arguments;
    va_start( arguments, msgfmt );

    if( yyTokenLine <= 0 ) return; /*ASB000119*/

    if( Flags[flag1].value ){
       if( yyTokenPos == 0 && yyTokenLine > 1 ) /* avoid jump to next string */
        MsgDebug( "@@P1@@ #%d,end:\t", yyTokenLine-1 );
      else
        MsgDebug( "@@P1@@ #%d,%d:\t", yyTokenLine, yyTokenPos );

      vsprintf( linebuffer, msgfmt, arguments );
      MsgDebug( linebuffer );
      va_end( arguments );
      MsgDebug( "\n");
    }
}/*P1debug()*/
/*=========================================================================*/
