
/*
 *  S H p a s s 2 . C  --  S2H compiler:  Pass 2 functions
 *
 *  03.01.2000 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# include   "shcommon.h"
# include   "y.tab.h"
/*-------------------------------------------------------------------------*/

static char linebuffer[maxINPLINELEN]; /* for temporary strings */

static int    exist_fi = FALSE; /* exist VJ model parameter for all diodes Schottky */
static double glb_Diode_fi, glb_Diode_f0;

static void InitPass2(void);   /* initializations for pass 2 */
static void P2debug( char *msgfmt, ... ); /* print debug message */

static void HabElemPart(void); /* print &TYP, &ELEM operators for all elements groups */
static void HabFreqPart(void); /* Frequency part */
static void HabVarPart(void);  /* Variations part */
static void HabQuaPart(void);  /* Quality parameters part */

    static void
HabTYPout(        /* generate Habala-operator '&TYP' */
    char *type,   /* IT='...' */
    long counter, /* KOL=... */
    char *param   /* P='...' */
);
    static int    /* return number of lines */
CountLinesBegin(  /* function for count lines from fileTmp begin with ... */
    char *strbeg  /* begin part of line */
);
/*-------------------------------------------------------------------------*/
    static void
OutLinesBegin(        /* function for Out lines from fileTmp begin with "strbeg" */
    char     *strbeg, /* begin part of line */
    unsigned what     /* indicator: what print */
);
/* static constants for OutLinesBegin(): */
# define    OUT_ALL     0
# define    OUT_FIRST   1
# define    OUT_LAST  (unsigned)(-1)

/*-------------------------------------------------------------------------*/
    int
Pass2(
    void
){
    int result;

    PassNo = 2;

    MsgConsole( msgPASSbegin, PassNo );
    MsgDebug( msgPASSbegin, PassNo );
    if( Flags[flagE].value ) MsgError( msgPASSbegin, PassNo );

    rewind( fileTmp );
 /* reopen start fileInp - in main() */

    InitPass2();
    P2debug( msgAVAILMEM, coreleft() ); /**/
    result =  yyparse(); /* 2nd pass */
    P2debug( msgAVAILMEM, coreleft() ); /**/

    /* skip line counter: */
    MsgConsole( "\n" );

    /* final statistic: */
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
    if( cntWarnings2 > 0 ){
        MsgDebug( msgTOTALWARNS, cntWarnings2 );
        MsgConsole( msgTOTALWARNS, cntWarnings2 );
        if( Flags[flagE].value ) MsgError( msgTOTALWARNS, cntWarnings2 );
    } else {
        MsgDebug( msgNOWARNS );
        MsgConsole( msgNOWARNS );
        if( Flags[flagE].value ) MsgError( msgNOWARNS );
    }

 /* MsgConsole( msgPASSend, PassNo ); /* message in *.Y */
    MsgDebug( msgPASSend, PassNo );
    if( Flags[flagE].value ) MsgError( msgPASSend, PassNo );

    PassNo = 3;  /* post-pass: final actions */

    HabElemPart();  /* Elements part */

    HabFreqPart();  /* Frequency part */

    HabVarPart();   /* Variations part */

    HabQuaPart();   /* Quality parameters part */

    MsgDebug( msgGENend );
    if( Flags[flagE].value ) MsgError( msgGENend );

    return result;
}/*Pass2()*/
/*-------------------------------------------------------------------------*/
    static void
InitPass2(   /* initializations for pass 2 */
    void
){
    exist_fi = FALSE; /* exist VJ model parameter for all diodes Schottky */


}/*InitPass2()*/
/*-------------------------------------------------------------------------*/
    char *
ftoa(           /* function for convert double to string */
    double var
){
    char *str;
    sprintf( linebuffer, "%g", var );
    if( ( str = (char *)malloc( strlen(linebuffer) + 1 ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC2 );
    strcpy( str, linebuffer );
    return str;
}/*ftoa()*/
/*-------------------------------------------------------------------------*/
    static void
HabElemPart(   /* print &TYP, &ELEM operators for all elements groups */
    void
){
    int    i;
    int    cntelem;
    int    isint;
    double dblparm;
    long   longparm;

    /* close and open for read temporary file: */
    fclose( fileTmp );
    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
    strcpy( linebuffer, nameDirTmp );
    strcat( linebuffer, sPATHSEPARATOR );
    strcat( linebuffer, nameFileTmp );
    fileTmp = fopen( linebuffer/*nameFileTmp*/, "r" );
    if( fileTmp == NULL )
        MsgErrorFatal( exitERRTMPCREAT, msgERRTMPCREAT, linebuffer/*nameFileTmp*/ );

/* C -> 'C   ' */
    cntelem = CountLinesBegin( " &ELEM NE='C" ); /* count tmp-lines begin with "..." */
    P2debug( msgP2TOTALELEM, cntelem, "C" ); /* debug diag about # of habelem */
    if( cntelem > 0 ){
        GetConfOption( "Gc", &isint, &dblparm, &longparm );
        for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
        sprintf( linebuffer, "%g", dblparm );
        HabTYPout( "'C   '", cntelem, /*Gc*/linebuffer );
        OutLinesBegin( " &ELEM NE='C", OUT_ALL ); /* Out lines from fileTmp begin with "..." */
    }

/* L -> 'L   ' */
    cntelem = CountLinesBegin( " &ELEM NE='L" ); /* count tmp-lines begin with "..." */
    P2debug( msgP2TOTALELEM, cntelem, "L" ); /* debug diag about # of habelem */
    if( cntelem > 0 ){
        GetConfOption( "Rl", &isint, &dblparm, &longparm );
        for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
        sprintf( linebuffer, "%g", dblparm );
        HabTYPout( "'L   '", cntelem, /*Rl*/linebuffer );
        OutLinesBegin( " &ELEM NE='L", OUT_ALL ); /* Out lines from fileTmp begin with "..." */
    }

/* R -> 'R   ' */
    cntelem = CountLinesBegin( " &ELEM NE='R" ); /* count tmp-lines begin with "..." */
    P2debug( msgP2TOTALELEM, cntelem, "R" ); /* debug diag about # of habelem */
    if( cntelem > 0 ){
        HabTYPout( "'R   '", cntelem, "0." );
        OutLinesBegin( " &ELEM NE='R", OUT_ALL ); /* Out lines from fileTmp begin with "..." */
    }

/* T, O -> 'LIB0','LL0 ' */
    cntelem  = CountLinesBegin( " &ELEM NE='T" ); /* count tmp-lines begin with "..." */
    cntelem += CountLinesBegin( " &ELEM NE='O" ); /* count tmp-lines begin with "..." */
    P2debug( msgP2TOTALELEM, cntelem, "LIB0 LL0" ); /* debug diag about # of habelem */
    if( cntelem > 0 ){
        GetConfOption( "Rline", &isint, &dblparm, &longparm );
        for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
        sprintf( linebuffer, "%g", dblparm );
        HabTYPout( "'LIB0', 'LL0 '", cntelem, /*Rline*/linebuffer );
        OutLinesBegin( " &ELEM NE='T", OUT_ALL ); /* Out lines from fileTmp begin with "..." */
        OutLinesBegin( " &ELEM NE='O", OUT_ALL ); /* Out lines from fileTmp begin with "..." */
    }

/* D -> 'VD  ','SCHT' */
    cntelem = CountLinesBegin( " &ELEM NE='D" ); /* count tmp-lines begin with "..." */
    P2debug( msgP2TOTALELEM, cntelem, "D" ); /* debug diag about # of habelem */
    if( cntelem > 0 ){
        for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
        sprintf( linebuffer, "%g,%g", glb_Diode_fi, glb_Diode_f0 );
        HabTYPout( "'VD  ', 'SCHT'", cntelem, /*fi,f0*/linebuffer );
        OutLinesBegin( " &ELEM NE='D", OUT_ALL ); /* Out lines from fileTmp begin with "..." */
    }

/* I, V -> 'J   ' */
    cntelem  = CountLinesBegin( " &ELEM NE='I" ); /* count tmp-lines begin with "..." */
    cntelem += CountLinesBegin( " &ELEM NE='V" ); /* count tmp-lines begin with "..." */
    P2debug( msgP2TOTALELEM, cntelem, "J" ); /* debug diag about # of habelem */
    if( cntelem > 0 ){
        HabTYPout( "'J   '", cntelem, ""/*nothing*/ );
        OutLinesBegin( " &ELEM NE='I", OUT_ALL ); /* Out lines from fileTmp begin with "..." */
        OutLinesBegin( " &ELEM NE='V", OUT_ALL ); /* Out lines from fileTmp begin with "..." */
    }

/* Z -> 'FET ','CURT' */
    cntelem = CountLinesBegin( " &ELEM NE='Z" ); /* count tmp-lines begin with "..." */
    P2debug( msgP2TOTALELEM, cntelem, "Z" ); /* debug diag about # of habelem */
    if( cntelem > 0 ){
        HabTYPout( "'FET ', 'CURT'", cntelem, ""/*no Param*/ );
        OutLinesBegin( " &ELEM NE='Z", OUT_ALL ); /* Out lines from fileTmp begin with "..." */
    }

    Out( "\n" );
    Out( " &TYP IT='END ' /\n" );
    Out( "\n" );

}/*HabElemPart()*/
/*-------------------------------------------------------------------------*/
    static void
HabTYPout(        /* generate Habala-operator '&TYP' */
    char *type,   /* IT='...' */
    long counter, /* KOL=... */
    char *param   /* P='...' */ /* PARAM='...' - old version */
){
    Out( "\n" );  /* delimit groups */
    if( strlen(param) > 0 )
      Out( " &TYP IT=%s, KOL=%ld, P=%s /\n", type, counter, param );
    else
      Out( " &TYP IT=%s, KOL=%ld /\n", type, counter );

}/*HabTYPout()*/
/*=========================================================================*/
    static void
HabFreqPart(    /* Frequency part */
    void
){
    P2debug( msgP2FREQPART ); /**/

 /* OutLinesBegin( " &FREQU", OUT_ALL );   /* Out all lines from fileTmp begin with " &FREQ" */
 /* OutLinesBegin( " &FREQU", OUT_FIRST ); /* Out first line from fileTmp begin with " &FREQ" */
    OutLinesBegin( " &FREQU", OUT_LAST );  /* Out last line from fileTmp begin with " &FREQ" */

    Out( "\n" );
}/*HabFreqPart()*/
/*=========================================================================*/
    static void
HabVarPart(     /* Variations part */
    void
){

    /*...*/

    Out( "\n" );
    Out( " &VAR FIN='END ' /\n" );
}/*HabVarPart()*/
/*=========================================================================*/
    static void
HabQuaPart(    /* Quality parameters part */
    void
){

    /*...*/

    Out( "\n" );
    Out( " &QUP IQ='END ' /\n" );
}/*HabQuaPart()*/
/*=========================================================================*/
    static int    /* return number of lines */
CountLinesBegin(  /* function for count lines from fileTmp begin with ... */
    char *strbeg  /* begin part of line */
){
    int  i;
    int  cnt = 0;

    rewind( fileTmp );

    while( !feof(fileTmp) ){
        for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0; /* clean buffer */
        if( fgets( linebuffer, maxINPLINELEN, fileTmp ) == NULL ) break;
        linebuffer[strlen(strbeg)] = '\0'; /* truncate input line */
        /* compare begin of string with 'strbeg': */
	if( strEQ( linebuffer, strbeg ) ) cnt++;
    }
 /* P2debug("CountLinesBegin(): "); SHOWint(cnt); /**/
    return cnt;
}/*CountLinesBegin()*/
/*-------------------------------------------------------------------------*/
    static void
OutLinesBegin(        /* function for Out lines from fileTmp begin with "strbeg" */
    char     *strbeg, /* begin part of line */
    unsigned what     /* indicator: what print */
){
    int  i, j, len, eq;
    static char savebuffer[maxINPLINELEN]; /* for temporary strings */

    rewind( fileTmp );

    while( !feof(fileTmp) ){

        for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0; /* clean buffer */
        if( fgets( linebuffer, maxINPLINELEN, fileTmp ) == NULL ) break;

        /* after fgets() linebuffer must be with '\n': */
        len = strlen(linebuffer);
        if( linebuffer[len-1] != '\n' ) strcat( linebuffer, "\n" );

        /* compare begin of string with 'strbeg': */
	for( j = 0, eq = TRUE; j < strlen(strbeg); j++ )
            if( linebuffer[j] != strbeg[j] ) eq = FALSE;

        /* if equal Out line to OBJ-file: */
        if( eq ){
          if( what != OUT_LAST ) 
            Out80( linebuffer ); /* long lines divide to 80-symb parts */
          if( what == OUT_FIRST ) break; /* print only first line */
          if( what == OUT_LAST ){
              for( i = 0; i < maxINPLINELEN; i++ ) savebuffer[i] = 0; /* clean buffer */
              strncpy( savebuffer, linebuffer, maxINPLINELEN );
          }
        }
    }
    /* print only last line: */
    if( what == OUT_LAST ) Out80( savebuffer ); /* long lines divide to 80-symb parts */

    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0; /* clean buffer */

}/*OutLinesBegin()*/
/*=========================================================================*/
    char *            /* return name of file if success */ /*not use*/
MakeCircBody(         /* function for make tmp-file with circuit body */
    SubCirc *circCall /* pointer subcircuit */
){
    int  i;
    long ln;   /* lines counter */
    FILE *fin; /* source-file contain circuit body */

    /* if subcircuit occupy more then one file - error: */
    if( !strEQ( circCall->begfname, circCall->endfname ) )
        MsgErrorFatal( exitERRSUBCIRPLACE, msgERRSUBCIRPLACE,
                       circCall->circname, circCall->begfname, circCall->endfname );

    /* generate temporary file name: */
    GetTmpFileName( nameFileInc, nameTMPMASK, cntTmpFiles++ );

    /* open this temporary file: */
    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
    strcpy( linebuffer, nameDirTmp );
    strcat( linebuffer, sPATHSEPARATOR );
    strcat( linebuffer, nameFileInc ); /* make full file name with path */
    fileInc = fopen( linebuffer/*nameFileInc*/, "w" );
    if( fileInc == NULL )
        MsgErrorFatal( exitERRTMPCREAT, msgERRTMPCREAT, linebuffer/*nameFileInc*/ );

    /* header-comment in subcircuit file (for debugging): */
    OutInc( msgCIRCFILEHEAD, circCall->circname,
        circCall->begfname, circCall->begline, circCall->endline );
    OutInc( "\n" ); /* empty line neccessary ! */

    /* fill temporary file with subcircuit body: */
    fin = fopen( circCall->begfname, "r" );
    if( fin == NULL )
        MsgErrorFatal( exitERRCIRCOPEN, msgERRCIRCOPEN,
                       circCall->begfname, circCall->circname );

    /* copy lines with circuit body: */
    for( ln = 1; ; ln++ ){
        /* get next line: */
        for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0; /*clear*/
        if( fgets( linebuffer, maxINPLINELEN, fin ) == NULL ) break;
             /* MsgErrorFatal( exitERRCIRCFAIL, msgERRCIRCFAIL,
                               circCall->begfname, circCall->circname ); */

        /* after fgets() linebuffer must be with '\n': */
        if( linebuffer[strlen(linebuffer)-1] != '\n' )
            strcat( linebuffer, "\n" );

        if( circCall->begline <= ln && ln <= circCall->endline ){
           #ifdef deBug
            OutInc( "/*%ld*/ ", ln ); /* debug line number */
           #endif
            OutInc( linebuffer );
        }
    }
    /* OutInc( "\n" ); /* empty line neccessary ! */
    fclose( fin );

    /* close temporary file: */
    fclose( fileInc );

    return nameFileInc;
}/*MakeCircBody()*/
/*=========================================================================*/
    SubParm *          /* returned pointer to list of parameters */
AddSubParm(            /* function to add node-parameter to parameters list */
    SubParm *parmlist, /* subcall parameters list */
    char    *parmname, /* name of add parameter */
    long    *parmcnt   /* number of new parameter in list */
){
    int     cnt;
    SubParm *newparm, *ptr;

    /* allocate new element: */
    if( ( newparm = (SubParm *)malloc( sizeof(SubParm) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC2 );
    newparm->parmname = parmname; /* new parameter name */
    newparm->nextparm = NULL;     /* pointer to next parm */

    if( parmlist != NULL ){ /* goto tail of list: */
        for( ptr = parmlist, cnt = 1; ptr->nextparm != NULL;
                                      ptr = ptr->nextparm, cnt++ ) ;
        /* add new element: */
        ptr->nextparm = newparm;
        cnt++;
    } else { /* new parm is head of list: */
        parmlist = newparm;
        cnt = 1;
    }
    *parmcnt = cnt;
    return parmlist;
}/*AddSubParm()*/
/*-------------------------------------------------------------------------*/
    void
PrintSubParm(          /* debug print of subcirc call parameters list */
    SubParm *parmlist  /* subcall parameters list */
){
    SubParm *pl;
    if( parmlist == NULL ){
      MsgDebug( msgEMPTY );
      MsgDebug( "\n" );
      return;
    }
    for( pl = parmlist; pl != NULL; pl = pl->nextparm ){
      MsgDebug( " %s", pl->parmname);
      /* SHOWptr(pl);/**/
    }
    MsgDebug( "\n" );
}/*PrintSubParm()*/
/*-------------------------------------------------------------------------*/
    void
DelParmSubCall(        /* empty subcirc call parameters list */
    SubParm *parmlist  /* subcall parameters list */
){                     /* no freed memory really ! */
    SubParm *pn, *pp;

    /*MsgDebug( msgAVAILMEM, coreleft() ); /**/
    pn = parmlist;
    while( pn ){
      pp = pn;
      pn = pn->nextparm;
    /*SHOWptr(pp);/**/
      free( pp );
    }
    /*MsgDebug( msgAVAILMEM, coreleft() ); /**/
}/*DelParmSubCall()*/
/*-------------------------------------------------------------------------*/
    int                  /* returned TRUE if OK of FALSE if fail function for */
RenameLocalNodes(        /* rename local nodes in subcircuit before it subcall */
    SubCirc *circcall,   /* called subcircuit */
    SubCirc *circcurr,   /* subcircuit-caller */
    SubParm *parmsubcall /* fact parameters in subcall */
){
    int     cnt;
    SubParm *pl;
    Node    *nl, *lst, *fnd, *pnl;

    if( circcall == NULL || parmsubcall == NULL ) return FALSE;

/* 1: substitute fact parameters: */
    P2debug( "RenameLocalNodes(): \n1) rename parameters:\n" );/**/
    for(
      pl = parmsubcall,  nl = circcall->nodeparmlist, cnt = 0;
      pl != NULL,        nl != NULL,                  cnt < circcall->cntnodesparm;
      pl = pl->nextparm, nl = nl ->nextnode,          cnt++
    ){
      /* list of nodes in subcircuit-caller: */
      lst = (circcurr == NULL)? listNodesMain: circcurr->circnodlist;/*ASB000408*/
      /* old: list of nodes in parent circuit: */
      /*lst = (circcall->parent == NULL)? listNodesMain: circcall->parent->circnodlist;/**/

      /* copy name of fact-parm to habala-name of node-parameter of circuit: */
   /* strncpy( nl->newname, pl->parmname, lenHABNAME ); /* - wrong */
        /* instead of spice-name 'pl->parmname' of fact parameter */
        /* must be substituted habala-translation of this nodes names */
        /* in parent subcircuit: */
      fnd = FindNode(
              lst,
              pl->parmname /* node name */
      );
      strncpy( nl->newname, fnd->newname, lenHABNAME );

      P2debug( "  subst '%s'\tto '%s'  \t-> '%-4s'",
          nl->nodename, pl->parmname, fnd->newname );/**/
      P2debug( " (from circ-caller '" );/**/ /*old: P2debug( "\tfrom parent circ " );/**/
      if( Flags[flag2].value ) OutFullCircName( fileDebug, circcall->parent );/**/
      P2debug( "')\n" );/**/
    }

/* 2: rename local nodes: */
    P2debug( "RenameLocalNodes(): \n2) rename local nodes:\n" );/**/
    P2debug( "\tcircuit '%s', start rename nodes number from %ld\n",
                         circcall->circname,        renNodes );
 /* SHOWptr( circcall->circnodlist );/**/
    /* clear Habala-nodes names is subcircuit, '0' -> '0   ': */
    ClrNodesName( circcall->circnodlist );
    /* replace nodes-parameter by fact parameters: */
    for( nl = circcall->circnodlist; nl != NULL; nl = nl->nextnode )
      for( pnl = circcall->nodeparmlist; pnl != NULL; pnl = pnl->nextnode )
        if( streq(nl->nodename, pnl->nodename) )
          strncpy( nl->newname, pnl->newname, lenHABNAME );
    /* rename others nodes: */
    RenameNodes( circcall->circnodlist );

    return TRUE;
}/*RenameLocalNodes()*/
/*=========================================================================*/
    void
AddCallPath(       /* add Xxxx-name to circuit call path in 'stackCallsHead' */
    char *callname /* name of current subcall 'Xxxx' */
){
    StackCalls *ne;

 /* MsgDebug( "AddCallPath('%s')\n", callname );/**/

    /* allocate new element: */
    if( ( ne = (StackCalls *)malloc( sizeof(StackCalls) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC2 );

    ne->callname = callname;       /* name of current subcall 'Xxxx' */
    ne->prev     = stackCallsTail; /* next element of stack */
    ne->next     = NULL;           /* previous element of stack */

    if( stackCallsHead == NULL )
        stackCallsHead = ne;       /* if 1st element - create head */
    else
        stackCallsTail->next = ne; /* if next element - append it to tail */

    stackCallsTail = ne;
}/*AddCallPath()*/
/*-------------------------------------------------------------------------*/
    void
RemCallPath(       /* remove last Xxxx-name from circuit call path 'stackCallsHead' */
    void
){
    StackCalls *tmp = stackCallsTail;

    if( stackCallsHead == NULL || stackCallsTail == NULL ) /* path empty */
        return;

    if( stackCallsHead == stackCallsTail ){ /* remove single element from path */
        stackCallsHead = stackCallsTail = NULL;
    }else{ /* remove last element from path */
        stackCallsTail->prev->next = NULL;
        stackCallsTail = stackCallsTail->prev;
    }
    free( tmp );

}/*RemCallPath()*/
/*-------------------------------------------------------------------------*/
    void
OutCallPath(       /* print circuit call path from 'stackCallsHead' */
    FILE    *ofil  /* file for out */
){
    StackCalls *ptr;
    OutFile( ofil, "/" );
    for( ptr = stackCallsHead; ptr != NULL; ptr = ptr->next ){
        OutFile( ofil, ptr->callname );
        OutFile( ofil, "/" );
    }
}/*PrintCallPath()*/
/*=========================================================================*/
    int       /* returned TRUE if OK, FALSE if fail */
MkElemOxxx(   /* function for making habala-code for element Oxxx */
    char *elemname, /* (spice) element name */
    char *nodebuf,  /* habala nodes-parameters list together */
    char *modelname /* model name */
){
    int       i;
    int       exist;
    ModelList *model;
    SubCirc   *circ;
    double    R      = 0.0,
              G      = 0.0,
              L      = 0.0,
              C      = 0.0,
              parmZ0 = 0.0,
              LEN    = 0.0,
              Length = 0.0;

    /* find model in current circuit and higher: */
    for( circ = circDefCurrent; ; circ = circ->parent ){
      if( Flags[flag2].value ){
        MsgDebug( "Find model in circuit " );
        OutFullCircName( fileDebug, circ);
        MsgDebug( "...\n" );
      }
      model = FindModel( modelname, circ );
      if( model != NULL ) break;
      if( circ == NULL ) break;
    }
    if( model == NULL ){
      MsgCompile( msgMODELNOTFOUND, modelname );
      return FALSE;
    }else
      if( Flags[flag2].value ){
        MsgDebug( "Model '%s' find in circuit ", modelname );
        OutFullCircName( fileDebug, circ);
        MsgDebug( "\n" );
      }

    /* test model type: */
    if( !strEQ( model->modeltype, "LTRA" ) ){
      MsgCompile( msgILLMODELTYPE, modelname, elemname, model->modeltype );
      return FALSE;
    }

    /* find required parameters and test values of required parameters: */
    /* R, G >= 0 (TZ0C13)*/					/*ASB001223*/
    GETORSETDEFPARMgeVAL( /* must be >= */ 0.0, /* or assign to */ 0.0,
                                    "R", R, model, modelname, elemname );
    GETORSETDEFPARMgeVAL( 0.0, 0.0, "G", G, model, modelname, elemname );
/* rem according to TZ0C13:
    ** R, G == 0 **
    GETORSETDEFPARMeqVAL( 0.0, 0.0, "R", R, model, modelname, elemname );
    GETORSETDEFPARMeqVAL( 0.0, 0.0, "G", G, model, modelname, elemname );
*/
/* rem according to TZ0530:
    GETANDCHKPARMeqVAL( 0.0, "R", R, model, modelname, elemname );
    GETANDCHKPARMeqVAL( 0.0, "G", G, model, modelname, elemname );
*/
    /* L, C, LEN > 0 */
    GETANDCHKPARMgtVAL( /* must be > */ 0.0, /* or error */
                             "L", L, model, modelname, elemname );
    GETANDCHKPARMgtVAL( 0.0, "C", C, model, modelname, elemname );
    GETANDCHKPARMgtVAL( 0.0, "LEN", LEN, model, modelname, elemname );

    /* calculations: */
    if( C != 0 ) parmZ0 = sqrt( L / C );
    Length = LEN * const_c * sqrt( L * C );

    /* code generation: */
    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
    sprintf( linebuffer, "%g, %g", parmZ0, Length );
    HabELEMout(
      /*NE=  */GetHabElemName( elemname, circDefCurrent, listO ),
      /*KNOT=*/nodebuf,
      /*PAR= */linebuffer,
      /*IDOP=*/"",
      /*ISTR=*/""
              );

    return TRUE;
}/*MkElemOxxx()*/
/*-------------------------------------------------------------------------*/
    int       /* returned TRUE if OK, FALSE if fail */
MkElemDxxx(   /* function for making habala-code for element Dxxx */
    char   *elemname,   /* (spice) element name */
    char   *nodebuf,    /* habala nodes-parameters list together */
    char   *modelname,  /* model name */
    double parmDiodArea /* diode parameter AREA */
){
    int       i;
/*  int       exist; /* for test model parameter macros */
    ModelList *model;
    SubCirc   *circ;
    int       isint;    long      longparm;  /* for GetConfOption() */
    double    spice_RS, spice_IS, spice_N, spice_TT, spice_CJ0, spice_M, spice_VJ;
    double    Rsmin = 0,
              Rs    = 0.0,
              Cp    = 0.0,
              Ls    = 0.0,
              I0    = 0.0,
              C0    = 0.0,
              a     = 0.0,
              U0    = 0.0,
              DU    = 0.0,
              k_Diode_f0 = 0.0;

    /* find model in current circuit and higher: */
    model = UpFindModel( modelname );

    /* test model type: */
    if( !strEQ( model->modeltype, "D" ) ){
      MsgCompile( msgILLMODELTYPE, modelname, elemname, model->modeltype );
      return FALSE;
    }

    /* get Rsmin, U0, DU from config file S2H.CFG: */
    GetConfOption( "Rsmin", &isint, &Rsmin, &longparm );
    GetConfOption( "U0", &isint, &U0, &longparm );
    GetConfOption( "DU", &isint, &DU, &longparm );

    /* find required parameters and test values of required parameters: */
    /* RS > 0 (!=0): */
    GETDEFCHKPARMgtVAL( 0.0, Rsmin, "RS", spice_RS, model, modelname, elemname );
    /* IS >= 0: */
    GETDEFCHKPARMgeVAL( 0.0, def_DiodIS, "IS", spice_IS, model, modelname, elemname );
    /* N > 0 (!=0): */
    GETDEFCHKPARMgtVAL( 0.0, def_DiodN, "N", spice_N, model, modelname, elemname );
    /* CJ0 >= 0: */
    GETDEFCHKPARMgeVAL( 0.0, def_DiodCJ0, "CJ0", spice_CJ0, model, modelname, elemname );
    /* VJ > 0: */
    GETDEFCHKPARMgtVAL( 0.0, def_DiodVJ, "VJ", spice_VJ, model, modelname, elemname );

    /* TT == def_DiodTT */
    GETDEFCHKPARMeqVAL( def_DiodTT, def_DiodTT, "TT", spice_TT, model, modelname, elemname );
    /* old: GETANDCHKPARMeqVAL( 0.0, "TT", spice_TT, model, modelname, elemname );*/
    /* M == def_DiodM */
    GETDEFCHKPARMeqVAL( def_DiodM, def_DiodM, "M", spice_M, model, modelname, elemname );
    /* old: GETANDCHKPARMeqVAL( def_DiodM, "M", spice_M, model, modelname, elemname );*/

    /* calculations: */
    if( parmDiodArea != 0 ) Rs = spice_RS / parmDiodArea;
    else                    Rs = spice_RS; /* parmDiodArea = 1.0 by default */
    Cp = 0.0;
    Ls = 0.0;
    I0 = spice_IS * parmDiodArea;
    C0 = spice_CJ0 * parmDiodArea;
    a = const_q / ( const_k * const_T * spice_N );

    if( exist_fi && spice_VJ != glb_Diode_fi ){
      MsgCompile( msgNOTEQPARMVJ, spice_VJ, glb_Diode_fi );
      return FALSE;
    }else{
      exist_fi = TRUE;
      glb_Diode_fi = spice_VJ;
      GetConfOption( "k_Diode_f0", &isint, &k_Diode_f0, &longparm );
      glb_Diode_f0 = k_Diode_f0 * glb_Diode_fi;
    }
    /* code generation: */
    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
    sprintf( linebuffer, "%g, %g, %g, %g, %g, %g, %g, %g",
                          Rs, Cp, Ls, I0, C0, a,  U0, DU );
    HabELEMout(
      /*NE=  */GetHabElemName( elemname, circDefCurrent, listD ),
      /*KNOT=*/nodebuf,
      /*PAR= */linebuffer,
      /*IDOP=*/"",
      /*ISTR=*/""
              );

    return TRUE;
}/*MkElemDxxx()*/
/*=========================================================================*/
    void
MkElemIxxx(   /* function for making habala-code for element Ixxx */
    char   *elemname, /* (spice) element name */
    char   *nodebuf,  /* habala nodes-parameters list together */
    double Gi,        /* Gi habala parameter */
    double F,         /* F  habala parameter */
    double Fi,        /* Fi habala parameter */
    double Jm         /* Jm habala parameter */
){
    int       i;

 /* MsgDebug( "MkElemIxxx(): elemname='%s', nodebuf=%s, Gi=%g, F=%g, Fi=%g, Jm=%g\n",
                                       elemname,  nodebuf, Gi, F, Fi, Jm );/**/

    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
    sprintf( linebuffer, "%g, %g, %g, %g", Gi, F, Fi, Jm );
    HabELEMout(
      /*NE=  */GetHabElemName( elemname, circDefCurrent, listI ),
      /*KNOT=*/nodebuf,
      /*PAR= */linebuffer,
      /*IDOP=*/"",
      /*ISTR=*/""
              );
}/*MkElemIxxx()*/
/*-------------------------------------------------------------------------*/
    void
MkElemVxxx(   /* function for making habala-code for element Vxxx */
    char   *elemname, /* (spice) element name */
    char   *nodebuf,  /* habala nodes-parameters list together */
    double Gi,        /* Gi habala parameter */
    double F,         /* F  habala parameter */
    double Fi,        /* Fi habala parameter */
    double Jm         /* Jm habala parameter */
){
    int       i;

 /* MsgDebug( "MkElemVxxx(): elemname='%s', nodebuf=%s, Gi=%g, F=%g, Fi=%g, Jm=%g\n",
                                       elemname,  nodebuf, Gi, F, Fi, Jm );/**/

    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
    sprintf( linebuffer, "%g, %g, %g, %g", Gi, F, Fi, Jm );
    HabELEMout(
      /*NE=  */GetHabElemName( elemname, circDefCurrent, listV ),
      /*KNOT=*/nodebuf,
      /*PAR= */linebuffer,
      /*IDOP=*/"",
      /*ISTR=*/""
              );
}/*MkElemVxxx()*/
/*-------------------------------------------------------------------------*/
    void
IVxxxParmFromDC(     /* get habala parameters from DC value */
    double *Gi,      /* returned Gi habala parameter */
    double *F,       /* returned F  habala parameter */
    double *Fi,      /* returned Fi habala parameter */
    double *Jm,      /* returned Jm habala parameter */
    double DCvalue   /* DC value */
){
    *Gi = 0.0;
    *F  = 0.0;
    *Fi = 0.0;
    *Jm = DCvalue;
}/*IVxxxParmFromDC()*/
/*-------------------------------------------------------------------------*/
    void
IVxxxParmFromDISTOF( /* get habala parameters from DISTOF values */
    double *Gi,      /* returned Gi habala parameter */
    double *F,       /* returned F  habala parameter */
    double *Fi,      /* returned Fi habala parameter */
    double *Jm,      /* returned Jm habala parameter */
    int    DISTOFno, /* =1 for DISTOF1, =2 for DISTOF2 */
    double value1,   /* DISTOF value #1 */
    double value2    /* DISTOF value #2 */
){
    *Gi = 0.0;
    switch(DISTOFno){
    case 1: *F = parm_HB_F1;
            break;
    case 2: *F = parm_HB_F2;
            break;
    }
/*  *Fi = value2 * ( 1 / 180 * const_Pi ) - const_Pi/2; /*rem ASB001223*/
    *Fi = - const_Pi/2; /*ASB001223*/
    *Jm = value1;
}/*IVxxxParmFromDISTOF()*/
/*-------------------------------------------------------------------------*/
    int       /* returned TRUE if OK, FALSE if fail */
MkElemZxxx(   /* function for making habala-code for element Zxxx */
    char   *elemname,     /* (spice) element name */
    char   *nodebuf,      /* habala nodes-parameters list together */
    char   *modelname,    /* model name */
    double parmMesfetArea /* mesfet parameter AREA */
){
    int       i;
    ModelList *model;
    double    Rtmp;

    /* model parameters: */
/*  int       exist; /* for test model parameter macros */
    double    mpVTO_S,
              mpBETA_S,
              mpB_S,
              mpALPHA_S,
              mpLAMBDA_S,
              mpRD_S,
              mpRS_S,
              mpCGS_S,
              mpCGD_S,
              mpPB_S,
              mpKF_S,  /* not use */
              mpAF_S,  /* not use */
              mpFC_S;  /* not use */

    /* Zxxx-parameters: */
    double    R1       = 0.0,
              R2       = 0.0,
              R3       = 0.0;
    double    C12      = 0.0,
              C13      = 0.0;
    double    ALPHA_H  = 0.0,
              BETA_H   = 0.0,
              LAMBDA_H = 0.0;
    double    VTH      = 0.0,
              IS0      = 0.0,
              AS_      = 0.0; /*AS*/
    double    C230     = 0.0,
              VBI_H    = 0.0;
    double    R31      = 0.0,
              R32      = 0.0;
    double    UBOUND   = 0.0,
              STEPM    = 0.0;

    /* configuration parameters: */
    int       isint;    long      longparm;  /* for GetConfOption() */
    double    MESFET_RDMIN,
              MESFET_RG,
              MESFET_RSMIN,
              MESFET_CDS,
              MESFET_IS0,
              MESFET_N=1.0,
              MESFET_RDS,
              MESFET_RGS,
              MESFET_UBOUND,
              MESFET_STERM;

 /* MsgDebug("MkElemZxxx('%s','%s','%s',%g): ...\n",
                      elemname, nodebuf, modelname, parmMesfetArea );/**/

    /* get configuration parameters: */
    GetConfOption( "MESFET_RDMIN",   &isint, &MESFET_RDMIN,  &longparm );
    GetConfOption( "MESFET_RG",      &isint, &MESFET_RG,     &longparm );
    GetConfOption( "MESFET_RSMIN",   &isint, &MESFET_RSMIN,  &longparm );
    GetConfOption( "MESFET_CDS",     &isint, &MESFET_CDS,    &longparm );
    GetConfOption( "MESFET_IS0",     &isint, &MESFET_IS0,    &longparm );
    GetConfOption( "MESFET_N",       &isint, &MESFET_N,      &longparm );
    GetConfOption( "MESFET_RDS",     &isint, &MESFET_RDS,    &longparm );
    GetConfOption( "MESFET_RGS",     &isint, &MESFET_RGS,    &longparm );
    GetConfOption( "MESFET_UBOUND",  &isint, &MESFET_UBOUND, &longparm );
    GetConfOption( "MESFET_STERM",   &isint, &MESFET_STERM,  &longparm );

    /* find model in current circuit and higher: */
    model = UpFindModel( modelname );

    /* test model type: */
    if( !strEQ( model->modeltype, "NMF" ) && !strEQ( model->modeltype, "PMF" ) ){
      MsgCompile( msgILLMODELTYPE, modelname, elemname, model->modeltype );
      return FALSE;
    }

    /* find required parameters and test values of required parameters: */
/* R1: */
    GETORSETDEFPARM( def_MesfetRD_S, "RD_S", mpRD_S,
                     model, modelname, elemname );
    if( mpRD_S < MESFET_RDMIN ) Rtmp = MESFET_RDMIN;
    else                        Rtmp = mpRD_S;
    R1 = Rtmp / parmMesfetArea;

/* R2: */
    R2 = MESFET_RG;

/* R3: */
    GETORSETDEFPARM( def_MesfetRS_S, "RS_S", mpRS_S,
                     model, modelname, elemname );
    if( mpRS_S < MESFET_RSMIN ) Rtmp = MESFET_RSMIN;
    else                        Rtmp = mpRS_S;
    R3 = Rtmp / parmMesfetArea;

/* C12: */
    GETORSETDEFPARM( def_MesfetCGD_S, "CGD_S", mpCGD_S,
                     model, modelname, elemname );
    C12 = mpCGD_S;

/* C13: */
    C13 = MESFET_CDS;

/* ALPHA_H: */
    GETORSETDEFPARM( def_MesfetALPHA_S, "ALPHA_S", mpALPHA_S,
                     model, modelname, elemname );
    ALPHA_H = mpALPHA_S;

/* BETA_H: */
    GETORSETDEFPARM( def_MesfetBETA_S, "BETA_S", mpBETA_S,
                     model, modelname, elemname );
    BETA_H = mpBETA_S * parmMesfetArea;

/* LAMBDA_H: */
    GETORSETDEFPARM( def_MesfetLAMBDA_S, "LAMBDA_S", mpLAMBDA_S,
                     model, modelname, elemname );
    LAMBDA_H = mpLAMBDA_S;

/* VTH: */
    GETORSETDEFPARM( def_MesfetVTO_S, "VTO_S", mpVTO_S,
                     model, modelname, elemname );
    VTH = -mpVTO_S;

/* IS0: */
    IS0 = MESFET_IS0;

/* AS: */
    if( MESFET_N == 0 ){
      MsgCompileWarn( warn1_INFO, msgMESFET_N_mb_ne0 );
      MESFET_N = def_MESFET_N;
    }
    AS_ = const_q / ( const_k * MESFET_N );

/* C230: */
    GETORSETDEFPARM( def_MesfetCGS_S, "CGS_S", mpCGS_S,
                     model, modelname, elemname );
    C230 = mpCGS_S;

/* VBI_H: */
    GETORSETDEFPARM( def_MesfetPB_S, "PB_S", mpPB_S,
                     model, modelname, elemname );
    VBI_H = mpPB_S;

/* R31: */
    if( MESFET_RDS == 0 ){
      MsgCompileWarn( warn1_INFO, msgMESFET_RDS_mb_ne0 );
      MESFET_RDS = def_MESFET_RDS;
    }
    R31 = MESFET_RDS;

/* R32: */
    if( MESFET_RGS == 0 ){
      MsgCompileWarn( warn1_INFO, msgMESFET_RGS_mb_ne0 );
      MESFET_RGS = def_MESFET_RGS;
    }
    R32 = MESFET_RGS;

/* UBOUND: */
    if( MESFET_UBOUND == 0 ){
      MsgCompileWarn( warn1_INFO, msgMESFET_UBOUND_mb_ne0 );
      MESFET_UBOUND = def_MESFET_UBOUND;
    }
    UBOUND = MESFET_UBOUND;

/* STEPM: */
    if( MESFET_STERM == 0 ){
      MsgCompileWarn( warn1_INFO, msgMESFET_STERM_mb_ne0 );
      MESFET_STERM = def_MESFET_STERM;
    }
    STEPM = MESFET_STERM;

    /* code generation: */
    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
    sprintf( linebuffer,
      "%g, %g, %g, %g, %g,  %g, %g, %g,  %g, %g, %g, %g, %g,  %g, %g, %g, %g",
       R1, R2, R3, C12,C13,
                            ALPHA_H,BETA_H,LAMBDA_H,
                                         VTH,IS0,AS_,C230,VBI_H,
                                                              R31,R32,UBOUND,
                                                                       STEPM );
    HabELEMout(
      /*NE=  */GetHabElemName( elemname, circDefCurrent, listZ ),
      /*KNOT=*/nodebuf,
      /*PAR= */linebuffer,
      /*IDOP=*/"",
      /*ISTR=*/""
              );

    return TRUE;
}/*MkElemZxxx()*/
/*=========================================================================*/
    static  void
P2debug(     /* print debug message */
    char    *msgfmt,    /* need '\n' in message */
            ...
){
    va_list arguments;
    va_start( arguments, msgfmt );

    if( Flags[flag2].value ){
      vsprintf( linebuffer, msgfmt, arguments );
      MsgDebug( linebuffer );
      va_end( arguments );
    /*MsgDebug( "\n");/**/
    }
}/*P2debug()*/
/*=========================================================================*/
