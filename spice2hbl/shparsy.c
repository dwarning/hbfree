
/*
 *  S H p a r s Y . C  --  S2H-compiler: C-part of Grammar parser
 *
 *  - contain all static functions from SHpars.Y and keywords table
 *
 *  03.06.2000 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */
/*=========================================================================*/

# include   "shcommon.h"
# include   "y.tab.h"

/*-------------------------------------------------------------------------*/

static char linebuffer[maxINPLINELEN]; /* for temporary strings */

/*-------------------------------------------------------------------------*/

/* keywords table: */
KeysTab KeywordsTab[] = {
    "AC",       AC,     /*ALT_CURR*/
    "AD",       AD,       /* for MOSFET */
    "AS",       AS,       /* for MOSFET */
    "CUR",      CUR,
    "DC",       DC,
    "DEC",      DEC,
    "DISTOF1",  DISTOF1,
    "DISTOF2",  DISTOF2,
    "DISTO",    DISTO,  /*DISTORTION*/
    "EXP",      EXP,
    "F",        F_FREQ,
    "I",        I_SRC,
    "IC",       IC,
 /* "JUNC",     JUNC,     /* for Habala only */
    "L",        L_LEN,
    "LIN",      LIN,
    "N",        N_LUMPS,
    "NL",       NL,
    "NOISE",    NOISE,
    "NRD",      NRD,      /* for MOSFET */
    "NRS",      NRS,      /* for MOSFET */
    "OCT",      OCT,
    "OFF",      OFF,
    "ON",       ON,
    "PD",       PD,       /* for MOSFET */
    "POL",      POL,
 /* "POLY5",    POLY5,    /* for Habala only */
    "PS",       PS,       /* for MOSFET */
    "PULSE",    PULSE,
    "PWL",      PWL,    /*PieceWiseLinear*/
    "PZ",       PZ,     /*POZ*/
    "SFFM",     SFFM,
    "SIN",      SIN,
    "TD",       TD,
    "TRAN",     TRAN,
    "TEMP",     TEMP,
    "UIC",      UIC,    /*TR_OPT*/
    "V",        V_SRC,
    "VDB",      VDB,      /* for .PRINT/.PLOT */
    "VI",       VI,       /* for .PRINT/.PLOT */
    "VM",       VM,       /* for .PRINT/.PLOT */
    "VOL",      VOL,
    "VP",       VP,       /* for .PRINT/.PLOT */
    "VR",       VR,       /* for .PRINT/.PLOT */
    "W",        W_WIDTH,
    "Z0",       Z0,     /*IMPEDENCE*/
    "ZER",      ZER,

    NULL,       0
};

/*=========================================================================*/
    char *
ShowKeyword( /* return image of token */
    long tokencode
){
    static char str_notfound[] = "<?>";
    static char str_unknown[]  = "<unknown>";
    KeysTab     *kt;

    for( kt = KeywordsTab; kt->value != 0; ++kt )
        if( tokencode == kt->value )  return kt->string;

    if( tokencode == tokenUNKNOWN ) return str_unknown;
    return str_notfound;
}/*ShowKeyword()*/
/*-------------------------------------------------------------------------*/
    int     /* returned number of equal token or 0 */
CheckKeyword(       /* function for find allowed token */
    char *ident,    /* (case of words ignored) */
 /* char *key1, */
         ...
 /* last keyN must be "" ! */
){
    int     i;
    char    *nextword;
    va_list ap;

    va_start( ap, ident );
    for( i = 1; ; i++ ){
        nextword = va_arg( ap, char * );
        if( strlen(nextword) == 0 ) break;
 /*     if( streq( strupr(ident), strupr(nextword) ) ) return i; /**/
        if( strEQ( ident, nextword ) ) return i;
    }
    va_end( ap );
    return 0;
}/*MsgAllowKeyword()*/
/*-------------------------------------------------------------------------*/
    void
MsgAllowKeywords(   /* print message about allowed keywords */
    char *msgno,  /* error code number */ /* Don't use '\n' in message */
    char *msgfmt,
    char *ident,
 /* char *key1, */
         ...
 /* last keyN must be "" ! */
){
    int     i, parm1st;
    char    *nextword;
    va_list ap;

    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;

    sprintf( linebuffer, msgfmt, ident ); /*strupr(ident)*/

    va_start( ap, ident );
    parm1st = TRUE;
    for(;;){
        nextword = va_arg( ap, char * );
        if( strlen(nextword) == 0 ) break;
        if( !parm1st ) strcat( linebuffer, ", " );
        parm1st = FALSE;
        strcat( linebuffer, nextword );
    }
 /* MsgCompile( linebuffer ); */
    MsgCompile( msgno, linebuffer ); /* ASB000626 */

    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
    va_end( ap );
}/*MsgAllowKeywords()*/
/*-------------------------------------------------------------------------*/
    int
LookupKeyword(  /* look up keyword */ /*ASB000124*/
    char    *str
){
    KeysTab *kt;
/*
    static char ucstr[maxINPLINELEN];
    strcpy(ucstr, str);
    strupr(ucstr);
*/
    for( kt = KeywordsTab; kt->string != NULL; ++kt )
        if( strEQ( str, kt->string ) )  return kt->value;
/*      if( streq( ucstr, kt->string ) )  return kt->value; */
    return FALSE;
}/*LookupKeyword()*/
/*=========================================================================*/
    char *
FindToken(  /* return of token 'type' */
    int     type
){
    KeysTab     *kt;
    int         i;
    static char buf[maxINPLINELEN];
    for( i = 0; i < maxINPLINELEN; i++ ) buf[i] = 0;
    if( type < 256 ){
        switch(type) {
        case '\t':  strncpy( buf, "\\t", maxINPLINELEN );
                    return buf;
        case '\n':  strncpy( buf, "\\n", maxINPLINELEN );
                    return buf;
        case '\0':  strncpy( buf, "\\0", maxINPLINELEN );
                    return buf;
        default:    sprintf( buf, "%c", type );
                    return buf;
        }
    }else if( type == 256 ){
        strncpy( buf, "<ERROR_TOKEN>", maxINPLINELEN );
        return buf;
    }else if( type == STRING ){
        sprintf( buf, "\"%s\"", yyTokenValue.strptr );
        return buf;
    }else if( type == IDENTNAME ){
        return yyTokenValue.strptr;
    }else if( type == DECINTNUM ){
        sprintf( buf, "%ld", yyTokenValue.longnum );
        return buf;
    }else if( type == FLOATNUM ){
        sprintf( buf, "%g", yyTokenValue.floatnum ); /* %g = %e or %f */
        return buf;
    }else{ /* keyword or network element */
        for( kt = OperKeysTab; kt->value != 0; ++kt ) /*ASB000124*/
            if( kt->value == type )
                return kt->string;
        for( kt = KeywordsTab; kt->value != 0; ++kt )
            if( kt->value == type )
                return kt->string;
        for( kt = NetElemPrefTab; kt->value != 0; ++kt )
            if( kt->value == type ){
                sprintf( buf, "%s='%s'", kt->string, yyTokenValue.strptr );
                return buf;
            }
    }
    strncpy( buf, "<UNKNOWN_TOKEN>", maxINPLINELEN );
    return buf;
}/*FindToken*/
/*-------------------------------------------------------------------------*/
    void
CheckCircBrackets( /* check numbers of .subckt = .ends, .end */
    void
){
    /* compare counters of circuit brackets: */
    if( cnt_SUBCKT != cnt_ENDS ){
        MsgErrorCnt( msgERR_SUBCKTCNT, cnt_SUBCKT, cnt_ENDS );
                     /* MsgErrorCnt() includes increase cntErrors */
    }
    /* check if .END not detected: */
    if( cnt_END != 1 ){
        MsgErrorCnt( msgERR_ENDNOTFND/*"CheckCircBrackets()"*/ );
                     /* MsgErrorCnt() includes increase cntErrors */
    }
}/*CheckCircBrackets()*/
/*-------------------------------------------------------------------------*/
# define    YDEBUGLEX   /**/
/*# define    FINDLEX     /**/
    int
yyerror(        /* standard YACC error message procedure */
    char    *msg
){
# ifdef  YYERRENABLE
        MsgError( "YYERROR() -- %s", msg );
        if(yyTokenLine) MsgError( ", line %d", yyTokenLine );
        if(yyTokenPos)  MsgError( ", pos %d", yyTokenPos );
        MsgError( ", on input " );
 # ifdef  FINDLEX
                /* non-correct: */
        MsgError( "'%s'", FindToken(yyTokenType) );
 # else
                /* ? correct: */
        /* instead of FindToken(yyTokenType) */
        if(yyTokenType/*yychar*/ > 256)
                MsgError( "'%s'", yyTokenText/*yysterm[yychar-257]*/ );
        else
                switch(yyTokenType) {
                case '\t':  MsgError( "'\\t'" ); break;
                case '\n':  MsgError( "'\\n'" ); break;
                case '\0':  MsgError( "'\\0'" ); break;
                default:    MsgError( "'%c'", yychar ); break;
                }
 # endif/*FINDLEX*/
 # ifdef  YDEBUGLEX
        MsgError( " (token type #%d)", yyTokenType );
 # endif/*YDEBUGLEX*/
        MsgError( "\n", yyTokenType );

# endif/*YYERRENABLE*/
    return strlen(msg);/**/
}/*yyerror*/
/*-------------------------------------------------------------------------*/
    void
Ydebug( /* Print debug message */  /* Don't use '\n' in message */
    char    *msgfmt,
            ...
){
    int i;
    va_list arguments;
    va_start( arguments, msgfmt );

    if( yyTokenLine <= 0 ) return; /*ASB000119*/

    if( Flags[flagG].value ){
      if( yyTokenPos == 0   /* avoid jump to next string *//* add by ASB000117 */
       && yyTokenLine > 1 ) /*ASB000120*/
        MsgDebug( " =Y= ln=%d,end:\t", yyTokenLine-1 );/**/
     /* MsgDebug( "**YACC** ln,pos=%d,end: \t", yyTokenLine-1 );/**/
      else
        MsgDebug( " =Y= ln=%d,%d:\t", yyTokenLine, yyTokenPos );/**/
     /* MsgDebug( "**YACC** ln,pos=%d,%d: \t", yyTokenLine, yyTokenPos );/**/

   /* MsgDebug( msgfmt, arguments ); *//* - not work */
      for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
      vsprintf( linebuffer, msgfmt, arguments ); /*OK*/
      MsgDebug( linebuffer );                    /*OK*/
      MsgDebug( "\n");
    }
    va_end( arguments );
}/*Ydebug()*/
/*=========================================================================*/
