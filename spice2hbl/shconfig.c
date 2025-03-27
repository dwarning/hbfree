
/*
 *  S H c o n f i g . C  --  S2H compiler:  Configuration file analyse functions
 *
 *  05.01.2000 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# include   "shcommon.h"
# include   "y.tab.h"
/*-------------------------------------------------------------------------*/

char   strTitul[maxTITULSTRLEN];

OptTab OptionsTab[] = {
 /* name         isint?  dblval   intval    isset?  isserv? */

 /* ASB000420: new, pre-init version: */
    "Rl",        FALSE,  0.001,    0,       TRUE,   FALSE,
    "Gc",        FALSE,  0.000001, 0,       TRUE,   FALSE,
    "Ri",        FALSE,  0.001,    0,       TRUE,   FALSE,
    "Gi",        FALSE,  0.000001, 0,       TRUE,   FALSE,
    "U0",        FALSE,  0.5,      0,       TRUE,   FALSE,
    "DU",        FALSE,  0.1,      0,       TRUE,   FALSE,
    "Rline",     FALSE,  0.05,     0,       TRUE,   FALSE,
    "Diode_f0",  FALSE,  0.845,    0,       TRUE,   FALSE,
    "k_Diode_f0",FALSE,  0.8,      0,       TRUE,   FALSE,
    "Rsmin",     FALSE,  1.0,      0,       TRUE,   FALSE,
    "R_emf_i",   FALSE,  0.1,      0,       TRUE,   FALSE,
    "MESFET_RDMIN", FALSE, 0.1,    0,       TRUE,   FALSE,
    "MESFET_RG",    FALSE, 0.1,    0,       TRUE,   FALSE,
    "MESFET_RSMIN", FALSE, 0.1,    0,       TRUE,   FALSE,
    "MESFET_CDS",   FALSE, 0.0,    0,       TRUE,   FALSE,
    "MESFET_IS0",   FALSE, 0.0,    0,       TRUE,   FALSE,
    "MESFET_N",     FALSE, def_MESFET_N,  0,TRUE,   FALSE,
    "MESFET_RDS",   FALSE, def_MESFET_RDS,0,TRUE,   FALSE,
    "MESFET_RGS",   FALSE, def_MESFET_RGS,0,TRUE,   FALSE,
    "MESFET_UBOUND",FALSE, def_MESFET_UBOUND,0,TRUE,FALSE,
    "MESFET_STERM",FALSE,  def_MESFET_STERM, 0,TRUE,FALSE,

    "EPSIW",     FALSE,  0.0001,   0,     TRUE,   TRUE,
    "KITU",      TRUE,   0.0,      0,     TRUE,   TRUE,
    "EPSSOL",    FALSE,  1.0e-8,   0,     TRUE,   TRUE,
    "EPSDU",     FALSE,  1.0e-5,   0,     TRUE,   TRUE,
    "EPSMIN",    FALSE,  1.0e-6,   0,     TRUE,   TRUE,
    "MAXDU",     FALSE,  100.0,    0,     TRUE,   TRUE,
    "LIMIT",     TRUE,   0.0,     50,     TRUE,   TRUE,
    "KPRLEN",    TRUE,   0.0,      0,     TRUE,   TRUE,
    "KPRSRT",    TRUE,   0.0,      0,     TRUE,   TRUE,
    "KPRNKR",    TRUE,   0.0,      0,     TRUE,   TRUE,
    "KPRLIN",    TRUE,   0.0,      0,     TRUE,   TRUE,
    "KPRSOL",    TRUE,   0.0,      0,     TRUE,   TRUE,
    "MGLOB",     TRUE,   0.0,      1,     TRUE,   TRUE,
    "IAPR",      TRUE,   0.0,      0,     TRUE,   TRUE,
    "KNC",       TRUE,   0.0,     32,     TRUE,   TRUE,

 /* old, no pre-init version:
    "Rl",        FALSE,  0.0,      0,     FALSE,  FALSE,
    "Gc",        FALSE,  0.0,      0,     FALSE,  FALSE,
    "Ri",        FALSE,  0.0,      0,     FALSE,  FALSE,
    "Gi",        FALSE,  0.0,      0,     FALSE,  FALSE,
    "U0",        FALSE,  0.0,      0,     FALSE,  FALSE,
    "DU",        FALSE,  0.0,      0,     FALSE,  FALSE,
    "Rline",     FALSE,  0.0,      0,     FALSE,  FALSE,
    "Diode_f0",  FALSE,  0.0,      0,     FALSE,  FALSE,
    "k_Diode_f0",FALSE,  0.0,      0,     FALSE,  FALSE,

    "EPSIW",     FALSE,  0.0,      0,     FALSE,  TRUE,
    "KITU",      TRUE,   0.0,      0,     FALSE,  TRUE,
    "EPSSOL",    FALSE,  0.0,      0,     FALSE,  TRUE,
    "EPSDU",     FALSE,  0.0,      0,     FALSE,  TRUE,
    "EPSMIN",    FALSE,  0.0,      0,     FALSE,  TRUE,
    "MAXDU",     FALSE,  0.0,      0,     FALSE,  TRUE,
    "LIMIT",     TRUE,   0.0,      0,     FALSE,  TRUE,
    "KPRLEN",    TRUE,   0.0,      0,     FALSE,  TRUE,
    "KPRSRT",    TRUE,   0.0,      0,     FALSE,  TRUE,
    "KPRNKR",    TRUE,   0.0,      0,     FALSE,  TRUE,
    "KPRLIN",    TRUE,   0.0,      0,     FALSE,  TRUE,
    "KPRSOL",    TRUE,   0.0,      0,     FALSE,  TRUE,
    "MGLOB",     TRUE,   0.0,      0,     FALSE,  TRUE,
    "IAPR",      TRUE,   0.0,      0,     FALSE,  TRUE,
    "KNC",       TRUE,   0.0,      0,     FALSE,  TRUE,
 */
    NULL,        FALSE,  0.0,      0,     FALSE,  FALSE
};

static char   linebuffer[maxINPLINELEN]; /* for temporary strings */
static char   optname[maxINPLINELEN];

static int    OptIsInt( char *optname );

/*-------------------------------------------------------------------------*/
    void
AnalyseConfFile( /* analyse configuration file, fill/update Habala-options table */
    void
){
    int    i;
    char   *str;
    int    intval;
    double dblval;

 /* MsgDebug("AnalyseConfFile() ...\n"); /**/
    if( fileCnf == NULL ) return; /* if config file not exist */

    for(;;){
     /* MsgDebug("--- next string ---\n"); /**/
        /* get next string of config file: */
        for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
        if( fgets( linebuffer, maxINPLINELEN, fileCnf ) == NULL ) break; /*EOF*/
        if( linebuffer[strlen(linebuffer)-1] != '\n' )
            strcat( linebuffer, "\n" );
        str = linebuffer;
        /* skip blanks: */
        while( *str == ' ' || *str == '\t' ) str++;
     /* SHOWstr(str); /**/
        if( *str == '\n' ) continue;  /* end of line */
        if( *str == '*' )  continue;  /* skip comment */
        sscanf( str, "%s", optname );
        /* truncate "  =..." in 'optname': */
        for( i = 0; i < strlen( optname ); i++ )
            if( optname[i] == '=' || optname[i] == ' ' || optname[i] == '\t' )
                optname[i] = '\0';
     /* SHOWstr(optname); /**/
     /**MsgConsole( "optname='%s'\n",optname );/**/
        if( GetOptNo( optname ) == -1 ){ /* illegal option */
            MsgError( msgUNKNWNOPTION, optname ); /**/
            continue;
        }
        /* skip up to '=': */
        while( *str != '=' ){
     /* SHOWchar(*str);
            if( *str == '\n' ) continue; /* end of line */
            str++;
        }
        str++;
     /* SHOWstr(str); /**/
        /* skip blanks in 'str': */
        while( *str == ' ' || *str == '\t' ) str++;
        /* truncate illegal tail in value-string: */
        for( i = 0; i < strlen( str ); i++ )
            if( str[i] != '.' && str[i] != '+' && str[i] != '-'
             && str[i] != 'e' && str[i] != 'E' && !isdigit(str[i])
              ) str[i] = '\0';
     /* SHOWstr(str); /**/
        intval = 0;
        dblval = 0.0;
     /* SHOWint(OptIsInt(optname)); /**/
        if( OptIsInt( optname ) )
             sscanf( str, "%d", &intval );
	else dblval = atof(str); /* sscanf( str, "%e", &dblval ); /* not work */

     /* SHOWfloat(dblval); /**/
     /* SHOWint(intval); /**/
     /**MsgConsole( "optname='%s'\n",optname );/**/
        ChangeConfOption( optname, dblval, intval );
    }

    DebugPrintOptions(); /* Debugging print Habala-options */

}/*AnalyseConfFile()*/
/*-------------------------------------------------------------------------*/
    int              /* returned FALSE if option not exist, TRUE - if OK */
GetConfOption(       /* function for get Habala-option */
    char   *optname, /* option name */
    int    *isint,   /* TRUE if option - integer */
    double *dblval,  /* pointer to float value */
    long   *intval   /* pointer to integer value */
){
    int idx;
    idx = GetOptNo(optname);
    if( idx == -1 ) return FALSE;
    *isint  = OptionsTab[idx].isint;
    *dblval = OptionsTab[idx].dblval;
    *intval = OptionsTab[idx].intval;
    return TRUE;
}/*GetOptValue()*/
/*-------------------------------------------------------------------------*/
    int              /* returned TRUE if option already set, else - FALSE */
ChangeConfOption(    /* function for set Habala-option */
    char   *optname, /* option name */
    double dblval,   /* float value, if exist */
    long   intval    /* integer value, if exist */
){
    int ret;
    int idx;
    idx = GetOptNo(optname);
    if( idx != -1 ){
        ret = OptionsTab[idx].isset;
        if( OptionsTab[idx].isint )
             OptionsTab[idx].intval = intval;
        else OptionsTab[idx].dblval = dblval;
        OptionsTab[idx].isset = TRUE;
    }
    return ret;
}/*ChangeConfOption()*/
/*-------------------------------------------------------------------------*/
    int              /* returned -1 if fail */
GetOptNo(            /* function for return option number in table by it name */
    char   *optname  /* option name */
){
    OptTab *ot;
    int    idx;

    for( idx = 0, ot = OptionsTab; ot->optname != NULL; idx++, ot++ )
	if( strEQ( ot->optname, optname ) ) return idx;

    return -1;
}/*GetParmNo()*/
/*-------------------------------------------------------------------------*/
    static int
OptIsInt(            /* return TRUE if option is integer, else - FALSE */
    char   *optname  /* option name */
){
    int idx;
    idx = GetOptNo(optname);
    if( idx != -1 )  return OptionsTab[idx].isint;
    return FALSE; /* if option not found */
}/*GetParmNo()*/
/*-------------------------------------------------------------------------*/
    void
DebugPrintOptions(   /* debugging print options */
    void
){
    OptTab *ot;

    if( Flags[flagG].value  || Flags[flag1].value  || Flags[flag2].value ){
      MsgDebug( msgHABOPTTAB );
      if( Flags[flagD].value )
        for( ot = OptionsTab; ot->optname != NULL; ot++ ){
            MsgDebug( "\t%s \t=  ", ot->optname );
            if( !ot->isset ) MsgDebug( "<not set>" );
            else if( ot->isint )
                      MsgDebug( "%d", ot->intval );
                 else MsgDebug( "%g", ot->dblval );
            MsgDebug( "\n" );
        }
    }
}/*DebugPrintOptions()*/
/*-------------------------------------------------------------------------*/
    void
SaveTitul(           /* save titul line of start file or file name */
    void
){
    int  i;
    char *str;

    /* clear strings buffers: */
    for( i = 0; i < maxTITULSTRLEN; i++ ) strTitul[i] = 0;
    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;

    if( fileInpDbl != NULL ){ /* try to get strTitul from titul line */
        rewind( fileInpDbl );
        if( fgets( linebuffer, maxINPLINELEN, fileInpDbl ) == NULL )
	    linebuffer[0] = 0;  /* fail read */
        rewind( fileInpDbl );
    }
    /* remove last '\n': */
    for( i = 0; i < strlen( linebuffer ); i++ )
        if( linebuffer[i] == '\n' ) linebuffer[i] = '\0';
    /* skip blanks at begin: */
    str = linebuffer;
    /* skip blanks: */
    while( *str == ' ' || *str == '\t' ) str++;

    strncpy( strTitul, str, maxTITULSTRLEN-1 ); /* copy with truncate >60 len */

    if( strlen(strTitul) == 0 ){ /* get strTitul from filename */
        strcpy( strTitul, nameFileInp );
        /* truncate file extansion ".EXT": */
        for( i = 0; i < strlen( strTitul ); i++ )
            if( strTitul[i] == '.' ) strTitul[i] = '\0';
    }

    MsgDebug( "Titul string: \"%s\"\n", strTitul );
}/*SaveTitul()*/
/*-------------------------------------------------------------------------*/
# define SERVHD " &SERV " /* "  &SERVIS " */
# define INDENT "       " /* "          " */
    void
OutHabSERVIS(        /* generate &SERVIS-statement */
    void
){
    int    cnt, existopt;
    OptTab *ot;

    Out( SERVHD );

    existopt = FALSE;
    cnt = 0;
 /* if( Flags[flagD].value ) */
    for( ot = OptionsTab; ot->optname != NULL; ot++ )
        if( ot->isserv && ot->isset ){
          if( existopt && cnt == 0 ) Out( INDENT );
          existopt = TRUE;
          cnt++;
          Out( "%s=", ot->optname );
          if( ot->isint )
               Out( "%d, ", ot->intval );
          else Out( "%g, ", ot->dblval );
          if( cnt >= maxOPTINLINE ){
            cnt = 0;
            Out( "\n" );
          }
        }
    if( existopt && cnt != 0 ) Out( "\n" );
    Out( " NAME='%s' /\n", strTitul );
    Out( "\n" );

}/*OutHabSERVIS()*/
# undef SERVHD
# undef INDENT
/*-------------------------------------------------------------------------*/
