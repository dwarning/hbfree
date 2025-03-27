
/*
 *  S H m a i n . C  --  S2H-compiler:  Main Procedure, I/O functions
 *
 *  29.12.1999 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# include   "shcommon.h"
# include   "y.tab.h"
/*-------------------------------------------------------------------------*/

int     PassNo = 0;  /* pass number */

                               /* files: */
FILE    *fileInp, *fileInpDbl, /* current input file and it copy */
        *fileCnf,              /* configuration file */
        *fileOut,              /* output (object) file */
        *fileErr,              /* file for save error diagnostic */
        *fileDebug,            /* file for save debug diagnostic */
        *fileTmp,              /* temporary file */
        *fileInc,              /* temporary file for make include subcircuit */
        *fileNodes,            /* nodes translations file */
        *fileElems;            /* elements names translations file */

                                    /* names for: */
char    nameFileStart[maxFNAMELEN], /* start input file (for restart pass2) */
        nameFileCnf[maxFNAMELEN],   /* configuration file */
        nameFileInp[maxFNAMELEN],   /* current input file */
        nameFileNew[maxFNAMELEN],   /* new .INCLUDE-file */
        nameFileOut[maxFNAMELEN],   /* output (object) file */
        nameFileErr[maxFNAMELEN],   /* file for save error diagnostic */
        nameFileDebug[maxFNAMELEN], /* file for save debug diagnostic */
        nameFileTmp[maxFNAMELEN],   /* temporary file */
        nameFileInc[maxFNAMELEN],   /* temporary file for make include subcircuit */
        nameFileNodes[maxFNAMELEN], /* nodes translations file */
        nameFileElems[maxFNAMELEN], /* elements names translations file */
        nameDirTmp[maxFNAMELEN];    /* temporary directory */

/* flags table: */
struct FlagsTable Flags[maxFLAG];

int cntErrors    = 0;  /* errors count */
int cntWarnings1 = 0;  /* count warnings for pass 1 */
int cntWarnings2 = 0;  /* count warnings for pass 2 */
int cntWarnL1 = 0, cntWarnL2 = 0, cntWarnL3 = 0, cntWarnL4 = 0, cntWarnL5 = 0,
    cntWarnL6 = 0, cntWarnL7 = 0; /* counters for warnings level 1..7 */

int cntTmpFiles  = 0;  /* tmp-files count */

static char linebuffer[maxINPLINELEN]; /* for temporary strings */

/*=========================================================================*/
#ifdef GNU_C
    int
#else
    void
#endif
main(
    int     argc,
    char    *argv[]
)
{
    int     i;
    int     cntParm;
    int     cntFlag;
    int     FlagNotFound;
    byte    retcode;

    PassNo = 0;  /* pre-pass: preparing actions */

    InitFlags(); /* must be before Usage() ! */
    MsgConsole( sFULLNAME, sVERSION, sVERSDATE ); /* print header */
    if( argc == 1 ) Usage( argv[0] );   /* quick help and exit */

    /* init files and files names: */
    fileCnf = fileInp = fileInpDbl = fileTmp =
              fileOut = fileNodes = fileElems = NULL;
    fileErr = fileDebug = stderr;

    for( i = 0; i < maxFNAMELEN; i++ )
        nameFileCnf[i]   =
        nameFileTmp[i]   =
        nameFileNew[i]   = /*Input*/
        nameFileOut[i]   =
        nameFileInc[i]   =
        nameFileNodes[i] =
        nameFileElems[i] =
        nameFileErr[i]   =
        nameFileDebug[i] = 0;
    strcpy( nameFileErr, nameSTDERR );

    cntTmpFiles  = 0;  /* tmp-files count */

    cntErrors    = 0;  /* errors count */
    cntWarnings1 = 0;  /* count warnings for pass 1 */
    cntWarnings2 = 0;  /* count warnings for pass 2 */
    cntWarnL1 = cntWarnL2 = cntWarnL3 = cntWarnL4 = cntWarnL5 = cntWarnL6
              = cntWarnL7 = 0; /* counters for warnings level 1..7 */

    /* command line processing: */
    for( cntParm = 1; cntParm < argc; cntParm++ )
    { /* next command line parameter: */
      if( argv[cntParm][0] == '-' || argv[cntParm][0] == '+' ||
                                     argv[cntParm][0] == '/' ) /* flag found */
      {
        FlagNotFound = TRUE;
        for( cntFlag = 0; cntFlag < maxFLAG; cntFlag++ )
          if( toupper(Flags[cntFlag].flagletter) == toupper(argv[cntParm][1]) )
          {
            FlagNotFound = FALSE;
            switch(argv[cntParm][0])
            {
              case '+': Flags[cntFlag].value = TRUE;
                        break;
              case '-':
              case '/': Flags[cntFlag].value = !Flags[cntFlag].value;
                        break;
            }
            if( Flags[cntFlag].havestrparm ){ /* flag with string parameter */
              if( strlen( argv[cntParm] ) <= 2 )
                  MsgErrorFatal( exitNOFLAGPARM, msgNOFLAGPARM, argv[cntParm][1] );
              /* read and save parameter: */
              sscanf( &argv[cntParm][2], "%s", Flags[cntFlag].strparm );
           /* strupr( Flags[cntFlag].strparm ); /* convert parameter to upper case */
              Flags[cntFlag].value = TRUE;
            }
            break;/*for*/
          }
        if( FlagNotFound ) MsgConsole( msgUNKNOWNFLAG, argv[cntParm][0], argv[cntParm][1] );
      }
      else
      { /* file name found */
        if( streq( nameFileNew, "" ) ) /*Input*/
        {
            strcpy( nameFileNew, argv[cntParm] );
# ifdef UPFNAME
            strupr( nameFileNew );
# endif
        }
        else if( streq( nameFileOut, "" ) )
        {
            strcpy( nameFileOut, argv[cntParm] );
# ifdef UPFNAME
            strupr( nameFileOut );
# endif
        }
      }
    }/*for*/

    /* avoid flags conflicts: */
    if( Flags[flagE].value )   Flags[flagP].value = FALSE ;
    if( !Flags[flagD].value )  Flags[flagL].value = FALSE ;
    if( !Flags[flagD].value )  Flags[flagG].value = FALSE ;
    if( !Flags[flagD].value )  Flags[flag1].value = FALSE ;
    if( !Flags[flagD].value )  Flags[flag2].value = FALSE ;

    /* prepare files names: */
    /* save start input file for restart pass 2: */
    strcpy( nameFileStart, nameFileNew );
    /* config file: */
    if( Flags[flagC].value )
         strcpy( nameFileCnf, Flags[flagC].strparm );
    else strncpy( nameFileCnf, nameCONFILE, maxFNAMELEN ); /*default*/
    /* nodes translation file: */
    if( Flags[flagN].value )
         strcpy( nameFileNodes, Flags[flagN].strparm );
    else strncpy( nameFileNodes, nameNODESFILE, maxFNAMELEN ); /*default*/
    /* elements names translation file: */
    if( Flags[flagM].value )
         strcpy( nameFileElems, Flags[flagM].strparm );
    else strncpy( nameFileElems, nameELEMSFILE, maxFNAMELEN ); /*default*/
    /* error file: */
    if( Flags[flagE].value )
         strcpy( nameFileErr, Flags[flagE].strparm );
    /* debug file: */
    if( Flags[flagD].value )
         strcpy( nameFileDebug, Flags[flagD].strparm );
    /* temporary file: */
    cntTmpFiles = 0;
    GetTmpFileName( nameFileTmp, nameTMPMASK, cntTmpFiles++ );
    /* temporary directory: */
    if( Flags[flagT].value )
         strcpy( nameDirTmp, Flags[flagT].strparm );
    else strncpy( nameDirTmp, nameTMPDIRDEF, maxFNAMELEN );
# ifdef UPFNAME
    /* convert files names to upper case: */
    strupr( nameFileCnf );
    strupr( nameFileNodes );
    strupr( nameFileElems );
    strupr( nameFileErr );
    strupr( nameFileDebug );
    strupr( nameFileTmp );
    strupr( nameDirTmp );
# endif

    /* debug files names: */
/* fileError not open yet - this output to STDERR: */
/*
    if( Flags[flagD].value ){
        SHOWstr( nameFileCnf );
        SHOWstr( nameFileNew );
        SHOWstr( nameFileStart );
        SHOWstr( nameFileOut );
        SHOWstr( nameFileNodes );
        SHOWstr( nameFileElems );
        SHOWstr( nameFileErr );
        SHOWstr( nameFileDebug );
        SHOWstr( nameFileTmp );
        SHOWstr( nameDirTmp );
    }
*/
    /* process help flags: */
    if( Flags[flagH].value )
        Usage( argv[0] );       /* help information and exit */
    if( Flags[flagA].value )
        AddInfo( Flags[cntFlag].strparm ); /* print additional information and exit */

    /* check for exist input/output (after help processing): */
    if( streq( nameFileNew, "" ) ) /* no input file - nothing to do */
        MsgErrorFatal( exitNOINPFNAME, msgNOINPFNAME );
    if( streq( nameFileOut, "" ) ) /* no output file - nothing to do */
        MsgErrorFatal( exitNOOUTFNAME, msgNOOUTFNAME );

    /* prepare files files: */
# ifdef GNU_C /* not use */
    /* for lexyy.c for GNU C */
 /* yyin  = stdin;  /* reassign in OpenInput() */
 /* yyout = stdout; /* reassign after open fileErr */
        /* instead of standard: FILE *yyin ={stdin}, *yyout ={stdout}; */
# endif

    /* open debug file: */
    if( !Flags[flagD].value )
        strcpy( nameFileDebug, sNULDEV ); /* default output to NUL */
    fileDebug = fopen( nameFileDebug, "w" );
    if( fileDebug == NULL )
        MsgErrorFatal( exitERRDEBCREAT, msgERRDEBCREAT, nameFileDebug );
/**/MsgDebug( "\n" ); /* test out */

    /* open error messages file: */
    fileErr = stderr;
    if( Flags[flagE].value ){
        fileErr = fopen( nameFileErr, "w" );
        if( fileDebug == NULL )
            MsgErrorFatal( exitERRERRCREAT, msgERRERRCREAT, nameFileErr );
    }
    yyout = fileErr;
/**/MsgError( "\n" ); /* test out */

    /* open config file: */
 /* fileCnf = fopen( nameFileCnf, "r" ); /* search config file as is */
    fileCnf = fopen( searchpath(nameFileCnf), "r" ); /* search in %PATH% */
    if( fileCnf == NULL ){
        MsgWarningCnt( warn1_INFO, msgWARNCNFOPEN, nameFileCnf );
 /* if config file not found - use pre-install CFG-parameters:
        MsgErrorFatal( exitERRCNFOPEN, msgERRCNFOPEN, nameFileCnf );
 */ }
    /* open input file: */
    if( !OpenInput( nameFileNew ) )
        MsgErrorFatal( exitERRINPOPEN, msgERRINPOPEN, nameFileNew );

    /* open temporary file: */
    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
    strcpy( linebuffer, nameDirTmp );
    strcat( linebuffer, sPATHSEPARATOR );
    strcat( linebuffer, nameFileTmp );
    fileTmp = fopen( linebuffer/*nameFileTmp*/, "w" );
    if( fileTmp == NULL )
        MsgErrorFatal( exitERRTMPCREAT, msgERRTMPCREAT, linebuffer/*nameFileTmp*/ );
/**/OutTmp( "\n" ); /* test out */

    /* open output file: */
    fileOut = fopen( nameFileOut, "w" );
    if( fileOut == NULL )
        MsgErrorFatal( exitERROUTCREAT, msgERROUTCREAT, nameFileOut );
/**/Out( "\n" ); /* test out */

    /* open nodes translation file: */
    fileNodes = fopen( nameFileNodes, "w" );
    if( fileNodes == NULL )
        MsgErrorFatal( exitERRFNODCREAT, msgERRFNODCREAT, nameFileNodes );
/**/OutNodeTr( "\n" ); /* test out */

    /* open elements names translation file: */
    fileElems = fopen( nameFileElems, "w" );
    if( fileElems == NULL )
        MsgErrorFatal( exitERRFELECREAT, msgERRFELECREAT, nameFileElems );
/**/OutElems( "\n" ); /* test out */

    /* report results if all opening is OK: */
    MsgDebug( msgDEBUGINF );
    MsgDebug( msgCNFFNAME,  nameFileCnf );
    MsgDebug( msgINPFNAME,  nameFileInp );
    MsgDebug( msgOUTFNAME,  nameFileOut );
    MsgDebug( msgNODFNAME,  nameFileNodes );
    MsgDebug( msgELEMFNAME, nameFileElems );
    MsgDebug( msgERRFNAME,  nameFileErr );
    MsgDebug( msgDEBFNAME,  nameFileDebug );
    MsgDebug( msgTMPDIRNAME,nameDirTmp );
    MsgDebug( msgTMPFNAME,  nameFileTmp );

    /* analyse configuration file, fill Habala-options table: */
    /* ASB000420: if config file not found - use pre-install CFG-parameters: */
    if( fileCnf != NULL )
        AnalyseConfFile(); /* analyse configuration file, fill Habala-options table */

    /* save titul line of start file or file name: */
    SaveTitul();

    /* pass 1: */
    cntInclNest = 0;  /* counter of include files nesting */
    isEOF = FALSE;    /* end of input file indicator */
    is1stline = TRUE; /* say to Lex about 1st line of file is title */
    /* cntWarnings1 = 0; /* - no! may contain warnings before pass 1 */
    Pass1();

    /* reopen start input file: */
    CloseInput();
    if( !OpenInput( nameFileStart ) )
        MsgErrorFatal( exitERRINPREOPN, msgERRINPREOPN, nameFileNew );
    ptrStack = NULL;  /* free stack of files */
    cntInclNest = 0;  /* counter of include files nesting */

    /* initial settings: */
    isEOF = FALSE;    /* end of input file indicator */
    is1stline = TRUE; /* say to Lex about 1st line of file is title */
    yyTokenPos = 0;
    yyTokenLine/*yylineno*/ = 1;
    //YY_FLUSH_BUFFER;// yySkipUnput; /* skip Lex unput()-buffer */
 /* SkipLines( fileInp, yyTokenLine/*yylineno** - 1 ); /* old: for skip Lex buffer */

    /* pass 2: */
    cntWarnings2 = 0; /* count warnings for pass 2 */
    if( cntErrors == 0 ) Pass2();

    /* calculate positive exit-code: */
    retcode = GetExitCode();
    /* close all, etc: */
    Done();
    /* finish: */
    exit( retcode );

 /* old exit version:
    if( cntErrors > maxERRCOUNT ) cntErrors = maxERRCOUNT;
        /* in MS DOS/WIN positive exit code is in range 1..127, >128 - negative */
 /* exit( cntErrors );   /* cntErrors = 0 if OK */

}/*main()*/
/*=========================================================================*/
    void
Usage(      /* print usage information */
    char    *prgname
){
    int cnt;
    static char defstr[80];

    MsgConsole( usgUSAGE, prgname );
    /* print main flags: */
    for( cnt=0; cnt<maxFLAG; cnt++ ){
        if( Flags[cnt].isdebug ) continue;
        if( strlen(Flags[cnt].defaultmesage) > 0 )
              sprintf( defstr, "(%s: %s)",
                                 usgDEFAULT, Flags[cnt].defaultmesage );
        else  defstr[0] = '\0';
        MsgConsole( Flags[cnt].message, Flags[cnt].flagletter, defstr );
    }
    if( Flags[flagH].value ){ /* extended information */
        /* print addition flags: */
        MsgConsole( usgFLAG_ADD );
        for( cnt=0; cnt<maxFLAG; cnt++ ){
            if( !Flags[cnt].isdebug ) continue;
            if( strlen(Flags[cnt].defaultmesage) > 0 )
                  sprintf( defstr, "(%s: %s)",
                                     usgDEFAULT, Flags[cnt].defaultmesage );
            else  defstr[0] = '\0';
            MsgConsole( Flags[cnt].message, Flags[cnt].flagletter, defstr );
        }
        /* print here more help information: */
        MsgConsole( usgMOREHELP );
    }
    Done();
    exit( exitUSAGE );    /* MsgErrorFatal( exitUSAGE, msgFATALHIDDEN ); */

}/*Usage()*/
/*-------------------------------------------------------------------------*/
    void
InitFlags(
    void
){

/* help ----------------------------------------*/
  # define numFLAG  flagH
    Flags[numFLAG].flagletter  = '?'; /* 'h' */
    strcpy( Flags[numFLAG].message, usgFLAG_H );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = FALSE;
    Flags[numFLAG].isdebug     = FALSE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
# undef numFLAG

/* additional info -----------------------------*/
  # define numFLAG  flagA
    Flags[numFLAG].flagletter  = 'a';
    strcpy( Flags[numFLAG].message, usgFLAG_A );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = TRUE;
    Flags[numFLAG].isdebug     = TRUE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
  # undef numFLAG

/* error output to file ... --------------------*/
  # define numFLAG  flagE
    Flags[numFLAG].flagletter  = 'e';
    strcpy( Flags[numFLAG].message, usgFLAG_E );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = TRUE;
    Flags[numFLAG].isdebug     = FALSE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
# undef numFLAG

/* pause after error ---------------------------*/
  # define numFLAG  flagP
    Flags[numFLAG].flagletter  = 'p';
    strcpy( Flags[numFLAG].message, usgFLAG_P );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = FALSE;
    Flags[numFLAG].isdebug     = FALSE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
# undef numFLAG

/* put statistic to compiler messages ----------*/
  # define numFLAG  flagS
    Flags[numFLAG].flagletter  = 's';
    strcpy( Flags[numFLAG].message, usgFLAG_S );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = FALSE;
    Flags[numFLAG].isdebug     = FALSE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
  # undef numFLAG

/* debug output to file ... --------------------*/
  # define numFLAG  flagD
    Flags[numFLAG].flagletter  = 'd';
    strcpy( Flags[numFLAG].message, usgFLAG_D );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = TRUE;
    Flags[numFLAG].isdebug     = TRUE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
# undef numFLAG

/* debug print token ---------------------------*/
  # define numFLAG  flagL
    Flags[numFLAG].flagletter  = 'l';
    strcpy( Flags[numFLAG].message, usgFLAG_L );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = FALSE;
    Flags[numFLAG].isdebug     = TRUE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
# undef numFLAG

/* debug grammar -------------------------------*/
  # define numFLAG  flagG
    Flags[numFLAG].flagletter  = 'g';
    strcpy( Flags[numFLAG].message, usgFLAG_G );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = FALSE;
    Flags[numFLAG].isdebug     = TRUE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
  # undef numFLAG

/* debug pass1 ---------------------------------*/
  # define numFLAG  flag1
    Flags[numFLAG].flagletter  = '1';
    strcpy( Flags[numFLAG].message, usgFLAG_1 );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = FALSE;
    Flags[numFLAG].isdebug     = TRUE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
  # undef numFLAG

/* debug pass2 ---------------------------------*/
  # define numFLAG  flag2
    Flags[numFLAG].flagletter  = '2';
    strcpy( Flags[numFLAG].message, usgFLAG_2 );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = FALSE;
    Flags[numFLAG].isdebug     = TRUE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
  # undef numFLAG

/* temporary directory name --------------------*/
  # define numFLAG  flagT
    Flags[numFLAG].flagletter  = 't';
    strcpy( Flags[numFLAG].message, usgFLAG_T );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = TRUE;
    Flags[numFLAG].isdebug     = FALSE;
    strcpy( Flags[numFLAG].defaultmesage, nameTMPDIRDEF );
  # undef numFLAG

/* keep temporary files ------------------------*/
  # define numFLAG  flagK
    Flags[numFLAG].flagletter  = 'k';
    strcpy( Flags[numFLAG].message, usgFLAG_K );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = FALSE;
    Flags[numFLAG].isdebug     = TRUE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
  # undef numFLAG

/* configuration file name ---------------------*/
  # define numFLAG  flagC
    Flags[numFLAG].flagletter  = 'c';
    strcpy( Flags[numFLAG].message, usgFLAG_C );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = TRUE;
    Flags[numFLAG].isdebug     = FALSE;
    strcpy( Flags[numFLAG].defaultmesage, nameCONFILE );
  # undef numFLAG

/* nodes translation file name -----------------*/
  # define numFLAG  flagN
    Flags[numFLAG].flagletter  = 'n';
    strcpy( Flags[numFLAG].message, usgFLAG_N );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = TRUE;
    Flags[numFLAG].isdebug     = FALSE;
    strcpy( Flags[numFLAG].defaultmesage, nameNODESFILE );
  # undef numFLAG

/* elements names translation file name --------*/
  # define numFLAG  flagM
    Flags[numFLAG].flagletter  = 'm';
    strcpy( Flags[numFLAG].message, usgFLAG_M );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = TRUE;
    Flags[numFLAG].isdebug     = FALSE;
    strcpy( Flags[numFLAG].defaultmesage, nameELEMSFILE );
  # undef numFLAG

/* convert input to upper case -----------------*/
  # define numFLAG  flagU
    Flags[numFLAG].flagletter  = 'u';
    strcpy( Flags[numFLAG].message, usgFLAG_U );
    Flags[numFLAG].value       = FALSE;
    Flags[numFLAG].havestrparm = FALSE;
    Flags[numFLAG].isdebug     = FALSE;
    strcpy( Flags[numFLAG].defaultmesage, Flags[numFLAG].value?msgYES:msgNO );
  # undef numFLAG

}/*InitFlags()*/
/*-------------------------------------------------------------------------*/
    void
Done(       /* final actions */
    void
){
/*
    . . .
*/
    ShutDown();
}/*Done()*/
/*-------------------------------------------------------------------------*/
