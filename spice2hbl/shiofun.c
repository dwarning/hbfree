
/*
 *  S H i o f u n . C  --  S2H compiler:  Common functions
 *
 *  30.12.1999 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# include   "shcommon.h"
# include   "y.tab.h"
/*-------------------------------------------------------------------------*/

static char linebuffer[maxINPLINELEN]; /* for temporary strings */
static char smallbuffer[20];           /* for temporary strings */
static int  currline = 0; /* in fileInpDbl */
    static void /* Print message with 'errprefix' to fileErr, */
MsgCompileOut(  /* error line and '^' in error position *//* Don't use '\n' in message */
    char    *errprefix, /* error prefix sign type of error (warning/error) */
    char    warnlevel,  /* (only for warning) level after prefix */
    char    *msgno,     /* error code number */
    char    *msgfmt,    /* message */
            ...
);
    static void 
MsgHeader( /* print errortype message prefix without error number to buffer */
    char    *buffer,    /* returned buffer */
    char    *errprefix, /* error prefix sign type of error (warning/error) */
    char    warnlevel   /* (only for warning) level after prefix */
);
char *strupr(char *str);

/*-------------------------------------------------------------------------*/
    char *     /* returned pointer to name */
GetTmpFileName(
    char *name,
    char *mask,
    int  cnt
){
    sprintf(name, mask, cnt );
    return name;
}/*GetTmpFileName()*/
/*-------------------------------------------------------------------------*/
    void
MsgConsole( /* Print message only to console (STDERR) */
            /* Not correct in UNIX if command line like: */
            /*   prog ... 2>file  (turn 'stderr' to 'file')  */
    char    *msgfmt,    /* Need '\n' in message */
            ...
){
    va_list arguments;
    va_start( arguments, msgfmt );
    vfprintf( stderr, msgfmt, arguments );
    fflush( stderr );
    CHECKSTREAM( stderr, "STDERR" ); /**/
    va_end( arguments );
}/*MsgConsole()*/
/*-------------------------------------------------------------------------*/
    void
Msg(    /* Print message to STDOUT */
    char    *msgfmt,    /* Need '\n' in message */
            ...
){
    va_list arguments;
    va_start( arguments, msgfmt );
    vfprintf( stdout, msgfmt, arguments );
    fflush( stdout );
    CHECKSTREAM( stdout, "STDOUT" ); /**/
    va_end( arguments );
}/*Msg()*/
/*-------------------------------------------------------------------------*/
    void
MsgDebug(   /* Print message to fileDebug */
    char    *msgfmt,    /* Need '\n' in message */
            ...
){
    va_list arguments;
    if( Flags[flagD].value )
    {
        va_start( arguments, msgfmt );
        vfprintf( fileDebug, msgfmt, arguments );
        fflush( fileDebug );
        CHECKSTREAM( fileDebug, nameFileDebug );
        va_end( arguments );
    }
}/*MsgDebug()*/
/*-------------------------------------------------------------------------*/
    void
MsgError(   /* Print error message to fileErr, _without_ count errors */
    char    *msgfmt,    /* Need '\n' in message */
            ...
){
    va_list arguments;
    va_start( arguments, msgfmt );
    vfprintf( fileErr, msgfmt, arguments );
    fflush( fileErr );
    CHECKSTREAM( fileErr, nameFileErr );
    va_end( arguments );
}/*MsgError()*/
/*-------------------------------------------------------------------------*/
    void
OutFile(    /* Print message to any file */
    FILE    *ofile,     /* output file */
    char    *msgfmt,    /* Need '\n' in message */
            ...
){
    va_list arguments;
    va_start( arguments, msgfmt );
    vfprintf( ofile, msgfmt, arguments );
    fflush( ofile );
    CHECKSTREAM( ofile, "<out_file>" );
    va_end( arguments );
}
/*-------------------------------------------------------------------------*/
    void
Out(    /* Print message to fileOut */
    char    *msgfmt,    /* Need '\n' in message */
            ...
){
    va_list arguments;
    va_start( arguments, msgfmt );
    vfprintf( fileOut, msgfmt, arguments );
    fflush( fileOut );
    CHECKSTREAM( fileOut, nameFileOut );
    va_end( arguments );
}/*Out()*/
/*-------------------------------------------------------------------------*/
# define  maxlen  72
# define  indent  "\n       "
    void
Out80(  /* Print message to fileOut and divide line to parts < 80 symbols */
    char    *msgfmt,    /* Need '\n' in message */
            ...
){
    char    *begin, *end;
    va_list arguments;

    va_start( arguments, msgfmt );
    vsprintf( linebuffer, msgfmt, arguments ); /* collect parameters */
    begin = linebuffer;

    /* divide line on symbols {' ' | ',' | '='}: */
    while( begin < linebuffer + strlen(linebuffer) ){
        if( strlen(begin) < maxlen ) break; /* break to print rest of line and exit */
        end = begin + maxlen;
        while( begin < end ) /* backward find delimiter */
            if( *end == ' ' || *end == ',' || *end == '=' ) break;
            else end--;
        /* delimiter not found: */
        if( begin == end ) break;       /* break to print rest of line and exit */
        end++;
        /* print from *begin to *end: */
        while( begin < end ) fprintf( fileOut, "%c", *begin++ );
        /* begin = end; */
        fprintf( fileOut, indent ); /* new line and indent 7 blanks */
    }
    fprintf( fileOut, begin );          /* print rest of line and exit */
    fflush( fileOut );
    CHECKSTREAM( fileOut, nameFileOut );
    va_end( arguments );
}/*Out80()*/
# undef  maxlen
# undef  indent
/*-------------------------------------------------------------------------*/
    void
OutNodeTr(  /* Print in nodes translation file */
    char    *msgfmt,    /* Need '\n' in message */
                ...
){
    va_list arguments;
    va_start( arguments, msgfmt );
    vfprintf( fileNodes, msgfmt, arguments );
    fflush( fileNodes );
    CHECKSTREAM( fileNodes, nameFileNodes );
    va_end( arguments );
}/*OutNodeTr()*/
/*-------------------------------------------------------------------------*/
    void
OutInc( /* Print message to temporary circuit file */
    char    *msgfmt,    /* Need '\n' in message */
                ...
){
    va_list arguments;
    va_start( arguments, msgfmt );
    vfprintf( fileInc, msgfmt, arguments );
    fflush( fileInc );
    CHECKSTREAM( fileInc, nameFileInc );
    va_end( arguments );
}/*OutInc()*/
/*-------------------------------------------------------------------------*/
    void
OutElems( /* Print message to fileElems */
    char    *msgfmt,    /* Need '\n' in message */
                ...
){
    va_list arguments;
    va_start( arguments, msgfmt );
    vfprintf( fileElems, msgfmt, arguments );
    fflush( fileElems );
    CHECKSTREAM( fileElems, nameFileElems );
    va_end( arguments );
}/*OutElems()*/
/*-------------------------------------------------------------------------*/
    void
OutTmp( /* Print message to fileTmp */
    char    *msgfmt,    /* Need '\n' in message */
                ...
){
    va_list arguments;
    va_start( arguments, msgfmt );
    vfprintf( fileTmp, msgfmt, arguments );
    fflush( fileTmp );
    CHECKSTREAM( fileTmp, nameFileTmp );
    va_end( arguments );
}/*OutTmp()*/
/*-------------------------------------------------------------------------*/
# define M 0xA5
    static void
Au(void){
    static byte au[] =
    {
'P'^M,'r'^M,'o'^M,'g'^M,'r'^M,'a'^M,'m'^M,'m'^M,'e'^M,'r'^M,':'^M,' '^M,
'A'^M,'l'^M,'e'^M,'x'^M,'a'^M,'n'^M,'d'^M,'r'^M,' '^M,
'B'^M,'e'^M,'l'^M,'o'^M,'g'^M,'o'^M,'l'^M,'o'^M,'v'^M,'s'^M,'k'^M,'y'^M,','^M,
' '^M,'K'^M,'i'^M,'e'^M,'v'^M,'('^M,'U'^M,'A'^M,')'^M,','^M,' '^M,
'2'^M,'0'^M,'0'^M,'0'^M,'\n'^M,
'E'^M,'m'^M,'a'^M,'i'^M,'l'^M,':'^M,' '^M,
's'^M,'p'^M,'i'^M,'c'^M,'e'^M,'3'^M,'m'^M,'a'^M,'n'^M,'@'^M,
'y'^M,'a'^M,'h'^M,'o'^M,'o'^M,'.'^M,'c'^M,'o'^M,'m'^M,','^M,' '^M,
's'^M,'p'^M,'i'^M,'c'^M,'e'^M,'3'^M,'m'^M,'a'^M,'n'^M,'@'^M,
'm'^M,'a'^M,'i'^M,'l'^M,'.'^M,'r'^M,'u'^M,'\0'^M
    };
    byte *str = au;
    while( *str^M ) fprintf( stderr, "%c", *str++^M );
    fprintf( stderr, "\n" );
    fflush( stderr );
    Done();
    exit( exitUSAGE );
}
# undef M
/*-------------------------------------------------------------------------*/
    void
AddInfo(
    char    *parm
){
    if( strEQ( parm, "BOUT" ) ){
        fprintf( stderr, msgABOUT );
        fflush( stderr );
    }
    if( strEQ( parm, "UTHOR" ) ) Au();
    Done();
    exit( exitUSAGE );
}/*AddInfo()*/
/*-------------------------------------------------------------------------*/
    int     /* returned success sign function for */
OpenInput(  /* open fileInp and fileInpDbl, set nameFileInp */
    char *nameFile
){
    int i;
 
    fileInp = fopen( nameFile, "r" );
    if( fileInp == NULL ){
        /* MsgErrorFatal( exitERRINPOPEN, msgERRINPOPEN, nameFile ); */
        return FALSE;                                 /* not exit() ! */
    }
    fileInpDbl = fopen( nameFile, "r" );
    if( fileInpDbl == NULL){
        /* MsgErrorFatal( exitERRINPREOPN, msgERRINPREOPN, nameFile ); */
        fclose( fileInpDbl );
        return FALSE;                                 /* not exit() ! */
    }
    /* if OK: */
    yyin = fileInp; /* Lex input */

    /* assign nameFileInp if OK: */
    for( i = 0; i < maxFNAMELEN; i++ ) nameFileInp[i] = 0; /* clear nameFileInp */
    strcpy( nameFileInp, nameFile );

    return TRUE;

}/*OpenInput()*/
/*-------------------------------------------------------------------------*/
    void
CloseInput( /* close fileInp and fileInpDbl, clean nameFileInp */
    void
){
    int i;

 /* for( i = 0; i < maxFNAMELEN; i++ ) nameFileInp[i] = 0; /* clear nameFileInp */
            /* no clear nameFileInp - use in Lex diagnostic if .include fail */

    if( fileInp != NULL )
        if( fclose(fileInp) == 0 )    fileInp = NULL;

    if( fileInpDbl != NULL ){
        if( fclose(fileInpDbl) == 0 ) fileInpDbl = NULL;
    }

}/*CloseInput()*/
/*-------------------------------------------------------------------------*/
    int     /* returned success sign function for */
FileExist(  /* test if file exist */
    char *nameFile
){
                    /* open + close - not correct !? */
    FILE *ftmp;
    ftmp = fopen(nameFile, "r");
    if( ftmp == NULL ){
 /* MsgDebug( "errno=%d, strerror=%s", errno, strerror(errno) ); */
        return FALSE;
    } else { fclose(fileInp); return TRUE; }

}/*FileExist()*/
/*-------------------------------------------------------------------------*/
    int     /* return TRUE if OK, FALSE if skipping fail */
SkipLines(  /* skip 'cntline' lines in file 'fileX' */
    FILE *inpfile,
    int  cntline    /* line number */
){
    int  i;

    rewind( inpfile );
    while( cntline > 0 ){
        for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
        if( fgets( linebuffer, maxINPLINELEN, inpfile ) == NULL ){
         /* MsgErrorFatal( exitINPFAIL, msgINPFAIL, nameFile??? ); */
            return FALSE;
        }
 /* SHOWint(cntline);MsgDebug("SkipLines(): %s",linebuffer);/*linebuffer have \n*/
        cntline--;
    }
    return TRUE;
}/*SkipLines()*/
/*-------------------------------------------------------------------------*/
    int     /* returned length of line function for */
PrintLine(  /* print input line (with '\n') to fileDiag */
    FILE *fileDiag,
    int  numline    /* line number */
){
    int i;
    int len;

    if( fileDiag == NULL || fileInpDbl == NULL ) return 0;

    /* rewind always: */
 /* if( currline > numline ){ /* test if need rewind */ /* don't use! */
      rewind( fileInpDbl );
      currline = 0;
 /* } /**/
    /* skipping lines: */
    while( currline < numline ){
        if( fgets( linebuffer, maxINPLINELEN, fileInpDbl ) == NULL ){
/*!?        MsgErrorFatal( exitINPFAIL, msgINPFAIL, nameFileInp ); /**/
            fprintf( fileDiag, "\n" );
            fflush( fileDiag );
            CHECKSTREAM( fileDiag, "<diag file>" );
            return 0; /* lead to unending loop */
        }
        ++currline;
    }
    /* after fgets() linebuffer must be with '\n': */
    len = strlen(linebuffer);
    if( linebuffer[len-1] != '\n' )
        strcat( linebuffer, "\n" );

    if( Flags[flagU].value ) strupr( linebuffer );

    fprintf( fileDiag, "%s", linebuffer );
    fflush( fileDiag );
    CHECKSTREAM( fileDiag, "<diag file>" );

    /* recalc length of string considering '\t': */
    len = 0;
    for( i = 0; i < strlen(linebuffer); i++ )
        if( linebuffer[i] == '\t' )
             len += TABSIZE - (len % TABSIZE);
        else len++;
    len--; /*'\n'*/

    /* clean linebuffer: */
    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;

    return len;
}/*PrintLine()*/
/*-------------------------------------------------------------------------*/
    void
MsgErrorCnt(         /* Print error message to fileErr with count errors */
    char    *msgno,  /* error code number */ /* Don't use '\n' in message */
    char    *msgfmt, /* Need '\n' in message */
            ...
){
    va_list arguments;

    va_start( arguments, msgfmt );
    vsprintf( linebuffer, msgfmt, arguments ); /* collect parameters */
    va_end( arguments );

    ++cntErrors; /* count errors */

    /* print errortype prefix: */
    MsgHeader( smallbuffer, msgErrorPrefix, '\0'/*warnlevel*/ );
    MsgError( smallbuffer );
    MsgError( "%s:  ", msgno );  /* error number */
    MsgDebug( "!!!" );
    MsgDebug( smallbuffer );
    MsgDebug( "%s:  ", msgno );  /* error number */

    MsgDebug( linebuffer );
    MsgError( linebuffer );

}/*MsgErrorCnt()*/
/*-------------------------------------------------------------------------*/
    void
MsgWarningCnt(         /* Print warning message to fileErr with count errors */
    char    warnlevel, /* warning level */
    char    *msgno,    /* error code number */ /* Don't use '\n' in message */
    char    *msgfmt,   /* Need '\n' in message */
            ...
){
    va_list arguments;

    va_start( arguments, msgfmt );
    vsprintf( linebuffer, msgfmt, arguments ); /* collect parameters */
    va_end( arguments );

    /* count warnings: */
    switch( PassNo ){
    case 0: /*preparing pass*/
    case 1: cntWarnings1++;
            break;
    case 2: cntWarnings2++;
            break;
    }
    /* count warnings divide by level: */
    switch( warnlevel ){
    case warn1_INFO:     cntWarnL1++; break;
    case warn2_RESERV:   cntWarnL2++; break;
    case warn3_CONFLICT: cntWarnL3++; break;
    case warn4_RESERV:   cntWarnL4++; break;
    case warn5_BREAK:    cntWarnL5++; break;
    case warn6_RESERV:   cntWarnL6++; break;
    case warn7_RESERV:   cntWarnL7++; break;
    }

    /* print errortype prefix: */
    MsgHeader( smallbuffer, msgWarningPrefix, warnlevel );
    MsgError( smallbuffer );
    MsgError( "%s:  ", msgno );  /* error number */
    MsgDebug( "!!!" );
    MsgDebug( smallbuffer );
    MsgDebug( "%s:  ", msgno );  /* error number */

    MsgDebug( linebuffer );
    MsgError( linebuffer );

}/*MsgWarningCnt()*/
/*-------------------------------------------------------------------------*/
    void        /* for compile error message */
MsgCompile( /* Print message to fileErr, error line and '^' in error position */
    char    *msgno,  /* error code number */ /* Don't use '\n' in message */
    char    *msgfmt, /* message */
            ...
){
    static char linebuffer[maxINPLINELEN]; /* for temporary strings */
    va_list arguments;

    ++cntErrors; /* count errors */

    /* make message: */
    va_start( arguments, msgfmt );
    vsprintf( linebuffer, msgfmt, arguments ); /* collect parameters */
    va_end( arguments );

    MsgCompileOut( msgErrorPrefix, '\0'/*no warnlevel*/, msgno/*!*/, linebuffer );

}/*MsgCompile()*/
/*-------------------------------------------------------------------------*/
    void        /* for compiler warning */
MsgCompileWarn( /* Print message to fileErr, error line and '^' in error position */
    char    warnlevel,  /* warning level */
    char    *msgno,     /* error code number */ /* Don't use '\n' in message */
    char    *msgfmt,    /* message */
            ...
){
    static char linebuffer[maxINPLINELEN]; /* for temporary strings */
    va_list arguments;

    /* count warnings divide by pass: */
    switch( PassNo ){
    case 0: /*preparing pass*/
    case 1: cntWarnings1++;
            break;
    case 2: cntWarnings2++;
            break;
    }
 /* SHOWint( cntWarnings1 ); /**/
 /* SHOWint( cntWarnings2 ); /**/

    /* count warnings divide by level: */
    switch( warnlevel ){
    case warn1_INFO:     cntWarnL1++; break;
    case warn2_RESERV:   cntWarnL2++; break;
    case warn3_CONFLICT: cntWarnL3++; break;
    case warn4_RESERV:   cntWarnL4++; break;
    case warn5_BREAK:    cntWarnL5++; break;
    case warn6_RESERV:   cntWarnL6++; break;
    case warn7_RESERV:   cntWarnL7++; break;
    }

    /* make message: */
    va_start( arguments, msgfmt );
 /* SHOWstr(msgfmt);/**/
    vsprintf( linebuffer, msgfmt, arguments ); /* collect parameters */

    va_end( arguments );

    MsgCompileOut( msgWarningPrefix, warnlevel, msgno/*!*/, linebuffer );

}/*MsgCompileWarn()*/
/*-------------------------------------------------------------------------*/
    static void 
MsgHeader( /* print errortype message prefix without error number to buffer */
    char    *buffer,    /* returned buffer */
    char    *errprefix, /* error prefix sign type of error (warning/error) */
    char    warnlevel   /* (only for warning) level after prefix */
){
    /* print errortype prefix: */
    if( warnlevel != '\0' )
      sprintf( buffer, "%s%c%d-", errprefix, warnlevel, PassNo );
    else
      sprintf( buffer, "%s%d-", errprefix, PassNo );

}/*MsgHeader()*/
/*-------------------------------------------------------------------------*/
    static void /* Print message with 'errprefix' to fileErr, */
MsgCompileOut(  /* error line and '^' in error position *//* Don't use '\n' in message */
    char    *errprefix, /* error prefix sign type of error (warning/error) */
    char    warnlevel,  /* (only for warning) level after prefix */
    char    *msgno,     /* error code number */
    char    *msgfmt,    /* message */
            ...
){
    int     i;
    int     pos;
    int     len;

    va_list arguments;

    if( isEOF ){
      MsgErrorFatal( exitERR_ENDNOTFND, msgFERR_ENDNOTFND );
      /*return;/**/
    }

    if( fileInp == NULL || fileInpDbl == NULL || fileErr == NULL ) return;

    /* print errortype prefix: */
    MsgHeader( smallbuffer, errprefix, warnlevel );
    MsgError( smallbuffer );
    MsgError( "%s:  ", msgno );    /* error number */
    MsgDebug( "!!!" );
    MsgDebug( smallbuffer );
    MsgDebug( "%s:  ", msgno );    /* error number */

    /* print file name and line number: */
    if( yyTokenPos == 0 && yyTokenLine > 1 ){ /* avoid jump to next string */
      MsgError( msgFILELINEPOS0, nameFileInp, yyTokenLine-1 );
      MsgDebug( msgFILELINEPOS0, nameFileInp, yyTokenLine-1 );
    } else{
      MsgError( msgFILELINEPOS, nameFileInp, yyTokenLine, yyTokenPos );
      MsgDebug( msgFILELINEPOS, nameFileInp, yyTokenLine, yyTokenPos );
    }
 
    /* separator between header and message: */
    MsgError( "\n\t" ); /* fprintf( fileErr, " " ); */
    MsgDebug( "\n\t" );         /* MsgDebug( " " ); */

    /* print message: */
    va_start( arguments, msgfmt );
    /* for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0; */
    vsprintf( linebuffer, msgfmt, arguments ); /* collect arguments */
    MsgError( linebuffer );
    MsgDebug( linebuffer );
    for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;

    va_end( arguments );

    MsgError( ":\n" );
    MsgDebug( "\n" );

    /* print input line: */
    if( yyTokenPos == 0 && yyTokenLine > 1 ) /* avoid jump to next string */
         len = PrintLine( fileErr, yyTokenLine-1 );
    else len = PrintLine( fileErr, yyTokenLine );

    /* print '^' in yyTokenPos: */
    if( !(yyTokenPos == 0 && yyTokenLine > 1) ) /* avoid jump to next string */
        len = yyTokenPos-1;
    for( pos = 0; pos < len; ++pos ){
        MsgError( " " ); /* putc(' ', fileErr); CHECKSTREAM( fileErr, nameFileErr ); */
    }
    MsgError( "^\n" );

    /* pause after message: */
    if( Flags[flagP].value ) WaitAnyKey( msgPRESSKEY );

}/*MsgCompileOut()*/
/*-------------------------------------------------------------------------*/
    void
MsgErrorFatal(  /* Print message to STDERR and exit to OS with status */
    int     status,  /* exit status */        /* Need '\n' in message */
    char    *msgno,  /* error code number */
    char    *msgfmt, /* message */
                ...
){
    va_list arguments;

    va_start( arguments, msgfmt );
    vsprintf( linebuffer, msgfmt, arguments ); /* collect parameters */
    va_end( arguments );

    if( strlen(msgno) != 0 || strlen(msgfmt) != 0 ){ /* print message */
      MsgHeader( smallbuffer, msgFatalPrefix, '\0'/*warnlevel*/ );

      /* output to debug file, if exist: */
      if( fileDebug != stderr ){
        MsgDebug( "!!!!!" );
        MsgDebug( smallbuffer );
        MsgDebug( "%s: ", msgno ); /* error number */
        MsgDebug( linebuffer );
      }
      /* output to errorlogfile, if exist: */
      if( fileErr != stderr ){
        MsgError( "\n" );
        MsgError( smallbuffer );
        MsgError( "%s: ", msgno ); /* - error number */
        MsgError( linebuffer );
      }
      /* always output to 'stderr': */
      fprintf( stderr, "\n" );
      fprintf( stderr, smallbuffer );
      fprintf( stderr, "%s: ", msgno ); /* - error number */
      fprintf( stderr, linebuffer );
      fflush( stderr );
      CHECKSTREAM( stderr, "STDERR" ); /**/
    }

    MsgDebug( "MsgErrorFatal(): returned exit-code = %d.(%02XH)\n",
                                                 status, status );/**/
    Done();
    exit( status );

}/*MsgErrorFatal()*/
/*-------------------------------------------------------------------------*/
    void
ShutDown(   /* close all files, not exit */
    void
){
    int cnt, i;

    CloseInput(); /* close fileInp, fileInpDbl */

    if( fileCnf != NULL )    if( fclose(fileCnf) == 0)   fileCnf   = NULL;
    if( fileOut != NULL )    if( fclose(fileOut) == 0)   fileOut   = NULL;
    if( fileTmp != NULL )    if( fclose(fileTmp) == 0)   fileTmp   = NULL;
    if( fileNodes != NULL )  if( fclose(fileNodes) == 0) fileNodes = NULL;
    if( fileErr != NULL )    if( fclose(fileErr) == 0)   fileErr   = stderr;
    if( fileDebug != NULL )  if( fclose(fileDebug) == 0) fileDebug = stderr;

    /* remove all temporary file(s): */
    if( !Flags[flagK].value ) /* if not keep tmp-files: */
        for( cnt = 0; cnt < cntTmpFiles; cnt++ ){
            GetTmpFileName( nameFileTmp, nameTMPMASK, cnt );
            for( i = 0; i < maxINPLINELEN; i++ ) linebuffer[i] = 0;
            strcpy( linebuffer, nameDirTmp );
            strcat( linebuffer, sPATHSEPARATOR );
            strcat( linebuffer, nameFileTmp );
            remove( linebuffer/*nameFileTmp*/ ); /*unlink();*/
        }
}/*ShutDown()*/
/*-------------------------------------------------------------------------*/
    byte
GetExitCode(   /* get exit code from info about warnings and errors */
    void
){
    byte  retcode = 0, tmp = 0;

    if( Flags[flag1].value || Flags[flag2].value ){
      MsgDebug("GetExitCode():\n");
      SHOWint( PassNo );
      SHOWint( cntErrors );
      SHOWint( cntWarnings1 );
      SHOWint( cntWarnings2 );
      SHOWint( cntWarnL1 );
      SHOWint( cntWarnL2 );
      SHOWint( cntWarnL3 );
      SHOWint( cntWarnL4 );
      SHOWint( cntWarnL5 );
      SHOWint( cntWarnL6 );
      SHOWint( cntWarnL7 );
    }
    /* if errors counter != 0 return _fatal_ error: */
    if( cntErrors )
      switch( PassNo ){
      case 1: /* errors found on pass 1 */
              MsgErrorFatal( exitERRORSONPASS1, msgERRORSONPASS1 );
              break;
      case 2: case 3: /* errors found on pass 2 and in final actions */
              MsgErrorFatal( exitERRORSONPASS2, msgERRORSONPASS2 );
              break;
      }
    if( cntWarnL1 ) retcode |= maskEXWARNL1;
    if( cntWarnL2 ) retcode |= maskEXWARNL2;
    if( cntWarnL3 ) retcode |= maskEXWARNL3;
    if( cntWarnL4 ) retcode |= maskEXWARNL4;
    if( cntWarnL5 ) retcode |= maskEXWARNL5;
    if( cntWarnL6 ) retcode |= maskEXWARNL6;
    if( cntWarnL7 ) retcode |= maskEXWARNL7;


  #ifdef OLDVEREXITCODE
    /* fill messages fields: */
    if( cntWarnings1 > 0 ) retcode |= maskEXWARNPASS1;
    if( cntWarnings2 > 0 ) retcode |= maskEXWARNPASS2;

    /* truncate error count: */
    if( cntErrors > maxERRCOUNT ) cntErrors = maxERRCOUNT;

    /* fill error count field: */
    if( cntErrors > 0 ) tmp = ((byte)cntErrors)<<2;
    tmp &= maskEXERRCOUNT;
    retcode |= tmp;

    /* fill error pass field: */
    if( cntErrors > 0 && PassNo == 2 )
        retcode |= maskEXERRPASS2;
  #endif /*OLDVEREXITCODE*/

    MsgDebug( "GetExitCode(): returned exit-code = %02XH\n", retcode );/**/

    return retcode;
}/*GetExitCode()*/
/*-------------------------------------------------------------------------*/
#ifdef WIN32
# include   <conio.h>
#else
#  define getch() getchar()
#endif
    void
WaitAnyKey( /* print message to STDERR (to console) and wait key */
    char    *message
){
    int i;

    fprintf( stderr, "\r%s", message ); fflush( stderr );
    if( getch() == ESC ) MsgErrorFatal( exitABORTBYUSER, msgABORTBYUSER );

/* clean message: */
    for( i = 0; i < strlen(message); ++i ) fprintf( stderr, "\b" );
    for( i = 0; i < strlen(message); ++i ) fprintf( stderr, " " );
    for( i = 0; i < strlen(message); ++i ) fprintf( stderr, "\b" );

}/*WaitAnyKey()*/
/*-------------------------------------------------------------------------*/
    int
strEQ(   /* return TRUE if "str1"=="str2", ignore case */
    char *str1, /* NB! 'str1' and 'str2' will be convert to upper case ! */
    char *str2
){
    if( strlen(str1) != strlen(str2) ) return FALSE;
    while( *str1 && *str1 ){
	if( toupper(*str1) != toupper(*str2) ) return FALSE;
	str1++;
	str2++;
    }
    return TRUE;
}/*strEQ()*/
/*-------------------------------------------------------------------------*/
    char *
strupr(
    char *str
){
    char *ptr = str;
    while( *ptr ){
        *ptr = toupper(*ptr);
        ptr++;
    }
    return str;
}/*strupr()*/
/*-------------------------------------------------------------------------*/
