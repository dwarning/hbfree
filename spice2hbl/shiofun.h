
/*
 *  S H i o f u n . H
 *
 *  29.12.1999 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# ifndef SHiofun_H
# define SHiofun_H
/*-------------------------------------------------------------------------*/
/* warnings level: */
# define    warn1_INFO     'I'  /* information */
# define    warn2_RESERV   '2'
# define    warn3_CONFLICT 'C'	/* confict in parameters, etc. */
# define    warn4_RESERV   '4'
# define    warn5_BREAK    'B'  /* break element generation */
# define    warn6_RESERV   '6'
# define    warn7_RESERV   '7'

/* constants for making positive exit-codes returned by GetExitCode(): */
# define    maskEXWARNL1   (byte)(0x01<<0)  /* 0 bit */
# define    maskEXWARNL2   (byte)(0x01<<1)  /* 1 bit */
# define    maskEXWARNL3   (byte)(0x01<<2)  /* 2 bit */
# define    maskEXWARNL4   (byte)(0x01<<3)  /* 3 bit */
# define    maskEXWARNL5   (byte)(0x01<<4)  /* 4 bit */
# define    maskEXWARNL6   (byte)(0x01<<5)  /* 5 bit */
# define    maskEXWARNL7   (byte)(0x01<<6)  /* 6 bit */
/*old:*/
/* # define    maskEXWARNPASS1   (byte)0x01  /* 0 bit */
/* # define    maskEXWARNPASS2   (byte)0x02  /* 1 bit */
/* # define    maskEXERRCOUNT    (byte)0x3C  /* 2-5 bits */
/* # define    maskEXERRPASS2    (byte)0x40  /* 6 bit */

/*-------------------------------------------------------------------------*/
    void
MsgConsole( /* Print message only to console (STDERR) */
            /* Not correct in UNIX if command line like: */
            /*   prog ... 2>file  (turn 'stderr' to 'file')  */
    char    *msgfmt,    /* Need '\n' in message */
            ...
);
/*-------------------------------------------------------------------------*/
    void
MsgDebug(   /* Print message to fileDebug */
    char    *msgfmt,    /* Need '\n' in message */
            ...
);
/*-------------------------------------------------------------------------*/
    void
Msg(    /* Print message to STDOUT */
    char    *msgfmt,    /* Need '\n' in message */
            ...
);
/*-------------------------------------------------------------------------*/
    void
MsgError(   /* Print error message to fileErr, _without_ count errors */
    char    *msgfmt,    /* Need '\n' in message */
            ...
);
/*-------------------------------------------------------------------------*/
    void
OutFile(    /* Print message to any file */
    FILE    *ofile,     /* output file */
    char    *msgfmt,    /* Need '\n' in message */
            ...
);
/*-------------------------------------------------------------------------*/
    void
Out(    /* Print message to fileOut */
    char    *msgfmt,    /* Need '\n' in message */
            ...
);
/*-------------------------------------------------------------------------*/
    void
Out80(  /* Print message to fileOut and divide line to parts < 80 symbols */
    char    *msgfmt,    /* Need '\n' in message */
            ...
);
/*-------------------------------------------------------------------------*/
    void
OutNodeTr(  /* Print in nodes translation file */
    char    *msgfmt,    /* Need '\n' in message */
                ...
);
/*-------------------------------------------------------------------------*/
    void
OutElems( /* Print message to fileElems */
    char    *msgfmt,    /* Need '\n' in message */
                ...
);
/*-------------------------------------------------------------------------*/
    void
OutInc( /* Print message to temporary circuit file */
    char    *msgfmt,    /* Need '\n' in message */
                ...
);
/*-------------------------------------------------------------------------*/
    void
OutTmp( /* Print message to fileTmp */
    char    *msgfmt,    /* Need '\n' in message */
                ...
);
/*-------------------------------------------------------------------------*/
    void
ShutDown(   /* close all files, not exit */
    void
);
/*-------------------------------------------------------------------------*/
    int     /* returned success sign function for */
OpenInput(  /* open fileInp and fileInpDbl, set nameFileInp */
    char *nameFile
);
/*-------------------------------------------------------------------------*/
    void
CloseInput( /* close fileInp and fileInpDbl, clean nameFileInp */
    void
);
/*-------------------------------------------------------------------------*/
    int     /* returned success sign function for */
FileExist(  /* test if file exist */
    char *nameFile
);
/*-------------------------------------------------------------------------*/
    int     /* return TRUE if OK, FALSE if skipping fail */
SkipLines(  /* skip 'cntline' lines in file 'fileX' */
    FILE *fileIn,
    int  cntline    /* line number */
);
/*-------------------------------------------------------------------------*/
    int     /* returned length of line function for */
PrintLine(  /* print input line (with '\n') to fileDiag */
    FILE *fileDiag,
    int  numline    /* line number */
);
/*-------------------------------------------------------------------------*/
    void
MsgErrorFatal(  /* Print message to STDERR and exit to OS with status */
    int     status,  /* exit status */        /* Need '\n' in message */
    char    *msgno,  /* error code number */
    char    *msgfmt, /* message */
                ...
);
/*-------------------------------------------------------------------------*/
    void
MsgErrorCnt(         /* Print error message to fileErr with count errors */
    char    *msgno,  /* error code number */ /* Don't use '\n' in message */
    char    *msgfmt, /* Need '\n' in message */
            ...
);
/*-------------------------------------------------------------------------*/
    void
MsgWarningCnt(         /* Print warning message to fileErr with count errors */
    char    warnlevel, /* warning level */
    char    *msgno,    /* error code number */ /* Don't use '\n' in message */
    char    *msgfmt,   /* Need '\n' in message */
            ...
);
/*-------------------------------------------------------------------------*/
    void        /* for compile error message */
MsgCompile( /* Print message to fileErr, error line and '^' in error position */
    char    *msgno,  /* error code number */ /* Don't use '\n' in message */
    char    *msgfmt, /* message */
            ...
);
/*-------------------------------------------------------------------------*/
    void        /* for compiler warning */
MsgCompileWarn( /* Print message to fileErr, error line and '^' in error position */
    char    warnlevel,  /* warning level */
    char    *msgno,     /* error code number */ /* Don't use '\n' in message */
    char    *msgfmt,    /* message */
            ...
);
/*-------------------------------------------------------------------------*/
    void
AddInfo(
    char    *parm
);
/*-------------------------------------------------------------------------*/
    char *     /* returned pointer to name */
GetTmpFileName(
    char *name,
    char *mask,
    int  cnt
);
/*-------------------------------------------------------------------------*/
    void
WaitAnyKey( /* print message to STDERR (to console) and wait key */
    char    *message
);
/*-------------------------------------------------------------------------*/
int strEQ( char *str1, char *str2 );/* return TRUE if "str1"=="str2", ignore case */
/*-------------------------------------------------------------------------*/
    byte
GetExitCode(   /* get positive exit code from info about warnings and errors */
    void
);
/*-------------------------------------------------------------------------*/
#if defined(BSD)
    char *
strupr(
    char *str
);
#endif
/*-------------------------------------------------------------------------*/
# endif /* define SHiofun_H */
