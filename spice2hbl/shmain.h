
/*
 *  S H m a i n . H
 *
 *  29.12.1999 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# ifndef SHmain_H
# define SHmain_H
/*-------------------------------------------------------------------------*/
/* vars: */
extern  int     PassNo; /* pass number */

                                       /* files: */
extern  FILE    *yyin, *yyout,         /* YACC input, output */
                *fileInp, *fileInpDbl, /* current input file and it copy */
                *fileCnf,              /* configuration file */
                *fileOut,              /* output (object) file */
                *fileErr,              /* file for save error diagnostic */
                *fileDebug,            /* file for save debug diagnostic */
                *fileTmp,              /* temporary file */
                *fileInc,              /* temporary file for make include subcircuit */
                *fileNodes,            /* nodes translations file */
                *fileElems;            /* elements names translations file */

                                            /* names for: */
extern  char    nameFileStart[maxFNAMELEN], /* start input file (for restart pass2) */
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

extern  int     cntErrors;    /* errors count */
extern  int     cntWarnings1; /* count warnings for pass 1 */
extern  int     cntWarnings2; /* count warnings for pass 2 */
extern  int     cntWarnL1, cntWarnL2, cntWarnL3, cntWarnL4, cntWarnL5,
                cntWarnL6, cntWarnL7; /* counters for warnings level 1..7 */

extern  int     cntTmpFiles;  /* tmp-files count */

/* flags: */
# define    flagH    0  /* help */
# define    flagC    1  /* configuration file name */
# define    flagE    2  /* error output to file ... */
# define    flagP    3  /* pause after error */
# define    flagS    4  /* put statistic to compiler messages */
# define    flagN    5  /* rename nodes translation file */
# define    flagM    6  /* rename elements names translation file */
# define    flagT    7  /* temporary directory name */
# define    flagU    8  /* convert input to upper case */
# define    flagK    9  /* keep temporary files */
# define    flagD   10  /* debug output to file ... */
# define    flagL   11  /* debug print token */
# define    flagG   12  /* debug grammar */
# define    flag1   13  /* debug pass1 */
# define    flag2   14  /* debug pass2 */
# define    flagA   15  /* additional info */
# define    maxFLAG 16

/* flags table: */
struct FlagsTable {
    int     value;
    char    flagletter;
    char    message[maxMSGLEN];
    char    defaultmesage[maxMSGLEN];
    int     havestrparm;
    char    strparm[maxMSGLEN];
    int     isdebug;            /*ASB000121*/
};
extern struct FlagsTable Flags[maxFLAG];

/*-------------------------------------------------------------------------*/
/* functions: */
    void
InitFlags(
    void
);
    void
Done(
    void
);
    void
Usage(      /* print usage information */
    char    *prgname
);
/*-------------------------------------------------------------------------*/
# endif /* define SHmsgEN_H */
