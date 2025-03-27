
/*
 *  S H l p a r s . H
 *
 *  29.12.1999 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# ifndef SHlpars_H
# define SHlpars_H
/*-------------------------------------------------------------------------*/
/* lexical analizer variables: */

extern int isEOF;       /* end of input file indicator */
extern int is1stline;   /* say to Lex about 1st line of new file */
extern int cntInclNest; /* counter of include files nesting */

typedef struct  { /* for tokens with mixed value */
    char   *text;
    long   longval;
    double floatval;
} value;

typedef struct  { /* Spice3 suffixes */
    char    *string;
    double  mult;
} MultTab;
extern  MultTab ScaleSufxTab[];

typedef struct  { /* keywords */
    char    *string;
    int     value;
} KeysTab;
extern  KeysTab NetElemPrefTab[];
extern  KeysTab OperKeysTab[];

typedef struct stackfiles { /* stack of files for include files processing */
    char           *fname;  /* file name */
    int            linenum; /* current line number */
struct stackfiles  *prev;
} StackFiles;

extern StackFiles  *ptrStack;

/*-------------------------------------------------------------------------*/

extern char yytext[];
# define    yyTokenText     yytext  /* text of current token */

/*extern    YYSTYPE yylval; /* defined in YTAB.H */
# define    yyTokenValue    yylval  /* value of current token */

extern  int yychar;
# define    yyTokenType     yychar  /* type of current token */

extern  int yylineno;
# define    yyTokenLine     yylineno /* line number where current token found */

extern  int yyTokenPos;

# define    yyErrCount      yynerrs /* number of errors */

//extern char *yysptr, yysbuf[];
//# define    yySkipUnput     yysptr = yysbuf /* skip Lex unput buffer */
# define    yySkipUnput    YY_FLUSH_BUFFER


/*-------------------------------------------------------------------------*/
/* common functions: */

int yylex( void );
int yylook( void );
int yyback( int *, int );
int yyinput( void );
int yyoutput( int c );
// int yyunput( int c );

    int     /* return TRUE to exit, FALSE - to continue processing */
yywrap(     /* instead of yywrap() from LEXLIB.LIB */
    void    /* if files stack not empty - return to previous file */
);

void    SkipRestLine( void ); /* skip rest of input line */

    void
IncludeFile(            /* process .include-file */
    char *nameFileIncl, /* .include-file name */
    int  startline      /* start line in include-file */
);
    int         /* return TRUE if OK, FALSE - if stack empty or fail */
ReturnPrevFile( /* restore input file from stack after .include or circ subcall */
    int isFromInclude /* = TRUE if return after .include, not subcall */
);

/*-------------------------------------------------------------------------*/
# endif /* define SHlpars_H */
