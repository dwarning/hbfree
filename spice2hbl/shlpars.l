

%{

/*
%C
%A2500

 *  S H l p a r s . L  --  S2H-compiler:  Lexical analyzer
 *
 *
 *  - Lexical analyser for Spice3 grammar
 *  - Correct processing line and position number for compile and debug messages
 *  - Process first line of input as comment (title line)
 *  - '+' at the begin of line mark previous line continue
 *  - Report for searched lexem in debug mode
 *  - Multi-level include files processing
 *  - Switch: first line of include file may be title or not
 *
 *
 *  History:
 *    31.12.1999 (v.0.01) - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 *    08.01.2000 (v.0.15) - first version complete (ASB)
 *    19.01.2000 (v.0.31) - multi-level include files processing (ASB)
 *    06.02.2000 (v.0.49) - recognize C-style comment (ASB)
 *    04.03.2000 (v.0.66) - returned values without sign (ASB)
 *    09.04.2000 (v.0.96) - convert input to uppercase if process flag '-u' (ASB)
 */

/* # define DEBUG_WRAP      /* enable dubugging yywrap() and files stack */
# define DEBUG_STACK     /* enable dubugging include-files stack */
/* # define DEBUG_SKIPEOLN  /* enable dubugging skip rest of line */

#  include   "shcommon.h"
#  include   "y.tab.h"

/* extern YYSTYPE yylval; /*in YTAB.H*/
         
# define    begin   BEGIN   /* for 2nd version of LEX */
# define    IncPos  yyTokenPos += yyleng

int  isEOF;       /* end of input file indicator */
int  is1stline;   /* TRUE say to Lex about 1st line of new file is title */
int  cntInclNest; /* counter of include files nesting */

int  yyTokenPos  = 0; /* token position */

static char linebuffer[maxINPLINELEN]; /* for temporary strings */

# define    LMSG( fmt, parm )                                   \
    sprintf( linebuffer, fmt, parm );                           \
    if( Flags[flagL].value ) MsgDebug( "%s", linebuffer );

# define    LFINDTOKEN( whatfound, text )                       \
    if( strlen(text) )                                          \
      Ldebug( "%s %s = '%s'", msgFINDTOKEN, whatfound, text );  \
    else                                                        \
      Ldebug( whatfound );

StackFiles  *ptrStack = NULL;

static void   Ldebug( char *msgfmt, ... );/* print debug message */
static void   ShowLine( char *etc );/* print current line number to console, to debug */
static double LookupScale( char *str );   /* scale suffix look up function */
static int    LookupOperator( char *str );/* operator keywords look up function */
static int    LookupElem( char *str );/* network elements prefix look up function */
static int    PushFileStack( char *name, int lno );
static int    PopFileStack( char *name );

/* scale suffixes table: */
MultTab ScaleSufxTab[] = {
    "T",    1e12,
    "G",    1e9,
    "MEG",  1e6,    /* must be before suffix "M" */
    "K",    1e3,
    "M",    1e-3,
    "U",    1e-6,
    "N",    1e-9,
    "P",    1e-12,
    "F",    1e-15,
    NULL,   0.0
};

/* All keywords divide to operator keywords and simple keywords: */
/* operator keywords table: */
KeysTab OperKeysTab[] = {
    ".AC",      _AC,
    ".ALTER",   _ALTER,
    ".DC",      _DC,
    ".CONTROL", _CONTROLS,/* not '_CONTROL' - in use by VisualC++ */
    ".DISTO",   _DISTO,
    ".END",     _END,
    ".ENDC",    _ENDC,
    ".ENDS",    _ENDS,
    ".FOUR",    _FOUR,
    ".HB",         _HB,          /* for Habala only */
    ".HB_OPTIONS", _HB_OPTIONS,  /* for Habala only */
    ".IC",      _IC,
    ".INCLUDE", _INCLUDE, /* only for lexical analyzer: process include file */
    ".LET",     _LET,
    ".MODEL",   _MODEL,
    ".NODESET", _NODESET,
    ".NOISE",   _NOISE,
    ".OP",      _OP,
    ".OPTIONS", _OPTIONS,
    ".OPTION",  _OPTIONS,
    ".OPT",     _OPTIONS,
    ".PLOT",    _PLOT,
    ".PRINT",   _PRINT,
    ".PZ",      _PZ,
    ".SAVE",    _SAVE,
    ".SENS",    _SENS,
    ".SUBCKT",  _SUBCKT,
    ".TF",      _TF,
    ".TLINE",   _TLINE,
    ".TRAN",    _TRAN,
    ".WIDTH",   _WIDTH,
    ".WRITE",   _WRITE,

    NULL,       0
};

/* network element prefixes table: */
KeysTab NetElemPrefTab[] = {    /* in this version prefix have only one letter */
 /* "A",        free, */
    "B",        NLDS,        /* Non-linear Dependent Sources *//*add ASB000117*/
    "C",        CAPACITOR,
    "D",        DIODE,
    "E",        LVCVS,       /* Linear Voltage-Controlled Voltage Sources */
    "F",        LCCCS,       /* Linear Current-Controlled Current Sources */
    "G",        LVCCS,       /* Linear Voltage-Controlled Current Sources */
    "H",        LCCVS,       /* Linear Current-Controlled Voltage Sources */
    "I",        CURRENT,
    "J",        JFET,        /* Junction Field-Effect Transistors */
    "K",        INDUCTCOUPL,
    "L",        INDUCTOR,
    "M",        MOSFET,
 /* "N",        free, */
    "O",        LTLINE,      /* Lossy Transmission Lines *//*add ASB000117*/
 /* "P",        free, */
    "Q",        TRANSISTOR,  /* Bipolar Junction Transistors */
    "R",        RESISTOR,
    "S",        VSWITCH,
    "T",        LLTLINE,     /* Lossless Transmission Lines */
    "U",        UDRCLINE,    /* Uniform Distributed RC Lines (Lossy) *//*ASB000117*/
    "V",        VOLTAGE,
    "W",        ISWITCH,
    "X",        SUBCALL,
 /* "Y",        free, */
    "Z",        MESFET,
    NULL,       0
};

%}

%start FIRSTLINE CODE LNCOMMENT COMMENT

DECDIGIT    [0-9]
LETTER      [A-Za-z]
IDLETTER    [A-Za-z_]
EXTSYM      [\128-\241] 

NETELSUFX   [0-9a-zA-Z_]*

SCALESUFX   {LETTER}[0-9a-zA-Z]*
 /* old:  SCALESUFX  [FfPpNnUuMmKkGgTt][0-9a-zA-Z]* */

 /* old:  clear number (include "+-.e"): */
 /* DECIMAL_NUM [-+][0-9]+ */
 /* FLOAT1S     [-+][0-9]+"."[0-9]*([eE][-+]?[0-9]+)? */
 /* FLOAT2S     [-+]?[0-9]+"."[0-9]*[eE][-+][0-9]+    */
 /* FLOAT3S     [-+][0-9]+[eE][-+]?[0-9]+             */
 /* FLOAT4S     [-+]?[0-9]+[eE][-+][0-9]+             */
 /* FLOAT1E     [-+]?[0-9]+"."[0-9]*[eE][-+]?[0-9]+   */
 /* FLOAT2E     [-+]?[0-9]+[eE][-+]?[0-9]+            */
 /* FLOAT1P     [-+]?"."[0-9]+([eE][-+]?[0-9]+)?      */
 /* ASB000304:  clear number (include "+-.e", not leading"+-"): */
FLOAT1S     [0-9]+"."[0-9]*([eE][-+]?[0-9]+)?
FLOAT2S     [0-9]+"."[0-9]*[eE][-+][0-9]+
FLOAT3S     [0-9]+[eE][-+]?[0-9]+
FLOAT4S     [0-9]+[eE][-+][0-9]+
FLOAT1E     [0-9]+"."[0-9]*[eE][-+]?[0-9]+
FLOAT2E     [0-9]+[eE][-+]?[0-9]+
FLOAT1P     "."[0-9]+([eE][-+]?[0-9]+)?

FLOAT_NUM   ({FLOAT1S}|{FLOAT2S}|{FLOAT3S}|{FLOAT4S}|{FLOAT1E}|{FLOAT2E}|{FLOAT1P})

     /* number-identifier: */
DECIMAL_ID  [0-9]+

FLOAT1ID    [0-9]+"."[0-9]*
FLOAT2ID    [0-9]+"."[0-9]*[eE][0-9]+
FLOAT3ID    [0-9]+[eE][0-9]+
FLOAT_ID    ({FLOAT1ID}|{FLOAT2ID}|{FLOAT3ID})

%%

%{

  /* static int is1stline = TRUE; /* is1stline make global for pass 2 processing */

  if (is1stline == TRUE){
        YY_FLUSH_BUFFER;
        begin FIRSTLINE; 
        }
  else {
        begin CODE;
        }
    
 /* MsgDebug( "yylex()..." );/**/

  # ifdef DEBUG_WRAP
      MsgDebug( msgYYLEXSTART );SHOWptr(fileInp);
  # endif

    /* return EOF if input closed: */
    if( fileInp == NULL || fileInpDbl == NULL ){
  # ifdef DEBUG_WRAP
      MsgDebug( msgYYLEXEOF );
  # endif
        return 0;/*EOF*/
    }

    /* process first/TITLE line: */

/* GS:        is1stline = FALSE; 
/*
/*    if( is1stline ){
/*      is1stline = FALSE;
/*      if( cntInclNest == 0 ){ /* main file */
/*          if(Flags[flagL].value) ShowLine( msgLINETITLE ); else ShowLine( "" );
/*          LFINDTOKEN( "TITLE_LINE", "" );
/*          SkipRestLine();
/*          LFINDTOKEN( msgFNDENDOFSTMT, "" );
/*          return _ENDOFSTMT_; /* pseudo-token */
/*      }
/*  # ifdef INCL_FILE_HAVE_TITLE /*** title have both main amd include files ***/
/*      else {                  /* include file */
/*          if(Flags[flagL].value) ShowLine( msgLINETITLE ); else ShowLine( "" );
/*          LFINDTOKEN( "TITLE_LINE", "" );
/*          SkipRestLine();
/*          LFINDTOKEN( msgFNDENDOFSTMT, "" );
/*          return _ENDOFSTMT_; /* pseudo-token */
/*      }
/*  # endif
/*    }  
*/

%}

<FIRSTLINE>[^\n]*"\n" {
    is1stline=FALSE;
    begin CODE;
    return _ENDOFSTMT_;
    }
    
<CODE>^[ \t]*"*"                        {       /* <begin of line> *...comment */
    int ip;
 /* IncPos; (yyTokenPos += yyleng;) - not correct on '\t' ! */
    for( ip = 0; ip < yyleng; ip++ )
        if( yytext[ip] == '\t' )
             yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
        else yyTokenPos++;
    LFINDTOKEN( msgCOMMENTbegin, yytext );
    begin LNCOMMENT;
 }
<CODE>";"                               {       /* [statement...] ;...comment */
    IncPos;
    LFINDTOKEN( msgCOMMENTbegin, yytext );
    begin LNCOMMENT;
 }
<LNCOMMENT>"\n"                         {       /* end of comment, end of stmt */
    /*yylineno*/yyTokenLine--; /* for correct line number diagnostic */
    yyTokenPos++;              /* for correct position of '\n' diagnostic */
    LFINDTOKEN( msgCOMMENTend, "" );

    LFINDTOKEN( msgFNDENDOFSTMT, "" ); /* also end of statement before ;comment */
    /*yylineno*/yyTokenLine++;

    yyTokenPos = 0;
    if(Flags[flagL].value) ShowLine( msgLINEAFTERCOMNT ); else ShowLine( "" );
    begin CODE;
    /* return <end of statement> for case "...statement ;comment" */
    return _ENDOFSTMT_; /* pseudo-token */
 }
<LNCOMMENT>.*                           {       /* comment body */
    /* ".*" correct recognize, but not echo extended symbols [\128-\255] */
    /* <LNCOMMENT>(.|{EXTSYM})* - not work correct, not portable */
    int ip;

    if( Flags[flagU].value ) strupr( yytext ); /* if '-u' convert to uppercase */

 /* IncPos; (yyTokenPos += yyleng;) - not correct on '\t' ! */
    for( ip = 0; ip < yyleng; ip++ )
        if( yytext[ip] == '\t' )
             yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
        else yyTokenPos++;
    LFINDTOKEN( msgCOMMENTbody, yytext );
 }

<CODE>"/*"                              {       /* C-style comment */
    IncPos;
    begin COMMENT;
    LFINDTOKEN( msgCOMMENTbegin, yytext );
 }
<COMMENT>"*/"                           {       /* end of C-style comment */
    IncPos;
    begin CODE;
    LFINDTOKEN( msgCOMMENTend, yytext );
 }
<COMMENT>.                              {       /* C-style comment body */
    int  ip;

    if( Flags[flagU].value ) strupr( yytext ); /* if '-u' convert to uppercase */

 /* IncPos; (yyTokenPos += yyleng;) - not correct on '\t' ! */
    /* correct token position: */
    for( ip = 0; ip < yyleng; ip++ )    /* yyleng==1 */
        if( yytext[ip] == '\t' )
             yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
        else yyTokenPos++;
 }
<COMMENT>"\n"                           {       /* continue C-comment on next line */
    yyTokenPos = 0;
    if(Flags[flagL].value) ShowLine( msgLINECOMMENT ); else ShowLine( "" );
 /* return _ENDOFSTMT_; /* pseudo-token */
 }

<CODE>"\n"[ \t]*[+]                     {       /* continue code line */
    int ip;
    if(Flags[flagL].value) ShowLine( msgLINECONTSTMT ); else ShowLine( "" );
    yyTokenPos = 0;
 /* IncPos; (yyTokenPos += yyleng;) - not correct on '\t' ! */
    for( ip = 1; ip < yyleng; ip++ ) /* 1st sym = '\n' */
        if( yytext[ip] == '\t' )
             yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
        else yyTokenPos++;
    LFINDTOKEN( msgCODECONTINUE, "" );
 }
<CODE>"\n"/[ \t]*[^+]                   {       /* end of statement, not stmt continue */
    /*yylineno*/yyTokenLine--; /* for correct line number diagnostic */
    yyTokenPos++;              /* for correct position of '\n' diagnostic */
    LFINDTOKEN( msgFNDENDOFSTMT, "" );  /* not correct show line,pos# before */
    /*yylineno*/yyTokenLine++;          /* include file if 1st line not title */

    yyTokenPos = 0;
    if(Flags[flagL].value) ShowLine( msgLINENEXTSTMT ); else ShowLine( "" );
    return _ENDOFSTMT_; /* pseudo-token */
 }

<CODE>["][^"]+["]                       {       /* string */
    /* [^"]  - not recognize extended symbols [\128-\255] */
    /* <CODE>["]([^"]|{EXTSYM})+["] - not work correct, not portable */
    /* if EXTSYM == [\128-\255] - crashed */
    char *ptr;
    int  ip;

    if( Flags[flagU].value ) strupr( yytext ); /* if '-u' convert to uppercase */

 /* IncPos; (yyTokenPos += yyleng;) - not correct on '\t' ! */
    for( ip = 0; ip < yyleng; ip++ )
        if( yytext[ip] == '\t' )
             yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
        else yyTokenPos++;

    if( (ptr=malloc(yyleng)) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
    else {
        strcpy(ptr, yytext+1);  /* +1 - del begin <"> */
        *(ptr+yyleng-2) = '\0'; /* -2 - del end <"> */
        yylval.strptr = ptr;
        LFINDTOKEN( "STRING", yytext );
        return STRING;
    }
 }

<CODE>^[ \t]*{LETTER}{NETELSUFX}        {
            /* keyword, network element or identifier at the begin of line */
    int     kw;
    char    *ptr;
    char    first[2];
    int     beginpos;
    int     ip;

    if( Flags[flagU].value ) strupr( yytext ); /* if '-u' convert to uppercase */

    /* correct token position: */
    for( ip = 0; ip < yyleng; ip++ )
        if( yytext[ip] == '\t' )
             yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
        else yyTokenPos++;

 /* LFINDTOKEN( msgFULLSTRatBOL, yytext ); */
    /* find first != ' ' char: */
    for( beginpos = 0; beginpos < yyleng; ++beginpos )
        if( yytext[beginpos] != ' ' && yytext[beginpos] != '\t' ) break;
 /* LFINDTOKEN( msgTRANSTRatBOL, yytext+beginpos ); */

    first[0] = yytext[beginpos]; /* get only first letter of identifier */
    first[1] = '\0';             /* for find prefix of network element */

    /* old1: 1: find keyword, 2: find netelement, 3: identifier */
    /* old2: 1: find netelement, 2: find keyword, 3: identifier *//*ASB000122*/
    /* now:  1: find netelement, 2: identifier *//*ASB000124*/

    if( ( kw = LookupElem( first ) ) != FALSE ){/* at 1st: find netelement prefix */
        if( ( ptr = malloc(yyleng-beginpos+1) ) == NULL )
            MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
        else {
            strcpy( ptr, yytext+beginpos );
            *(ptr+yyleng) = '\0';
            yylval.strptr = ptr;    /* return element name */
            LFINDTOKEN( "<BOL>NETELEMENT", yytext+beginpos );
            return kw; /* return type of network element */
        }
 /* removed by ASB000124:
    } else if( ( kw = LookupKeyword( yytext+beginpos ) ) != FALSE ){** find keyword **
        LFINDTOKEN( "<BOL>KEYWORD", yytext+beginpos );
        return kw; ** find reserwed word **
 */
    } else {                                        /* identifier */
        if( ( ptr = malloc(yyleng-beginpos+1) ) == NULL )
            MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
        else {
            strcpy( ptr, yytext+beginpos );
            *(ptr+yyleng) = '\0';
            yylval.strptr = ptr;
            LFINDTOKEN( "<BOL>IDENTNAME", yytext+beginpos );
            return IDENTNAME;   /* classic identifier */
        }
    }
 }

<CODE>^[ \t]*"."{IDLETTER}+             {       /* operator keyword, .include or error */
    /* old: <CODE>^[ \t]*"."{LETTER}({IDLETTER}|{DECDIGIT})* */
    char    *yytextptr;
    int     kw;
    int     ip;

    if( Flags[flagU].value ) strupr( yytext ); /* if '-u' convert to uppercase */

 /* IncPos; (yyTokenPos += yyleng;) - not correct on '\t' ! */
    for( ip = 0; ip < yyleng; ip++ )
        if( yytext[ip] == '\t' )
             yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
        else yyTokenPos++;

    /* skip blanks after .XXXXXX: */
    yytextptr = yytext;
    while( *yytextptr == ' ' || *yytextptr == '\t' ) yytextptr++;

    LFINDTOKEN( ".XXXXXX", yytextptr );

    kw = LookupOperator( yytextptr ); /*ASB000124*/
    if( kw == FALSE ){                          /* error keyword */
        LFINDTOKEN( "(UNRECOGNIZED)", yytextptr );
        MsgCompile( msgILLKEYWORD, yytextptr );
    } else if( kw != _INCLUDE ){                /* keyword != .INCLUDE */
        LFINDTOKEN( ".KEYWORD", yytextptr );
        return kw; /* find reserwed word */
    } else {                                    /* .INCLUDE */
        int         ip, savpos, ch;

        LFINDTOKEN( ".INCLUDE", yytextptr );

        /* skip blanks after .INCLUDE: */
        while( ( ch = input() ) != '\n' ){
            if( ch == '\t' ) yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
            else             yyTokenPos++;
            if( ch != ' ' && ch != '\t' ) break;
        }
        unput( ch ); /* return to Lex last symbol !=' ' || !='\t' || =='\n' */
        if( ch != '\n' ) yyTokenPos--;
        LFINDTOKEN( "_blank(s)_", "" );

        /* get rest of line (required file name) up to blank or '\n': */
        for( ip = 0; ip < maxFNAMELEN; ip++ ) nameFileNew[ip] = '\0';/*clean*/
        for( ip = 0; ( ch = input() ) != '\n'; ip++ ){
            nameFileNew[ip] = ch;
            savpos = yyTokenPos;
            if( ch == '\t' ) yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
            else             yyTokenPos++;
            if( ch == ' ' || ch == '\t' ) break;
        }
        nameFileNew[ip] = '\0';
      # ifdef UPFNAME
        strupr( nameFileNew );
      # endif

        /* put back to input next after filename symbol and correct position#: */
        if( ch != '\n' && ch != '\t' ) yyTokenPos--;
        else if( ch == '\t' ) yyTokenPos = savpos;
        unput( ch );

        /* include file: */
        IncludeFile( nameFileNew, 1 );

        return _ENDOFSTMT_; /* pseudo-token */
    }
 }

<CODE>{IDLETTER}({IDLETTER}|{DECDIGIT})*  {    /* only identifier inside line */
    /* prev: <CODE>{LETTER}({IDLETTER}|{DECDIGIT})* */
    int     kw;                                /* old: identifier or keyword */
    char    *ptr;

    IncPos;

    if( Flags[flagU].value ) strupr( yytext ); /* if '-u' convert to uppercase */

 /* ASB000124: this part move to YACC-part:
    if( ( kw = LookupKeyword(yytext) ) != FALSE ){  ** keyword **
        LFINDTOKEN( "KEYWORD", yytext );
        Ldebug( "\t keyword #%d", kw );
        return kw;
    } else                                          ** identifier **
 */
        if( ( ptr = malloc(yyleng+1) ) == NULL )
            MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
        else {
            strcpy(ptr, yytext);
            *(ptr+yyleng) = '\0';
            yylval.strptr = ptr;
            LFINDTOKEN( "IDENTNAME", yytext );
            return IDENTNAME;   /* classic identifier */
        }
 }

<CODE>{FLOAT_NUM}({SCALESUFX})?         {       /* float number */
                                                /* that not be node/identifier */
                                                /* may be with scale suffix */
    char    *sfx;
    double  mult = 1.0;

    IncPos;

    if( Flags[flagU].value ) strupr( yytext ); /* if '-u' convert to uppercase */

    /* find legal scale suffix: */
    sfx = yytext;   /* skip not alpha symbols and alpha 'e' or 'E': */
    while( sfx < yytext+yyleng ){
        if( isalpha( *sfx ) && *sfx != 'e' && *sfx != 'E' ) break;
        sfx++;
    }
    if( sfx <= yytext+yyleng && isalpha( *sfx ) ) { /* find letter suffix */
      if(Flags[flagL].value){
        LFINDTOKEN( "(FLOAT+SUFFIX)", yytext );
        LMSG( msgEXTRACTSUFFIX, sfx );
      }
      mult = LookupScale( sfx );
      if( mult == 1.0 ){ /* suffix is not scale multiplier */
        LMSG( msgNOTSCALESUFX, sfx );
      } else {
        LMSG( msgSCALEQUAL, mult );
      }
      *sfx = '\0';  /* cut suffix in yytext */
    }
    yylval.floatnum = atof(yytext); /* sscanf( yytext, "%e", &yylval.floatnum ); */
    yylval.floatnum *= mult;
    LFINDTOKEN( "FLOATNUM", yytext ); /* float without scale */
    if(Flags[flagL].value){SHOW4tab;SHOWfloat(yylval.floatnum);}/* show real value */
    return FLOATNUM;
 }

<CODE>{FLOAT_ID}({SCALESUFX})?          {       /* float identifier */
                                                /* may be with scale suffix */
    value   *vp;
    char    *ptr;
    char    *sfx;
    double  mult = 1.0;

    IncPos;

    if( Flags[flagU].value ) strupr( yytext ); /* if '-u' convert to uppercase */

    /* allocate yylval.mixed: */
    if( ( vp = (value *)malloc( sizeof(value) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
    else {
        yylval.mixed = vp;
        if( ( ptr = malloc(yyleng+1) ) == NULL )
            MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
        else {
            strcpy(ptr, yytext);
            *(ptr+yyleng) = '\0';
            yylval.mixed->text = ptr;
        }
    }
    /* find legal scale suffix: */
    sfx = yytext;   /* skip not alpha symbols and alpha 'e' or 'E': */
    while( sfx < yytext+yyleng ){
        if( isalpha( *sfx ) && *sfx != 'e' && *sfx != 'E' ) break;
        sfx++;
    }
    if( sfx <= yytext+yyleng && isalpha( *sfx ) ) { /* find letter suffix */
      if(Flags[flagL].value){
        LFINDTOKEN( "IDENTIFIER | FLOAT+SUFFIX", yytext );
        LMSG( msgEXTRACTSUFFIX, sfx );
      }
      mult = LookupScale( sfx );
      LMSG( msgSCALEQUAL, mult );
    }
    yylval.mixed->floatval = atof(yytext);
    yylval.mixed->floatval *= mult;
 
    LFINDTOKEN( "IDENTFLOAT", yytext );
    if(Flags[flagL].value){ /* show real value and string */
        SHOW4tab; SHOWfloat( yylval.mixed->floatval );
        SHOW4tab; SHOWstr( yylval.mixed->text );
    }
    return IDENTFLOAT;
 }

<CODE>{DECIMAL_ID}({SCALESUFX})?        {       /* decimal identifier */
                                                /* may be with scale suffix */
    value   *vp;
    char    *ptr;
    char    *sfx;
    double  mult = 1.0;

    IncPos;

    if( Flags[flagU].value ) strupr( yytext ); /* if '-u' convert to uppercase */

    /* allocate yylval.mixed: */
    if( ( vp = (value *)malloc( sizeof(value) ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
    else {
        yylval.mixed = vp;
        if( ( ptr = malloc(yyleng+1) ) == NULL )
            MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
        else {
            strcpy(ptr, yytext);
            *(ptr+yyleng) = '\0';
            yylval.mixed->text = ptr;
        }
    }
    /* find legal scale suffix: */
    sfx = yytext;   /* skip not alpha symbols and alpha 'e' or 'E': */
    while( sfx < yytext+yyleng ){
        if( isalpha( *sfx ) && *sfx != 'e' && *sfx != 'E' ) break;
        sfx++;
    }
    if( sfx <= yytext+yyleng && isalpha( *sfx ) ) { /* find letter suffix */
        LFINDTOKEN( "IDENTIFIER | DECIMAL+SUFFIX", yytext );
        LMSG( msgEXTRACTSUFFIX, sfx );
        mult = LookupScale( sfx );
        LMSG( msgSCALEQUAL, mult );
        *sfx = '\0';  /* cut suffix in yytext */
    }
    if( mult == 1.0 ){  /* suffix not found - this is decimal ident */
        yylval.mixed->longval = atol(yytext);

        LFINDTOKEN( "IDENTINT", yytext );
        if(Flags[flagL].value){ /* show real value and string */
            SHOW4tab; SHOWlong( yylval.mixed->longval );
            SHOW4tab; SHOWstr( yylval.mixed->text );
        }
        return IDENTINT;
    } else {            /* suffix found - this is float ident */
        yylval.mixed->floatval = atof(yytext);
        yylval.mixed->floatval *= mult;

        LFINDTOKEN( "IDENTFLOAT", yytext );
        if(Flags[flagL].value){ /* show real value and string */
            SHOW4tab; SHOWfloat( yylval.mixed->floatval );
            SHOW4tab; SHOWstr( yylval.mixed->text );
        }
        return IDENTFLOAT;
    }
 /* <CODE>{DECIMAL_NUM}({SCALESUFX})? ** decimal num - removed */
 }

<CODE>[-+*/^(),=.]                      {       /* legal single symbols */
    IncPos;
    LFINDTOKEN( "SYMBOL", yytext );
    return yytext[0];
 }

<CODE>[ ]                               {       /* blank */
    IncPos;
 }
<CODE>[\t]                              {       /* tabulation */
 /* IncPos; (yyTokenPos += yyleng;) - not correct on '\t' ! */
    yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
 }

<CODE>.                                 {       /* unrecognized symbol */
    /* "." - not recognize extended symbols [\128-\255] */
    /* <CODE>{EXTSYM} - not work correct, not portable (EXTSYM=[\128-\241]) */
    /* if EXTSYM == [\128-\255] - crashed */
    IncPos;
    LFINDTOKEN( msgUNRECOGNSYM, yytext );
    MsgCompile( msgILLSYMB, yytext[0], yytext[0] );

 /* ! don't use this rule: */
 /*<CODE>[^ \t\n=,()]*                     {*/
                                                /* unrecognized token */
 /*   IncPos;*/
 /*   LFINDTOKEN( "(UNRECOGNIZED)", yytext );*/
 /*   MsgCompile( msgILLTOKEN, yytext );*/
 /*}*/

 }

%%

/*-------------------------------------------------------------------------*/
    static int
LookupOperator(  /* look up operator keyword */ /*ASB000124*/
    char    *str
){
    KeysTab *kt;
/*
    static char ucstr[maxINPLINELEN];
    strcpy(ucstr, str);
    strupr(ucstr);
*/
    for( kt = OperKeysTab; kt->string != NULL; ++kt )
        if( strEQ( str, kt->string ) )  return kt->value;
/*      if( streq( ucstr, kt->string ) )  return kt->value; */
    return FALSE;
}/*LookupOperator()*/
/*-------------------------------------------------------------------------*/
    static  double
LookupScale(/* scale suffix look up function */
    char    *str
){
    MultTab *mt;

    static char ucstr[maxINPLINELEN];
    strcpy(ucstr, str);
    strupr(ucstr);
    for( mt = ScaleSufxTab; mt->string != NULL; ++mt )
        if( strncmp( ucstr, mt->string, strlen(mt->string) ) == 0 )
            return mt->mult;
    return 1.0;
}/*LookupScale()*/
/*-------------------------------------------------------------------------*/
    static  int
LookupElem( /* network elements prefix look up function */
    char    *str
){
    KeysTab *kt;
/*
    static char ucstr[maxINPLINELEN];
    strcpy(ucstr, str);
    strupr(ucstr);
*/
    for( kt = NetElemPrefTab; kt->string != NULL; ++kt )
        if( strEQ( str, kt->string ) )  return kt->value;
/*      if( streq( ucstr, kt->string ) )  return kt->value; */
    return FALSE;
}/*LookupElem()*/
/*-------------------------------------------------------------------------*/
    static  void
ShowLine(   /* print current line number to console, to debug */
    char *etc
){
    if( yyTokenLine <= 0 ) return; /*ASB000119*/

    /* ASB000221 - not show included files-subcircuits: */
    if( isSubCall != TRUE || (levelSubCall == 0) ){
 /*   MsgConsole( "\r%s%6d  (%s)    \r", msg_LINE, yyTokenLine, nameFileInp ); */
      MsgConsole( "\r%s%6d  (%s)      ", msg_LINE, yyTokenLine, nameFileInp );
    }
    MsgDebug( "%s %d (%s): %s\n", msg_NEWLINE, yyTokenLine, nameFileInp,
                            (Flags[flagL].value)? etc: "" );
    PrintLine( fileDebug, yyTokenLine/*yylineno*/ );

}/*ShowLine()*/
/*-------------------------------------------------------------------------*/
    void
SkipRestLine(   /* skip rest of input line */
    void
){
    int ch = 0;

 #ifdef DEBUG_SKIPEOLN
    if( Flags[flagL].value ){
        MsgDebug( msgSKIPRESTLINE );
        MsgDebug( "'" );
    }
 #endif

    while( ( ch = input() ) != '\n' ){
        if( ch == 0 ) break; /*EOF*/
        if( ch == '\t' ) yyTokenPos += TABSIZE - (yyTokenPos % TABSIZE);
        else             yyTokenPos++;
 #ifdef DEBUG_SKIPEOLN
        if( Flags[flagL].value ) MsgDebug( "%c", ch );
 #endif
    }/*while*/

 #ifdef DEBUG_SKIPEOLN
    if( Flags[flagL].value ) MsgDebug( "'\n" );
 #endif

    unput( '\n' ); /* '\n' return to Lex for correct recognize next line */
                   /* as new statement, not continuation of old statement */

}/*SkipRestLine()*/
/*-------------------------------------------------------------------------*/
    void
IncludeFile(            /* process .include-file */
    char *nameFileIncl, /* .include-file name */
    int  startline      /* start line in include-file */
){

/* Ldebug( "IncludeFile(): begin of process file '%s':\n", nameFileIncl );/**/
/* SHOWint(levelSubCall);/**/
/* SHOWint(isSubCall);/**/

        /* show file name to debug: */
        if( strlen( nameFileIncl ) > 0 ){ /* filename not empty */
            LFINDTOKEN( "INCLUDE_FILE", nameFileIncl );
        } else {                         /* filename is empty - include fail */
            LFINDTOKEN( "INCLUDE_FILE", "<nothing>" );
            MsgCompile( msgNOINCLFILE, nameFileIncl );
            goto end_of_incl_rule;
        }

        /* if obtain max include nesting skip include processing: */
        if( cntInclNest >= maxINCLNEST ){
            MsgDebug( msgMAXINCLNEST, maxINCLNEST ); MsgDebug( "\n" );
      #  ifndef INCL_NEST_EXCEED_IS_FATAL
            MsgCompile( msgMAXINCLNEST, maxINCLNEST );
      #  else 
            /* fatal exit if including is recursive: */
            MsgErrorFatal( exitMAXINCLNEST, msgFEMAXINCLNEST, maxINCLNEST );
      #  endif
            goto end_of_incl_rule;
        }

      # ifdef TEST_INCL_EXIST
        /* test for exist include file: */
        if( !FileExist( nameFileIncl ) ){  /* - not work correct */
            if(Flags[flagL].value)
            MsgDebug( msgINCFILNOTFND, nameFileIncl ); MsgDebug( "\n" );
            MsgCompile( msgINCFILNOTFND, nameFileIncl );
            goto end_of_incl_rule;
        }
      # endif

        /* push file and current line number in stack: */
        if( !PushFileStack( nameFileInp, /*yylineno*/yyTokenLine ) ){
            MsgDebug( msgPUSHFAIL );
            goto end_of_incl_rule;
        }

        /* close old input file: */
        CloseInput();

        /* prepare to show lines# on console for new file: */
        if( isSubCall != TRUE ) /*ASB000221 - not show included files-subcircuits*/
            MsgConsole( "\n" );

        /* skip Lex buffer: */
     /* yysbuf[YYLMAX-1] = 0; *(yysptr+1) = 0; SHOWstr(yysbuf); /* show buffer */
        YY_FLUSH_BUFFER; // yySkipUnput; /* skip buffer */

        /* try to open new input file: */
        if( !OpenInput( nameFileIncl ) ){ /* fail to open new include file */

            MsgDebug( msgERRINCLOPEN, nameFileIncl, yyTokenLine, nameFileInp );
                MsgDebug( "\n" );
         /* MsgCompile( msgERRINCLOPEN, nameFileIncl, yyTokenLine, nameFileInp ); */
                /* MsgCompile() not work if fileInp closed */
            MsgErrorCnt( msgERRINCLOPEN, nameFileIncl, yyTokenLine, nameFileInp );
                /* MsgErrorCnt() also increase cntErrors as MsgCompile() */
            MsgError( "\n" );

            /* return to just now closed file: */
            if( !ReturnPrevFile( TRUE ) ) /* return after .include */
                return; /*0*//*EOF*/

            input(); /* skip Lex buffer */

        } else { /* open new include file is OK */

            /* initial set of position and line number of new file: */
            yyTokenPos = 0;
            /*yylineno*/yyTokenLine = startline;

            /* skip lines: */
            SkipLines( fileInp, startline-1 ); /* if startline==1 no skip */

            unput('\n'); /* to avoid skip 1st line in new file */
                         /* unput('\n') => yylineno-- ! */

 /*         is1stline = TRUE; /* say to Lex about 1st line of new file */
            begin CODE; /*!*/
        }
 /*     SHOWint(yyTokenLine); /**/

            /* not skip rest of line in end_of_incl_rule: */
        return; /* _ENDOFSTMT_ pseudo-token */

    end_of_incl_rule:
        SkipRestLine(); /* skip rest of line in previous file */

}/*IncludeFile()*/
/*-------------------------------------------------------------------------*/
    static int  /* returned TRUE if OK */
PushFileStack(  /* function saved filename and lineno in stack */
    char *name, /* saved filename */
    int  lno    /* saved line number */
){
    StackFiles  *ptrTmp;
    char        *strnew;

/*#define TEST_INCL_NEST_HERE /**/
 # ifdef  TEST_INCL_NEST_HERE
    if( cntInclNest >= maxINCLNEST ){ /* obtain max include nesting */
        MsgDebug( msgMAXINCLNEST, maxINCLNEST ); MsgDebug( "\n" );
  # ifndef INCL_NEST_EXCEED_IS_FATAL
        MsgCompile( msgMAXINCLNEST, maxINCLNEST );
  # else 
     /* exit if including is recursive */
        MsgErrorFatal( exitMAXINCLNEST, msgFEMAXINCLNEST, maxINCLNEST );
  # endif
        return FALSE; 
    }
 # endif
 # ifdef DEBUG_STACK
    if( Flags[flagL].value )
        MsgDebug( msgPUSHSTACK, name, lno );
 # endif
    if( ( ptrTmp = (StackFiles *)malloc(sizeof(StackFiles)) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
    if( ( strnew = (char *)malloc( strlen(name)+1 ) ) == NULL )
        MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
    strcpy( strnew, name );
    ptrTmp->fname   = strnew;
    ptrTmp->linenum = lno;

    /* ASB000318: removed subcirc processing here */

    ptrTmp->prev    = ptrStack;
    ptrStack        = ptrTmp;
    cntInclNest++;

 # ifdef DEBUG_WRAP
    MsgDebug( msgNESTOFINCL, cntInclNest );
 # else
    if( Flags[flagL].value ) /**/
        MsgDebug( msgNESTOFINCL, cntInclNest );
 # endif

    return TRUE;
}/*PushFileStack()*/
/*-------------------------------------------------------------------------*/
    static int  /* returned line number or 0 if stack is empty */
PopFileStack(   /* function restored filename and lineno from stack */
    char *name  /* returned filename */
){
    StackFiles  *ptrTmp;
    int lno;

    if( ptrStack == NULL )
        return 0;
    else {
 # ifdef DEBUG_STACK
        if( Flags[flagL].value )
            MsgDebug( msgPOPSTACK, ptrStack->fname, ptrStack->linenum );
 # endif
        strcpy( name, ptrStack->fname );

        /* MsgDebug( msgAVAILMEM, coreleft() ); /**/
        /* MsgDebug("free( ptrStack->fname );\n");/**/
        free( ptrStack->fname ); /* compiler not free memory actually ! */
        /* MsgDebug( msgAVAILMEM, coreleft() ); /**/

        /* ASB000318: removed subcirc processing here */

        lno      = ptrStack->linenum;
        ptrTmp   = ptrStack;
        ptrStack = ptrStack->prev;
        free( ptrTmp ); /* free top of stack */ /* compiler not free memory actually ! */
        cntInclNest--;

 # ifdef DEBUG_WRAP
    MsgDebug( msgNESTOFINCL, cntInclNest );
 # else
    if( Flags[flagL].value )
        MsgDebug( msgNESTOFINCL, cntInclNest );
 # endif

        return lno;
    }
}/*PopFileStack()*/
/*-------------------------------------------------------------------------*/
    int         /* return TRUE if OK, FALSE - if stack empty or fail */
ReturnPrevFile( /* restore input file from stack after .include or circ subcall */
    int isFromInclude /* = TRUE if return after .include, not subcall */
){
    int   lno;

SHOWCIRCSTATE("ReturnPrevFile()...");/**/

    /* skip Lex buffer: */
 /* yysbuf[YYLMAX-1] = 0; *(yysptr+1) = 0; SHOWstr(yysbuf); /**/
    YY_FLUSH_BUFFER; //yySkipUnput;

    lno = PopFileStack( nameFileNew ); /* pop file from stack */

    if( lno != 0 ){
        if( fileInp != NULL ){ /* CloseInput() not need if .include failed */
            if(Flags[flagL].value) MsgDebug( msgRETformINCL, nameFileInp );
            CloseInput();
        }
        if( !OpenInput( nameFileNew ) ){
            MsgError( msgERRINCLRETN, nameFileNew );
            return FALSE;
        }

        /* for subcall: */
        if( !isFromInclude && isSubCall && (levelSubCall > 0)
            && (circCall == circDefCurrent) )
                lno--; /*ASB000401*/
     /* if( isSubCall && levelSubCall > 0 ) lno--; /*ASB000221*/

        /* skip 'lno' lines in input file: */
        SkipLines( fileInp , lno );

        if( isSubCall != TRUE ) /*ASB000221 - not show included files-subcircuits*/
            MsgConsole( "\n" ); /* prepare to show lines# on console for new file */

        /* set position and line numbers: */
        yyTokenPos = 0;
        /*yylineno*/yyTokenLine = lno + 1; /* next after .include line */

        begin CODE; /*!*/
        unput('\n'); /* '\n' return to Lex for correct recognize next line */
                     /* as new statement, not continuation of old statement */

        return TRUE;
    } else /* stack is empty */
        return FALSE;

}/*ReturnPrevFile()*/
/*-------------------------------------------------------------------------*/
    static  void
Ldebug(     /* print debug message */
    char    *msgfmt,    /* Don't use '\n' in message */
            ...
){
    va_list arguments;
    va_start( arguments, msgfmt );

    if( yyTokenLine <= 0 ) return; /*ASB000119*/

    if( Flags[flagL].value ){
      if( yyTokenPos == 0   /* avoid jump to next string *//* add by ASB000117 */
       && yyTokenLine > 1 ) /*ASB000120*/
        MsgDebug( "   *lex* ln,pos=%d,end:\t", yyTokenLine-1 );
      else
        MsgDebug( "   *lex* ln,pos=%d,%d:\t", yyTokenLine, yyTokenPos );

   /* MsgDebug( msgfmt, arguments ); *//* - not work */
      vsprintf( linebuffer, msgfmt, arguments ); /*OK*/
      MsgDebug( linebuffer );                    /*OK*/
      va_end( arguments );
      MsgDebug( "\n");
    }
}/*Ldebug()*/
/*-------------------------------------------------------------------------*/
    int     /* return TRUE to exit, FALSE - to continue processing */
yywrap(     /* instead of yywrap() from LEXLIB.LIB */
    void    /* if files stack not empty - return to previous file */
){
 /* MsgDebug("yywrap()...");/**/
    /* try to return file at the EOF (after .include, not from subcall): */
    if( ReturnPrevFile( TRUE ) )
        return FALSE; /*OK*/
    else {                  /* EOF of last file */
        CloseInput(); /*!*/
 #      ifdef DEBUG_WRAP
          MsgDebug( msgYYWRAP_1 ); SHOWptr(fileInp); /**/
 #      endif
        MsgConsole( "\n" ); /* end of show line# */

        /* end of all files processing: */
        isEOF = TRUE;
        return TRUE;
    }
}/*yywrap()*/
/*-------------------------------------------------------------------------*/
