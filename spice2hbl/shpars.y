
%{
/*
 *  S H p a r s . Y  --  S2H-compiler:  Grammar parser
 *
 *
 *  shift/reduce conflicts:
 *   (+ 2  - if allow .MODEL continue to next line without '+' )
 *   -----------
 *    = 0 total
 *
 *  reduce/reduce conflicts:
 *   (+ 1  - on rule SemiTransistor if Node*S* may be identifier )
 *   -----------
 *    = 0 total
 *
 *
 *  History:
 *    31.12.1999 (v.0.01) - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 *    20.01.2000 (v.0.35) - first version of grammar complete (ASB)
 *    29.01.2000 (v.0.44) - second version of grammar complete without more bugs (ASB)
 *    04.03.2000 (v.0.66) - add some new Spice-constructions (ASB)
 *    22.03.2000 (v.0.78) - add subcircuit and subcall processing (ASB)
 *    21.10.2019 (v.0.80) - port to modern parser generator -Gennadiy Serdyuk (GS)
 */
/*-------------------------------------------------------------------------*/

/* # define YYDEBUG      /* YACC internal debug info */
/* # define YYERRENABLE  /* enable yyerror() */

#include "shcommon.h"

/*
# if defined(BSD)
#  include   "y.tab.h"
# endif
*/
/*-------------------------------------------------------------------------*/

static char linebuffer[maxINPLINELEN]; /* for temporary strings */
static char parmbuffer[maxINPLINELEN]; /* for temporary strings */

/* static int savLineNo; /* saved line number (yyTokenLine/lineno) */

StackCirc *stackCircuits  = NULL; /* stack of subcircuits */
SubCirc   *circDefCurrent = NULL; /* pointer to current subcircuit */
                                  /* if parser inside _subckt ... _ends, */
                                  /* = NULL for main circuit */
StackCalls *stackCallsHead = NULL; /* stack of subcalls (subcalls path) */
StackCalls *stackCallsTail = NULL; /* tail of subcalls stack */

int isSubDefBody;      /* set TRUE if parser go into subcircuit definition */
int levelSubDef = 0;   /* counter of nested circuits definitions */

int isSubCall = FALSE; /* set when operator Xxxx in process */
int levelSubCall = 0;  /* counter of nested circuits subcall */

SubCirc *circCall = NULL; /* pointer to called subcircuit, default = NULL */
static char    *namePrepCall = NULL; /* name of prepared subcall */
static SubCirc *circPrepCall = NULL; /* prepare circuit to call */

static SubParm *parmSubCall = NULL; /* Xxxx nodes-parameters */
static long    cntParmSubCall;      /* counter of Xxxx nodes-parameters */

static ModelList *modelCurr = NULL; /* pointer to current model */

int cnt_SUBCKT = 0, cnt_ENDS = 0; /* counters of circuit brackets */
int cnt_END = 0; /* .END indicator */

/*parameters:*/
static int existHB00, cntHB;
static int existHB = FALSE; /* = TRUE if .HB-statement already exist in program */
static int exist_HB_F2; 
double parm_HB_F1, parm_HB_F2; /* extern parameters of .HB-operator */

static int isGenOK;
static int existZ0;       static double parmZ0;
static int existTD;       static double parmTD;
static int existF_FREQ;   static double parmF_FREQ;
static int existNL;       static double parmNL;
                          static double parmLength;
static int existDiodArea;   static double parmDiodArea;
static int existMesfetArea; static double parmMesfetArea;

/* for Ixxx, Vxxx processing: */
static int    isIxxx, isVxxx;       /* indicate type of processing element */
static char   *IxxxName, *VxxxName; /* name of processing element */
static int    isIVxxxOpt;           /* indicate valid processing options */
static double IVxxxGi, IVxxxF, IVxxxFi, IVxxxJm; /* save current I/Vxxx options */
static char   *RiVxxxName; /* virtual resistor name for Vxxx */
static double VxxxRi;    /* virtual resistor value for Vxxx */

/*-------------------------------------------------------------------------*/
%}

%union {
    long    longnum;    /* integer value */
    double  floatnum;   /* float value */
    char    *strptr;    /* pointer to identifier */
    value   *mixed;     /* mixed value for number-identifier */
}

/* pseudo-tokens: */
%token  _ENDOFSTMT_     /* end of line = end of instruction (pseudo-token) */
%token  _INCLUDE        /* .include keyword - use only in Lex */
%token  tokenUNKNOWN    /* <unknown> */

/* tokens: */
%token <strptr>     IDENTNAME   STRING
%token <floatnum>   FLOATNUM
%token <longnum>    DECINTNUM
%token <value>      IDENTINT    IDENTFLOAT      /* number-identifier */

/* network elements: */
%token <strptr> RESISTOR CAPACITOR  INDUCTOR INDUCTCOUPL
%token <strptr> CURRENT  VOLTAGE    ISWITCH  VSWITCH
%token <strptr> DIODE    TRANSISTOR JFET     MOSFET   MESFET        
%token <strptr> LLTLINE  LTLINE     UDRCLINE
%token <strptr> LCCCS    LCCVS      LVCCS    LVCVS    NLDS
%token <strptr> SUBCALL

/* operator keywords: */
%token  _AC     _DC     _DISTO  _END    _ENDS   _IC     _MODEL  _NODESET
%token  _NOISE  _OP     _PZ     _SENS   _SUBCKT _TF
%token  _TRAN   _WIDTH  _OPTIONS
%token  _SAVE   _PLOT   _PRINT  _FOUR
%token  _TLINE  /*???*/
%token  _ENDC   _CONTROLS       /*add ASB000303*/
%token  _ALTER  _WRITE  _LET    /*add ASB000303*/
%token  _HB     _HB_OPTIONS  /* for Habala only */

/* options keywords: */
%token  AC      AD      AS      CUR     DC      DEC     DISTOF1 DISTOF2 DISTO
%token  EXP     F_FREQ  I_SRC   IC      L_LEN   LIN     N_LUMPS NL      NOISE
%token  NRD     NRS     OCT     OFF     ON      PD      POL     PS      PULSE
%token  PWL     PZ      SFFM    SIN     TD      TRAN    TEMP    UIC     V_SRC
%token  VR      VI      VM      VP      VDB  /* - for .PRINT/.PLOT only */
%token  VOL     W_WIDTH Z0      ZER
/* %token  POLY5   JUNC    /* for Habala only */

/*%type <longnum>   OptionStartKeyword */
%type   <strptr>    startVoltCurrOption
%type   <strptr>    bjtSnode

/* neterminals types: */
%type   <strptr>    Identifier  ModelParm   /*ExtIdentifier*/  /*Name*/
%type   <strptr>    NodeName    Node
%type   <floatnum>  Float
%type   <longnum>   Switch      DecInt
%type   <longnum>   PrintType   CURorVOL    DECorOCTorLIN   POLorZERorPZ

/* for arithmetic expressions - not use: */
%type   <floatnum>  Value
/*%type <floatnum>  ArExpr /* arithmetic expression *//*rem ASB000119*/
%left   <floatnum>  '^'
%left   <floatnum>  '+' '-'
%left   <floatnum>  '*' '/'
%right  <floatnum>  UMINUS  /* %precedence for unary '-' */
%left               BRACK   /* %prec () */

%start  Program

%{
/*  ASB000603: keywords table - move to SHparsY.C:
KeysTab KeywordsTab[] = {...};
*/
%}

%%

/*=========================================================================*/
Program
 : /* start actions: */
                {
                   cnt_SUBCKT = 0; cnt_ENDS = 0; /* counters of circuit brackets */
                   cnt_END = 0;                  /* .END indicator */
                   existHB       = FALSE; /* = TRUE if .HB-statement already exist */
                   stackCircuits = NULL;  /* stack of subcircuits */

                   stackCallsHead = NULL; /* stack of subcalls (subcalls path) */
                   stackCallsTail = NULL; /* tail of subcalls stack */

                   /* states for subcirc processing: */
                   isSubCall    = FALSE;  /* set when operator Xxxx in process */
                   levelSubCall = 0;      /* counter of nested circuits subcall */
                   circCall     = NULL;   /* pointer to called subcircuit */

                   isSubDefBody = FALSE;  /* process main circuit */
                   levelSubDef  = 0;      /* counter of nested circuits definitions */
                   circDefCurrent = NULL; /* pointer to current subcircuit */
                                          /* if parser inside _subckt ... _ends, */
                                          /* = NULL for main circuit */
                   parmSubCall = NULL;    /* Xxxx nodes-parameters */
                   namePrepCall = NULL;   /* name of prepared subcall */
                   circPrepCall = NULL;   /* prepare circuit to call */

                   isIxxx = isVxxx = isIVxxxOpt = FALSE;
                   IxxxName = VxxxName = NULL;

                   exist_HB_F2 = FALSE; /* initial values for .HB */
                   if( PassNo == 1 ){
                     parm_HB_F1 = 0.0;
                     parm_HB_F2 = 0.0;
                   }
/*                 if( PassNo == 2 ) SHOWCIRCSTATE( "Begin pass 2" );/**/
                 }
   ProgramBody  { CheckCircBrackets(); }
 ;
ProgramBody
 : StatementList _END
                { MsgConsole( msgPASSCOMPLETE, PassNo );
                   cnt_END++; /* .END indicator */
                   CheckCircBrackets();
                   return cntErrors;
                 }
/*
 | StatementList error
                { MsgErrorCnt( msgNEED_END ); /* have ++cntErrors; */
                              /*MsgCompile( msgNEED_END );*/
                   /* CheckCircBrackets(); */
/*                 return cntErrors; 
                 }
   /*_ENDOFSTMT_*/ /* _ENDOFSTMT_ at the end of this rule
                    * produce 1 reduce/reduce conflicts on next rule:
                    *   "Program : StatementList error _ENDOFSTMT_"
                    */
 ;
StatementList
 : StatementItem { TEST_EOF;
                    CLR_ELEM_PARM;
                  }
 | StatementList StatementItem
                 { TEST_EOF;
                    CLR_ELEM_PARM;
                  }
 ;
StatementItem
 :
   _ENDOFSTMT_ /* "free line" - produse bug in programs without .END */
                { TEST_EOF; }
 | error        { TEST_EOF;
                   MsgCompile( msgERRSTMT );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
/*
 | _END         { cnt_END++; /* .END indicator */
/*                 return cntErrors;
                 }
*/
 | StmtElement
 | StmtSwitch/**/
 | StmtTransmissionLine/*T,O*/
 | StmtModel
 | StmtControlLines /*add ASB000303*/
 | StmtCtrlLine
 | StmtOutput
 | StmtSubCircuit
 | CallSubCircuit
 ;
StmtElement
 : ElemINDUCTOR/*L,K*/
 | ElemRESISTOR/*R*/
 | ElemCAPACITOR/*C*/
 | ElemVOLTAGE/*V*/
 | ElemCURRENT/*C*/
 | ElemLVCCS/*E*/
 | ElemLVCVS/*G*/
 | ElemLCCCS/*F*/
 | ElemLCCVS/**/
 | ElemNLDS/**/
 | SemiDiode/*D*/
 | SemiTransistor/*Q*/
 | SemiJFET/*J*/
 | SemiMOSFET/*M*/
 | SemiMESFET/*Z*/
 ;
/*=========================================================================*/
StmtModel /*.MODEL*/
 :
   _MODEL error
                { MsgCompile( msgERR_MODEL );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _MODEL Identifier Identifier
                { Ydebug( "Model '%s' (type '%s')", $2, $3 );
                   modelCurr = NULL;
                   if( PassNo == 1 ){
                     listModels = AddToModelList( $2, $3, circDefCurrent );
                   }
                   modelCurr = FindModel( $2, circDefCurrent );
                 }
   maybeModelParameters
                {
                   modelCurr = NULL;
                 }
   _ENDOFSTMT_
 ;
maybeModelParameters
 : /*nothing*/
 | ModelParameters
 | '(' ModelParameters BRKEND/*')'*/
 ;
ModelParameters
 : ModelParameter
 | ModelParameters ModelParameter
 | ModelParameters '(' ModelParameter ')'
 ;
ModelParameter
 : ModelParm mbModelAssign Value
                { Ydebug( "... model parameter '%s' = %g", $1, $3 );
                   if( PassNo == 1 ){
                     if( FindModelParm( modelCurr, $1 ) )
                       MsgCompile(  msgMODELPARMEXIST, $1, modelCurr->modelname );
                     else
                       AddModelParm( modelCurr, $1, TRUE, $3 );
                   }
                 }
 | ModelParm    { Ydebug( "... model parameter '%s'", $1 );
                   if( PassNo == 1 ){
                     if( FindModelParm( modelCurr, $1 ) )
                       MsgCompile(  msgMODELPARMEXIST, $1, modelCurr->modelname );
                     else
                       AddModelParm( modelCurr, $1, FALSE, 0.0 );
                   }
                 }
/* ?:
 | Value        { Ydebug( "... model parameter-value %g", $1 );
                   if( PassNo == 1 ){
                     AddModelParm( modelCurr, NULL, TRUE, $1 );
                   }
                 }
*/
 ;
mbModelAssign : /*nothing*/ | '=' | ',' ;
ModelParm
 : Identifier /* in the middle of the statement line */
                { $<strptr>$ = $<strptr>1; }

/* next part used only if .MODEL statement continue on next line witout '+': */
/* (produce many shift/reduce conflicts) */

/* | _ENDOFSTMT_  Identifier /* at the begin of the statement line */
/*                { $<strptr>$ = $<strptr>2; }             /*Identifier*/
                                                          /*Axxx*/
/* | _ENDOFSTMT_ NLDS         { $<strptr>$ = $<strptr>2; } /*Bxxx*/
/* | _ENDOFSTMT_ CAPACITOR    { $<strptr>$ = $<strptr>2; } /*Cxxx*/
/* | _ENDOFSTMT_ DIODE        { $<strptr>$ = $<strptr>2; } /*Dxxx*/
/* | _ENDOFSTMT_ LVCVS        { $<strptr>$ = $<strptr>2; } /*Exxx*/
/* | _ENDOFSTMT_ LCCCS        { $<strptr>$ = $<strptr>2; } /*Fxxx*/
/* | _ENDOFSTMT_ LVCCS        { $<strptr>$ = $<strptr>2; } /*Gxxx*/
/* | _ENDOFSTMT_ LCCVS        { $<strptr>$ = $<strptr>2; } /*Hxxx*/
/* | _ENDOFSTMT_ CURRENT      { $<strptr>$ = $<strptr>2; } /*Ixxx*/
/* | _ENDOFSTMT_ JFET         { $<strptr>$ = $<strptr>2; } /*Jxxx*/
/* | _ENDOFSTMT_ INDUCTCOUPL  { $<strptr>$ = $<strptr>2; } /*Kxxx*/
/* | _ENDOFSTMT_ INDUCTOR     { $<strptr>$ = $<strptr>2; } /*Lxxx*/
/* | _ENDOFSTMT_ MOSFET       { $<strptr>$ = $<strptr>2; } /*Mxxx*/
                                                          /*Nxxx*/
/* | _ENDOFSTMT_ LTLINE       { $<strptr>$ = $<strptr>2; } /*Oxxx*/
                                                          /*Pxxx*/
/* | _ENDOFSTMT_ TRANSISTOR   { $<strptr>$ = $<strptr>2; } /*Qxxx*/
/* | _ENDOFSTMT_ RESISTOR     { $<strptr>$ = $<strptr>2; } /*Rxxx*/
/* | _ENDOFSTMT_ VSWITCH      { $<strptr>$ = $<strptr>2; } /*Sxxx*/
/* | _ENDOFSTMT_ LLTLINE      { $<strptr>$ = $<strptr>2; } /*Txxx*/
/* | _ENDOFSTMT_ UDRCLINE     { $<strptr>$ = $<strptr>2; } /*Uxxx*/
/* | _ENDOFSTMT_ VOLTAGE      { $<strptr>$ = $<strptr>2; } /*Vxxx*/
/* | _ENDOFSTMT_ ISWITCH      { $<strptr>$ = $<strptr>2; } /*Wxxx*/
/* | _ENDOFSTMT_ SUBCALL      { $<strptr>$ = $<strptr>2; } /*Xxxx*/
                                                          /*Yxxx*/
/* | _ENDOFSTMT_ MESFET       { $<strptr>$ = $<strptr>2; } /*Zxxx*/
 ;
/*=========================================================================*/
StmtSubCircuit /*.SUBCKT*/
 : _SUBCKT error { MsgCompile( msgERR_SUBCKT );
                    YERRSKIP;
                    cnt_SUBCKT++; /* counter of circuit brackets */
                 }
   _ENDOFSTMT_
 |
   _SUBCKT NodeName/*Identifier*/ /* subcirc name may begin with digit */
                { Ydebug( "Begin subcircuit '%s' definition body...", $2 );
                   SHOWCIRCSTATE( "Pass1,2: before process '.subckt'" );/**/

                   isSubDefBody = TRUE;
                   levelSubDef++;
                   PushCircStack();

                   if( PassNo == 1 ){
                     SubCirc *circThis;
                     cnt_SUBCKT++; /* counter of circuit brackets */
                     circThis = FindSubCirc( listSubCirc,
                                             circDefCurrent/*parent*/, $2 );
                     if( circThis != NULL ){
                       MsgCompile( msgCIRCDEFEXIST, $2, (circDefCurrent==NULL)?
                                     msgMainCircuit: circDefCurrent->circname);
                     /*MsgErrorFatal( exitPARSEREXIT, msgPARSEREXIT );/*exit*/
                       /* if not exit here, parser give for this circuit */
                       /* many errors about elements with equal names */
                     }else{ /* not found: add to list */
                       Ydebug( "Pass1: add to list subcirc def '%s', 1st line#", $2 );
                       listSubCirc = AddSubCirc( listSubCirc,
                                                 circDefCurrent/*parent*/, $2 );
                       circThis = FindSubCirc( listSubCirc,
                                               circDefCurrent/*parent*/, $2 );
                     }
                     circDefCurrent = circThis;
                   }/*pass1*/

                   if( PassNo == 2 ){
                     SubCirc *circThis;
                /*   circThis = FindSubCirc( listSubCirc,
                         circDefCurrent/*parent*\, $2 ); /* in current */
                /**/ circThis = FindSubCircHigh( listSubCirc,
                         circDefCurrent/*parent*/, $2 ); /* in current and in parents */
                     if( !isSubCall ){
                       Ydebug( "begin of skipping subcirc definition body..." );
                     } else if( circCall == circThis ){ /* && isSubCall */
                       Ydebug( "process call subcircuit ..." );
                       /* AddCallPath( $2 ); /* move in Xxxx processing */
                       if( Flags[flag2].value ){
                         MsgDebug( "\t\tBegin process subcall path: " );
                         OutCallPath( fileDebug );
                         MsgDebug( " ...\n" );
                       }
                     } else { /* isSubCall but circCall != circThis */
                       Ydebug( "begin of skipping subcirc def body in subcall ..." );
                     }
                     circDefCurrent = circThis; /**/
                   }/*pass2*/
                   SHOWCIRCSTATE( "Pass1,2: after process '.subckt'" );/**/
                 }
   SubCircNodeList
   SubCircStatementList
                { Ydebug( "... end of body subcircuit '%s'.", $2 );

                   if( PassNo == 1 ){
                     SHOWCIRCSTATE( "Pass1: before process '.ends'" );/**/
                     PopCircStack();
                     SHOWCIRCSTATE( "Pass1: after restore subcirc states" );/**/

                     if( Flags[flagG].value || Flags[flag1].value || Flags[flag2].value )
                     { MsgDebug( "circDefCurrent: " );
                       OutFullCircName( fileDebug, circDefCurrent );
                       MsgDebug( "\n" );
                     }
                     Ydebug( "Pass1: save last line# for .ENDS '%s'", $2 );
                     if( AddSubCircEnd( listSubCirc, circDefCurrent/*->parent*/, $2 )
                         == FALSE )
                       MsgCompile( msgERR_ENDS, $2 );
                     else { /* test if .subckt and .ends in same file: */
                       SubCirc *cr;
                       cr = FindSubCirc( listSubCirc, circDefCurrent/*->parent*/, $2 );
                       if( !strEQ( cr->begfname, cr->endfname ) )
                         MsgCompile( msgCIRCNOTIN1FILE, $2 );
                     }
                   }/*pass1*/

                   if( PassNo == 2 ){

                     if( !isSubCall ){
                       Ydebug( "... end of skipping subcirc definition body." );
                     } else if( circCall == circDefCurrent ){ /* && isSubCall */
                       Ydebug( "... end of call subcircuit." );
                       RemCallPath();
                       if( Flags[flag2].value ){
                         MsgDebug( "\t\t... return to subcall path: " );
                         OutCallPath( fileDebug );
                         MsgDebug( "\n" );
                       }
                     } else { /* isSubCall but circCall != circDefCurrent */
                       Ydebug( "... end of skipping subcirc def body in subcall." );
                     }
                     if( isSubDefBody && isSubCall && (circCall == circDefCurrent) )
                     {
                       ReturnPrevFile( FALSE ); /* return from subcall, not from .include! */
                       SHOWCIRCSTATE( "Pass2: before restore states at the end of subcall" );
                       PopCircStack();
                       SHOWCIRCSTATE( "Pass2: after restore states at the end of subcall" );

                       /* for subcall processing: */
                       /*if( isSubCall )*/
                       levelSubCall--;
                       if( levelSubCall == 0 ) isSubCall = FALSE;

                     }
                     SHOWCIRCSTATE( "Pass2: before restore subcirc states on '.ends'" );
                     PopCircStack();
                     SHOWCIRCSTATE( "Pass2: after restore subcirc states on '.end'" );

                   }/*pass2*/

                   levelSubDef--;
                   if( levelSubDef == 0 ) isSubDefBody = FALSE;

                   SHOWCIRCSTATE( "Pass1,2: total result after process '.ends'" );
                 }
   ENDSubCircuit /*.ENDS*/
                { Ydebug( "... end of subcircuit '%s' detected.", $2 ); }
 ;
SubCircNodeList : SubCircNode | SubCircNodeList SubCircNode ;
SubCircNode : Node
                { Ydebug( "... subcircuit node '%s'", $1 );
                   if( PassNo == 1 ){
                     if( strEQ( $1, "0" ) )
                         MsgCompile( msgSUBCIRCPARM0 );
                     /* save local node-parameter for 'circDefCurrent': */
                     ADD_NODE_SUBCIRCPARM( $1, nodtypSubCirc,
                                           circDefCurrent->circname );
                   }/*pass2*/
                 }
 ;
SubCircStatementList
 : error        { MsgCompile( msgERR_SUBCKTSTMT );
                    YERRSKIP;
                 }
 | StatementList /* all statements but no control line in subcircuit ! */
 ;
ENDSubCircuit
 : _ENDS _ENDOFSTMT_
                { if( PassNo == 1 ) cnt_ENDS++; /* counter of circuit brackets */ }
 | _ENDS NodeName/*Identifier*/ _ENDOFSTMT_
                { if( PassNo == 1 ) cnt_ENDS++; /* counter of circuit brackets */ }
 ;
/*-------------------------------------------------------------------------*/
CallSubCircuit /*Xxxx*/
 : SUBCALL error
                { MsgCompile( msgERRSUBCALL, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 | SUBCALL      { namePrepCall = $1;
                   if( PassNo == 2 ){
                       /* free subcall parameters list: */
                       DelParmSubCall( parmSubCall );
                       parmSubCall = NULL;
                       cntParmSubCall = 0;
                   }
                 }
   SubCallNodeList
   NodeName/*Identifier*/ /* subcirc name may begin with digit */
                { Ydebug( "Call '%s' of subcircuit '%s'", $1, $4 );

                   if( PassNo == 1 ){
                     listX = AddElem( listX, &cntX, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/

                   if( PassNo == 2 ){
                     int  i;
                     /*static char fullname[maxINPLINELEN];*/
                     char *name;

                     SHOWCIRCSTATE( "test if generate code for subcall:" );/**/
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                          isSubDefBody && isSubCall && (circCall == circDefCurrent) )
                     {
                       /* find called circuit definition: */
                 /*    circPrepCall = FindSubCirc( listSubCirc,
                         circDefCurrent/*parent*\, $4 ); /* in current */
                 /**/  circPrepCall = FindSubCircHigh( listSubCirc,
                         circDefCurrent/*parent*/, $4 ); /* in current and in parents */
                       if( circPrepCall == NULL ) /* subcirc definition not found */
                         MsgCompile( msgSUBCKTNOTEXIST, $4 );
                       else{
                         /* called subcirc found: */

                         Ydebug( "Pass2:  process call '%s' of subcircuit '%s'...",
                                                        $1,                $4 );
                         if( Flags[flagG].value ){ /* debug info: */
                           /* from wich surcuit process call: */
                           MsgDebug( "\t\t\tcall from " );
                           OutFullCircName( fileDebug, circDefCurrent );
                           MsgDebug( "\n" );
                           /* debug print fact parameters: */
                           MsgDebug( "\t\t(%ld) nodes-parameters in call '%s' of '%s': ",
                                      cntParmSubCall, $1, $4 );
                           PrintSubParm( parmSubCall );
                         }
                         /* compare number of parameters with subcirc definition: */
                         if( cntParmSubCall != circPrepCall->cntnodesparm){
                           MsgCompile( msgCNTPARMSUBCAL,
                               circPrepCall->cntnodesparm, $1, $4, cntParmSubCall );
                         } else
                           /* rename local nodes: */
                         if( RenameLocalNodes( circPrepCall, circDefCurrent,
                                               parmSubCall ) == FALSE )
                             MsgCompile( msgPARMSETSUBCAL, $1 );
                         else { /* generate call: */

                           /* save state: */
/*                         SHOWCIRCSTATE( "Pass2: subcall processing: before saving states" );/**/
                           PushCircStack();
                           /* new state: process of call subcircuit */
                           isSubCall = TRUE;
                           levelSubCall++;
                           circCall = circPrepCall;
                           SHOWCIRCSTATE( "Pass2: subcall processing: after saving states" );/**/

                           AddCallPath( $1 ); /* add name to subcall path */

                           /* debug print local nodes-parameters list: */
                           if( Flags[flag2].value ){
                             MsgDebug( msgSHOWNODESPARM, circCall->cntnodesparm );
                             PrintNodesList( circCall->nodeparmlist );
                           }
                           /* debug print renamed nodes: */
                           if( Flags[flag2].value ){
                             MsgDebug( msgRENNODESLOCAL, circCall->cntnodes );
                             PrintNodesList( circCall->circnodlist );
                           }
                           /* print to nodes translation table: */
                           /*PrintNodesTransl( circCall, circCall->nodeparmlist );/**/
                               /* - nodes-parameters only */
                           PrintNodesTransl( circCall, circCall->circnodlist );
                               /* - all nodes */

                           /* rename local elements: */
                           RenameAllElems( circCall );
                           if( Flags[flag2].value )
                               DebugPrintElemLists( circCall );

                           /* switch to circuit body as to new .include-file: */
                           if( Flags[flagG].value || Flags[flag1].value || Flags[flag2].value )
                           { MsgDebug( "Pass2: begin include subcirc file '%s'"
                                       " from line #%ld to #%ld...\n",
                                       circCall->begfname,
                                       circCall->begline, circCall->endline );
                           }
                           IncludeFile( circCall->begfname, circCall->begline );
                         }/*generate code*/

                       }/*called subcirc found*/
                     }
                   }/*pass2*/

                   circPrepCall = NULL;   /* prepare circuit to call */
                   namePrepCall = NULL;   /* name of prepared subcall */
                 }
   _ENDOFSTMT_
 ;
SubCallNodeList : SubCallNode | SubCallNodeList SubCallNode ;
SubCallNode : Node
                { Ydebug( "... subcircuit node '%s'", $1 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( $1, nodtypUNKNOWN, namePrepCall );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $1, nodtypUNKNOWN, namePrepCall );
                     }
                   }/*pass1*/

                   if( PassNo == 2 ){
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       /* append node-parameter to parameters list: */
                       parmSubCall = AddSubParm( parmSubCall, $1, &cntParmSubCall );
                     }
                   }/*pass2*/
                 }
 ;
/*=========================================================================*/
ElemINDUCTOR /*Lxxx,Kxxx*/
 :
   INDUCTOR error
                { MsgCompile( msgERRINDUCTOR, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   INDUCTCOUPL error
                { MsgCompile( msgERRCOUPINDUC, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   INDUCTOR Node Node Value maybeINDUoptIC
                { Ydebug( "(%s,%s) Inductor '%s' = %g Henries",
                             $2,$3,           $1,   $4 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $3, nodtypUNKNOWN, $1 );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypUNKNOWN, $1 );
                     }
                     listL = AddElem( listL, &cntL, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/

                   if( PassNo == 2 ){
                     SHOWCIRCSTATE( "test if generate code for Inductor:" );/**/
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) )
                     { /* generate code: */
                       Ydebug( "Generate code for Inductor '%s'...", $1 );
                       PrintElemsTransl( $1, circDefCurrent, listL );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s",
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $2 ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $3 )
                              );
                       HabELEMout(
                         /*NE=  */GetHabElemName( $1, circDefCurrent, listL ),
                         /*KNOT=*/linebuffer,
                         /*PAR= */ftoa($4),
                         /*IDOP=*/"",
                         /*ISTR=*/""
                                 );
                     }
                   }/*pass2*/
                 }
   _ENDOFSTMT_
 |
   INDUCTCOUPL
   Identifier/*L...*/
                { if( toupper( $2[0] ) != 'L' ){
                     MsgCompile( msgNEEDINDUCTOR, $2 );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
   Identifier/*L...*/
                { if( toupper( $4[0] ) != 'L' ){
                     MsgCompile( msgNEEDINDUCTOR, $4 );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
   Value
                { Ydebug( "find Coupled Inductor '%s' (%s,%s) = %g Henries",
                                                   $1,  $2,$4,   $6 );
                 }
   _ENDOFSTMT_
 ;
maybeINDUoptIC
 : /*nothing*/
 | Identifier/*IC*/
                { if( !CheckKeyword( $1, "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, $1, "IC", "" );
                 }
   '=' Value    { Ydebug( "... inductor option IC = %g", $4 );
                 }
 ;
/*-------------------------------------------------------------------------*/
ElemRESISTOR /*Rxxx*/
 :
   RESISTOR error
                { MsgCompile( msgERRRESISTOR, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   RESISTOR Node Node Value
                { Ydebug( "(%s,%s) Resistor '%s' = %g Ohms",
                             $2,$3,           $1,   $4 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $3, nodtypUNKNOWN, $1 );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypUNKNOWN, $1 );
                     }
                     listR = AddElem( listR, &cntR, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                   if( PassNo == 2 ){
                     SHOWCIRCSTATE( "test if generate code for Resistor:" );/**/
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) )
                     { /* generate code: */
                       Ydebug( "Generate code for Resistor '%s'...", $1 );
                       PrintElemsTransl( $1, circDefCurrent, listR );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s",
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $2 ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $3 )
                              );
                       HabELEMout(
                         /*NE=  */GetHabElemName( $1, circDefCurrent, listR ),
                         /*KNOT=*/linebuffer,
                         /*PAR= */ftoa($4),
                         /*IDOP=*/"",
                         /*ISTR=*/""
                                 );
                     }
                   }/*pass2*/
                 }
   _ENDOFSTMT_
 |
   RESISTOR Node Node Identifier
                { Ydebug( "(%s,%s) Resistor '%s' (model '%s')",
                             $2,$3,           $1,         $4 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $3, nodtypUNKNOWN, $1 );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypUNKNOWN, $1 );
                     }
                     listR = AddElem( listR, &cntR, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/

                   if( PassNo == 2 ){
		     MsgCompileWarn( warn1_INFO, msgNotProcSemiR, $1 );
                   }/*pass2*/
                 }
   maybeRESIoptions
                {
                   /* in Habala no semiResistor ! */
                 }
   _ENDOFSTMT_
 ;
maybeRESIoptions : /*nothing*/ | RESIoptions ;
RESIoptions : RESIoption | RESIoptions RESIoption ;
RESIoption
 : Identifier/*L_LEN/W_WIDTH/TEMP*/
                { if( !CheckKeyword( $1, "L", "W", "TEMP", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, $1, "L", "W", "TEMP", "" );
                 }
   '=' Value    { Ydebug( "... resistor option '%s' = %g", $1, $4 );
                 }
 ;
/*-------------------------------------------------------------------------*/
ElemCAPACITOR /*Cxxx*/
 :
   CAPACITOR error
                { MsgCompile( msgERRCAPACITOR, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   CAPACITOR Node/*+*/ Node/*-*/ Value
                { Ydebug( "(%s,%s) Capacitor '%s' = %g Farads",
                             $2,$3,            $1,   $4 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1 );
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1 );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                     }
                     listC = AddElem( listC, &cntC, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   maybeCAPAoptIC
                {
                   if( PassNo == 2 ){
                     SHOWCIRCSTATE( "test if generate code for Capacitor:" );/**/
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) )
                     { /* generate code: */
                       Ydebug( "Generate code for Capacitor '%s'...", $1 );
                       PrintElemsTransl( $1, circDefCurrent, listC );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s",
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $2 ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $3 )
                              );
                       HabELEMout(
                         /*NE=  */GetHabElemName( $1, circDefCurrent, listC ),
                         /*KNOT=*/linebuffer,
                         /*PAR= */ftoa($4),
                         /*IDOP=*/"",
                         /*ISTR=*/""
                                 );
                     }
                   }/*pass2*/
                 }
   _ENDOFSTMT_
 |
   CAPACITOR Node/*+*/ Node/*-*/ Identifier
                { Ydebug( "(%s,%s) Capacitor '%s' (model '%s')",
                             $2,$3,            $1,         $4 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1 );
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1 );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                     }
                     listC = AddElem( listC, &cntC, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/

                   if( PassNo == 2 ){
		     MsgCompileWarn( warn1_INFO, msgNotProcSemiC, $1 );
                   }/*pass2*/
                 }
   maybeCAPAoptions
                {
                   /* in Habala no semiCapacitor ! */
                 }
   _ENDOFSTMT_
 ;
maybeCAPAoptions : /*nothing*/ | CAPAoptions ;
CAPAoptions : CAPAoption | CAPAoptions CAPAoption ;
CAPAoption
 : Identifier/*L_LEN/W_WIDTH/IC*/
                { if( !CheckKeyword( $1, "L", "W", "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, $1, "L", "W", "IC", "" );
                 }
   '=' Value    { Ydebug( "... capacitor option '%s' = %g", $1, $4 );
                 }
 ;
maybeCAPAoptIC
 : /*nothing*/
 | Identifier/*IC*/
                { if( !CheckKeyword( $1, "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, $1, "IC", "" );
                 }
   '=' Value    { Ydebug( "... capacitor option IC = %g", $4 );
                 }
 ;
/*-------------------------------------------------------------------------*/
ElemVOLTAGE /*Vxxx*/
 :
   VOLTAGE error
                { MsgCompile( msgERRVOLTAGE, $1 );
                   YERRSKIP;
                   isVxxx = isIVxxxOpt = FALSE;
                   IxxxName = VxxxName = NULL;
                 }
   _ENDOFSTMT_
 |
   VOLTAGE Node/*+*/ Node/*-*/
                { Ydebug( "(%s,%s) Voltage '%s'",
                             $2,$3,          $1 );
                   VxxxName = $1;
                   isVxxx = TRUE;
                   /* make virtual resistor Ri name: */
                   RiVxxxName = (char *)malloc( ( strlen($1) + 2 ) * sizeof(char) );
                   if( RiVxxxName == NULL )
                     MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
                   else {
                     RiVxxxName[0] = 'R';
                     strcpy( &RiVxxxName[1], $1 );
                     RiVxxxName[strlen(RiVxxxName)] = '\0';
                   }
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1 );
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                     }
                     listV = AddElem( listV, &cntV, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                     /* add virtual Ri for this Vxxx with name "Vxxx": */
                     listR = AddElem( listR, &cntR, RiVxxxName, circDefCurrent ); /* add to list */
                   }/*pass1*/
                   if( PassNo == 2 ){
                     /* get value of virtual resistor for current 'Vxxx' */
                     int    isint;
                     long   longparm;
                     GetConfOption( "Ri", &isint, &VxxxRi/*dblparm*/, &longparm );

                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) )
                     { /* generate code for virtual resistor: */
                       Ydebug( "Generate code for Ri for '%s'...", $1 );
                       /* _no_ rename virtual Ri: */
                       /* renR = Rename1elem( RiVxxxName, listR, circCall, renR ); */
                       PrintElemsTransl( RiVxxxName, circDefCurrent, listR );
                                    /* out to elements names tranlations file */
                       /* save habala-nodes list to 'linebuffer' */
                       sprintf( linebuffer, "%s, %s",
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $2 ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $3 )
                              );
                         /* this 'linebuffer' with nodes-list will be use later */

                       HabELEMout(
                         /*NE=  */GetHabElemName( RiVxxxName, circDefCurrent, listR ),
                         /*KNOT=*/linebuffer,
                         /*PAR= */ftoa(VxxxRi),
                         /*IDOP=*/"",
                         /*ISTR=*/""
                                 );
                     }
                   }/*pass2*/
                 }
   maybeOptDCfree  /* if DC-option without keyword 'DC' - must be first ! */
   maybeVoltCurrOptions  /* old version with strong order of parameters:
                          *   maybeOptDC maybeOptAC maybeOptDistof1
                          *   maybeOptDistof2 maybeIndepSrcFuns
                          */
   _ENDOFSTMT_  { isVxxx = isIVxxxOpt = FALSE;
                   IxxxName = VxxxName = NULL;
                 }
 ;

ElemCURRENT /*Ixxx*/
 :
   CURRENT error
                { MsgCompile( msgERRCURRENT, $1 );
                   YERRSKIP;
                   isIxxx = isIVxxxOpt = FALSE;
                   IxxxName = VxxxName = NULL;
                 }
   _ENDOFSTMT_
 |
   CURRENT Node/*+*/ Node/*-*/
                { Ydebug( "(%s,%s) Current '%s'",
                             $2,$3,          $1 );
                   IxxxName = $1;
                   isIxxx = TRUE;
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1 );
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                     }
                     listI = AddElem( listI, &cntI, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                   if( PassNo == 2 ){
                     /* put to 'linebuffer' list of nodes: */
                     sprintf( linebuffer, "%s, %s",
                       GetHabNodeName( isSubCall?circCall->circnodlist
                                                :listNodesMain, $2 ),
                       GetHabNodeName( isSubCall?circCall->circnodlist
                                                :listNodesMain, $3 )
                            );
                   }/*pass2*/
                 }
   maybeOptDCfree  /* if DC-option without keyword 'DC' - must be first ! */
   maybeVoltCurrOptions /* old version with strong order of parameters:
                         *   maybeOptDC maybeOptAC maybeOptDistof1
                         *   maybeOptDistof2 maybeIndepSrcFuns
                         */
   _ENDOFSTMT_  { isIxxx = isIVxxxOpt = FALSE;
                   IxxxName = VxxxName = NULL;
                 }
 ;

maybeOptDCfree : /*nothing*/ | OptDCfree ;
OptDCfree /* DC-option without keyword 'DC' */
 : Value        { Ydebug( "... voltage/current (DC) option (%g)", $1 );
                   if( PassNo == 2 ){
                     /* get habala parameters: */
                     if( isIxxx )
                       IVxxxParmFromDC( &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                                        $1
                       );
                     else if( isVxxx )
                       IVxxxParmFromDC( &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                                        $1/VxxxRi
                       );
                     /* generate code for option: */
                     MAKEELEM_IVxxx; /* generate code */
                   }/*pass2*/
                   isIVxxxOpt = FALSE;
                 }
 ;
maybeVoltCurrOptions : /*nothing*/ | VoltCurrOptions ;
VoltCurrOptions
 : VoltCurrOption
                { /* generate code for option: */
                   if( PassNo == 2 && isIVxxxOpt ){ /* valid option */
                     MAKEELEM_IVxxx; /* generate code */
                   }/*pass2*/
                   isIVxxxOpt = FALSE;
                 }
 | VoltCurrOptions
   VoltCurrOption
                { /* generate code for option: */
                   if( PassNo == 2 && isIVxxxOpt ){ /* valid option */
                     MAKEELEM_IVxxx; /* generate code */
                   }/*pass2*/
                   isIVxxxOpt = FALSE;
                 }
 ;
startVoltCurrOption
 : Identifier/*DC/AC/DISTOF1/DISTOF2/PULSE/SIN/EXP/SFFM/PWL*/
                { if( CheckKeyword( $1,
                          "DC", "AC", "DISTOF1", "DISTOF2",
                          "PULSE", "SIN", "EXP", "SFFM", "PWL", "" )
                   ){
                     $$ = $1;
                   }else{
                     MsgAllowKeywords( msgALLOWKEYWORD, $1,
                          "DC", "AC", "DISTOF1", "DISTOF2",
                          "PULSE", "SIN", "EXP", "SFFM", "PWL", "" );
                   }
                   isIVxxxOpt = FALSE;
                 }
 ;
VoltCurrOption
 : /* option(s) with 0 parameters */
   startVoltCurrOption/*AC/DISTOF1/DISTOF2*/ /*0.0*/ /*0.0*/
                {
                   if( !CheckKeyword( $1, "AC", "DISTOF1", "DISTOF2", "" ) )
                     MsgCompile( msgILLOPTION, $1 );
                   else {
                     Ydebug( "... voltage/current '%s' option (%g,%g)", $1, 0.0, 0.0 );
                     if( PassNo == 2 ){
                       int DISTOFno;
                       if( CheckKeyword( $1, "DISTOF1", "" ) )      DISTOFno = 1;
                       else if( CheckKeyword( $1, "DISTOF2", "" ) ) DISTOFno = 2;
                       else                                         DISTOFno = 0;
                       if( DISTOFno ){ /* get habala parameters: */
                         if( isIxxx )
                           IVxxxParmFromDISTOF(
                              &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                              DISTOFno, 0.0, 0.0
                           );
                         else if( isVxxx )
                           IVxxxParmFromDISTOF(
                              &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                              DISTOFno, 0.0/VxxxRi, 0.0 - 90.0 /*ASB000623*/
                           );
                         isIVxxxOpt = TRUE; /* processed option */
                       }
                     }/*pass2*/
                   }
                 }
 | /* option(s) with 1 parameter */
   startVoltCurrOption/*DC/AC/DISTOF1/DISTOF2*/ Value /*[0.0]*/
                {
                   if( CheckKeyword( $1, "AC", "DISTOF1", "DISTOF2", "" ) ){
                     Ydebug( "... voltage/current '%s' option (%g,%g)", $1, $2, 0.0 );
                     if( PassNo == 2 ){
                       int DISTOFno;
                       if( CheckKeyword( $1, "DISTOF1", "" ) )      DISTOFno = 1;
                       else if( CheckKeyword( $1, "DISTOF2", "" ) ) DISTOFno = 2;
                       else                                         DISTOFno = 0;
                       if( DISTOFno ){ /* get habala parameters: */
                         if( isIxxx )
                           IVxxxParmFromDISTOF(
                              &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                              DISTOFno, $2, 0.0
                           );
                         else if( isVxxx )
                           IVxxxParmFromDISTOF(
                              &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                              DISTOFno, $2/VxxxRi, 0.0 - 90.0 /*ASB000623*/
                           );
                         isIVxxxOpt = TRUE; /* processed option */
                       }
                     }/*pass2*/
                   }else if( CheckKeyword( $1, "DC", "" ) ){
                     Ydebug( "... voltage/current '%s' option (%g)", $1, $2 );
                     if( PassNo == 2 ){
                       /* get habala parameters: */
                       if( isIxxx )
                         IVxxxParmFromDC( &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                                          $2
                         );
                       else if( isVxxx )
                         IVxxxParmFromDC( &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                                          $2/VxxxRi
                         );
                       isIVxxxOpt = TRUE; /* processed option */
                     }
                   }else
                     MsgCompile( msgILLOPTION, $1 );
                 }
  
 | /* option(s) with 2 parameters */
   startVoltCurrOption/*AC/DISTOF1/DISTOF2*/ Value Value
                {
                   if( !CheckKeyword( $1, "AC", "DISTOF1", "DISTOF2", "" ) )
                     MsgCompile( msgILLOPTION, $1 );
                   else{
                     Ydebug( "... voltage/current '%s' option (%g,%g)", $1, $2, $3 );
                     if( PassNo == 2 ){
                       int DISTOFno;
                       if( CheckKeyword( $1, "DISTOF1", "" ) )      DISTOFno = 1;
                       else if( CheckKeyword( $1, "DISTOF2", "" ) ) DISTOFno = 2;
                       else                                         DISTOFno = 0;
                       if( DISTOFno ){ /* get habala parameters: */
                         if( isIxxx )
                           IVxxxParmFromDISTOF(
                              &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                              DISTOFno, $2, $3
                           );
                         else if( isVxxx )
                           IVxxxParmFromDISTOF(
                              &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                              DISTOFno, $2/VxxxRi, $3 - 90.0 /*ASB000623*/
                           );
                         isIVxxxOpt = TRUE; /* processed option */
                       }
                     }/*pass2*/
                   }
                 }
 |
   IndepSrcFun
 ;
IndepSrcFun
 : startVoltCurrOption/*PULSE/SIN/SFFM/EXP/PWL*/ '(' Value Value /*...*/
                {
                   if( !CheckKeyword( $1, "PULSE", "SIN", "SFFM", "EXP", "PWL", "" ) ){
                       MsgAllowKeywords( msgEXPECTFUNNAME, strupr($1),
                           "PULSE()", "SIN()", "SFFM()", "EXP()", "PWL()", "" );
                       SKIPTOEOLN; /*SkipRestLine();/*?*/
                   } else
                       Ydebug( "... voltage/current function %s(%g,%g,...)",
                                                             $1,$3,$4 );
                 }
   maybeISFunParmList
   ')'
                { Ydebug( "... end of voltage/current function %s()", $1 );
                 }
 ;
maybeISFunParmList : /*nothing*/ | ISFunParmList ;
ISFunParmList : ISFunParm | ISFunParmList ISFunParm ;
ISFunParm
 : Value        { Ydebug( "    ... next function parameter %g", $1 ); }
 ;
/*-------------------------------------------------------------------------*/
ElemLVCCS /*Exxx*/
 :
   LVCVS error  { MsgCompile( msgERRLVCVS, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   LVCVS Node/*+*/ Node/*-*/ Node/*+*/ Node/*-*/ Value
                { Ydebug( "(%s,%s,%s,%s) Linear Voltage-Controlled Voltage"
                                        " Source '%s', voltage gain = %g",
                             $2,$3,$4,$5,         $1,                 $6 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1 );
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1 );
                       ADD_NODE_MAIN( $4, nodtypCTRLPLUS, $1 );
                       ADD_NODE_MAIN( $5, nodtypCTRLMINUS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypCTRLPLUS, $1 );
                       ADD_NODE_SUBCIRC( $5, nodtypCTRLMINUS, $1 );
                     }
                     listE = AddElem( listE, &cntE, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   _ENDOFSTMT_
 ;
ElemLVCVS /*Gxxx*/
 :
   LVCCS error  { MsgCompile( msgERRLVCCS, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   LVCCS Node/*+*/ Node/*-*/ Node/*+*/ Node/*-*/ Value
                { Ydebug( "(%s,%s,%s,%s) Linear Voltage-Controlled Current Source"
                                        " '%s', transconductance %g mhos",
                             $2,$3,$4,$5,  $1,                   $6 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1);
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1);
                       ADD_NODE_MAIN( $4, nodtypCTRLPLUS, $1);
                       ADD_NODE_MAIN( $5, nodtypCTRLMINUS, $1);
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypCTRLPLUS, $1 );
                       ADD_NODE_SUBCIRC( $5, nodtypCTRLMINUS, $1 );
                     }
                     listG = AddElem( listG, &cntG, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   _ENDOFSTMT_
 ;
ElemLCCCS /*Fxxx*/
 :
   LCCCS error  { MsgCompile( msgERRLCCCS, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   LCCCS Node/*+*/ Node/*-*/ Identifier/*V...*/
                { if( toupper( $4[0] ) != 'V' ){
                     MsgCompile( msgNEEDVOLTAGE, $4 );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
   Value
                { Ydebug( "(%s,%s) Linear Current-Controlled Current Source"
                                 " '%s', voltage source: '%s', current gain = %g",
                             $2,$3, $1,                   $4,                 $6 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1);
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1);
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                     }
                     listF = AddElem( listF, &cntF, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   _ENDOFSTMT_
 ;
ElemLCCVS /*Hxxx*/
 :
   LCCVS error  { MsgCompile( msgERRLCCVS, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   LCCVS Node/*+*/ Node/*-*/ Identifier/*V...*/
                { if( toupper( $4[0] ) != 'V' ){
                     MsgCompile( msgNEEDVOLTAGE, $4 );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
   Value
                { Ydebug( "(%s,%s) Linear Current-Controlled Voltage Source '%s',"
                              " voltage source: '%s', transresistance %g ohms",
                             $2,$3,                                           $1,
                                                 $4,                  $6 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1 );
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                     }
                     listH = AddElem( listH, &cntH, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   _ENDOFSTMT_
 ;
/*-------------------------------------------------------------------------*/
ElemNLDS /*Bxxx*/
 :
   NLDS error   { MsgCompile( msgERRNLDS, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   NLDS Node/*+*/ Node/*-*/
                { Ydebug( "(%s,%s) Non-linear dependent source '%s'",
                             $2,$3,                              $1 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1 );
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                     }
                     listB = AddElem( listB, &cntB, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   maybeNLDSoptions
/***  old version with strong order of parameters:
   maybeNLDSoptI maybeNLDSoptV
***/
   _ENDOFSTMT_
 ;
maybeNLDSoptions : /*nothing*/ | NLDSoptions ;
NLDSoptions : NLDSoption | NLDSoptions NLDSoption ;
NLDSoption
 : Identifier/*I_SRC/V_SRC*/ '=' ArExpr 
                { if( !CheckKeyword( $1, "I", "V", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, $1, "I", "V", "" );
                   else
                     Ydebug( "... NLDS %s-source: %s=...", $1, $1 );
                 }
/*?*:
 | Identifier '=' error
                { MsgCompile( msgUNCLOSEDAREXPR );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
/**/
 ;
/*-------------------------------------------------------------------------*/
ArExpr /* Arithmetic Expression - not compute in compile time */
 : error        { MsgCompile( msgERRAREXPR );
                   /*yyerrok; yyclearin;/*?*/
                   /*YERRSKIP;/*?*/
                 }
/*?*:
 | error _ENDOFSTMT_
                { MsgCompile( msgUNCLOSEDAREXPR );
                   yyerrok; yyclearin;
                 }
/**/
 | UnsignValue  { Ydebug( "reduced:  ArExpr : Value ;" );
                   /* $$ = $1; */
                 }
 | Identifier   { Ydebug( "reduced:  ArExpr : Identifier ;" );
		 }

 | '-' ArExpr %prec UMINUS
                { Ydebug( "reduced:  ArExpr : '-' ArExpr ;" );
                   /* $$ = -$2; */
                 }
 | ArExpr '+' ArExpr
                { Ydebug( "reduced:  ArExpr : ArExpr '+' ArExpr ;" );
                   /* $$ = $1 + $3; */
                 }
 | ArExpr '-' ArExpr
                { Ydebug( "reduced:  ArExpr : ArExpr '-' ArExpr ;" );
                   /* $$ = $1 - $3; */
                 }
 | ArExpr '*' ArExpr
                { Ydebug( "reduced:  ArExpr : ArExpr '*' ArExpr ;" );
                   /* $$ = $1 * $3; */
                 }
 | ArExpr '/' ArExpr
                { Ydebug( "reduced:  ArExpr : ArExpr '/' ArExpr ;" );
                   /* $$ = $1 / $3; */
                 }
 | ArExpr '^' ArExpr
                { Ydebug( "reduced:  ArExpr : ArExpr '^' ArExpr ;" );
                   /* $$ = pow( $1, $3 ); */
                 }
 | '(' ArExpr BRKEND/*')'*/ %prec BRACK
                { Ydebug( "ArExpr : '(' ArExpr ')' ;" );
                   /* $$ = $2; */
                 }

 | Identifier '(' ArExpr ')' /* function or OutVar */
                { Ydebug( "reduced:  ArExpr : Identifier '(' ArExpr ')' ;" );
                   if( !CheckKeyword( $1,
                           "i", "v",
                           "abs",   "asinh", "cosh", "sin",
                           "acos",  "atan",  "exp",  "sinh",
                           "acosh", "atanh", "ln",   "sqrt",
                           "asin",  "cos",   "log",  "tan",
                           "poly5", "junc",
                           "" ) )
                     MsgAllowKeywords( msgALLOW_V_C_FUN, $1,
                                       "i()",     "v()",
/* disable wrap message:     "\n\t"/**/"abs()",   "asinh()", "cosh()",
                                       "sin()",   "acos()",  "atan()",
                                       "exp()",   "sinh()",  "acosh()",
/* disable wrap message:     "\n\t"/**/"atanh()", "ln()",    "sqrt()",
                                       "asin()",  "cos()",   "log()",
                                       "tan()",   "poly5()", "junc()",
                                       "" );
                 }
 | Identifier '(' NodeName ',' NodeName ')' /* OutVar */
                { Ydebug( "reduced:  ArExpr : Identifier '(' Node ',' Node ')' ;" );
                   if( !CheckKeyword( $1, "I", "V", "" ) )
                     MsgAllowKeywords( msgALLOWVOLTCURR2, $1,
                           "i(_,_)", "v(_,_)", "" );
                 }
/* | OutVar /* +3 reduce/reduce with 'function()' */
 ;
BRKEND/*')'*/ : ')'
 | error        { MsgCompile( msgRIGHTBRKREQ );
                   YERRSKIP;
                 }
/*?*:
 | error _ENDOFSTMT_
                { MsgCompile( msgUNCLOSEDAREXPR );
                   YERRSKIP;
                 }
/**/
 ;
/*-------------------------------------------------------------------------*/
StmtSwitch /*Sxxx,Wxxx*/
 :
   VSWITCH error
                { MsgCompile( msgERRVSWITCH, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   ISWITCH error
                { MsgCompile( msgERRISWITCH, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   VSWITCH/*Sxxx*/ Node/*+*/ Node/*-*/ Node/*+*/ Node/*-*/ Identifier Switch
                { Ydebug( "(%s,%s,%s,%s) V-switch '%s' (model '%s'),"
                                                              " status = %s",
                             $2,$3,$4,$5,           $1,         $6,
                                                             ShowKeyword($7) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1 );
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1 );
                       ADD_NODE_MAIN( $4, nodtypCTRLPLUS, $1 );
                       ADD_NODE_MAIN( $5, nodtypCTRLMINUS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypCTRLPLUS, $1 );
                       ADD_NODE_SUBCIRC( $5, nodtypCTRLMINUS, $1 );
                     }
                     listS = AddElem( listS, &cntS, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   _ENDOFSTMT_
 |
   ISWITCH/*Wxxx*/ Node/*+*/ Node/*-*/ Identifier/*V...*/
                { if( toupper( $4[0] ) != 'V' ){
                     MsgCompile( msgNEEDVOLTAGE, $4 );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
   Identifier Switch
                {
                     Ydebug( "(%s,%s) I-switch '%s' (voltage '%s', model '%s'),"
                                                                 " status = %s",
                               $2,$3,           $1,           $4,         $6,
                                                                ShowKeyword($7) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1 );
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                     }
                     listW = AddElem( listW, &cntW, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   _ENDOFSTMT_
 ;
Switch
 : /*nothing*/  { $$ = tokenUNKNOWN; /*unknown*/ /* $$ = 0; - <?> */ }
 | Identifier/*ON/OFF*/
                {      if( CheckKeyword( $1, "ON", "" ) )  $$ = ON;
                   else if( CheckKeyword( $1, "OFF", "" ) ) $$ = OFF;
                   else MsgAllowKeywords( msgALLOWKEYWORD, $1, "ON", "OFF", "" );
                 }
 ;
/*-------------------------------------------------------------------------*/
StmtTransmissionLine
 :
   LLTLINE/*Txxx*/  error
                { MsgCompile( msgERRLLTLINE, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   LTLINE/*Oxxx*/   error
                { MsgCompile( msgERRLTLINE, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   UDRCLINE/*Uxxx*/ error
                { MsgCompile( msgERRUDRCLINE, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   UDRCLINE/*Uxxx*/ Node Node Node      Identifier L_LEN '=' Value
   maybeUDRCLoptN
                { Ydebug( "(%s,%s,%s) Uni Distrib RC Line '%s'"
                                                     " (model '%s', length = %g)",
                             $2,$3,$4,                      $1,$5,           $8 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $3, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $4, nodtypUNKNOWN, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypUNKNOWN, $1 );
                     }
                     listU = AddElem( listU, &cntU, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   _ENDOFSTMT_
 |
   LTLINE/*Oxxx*/   Node Node Node Node Identifier
                { Ydebug( "(%s,%s,%s,%s) Lossy Transmission line '%s' (model '%s')",
                             $2,$3,$4,$5,                          $1,         $6 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $3, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $4, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $5, nodtypUNKNOWN, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $5, nodtypUNKNOWN, $1 );
                     }
                     listO = AddElem( listO, &cntO, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/

                   if( PassNo == 2 ){
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       Ydebug( "Try to generate code for LTLLINE '%s'...", $1 );
                       PrintElemsTransl( $1, circDefCurrent, listO );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s, %s, %s",
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, $2 ),
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, $4 ), /*not 3!*/
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, $3 ),
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $5 )
                              );
                       if( !MkElemOxxx( $1, linebuffer, $6 ) )
                         MsgCompileWarn( warn5_BREAK, msgNOTGENELEM, $1 );
                     }
                   }/*pass2*/
                 }
   _ENDOFSTMT_
 |
   LLTLINE/*Txxx*/  Node Node Node Node
   maybeLLTLoptions /* Z0 '=' Value maybeLLTLoptTD maybeLLTLoptF maybeLLTLoptIC */
                { Ydebug( "(%s,%s,%s,%s) Lossless Transmission line '%s'",
                             $2,$3,$4,$5,                             $1 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $3, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $4, nodtypUNKNOWN, $1 );
                       ADD_NODE_MAIN( $5, nodtypUNKNOWN, $1 );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypUNKNOWN, $1 );
                       ADD_NODE_SUBCIRC( $5, nodtypUNKNOWN, $1 );
                     }
                     listT = AddElem( listT, &cntT, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                   if( PassNo == 2 ){
                     if( existZ0 && existTD ){
                       parmLength = parmTD * const_c; /* TZ0530: '/' -> '*' */
                       isGenOK = TRUE;
                     }else if( existZ0 && existF_FREQ ){
                       if( !existNL ) parmNL = def_NRMLEN;
                       parmLength = (1.0 / parmF_FREQ ) * parmNL * const_c;
                                    /* TZ0530: '/ const_c' -> '*  const_c' */
                       isGenOK = TRUE;
                     }else{
                       MsgCompileWarn( warn3_CONFLICT, msgNOTGENILLPARM, $1 );
                       isGenOK = FALSE;
                     }
                     if( isGenOK )
                     { /* generate code: */
                       Ydebug( "Generate code for LLTLINE '%s'...", $1 );
                       PrintElemsTransl( $1, circDefCurrent, listT );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s, %s, %s",
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $2 ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $4 ), /*not 3!*/
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $3 ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, $5 )
                              );
                       sprintf( parmbuffer, "%g, %g", parmZ0, parmLength );
                       HabELEMout(
                         /*NE=  */GetHabElemName( $1, circDefCurrent, listT ),
                         /*KNOT=*/linebuffer,
                         /*PAR= */parmbuffer,
                         /*IDOP=*/"",
                         /*ISTR=*/""
                                 );
                     }
                   }/*pass2*/
                 }
   _ENDOFSTMT_
 ;
maybeLLTLoptions : /*nothing*/ | LLTLoptions ;
LLTLoptions : LLTLopt | LLTLoptions LLTLopt ;
LLTLopt
 : Identifier/*Z0/TD/F_FREQ/NL*/ '=' Value   
                {
                   if( CheckKeyword( $1, "Z0", "TD", "F_FREQ", "NL", "" ) )
                     Ydebug( "... lossy trans line option '%s' = %g", $1,  $3 );
                   if( PassNo == 2 ){
                     if( CheckKeyword( $1, "IC", "" ) ){
                       MsgCompile( msgTOOFEWPARM, $1 );
                     }else if( CheckKeyword( $1, "Z0", "" ) ){
                       if( $3 <= 0.0 ){
                         MsgCompile( msgERPARMILLVALUE, $3, $1 );
                         existZ0 = FALSE;
                       }else if( existZ0 ){
                         MsgCompile( msgPARMALREADYDEF, $1 );
                         existZ0 = FALSE;
                       }else{
                         existZ0 = TRUE;
                         parmZ0 = $3;
                       }
                     }else if( CheckKeyword( $1, "TD", "" ) ){
                       if( $3 <= 0.0 ){
                         MsgCompile( msgERPARMILLVALUE, $3, $1 );
                         existTD = FALSE;
                       }else if( existTD ){
                         MsgCompile( msgPARMALREADYDEF, $1 );
                         existTD = FALSE;
                       }else{
                         existTD = TRUE;
                         parmTD = $3;
                       }
                     }else if( CheckKeyword( $1, "F"/*F_FREQ*/, "" ) ){
                       if( $3 <= 0.0 ){
                         MsgCompile( msgERPARMILLVALUE, $3, $1 );
                         existF_FREQ = FALSE;
                       }else if( existF_FREQ ){
                         MsgCompile( msgPARMALREADYDEF, $1 );
                         existF_FREQ = FALSE;
                       }else{
                         existF_FREQ = TRUE;
                         parmF_FREQ = $3;
                       }
                     }else if( CheckKeyword( $1, "NL", "" ) ){
                       if( $3 <= 0.0 ){
                         MsgCompile( msgERPARMILLVALUE, $3, $1 );
                         existNL = FALSE;
                       } else if( existNL ){
                         MsgCompile( msgPARMALREADYDEF, $1 );
                         existNL = FALSE;
                       }else{
                         existNL = TRUE;
                         parmNL = $3;
                       }
                     }else{
                       MsgCompile( msgILLOPTION, $1 );
                       break;
                     }
                   }/*pass2*/
                 }
 | Identifier/*IC*/ '=' Value ',' Value ',' Value ',' Value
                {
                   if( !CheckKeyword( $1, "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "IC", "" );
                   else
                     Ydebug( "... lossy trans line option IC = %g,%g,%g,%g",
                                                               $3,$5,$7,$9 );
                 }
 ;
maybeUDRCLoptN
 : /*nothing*/
 | Identifier/*N_LUMPS*/ '=' Value
                {
                   if( !CheckKeyword( $1, "N", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "N", "" );
                   else
                     Ydebug( "... uni distrib RC line option N = %g lumps", $3 );
                 }
 ;
/*=========================================================================*/
SemiDiode /*Dxxx*/
 :
   DIODE error
                { MsgCompile( msgERRDIODE, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   DIODE Node/*+*/ Node/*-*/ Identifier
                { Ydebug( "(%s,%s) Diode '%s' (model '%s')",
                             $2,$3,        $1,         $4 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypPLUS, $1 );
                       ADD_NODE_MAIN( $3, nodtypMINUS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypPLUS, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypMINUS, $1 );
                     }
                     listD = AddElem( listD, &cntD, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                   if( PassNo == 2 ){ /* clear AREA befor process options: */
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       if( UpFindModel( $4 ) == NULL ){ /* find model */
                         MsgCompile( msgMODELNOTFOUND, $4 );
                       }
                     }
                   }/*pass2*/
                 }
   maybeDIODEoptions
/*** old version with strong order of parameters:
   maybeSEMIoptArea maybeSEMIoptOFF maybeDIODoptIC maybeSEMIoptTEMP
***/
                {
                   if( PassNo == 2 ){
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       Ydebug( "Try to generate code for Diode Schottky '%s'...", $1 );
                       PrintElemsTransl( $1, circDefCurrent, listD );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s",
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, $2 ),
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, $3 )
                              );
                       /* test for diode parameter AREA: */
                       if( existDiodArea && parmDiodArea <= 0.0 ){
                         MsgCompileWarn( warn3_CONFLICT, msgPARMILLVALUE, parmDiodArea, msgAREA );
                         parmDiodArea = def_DiodArea;
                       }
                       if( !existDiodArea ) parmDiodArea = def_DiodArea;
                       if( UpFindModel( $4 ) != NULL ){
                         if( !MkElemDxxx( $1, linebuffer, $4, parmDiodArea ) )
                           MsgCompileWarn( warn5_BREAK, msgNOTGENELEM, $1 );
                       }
                     }
                   }/*pass2*/
                 }
   _ENDOFSTMT_
 ;
maybeDIODEoptions : /*nothing*/ | DIODEoptions ;
DIODEoptions : DIODEoption | DIODEoptions DIODEoption ;
DIODEoption
 : Value        { Ydebug( "... diode option area = %g", $1 );
                   if( PassNo == 2 ){
                     if( existDiodArea ){
                       MsgCompile( msgDIODAREAAGAIN );
                     }else{
                       existDiodArea = TRUE;
                       parmDiodArea  = $1;
                     }
                   }/*pass2*/
                 }
 | Identifier/*OFF*/
                { if( !CheckKeyword( $1, "OFF", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "OFF", "" );
                   else
                     Ydebug( "... diode option OFF" );
                 }
 | Identifier/*IC/TEMP*/ '=' Value
                { if( !CheckKeyword( $1, "IC", "TEMP", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "IC", "TEMP", "" );
                   else
                     Ydebug( "... diode option '%s' = %g", $1, $3 );
                 }
 ;
/*-------------------------------------------------------------------------*/
SemiTransistor /*Qxxx*/
 :
   TRANSISTOR error
                { MsgCompile( msgERRTRANSISTOR, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   TRANSISTOR Node/*C*/ Node/*B*/ Node/*E*/          Identifier
                { Ydebug( "(%s,%s,%s) Bipolar Trasistor '%s', model '%s'",
                             $2,$3,$4,                    $1,         $5 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypBjtC, $1 );
                       ADD_NODE_MAIN( $3, nodtypBjtB, $1 );
                       ADD_NODE_MAIN( $4, nodtypBjtE, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypBjtC, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypBjtB, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypBjtE, $1 );
                     }
                     listQ = AddElem( listQ, &cntQ, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   maybeTRANSoptions
/*** old version with strong order of parameters:
   maybeSEMIoptArea maybeSEMIoptOFF maybeTRANoptIC maybeSEMIoptTEMP
***/
   _ENDOFSTMT_
 |
   TRANSISTOR Node/*C*/ Node/*B*/ Node/*E*/ bjtSnode Identifier
                { Ydebug( "(%s,%s,%s,%s) Bipolar Trasistor '%s', model '%s'",
                             $2,$3,$4,$5,                    $1,         $6 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypBjtC, $1 );
                       ADD_NODE_MAIN( $3, nodtypBjtB, $1 );
                       ADD_NODE_MAIN( $4, nodtypBjtE, $1 );
                       ADD_NODE_MAIN( $5, nodtypBjtS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypBjtC, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypBjtB, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypBjtE, $1 );
                       ADD_NODE_SUBCIRC( $5, nodtypBjtS, $1 );
                    }
                     listQ = AddElem( listQ, &cntQ, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   maybeTRANSoptions
/*** old version with strong order of parameters:
   maybeSEMIoptArea maybeSEMIoptOFF maybeTRANoptIC maybeSEMIoptTEMP
***/
   _ENDOFSTMT_
 ;
bjtSnode/*Node/*S*/ /* normal Node: may be decimal/float/ident */
 : IDENTINT     /* decimal number or identifier */
                        { $<strptr>$ = $<mixed>1->text; }
 | IDENTFLOAT   /* float number or identifier */
                        { $<strptr>$ = $<mixed>1->text; }
/*
 | IDENTNAME
                        { $<strptr>$ = $<strptr>1; }
*/
 ;
maybeTRANSoptions: /*nothing*/ | TRANSoptions ;
TRANSoptions : TRANSoption | TRANSoptions TRANSoption ;
TRANSoption
 : Value        { Ydebug( "... transistor option area = %g", $1 ); }
 | Identifier/*OFF*/
                { if( !CheckKeyword( $1, "OFF", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "OFF", "" );
                   else
                     Ydebug( "... transistor option OFF" );
                 }
 | Identifier/*TEMP*/ '=' Value
                { if( !CheckKeyword( $1, "TEMP", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "TEMP", "" );
                   else
                     Ydebug( "... transistor option TEMP = %g", $3 );
                 }
 | Identifier/*IC*/ '=' Value ',' Value
                {
                   if( !CheckKeyword( $1, "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "IC", "" );
                   else
                     Ydebug( "... transistor option IC = %g,%g", $3, $5 );
                 }
 ;
/*-------------------------------------------------------------------------*/
SemiJFET /*Jxxx*/
 :
   JFET error
                { MsgCompile( msgERRJFET, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   JFET Node/*D*/ Node/*G*/ Node/*S*/ Identifier
                { Ydebug( "(%s,%s,%s) JFET '%s', model '%s'",
                             $2,$3,$4,       $1,         $5 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypFetD, $1 );
                       ADD_NODE_MAIN( $3, nodtypFetG, $1 );
                       ADD_NODE_MAIN( $4, nodtypFetS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypFetD, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypFetG, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypFetS, $1 );
                     }
                     listJ = AddElem( listJ, &cntJ, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   maybeJFEToptions
/*** old version with strong order of parameters:
   maybeSEMIoptArea maybeSEMIoptOFF maybeTRANoptIC maybeSEMIoptTEMP
***/
   _ENDOFSTMT_
 ;
maybeJFEToptions : /*nothing*/ | JFEToptions ;
JFEToptions : JFEToption | JFEToptions JFEToption ;
JFEToption
 : Value        { Ydebug( "... jfet option area = %g", $1 ); }
 | Identifier/*OFF*/
                { if( !CheckKeyword( $1, "OFF", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "OFF", "" );
                   else
                     Ydebug( "... jfet option OFF" );
                 }
 | Identifier/*TEMP*/ '=' Value
                { if( !CheckKeyword( $1, "TEMP", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "TEMP", "" );
                   else
                     Ydebug( "... jfet option TEMP = %g", $3 );
                 }
 | Identifier/*IC*/ '=' Value ',' Value
                {
                   if( !CheckKeyword( $1, "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "IC", "" );
                   else
                     Ydebug( "... jfet option IC = %g,%g", $3, $5 );
                 }
 ;
/*-------------------------------------------------------------------------*/
SemiMESFET /*Zxxx*/
 :
   MESFET error
                { MsgCompile( msgERRMESFET, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   MESFET Node/*D*/ Node/*G*/ Node/*S*/ Identifier
                { Ydebug( "(%s,%s,%s) MESFET '%s', model '%s'",
                             $2,$3,$4,         $1,         $5 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypFetD, $1 );
                       ADD_NODE_MAIN( $3, nodtypFetG, $1 );
                       ADD_NODE_MAIN( $4, nodtypFetS, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypFetD, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypFetG, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypFetS, $1 );
                     }
                     listZ = AddElem( listZ, &cntZ, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                   if( PassNo == 2 ){ /* clear AREA befor process options: */
                     parmMesfetArea  = def_MesfetAREA;
                     existMesfetArea = FALSE;
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       if( UpFindModel( $5 ) == NULL ){ /* find model */
                         MsgCompile( msgMODELNOTFOUND, $5 );
                       }
                     }
                   }/*pass2*/
                 }
   maybeMESFEToptions
/*** old version with strong order of parameters:
   maybeSEMIoptArea maybeSEMIoptOFF maybeTRANoptIC
***/
                {
                   if( PassNo == 2 ){
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       Ydebug( "Try to generate code for MESFET '%s'...", $1 );
                       PrintElemsTransl( $1, circDefCurrent, listZ );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s, %s",
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, $2 ),
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, $3 ),
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, $4 )
                              );
                       if( !existMesfetArea ){
                         MsgCompileWarn( warn1_INFO, msgZXXX_NOTAREA,
                                                     def_MesfetAREA );
                         parmMesfetArea  = def_MesfetAREA;
                         existMesfetArea = TRUE;
                       }
                       if( existMesfetArea && parmMesfetArea <= 0.0 ){
                         MsgCompileWarn( warn1_INFO, msgZXXX_ILLAREA,
                                           parmMesfetArea, def_MesfetAREA );
                         parmMesfetArea  = def_MesfetAREA;
                       }
                       if( UpFindModel( $5 ) != NULL ){
                         if( !MkElemZxxx( $1, linebuffer, $5, parmMesfetArea ) )
                           MsgCompileWarn( warn5_BREAK, msgNOTGENELEM, $1 );
                       }
                     }
                   }/*pass2*/
                 }
   _ENDOFSTMT_
 ;
maybeMESFEToptions : /*nothing*/ | MESFEToptions ;
MESFEToptions : MESFEToption | MESFEToptions MESFEToption ;
MESFEToption
 : Value        { Ydebug( "... mesfet option area = %g", $1 );
                   parmMesfetArea  = $1;
                   existMesfetArea = TRUE;
                 }
 | Identifier/*OFF*/
                { if( !CheckKeyword( $1, "OFF", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "OFF", "" );
                   else
                     Ydebug( "... mesfet option OFF" );
                 }
 | Identifier/*IC*/ '=' Value ',' Value
                {
                   if( !CheckKeyword( $1, "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "IC", "" );
                   else
                     Ydebug( "... mesfet option IC = %g,%g", $3, $5 );
                 }
 ;
/*-------------------------------------------------------------------------*/
SemiMOSFET /*Mxxx*/
 :
   MOSFET error
                { MsgCompile( msgERRMOSFET, $1 );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   MOSFET Node/*D*/ Node/*G*/ Node/*S*/ Node/*B*/ Identifier
                { Ydebug( "(%s,%s,%s,%s) MOSFET '%s' (model '%s')",
                             $2,$3,$4,$5,         $1,         $6 );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( $2, nodtypFetD, $1 );
                       ADD_NODE_MAIN( $3, nodtypFetG, $1 );
                       ADD_NODE_MAIN( $4, nodtypFetS, $1 );
                       ADD_NODE_MAIN( $5, nodtypFetB, $1 );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( $2, nodtypFetD, $1 );
                       ADD_NODE_SUBCIRC( $3, nodtypFetG, $1 );
                       ADD_NODE_SUBCIRC( $4, nodtypFetS, $1 );
                       ADD_NODE_SUBCIRC( $5, nodtypFetB, $1 );
                     }
                     listM = AddElem( listM, &cntM, $1, circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
   maybeMOSFEToptions
   _ENDOFSTMT_
 ;
maybeMOSFEToptions : /*nothing*/ | MOSFEToptions ;
MOSFEToptions : MOSFEToption | MOSFEToptions MOSFEToption ;
MOSFEToption/*OFF,L_LEN/W_WIDTH/AD/AS/PD/PS/NRD/NRS/TEMP/IC*/
 : /* option(s) with 0 parameters */
   Identifier/*OFF*/
                {
                   if( CheckKeyword( $1, "OFF", "" ) ){
                       Ydebug( "... mosfet option OFF" );
                   }else if( CheckKeyword( $1, "L_LEN", "W_WIDTH",
                                               "AD", "AS", "PD", "PS",
                                               "NRD", "NRS", "TEMP", "IC",
                                               "" ) ){
                       MsgCompile( msgTOOFEWPARM, $1 );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }else{
                       MsgCompile( msgILLOPTION, $1 );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
 | /* option(s) with 1 parameter */
   Identifier/*L_LEN/W_WIDTH/AD/AS/PD/PS/NRD/NRS/TEMP*/ '=' Value
                {
                   if( CheckKeyword( $1, "L_LEN", "W_WIDTH",
                                         "AD", "AS", "PD", "PS",
                                         "NRD", "NRS", "TEMP",
                                         "" ) ){
                       Ydebug( "... mosfet option '%s' = %g", $1, $3 );
                   }else if( CheckKeyword( $1, "IC", "" ) ){
                       MsgCompile( msgTOOFEWPARM, $1 );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }else if( CheckKeyword( $1, "OFF", "" ) ){
                       MsgCompile( msgTOOMUCHPARM, $1 );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }else{
                       MsgCompile( msgILLOPTION, $1 );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
 | /* option(s) with 2 parameters */
   Identifier/*IC*/ '=' Value ',' Value
                {
                   if( CheckKeyword( $1, "IC", "" ) ){
                       Ydebug( "... mosfet option IC = %g,%g", $3, $5 );
                   } else if( CheckKeyword( $1, "OFF", "L_LEN", "W_WIDTH",
                                                "AD", "AS", "PD", "PS",
                                                "NRD", "NRS", "TEMP",
                                                "" ) ){
                       MsgCompile( msgTOOMUCHPARM, $1 );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }else{
                       MsgCompile( msgILLOPTION, $1 );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
 ;
/*-------------------------------------------------------------------------*/
/* old common semiconductors options: */
/*** old area option version (with default processing):
maybeSEMIoptArea
 : /*nothing*/
/*              { Ydebug( "... semiconductor (omited) option area = %g (default)", 1.0 ); }
 | Value
                { Ydebug( "... semiconductor option area = %g", $1 ); }
 ;
...***/
/*=========================================================================*/
StmtControlLines /*.CONTROL ... .ENDC*/ /*add ASB000303*/
 : _CONTROLS error
                { MsgCompile( msgERR_CONTROL );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 | _CONTROLS StmtCtrlLineList _ENDC
   _ENDOFSTMT_
 ;
StmtCtrlLineList : StmtControl | StmtCtrlLineList StmtControl ;
StmtControl
 : /*empty line*/ _ENDOFSTMT_
 | StmtOutput
 | StmtCtrlLine
 ;
/*=========================================================================*/
StmtCtrlLine /*.LET*/                   /*add ASB000304*/
 : _LET error   { MsgCompile( msgERR_LET );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 | _LET Identifier '=' ArExpr
   _ENDOFSTMT_
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.ALTER*/                 /*add ASB000304*/
 : _ALTER error { MsgCompile( msgERR_ALTER );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 ;
/*-------------------------------------------------------------------------*/
StmtOutput   /*.WRITE*/                 /*add ASB000304*/
 : _WRITE error { MsgCompile( msgERR_WRITE );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 ;
/*=========================================================================*/
StmtCtrlLine /*.OPTIONS*/
 : _OPTIONS error
                { MsgCompile( msgERR_OPTIONS );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 | _OPTIONS OPTIONSoptions
   _ENDOFSTMT_
 ;
OPTIONSoptions : OPTIONSopt | OPTIONSoptions OPTIONSopt ;
OPTIONSopt
 : Identifier   { Ydebug( ".OPTIONS: '%s'", $1 ); }
 | Identifier '=' Value
                { Ydebug( ".OPTIONS: '%s'=%g", $1, $3 ); }
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.NODESET*/
 : _NODESET error
                { MsgCompile( msgERR_NODESET );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 | _NODESET NODESEToptions
   _ENDOFSTMT_
 ;
NODESEToptions : NODESETopt | NODESEToptions NODESETopt ;
NODESETopt
 : Identifier/*V_SRC*/
                { if( !CheckKeyword( $1, "V", "" ) ){
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "V", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
   '(' Node ')' '=' Value
                { Ydebug( ".NODESET: V(%g) = %g", $4, $7 ); }
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.IC*/
 : _IC error
                { MsgCompile( msgERR_IC );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _IC ICoptions _ENDOFSTMT_
 ;
ICoptions : ICopt | ICoptions ICopt ;
ICopt
 : Identifier/*V_SRC*/
                { if( !CheckKeyword( $1, "V", "" ) ){
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "V", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
   '(' Node ')' '=' Value
                { Ydebug( ".IC: V(%g) = %g", $4, $7 ); }
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.AC*/
 : _AC error    { MsgCompile( msgERR_AC );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _AC DECorOCTorLIN DecInt Value Value _ENDOFSTMT_
                { Ydebug( ".AC: number of points = %ld, start freq = %g, stop freq = %g",
                                                     $3,              $4,             $5 );
                 }
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.DC*/
 : _DC error    { MsgCompile( msgERR_DC );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _DC _DClist _ENDOFSTMT_
 ;
_DClist : _DCquad | _DClist _DCquad ;
_DCquad
 : Identifier/*V... | I...*/
                { if( toupper( $1[0] ) != 'V' && toupper( $1[0] ) != 'I' ){
                     /* MsgCompile( msgNEEDVOLTAGE, $1 ); /* old: only V... */
                     MsgCompile( msgNEEDVOLTCURR, $1 ); /*add ASB000303*/
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
   Value Value Value
                { Ydebug( ".DC: src name '%s', start = %g, stop = %g, incr = %g",
                                           $1,          $3,        $4,        $5 );
                 }
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.DISTO*/
 : _DISTO error { MsgCompile( msgERR_DISTO );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _DISTO DECorOCTorLIN DecInt Value Value
                { Ydebug( ".DC: number of points = %ld, start freq = %g, stop freq = %g",
                                                     $3,              $4,             $5 );
                 }
   _ENDOFSTMT_
 |
   _DISTO DECorOCTorLIN DecInt Value Value Value
                { Ydebug( ".AC: number of points = %ld, start freq = %g, stop freq = %g, overf = %g",
                                                     $3,              $4,             $5,         $6 );
                 }
   _ENDOFSTMT_
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.NOISE*/
 : _NOISE error { MsgCompile( msgERR_NOISE );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _NOISE NoiseVoltage Identifier DECorOCTorLIN
   Value Value Value
                { Ydebug( ".NOISE source '%s', '%s' pts = %g,"
                                                    " fstart = %g, fstop = %g",
                              $3,   ShowKeyword($4),       $5,
                                                               $6,         $7 );
                 }
   maybePtsPerSum
   _ENDOFSTMT_
 ;
startNoiseVoltage
 : Identifier/*V_SRC*/
                { if( !CheckKeyword( $1, "V", "" ) ){
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "V", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
 ;
NoiseVoltage
 : startNoiseVoltage '(' Node ')'
                { Ydebug( "... .noise voltage V(%s)", $3 );
                 }
 | startNoiseVoltage '(' Node ',' Node ')'
                { Ydebug( "... .noise voltage V(%s)-V(%s)", $3, $5 ); }
 ;
 maybePtsPerSum
 : /*mothing*/
 | DecInt
                { Ydebug( "... .noise option pts_per_summary = %ld", $1 ); }
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.OP*/
 : _OP error    { MsgCompile( msgERR_OP );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _OP
                { Ydebug( ".OP statement" ); }
   _ENDOFSTMT_
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.PZ*/
 : _PZ error    { MsgCompile( msgERR_PZ );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
  _PZ Node Node Node Node CURorVOL POLorZERorPZ
                { Ydebug( ".PZ: nodes: inp='%s','%s' out='%s','%s',"
                           " type='%s', analysis='%s'",
                                             $2,  $3,       $4,  $5,
                       ShowKeyword($6),           ShowKeyword($7)
                         );
                 }
  _ENDOFSTMT_
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.SENS*/
 : _SENS error  { MsgCompile( msgERR_SENS );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _SENS OutVar maybeSENSopt
                { Ydebug( ".SENS statement" ); }
  _ENDOFSTMT_
 ;
maybeSENSopt
 : /*nothing*/
                { Ydebug( "... .sens option DC" ); }
 | AC DECorOCTorLIN DecInt Value Value
                { Ydebug( "... .sens options: AC '%s' poins = %ld, fstart = %g, fstop = %g",
                                                    $2,         $3,           $4,         $5 );
                 }
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.TF*/
 : _TF error    { MsgCompile( msgERR_TF );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _TF OutVar Identifier
                { Ydebug( ".TF: input source = '%s'", $3 ); }
  _ENDOFSTMT_
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.TRAN*/
 : _TRAN error  { MsgCompile( msgERR_TRAN );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 | _TRAN Value Value maybeUIC
                { Ydebug( ".TRAN: tstep = %g, tstop = %g, tstart = 0",
                                           $2,         $3 );
                 }
 | _TRAN Value Value Value maybeUIC
                { Ydebug( ".TRAN: tstep = %g, tstop = %g, tstart = %g",
                                           $2,         $3,          $4 );
                 }
 | _TRAN Value Value Value Value maybeUIC
                { Ydebug( ".TRAN: tstep = %g, tstop = %g, tstart = %g, tmax = %g",
                                           $2,         $3,          $4,        $5 );
                 }
   _ENDOFSTMT_
 ;
maybeUIC
 : /* nothing */
 | Identifier/*UIC*/
                {
                   if( !CheckKeyword( $1, "UIC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "UIC", "" );
                   else
                     Ydebug( "... .tran option UIC" );
                 }
 ;
/*-------------------------------------------------------------------------*/
/*StmtCtrlLine /*.TLINE*//*???*/
/*
 : _TLINE error { MsgCompile( msgERR_TLINE );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _TLINE DecInt DecInt DecInt DecInt Z0 '=' DecInt
   _TLINEopt1 _TLINEopt2 _TLINEopt3
   Identifier ',' Identifier ',' Identifier ',' Identifier
   _ENDOFSTMT_
 ;
_TLINEopt1
 : IdentifierTD '=' Identifier
                {
                   if( !CheckKeyword( $1, "TD", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "TD", "" );
                   else
                     Ydebug( "... .tline option TD = '%s'", $3 );
                 }
 ;
_TLINEopt2
 : IdentifierF_FREQ '=' Identifier
                {
                   if( !CheckKeyword( $1, "F", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "F", "" );
                   else
                     Ydebug( "... .tline option F = '%s'", $3 );
                 }
 | IdentifierF_FREQ '=' Identifier IdentifierNL '=' DecInt
                {
                   if( !CheckKeyword( $1, "F", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "F", "" );
                   else
                     Ydebug( "... .tline option F = '%s'", $3 );
                 }
 ;
_TLINEopt3 : IdentifierIC '=' Identifier ;
*/
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.WIDTH*/
 : _WIDTH error { MsgCompile( msgERR_WIDTH );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _WIDTH WidthParameters
                { Ydebug( ".WIDTH statement" ); }
   _ENDOFSTMT_
 ;
WidthParameters : WidthParameter | WidthParameters WidthParameter ;
WidthParameter
 : Identifier '=' Value
                { Ydebug( "... .width parameter '%s' = %g", $1, $3 ); }
 ;

/*=========================================================================*/
StmtCtrlLine /*.HB*/
 : _HB error    { MsgCompile( msgERR_HB );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _HB          { if( PassNo == 1 ){
                     exist_HB_F2 = FALSE;
                     parm_HB_F2 = 0.0;
                   }
                   /* may be a lot of .HB-operators in program: */
                 /*if( existHB ) MsgCompile( msgHBALREADYEXIST ); /*rem ASB000320*/
                 }
   Identifier/*F1*/ '=' Value /*','*/
   maybe_HB_F2
                { if( !CheckKeyword( $3, "F1", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, $3, "F1", "" );
                   else {
                     if( !existHB ) existHB = TRUE;
                     Ydebug( ".HB statement: F1=%g, F2=...", $5 );
                     /* parm_HB_F2 fixed in pass 1: not correct:
                     Ydebug( ".HB statement: F1=%g, F2=%g",
                             $5,    exist_HB_F2?parm_HB_F2:0.0 );
                     */
                     existHB00 = FALSE;
                     cntHB = 0;
                     if( PassNo == 1 ) /* save F1-parameter of .HB-operator: */
                         parm_HB_F1 = $5;
                     if( PassNo == 2 ) OutTmp( " &FREQU F1=%g, F2=%g, MN=",
                                       parm_HB_F1, exist_HB_F2?parm_HB_F2:0.0 );
                     /*NB! parm_HB_F* have illegal values for first
                           &FREQU-operators:
                           - it fixed on pass 1 for _last_ operator .HB
                           - first &FREQU-operators only put in temp-file,
                             not to result output
                     */
                   }
                 }
   _HBpairs     { if( PassNo == 2 ){
                     if( !existHB00 ){
                         OutTmp( ", 0,0" ); /* add required 0,0-pair */
                         cntHB++;
                     }
                     OutTmp( ", KN=%d /\n", cntHB );
                   }
                 }
   _ENDOFSTMT_
 ;
maybe_HB_F2
 : /*nothing*/
 | Identifier/*F2*/ '=' Value /*','*/
                { if( !CheckKeyword( $1, "F2", "" ) ){
                     MsgAllowKeywords( msgALLOWKEYWORD, $1, "F2", "" );
                     if( PassNo == 1 ) /* save F2-parameter of .HB-operator: */
                         parm_HB_F2 = 0.0;
                   }else{
                     exist_HB_F2 = TRUE;
                     if( PassNo == 1 ) /* save F2-parameter of .HB-operator: */
                         parm_HB_F2 = $3;
                   }
                 }
 ;
_HBpairs
 : _HBpair
 | _HBpairs /*','*/
                { if( PassNo == 2 ) OutTmp( ", " ); }
   _HBpair
 ;
_HBpair
 : DecInt /*','*/ DecInt
                {
                   Ydebug( "... .HB pair: %ld, %ld", $1, $2 );    /*$3->$2*/
                   cntHB++;
                   if( $1 == 0 && $2 == 0 ) existHB00 = TRUE;	
                   if( PassNo == 2 ) OutTmp( "%ld,%ld", $1, $2 ); /*$3->$2*/
                 }
 ;
/*-------------------------------------------------------------------------*/
StmtCtrlLine /*.HB_OPTIONS*/
 : _HB_OPTIONS error
                { MsgCompile( msgERR_HB_OPTIONS );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _HB_OPTIONS
                { Ydebug( ".HB_OPTIONS statement" ); }
   HabalaOptions
   _ENDOFSTMT_
 ;
HabalaOptions : HabalaOption | HabalaOptions HabalaOption ;
HabalaOption
 : Identifier '=' DecInt
                { if( GetOptNo( $1 ) == -1 ) /* illegal option */
                     MsgCompile( msgUNKNWN_HB_OPT, $1 ); /**/
                   else {
                     Ydebug( "... .HB_OPTIONS decimal parameter '%s' = %ld", $1, $3 );
                     if( PassNo == 1 ) ChangeConfOption( $1, 0.0, $3 );
                   }
                 }
 | Identifier '=' Float
                { if( GetOptNo( $1 ) == -1 ) /* illegal option */
                     MsgCompile( msgUNKNWN_HB_OPT, $1 ); /**/
                   else {
                     Ydebug( "... .HB_OPTIONS float parameter '%s' = %g", $1, $3 );
                     if( PassNo == 1 ) ChangeConfOption( $1, $3, 0 );
                   }
                 }
 ;
/*=========================================================================*/
StmtOutput /*.SAVE*/
 : _SAVE error  { MsgCompile( msgERR_SAVE );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _SAVE SaveVectors
                { Ydebug( ".SAVE statement" ); }
   _ENDOFSTMT_
 ;
SaveVectors : SaveVector | SaveVectors SaveVector ;
SaveVector
 : Identifier
                { Ydebug( "... .save vector '%s'", $1 ); }
 | STRING
                { Ydebug( "... .save vector '%s'", $1 ); }
 | OutVar
 ;
/*-------------------------------------------------------------------------*/
StmtOutput /*.FOUR*/
 : _FOUR error  { MsgCompile( msgERR_FOUR );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _FOUR Value FourOutVarList
                { Ydebug( ".FOUR: frequency = %g", $2 ); }
   _ENDOFSTMT_
 ;
FourOutVarList : OutVar | FourOutVarList OutVar ;
/*-------------------------------------------------------------------------*/
StmtOutput /*.PRINT*/
 : _PRINT error { MsgCompile( msgERR_PRINT );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _PRINT PrintType PrintList
                { Ydebug( ".PRINT: type of analysis = '%s'", ShowKeyword($2) ); }
   _ENDOFSTMT_
 ;
PrintList : PrintItem | PrintList PrintItem ;
PrintItem
 : STRING
                { Ydebug( "... .print parameter \"%s\"", $1 ); }
 | Identifier /*?*/
                { Ydebug( "... .print parameter '%s'", $1 ); }
 | PrintOutVar
 ;
/*-------------------------------------------------------------------------*/
StmtOutput /*.PLOT*/
 : _PLOT error  { MsgCompile( msgERR_PLOT );
                   YERRSKIP;
                 }
   _ENDOFSTMT_
 |
   _PLOT PrintType PlotOutVarList
                { Ydebug( ".PLOT: type of analysis = '%s'", ShowKeyword($2) ); }
   _ENDOFSTMT_
 ;
PlotOutVarList : PlotOutVar | PlotOutVarList PlotOutVar ;
PlotOutVar : PrintOutVar | PrintOutVar '(' Value ',' Value ')' ;
/*-------------------------------------------------------------------------*/
PrintType
 : Identifier/*DC/AC/TRAN/NOISE/DISTO/PZ*/
                {
                   int kw;
                   kw = LookupKeyword( $1 );
                   switch( kw ){
                   case DC: case AC: case TRAN: case NOISE: case DISTO: case PZ:
                     $$ = kw;
                     break; 
                   /*
                   case FALSE:
                     MsgCompile( msgNOTRESERVKEY, strupr($1) );
                     break;
                   */ 
                   default:
                     MsgAllowKeywords( msgILLPRNTYPE, strupr($1),
                        "DC", "AC", "TRAN", "NOISE", "DISTO", "PZ", "" );
                     break; 
                   }
                 }
 ;
PrintOutVar
 : Identifier/*I_SRC/V_SRC/VR/VI/VM/VP/VDB*/ '(' Node ')'
                { if( !CheckKeyword( $1, "I","V","VR","VI","VM","VP","VDB", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, strupr($1),
                                 "I","V","VR","VI","VM","VP","VDB", "" );
                   else
                     Ydebug( "... outvar %s(%s)", $1, $3 );
                 }
 | Identifier/*I_SRC/V_SRC/VR/VI/VM/VP/VDB*/ '(' Node ',' Node ')'
                { if( !CheckKeyword( $1, "I","V","VR","VI","VM","VP","VDB", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, strupr($1),
                                 "I","V","VR","VI","VM","VP","VDB", "" );
                   else
                     Ydebug( "... outvar V(%s,%s)", $1, $3, $5 );
                 }
 ;
/*=========================================================================*/
/* common: */
/*-------------------------------------------------------------------------*/
/*OptionStartKeyword /* common begin of options like "KEYWORD = ..." */
/* : Identifier/*KEYWORD*/
/*              {
                   int kw;
                   kw = LookupKeyword( $1 );
                   if( kw == FALSE ){
                       MsgCompile( msgNOTRESERVKEY, $1 );
                    /* SKIPTOEOLN; /*SkipRestLine();/**/
/*                 }else{
                       $$ = kw;
                    /* Ydebug( "... start keyword '%s' of option", $1 ); */
/*                 }
                 }
 ;
*/
/*-------------------------------------------------------------------------*/
OutVar
 : Identifier/*V_SRC/I_SRC*/ '(' Node ')'
                { if( !CheckKeyword( $1, "V", "I", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, $1, "V", "I", "" );
                   else
                     Ydebug( "... outvar %s(%s)", $1, $3 );
                 }
 | Identifier/*V_SRC/I_SRC*/ '(' Node ',' Node ')'
                { if( !CheckKeyword( $1, "V", "I", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, $1, "V", "I", "" );
                   else
                     Ydebug( "... outvar V(%s,%s)", $1, $3, $5 );
                 }
 ;
/*-------------------------------------------------------------------------*/
Identifier  /* Identifier only: returned pointer to strint */
 : IDENTNAME    { $<strptr>$ = $<strptr>1; }
 ;
/*-------------------------------------------------------------------------*/
Node        /* Node with delimiters */
 : NodeName
                        { $<strptr>$ = $<strptr>1; }
/* rem by ASB000418:
 | ',' NodeName
                        { $<strptr>$ = $<strptr>2; }
*/
/* produce 1 shift/reduce conflict:
 | '(' NodeName ')'
                        { $<strptr>$ = $<strptr>2; }
 | '(' NodeName ','
                        { $<strptr>$ = $<strptr>2; }
 | NodeName ')'
                        { $<strptr>$ = $<strptr>1; }
*/
 ;
/*Node : mbLNodeDelim NodeName mbRNodeDelim ;/**/
/*mbLNodeDelim : /*empty** | '(' ;           /**/
/*mbRNodeDelim : /*empty** | ')' | ',' ;     /**/
/*-------------------------------------------------------------------------*/
NodeName    /* Node: may be not only decimal int and float/identifier yet */
            /* returned pointer to strint */
 : IDENTINT     /* decimal number or identifier */
                        { $<strptr>$ = $<mixed>1->text; }
 | IDENTFLOAT   /* float number or identifier */
                        { $<strptr>$ = $<mixed>1->text; }
 | IDENTNAME
                        { $<strptr>$ = $<strptr>1; }
 ;
/*-------------------------------------------------------------------------*/
Value           /* returned signed float value */
 :     UnsignValue      { $<floatnum>$ = $<floatnum>1;
                         }
 | '+' UnsignValue      { $<floatnum>$ = $<floatnum>2;
                         }
 | '-' UnsignValue      { $<floatnum>$ = -$<floatnum>2;
                         }
 ;
UnsignValue /* returned unsigned float value */
 : DECINTNUM    /* decimal that not be node/identifier */
                        { $<floatnum>$ = (double)$<longnum>1;
                         }
 | FLOATNUM     /* float that not be node/identifier */
                        { $<floatnum>$ = $<floatnum>1;
                         }
 | IDENTINT     /* decimal number or identifier */
                        { $<floatnum>$ = (double)$<mixed>1->longval;
                /*
                MsgDebug( "IDENTINT" );
                SHOWlong( $<mixed>1->longval );
                SHOWfloat( (double)$<mixed>1->longval );
                SHOWfloat( $<floatnum>$ );
                */
                         } 
 | IDENTFLOAT   /* float number or identifier */
                        { $<floatnum>$ = $<mixed>1->floatval;
                         }
 ;
/*-------------------------------------------------------------------------*/
Float         /* returned signed float value */
 :     UnsignFloat      { $<floatnum>$ = $<floatnum>1;
                         }
 | '+' UnsignFloat      { $<floatnum>$ = $<floatnum>1;
                         }
 | '-' UnsignFloat      { $<floatnum>$ = -$<floatnum>1;
                         }
 ;
UnsignFloat   /* returned unsigned float value */
 : FLOATNUM     /* float that not be node/identifier */
                        { $<floatnum>$ = $<floatnum>1;
                         }
 | IDENTFLOAT   /* float number or identifier */
                        { $<floatnum>$ = $<mixed>1->floatval;
                         } 
 ;
/*-------------------------------------------------------------------------*/
DecInt        /* returned signed integer value */
 :     UnsignDecInt     { $<longnum>$ = $<longnum>1;
                           /* Ydebug( "DecInt = %ld", $$ );/**/
                         }
 | '-' UnsignDecInt     { $<longnum>$ = -$<longnum>2;
                           /* Ydebug( "-DecInt = %ld", $$ );/**/
                         }
 | '+' UnsignDecInt     { $<longnum>$ = $<longnum>2;
                           /* Ydebug( "+DecInt = %ld", $$ );/**/
                         }
 ;
UnsignDecInt  /* returned unsigned integer value */
 : DECINTNUM    /* decimal that not be node/identifier */
                        { $<longnum>$ = $<longnum>1;
                         }
 | IDENTINT     /* decimal number or identifier */
                        { $<longnum>$ = $<mixed>1->longval;
                         } 
 ;
/*-------------------------------------------------------------------------*/
DECorOCTorLIN
 : Identifier/*DEC/OCT/LIN*/
                {
                   int kw;
                   kw = LookupKeyword( $1 );
                   switch( kw ){
                   case DEC: case OCT: case LIN:
                     $$ = kw;
                     break; 
                   case FALSE:
                     MsgCompile( msgNOTRESERVKEY, $1 );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   default:
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "DEC", "OCT", "LIN", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   }
                 }
 ;
/*-------------------------------------------------------------------------*/
CURorVOL
 : Identifier/*CUR/VOL*/
                {
                   int kw;
                   kw = LookupKeyword( $1 );
                   switch( kw ){
                   case CUR: case VOL:
                     $$ = kw;
                     break; 
                   case FALSE:
                     MsgCompile( msgNOTRESERVKEY, $1 );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   default:
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "CUR", "VOL", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   }
                 }
 ;
/*-------------------------------------------------------------------------*/
POLorZERorPZ
/*** old:
 : POL      { $$ = POL; }
 | ZER      { $$ = ZER; }
 | PZ       { $$ = PZ; }
***/
 : Identifier/*POL/ZER/PZ*/
                {
                   int kw;
                   kw = LookupKeyword( $1 );
                   switch( kw ){
                   case POL: case ZER: case PZ:
                     $$ = kw;
                     break; 
                   case FALSE:
                     MsgCompile( msgNOTRESERVKEY, $1 );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   default:
                     MsgAllowKeywords( msgALLOWKEYWORD,  $1, "POL", "ZER", "PZ", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   }
                 }
 ;
/*=========================================================================*/

%%

/*-------------------------------------------------------------------------*/
/* ASB000603: all static functions move to SHparsY.C */
/*-------------------------------------------------------------------------*/
