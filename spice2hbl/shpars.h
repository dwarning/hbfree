
/*
 *  S H p a r s . H
 *
 *  29.12.1999 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# ifndef SHpars_H
# define SHpars_H
/*-------------------------------------------------------------------------*/

# define    YERRSKIP     yyerrok; yyclearin; SkipRestLine(); /**/

# define TEST_EOF                                    \
    if( isEOF ){ /* end of file without .END */      \
      MsgErrorCnt( msgERR_ENDNOTFND/*"TEST_EOF"*/ ); \
                   /* ++cntErrors; */                \
      return cntErrors;                              \
    }                                                \
/*TEST_EOF*/

# define SKIPTOEOLN                                    \
 /* MsgDebug( "SKIPTOEOLN...\n" );/**/                 \
    SkipRestLine();                                    \
    if( isEOF ){ /* end of file without .END */        \
      MsgErrorCnt( msgERR_ENDNOTFND/*"SKIPTOEOLN"*/ ); \
                   /* ++cntErrors; */                  \
      return cntErrors;                                \
    }                                                  \
/*SKIPTOEOLN*/

# define ADD_NODE_MAIN( name, type, elem )                       \
 /* MsgDebug("ADD_NODE_MAIN(%s,%s,%s)\n", name, type, elem);/**/ \
    listNodesTmp = listNodesMain; cntNodesTmp = cntNodesMain;    \
    if( ( tailNodesMain = AddNodeInfo(                           \
                            listNodesTmp, tailNodesMain,         \
                            cntNodesTmp, name, type, elem        \
                                     ) ) == NULL                 \
      ) MsgErrorFatal( exitFAILADDNODE, msgFAILADDNODE );        \
    else { listNodesMain = listNodesTmp;                         \
           cntNodesMain  = cntNodesTmp; }                        \
/*ADD_NODE_MAIN()*/

# define ADD_NODE_SUBCIRC( name, type, elem )                            \
 /* MsgDebug("ADD_NODE_SUBCIRC(%s,%s,%s)\n", name, type, elem);/**/      \
    listNodesTmp = circDefCurrent->circnodlist;                          \
    cntNodesTmp  = circDefCurrent->cntnodes;                             \
    if( ( circDefCurrent->circnodtail = AddNodeInfo(                     \
                                            listNodesTmp,                \
                                            circDefCurrent->circnodtail, \
                                            cntNodesTmp,                 \
                                            name, type, elem             \
                                                   )                     \
        ) == NULL                                                        \
      ) MsgErrorFatal( exitFAILADDNODE, msgFAILADDNODE );                \
    else { circDefCurrent->circnodlist = listNodesTmp;                   \
           circDefCurrent->cntnodes    = cntNodesTmp; }                  \
/*ADD_NODE_SUBCIRC()*/
  
# define ADD_NODE_SUBCIRCPARM( name, type, elem )                       \
 /* MsgDebug("ADD_NODE_SUBCIRCPARM(%s,%s,%s)\n", name, type, elem);/**/ \
    listNodesTmp = circDefCurrent->nodeparmlist;                        \
    cntNodesTmp  = circDefCurrent->cntnodesparm;                        \
    if( ( circDefCurrent->nodeparmtail = AddNodeInfo(                   \
                                       listNodesTmp,                    \
                                       circDefCurrent->nodeparmtail,    \
                                       cntNodesTmp,                     \
                                       name, type, elem                 \
                                                ) ) == NULL             \
      ) MsgErrorFatal( exitFAILADDNODE, msgFAILADDNODE );               \
    else { circDefCurrent->nodeparmlist = listNodesTmp;                 \
           circDefCurrent->cntnodesparm = cntNodesTmp; }                \
/*ADD_NODE_SUBCIRCPARM()*/

# define CLR_ELEM_PARM /* clear elems parms: */  \
    isGenOK      = FALSE;                        \
    existZ0      = FALSE;    parmZ0      = 0.0;  \
    existTD      = FALSE;    parmTD      = 0.0;  \
    existF_FREQ  = FALSE;    parmF_FREQ  = 0.0;  \
    existNL      = FALSE;    parmNL      = 0.0;  \
                             parmLength  = 0.0;  \
    existDiodArea= FALSE;    parmDiodArea= 0.0;  \
/*CLR_ELEM_PARM*/

# define MAKEELEM_IVxxx /* generate code for I/Vxxx: */       \
  if( !isSubDefBody /*subcall in main circuit*/ ||            \
      ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ) \
  {                                                           \
    if( isIxxx ){ /* for Ixxx */                              \
 /*   MsgDebug( "MAKEELEM_Ixxx: Gi=%g, F=%g, Fi=%g, Jm=%g\n", \
                IVxxxGi, IVxxxF, IVxxxFi, IVxxxJm );/**/      \
      /* rename current element only: */                      \
      renI = Rename1elem( IxxxName, listI, circCall, renI );  \
      /* out to elements names tranlations file: */           \
      PrintElemsTransl( IxxxName, circDefCurrent, listI );    \
      /* generate code: */                                    \
      MkElemIxxx( IxxxName,                                   \
                  linebuffer, /* saved habala-nodes list */   \
                  IVxxxGi, IVxxxF, IVxxxFi, IVxxxJm );        \
      IVxxxGi = IVxxxF = IVxxxFi = IVxxxJm = 0.0;             \
    }else if( isVxxx ){ /* for Vxxx */                        \
 /*   MsgDebug( "MAKEELEM_Vxxx: Gi=%g, F=%g, Fi=%g, Jm=%g\n", \
                IVxxxGi, IVxxxF, IVxxxFi, IVxxxJm );/**/      \
      /* rename current element only: */                      \
      renV = Rename1elem( VxxxName, listV, circCall, renV );  \
      /* out to elements names tranlations file: */           \
      PrintElemsTransl( VxxxName, circDefCurrent, listV );    \
      /* generate code: */                                    \
      MkElemVxxx( VxxxName,                                   \
                  linebuffer, /* saved habala-nodes list */   \
                  IVxxxGi, IVxxxF, IVxxxFi, IVxxxJm );        \
      IVxxxGi = IVxxxF = IVxxxFi = IVxxxJm = 0.0;             \
    }                                                         \
  }                                                           \
/*MAKEELEM_IVxxx*/

/*-------------------------------------------------------------------------*/

extern KeysTab KeywordsTab[];

extern int isSubCktDefBody; /* set if parser go into subcircuit definition */

extern StackCirc *stackCircuits;  /* stack of subcircuits */
extern SubCirc   *circDefCurrent; /* pointer to current subcircuit */
                                  /* if parser inside .subckt ... .ends, */
                                  /* = NULL for main circuit */

extern StackCalls *stackCallsHead; /* stack of subcalls (subcalls path) */
extern StackCalls *stackCallsTail; /* tail of subcalls stack */

extern int isSubDefBody;  /* set TRUE if parser go into subcircuit definition */
extern int levelSubDef;   /* counter of nested circuits definitions */

extern int isSubCall;     /* set when operator Xxxx in process */
extern int levelSubCall;  /* counter of nested circuits subcall */

extern SubCirc *circCall; /* pointer to called subcircuit, default = NULL */

extern int cnt_SUBCKT, cnt_ENDS; /* counters of circuit brackets */
extern int cnt_END;              /* .END indicator */

extern double parm_HB_F1, parm_HB_F2; /* extern parameters of .HB-operator */

/*-------------------------------------------------------------------------*/
/* common functions: */

int     yyparse( void );

/*-------------------------------------------------------------------------*/
/* functions used only in SHpars.Y: */

void CheckCircBrackets( void ); /* check numbers of .subckt = .ends, .end */
char *ShowKeyword( long tokencode ); /* return image of keyword */
char *FindToken(int type);           /* return image of token 'type' */
int  LookupKeyword( char *str );     /* keywords look up function */
int  CheckKeyword( char *ident, /*char *key1,*/ ... /*,""*/ );
void Ydebug( char *msgfmt, ... );    /* Print debug message */
int  yyerror( char *msg );           /* standard YACC error message procedure */
    void
MsgAllowKeywords( /* print message about allowed keywords */
    char *msgno,  /* error code number */ /* Don't use '\n' in message */
    char *msgfmt,
    char *ident,
 /* char *key1, */
         ...
 /* last keyN must be "" ! */
);
/*-------------------------------------------------------------------------*/
# endif /* define SHpars_H */
