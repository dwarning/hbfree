
/*
 *  S H p a s s 2 . H
 *
 *  03.01.2000 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# ifndef SHpass2_H
# define SHpass2_H
/*-------------------------------------------------------------------------*/
/* physical constants: */
# define  const_M0  1.256637E-06  /* permeability - магнитная проницаемость вакуума */
# define  const_E0  8.85418E-12   /* permittivity - диэлектрическая проницаемость вакумма */
# define  const_c   299792597.676 /* light velocity - скороть света в вакууме */
                    /* = ( 1 / sqrt( const_M0 * const_E0 ) ) */
# define  const_q   1.602E-19     /* elementary charge - заряд электрона */
# define  const_k   1.381E-23     /* Boltzman constant - постоянная Больцмана */
# define  const_T   300           /* normal temperature - нормальная температура, K */
                    /*  = 27 Сelsium = 273 + 27 [K] */

# define  const_Pi  ( 4.0 * atan( 1.0 ) ) /* = 3.141592653589793 */

/* defaults for diodes(Dxxx): */
# define  def_NRMLEN    0.25 /*NL*/
# define  def_DiodArea  1.0
# define  def_DiodIS    1.0e-14
# define  def_DiodN     1.0
# define  def_DiodCJ0   0.0
# define  def_DiodVJ    1.0
# define  def_DiodM     0.5
# define  def_DiodTT    0.0

/* defaults for MESFET(Zxxx): */
# define  def_MesfetAREA     1.0
/* default model parameters for MESFET(Zxxx): */
# define  def_MesfetVTO_S   -2.0
# define  def_MesfetBETA_S   1.e-4
# define  def_MesfetB_S      0.3
# define  def_MesfetALPHA_S  2
# define  def_MesfetLAMBDA_S 0
# define  def_MesfetRD_S     0
# define  def_MesfetRS_S     0
# define  def_MesfetCGS_S    0
# define  def_MesfetCGD_S    0
# define  def_MesfetPB_S     0.6

/*-------------------------------------------------------------------------*/
/* get and check model parameter for MkElem?xxx(): */

/*** get and check model parameters, but not set to def for ill value: */

/* param MUST be == VAL: */
# define GETANDCHKPARMeqVAL(VAL,PARMNAME,PARMVAR,model,modelname,elemname)    \
{ int exist;                                                                  \
    if( !GetModelParm( model, PARMNAME, &PARMVAR, &exist ) ){                 \
                                                 /* param not found: ERROR: */\
      MsgCompile( msgMODPARMNOTFND, PARMNAME, modelname, elemname );          \
      return FALSE;                                                           \
    }else if( !exist ){         /* param found in model but not set: ERROR: */\
      MsgCompile( msgPARMNEEDVALUE, PARMNAME, modelname, elemname );          \
      return FALSE;                                                           \
    }else if( PARMVAR != VAL ){                   /* ill parm value: ERROR: */\
      MsgCompile( msgMODLPARMILLVAL, PARMNAME, modelname, elemname, PARMVAR );\
      return FALSE;                                                           \
}   }

/* param MUST be > VAL: */
# define GETANDCHKPARMgtVAL(VAL,PARMNAME,PARMVAR,model,modelname,elemname)    \
{ int exist;                                                                  \
    if( !GetModelParm( model, PARMNAME, &PARMVAR, &exist ) ){                 \
                                                 /* param not found: ERROR: */\
      MsgCompile( msgMODPARMNOTFND, PARMNAME, modelname, elemname );          \
      return FALSE;                                                           \
    }else if( !exist ){         /* param found in model but not set: ERROR: */\
      MsgCompile( msgPARMNEEDVALUE, PARMNAME, modelname, elemname );          \
      return FALSE;                                                           \
    }else if( PARMVAR <= VAL ){                   /* ill parm value: ERROR: */\
      MsgCompile( msgMODLPARMILLVAL, PARMNAME, modelname, elemname, PARMVAR );\
      return FALSE;                                                           \
}   }


/*** get (without check) model parameter, warn and set to def: */
# define GETORSETDEFPARM(DEF,PARMNAME,PARMVAR,model,modelname,elemname)       \
{ int exist; double tmpDEF = DEF;                                             \
    if( !GetModelParm( model, PARMNAME, &PARMVAR, &exist ) ){                 \
                                      /* param not found: warn, set to def: */\
      PARMVAR = DEF;                                                          \
      MsgCompileWarn( warn1_INFO, msgPARMVALSETDEF,                           \
                      elemname, PARMNAME, modelname, tmpDEF );                \
    }else if( !exist ){   /* param found but not set (?): warn, set to def: */\
      MsgCompileWarn( warn1_INFO, msgPARMNOVALSETDEF,                         \
                      PARMNAME, modelname, elemname, DEF );                   \
      PARMVAR = DEF;                                                          \
}   }

/*-----------------------------------*/

/*** get and check model parameter, warn and set to def: */
/* ASB001223: if ill condition change warn1_INFO to warn3_CONFLICT: */

/* param must be == VAL, if it not exist or wrong then set param to DEF: */
# define GETORSETDEFPARMeqVAL(VAL,DEF,PARMNAME,PARMVAR,model,modelname,elemname)\
{ int exist; double tmpDEF = DEF;                                             \
    if( !GetModelParm( model, PARMNAME, &PARMVAR, &exist ) ){                 \
                                      /* param not found: warn, set to def: */\
      PARMVAR = DEF;                                                          \
      MsgCompileWarn( warn1_INFO, msgPARMVALSETDEF,                           \
                      elemname, PARMNAME, modelname, tmpDEF );                \
    }else if( !exist ){   /* param found but not set (?): warn, set to def: */\
      MsgCompileWarn( warn1_INFO, msgPARMNOVALSETDEF,                         \
                      PARMNAME, modelname, elemname, DEF );                   \
      PARMVAR = DEF;                                                          \
    }else if( !(PARMVAR == VAL) ){     /* ill parm value: warn, set to def: */\
      MsgCompileWarn(  warn3_CONFLICT, msgPARMILLVALSETDEF,                   \
                      PARMNAME, modelname, elemname, PARMVAR, DEF );          \
      PARMVAR = DEF;                                                          \
}   }

/* param must be > VAL, if it not exist or wrong then set param to DEF: */
# define GETORSETDEFPARMgtVAL(VAL,DEF,PARMNAME,PARMVAR,model,modelname,elemname)\
{ int exist; double tmpDEF = DEF;                                             \
    if( !GetModelParm( model, PARMNAME, &PARMVAR, &exist ) ){                 \
                                      /* param not found: warn, set to def: */\
      MsgCompileWarn( warn1_INFO, msgPARMVALSETDEF,                           \
                      elemname, PARMNAME, modelname, tmpDEF );                \
      PARMVAR = DEF;                                                          \
    }else if( !exist ){   /* param found but not set (?): warn, set to def: */\
      MsgCompileWarn( warn1_INFO, msgPARMNOVALSETDEF,                         \
                      PARMNAME, modelname, elemname, tmpDEF );                \
      PARMVAR = DEF;                                                          \
    }else if( !(PARMVAR > VAL) ){      /* ill parm value: warn, set to def: */\
      MsgCompileWarn(  warn3_CONFLICT, msgPARMILLVALSETDEF,                   \
                      PARMNAME, modelname, elemname, PARMVAR, tmpDEF );       \
      PARMVAR = DEF;                                                          \
}   }

/*add ASB001223:*/
/* param must be >= VAL, if it not exist or wrong then set param to DEF: */
# define GETORSETDEFPARMgeVAL(VAL,DEF,PARMNAME,PARMVAR,model,modelname,elemname)\
{ int exist; double tmpDEF = DEF;                                             \
    if( !GetModelParm( model, PARMNAME, &PARMVAR, &exist ) ){                 \
                                      /* param not found: warn, set to def: */\
      MsgCompileWarn( warn1_INFO, msgPARMVALSETDEF,                           \
                      elemname, PARMNAME, modelname, tmpDEF );                \
      PARMVAR = DEF;                                                          \
    }else if( !exist ){   /* param found but not set (?): warn, set to def: */\
      MsgCompileWarn( warn1_INFO, msgPARMNOVALSETDEF,                         \
                      PARMNAME, modelname, elemname, tmpDEF );                \
      PARMVAR = DEF;                                                          \
    }else if( !(PARMVAR >= VAL) ){     /* ill parm value: warn, set to def: */\
      MsgCompileWarn(  warn3_CONFLICT, msgPARMILLVALSETDEF,                   \
                      PARMNAME, modelname, elemname, PARMVAR, tmpDEF );       \
      PARMVAR = DEF;                                                          \
}   }
/*-----------------------------------*/
/* param must be == VAL, set to DEF if not exist: */
# define GETDEFCHKPARMeqVAL(VAL,DEF,PARMNAME,PARMVAR,model,modelname,elemname)\
{ int exist; double tmpDEF = DEF;                                             \
    if( !GetModelParm( model, PARMNAME, &PARMVAR, &exist ) ){                 \
                                      /* param not found: warn, set to def: */\
      MsgCompileWarn( warn1_INFO, msgPARMVALSETDEF,                           \
                      elemname, PARMNAME, modelname, tmpDEF );                \
      PARMVAR = DEF;                                                          \
    }else if( !exist ){              /* param found but not set (?): ERROR: */\
      MsgCompile( msgPARMNEEDVALUE, PARMNAME, modelname, elemname );          \
      return FALSE;                                                           \
    }else if( PARMVAR != VAL ){                   /* ill parm value: ERROR: */\
      MsgCompile( msgMODLPARMILLVAL, PARMNAME, modelname, elemname, PARMVAR );\
      return FALSE;                                                           \
}   }

/* param must be > VAL, set to DEF if not exist: */
# define GETDEFCHKPARMgtVAL(VAL,DEF,PARMNAME,PARMVAR,model,modelname,elemname)\
{ int exist; double tmpDEF = DEF;                                             \
    if( !GetModelParm( model, PARMNAME, &PARMVAR, &exist ) ){                 \
                                      /* param not found: warn, set to def: */\
      MsgCompileWarn( warn1_INFO, msgPARMVALSETDEF,                           \
                      elemname, PARMNAME, modelname, tmpDEF );                \
      PARMVAR = DEF;                                                          \
    }else if( !exist ){              /* param found but not set (?): ERROR: */\
      MsgCompile( msgPARMNEEDVALUE, PARMNAME, modelname, elemname );          \
      return FALSE;                                                           \
    }else if( PARMVAR <= VAL ){                   /* ill parm value: ERROR: */\
      MsgCompile( msgMODLPARMILLVAL, PARMNAME, modelname, elemname, PARMVAR );\
      return FALSE;                                                           \
}   }

/* param must be >= VAL, set to DEF if not exist: */
# define GETDEFCHKPARMgeVAL(VAL,DEF,PARMNAME,PARMVAR,model,modelname,elemname)\
{ int exist; double tmpDEF = DEF;                                             \
    if( !GetModelParm( model, PARMNAME, &PARMVAR, &exist ) ){                 \
                                      /* param not found: warn, set to def: */\
      MsgCompileWarn( warn1_INFO, msgPARMVALSETDEF,                           \
                      elemname, PARMNAME, modelname, tmpDEF );                \
      PARMVAR = DEF;                                                          \
    }else if( !exist ){              /* param found but not set (?): ERROR: */\
      MsgCompile( msgPARMNEEDVALUE, PARMNAME, modelname, elemname );          \
      return FALSE;                                                           \
    }else if( PARMVAR < VAL ){                    /* ill parm value: ERROR: */\
      MsgCompile( msgMODLPARMILLVAL, PARMNAME, modelname, elemname, PARMVAR );\
      return FALSE;                                                           \
}   }

/*-------------------------------------------------------------------------*/
typedef struct subparm {    /* subcircuit call fact parameters list */
        char    *parmname;
 struct subparm *nextparm;
} SubParm;
/*-------------------------------------------------------------------------*/
typedef struct  stackcall {  /* stack of subcalls */
        char      *callname; /* name of subcall: "Xxxx" */
 struct stackcall *next;     /* next element of stack */
 struct stackcall *prev;     /* previous element of stack */
} StackCalls;
/*-------------------------------------------------------------------------*/
    SubParm *          /* returned pointer to list of parameters */
AddSubParm(            /* function to add node-parameter to parameters list */
    SubParm *parmlist, /* subcall parameters list */
    char    *parmname, /* name of add parameter */
    long    *parmcnt   /* number of new parameter in list */
);
/*-------------------------------------------------------------------------*/
    void
PrintSubParm(          /* debug print of subcirc call parameters list */
    SubParm *parmlist  /* subcall parameters list */
);
/*-------------------------------------------------------------------------*/
    void
DelParmSubCall(        /* empty subcirc call parameters list */
    SubParm *parmlist  /* subcall parameters list */
);
/*-------------------------------------------------------------------------*/
    char *            /* return name of file if success */ /*not use*/
MakeCircBody(         /* function for make tmp-file with circuit body */
    SubCirc *circCall /* pointer subcircuit */
);
/*-------------------------------------------------------------------------*/
    int                  /* returned TRUE if OK of FALSE if fail function for */
RenameLocalNodes(        /* rename local nodes in subcircuit before it subcall */
    SubCirc *circCall,   /* called subcircuit */
    SubCirc *circcurr,   /* subcircuit-caller */
    SubParm *parmSubCall /* fact parameters in subcall */
);
/*-------------------------------------------------------------------------*/
    void
AddCallPath(       /* add Xxxx-name to circuit call path in 'stackCallsHead' */
    char *callname /* name of current subcall 'Xxxx' */
);
    void
RemCallPath(       /* remove last Xxxx-name from circuit call path 'stackCallsHead' */
    void
);
    void
OutCallPath(       /* print circuit call path from 'stackCallsHead' */
    FILE    *ofil  /* file for out */
);
/*-------------------------------------------------------------------------*/
int Pass2( void );
/*-------------------------------------------------------------------------*/
char *ftoa( double var ); /* function for convert double to string */
/*-------------------------------------------------------------------------*/
    int       /* returned TRUE if OK, FALSE if fail */
MkElemOxxx(   /*  function for make habala-code for element Oxxx */
    char *elemname, /* (spice) element name */
    char *nodebuf,  /* habala nodes-parameters list together */
    char *modelname /* model name */
);
    int       /* returned TRUE if OK, FALSE if fail */
MkElemDxxx(   /* function for making habala-code for element Dxxx */
    char   *elemname,   /* (spice) element name */
    char   *nodebuf,    /* habala nodes-parameters list together */
    char   *modelname,  /* model name */
    double parmDiodArea /* diode parameter AREA */
);
    void
MkElemIxxx(   /* function for making habala-code for element Ixxx */
    char   *elemname, /* (spice) element name */
    char   *nodebuf,  /* habala nodes-parameters list together */
    double Gi,        /* Gi habala parameter */
    double F,         /* F  habala parameter */
    double Fi,        /* Fi habala parameter */
    double Jm         /* Jm habala parameter */
);
    void
MkElemVxxx(   /* function for making habala-code for element Vxxx */
    char   *elemname, /* (spice) element name */
    char   *nodebuf,  /* habala nodes-parameters list together */
    double Gi,        /* Gi habala parameter */
    double F,         /* F  habala parameter */
    double Fi,        /* Fi habala parameter */
    double Jm         /* Jm habala parameter */
);
    void
IVxxxParmFromDC(     /* get habala parameters from DC value */
    double *Gi,      /* returned Gi habala parameter */
    double *F,       /* returned F  habala parameter */
    double *Fi,      /* returned Fi habala parameter */
    double *Jm,      /* returned Jm habala parameter */
    double DCvalue   /* DC value */
);
    void
IVxxxParmFromDISTOF( /* get habala parameters from DISTOF values */
    double *Gi,      /* returned Gi habala parameter */
    double *F,       /* returned F  habala parameter */
    double *Fi,      /* returned Fi habala parameter */
    double *Jm,      /* returned Jm habala parameter */
    int    DISTOFno, /* =1 for DISTOF1, =2 for DISTOF2 */
    double value1,   /* DISTOF value #1 */
    double value2    /* DISTOF value #2 */
);
    int       /* returned TRUE if OK, FALSE if fail */
MkElemZxxx(   /* function for making habala-code for element Zxxx */
    char   *elemname,     /* (spice) element name */
    char   *nodebuf,      /* habala nodes-parameters list together */
    char   *modelname,    /* model name */
    double parmMesfetArea /* mesfet parameter AREA */
);
/*-------------------------------------------------------------------------*/
# endif /* define SHpass2_H */
