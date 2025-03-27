
/*
 *  S H p a s s 1 . H
 *
 *  03.01.2000 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# ifndef SHpass1_H
# define SHpass1_H

/*=========================================================================*/
/* range, step of rename counters for elements: */
# define startRENCNT      1   /*999*/
# define  stopRENCNT    999   /*001*/
# define   incRENCNT      1   /* -1*/

/* range, step of rename counters for nodes: */
# define startRENNODES    1
# define  stopRENNODES 9999   /* max lenHABNAME=4 digits ! */
# define   incRENNODES    1

/*-------------------------------------------------------------------------*/
/* types of nodes: */
extern char nodnameGND[];   /* name of GND-node in Spice */
extern char nodtypUNKNOWN[];
extern char nodtypPLUS[];
extern char nodtypMINUS[];
extern char nodtypCTRLPLUS[];
extern char nodtypCTRLMINUS[];
extern char nodtypBjtC[];
extern char nodtypBjtB[];
extern char nodtypBjtE[];
extern char nodtypBjtS[];
extern char nodtypFetD[];
extern char nodtypFetG[];
extern char nodtypFetS[];
extern char nodtypFetB[];
extern char nodtypSubCirc[];
/*=========================================================================*/
typedef struct  nodeitem {  /* list of items - (network element) wich contain node */
       char     *nodetype;  /* type of node (optional) */
       char     *elemname;  /* network element name wich contain node */
       char     *filename;  /* source file name */
       long     linenum;    /* line number in this file */
struct nodeitem *nextitem;  /* pointer to next item */
} NodeItem;
/*-------------------------------------------------------------------------*/
typedef struct node {       /* network nodes list */
    char        *nodename;  /* node name */
    char        newname[lenHABNAME0]; /* new node Habala-name: 4? symbols + '\0' */
    long        cntref;     /* counter of references to this node */
    NodeItem    *itemslist; /* pointer to list of elements wich contain node */
    NodeItem    *itemstail; /* pointer to tail of elements list */
    struct node *nextnode;  /* pointer to next node */
} Node;
/*-------------------------------------------------------------------------*/
typedef struct filelist {   /* files list */
    char        *filename;  /* file name */
struct filelist *nextfile;  /* pointer to next file */
} FileList;
/*-------------------------------------------------------------------------*/

typedef struct subcirc {      /* subcircuits list */

       char     *circname;    /* subcircuit name */
struct subcirc  *parent;      /* pointer to parent circuit or NULL if in main circuit */

       long     cntnodesparm; /* counter of local nodes-parameters */
       Node     *nodeparmlist;/* list of local nodes-parameters */
       Node     *nodeparmtail;/* tail of list of local nodes-parameters */

       long     cntnodes;     /* counter of local elements nodes */
       Node     *circnodlist; /* list of all local elements nodes */
       Node     *circnodtail; /* tail of list of all local nodes */

       char     *begfname;    /* subcircuit begin at file name */
       long     begline;      /* subcircuit begin at line number in this file */
       char     *endfname;    /* subcircuit end at file name */
       long     endline;      /* subcircuit end at line number in this file */

struct subcirc  *nextcirc;    /* pointer to next circuit */
} SubCirc;

/*-------------------------------------------------------------------------*/
typedef struct elemlist {   /* elements list */
       char     *elemname;  /* element name */
       char     newname[lenHABNAME0]; /* new Habala-name: 4 symbols + '\0' */
       SubCirc  *parent;    /* pointer to parent circuit or NULL if in main circuit */
       char     *filename;  /* source file name */
       long     linenum;    /* line number in this file */
struct elemlist *nextelem;  /* pointer to next element */
} ElemList;
/*-------------------------------------------------------------------------*/
typedef struct  stackcirc {     /* stack of subcircuits */
/*      int       issubcall;    /* for save isSubCall */
/*      int       levelsubcall; /* for save levelSubCall */
        SubCirc   *circcall;    /* for save circCall */
/*      int       issubdefbody; /* for save isSubDefBody */
/*      int       levelsubdef;  /* for save levelSubDef */
        SubCirc   *circdefcurr; /* for save circDefCurrent */
/*      SubCirc   *circ;        /* pointer to current subcircuit == circDefCurrent */
 struct stackcirc *prev;        /* previous element of stack */
} StackCirc;
/*-------------------------------------------------------------------------*/
typedef struct modparmlist {     /* model parameters list */
        char        *parmname;   /* parameter name */
        int         valueexist;  /* = TRUE if parameter have value */
        double      value;       /* parameter value */
 struct modparmlist *nextparm;   /* pointer to next parameter */
} ModParmList;
typedef struct modellist {       /* models list */
        char        *modelname;  /* model name */
        char        *modeltype;  /* model type */
        SubCirc     *circuit;    /* circuit in wich define model */
        ModParmList *parameters; /* pointer to model parameters list */
 struct modellist   *nextmodel;  /* pointer to next model */
} ModelList;
/*=========================================================================*/
/* counters: */
extern long  cntNodesMain; /* nodes in main circuit */
extern long  cntSubCirc;   /* subcircuits */
extern long  cntNodesTmp;  /* temporary nodes count for AddNodeInfo() */
extern long  cntModels;    /* models definitions counter */
extern long  renNodes;     /* rename counter for nodes */
/* . . . */

/* pointers to lists: */
extern FileList  *listFiles;     /* list of input files names */
extern Node      *listNodesMain; /* list of nodes in main circuit */
extern Node      *tailNodesMain; /* tail of nodes list in main circuit */
extern Node      *listNodesTmp;  /* temporary list of nodes for AddNodeInfo() */
extern SubCirc   *listSubCirc;
extern ModelList *listModels;    /* list of all models */
/* . . . */

/* counters of elements: */
extern long  cntA; /*-*/
extern long  cntB;
extern long  cntC;
extern long  cntD;
extern long  cntE;
extern long  cntF;
extern long  cntG;
extern long  cntH;
extern long  cntI;
extern long  cntJ;
extern long  cntK;
extern long  cntL;
extern long  cntM;
extern long  cntN; /*-*/
extern long  cntO;
extern long  cntP; /*-*/
extern long  cntQ;
extern long  cntR;
extern long  cntS;
extern long  cntT;
extern long  cntU;
extern long  cntV;
extern long  cntW;
extern long  cntX;
extern long  cntY; /*-*/
extern long  cntZ;

/* rename counters for elements:  */
extern int   renA; /*-*/
extern int   renB;
extern int   renC;
extern int   renD;
extern int   renE;
extern int   renF;
extern int   renG;
extern int   renH;
extern int   renI;
extern int   renJ;
extern int   renK;
extern int   renL;
extern int   renM;
extern int   renN; /*-*/
extern int   renO;
extern int   renP; /*-*/
extern int   renQ;
extern int   renR;
extern int   renS;
extern int   renT;
extern int   renU;
extern int   renV;
extern int   renW;
extern int   renX;
extern int   renY; /*-*/
extern int   renZ;

/* pointers to lists of elements: */
extern ElemList *listA; /*-*/
extern ElemList *listB;
extern ElemList *listC;
extern ElemList *listD;
extern ElemList *listE;
extern ElemList *listF;
extern ElemList *listG;
extern ElemList *listH;
extern ElemList *listI;
extern ElemList *listJ;
extern ElemList *listK;
extern ElemList *listL;
extern ElemList *listM;
extern ElemList *listN; /*-*/
extern ElemList *listO;
extern ElemList *listP; /*-*/
extern ElemList *listQ;
extern ElemList *listR;
extern ElemList *listS;
extern ElemList *listT;
extern ElemList *listU;
extern ElemList *listV;
extern ElemList *listW;
extern ElemList *listX;
extern ElemList *listY; /*-*/
extern ElemList *listZ;

/*=========================================================================*/
    int  Pass1( void ); /* 1st pass */
/*-------------------------------------------------------------------------*/
    void
ClrNodesName(       /* clear Habala-nodes names, for node '0' set H-name '0   ' */
    Node *listnodes /* list of nodes */
);
    void
RenameNodes(        /* rename nodes according to Habala rules */
    Node *listnodes /* list of nodes */
);
    void
PrintNodesList(     /* debug print nodes list */
    Node *listnodes /* list of nodes */
);
/*-------------------------------------------------------------------------*/
    void
DebugPrintElemLists(    /* debugging print all elements lists */
    SubCirc  *circuit   /* for 'circuit', if NULL - for all circuits */
);
/*-------------------------------------------------------------------------*/
    void
RenameAllElems(         /* rename all elements in circuit */
    SubCirc *circuit
);
/*-------------------------------------------------------------------------*/
    int                 /* returned updated rename counter renCntElem */
Rename1elem(            /* function for rename _single_ elements in list */
    char     *spicename,/* original name of element */
    ElemList *listElem, /* pointer to elements list or it part */
    SubCirc  *circuit,  /* from wich subcirc elements */
    int      renCntElem /* rename counter */
);
/*-------------------------------------------------------------------------*/
    Node *          /* returned pointer to node or NULL if not found */
FindNode(	    /* function to search node in list */
    Node *listnodes,/* list of nodes */
    char *nodename  /* node name */
);
/*-------------------------------------------------------------------------*/
    Node * /* returned pointer to tail (last node) or NULL if fail or -1 if not processing */
AddNodeInfo(          /* function to add information about network node */
    Node *listnodes,  /* list of nodes */
    Node *tailnodes,  /* tail of nodes (last element) */
    long cntnodes,    /* counter of nodes */
    char *nodename,   /* node name */
    char *nodetype,   /* type of node (optional) */
    char *elemname    /* network element name wich contain node */
);
/*-------------------------------------------------------------------------*/
    ElemList *          /* returned (updated if OK) pointer to elements list */
AddElem(                /* function for add new element to list */
    ElemList *listElem, /* pointer to elements list */
    long     *counter,  /* pointer to counter of elements */
    char     *elemname, /* spice-name of element */
    SubCirc  *parent    /* pointer to parent circuit or NULL if in main circuit */
);
/*-------------------------------------------------------------------------*/
    char *          /* returned habala-name of node */
GetHabNodeName(     /* function */
    Node *listnodes,/* list of nodes */
    char *spicename /* from spice-name of node */
);
/*-------------------------------------------------------------------------*/
    char *               /* returned habala-name of element or NULL if not found */
GetHabElemName(          /* function for convert spice- to habala-name of element */
    char     *spicename, /* spice-name of element */
    SubCirc  *circ,      /* pointer to circuit, = NULL for main circuit */
    ElemList *listElem   /* pointer to elements list */
);
/*-------------------------------------------------------------------------*/
    void
HabELEMout(           /* generate Habala-operator '&ELEM' */
    char *HBname,     /* habala-name of element */
    char *HBnodelist, /* string-list of habala nodes */
    char *PAR,        /* PAR=... */
    char *IDOP,       /* IDOP=... */
    char *ISTR        /* ISTR=... */
);
/*-------------------------------------------------------------------------*/
    int           /* returned TRUE if OK */
PushCircStack(    /* function for save current subcircuit states in stack */
    void
 /* SubCirc *circ /* pointer to current subcircuit = circDefCurrent */
);
    SubCirc * /* returned current circuit from stack, NULL if main (1st pushed) */
PopCircStack( /* function for restore circuit states from stack */
    void
);
/*-------------------------------------------------------------------------*/
    SubCirc *          /* returned new pointer to circuits list */
AddSubCirc(            /* function for add to list info about circuit definition */
    SubCirc *circlist, /* circuits list */
    SubCirc *parent,   /* pointer to parent circuit or NULL if in main circuit */
    char    *circname  /* circuit name name */
);
    int                /* returned TRUE if OK or FALSE if not found */
AddSubCircEnd(         /* function for add to list info about end of circuit definition */
    SubCirc *circlist, /* circuits list */
    SubCirc *parent,   /* pointer to parent circuit or NULL if in main circuit */
    char    *circname  /* circuit name name */
);
    SubCirc *          /* returned pointer to find circuit or NULL if not found */
FindSubCirc(           /* function for find circ definition inside 'currcirc' */
    SubCirc *circlist, /* circuits list */
    SubCirc *currcirc, /* pointer to current circuit or NULL if in main circuit */
    char    *circname  /* circuit name */
);
    SubCirc *          /* returned pointer to find circuit or NULL if not found */
FindSubCircHigh(       /* function for find circdef inside 'currcirc' and parents */
    SubCirc *circlist, /* circuits list */
    SubCirc *currcirc, /* pointer to current circuit or NULL if in main circuit */
    char    *circname  /* circuit name */
);
/*-------------------------------------------------------------------------*/
    void
PrintNodesTransl(      /* print nodes translation */
    SubCirc *circ,     /* pointer to circuit, = NULL for main circuit */
    Node    *listnodes /* list of nodes */
);
    void
PrintElemsTransl(      /* print element name translation line */
    char     *elname,  /* element name */
    SubCirc  *circ,    /* pointer to circuit, = NULL for main circuit */
    ElemList *ellist   /* pointer to elements list */
);
    void
OutFullCircName(   /* print full circuit name */
    FILE    *ofil, /* file for out */
    SubCirc *circ  /* pointer to circuit, = NULL for main circuit */
);
/*-------------------------------------------------------------------------*/
    ModelList * /* returned pointer to model in list or NULL if not found */
FindModel(      /* function for find model in list */
    char    *modelname, /* model name */
    SubCirc *circuit    /* circuit contain model definition */
);
    ModelList * /* returned pointer to head of models list / NULL if exist / exit if fail */
AddToModelList( /* function for add model to list of models */
    char    *modelname, /* model name */
    char    *modeltype, /* model type */
    SubCirc *circuit    /* circuit contain model definition */
);
    int       /* returned TRUE if OK, exit if fail */
AddModelParm( /* function for add model parameter to model definition in list */
    ModelList *model,     /* model */
    char      *parmname,  /* parameter name */
    int       valueexist, /* = TRUE if parameter have value */
    double    value       /* parameter value */
);
    int        /* returned TRUE if found, FALSE if not found */
FindModelParm( /* function for find model parameter of model definition in list */
    ModelList *model,    /* model */
    char      *parmname  /* parameter name */
);
    int        /* returned TRUE if found, FALSE if not found */
GetModelParm(  /* function for get model parameter */
    ModelList *model,     /* model */
    char      *parmname,  /* parameter name, case ignored(!) */
    double    *parmvalue, /* pointer to parameter value */
    int       *valueexist /* = TRUE if parameter have value */
);
    ModelList *     /* returned pointer to subcirc or NULL if not found */
UpFindModel( /* model find function in current circ 'circDefCurrent' and higher */
    char *modelname /* model name */
);
/*-------------------------------------------------------------------------*/
    void
OutHabCIRCOM(        /* generate &CIRCOM-statement */
    void
);
/*=========================================================================*/
# endif /* define SHpass1_H */
