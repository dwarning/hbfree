
/*
 *  S H c o n f i g . H
 *
 *  05.02.2000 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# ifndef SHconfig_H
# define SHconfig_H
/*-------------------------------------------------------------------------*/
/* default values for habala config parameters: */
# define   def_MESFET_N        1.0
# define   def_MESFET_RDS      1.E09
# define   def_MESFET_RGS      1.E09
# define   def_MESFET_UBOUND   0.52
# define   def_MESFET_STERM    0.12

/*-------------------------------------------------------------------------*/

# define   maxOPTINLINE 5   /* max options in one line in &SERVIS-command */

/*# define maxOPTLEN    10  /* max length of option name */

# define maxTITULSTRLEN  61  /* 60 chars + '\0' */
extern char strTitul[maxTITULSTRLEN];

typedef struct  { /* options */
    char   *optname;  /* option name */
    int    isint;     /* is integer value ? */
    double dblval;    /* float value, if exist */
    int    intval;    /* integer value, if exist */
    int    isset;     /* is already set ? */
    int    isserv;    /* options for '&SERVIS'-command */
} OptTab;

extern OptTab OptionsTab[];

/*-------------------------------------------------------------------------*/
    void
AnalyseConfFile( /* analyse configuration file, fill/update Habala-options table */
    void
);
/*-------------------------------------------------------------------------*/
    int              /* returned FALSE if option not exist, TRUE - if OK */
GetConfOption(       /* function for get Habala-option */
    char   *optname, /* option name */
    int    *isint,   /* TRUE if option - integer */
    double *dblval,  /* pointer to float value */
    long   *intval   /* pointer to integer value */
);
/*-------------------------------------------------------------------------*/
    int              /* returned TRUE if option already set, else - FALSE */
ChangeConfOption(    /* function for set Habala-option */
    char   *optname, /* option name */
    double dblval,   /* float value, if exist */
    long   intval    /* integer value, if exist */
);
/*-------------------------------------------------------------------------*/
    int              /* returned -1 if fail */
GetOptNo(            /* function for return option number in table by it name */
    char   *optname  /* option name */
);
/*-------------------------------------------------------------------------*/
    void
DebugPrintOptions(   /* debugging print options */
    void
);
/*-------------------------------------------------------------------------*/
    void
SaveTitul(           /* save titul line of start file or file name */
    void
);
/*-------------------------------------------------------------------------*/
    void
OutHabSERVIS(        /* generate &SERVIS-statement */
    void
);
/*-------------------------------------------------------------------------*/
# endif /* define SHconfig_H */
