
/*
 *  S H e x c o d e . H  --  S2H-compiler:  Exit error codes
 *
 *  11.03.2000 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

/*-------------------------------------------------------------------------*/
# define    maxERRCOUNT   15  /* for new exit-codes processing */
/*# define  maxERRCOUNT  100  /* in DOS/WIN positive exit code - in range 1..127, */
                                                   /* exit code > 128 - negative ! */

/*-------------------------------------------------------------------------*/
/* exit codes: */
# define    exitOK              0       /* all OK, no errors in input */

# define    exitERRORSONPASS1	(-101)  /* errors found on pass 1 */
# define    exitERRORSONPASS2	(-102)  /* errors found on pass 2 */

# define    exitUSAGE           (-1)    /* process with flag '-?' or without cmdline */
# define    exitNOINPFNAME      (-2)    /* no input file in command line */
# define    exitNOOUTFNAME      (-3)    /* no output file in command line */
# define    exitNOFLAGPARM      (-4)    /* no string parameter in flag -xXXX */
# define    exitERRCNFOPEN      (-10)   /* can't open config file */
# define    exitERRINPOPEN      (-11)   /* can't open input file */
# define    exitERRINPREOPN     (-12)   /* can't (re)open input file */

#ifdef  INCL_NEST_EXCEED_IS_FATAL
# define    exitMAXINCLNEST     (-13)   /* max include-file nest exceed */
#endif

/* # define exitERRINCLOPEN     (-14)/* not use: can't open include-file */
/* # define exitERRINCLRETN     (-15)/* not use: error of return from include-file */

# define    exitERROUTCREAT     (-16)   /* can't create output file */
# define    exitERRFNODCREAT    (-17)   /* can't create nodes translation file */
# define    exitERRFELECREAT    (-18)   /* can't create elements names translation file */
# define    exitERRERRCREAT     (-19)   /* can't create error file */
# define    exitERRDEBCREAT     (-20)   /* can't create debug file */
# define    exitERRTMPCREAT     (-21)   /* can't create temporary file */

# define    exitINPFAIL         (-22)   /* read error on input file */
# define    exitOUTFAIL         (-23)   /* error on output stream */
/* # define exitTMPFAIL         (-24)/* not use: error on temp file */
/* # define exitDEBFAIL         (-25)/* not use: error on debug file */

/* # define exitERRCLOSEALL     (-30)/* not use: error on close files */

# define    exitFAILADDNODE     (-40)   /* pass1: fail to add node in list */
# define    exitRENELEMFAIL     (-41)   /* rename elements fail (counter out of bound) */
# define    exitRENNODESFAIL    (-42)   /* rename nodes fail (counter out of bound) */

/* # define exitFAILPASS2       (-51)   /* not use: fail in pass2 */
# define    exitERRSUBCIRPLACE  (-52)/* old: subcircuit occupy more than one file */
# define    exitERRCIRCOPEN     (-53)/* old: can't create circuit file */
/* # define exitERRCIRCFAIL     (-54)/* old,not use: error on output to circuit file */
# define    exitCIRCNAMETOOLONG (-55)   /* full circuit name is too long */

/* # define exitERRINPROG       (-90)/* not use: unrecognized error in program syntax */

/* # define exitERR_SUBCKTCNT   (-91)/* not use: number of .SUBCKT != number of .ENDS */
/* # define exitERR_ENDNOTFND   (-92)/* not use: not found .END in program */

# define    exitERR_ENDNOTFND   (-90)   /* .END not found in input */
# define    exitPARSEREXIT      (-96)   /* parsing exit on unrecovering error */

# define    exitABORTBYUSER     (-98)   /* error diagnostic interrupted by user */
# define    exitERRNOMEM        (-99)   /* error on allocate memory in all passes */

/*-------------------------------------------------------------------------*/
