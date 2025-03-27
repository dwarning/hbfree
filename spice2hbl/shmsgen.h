
/*
 *  S H m s g E N . H  --  S2H-compiler: Messages file (ENglish language)
 *
 *  29.12.1999 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

# ifndef SHmsgEN_H
# define SHmsgEN_H
/*-------------------------------------------------------------------------*/
/* headline: */

# define    sVERSION    "1.37"
/*# define  sVERSDATE   "23-Dec-2000" /* real date */
# define    sVERSDATE   __DATE__      /* build date */
# define    sPROGNAME   "S2H"
# define    sFULLNAME   "Convertor from Spice3 to HaBala language v.%s / %s\n"
# define    msgABOUT    "About program: \n" \
    "\n\t.\t.\t.\t.\t.\n\n" \
    "\tDesigned by ASB, Email: spice3man@yahoo.com, spice3man@mail.ru\n" \
    "\tKiev(UA), 2000\n"

/*-------------------------------------------------------------------------*/
/* common: */

# define    msgNO          "no"
# define    msgYES         "yes"

# define    msgPASSbegin   "Pass %d ...\n"
# define    msgPASSend     "... pass %d done.\n"
# define    msgGENend      "Code generation complete.\n"

# define    msgPASS_       "Pass %d:"
# define    msgNOERRORS    " errors not found" /*\n*/
# define    msgTOTALERRORS " total %d errors found" /*\n*/
# define    msgNOWARNS     ", no warnings.\n"
# define    msgTOTALWARNS  ", total %d warnings.\n"

# define    msgAVAILMEM    "Available dynamic memory %ld bytes.\n"

# define    msgFATALHIDDEN "", "" /* for hidden exit by MsgErrorFatal() */

/*-------------------------------------------------------------------------*/
/* for SHmain.C: */

/* command line flags: */
# define    usgDEFAULT  "default"
# define    usgUSAGE    "Usage:  %s [-flag -flag...] INPUT_FILE OUTPUT_FILE\n" \
                        "  standard flags:\n"
# define    usgFLAG_H   " -%c           - more help information %s\n"
# define    usgFLAG_C   " -%cCONF_FILE  - get configurations settings from file %s\n"
# define    usgFLAG_E   " -%cERROR_FILE - output error messages to file"\
                                                " instead of stderr %s\n"
# define    usgFLAG_P   " -%c           - pause after error if diagnostic to stderr %s\n"
# define    usgFLAG_S   " -%c           - print statistic %s\n"
# define    usgFLAG_N   " -%cNODES_FILE - rename nodes translation file %s\n"
# define    usgFLAG_M   " -%cELEMS_FILE - rename elements names translation file %s\n"
# define    usgFLAG_T   " -%cTEMP_DIR   - directory for temporary files %s\n"
# define    usgFLAG_U   " -%c           - convert input to upper case %s\n"
# define    usgFLAG_ADD "  addition flags:\n"
# define    usgFLAG_K   " -%c           - keet temporary files for debugging %s\n"
# define    usgFLAG_D   " -%cDEBUG_FILE - output debug information to file %s\n"
# define    usgFLAG_L   " -%c           - print parsed token to debug stream %s\n"
# define    usgFLAG_G   " -%c           - debug grammar %s\n"
# define    usgFLAG_1   " -%c           - show debug info for pass 1 %s\n"
# define    usgFLAG_2   " -%c           - show debug info for pass 2 %s\n"
# define    usgFLAG_A   " -%cbout       - additional info\n"
# define    usgMOREHELP \
" .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  . \n"\
" .  .  .  .  .  .  .   More help information  .  .  .  .  .  .  . \n"\
" .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  . \n"

# define    msgNOFLAGPARM   "9001", "No string parameter in flag -%c...\n"
# define    msgUNKNOWNFLAG  "Unknown flag: '%c%c'\n" /* to console only */
# define    msgNOINPFNAME   "9002", "No input file name in command line\n"
# define    msgNOOUTFNAME   "9003", "No output file name in command line\n"
# define    msgWARNCNFOPEN  "1001", "Can't open config file '%s'\n"
/* # define msgERRCNFOPEN   "9011", "Can't open config file '%s'\n" /**/
# define    msgERRINPOPEN   "9012", "Can't open input file '%s'\n"
# define    msgERRINPREOPN  "9013", "Can't reopen input file '%s'\n" /**/

# define    msgERROUTCREAT  "9014", "Can't create output file '%s'\n"
# define    msgERRFNODCREAT "9015", "Can't create nodes translation file '%s'\n"
# define    msgERRFELECREAT "9016", "Can't create elements names translation file '%s'\n"
# define    msgERRERRCREAT  "9017", "Can't create error file '%s'\n"
# define    msgERRDEBCREAT  "9018", "Can't create debug file '%s'\n"
# define    msgERRTMPCREAT  "9019", "Can't create temporary file '%s'\n"

/* debug only messages: */
# define    msgDEBUGINF     "********** Debug information **********\n"
# define    msgCNFFNAME     "Config file:  %s\n"
# define    msgINPFNAME     " Input file:  %s\n"
# define    msgOUTFNAME     "Output file:  %s\n"
# define    msgNODFNAME     "Nodes trans:  %s\n"
# define    msgELEMFNAME    "Elems trans:  %s\n"
# define    msgERRFNAME     " Error file:  %s\n"
# define    msgDEBFNAME     " Debug file:  %s\n"
# define    msgTMPDIRNAME   " Tempo dir:   %s\n"
# define    msgTMPFNAME     " Tempo file:  %s\n"

/*-------------------------------------------------------------------------*/
/* for SHiofun.C: */

/* # define msgFatalPrefix   "*FATAL*"   /**/
# define    msgFatalPrefix   "#FE"
/* # define msgErrorPrefix   "*ERROR*"   /**/
# define    msgErrorPrefix   "#ER"
/* # define msgWarningPrefix "*Warning*" /**/
# define    msgWarningPrefix "#W"

# define    msgERRCLOSEALL   "9029", "Streams close error\n"

# define    msgINPFAIL       "9025", "Read error on input file\n" /**/
# define    msgOUTFAIL       "9026", "Error on output stream '%s'\n"

/*# define  msgFILELINEPOS   " in file %s, line %d.%d: " /**/
# define    msgFILELINEPOS   "file %s, line %d, pos %d: " /**/
/*# define  msgFILELINEPOS0  " in file %s, line %d.end: " /**/
# define    msgFILELINEPOS0  "file %s, end of line %d: " /**/

# define    msgPRESSKEY      "Press any key to continue or ESC to abort ... "
# define    msgABORTBYUSER   "9911", "\n*** Terminate by user ***\n"

# define    msgERRORSONPASS1 "9901", "Errors found on pass 1\n"
# define    msgERRORSONPASS2 "9902", "Errors found on pass 2\n"

/*-------------------------------------------------------------------------*/
/* for SHconfig.C: */

# define    msgUNKNWNOPTION  "5685", "Unknown HB-option '%s' in config file detected\n"

/*-------------------------------------------------------------------------*/
/* for SHlpars.L: */

# define    msg_LINE          "  Line"/**/
# define    msg_NEWLINE       "\n---Line"/**/

/* compile messages (without \n at end !): */
# define    msgILLSYMB        "5402", "(Lex) Illegal symbol '%c'(%02XH)"/**/
# define    msgILLTOKEN       "5403", "(Lex) Illegal token '%s'"
# define    msgILLKEYWORD     "5401", "(Lex) Illegal keyword '%s'"
/*# define  msgINCFILNOTFND   "5406", "(Lex) Include file '%s' not found"/**/
# define    msgNOINCLFILE     "5404", "(Lex) Include file name is absent"
# define    msgMAXINCLNEST    "5405", "(Lex) Maximum include files nesting (%d) exceed"
# define    msgFEMAXINCLNEST  "9020", "(Lex) Maximum include files nesting (%d) exceed"

/* non-fatal error messages: */
# define    msgERRINCLOPEN    "4010", \
                    "(Lex) Can't open file '%s' in .include at line %d (%s)\n"
# define    msgERRINCLRETN    "5407", "(Lex) Can't return from include file to file '%s'\n"

/* fatal: */
# define    msgERRMEMALLOC    "9099", "(Lex) Memory allocation error\n"

/* debug only messages: */
# define    msgYYLEXSTART     "--------------- start yylex() --------------- "
# define    msgYYLEXEOF       "! yylex() not found input and return 0(EOF) !\n"
# define    msgFINDTOKEN      "find token"
# define    msgLINETITLE      "<title line>"
# define    msgLINEFIRST      "<first line of include file - not title>"/**/
# define    msgLINEAFTERCOMNT "<after comment>"
# define    msgLINECONTSTMT   "<continue statement>"
# define    msgLINENEXTSTMT   "<next statement>"
# define    msgLINECOMMENT    "<continue C-comment>"
# define    msgFNDENDOFSTMT   "find END_OF_STATEMENT"
# define    msgFULLSTRatBOL   "[__ string at BOL]"
# define    msgTRANSTRatBOL   "[truncated string at BOL]"
# define    msgCODECONTINUE   "[+ mark of continue statement...]"
# define    msgCOMMENTbegin   "[begin of COMMENT...]"
# define    msgCOMMENTbody    "[...COMMENT body...]"
# define    msgCOMMENTend     "...end of COMMENT."
# define    msgCONTINUESTMT   "[continue statement...]"
# define    msgUNRECOGNSYM    "unrecognized symbol"
# define    msgEXTRACTSUFFIX  "\t\t\t\textract suffix = '%s'\n"
# define    msgNOTSCALESUFX   "\t\t\t\tsuffix '%s' not include scale multiplier\n"
# define    msgSCALEQUAL      "\t\t\t\tsuffix contain scale multiplier = %g\n"
# define    msgRETformINCL    "\t<EOF>RETURN from INCLUDE-file '%s'\n"
# define    msgINCLFAIL       "\t??? INCLUDE fail !\n"
# define    msgPUSHSTACK      "\t\t\t\t### PushStack ( file '%s', line %d )\n"
# define    msgPOPSTACK       "\t\t\t\t### PopStack ( file '%s' ) -> line %d\n"
# define    msgNESTOFINCL     "\t\t\t\t\t\t-!- Nesting of include = %d\n"
# define    msgPUSHFAIL       "\t\t\t\t??? Push file in stack fail !\n"
# define    msgYYWRAP_1       "\t\t### yywrap() prepare to return TRUE: "
# define    msgSKIPRESTLINE   "... skip rest of line: "

/*-------------------------------------------------------------------------*/
/* for SHpars.Y: */

# define    msgPASSCOMPLETE   "\n... pass %d complete.\n"

/* fatal/exit messages (must end with '\n'): */
/*# define  msgERRINPROG      "5xxx", "Unrecognized error in Program syntax\n"/**/
# define    msgFAILADDNODE    "9021", "Error on add to node table\n"
# define    msgERR_SUBCKTCNT  "4040", \
    "Number of .SUBCKT (%d) not equal to number of .ENDS (%d)\n"
# define    msgFERR_ENDNOTFND "9022", "Not found .END in program\n"
# define    msgERR_ENDNOTFND  "4020", "Not found .END in program\n"
# define    msgPARSEREXIT     "9023", "Parsing exit on unrecovering error\n"

/* non-fatal error messages (for MsgCompile() - without end '\n'): */
# define    msgALLOWKEYWORD   "5001", "Illegal keyword %s instead of "/*\n\t...*/
# define    msgILLOPTION      "5591", "Illegal option '%s'"
# define    msgILLFUNCTION    "5xxx", "Illegal function %s()"
# define    msgEXPECTFUNNAME  "5590", \
    "Illegal function %s(), with this numbers of parameters must be "
# define    msgNOTRESERVKEY   "5699", "Keyword '%s' is not reserved"
# define    msgTOOFEWPARM     "5595", "Too few parameters in option '%s'"
# define    msgTOOMUCHPARM    "5594", "Too much parameters in option '%s'"
# define    msgERRAREXPR      "5532", "Error in arithmetic expression"
# define    msgUNCLOSEDAREXPR "5531", "Unclosed arithmetic expression"
/*# define  msgALLOWFUNCTION  "5xxx", "Illegal function %s() instead of "/*\n\t...*/
/*# define  msgALLOWVOLTCURR1 "5xxx", "Illegal %s(_) instead of "/*\n\t...*/
# define    msgALLOW_V_C_FUN  "5535", "Illegal voltage/current/function %s() instead of "/*\n\t...*/
# define    msgALLOWVOLTCURR2 "5536", "Illegal %s(_,_) instead of "/*\n\t...*/

# define    msgERRSTMT        "5002", "Error statement"
# define    msgERRINDUCTOR    "5501", "Error in Inductor '%s'"
# define    msgERRCOUPINDUC   "5502", "Error in Coupled Inductor '%s'"
# define    msgNEEDINDUCTOR   "5503", "Need Inductor instead of '%s'"
# define    msgERRRESISTOR    "5504", "Error in Resistor '%s'"
# define    msgERRCAPACITOR   "5505", "Error in Capacitor '%s'"
# define    msgERRVOLTAGE     "5506", "Error in Voltage '%s'"
# define    msgERRCURRENT     "5507", "Error in Current '%s'"
# define    msgERRLVCVS       "5521", \
    "Error in Linear Voltage-Controlled Voltage Source '%s'"
# define    msgERRLVCCS       "5522", \
    "Error in Linear Voltage-Controlled Current Source '%s'"
# define    msgERRLCCCS       "5523", \
    "Error in Linear Current-Controlled Current Source '%s'"
# define    msgERRLCCVS       "5524", \
    "Error in Linear Current-Controlled Voltage Source '%s'"
# define    msgERRNLDS        "5525", "Error in Non-linear Dependent Source '%s'"
/*# define  msgLEFTBRKREQ     "5534", "'(' required"/**/
# define    msgRIGHTBRKREQ    "5533", "')' required"
# define    msgERRVSWITCH     "5541", "Error in Vswitch '%s'"
# define    msgERRISWITCH     "5542", "Error in Iswitch '%s'"
# define    msgNEEDVOLTAGE    "5592", "Need Voltage instead of '%s'"
# define    msgNEEDVOLTCURR   "5593", "Need Voltage or Currency instead of '%s'"
# define    msgERRLLTLINE     "5543", "Error in Lossless Transmission Line '%s'"
# define    msgERRLTLINE      "5544", "Error in Lossy Transmission Lines '%s'"
# define    msgERRUDRCLINE    "5545", \
    "Error in Uniform Distributed RC Line statement '%s'"
# define    msgERRDIODE       "5551", "Error in Diode '%s'"
# define    msgERRTRANSISTOR  "5553", "Error in Bipolar Junction Trasistor '%s'"
# define    msgERRJFET        "5554", "Error in JFET '%s'"
# define    msgERRMESFET      "5555", "Error in MESFET '%s'"
# define    msgERRMOSFET      "5556", "Error in MOSFET '%s'"

# define    msgERR_MODEL      "5003", "Error in .MODEL statement"
# define    msgMODELPARMEXIST "5004", "Parameter '%s' already defined in model '%s'"

# define    msgERR_OPTIONS    "5605", "Error in .OPTIONS controlline statement"
# define    msgERR_CONTROL    "5601", "Error in .CONTROL commands"
# define    msgERR_NODESET    "5606", "Error in .NODESET controlline statement"
# define    msgERR_IC         "5607", "Error in .IC controlline statement"
# define    msgERR_AC         "5608", "Error in .AC controlline statement"
# define    msgERR_DC         "5609", "Error in .DC controlline statement"
# define    msgERR_DISTO      "5610", "Error in .DISTO controlline statement"
# define    msgERR_NOISE      "5611", "Error in .NOISE controlline statement"
# define    msgERR_OP         "5612", "Error in .OP controlline statement"
# define    msgERR_PZ         "5613", "Error in .PZ controlline statement"
# define    msgERR_SENS       "5614", "Error in .SENS controlline statement"
# define    msgERR_TF         "5615", "Error in .TF controlline statement"
# define    msgERR_TRAN       "5616", "Error in .TRAN controlline statement"
# define    msgERR_TLINE      "5617", "Error in .TLINE controlline statement"
# define    msgERR_SAVE       "5631", "Error in .SAVE statement"
# define    msgERR_PRINT      "5632", "Error in .PRINT statement"
# define    msgERR_PLOT       "5633", "Error in .PLOT statement"
# define    msgERR_FOUR       "5634", "Error in .FOUR statement"
# define    msgILLPRNTYPE     "5537", "Illegal print type %s instead of "
# define    msgERR_WIDTH      "5618", "Error in .WIDTH statement"

# define    msgERR_SUBCKT     "5005", "Error in subcircuit statement"
# define    msgERR_ENDS       "5007", "Fail in processing .ends for subcirc '%s'"
# define    msgCIRCNOTIN1FILE "5008", \
    "Circuit .ends for '%s' must be in same file with .subckt"
# define    msgCIRCDEFEXIST   "5006", \
    "Subcircuit with name '%s' already defined inside circuit '%s'"
# define    msgERR_SUBCKTSTMT "5010", "Error in subcircuit internal statements"
# define    msgERRSUBCALL     "5011", "Error in call subcircuit '%s'"
# define    msgSUBCKTNOTEXIST "5012", \
    "Definition for called subcircuit '%s' not found in current circuit or in parents"
# define    msgSUBCIRCPARM0   "5009", "Subcircuit parameter must not be node '0'"
# define    msgCNTPARMSUBCAL  "5013", \
    "Must be %ld parms in call '%s' of '%s' instead of %ld"
# define    msgPARMSETSUBCAL  "5014", "Error in substitute parameters for subcall '%s'"

# define    msgERR_HB         "5681", "Error in .HB statement"
# define    msgHBALREADYEXIST "5682", \
    "Statement .HB already exist - doubling unallowable"
# define    msgERR_HB_OPTIONS "5683", "Error in .HB_OPTIONS statement"
# define    msgUNKNWN_HB_OPT  "5684", "Unknown option '%s' in .HB_OPTIONS"

# define    msgERR_LET        "5602", "Error in .LET statement"
# define    msgERR_ALTER      "5603", "Statement .ALTER not support"
# define    msgERR_WRITE      "5604", "Statement .WRITE not support"

/*# define  msgNEED_END       "4030", "Need .END at the end of file" /*old*/
# define    msgNEED_END       "4030", \
    "Invalid token at the begin of statement or missing .END at the end of file"

# define    msgSHOWNODESPARM  "--------- total number of nodes-parameters in subcircuit = %ld\n"
# define    msgRENNODESLOCAL  "--------- total number of renamed local nodes in subcircuit = %ld\n"

/* pass2: code generation: */
# define    msgPARMALREADYDEF "5597", "Parameter '%s' already defined"
# define    msgPARMILLVALUE   "2091", "Illegal value %g for parameter '%s'"
# define    msgERPARMILLVALUE "5596", "Illegal value %g for parameter '%s'"
# define    msgNOTGENILLPARM  "2092", \
    "Can't generate code for element '%s' because of error in parameter(s)"

# define    msgNOTGENELEM     "2099", "Can't generate code for element '%s'"

# define    msgMODELNOTFOUND  "5204", "Model '%s' not found"
# define    msgILLMODELTYPE   "5205", \
    "Model '%s' used in element '%s' have illegal type '%s'"

# define    msgAREA           "AREA"
# define    msgDIODAREAAGAIN  "5552", "Diode option AREA repeat again"
# define    msgNOTEQPARMVJ    "5206", \
    "Model parameter VJ=%g must be equal to same parameter VJ=%g for previous diodes Schottky"

/* pass2: warnings for not processed elements (without '\n'): */
# define    msgNotProcElement "5xxx", "Elements of this type (%s) not support"
# define    msgNotProcSemiR   "2010", "Semi-resistors (%s) not support"
# define    msgNotProcSemiC   "2020", "Semi-capacitor (%s) not support"

/* MESFET: */
# define    msgZXXX_ILLAREA   "2501", \
    "Illegal mesfet option AREA '%g', use default '%g'"
# define    msgZXXX_NOTAREA   "2502", "Mesfet option AREA not found, use default '%g'"
# define    msgMESFET_N_mb_ne0       "2511", \
    "For calculation parameter MESFET_N must be not equal 0"
# define    msgMESFET_RDS_mb_ne0     "2512", \
    "For calculation parameter MESFET_RDS must be not equal 0"
# define    msgMESFET_RGS_mb_ne0     "2513", \
    "For calculation parameter MESFET_RGS must be not equal 0"
# define    msgMESFET_UBOUND_mb_ne0  "2514", \
    "For calculation parameter MESFET_UBOUND must be not equal 0"
# define    msgMESFET_STERM_mb_ne0   "2515", \
    "For calculation parameter MESFET_STERM must be not equal 0"

/* all debug messages - in SHpars.Y */

/*-------------------------------------------------------------------------*/
/* for SHpass1.C: */

/* fatal: */
# define    msgERRMEMALLOC1    "9199", "(Pass1) Memory allocation error\n"
# define    msgRENELEMFAIL     "9101", \
    "Rename elements fail: too many elements / subcircuits\n"
# define    msgRENNODESFAIL    "9102", \
    "Rename nodes fail: total number of nodes in all circuits exceed limit\n"
# define    msgCIRCNAMETOOLONG "9103", \
    "Full circuit name for '%s' is too long\n"

/* errors: */
# define    msgMainCircuit    "(main)"  /* "(main circuit)" */
# define    msgDEFCIRCEXIST   "5102", \
    "Definition of subcircuit '%s' already exist in circuit '%s'"

/* msg: */
# define    msgSTATHEADER     "*** Pass 1 statistic ***\n"
# define    msgNODENAMEREF    \
    "%4ld: node '%s'  \t new name '%-4s' \t(%ld references):\n"
# define    msgNODETYPEFREE   "               \t"
# define    msgNODETYPE       "      type %s\t"
# define    msgNODEITEM       " in element '%s'\t(file '%s', line %ld)\n"

# define    msgCNTDEFSELEM    \
    "Total definitions of elements with type '%s'\t= %ld\n"

# define    msgELEMLIST4CIRC  \
    "=============== Elements list for circuit "
# define    msgELEMLISTEMPTY  \
    "----- List of elements with type '%s' is empty ------------------------\n"
# define    msgELEMLISTPRN    \
    "----- Backward list of elements with type '%s' (total %ld elements): ---\n"
/* # define msgELEMDEBPRN     "%4ld: '%s':\t'%s'\t  =>\t'%s'\t(file '%s', line %ld)\n" /**/
# define    msgELEMDEBPRN     "%4ld: '%s'\t=> '%4s' (file '%s', line %ld)\tin '%s'= "
# define    msgELEMNAMEEXIST  "5101", "Element with name '%s' already exist in this circuit" /* no '\n' */

# define    msgTABLENODESMAIN "--- Main circuit nodes table -----------------\n"
# define    msgCNTNODESMAIN   "Total number of nodes in main circuit \t\t= %ld\n"

# define    msgCIRCLISTEMPTY  \
    "----- List of subcircuits is empty ------------------------------------\n"
# define    msgCIRCLISTPRN    \
    "----- Backward list of subcircuits (total %ld subcircuits): ------------\n"
# define    msgCIRCDEBPRN     \
    "=======\t at %p subcirc definition:\n" \
    "%5ld) '%s', parent='%s'\t(begin='%s':%ld, end='%s':%ld)\n"
# define    msgFULLCIRCNAME   "Full circuit name:\t"
# define    msgFULLPARENTNAME "Full parent name:\t"
# define    msgCNTSUBCIRC     "Total number of subcircuits \t\t\t= %ld\n"
# define    msgMainCirc       "(maincirc)"
# define    msgCNTNODESPARM   " -------- total number of nodes-parameters in subcircuit = %ld\n"
# define    msgCNTNODESLOCAL  "  ------- total number of local nodes in subcircuit = %ld\n"

# define    msgCNTMODELS      "Total number of models in all circuits \t\t= %ld\n"
# define    msgModelNameType  "Model '%s' type '%s' in circuit "
# define    msgModParm        "   parameter:"
# define    msgModParmName    "\tname = '%s'"
# define    msgModParmVal     "\tvalue = %g"

# define    msgCircMainSign   "/"
# define    msgCircSeparator  "/"

# define    msgHDRNODESFILE   \
    "********* Nodes translation file *********\n"\
    "* node name    path of  elem define in\n"\
    "*Hab <- Spice  subcall  spice-circuit\n"\
    "*-----------------------------------------\n"
/*# define  msgNODETRANSLINE  "%-4s\t%s\t" /*+%s\n*/
# define    msgNODETRANSLINE  "%-s\t%s\t"  /*+OutCallPath(),OutFullCircName()*/

# define    msgHDRELEMSFILE   \
    "***** Elements names translation file *****\n"\
    "* elem name    path of  node define in\n"\
    "*Hab <- Spice  subcall  spice-circuit\n"\
    "*------------------------------------------\n"
/*# define  msgELEMTRANSLINE  "%-4s\t%s\t" /*+%s\n*/
# define    msgELEMTRANSLINE  "%-s\t%s\t"  /*+OutCallPath(),OutFullCircName()*/

/*-------------------------------------------------------------------------*/
/* for SHpass2.C: */

/* msg: */
# define    msgCircState      " circuit state:" /* for debug only */
# define    msgP2TOTALELEM    \
    "(Pass2) Total %d element(s) of habala type '%s' found\n"
# define    msgP2FREQPART     "(Pass2) &FREQuency part\n"

/* code generation messages (for check model parameters macros): */
# define    msgMODPARMNOTFND    "5202", \
    "Parameter '%s' not found in model '%s' for element '%s'"
# define    msgPARMNEEDVALUE    "5201", \
    "Parameter '%s' in model '%s' for element '%s' need value"
# define    msgMODLPARMILLVAL   "5203", \
    "Parameter '%s' in model '%s' for element '%s' have illegal value %g"
# define    msgPARMVALSETDEF    "2001", \
    "For element '%s' value of parameter '%s' in model '%s' set to default value %g"
# define    msgPARMNOVALSETDEF  "2002", \
    "Parameter '%s' in model '%s' for element '%s' need value, set to default %g"
# define    msgPARMILLVALSETDEF "2003", \
    "Parameter '%s' in model '%s' for element '%s' have illegal value %g, set to default %g"

/* fatal: */
# define    msgERRMEMALLOC2   "9299", "(Pass2) Memory allocation error\n"
# define    msgERRSUBCIRPLACE "9201", \
    "(Pass2) Subsircuit '%s' occupy two files '%s', '%s'\n"
# define    msgERRCIRCOPEN    "9202", \
    "(Pass2) Open error file '%s' with subcircuit '%s' body\n"
/* # define msgERRCIRCFAIL    "9203", \
    "(Pass2) Read error file '%s' with subcircuit '%s' body\n" /**/

/* output: */
# define    msgCIRCFILEHEAD   \
    ";*** Body of circuit '%s' from file '%s', lines %ld..%ld\n"
# define    msgEMPTY          "(empty)"

/*-------------------------------------------------------------------------*/
/* for SHconfig.C: */

# define    msgHABOPTTAB      "*** Habala-options table:\n"

/*-------------------------------------------------------------------------*/
# endif /* define SHmsgEN_H */
