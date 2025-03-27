#define ERR_USAGE 	65
#define ERR_INPUT_OPEN	66
/*
# Harmonic Balance Simulator wrapper for Win release
# wrapper version 0.1.0  dd. 06-09-2000
# G. Serdyuk, gserdyuk@mail.ru
# GPL
*/

#define ERR_OUTPUT_OPEN	66
#define ERR_LOG_OPEN	67
#define BAD_EXIT	68
	


#define UNR_OPTION "unrecognized option -"
#define CONFIG_IS "config file is: "
#define INPUT_OPEN_U "unable to open input file: "
#define OUTPUT_OPEN_U "unable to open output file: "
#define LOG_OPEN_U "unable to open log file: "
#define TR_MSG "Translator result: \t"
#define SIM_MSG "Simulator result: \t"
#define HNR_MSG "Hnr result: \t\t"
#define CP_MSG1 "Error: file "
#define CP_MSG2 " or "
#define CP_MSG3 "failed to open\n" 
#define SIM_ERRS "simulation errors encoutered - see file: "
#define TR_ERRS "translation errors encoutered - see file: "
#define USAGE "  harmonic balance simulator wrapper\n" << \
              "  usage: hbw [-h] [-c <config file>] [-l <logfile> ] <infile> [<outfile>]\n\n" 


#define SIMULATOR_NORM 16 /* upper boundary of good result of simulator */
#define TRANSLATOR_NORM 3 /* upper boundary of good result of translator */
