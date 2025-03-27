/*
# Harmonic Balance Simulator wrapper for Win release
# wrapper version 0.1.0  dd. 06-09-2000
# G. Serdyuk, gserdyuk@mail.ru
# GPL
*/
#define mkstemp mktemp // for cygwin

#include <unistd.h>

#include <iostream.h>
#include <fstream.h>
#include <strstream.h>

#include <stdio.h>
#include <stdlib.h>

#include "err.h"

int usage(void);

#define ERRCOEF 256

int main (int argc, char **argv)
{

char dir[]="";
char s2h[]="s2h.exe";
char hb[]="habala-f.exe";
char hnr[]="hnr.exe";
char s2hcfg[]="s2h.cfg";
char *config=NULL;
char *input=NULL;
char *output=NULL;
char *log=NULL;

#if defined DEBUG
for (int j=0; j<argc; j++) { cout<<argv[j]<<" "; }
cout <<"\n";
*/
#endif
	
/* parse command line and assign filenames; check filenames */
int i=1;
int finish=0;
while (i<argc && !finish)
	{
	if (argv[i][0] == '-')
		{ // parse options	
		switch (argv[i][1]) {
			case 'c':// config
				config=argv[i+1];
				i++;
				break;
			case 'a'://about
				usage();
				exit (ERR_USAGE);
				break;					
			case 'h'://help
				usage();
				exit (ERR_USAGE);
				break;		
			case 'l'://logname
				log=argv[i+1];
				i++;
				break;		
			default: // unrecognized option
				cerr<<UNR_OPTION<<argv[i][1]<<"\n";
		}				
		i++;			
		}
	else finish=1;
	}


if(i<argc) {
	input=argv[i];
	i++;
	}
else 	{// usage : miss of parameters
	usage();
	exit (ERR_USAGE);
	}
// check input file
if (access(input, R_OK))
	{ 
	cerr<<INPUT_OPEN_U<<input<<"\n";
	exit(ERR_INPUT_OPEN);
	}
#if defined DEBUG
cout <<"input file: " <<input<<"\n";
#endif 	
if(i<argc) {
	output=argv[i];
	i++;
	}
else 	{// assign outfile
	output= new char[strlen(input)+1+4];
	ostrstream(output, sizeof(char)*(strlen(input)+1+4))<<input<<".raw"<<'\0';
	}

//check output
FILE *output_f;
output_f=fopen(output,"w");	
if (output_f == NULL) 
	{cerr<<OUTPUT_OPEN_U<<output<<"\n";
	exit(ERR_OUTPUT_OPEN);
	}
fclose(output_f);
remove(output);

#if defined DEBUG
cout <<"output file: " <<output<<"\n";
#endif

//assign logfile
if (log == NULL) {
	log =new char(strlen(input)+1+4);
	ostrstream(log, sizeof(char)*(strlen(input)+1+4))<<input<<".log"<<'\0';
	}
	
//check log
FILE *log_f;
log_f=fopen(log,"w");	
if (log_f == NULL)
	{cerr<<LOG_OPEN_U<<input<<"\n";
	exit(ERR_LOG_OPEN);
	}
fclose (log_f);	
remove(log);
#if defined DEBUG
cout <<"log file: " <<log<<"\n";
#endif

if((config != NULL) && ( 0 != access(config, R_OK))) { // probler reading assigned name - try std
		config=s2hcfg; // s2h will handle config availability by itself
	}
else if	(config == NULL){ //assign name
	config=s2hcfg;
	}
#if defined DEBUG	
cout <<CONFIG_IS<<config<<"\n";
#endif
// huuh

//make temp files:
char nodetbl[]="nodesXXXXXXX";
char  eletbl[]="elemsXXXXXXX";
char  s2hout[]="s2houtXXXXXXX";
mkstemp(nodetbl);
mkstemp(eletbl);
mkstemp(s2hout);
#if defined DEBUG
cout<<"nodetbl: "<<nodetbl<<'\n'; 
cout<<"eletbl:  "<<eletbl<<'\n'; 
cout<<"s2hout:  "<<s2hout<<'\n'; 
#endif

//call functions

/* TRANSLATOR ==================================================
#Convertor from Spice3 to HaBala language v.1.10 / May 30 2000
#Usage:  s2h [-flag -flag...] INPUT_FILE OUTPUT_FILE
#  standard flags:
# -?           - more help information (default: no)
# -cCONF_FILE  - get configurations settings from file (default: S2H.CFG)
# -eERROR_FILE - output error messages to file instead of stderr (default: no)
# -p           - pause after error if diagnostic to stderr (default: no)
# -s           - print statistic (default: no)
# -nNODES_FILE - rename nodes translation file (default: S2HNODES.TMP)
# -mELEMS_FILE - rename elements names translation file (default: S2HELEMS.TMP)
# -tTEMP_DIR   - directory for temporary files (default: .)
# -u           - convert input to upper case (default: no)
S2H -c$S2HCNF -e$S2HLOG -n$NODETBL -m$ELETBL -t./ $1 $S2HOUT >/dev/null 2>&1
*/

int s2h_sz = strlen(s2h)+strlen(" -c")+strlen(config)+ \
             strlen(" -e")+strlen(log)+strlen(" -n")+strlen(nodetbl)+ \
	     strlen(" -m")+strlen(eletbl)+ strlen(" -t. ")+ \
	     strlen(input)+strlen(" ")+strlen(s2hout)+1;
char s2h_call[s2h_sz];
#if defined DEBUG
cout<<"s2h_sz= "<<s2h_sz<<"\n";		 		 
cout<<"sizeof s2h_call= "<<sizeof(s2h_call)<<"\n";		 
#endif
ostrstream (s2h_call, sizeof(s2h_call)) <<s2h<<" -c"<<config<< \
			" -e"<<log<<" -n"<<nodetbl<<" -m"<<eletbl<<" -t. "<< \
			input<<" "<<s2hout<<'\0';
#if defined DEBUG
cout<<"s2h call: "<<s2h_call<<'\n';
#endif
int s2hcode=system(s2h_call)/ERRCOEF;
cout<<TR_MSG<<s2hcode<<'\n';

if (s2hcode <=TRANSLATOR_NORM){
	// habala call
	/*
	# SIMULATE ===================================================
	HBLOG=$1.hlog
	FTEMP=`mktemp ./rawXXXXXXX`;
	
	#hb $S2HOUT -f$FTEMP | tee $HBLOG - use this if you need continious graph
	$HB $S2HOUT -p$FTEMP > $HBLOG   # pulsed graph

	HB_RETCODE=$?
	echo "ERRCODE = " $HB_RETCODE
	#usage: hb[.exe] <infile> [<outfile> -f<freq-table> -p<p-freq-table> -u<init> -a]
	#       infile         - HBP input file
	#       outfile        - HBP output file, optional, stdout if not set
	#       freq-table     - output variables in frequency domain
	#       p-freq-table   - "pulsed" output variables in frequency domain 
	#       uinit          - file of input variables (not supported now)
	#       -a             - about
	
	cat $HBLOG >> $S2HLOG
	rm $HBLOG
	*/	
	char  hblog[]="hblogXXXXXXX";
	char  ftemp[]="rawXXXXXXX";
	mkstemp(hblog);
	mkstemp(ftemp);
	
	int habala_sz=strlen(hb)+strlen(" ")+ \
		strlen(s2hout)+strlen(" ")+ \
		strlen(hblog)+strlen(" -f")+strlen(ftemp)+1;
	char habala_call[habala_sz];
	
	ostrstream(habala_call, sizeof(habala_call))<<hb<<" "<<s2hout<< \
				" "<<hblog<<" -p"<<ftemp<<'\0';
	#if defined DEBUG			
	cout<<"sizeof habala_call= "<<sizeof(habala_call)<<"\n";		 			
	cout<<"habala call: "<<habala_call<<"\n";
	#endif
	int habala_code=system(habala_call)/ERRCOEF;
	cout<<SIM_MSG<<habala_code<<"\n";
	
	// append hblog to log
	char i;
	ifstream from(hblog);
	ofstream to (log, ios::app);
	if (! from && !to)
		cerr<<CP_MSG1<<log<<CP_MSG2<<hblog<<CP_MSG3;
	else for ( char c=0;to && from.get(c);)
		to.put(c);
	// remove hblog
	remove (hblog);	
	
	
	if(habala_code <= SIMULATOR_NORM) {
		// restore node names
		/*#RESTORE ==================================================== 
		#hnr
		#usage: hnr[.exe] rawfile table-file [outfile] 
		#  change names of variables in "rawfile" 
		#  according to "table-file" and write result 
		#  to "outfile" 
		#  (c) Gennady Serdyuk, 2000, gserdyuk@mail.ru 
		
		$HNR $FTEMP $NODETBL $OUTFILE >/dev/null 2>&1
		*/
		int hnr_sz=strlen(hnr)+strlen(" ")+ \
					strlen(ftemp)+strlen(" ")+ \
					strlen(nodetbl)+ strlen(" ")+ \
					strlen(output)+1;
		char hnr_call[hnr_sz];
		ostrstream(hnr_call, sizeof(hnr_call))<<hnr<<" "<< \
					ftemp<<" "<<nodetbl<<" "<<output<<'\0';
		#if defined DEBUG			
		cout<<"sizeof hbr_call= "<<sizeof(hnr_call)<<"\n";		 			
		cout<<"hnr call: "<<hnr_call<<"\n";
		#endif
		int hnr_code=system(hnr_call)/ERRCOEF;
		cout<<HNR_MSG<<hnr_code<<"\n";
		remove (ftemp);
		}
	else {
		cerr <<SIM_ERRS<<log<<'\n';
		remove (nodetbl);
		remove (eletbl);
		remove (s2hout);
		remove (ftemp);
		return (BAD_EXIT);
		}
	}			
else {
	cerr <<TR_ERRS<<log<<'\n';
	remove (nodetbl);
	remove (eletbl);
	remove (s2hout);
	return (BAD_EXIT);

	}
	
remove (nodetbl);
remove (eletbl);
remove (s2hout);


return 0;
}

int usage()
{
cerr<< USAGE;
return 0;
}
