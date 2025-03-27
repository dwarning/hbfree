
/*
 *  S H c o m m o n . H  --  S2H-compiler:  Common Definitions
 *
 *  29.12.1999 - create by Alexandr S. Belogolovsky (ASB)
 *                                     (a_b@i.am, spice3man@yahoo.com)
 */

/*-------------------------------------------------------------------------*/
/*** common significant settings: ***/

# define    DEBUGCIRCSTATE /* print to debug information about circuit state */

/* messages language: */
# define    ENGLISH /* switch to english messages file */

/*# define  UPFNAME       /* before open translate file names to uppercase */
# define    maxINPLINELEN 1000 /* restriction! */
# define    maxFNAMELEN   250  /* restriction! */

# define    nameSTDERR    "STDERR"       /* "name" of default error file */
# define    nameCONFILE   "S2H.CFG"      /* default config file name */
# define    nameTMPMASK   "S2H_%04d.TMP" /* mask for tmp-file name */
# define    nameTMPDIRDEF "."            /* default temporary directory */
# define    nameNODESFILE "S2HNODES.TMP" /* default nodes translation file name */
# define    nameELEMSFILE "S2HELEMS.TMP" /* default elements names translation file name */

/* SHmain.H (SHmsgEN.H): */
# define    maxMSGLEN      80

/* SHlpars.L: */
# define    maxINCLNEST    32 /* max nesting level of .INCLUDE */
/* # define INCL_NEST_EXCEED_IS_FATAL /* exit if exceed include nesting is fatal */
/* # define INCL_FILE_HAVE_TITLE      /* include file have title */

# define    lenHABNAME   4 /* ! '4' may be in format specification, f.e. "%4s" */
# define    lenHABNAME0  5

typedef  unsigned char  byte;

/*-------------------------------------------------------------------------*/
/* OS differences: */
# if defined(BORLAND_C)
#   define  DOS     /* MS DOS */
# elif defined(VISUAL_C) || defined(GNU_C)
#   define  W32     /* Windows 9x/NT4 */
# else
#   define  UNIX    /* UNIX on Intel */
# endif

# if defined(DOS)
#   define sNULDEV "NUL"
#   define sPATHSEPARATOR "\\"
# elif defined(W32)
#   define sNULDEV "NUL"
#   define sPATHSEPARATOR "\\"
# elif defined(UNIX) || defined(BSD)
#   define sNULDEV "/dev/null"
#   define sPATHSEPARATOR "/"
# endif

/*-------------------------------------------------------------------------*/
/* standard headers: */
# include   <stdio.h>
# include   <stdlib.h>
# include   <string.h>
# include   <stdarg.h>
# include   <ctype.h>
# include   <math.h>
#if defined(BORLAND_C)
# include   <alloc.h>
# include   <dir.h>  /* for searchpath() */
#elif defined(BSD)
# define    searchpath(fname)   fname  /* instead of searchpath() */
# define    coreleft()          (0L)   /* instead of coreleft() */
#else
# include   <malloc.h>
# define    searchpath(fname)   fname  /* instead of searchpath() */
# define    coreleft()          (0L)   /* instead of coreleft() */
#endif

/*-------------------------------------------------------------------------*/
/* files with declarations and prototypes: */
#ifdef     ENGLISH
# include   "shmsgen.h" /* file with english messages */
#else
# include   "shmsgen.h" /* file with default language messages */
#endif
# include   "shmain.h"
# include   "shiofun.h"
# include   "shpass1.h"  /* include useful types definitions */
# include   "shpass2.h"
# include   "shlpars.h"  /* must be after SHpass1.H, SHpass2.H */
# include   "shpars.h"   /* must be after SHpass1.H, SHpass2.H */
# include   "shconfig.h"
# include   "shexcode.h"
/* not include here:
#if defined(UNIX) || defined(BSD)
# include   "y.tab.h"
#else
# include   "YTAB.H"
#endif
*/
/*-------------------------------------------------------------------------*/
/* useful constants: */
# define    FALSE   0
# define    TRUE    (!0)
# define    ESC     0x1B
# define    TABSIZE 8

/*-------------------------------------------------------------------------*/
/* macros: */
# define  streq( s1, s2 ) ( strcmp(s1, s2) == 0 )
          
# define  CHECKSTREAM( file, name ) \
    if( ferror( file ) )  MsgErrorFatal( exitOUTFAIL, msgOUTFAIL, name )

#ifdef SHOWtoSTDERR
#  define SHOWstr(var)   fprintf(stderr,"(char*)%s = \"%s\"\n",#var,var);fflush(stderr);
#  define SHOWchar(var)  fprintf(stderr,"(char)%s = '%c'\n",#var,var);fflush(stderr);
#  define SHOWint(var)   fprintf(stderr,"(int)%s = %d\n",#var,var);fflush(stderr);
#  define SHOWlong(var)  fprintf(stderr,"(long)%s = %ld\n",#var,var);fflush(stderr);
#  define SHOWfloat(var) fprintf(stderr,"(float)%s = %g\n",#var,var);fflush(stderr);
#  define SHOWptr(var)   fprintf(stderr,"(ptr)%s = %p\n",#var,var);fflush(stderr);
#  define SHOW3tab       fprintf(stderr,"\t\t\t");
#  define SHOW4tab       fprintf(stderr,"\t\t\t\t");
#else
#  define SHOWstr(var)   fprintf(fileDebug,"(char*)%s = \"%s\"\n",#var,var);fflush(fileDebug);
#  define SHOWchar(var)  fprintf(fileDebug,"(char)%s = '%c'\n",#var,var);fflush(fileDebug);
#  define SHOWint(var)   fprintf(fileDebug,"(int)%s = %d\n",#var,var);fflush(fileDebug);
#  define SHOWlong(var)  fprintf(fileDebug,"(long)%s = %ld\n",#var,var);fflush(fileDebug);
#  define SHOWfloat(var) fprintf(fileDebug,"(float)%s = %g\n",#var,var);fflush(fileDebug);
#  define SHOWptr(var)   fprintf(fileDebug,"(ptr)%s = %p\n",#var,var);fflush(fileDebug);
#  define SHOW3tab       fprintf(fileDebug,"\t\t\t");
#  define SHOW4tab       fprintf(fileDebug,"\t\t\t\t");
#endif

#ifdef DEBUGCIRCSTATE                                            
# define SHOWCIRCSTATE( sWhere )                                       \
 if( Flags[flagG].value || Flags[flag1].value || Flags[flag2].value ){ \
   MsgDebug( sWhere ); /*MsgDebug( msgCircState );*/ MsgDebug( "\n" ); \
   MsgDebug( "  level SubDef\t= %c%d",                                 \
                       isSubDefBody?'+':'-', levelSubDef );            \
   MsgDebug( "\tcircDefCurr = %p = '%s' :\t", circDefCurrent,          \
                   (circDefCurrent==NULL)? msgMainCircuit:             \
                                           circDefCurrent->circname ); \
   OutFullCircName( fileDebug, circDefCurrent ); MsgDebug( "\n" );     \
   MsgDebug( "  level SubCall\t= %c%d",                                \
                          isSubCall?'+':'-', levelSubCall );           \
   MsgDebug( "\tcircCall    = %p = '%s' :\t", circCall,                \
                                (circCall==NULL)? msgMainCircuit:      \
                                                  circCall->circname );\
   OutFullCircName( fileDebug, circCall ); MsgDebug( "\n" );           \
 }
#else
# define SHOWCIRCSTATE( sWhere )
#endif

/*-------------------------------------------------------------------------*/
