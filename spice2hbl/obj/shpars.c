/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 2 "shpars.y" /* yacc.c:339  */

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

#line 161 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    _ENDOFSTMT_ = 258,
    _INCLUDE = 259,
    tokenUNKNOWN = 260,
    IDENTNAME = 261,
    STRING = 262,
    FLOATNUM = 263,
    DECINTNUM = 264,
    IDENTINT = 265,
    IDENTFLOAT = 266,
    RESISTOR = 267,
    CAPACITOR = 268,
    INDUCTOR = 269,
    INDUCTCOUPL = 270,
    CURRENT = 271,
    VOLTAGE = 272,
    ISWITCH = 273,
    VSWITCH = 274,
    DIODE = 275,
    TRANSISTOR = 276,
    JFET = 277,
    MOSFET = 278,
    MESFET = 279,
    LLTLINE = 280,
    LTLINE = 281,
    UDRCLINE = 282,
    LCCCS = 283,
    LCCVS = 284,
    LVCCS = 285,
    LVCVS = 286,
    NLDS = 287,
    SUBCALL = 288,
    _AC = 289,
    _DC = 290,
    _DISTO = 291,
    _END = 292,
    _ENDS = 293,
    _IC = 294,
    _MODEL = 295,
    _NODESET = 296,
    _NOISE = 297,
    _OP = 298,
    _PZ = 299,
    _SENS = 300,
    _SUBCKT = 301,
    _TF = 302,
    _TRAN = 303,
    _WIDTH = 304,
    _OPTIONS = 305,
    _SAVE = 306,
    _PLOT = 307,
    _PRINT = 308,
    _FOUR = 309,
    _TLINE = 310,
    _ENDC = 311,
    _CONTROLS = 312,
    _ALTER = 313,
    _WRITE = 314,
    _LET = 315,
    _HB = 316,
    _HB_OPTIONS = 317,
    AC = 318,
    AD = 319,
    AS = 320,
    CUR = 321,
    DC = 322,
    DEC = 323,
    DISTOF1 = 324,
    DISTOF2 = 325,
    DISTO = 326,
    EXP = 327,
    F_FREQ = 328,
    I_SRC = 329,
    IC = 330,
    L_LEN = 331,
    LIN = 332,
    N_LUMPS = 333,
    NL = 334,
    NOISE = 335,
    NRD = 336,
    NRS = 337,
    OCT = 338,
    OFF = 339,
    ON = 340,
    PD = 341,
    POL = 342,
    PS = 343,
    PULSE = 344,
    PWL = 345,
    PZ = 346,
    SFFM = 347,
    SIN = 348,
    TD = 349,
    TRAN = 350,
    TEMP = 351,
    UIC = 352,
    V_SRC = 353,
    VR = 354,
    VI = 355,
    VM = 356,
    VP = 357,
    VDB = 358,
    VOL = 359,
    W_WIDTH = 360,
    Z0 = 361,
    ZER = 362,
    UMINUS = 363,
    BRACK = 364
  };
#endif
/* Tokens.  */
#define _ENDOFSTMT_ 258
#define _INCLUDE 259
#define tokenUNKNOWN 260
#define IDENTNAME 261
#define STRING 262
#define FLOATNUM 263
#define DECINTNUM 264
#define IDENTINT 265
#define IDENTFLOAT 266
#define RESISTOR 267
#define CAPACITOR 268
#define INDUCTOR 269
#define INDUCTCOUPL 270
#define CURRENT 271
#define VOLTAGE 272
#define ISWITCH 273
#define VSWITCH 274
#define DIODE 275
#define TRANSISTOR 276
#define JFET 277
#define MOSFET 278
#define MESFET 279
#define LLTLINE 280
#define LTLINE 281
#define UDRCLINE 282
#define LCCCS 283
#define LCCVS 284
#define LVCCS 285
#define LVCVS 286
#define NLDS 287
#define SUBCALL 288
#define _AC 289
#define _DC 290
#define _DISTO 291
#define _END 292
#define _ENDS 293
#define _IC 294
#define _MODEL 295
#define _NODESET 296
#define _NOISE 297
#define _OP 298
#define _PZ 299
#define _SENS 300
#define _SUBCKT 301
#define _TF 302
#define _TRAN 303
#define _WIDTH 304
#define _OPTIONS 305
#define _SAVE 306
#define _PLOT 307
#define _PRINT 308
#define _FOUR 309
#define _TLINE 310
#define _ENDC 311
#define _CONTROLS 312
#define _ALTER 313
#define _WRITE 314
#define _LET 315
#define _HB 316
#define _HB_OPTIONS 317
#define AC 318
#define AD 319
#define AS 320
#define CUR 321
#define DC 322
#define DEC 323
#define DISTOF1 324
#define DISTOF2 325
#define DISTO 326
#define EXP 327
#define F_FREQ 328
#define I_SRC 329
#define IC 330
#define L_LEN 331
#define LIN 332
#define N_LUMPS 333
#define NL 334
#define NOISE 335
#define NRD 336
#define NRS 337
#define OCT 338
#define OFF 339
#define ON 340
#define PD 341
#define POL 342
#define PS 343
#define PULSE 344
#define PWL 345
#define PZ 346
#define SFFM 347
#define SIN 348
#define TD 349
#define TRAN 350
#define TEMP 351
#define UIC 352
#define V_SRC 353
#define VR 354
#define VI 355
#define VM 356
#define VP 357
#define VDB 358
#define VOL 359
#define W_WIDTH 360
#define Z0 361
#define ZER 362
#define UMINUS 363
#define BRACK 364

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 97 "shpars.y" /* yacc.c:355  */

    long    longnum;    /* integer value */
    double  floatnum;   /* float value */
    char    *strptr;    /* pointer to identifier */
    value   *mixed;     /* mixed value for number-identifier */

#line 426 "y.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */
#line 164 "shpars.y" /* yacc.c:358  */

/*  ASB000603: keywords table - move to SHparsY.C:
KeysTab KeywordsTab[] = {...};
*/

#line 448 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1531

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  119
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  243
/* YYNRULES -- Number of rules.  */
#define YYNRULES  451
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  799

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   364

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     115,   116,   111,   109,   118,   110,     2,   112,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   117,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,   108,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   113,   114
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   175,   175,   175,   211,   230,   233,   240,   242,   242,
     252,   253,   254,   255,   256,   257,   258,   259,   260,   263,
     264,   265,   266,   267,   268,   269,   270,   271,   272,   273,
     274,   275,   276,   277,   283,   282,   289,   297,   288,   302,
     304,   305,   308,   309,   310,   313,   322,   338,   338,   338,
     340,   377,   377,   384,   437,   383,   503,   503,   504,   516,
     519,   522,   524,   530,   529,   534,   544,   534,   644,   644,
     645,   670,   669,   676,   675,   682,   681,   726,   732,   738,
     724,   743,   746,   745,   756,   755,   762,   761,   804,   825,
     803,   830,   830,   831,   831,   834,   833,   844,   843,   850,
     867,   849,   895,   916,   894,   921,   921,   922,   922,   925,
     924,   931,   934,   933,   944,   943,   952,   951,  1024,  1023,
    1032,  1031,  1067,  1067,  1069,  1086,  1086,  1088,  1095,  1105,
    1121,  1149,  1191,  1219,  1223,  1222,  1237,  1237,  1238,  1238,
    1240,  1245,  1245,  1251,  1250,  1274,  1274,  1280,  1279,  1303,
    1303,  1309,  1315,  1308,  1334,  1334,  1340,  1346,  1339,  1367,
    1367,  1373,  1372,  1393,  1393,  1394,  1394,  1396,  1412,  1422,
    1425,  1428,  1432,  1436,  1440,  1444,  1448,  1452,  1457,  1477,
    1485,  1486,  1500,  1499,  1506,  1505,  1512,  1511,  1535,  1541,
    1534,  1561,  1562,  1572,  1571,  1578,  1577,  1584,  1583,  1591,
    1589,  1611,  1610,  1654,  1652,  1714,  1714,  1715,  1715,  1717,
    1774,  1783,  1785,  1797,  1796,  1803,  1829,  1802,  1857,  1857,
    1858,  1858,  1860,  1870,  1876,  1887,  1886,  1893,  1892,  1916,
    1915,  1941,  1943,  1950,  1950,  1951,  1951,  1953,  1954,  1960,
    1966,  1978,  1977,  1984,  1983,  2006,  2006,  2007,  2007,  2009,
    2010,  2016,  2022,  2034,  2033,  2040,  2070,  2039,  2105,  2105,
    2106,  2106,  2108,  2112,  2118,  2130,  2129,  2136,  2135,  2157,
    2157,  2158,  2158,  2161,  2177,  2196,  2225,  2224,  2229,  2232,
    2232,  2234,  2235,  2236,  2240,  2240,  2244,  2249,  2249,  2256,
    2256,  2264,  2263,  2268,  2271,  2271,  2273,  2274,  2280,  2279,
    2284,  2287,  2287,  2290,  2289,  2301,  2300,  2306,  2308,  2308,
    2311,  2310,  2321,  2321,  2326,  2333,  2333,  2338,  2340,  2340,
    2343,  2342,  2356,  2356,  2362,  2361,  2368,  2367,  2375,  2375,
    2382,  2380,  2391,  2399,  2402,  2405,  2407,  2412,  2412,  2418,
    2417,  2423,  2423,  2429,  2428,  2439,  2439,  2445,  2444,  2450,
    2451,  2458,  2458,  2464,  2463,  2469,  2469,  2473,  2477,  2482,
    2481,  2487,  2489,  2539,  2539,  2545,  2544,  2548,  2548,  2550,
    2556,  2556,  2561,  2570,  2593,  2561,  2603,  2605,  2618,  2620,
    2619,  2624,  2635,  2634,  2641,  2640,  2645,  2645,  2647,  2655,
    2666,  2666,  2672,  2671,  2675,  2675,  2677,  2679,  2681,  2685,
    2685,  2691,  2690,  2694,  2694,  2697,  2697,  2703,  2702,  2706,
    2706,  2708,  2710,  2712,  2716,  2716,  2722,  2721,  2725,  2725,
    2726,  2726,  2729,  2750,  2757,  2785,  2791,  2800,  2804,  2825,
    2827,  2829,  2834,  2836,  2838,  2842,  2845,  2848,  2857,  2863,
    2865,  2867,  2871,  2874,  2880,  2883,  2886,  2891,  2894,  2900,
    2921,  2947
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "_ENDOFSTMT_", "_INCLUDE",
  "tokenUNKNOWN", "IDENTNAME", "STRING", "FLOATNUM", "DECINTNUM",
  "IDENTINT", "IDENTFLOAT", "RESISTOR", "CAPACITOR", "INDUCTOR",
  "INDUCTCOUPL", "CURRENT", "VOLTAGE", "ISWITCH", "VSWITCH", "DIODE",
  "TRANSISTOR", "JFET", "MOSFET", "MESFET", "LLTLINE", "LTLINE",
  "UDRCLINE", "LCCCS", "LCCVS", "LVCCS", "LVCVS", "NLDS", "SUBCALL", "_AC",
  "_DC", "_DISTO", "_END", "_ENDS", "_IC", "_MODEL", "_NODESET", "_NOISE",
  "_OP", "_PZ", "_SENS", "_SUBCKT", "_TF", "_TRAN", "_WIDTH", "_OPTIONS",
  "_SAVE", "_PLOT", "_PRINT", "_FOUR", "_TLINE", "_ENDC", "_CONTROLS",
  "_ALTER", "_WRITE", "_LET", "_HB", "_HB_OPTIONS", "AC", "AD", "AS",
  "CUR", "DC", "DEC", "DISTOF1", "DISTOF2", "DISTO", "EXP", "F_FREQ",
  "I_SRC", "IC", "L_LEN", "LIN", "N_LUMPS", "NL", "NOISE", "NRD", "NRS",
  "OCT", "OFF", "ON", "PD", "POL", "PS", "PULSE", "PWL", "PZ", "SFFM",
  "SIN", "TD", "TRAN", "TEMP", "UIC", "V_SRC", "VR", "VI", "VM", "VP",
  "VDB", "VOL", "W_WIDTH", "Z0", "ZER", "'^'", "'+'", "'-'", "'*'", "'/'",
  "UMINUS", "BRACK", "'('", "')'", "'='", "','", "$accept", "Program",
  "$@1", "ProgramBody", "StatementList", "StatementItem", "$@2",
  "StmtElement", "StmtModel", "$@3", "$@4", "$@5", "maybeModelParameters",
  "ModelParameters", "ModelParameter", "mbModelAssign", "ModelParm",
  "StmtSubCircuit", "$@6", "$@7", "$@8", "SubCircNodeList", "SubCircNode",
  "SubCircStatementList", "ENDSubCircuit", "CallSubCircuit", "$@9", "$@10",
  "$@11", "SubCallNodeList", "SubCallNode", "ElemINDUCTOR", "$@12", "$@13",
  "$@14", "$@15", "$@16", "$@17", "maybeINDUoptIC", "$@18", "ElemRESISTOR",
  "$@19", "$@20", "$@21", "$@22", "maybeRESIoptions", "RESIoptions",
  "RESIoption", "$@23", "ElemCAPACITOR", "$@24", "$@25", "$@26", "$@27",
  "$@28", "maybeCAPAoptions", "CAPAoptions", "CAPAoption", "$@29",
  "maybeCAPAoptIC", "$@30", "ElemVOLTAGE", "$@31", "$@32", "ElemCURRENT",
  "$@33", "$@34", "maybeOptDCfree", "OptDCfree", "maybeVoltCurrOptions",
  "VoltCurrOptions", "startVoltCurrOption", "VoltCurrOption",
  "IndepSrcFun", "$@35", "maybeISFunParmList", "ISFunParmList",
  "ISFunParm", "ElemLVCCS", "$@36", "$@37", "ElemLVCVS", "$@38", "$@39",
  "ElemLCCCS", "$@40", "$@41", "$@42", "ElemLCCVS", "$@43", "$@44", "$@45",
  "ElemNLDS", "$@46", "$@47", "maybeNLDSoptions", "NLDSoptions",
  "NLDSoption", "ArExpr", "BRKEND", "StmtSwitch", "$@48", "$@49", "$@50",
  "$@51", "$@52", "Switch", "StmtTransmissionLine", "$@53", "$@54", "$@55",
  "$@56", "$@57", "$@58", "maybeLLTLoptions", "LLTLoptions", "LLTLopt",
  "maybeUDRCLoptN", "SemiDiode", "$@59", "$@60", "$@61",
  "maybeDIODEoptions", "DIODEoptions", "DIODEoption", "SemiTransistor",
  "$@62", "$@63", "$@64", "bjtSnode", "maybeTRANSoptions", "TRANSoptions",
  "TRANSoption", "SemiJFET", "$@65", "$@66", "maybeJFEToptions",
  "JFEToptions", "JFEToption", "SemiMESFET", "$@67", "$@68", "$@69",
  "maybeMESFEToptions", "MESFEToptions", "MESFEToption", "SemiMOSFET",
  "$@70", "$@71", "maybeMOSFEToptions", "MOSFEToptions", "MOSFEToption",
  "StmtControlLines", "$@72", "StmtCtrlLineList", "StmtControl",
  "StmtCtrlLine", "$@73", "$@74", "StmtOutput", "$@75", "$@76",
  "OPTIONSoptions", "OPTIONSopt", "$@77", "NODESEToptions", "NODESETopt",
  "$@78", "$@79", "ICoptions", "ICopt", "$@80", "$@81", "$@82", "_DClist",
  "_DCquad", "$@83", "$@84", "$@85", "$@86", "$@87", "$@88",
  "startNoiseVoltage", "NoiseVoltage", "maybePtsPerSum", "$@89", "$@90",
  "$@91", "$@92", "$@93", "$@94", "maybeSENSopt", "$@95", "$@96", "$@97",
  "$@98", "maybeUIC", "$@99", "$@100", "WidthParameters", "WidthParameter",
  "$@101", "$@102", "$@103", "$@104", "maybe_HB_F2", "_HBpairs", "$@105",
  "_HBpair", "$@106", "$@107", "HabalaOptions", "HabalaOption", "$@108",
  "$@109", "SaveVectors", "SaveVector", "$@110", "$@111", "FourOutVarList",
  "$@112", "$@113", "PrintList", "PrintItem", "$@114", "$@115",
  "PlotOutVarList", "PlotOutVar", "PrintType", "PrintOutVar", "OutVar",
  "Identifier", "Node", "NodeName", "Value", "UnsignValue", "Float",
  "UnsignFloat", "DecInt", "UnsignDecInt", "DECorOCTorLIN", "CURorVOL",
  "POLorZERorPZ", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,    94,    43,
      45,    42,    47,   363,   364,    40,    41,    61,    44
};
# endif

#define YYPACT_NINF -471

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-471)))

#define YYTABLE_NINF -432

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -471,    24,  1420,  -471,  -471,  -471,    29,    47,   102,    25,
     104,   156,   169,   185,   192,   198,   204,   210,   254,   262,
     268,   315,   317,   323,   329,   331,   344,   384,    74,   103,
     105,   181,   206,   212,   251,   146,   386,   270,   392,   279,
      55,   285,   330,    27,   337,   352,    59,   189,    37,    60,
     373,   385,   398,  -471,  1208,  -471,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,
     126,  -471,  -471,  -471,  -471,   111,  -471,  -471,   111,  -471,
     111,  -471,  -471,  -471,  -471,   111,  -471,   111,  -471,   111,
    -471,   111,  -471,   111,  -471,   111,  -471,   111,  -471,   111,
    -471,   111,  -471,   111,  -471,   111,  -471,   111,  -471,   111,
    -471,   111,  -471,   111,  -471,   111,  -471,   111,  -471,   111,
    -471,  -471,    45,  -471,    56,  -471,  -471,  -471,    45,  -471,
     117,  -471,  -471,  -471,    76,  -471,   191,  -471,  -471,  -471,
      -8,    76,  -471,  -471,   145,  -471,   111,  -471,    96,    58,
    -471,  -471,  -471,    76,  -471,  -471,  -471,  -471,  -471,    63,
      63,    79,  -471,  -471,    76,  -471,    64,  -471,   314,  -471,
      89,  -471,  -471,    85,  -471,  -471,    58,  -471,    76,  -471,
    -471,   247,  -471,    76,  -471,  -471,   253,  -471,  -471,  -471,
    -471,  -471,  -471,   109,  -471,    76,  -471,    76,  -471,  -471,
    -471,   216,    75,   232,    75,   242,    79,   256,    76,   267,
    -471,   274,  -471,   290,    76,   296,   111,   316,    76,   356,
     111,   372,   111,   404,   111,   411,   111,   413,   111,   422,
     111,   433,   111,   446,    76,   447,    76,   448,   111,   449,
     111,   450,  -471,   451,   111,  -471,  -471,   453,  -471,  -471,
     391,   391,    79,  -471,   454,  -471,  -471,    79,   455,    79,
     457,  -471,  -471,   347,   460,  -471,   461,  -471,  -471,   350,
     463,   111,    76,   464,  -471,   465,   111,   466,    76,  -471,
     111,   467,   111,   468,  -471,   469,  -471,  -471,    75,   471,
     472,  -471,    79,   473,  -471,  -471,    79,   474,   475,  -471,
     477,    76,  -471,   366,   367,   481,  -471,   247,  -471,  -471,
     367,   483,    76,  -471,   484,   486,  -471,   488,   489,   490,
      35,   491,   378,   493,   418,  -471,   380,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,    76,  -471,  -471,  -471,    79,  -471,
      79,  -471,  -471,  -471,   111,  -471,  -471,  -471,   346,  -471,
      76,  -471,   111,  -471,    76,  -471,   111,  -471,   111,  -471,
      76,  -471,  -471,  -471,  -471,  -471,   111,  -471,   111,  -471,
      76,  -471,  -471,   495,  -471,  -471,  -471,    79,  -471,    79,
    -471,    79,  -471,   111,  -471,    15,  -471,   111,  -471,    42,
      79,  -471,  -471,   111,  -471,    45,   496,   128,  -471,  1074,
    -471,  -471,  -471,   497,  -471,  -471,  -471,    75,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,   498,  -471,    79,   111,
    -471,   500,  -471,  -471,   501,  -471,  -471,  -471,  -471,  -471,
    -471,  -471,    35,    35,    16,   390,  -471,  -471,    79,  -471,
    -471,  -471,    91,    76,   503,    76,    76,  -471,  -471,    79,
      76,  -471,  -471,    76,    76,    76,    75,  -471,  -471,    76,
    -471,  -471,    76,  -471,    76,    76,   431,    79,    79,    79,
      79,   505,    76,  -471,   394,   506,   509,    79,    79,   399,
      76,  -471,    31,  -471,    17,  -471,   400,  -471,   111,    79,
      76,    79,  -471,  -471,   111,   476,  1345,  -471,  -471,  -471,
    -471,    76,  -471,   401,   228,  -471,  -471,  -471,    28,  -471,
      35,    35,    35,    35,    35,    41,    76,  -471,  -471,   424,
     424,  -471,  -471,  -471,  -471,    76,  -471,  -471,  -471,  -471,
      76,  -471,  -471,  -471,  -471,   517,   406,  -471,   518,    76,
      68,  -471,  -471,  -471,   522,    76,    76,  -471,    75,  -471,
     414,  -471,  -471,    75,    75,  -471,    75,  -471,    76,  -471,
     416,  -471,   417,  -471,  -471,  -471,  -471,  -471,  -471,    35,
    -471,  -471,  -471,   525,  -471,   419,    26,   526,    76,  -471,
    -471,  -471,    79,   420,   423,    79,  -471,    76,    79,   425,
     502,  -471,    79,  -471,   111,  -471,  -471,  -471,   332,   294,
     294,  -471,  -471,   427,   429,   430,   301,   432,  -471,   421,
    -471,  -471,   532,  -471,   426,   539,  -471,   437,   546,   439,
    -471,    79,   556,  -471,  -471,    79,    79,  -471,  -471,  -471,
    -471,   557,  -471,    79,    75,   558,    75,  -471,   452,  -471,
     561,    75,  -471,   456,  -471,    76,  -471,    75,  -471,   458,
    -471,   562,  -471,    79,   563,    79,   565,   567,   568,   569,
     319,  -471,   571,    79,  -471,  -471,   462,  -471,    79,  -471,
    -471,  -471,  -471,  -471,  -471,   412,  -471,   573,   470,   478,
    -471,   111,    45,    79,  -471,    79,  -471,    79,  -471,    79,
    -471,  -471,    79,  -471,   574,   576,  -471,  -471,   577,  -471,
    -471,    79,  -471,  -471,    79,   578,    76,  -471,   480,   579,
    -471,    79,  -471,   482,  -471,    76,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,    45,   581,  -471,   584,  -471,  -471,
    -471,   485,   585,  -471,    45,  -471,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,   487,   492,  -471,  -471,    79,  -471,   494,
      79,  -471,   499,   586,  -471,  -471,  -471,  -471,   587,    45,
    -471,    79,    79,    79,   504,    79,   508,   589,    79,  -471,
    -471,  -471,   512,    79,  -471,  -471,  -471,  -471,    79,  -471,
      79,  -471,  -471,  -471,  -471,  -471,   511,    79,  -471
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     8,     7,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     3,     0,     5,    10,    13,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      11,    12,    29,    30,    31,    33,    32,    14,    15,    16,
       0,    84,   431,   429,   430,     0,   428,    97,     0,    71,
       0,    73,   427,    77,   118,     0,   114,     0,   184,     0,
     182,     0,   213,     0,   225,     0,   241,     0,   265,     0,
     253,     0,   193,     0,   195,     0,   197,     0,   149,     0,
     154,     0,   145,     0,   141,     0,   159,     0,    63,     0,
     312,   449,     0,   315,     0,   318,   320,   322,     0,   305,
       0,   308,   310,    34,     0,   298,     0,   301,   303,   328,
       0,     0,   332,   337,     0,   341,     0,   345,   349,     0,
      51,    53,   351,     0,   355,   436,   435,   437,   438,     0,
       0,     0,   432,   363,   365,   367,     0,   291,     0,   294,
     296,   390,   397,   392,   394,   398,   396,   414,     0,   422,
     405,     0,   399,     0,   276,   281,     0,   279,   283,   282,
     287,   289,   284,     0,   370,     0,   382,     0,     4,     6,
       9,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,   116,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   161,     0,     0,    68,    70,     0,   447,   448,
       0,     0,     0,   444,     0,   317,   319,     0,     0,     0,
       0,   307,   309,     0,     0,    36,     0,   300,   302,     0,
       0,     0,     0,     0,   340,     0,     0,     0,     0,   347,
       0,     0,     0,     0,   353,     0,   433,   434,   361,     0,
       0,   368,     0,     0,   293,   295,     0,     0,     0,   395,
       0,   416,   418,   420,     0,     0,   411,   407,   409,   413,
     412,     0,   401,   403,     0,     0,   280,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,    85,    88,    86,
      98,   102,    99,    72,    81,    74,    78,   119,   122,   115,
     122,   185,   188,   183,     0,   214,   215,   226,     0,   242,
       0,   266,     0,   254,     0,   194,     0,   196,     0,   198,
       0,   150,   151,   155,   156,   146,     0,   142,     0,   160,
     163,    64,    69,   428,   313,   446,   445,     0,   316,     0,
     323,     0,   306,     0,    35,    39,   299,     0,   329,     0,
       0,   338,   342,     0,   346,     0,     0,     0,    52,     0,
      56,    58,   352,     0,   356,   357,   362,   361,   364,   366,
     369,   292,   297,   391,   393,   415,     0,   419,     0,     0,
     406,     0,   410,   400,     0,   404,   277,   278,   288,   290,
     285,   168,     0,     0,     0,   170,   169,   371,     0,   383,
     385,   387,     0,    91,     0,   105,   111,    75,    82,     0,
     125,   123,   124,   125,     0,     0,   218,   231,   232,     0,
     227,   243,     0,   255,   205,     0,     0,     0,     0,     0,
       0,     0,   164,   165,     0,     0,     0,     0,   324,     0,
       0,    37,    40,    42,    47,    50,     0,   333,     0,     0,
       0,     0,   348,   425,     0,     8,     0,    57,    54,   354,
     358,   361,   417,     0,     0,   408,   402,   171,     0,   286,
       0,     0,     0,     0,     0,     0,   376,   442,   443,     0,
       0,   389,   439,   388,    89,    92,    93,    95,    87,   103,
     106,   107,   109,   100,   112,     0,     0,    79,     0,   126,
     130,   127,   133,   129,     0,   191,   191,   216,   219,   220,
     223,   222,   229,   233,   245,   267,   258,   203,   206,   207,
       0,   201,     0,   152,   157,   147,   143,   162,   166,     0,
      67,   314,   321,     0,   326,     0,     0,     0,     0,    43,
      48,    49,     0,     0,     0,     0,   450,     0,     0,     0,
       0,   359,     0,   423,     0,   181,   180,   177,   176,   172,
     173,   174,   175,   427,   437,   438,     0,     0,   373,     0,
     440,   441,     0,    94,     0,     0,   108,     0,     0,     0,
      76,     0,     0,   121,   128,     0,   131,   117,   189,   192,
     186,     0,   221,     0,   233,     0,   234,   235,   238,   237,
       0,   246,   247,   250,   249,   269,   256,   259,   260,   263,
     262,     0,   208,     0,     0,     0,     0,     0,     0,     0,
     167,   325,     0,     0,    41,    38,     0,    45,     0,   334,
     330,   451,   343,   350,   426,     0,    55,     0,     0,     0,
     178,     0,     0,     0,    90,     0,   104,     0,   101,     0,
      83,    80,     0,   132,     0,     0,   217,   224,     0,   228,
     236,     0,   244,   248,     0,     0,   270,   271,   273,     0,
     261,     0,   204,   209,   202,   211,   153,   158,   148,   144,
     327,   311,    44,   304,   335,     0,    61,     0,   360,   421,
     424,     0,   379,   378,     0,   377,    96,   110,   113,   134,
     190,   187,   230,   239,   251,   268,   272,     0,   257,     0,
       0,   199,     0,     0,   336,   344,    62,   179,     0,     0,
     381,   136,     0,     0,   274,     0,     0,     0,     0,   331,
     375,   380,     0,   137,   138,   140,   240,   252,     0,   264,
       0,   200,   212,   135,   139,   275,     0,     0,   210
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -471,  -471,  -471,  -471,   184,   -51,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,   108,  -470,  -471,  -471,  -471,  -471,  -471,
    -471,  -471,   190,  -471,  -471,  -471,  -471,  -471,  -471,  -471,
     348,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,  -471,    71,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,  -471,    67,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,  -471,   258,  -471,   150,
    -471,  -471,    66,  -471,  -471,  -471,  -471,  -165,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,  -471,   127,  -427,    33,
    -471,  -471,  -471,  -471,  -471,  -471,    78,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,    52,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,    72,  -471,  -471,  -471,  -471,  -471,
     -20,  -471,   -15,  -471,  -471,  -471,  -471,  -471,   -19,  -471,
    -471,  -471,  -471,  -471,  -471,   -22,  -471,  -471,  -471,  -471,
    -471,   -80,  -471,  -471,  -471,   443,   -43,  -471,  -471,   -33,
    -471,  -471,  -471,   507,  -471,  -471,   514,  -471,  -471,  -471,
     523,  -471,  -471,  -471,  -471,   510,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -471,
    -471,  -471,  -471,  -471,  -471,  -471,  -392,  -471,  -471,  -471,
     513,  -471,  -471,  -471,  -471,  -471,  -471,  -471,  -129,  -471,
    -471,  -471,   307,  -471,  -471,  -471,   459,  -471,  -471,  -471,
    -471,  -471,  -471,   326,  -471,  -471,  -471,   334,   601,  -174,
     -32,   339,   927,   -37,   -40,  -159,  -471,   -84,  -130,  -248,
     -21,  -471,  -471
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    53,    54,    55,    80,    56,    57,   274,
     395,   587,   491,   492,   493,   592,   494,    58,   291,   292,
     600,   409,   410,   508,   686,    59,   253,   129,   485,   254,
     255,    60,   215,   217,   545,   218,   459,   632,   457,   546,
      61,   211,   454,   453,   622,   534,   535,   536,   624,    62,
     213,   456,   628,   455,   625,   539,   540,   541,   627,   543,
     629,    63,   221,   350,    64,   219,   348,   460,   461,   548,
     549,   550,   551,   552,   771,   782,   783,   784,    65,   249,
     669,    66,   247,   668,    67,   243,   477,   666,    68,   245,
     478,   667,    69,   251,   380,   481,   482,   483,   444,   607,
      70,   225,   223,   705,   464,   704,   638,    71,   237,   239,
     241,   777,   664,   661,   567,   568,   569,   761,    72,   227,
     466,   641,   557,   558,   559,    73,   229,   563,   644,   469,
     645,   646,   647,    74,   231,   564,   650,   651,   652,    75,
     235,   566,   719,   656,   657,   658,    76,   233,   655,   715,
     716,   717,    77,   324,   196,   197,    78,   329,   327,    79,
     328,   303,   178,   179,   276,   146,   147,   279,   270,   140,
     141,   273,   257,   264,   134,   135,   267,   268,   583,   672,
     280,   734,   150,   151,   763,   283,   154,   285,   735,   287,
     406,   289,   293,   413,   295,   687,   415,   299,   300,   174,
     175,   331,   205,   692,   768,   618,   742,   769,   743,   333,
     207,   334,   335,   307,   308,   183,   184,   321,   434,   322,
     315,   431,   317,   318,   310,   426,   311,   312,   188,   313,
     185,   445,   256,    86,   649,   172,   531,   532,   744,   263,
     132,   597,   682
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     171,   161,   262,   209,   198,   158,   193,   163,   269,   138,
     296,   297,   385,   386,   199,   517,   518,   319,   -46,   519,
     -46,    92,   589,   -46,     3,   510,    91,   605,   181,   605,
      81,    92,    92,    92,   182,    82,   441,    92,   200,    83,
      84,    92,   441,   165,   166,   167,   168,   613,    87,   165,
     166,   614,   615,    82,   258,   259,   164,    83,    84,   265,
     192,   201,    92,   165,   166,   167,   168,   165,   166,   167,
     168,   165,   166,   167,   168,   130,   165,   166,   167,   168,
      92,    92,    92,   165,   166,   167,   168,   165,   166,   167,
     168,    92,   182,   608,   609,   610,   611,   612,   616,   527,
     258,   259,   528,    89,   133,    94,   137,   281,    82,    92,
      82,    92,    83,    84,    83,    84,   589,    82,   676,   601,
     271,    83,    84,    92,   520,   521,   522,   523,   524,   210,
     490,   298,   -46,   -46,   590,   591,   520,   521,   522,   523,
     524,   588,   606,   319,   606,   442,   588,   153,   284,  -339,
     443,   442,   670,   198,   260,   261,   443,    96,   497,   288,
     498,   323,    82,   199,   169,   170,    83,    84,   169,   170,
      98,   446,   339,   290,   342,    82,   344,   169,   170,    83,
      84,   302,   139,   635,   169,   170,   100,    92,   169,   170,
     194,    82,   195,   102,   277,    83,    84,    92,    82,   104,
     529,   530,    83,    84,    82,   106,   306,   143,    83,    84,
      82,   108,    92,   145,    83,    84,    82,   383,    92,   337,
      83,    84,   387,    28,    29,    30,   330,   389,    31,   391,
      33,    34,    35,    36,    37,   340,    39,    40,    41,    42,
      43,    44,    45,    46,   503,   343,   504,    48,    49,    50,
      51,    52,   149,    92,   316,   110,   195,    92,   417,   345,
      82,   400,   420,   112,    83,    84,   422,   405,    82,   114,
     347,   157,    83,    84,    82,   501,    92,   349,    83,    84,
     162,   385,   386,   446,   446,    92,   173,    28,    29,    30,
     435,    92,    31,   351,    33,    34,    35,    36,    37,   353,
      39,    40,    41,    42,    43,    44,    45,    46,   462,   325,
     462,    48,    49,    50,    51,    52,   116,   304,   118,   355,
      92,    82,   533,    82,   120,    83,    84,    83,    84,    82,
     122,   177,   124,    83,    84,    82,    92,    82,   187,    83,
      84,    83,    84,    92,   603,   126,   604,   486,    93,   487,
      82,   488,    92,   190,    83,    84,   467,   468,    92,   357,
     499,   446,   446,   446,   446,   446,   446,   131,   136,   131,
     142,   144,   148,   152,   202,   359,   159,   511,   159,    92,
     176,   180,   186,   189,   189,   128,   204,   155,   513,   203,
     -65,  -372,    82,   160,   -65,   -65,    83,    84,    82,   206,
     258,   259,    83,    84,  -384,   523,   524,   361,   526,   520,
     521,   522,   523,   524,   363,   736,   365,   690,    82,   547,
     446,   450,    83,    84,    92,   367,   561,   520,   521,   522,
     523,   524,   527,   258,   259,   528,   369,   573,   574,   575,
     576,   521,   522,   523,   524,   620,   621,   582,   584,   371,
     373,   375,   377,   379,   381,   209,   384,   388,   390,   595,
     392,   598,   393,   394,   396,   397,   398,   401,   402,   404,
     408,   412,   414,   136,   418,   419,   421,   423,   424,   142,
     425,   428,   429,   275,   430,   148,   433,   436,   617,   437,
     282,   438,   439,   440,   447,   448,   449,   452,   -66,   502,
     509,   512,   294,   515,   516,   525,   538,   572,   577,   580,
     636,   579,   581,   176,   -59,   585,   593,   180,   561,   602,
     630,   633,   186,   631,   654,   637,   660,   314,   671,   675,
     320,   643,   159,   663,   665,   694,   673,   678,   693,   679,
     685,   684,   696,   695,   332,  -431,   336,  -429,  -430,   698,
     691,   338,   677,   341,   697,   680,   699,   346,   683,   701,
     706,   709,   688,   352,   712,   722,   724,   356,   726,   711,
     727,   728,   729,   714,   730,   721,   738,   750,   732,   751,
     752,   755,   758,   372,   765,   374,   739,   766,  -374,   779,
     780,   700,   791,   506,   740,   702,   703,   757,   586,   507,
     760,   767,   382,   707,   764,   772,   623,   626,   463,   578,
     773,   654,   775,   554,   770,   634,   778,   660,   794,   674,
     662,   131,   788,   723,   708,   725,   790,   131,   793,   797,
     642,   710,   713,   731,   640,   720,   756,   416,   733,   326,
     781,   451,   309,   432,   266,   427,   191,     0,   737,     0,
     314,     0,     0,   745,   741,   746,   320,   747,     0,   748,
     278,   159,   749,   272,     0,     0,     0,     0,     0,     0,
       0,   753,     0,   336,   754,     0,     0,     0,     0,     0,
       0,   759,     0,   458,     0,   305,     0,   301,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   470,     0,   471,
       0,     0,     0,   473,     0,     0,     0,     0,     0,   476,
       0,     0,     0,     0,     0,     0,     0,   774,     0,   484,
     776,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   785,   786,   787,   495,   789,     0,     0,   792,     0,
       0,     0,     0,   785,     0,     0,     0,     0,   795,     0,
     796,     0,     0,     0,     0,     0,   416,   798,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   537,     0,   542,   544,     0,     0,     0,   553,
       0,     0,   553,   555,   556,   560,     0,     0,   562,     0,
       0,   565,     0,   570,   571,     0,     0,     0,     0,     0,
       0,   484,     0,     0,     0,     0,     0,     0,     0,   495,
       0,   495,     0,     0,     0,     0,     0,     0,     0,   596,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     416,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   619,     0,     0,     0,     0,
       0,     0,     0,     0,   537,     0,     0,     0,     0,   542,
       0,     0,     0,     0,     0,     0,     0,     0,   553,     0,
       0,     0,     0,     0,   639,   639,     0,   560,     0,     0,
       0,     0,   648,   653,     0,   659,     0,   570,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   495,     0,   495,     0,     0,
       0,     0,     0,    85,    88,    90,   681,    95,    97,    99,
     101,   103,   105,   107,   109,   111,   113,   115,   117,   119,
     121,   123,   125,   127,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   156,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   648,     0,   648,     0,     0,     0,     0,
     653,     0,     0,     0,   718,     0,   659,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   212,     0,     0,   214,     0,   216,     0,     0,
       0,     0,   220,     0,   222,     0,   224,     0,   226,     0,
     228,     0,   230,     0,   232,     0,   234,     0,   236,     0,
     238,     0,   240,     0,   242,     0,   244,     0,   246,     0,
     248,     0,   250,     0,   252,   718,     0,     0,     0,     0,
       0,     0,     0,     0,   762,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   505,     0,     5,     0,     0,
      82,     0,     0,   286,    83,    84,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,     0,
       0,    47,    48,    49,    50,    51,    52,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   354,     0,     0,     0,   358,     0,   360,
       0,   362,     0,   364,     0,   366,     0,   368,     0,   370,
       0,     0,     0,     0,     0,   376,     0,   378,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   399,     4,
       0,     5,     0,   403,     0,     0,     0,   407,     0,   411,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,   208,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,     0,     0,    47,    48,    49,    50,    51,
      52,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   465,     0,     0,     0,     0,     0,     0,     0,   472,
       0,     0,     0,   474,     0,   475,     0,     0,     0,     0,
       0,     0,     0,   479,     0,   480,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     489,     0,     0,     0,   496,     0,     0,     0,     0,     0,
     500,     0,     0,     0,     0,     0,   411,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     4,     0,     5,     0,
       0,     0,     0,     0,     0,     0,   514,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,     0,   -60,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
       0,     0,    47,    48,    49,    50,    51,    52,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     4,     0,     5,     0,   594,     0,     0,     0,     0,
       0,   599,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,     0,     0,    47,    48,    49,
      50,    51,    52,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   689
};

static const yytype_int16 yycheck[] =
{
      40,    38,   132,    54,    47,    37,    46,    39,   138,    30,
     169,   170,   260,   261,    47,   442,   443,   191,     1,     3,
       3,     6,   492,     6,     0,   417,     1,     1,     1,     1,
       1,     6,     6,     6,     7,     6,     1,     6,     1,    10,
      11,     6,     1,     8,     9,    10,    11,     6,     1,     8,
       9,    10,    11,     6,     9,    10,     1,    10,    11,     3,
       1,     1,     6,     8,     9,    10,    11,     8,     9,    10,
      11,     8,     9,    10,    11,     1,     8,     9,    10,    11,
       6,     6,     6,     8,     9,    10,    11,     8,     9,    10,
      11,     6,     7,   520,   521,   522,   523,   524,   525,     8,
       9,    10,    11,     1,     1,     1,     1,   115,     6,     6,
       6,     6,    10,    11,    10,    11,   586,     6,   588,   511,
       3,    10,    11,     6,   108,   109,   110,   111,   112,     3,
     115,   171,   115,   116,   117,   118,   108,   109,   110,   111,
     112,   115,   116,   317,   116,   110,   115,     1,     3,     3,
     115,   110,   579,   196,   109,   110,   115,     1,   116,    63,
     118,   193,     6,   196,   109,   110,    10,    11,   109,   110,
       1,   330,   212,   115,   214,     6,   216,   109,   110,    10,
      11,   117,     1,   115,   109,   110,     1,     6,   109,   110,
       1,     6,     3,     1,     3,    10,    11,     6,     6,     1,
     109,   110,    10,    11,     6,     1,   117,     1,    10,    11,
       6,     1,     6,     1,    10,    11,     6,   254,     6,     3,
      10,    11,   262,    34,    35,    36,   117,   267,    39,   269,
      41,    42,    43,    44,    45,     3,    47,    48,    49,    50,
      51,    52,    53,    54,   116,     3,   118,    58,    59,    60,
      61,    62,     1,     6,     7,     1,     3,     6,   298,     3,
       6,   282,   302,     1,    10,    11,   306,   288,     6,     1,
       3,     1,    10,    11,     6,   405,     6,     3,    10,    11,
       1,   529,   530,   442,   443,     6,     1,    34,    35,    36,
     322,     6,    39,     3,    41,    42,    43,    44,    45,     3,
      47,    48,    49,    50,    51,    52,    53,    54,   348,    56,
     350,    58,    59,    60,    61,    62,     1,     3,     1,     3,
       6,     6,   452,     6,     1,    10,    11,    10,    11,     6,
       1,     1,     1,    10,    11,     6,     6,     6,     1,    10,
      11,    10,    11,     6,   116,     1,   118,   387,     9,   389,
       6,   391,     6,     1,    10,    11,    10,    11,     6,     3,
     400,   520,   521,   522,   523,   524,   525,    28,    29,    30,
      31,    32,    33,    34,     1,     3,    37,   417,    39,     6,
      41,    42,    43,    44,    45,     1,     1,     1,   428,    50,
       6,     6,     6,     1,    10,    11,    10,    11,     6,     1,
       9,    10,    10,    11,     6,   111,   112,     3,   448,   108,
     109,   110,   111,   112,     3,     3,     3,   116,     6,   459,
     579,     3,    10,    11,     6,     3,   466,   108,   109,   110,
     111,   112,     8,     9,    10,    11,     3,   477,   478,   479,
     480,   109,   110,   111,   112,   529,   530,   487,   488,     3,
       3,     3,     3,     3,     3,   506,     3,     3,     3,   499,
       3,   501,   115,     3,     3,   115,     3,     3,     3,     3,
       3,     3,     3,   134,     3,     3,     3,     3,     3,   140,
       3,   115,   115,   144,     3,   146,     3,     3,   525,     3,
     151,     3,     3,     3,     3,   117,     3,   117,     3,     3,
       3,     3,   163,     3,     3,   115,     3,    76,     3,     3,
     550,   117,     3,   174,    38,   116,   116,   178,   558,   118,
       3,     3,   183,   117,   564,     3,   566,   188,     3,     3,
     191,   117,   193,   117,   117,     3,   117,   117,   117,   116,
      38,   116,     3,   117,   205,   118,   207,   118,   118,     3,
     118,   212,   592,   214,   117,   595,   117,   218,   598,     3,
       3,     3,   602,   224,     3,     3,     3,   228,     3,   117,
       3,     3,     3,   117,     3,   117,     3,     3,   116,     3,
       3,     3,     3,   244,     3,   246,   116,     3,     3,     3,
       3,   631,     3,   409,   116,   635,   636,   117,   490,   409,
     118,   116,   254,   643,   734,   118,   535,   540,   350,   482,
     118,   651,   118,   463,   744,   549,   117,   657,   783,   586,
     568,   282,   118,   663,   644,   665,   118,   288,   116,   118,
     558,   646,   651,   673,   556,   657,   716,   298,   678,   196,
     769,   334,   183,   317,   134,   311,    45,    -1,   685,    -1,
     311,    -1,    -1,   693,   691,   695,   317,   697,    -1,   699,
     146,   322,   702,   140,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   711,    -1,   334,   714,    -1,    -1,    -1,    -1,    -1,
      -1,   721,    -1,   344,    -1,   178,    -1,   174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   358,    -1,   360,
      -1,    -1,    -1,   364,    -1,    -1,    -1,    -1,    -1,   370,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   757,    -1,   380,
     760,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   771,   772,   773,   395,   775,    -1,    -1,   778,    -1,
      -1,    -1,    -1,   783,    -1,    -1,    -1,    -1,   788,    -1,
     790,    -1,    -1,    -1,    -1,    -1,   417,   797,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   453,    -1,   455,   456,    -1,    -1,    -1,   460,
      -1,    -1,   463,   464,   465,   466,    -1,    -1,   469,    -1,
      -1,   472,    -1,   474,   475,    -1,    -1,    -1,    -1,    -1,
      -1,   482,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   490,
      -1,   492,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   500,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     511,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   526,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   535,    -1,    -1,    -1,    -1,   540,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   549,    -1,
      -1,    -1,    -1,    -1,   555,   556,    -1,   558,    -1,    -1,
      -1,    -1,   563,   564,    -1,   566,    -1,   568,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   586,    -1,   588,    -1,    -1,
      -1,    -1,    -1,     6,     7,     8,   597,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    36,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   644,    -1,   646,    -1,    -1,    -1,    -1,
     651,    -1,    -1,    -1,   655,    -1,   657,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    -1,    -1,    88,    -1,    90,    -1,    -1,
      -1,    -1,    95,    -1,    97,    -1,    99,    -1,   101,    -1,
     103,    -1,   105,    -1,   107,    -1,   109,    -1,   111,    -1,
     113,    -1,   115,    -1,   117,    -1,   119,    -1,   121,    -1,
     123,    -1,   125,    -1,   127,   716,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   725,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,     3,    -1,    -1,
       6,    -1,    -1,   156,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      -1,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   226,    -1,    -1,    -1,   230,    -1,   232,
      -1,   234,    -1,   236,    -1,   238,    -1,   240,    -1,   242,
      -1,    -1,    -1,    -1,    -1,   248,    -1,   250,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   281,     1,
      -1,     3,    -1,   286,    -1,    -1,    -1,   290,    -1,   292,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    -1,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    -1,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   354,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   362,
      -1,    -1,    -1,   366,    -1,   368,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   376,    -1,   378,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     393,    -1,    -1,    -1,   397,    -1,    -1,    -1,    -1,    -1,
     403,    -1,    -1,    -1,    -1,    -1,   409,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   429,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    -1,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    -1,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,     3,    -1,   498,    -1,    -1,    -1,    -1,
      -1,   504,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    -1,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   604
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   120,   121,     0,     1,     3,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    57,    58,    59,
      60,    61,    62,   122,   123,   124,   126,   127,   136,   144,
     150,   159,   168,   180,   183,   197,   200,   203,   207,   211,
     219,   226,   237,   244,   252,   258,   265,   271,   275,   278,
     125,     1,     6,    10,    11,   351,   352,     1,   351,     1,
     351,     1,     6,   350,     1,   351,     1,   351,     1,   351,
       1,   351,     1,   351,     1,   351,     1,   351,     1,   351,
       1,   351,     1,   351,     1,   351,     1,   351,     1,   351,
       1,   351,     1,   351,     1,   351,     1,   351,     1,   146,
       1,   350,   359,     1,   293,   294,   350,     1,   359,     1,
     288,   289,   350,     1,   350,     1,   284,   285,   350,     1,
     301,   302,   350,     1,   305,     1,   351,     1,   349,   350,
       1,   352,     1,   349,     1,     8,     9,    10,    11,   109,
     110,   353,   354,     1,   318,   319,   350,     1,   281,   282,
     350,     1,     7,   334,   335,   349,   350,     1,   347,   350,
       1,   347,     1,   353,     1,     3,   273,   274,   275,   278,
       1,     1,     1,   350,     1,   321,     1,   329,    37,   124,
       3,   160,   351,   169,   351,   151,   351,   152,   154,   184,
     351,   181,   351,   221,   351,   220,   351,   238,   351,   245,
     351,   253,   351,   266,   351,   259,   351,   227,   351,   228,
     351,   229,   351,   204,   351,   208,   351,   201,   351,   198,
     351,   212,   351,   145,   148,   149,   351,   291,     9,    10,
     109,   110,   357,   358,   292,     3,   294,   295,   296,   357,
     287,     3,   289,   290,   128,   350,   283,     3,   285,   286,
     299,   115,   350,   304,     3,   306,   351,   308,    63,   310,
     115,   137,   138,   311,   350,   313,   354,   354,   353,   316,
     317,   319,   117,   280,     3,   282,   117,   332,   333,   335,
     343,   345,   346,   348,   350,   339,     7,   341,   342,   348,
     350,   336,   338,   349,   272,    56,   274,   277,   279,   276,
     117,   320,   350,   328,   330,   331,   350,     3,   350,   353,
       3,   350,   353,     3,   353,     3,   350,     3,   185,     3,
     182,     3,   350,     3,   351,     3,   350,     3,   351,     3,
     351,     3,   351,     3,   351,     3,   351,     3,   351,     3,
     351,     3,   350,     3,   350,     3,   351,     3,   351,     3,
     213,     3,   149,   352,     3,   358,   358,   353,     3,   353,
       3,   353,     3,   115,     3,   129,     3,   115,     3,   351,
     359,     3,     3,   351,     3,   359,   309,   351,     3,   140,
     141,   351,     3,   312,     3,   315,   350,   353,     3,     3,
     353,     3,   353,     3,     3,     3,   344,   346,   115,   115,
       3,   340,   342,     3,   337,   349,     3,     3,     3,     3,
       3,     1,   110,   115,   217,   350,   354,     3,   117,     3,
       3,   331,   117,   162,   161,   172,   170,   157,   350,   155,
     186,   187,   353,   186,   223,   351,   239,    10,    11,   248,
     350,   350,   351,   350,   351,   351,   350,   205,   209,   351,
     351,   214,   215,   216,   350,   147,   353,   353,   353,   351,
     115,   131,   132,   133,   135,   350,   351,   116,   118,   353,
     351,   357,     3,   116,   118,     1,   123,   141,   142,     3,
     315,   353,     3,   353,   351,     3,     3,   217,   217,     3,
     108,   109,   110,   111,   112,   115,   353,     8,    11,   109,
     110,   355,   356,   357,   164,   165,   166,   350,     3,   174,
     175,   176,   350,   178,   350,   153,   158,   353,   188,   189,
     190,   191,   192,   350,   188,   350,   350,   241,   242,   243,
     350,   353,   350,   246,   254,   350,   260,   233,   234,   235,
     350,   350,    76,   353,   353,   353,   353,     3,   216,   117,
       3,     3,   353,   297,   353,   116,   132,   130,   115,   133,
     117,   118,   134,   116,   351,   353,   350,   360,   353,   351,
     139,   315,   118,   116,   118,     1,   116,   218,   217,   217,
     217,   217,   217,     6,    10,    11,   217,   352,   324,   350,
     356,   356,   163,   166,   167,   173,   176,   177,   171,   179,
       3,   117,   156,     3,   191,   115,   353,     3,   225,   350,
     225,   240,   243,   117,   247,   249,   250,   251,   350,   353,
     255,   256,   257,   350,   353,   267,   262,   263,   264,   350,
     353,   232,   235,   117,   231,   117,   206,   210,   202,   199,
     217,     3,   298,   117,   218,     3,   133,   353,   117,   116,
     353,   350,   361,   353,   116,    38,   143,   314,   353,   351,
     116,   118,   322,   117,     3,   117,     3,   117,     3,   117,
     353,     3,   353,   353,   224,   222,     3,   353,   249,     3,
     251,   117,     3,   257,   117,   268,   269,   270,   350,   261,
     264,   117,     3,   353,     3,   353,     3,     3,     3,     3,
       3,   353,   116,   353,   300,   307,     3,   352,     3,   116,
     116,   352,   325,   327,   357,   353,   353,   353,   353,   353,
       3,     3,     3,   353,   353,     3,   270,   117,     3,   353,
     118,   236,   350,   303,   357,     3,     3,   116,   323,   326,
     357,   193,   118,   118,   353,   118,   353,   230,   117,     3,
       3,   327,   194,   195,   196,   353,   353,   353,   118,   353,
     118,     3,   353,   116,   196,   353,   353,   118,   353
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   119,   121,   120,   122,   123,   123,   124,   125,   124,
     124,   124,   124,   124,   124,   124,   124,   124,   124,   126,
     126,   126,   126,   126,   126,   126,   126,   126,   126,   126,
     126,   126,   126,   126,   128,   127,   129,   130,   127,   131,
     131,   131,   132,   132,   132,   133,   133,   134,   134,   134,
     135,   137,   136,   138,   139,   136,   140,   140,   141,   142,
     142,   143,   143,   145,   144,   146,   147,   144,   148,   148,
     149,   151,   150,   152,   150,   153,   150,   154,   155,   156,
     150,   157,   158,   157,   160,   159,   161,   159,   162,   163,
     159,   164,   164,   165,   165,   167,   166,   169,   168,   170,
     171,   168,   172,   173,   168,   174,   174,   175,   175,   177,
     176,   178,   179,   178,   181,   180,   182,   180,   184,   183,
     185,   183,   186,   186,   187,   188,   188,   189,   189,   190,
     191,   191,   191,   191,   193,   192,   194,   194,   195,   195,
     196,   198,   197,   199,   197,   201,   200,   202,   200,   204,
     203,   205,   206,   203,   208,   207,   209,   210,   207,   212,
     211,   213,   211,   214,   214,   215,   215,   216,   217,   217,
     217,   217,   217,   217,   217,   217,   217,   217,   217,   217,
     218,   218,   220,   219,   221,   219,   222,   219,   223,   224,
     219,   225,   225,   227,   226,   228,   226,   229,   226,   230,
     226,   231,   226,   232,   226,   233,   233,   234,   234,   235,
     235,   236,   236,   238,   237,   239,   240,   237,   241,   241,
     242,   242,   243,   243,   243,   245,   244,   246,   244,   247,
     244,   248,   248,   249,   249,   250,   250,   251,   251,   251,
     251,   253,   252,   254,   252,   255,   255,   256,   256,   257,
     257,   257,   257,   259,   258,   260,   261,   258,   262,   262,
     263,   263,   264,   264,   264,   266,   265,   267,   265,   268,
     268,   269,   269,   270,   270,   270,   272,   271,   271,   273,
     273,   274,   274,   274,   276,   275,   275,   277,   275,   279,
     278,   280,   275,   275,   281,   281,   282,   282,   283,   275,
     275,   284,   284,   286,   285,   287,   275,   275,   288,   288,
     290,   289,   291,   275,   275,   292,   275,   275,   293,   293,
     295,   294,   296,   275,   297,   275,   298,   275,   299,   275,
     300,   275,   301,   302,   302,   303,   303,   304,   275,   305,
     275,   306,   275,   307,   275,   308,   275,   309,   275,   310,
     310,   311,   275,   312,   275,   313,   275,   275,   275,   314,
     275,   315,   315,   316,   275,   317,   275,   318,   318,   319,
     320,   275,   321,   322,   323,   275,   324,   324,   325,   326,
     325,   327,   328,   275,   329,   275,   330,   330,   331,   331,
     332,   278,   333,   278,   334,   334,   335,   335,   335,   336,
     278,   337,   278,   338,   338,   339,   278,   340,   278,   341,
     341,   342,   342,   342,   343,   278,   344,   278,   345,   345,
     346,   346,   347,   348,   348,   349,   349,   350,   351,   352,
     352,   352,   353,   353,   353,   354,   354,   354,   354,   355,
     355,   355,   356,   356,   357,   357,   357,   358,   358,   359,
     360,   361
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     2,     1,     0,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     4,     0,     0,     7,     0,
       1,     3,     1,     2,     4,     3,     1,     0,     1,     1,
       1,     0,     4,     0,     0,     7,     1,     2,     1,     1,
       1,     2,     3,     0,     4,     0,     0,     6,     1,     2,
       1,     0,     4,     0,     4,     0,     7,     0,     0,     0,
       8,     0,     0,     4,     0,     4,     0,     6,     0,     0,
       8,     0,     1,     1,     2,     0,     4,     0,     4,     0,
       0,     8,     0,     0,     8,     0,     1,     1,     2,     0,
       4,     0,     0,     4,     0,     4,     0,     7,     0,     4,
       0,     7,     0,     1,     1,     0,     1,     1,     2,     1,
       1,     2,     3,     1,     0,     7,     0,     1,     1,     2,
       1,     0,     4,     0,     8,     0,     4,     0,     8,     0,
       4,     0,     0,     8,     0,     4,     0,     0,     8,     0,
       4,     0,     6,     0,     1,     1,     2,     3,     1,     1,
       1,     2,     3,     3,     3,     3,     3,     3,     4,     6,
       1,     1,     0,     4,     0,     4,     0,     9,     0,     0,
       9,     0,     1,     0,     4,     0,     4,     0,     4,     0,
      11,     0,     8,     0,     8,     0,     1,     1,     2,     3,
       9,     0,     3,     0,     4,     0,     0,     8,     0,     1,
       1,     2,     1,     1,     3,     0,     4,     0,     8,     0,
       9,     1,     1,     0,     1,     1,     2,     1,     1,     3,
       5,     0,     4,     0,     8,     0,     1,     1,     2,     1,
       1,     3,     5,     0,     4,     0,     0,     9,     0,     1,
       1,     2,     1,     1,     5,     0,     4,     0,     9,     0,
       1,     1,     2,     1,     3,     5,     0,     4,     4,     1,
       2,     1,     1,     1,     0,     4,     5,     0,     4,     0,
       4,     0,     4,     3,     1,     2,     1,     3,     0,     4,
       3,     1,     2,     0,     7,     0,     4,     3,     1,     2,
       0,     7,     0,     4,     6,     0,     4,     3,     1,     2,
       0,     5,     0,     4,     0,     7,     0,     8,     0,     4,
       0,    10,     1,     4,     6,     0,     1,     0,     4,     0,
       3,     0,     4,     0,     9,     0,     4,     0,     5,     0,
       5,     0,     4,     0,     5,     0,     4,     4,     5,     0,
       8,     0,     1,     0,     4,     0,     4,     1,     2,     3,
       0,     4,     0,     0,     0,    10,     0,     3,     1,     0,
       3,     2,     0,     4,     0,     4,     1,     2,     3,     3,
       0,     4,     0,     4,     1,     2,     1,     1,     1,     0,
       4,     0,     5,     1,     2,     0,     4,     0,     5,     1,
       2,     1,     1,     1,     0,     4,     0,     5,     1,     2,
       1,     6,     1,     4,     6,     4,     6,     1,     1,     1,
       1,     1,     1,     2,     2,     1,     1,     1,     1,     1,
       2,     2,     1,     1,     1,     2,     2,     1,     1,     1,
       1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 175 "shpars.y" /* yacc.c:1646  */
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
#line 2350 "y.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 208 "shpars.y" /* yacc.c:1646  */
    { CheckCircBrackets(); }
#line 2356 "y.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 212 "shpars.y" /* yacc.c:1646  */
    { MsgConsole( msgPASSCOMPLETE, PassNo );
                   cnt_END++; /* .END indicator */
                   CheckCircBrackets();
                   return cntErrors;
                 }
#line 2366 "y.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 230 "shpars.y" /* yacc.c:1646  */
    { TEST_EOF;
                    CLR_ELEM_PARM;
                  }
#line 2374 "y.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 234 "shpars.y" /* yacc.c:1646  */
    { TEST_EOF;
                    CLR_ELEM_PARM;
                  }
#line 2382 "y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 241 "shpars.y" /* yacc.c:1646  */
    { TEST_EOF; }
#line 2388 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 242 "shpars.y" /* yacc.c:1646  */
    { TEST_EOF;
                   MsgCompile( msgERRSTMT );
                   YERRSKIP;
                 }
#line 2397 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 283 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_MODEL );
                   YERRSKIP;
                 }
#line 2405 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 289 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "Model '%s' (type '%s')", (yyvsp[-1].strptr), (yyvsp[0].strptr) );
                   modelCurr = NULL;
                   if( PassNo == 1 ){
                     listModels = AddToModelList( (yyvsp[-1].strptr), (yyvsp[0].strptr), circDefCurrent );
                   }
                   modelCurr = FindModel( (yyvsp[-1].strptr), circDefCurrent );
                 }
#line 2417 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 297 "shpars.y" /* yacc.c:1646  */
    {
                   modelCurr = NULL;
                 }
#line 2425 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 314 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... model parameter '%s' = %g", (yyvsp[-2].strptr), (yyvsp[0].floatnum) );
                   if( PassNo == 1 ){
                     if( FindModelParm( modelCurr, (yyvsp[-2].strptr) ) )
                       MsgCompile(  msgMODELPARMEXIST, (yyvsp[-2].strptr), modelCurr->modelname );
                     else
                       AddModelParm( modelCurr, (yyvsp[-2].strptr), TRUE, (yyvsp[0].floatnum) );
                   }
                 }
#line 2438 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 322 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... model parameter '%s'", (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( FindModelParm( modelCurr, (yyvsp[0].strptr) ) )
                       MsgCompile(  msgMODELPARMEXIST, (yyvsp[0].strptr), modelCurr->modelname );
                     else
                       AddModelParm( modelCurr, (yyvsp[0].strptr), FALSE, 0.0 );
                   }
                 }
#line 2451 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 341 "shpars.y" /* yacc.c:1646  */
    { (yyval.strptr) = (yyvsp[0].strptr); }
#line 2457 "y.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 377 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_SUBCKT );
                    YERRSKIP;
                    cnt_SUBCKT++; /* counter of circuit brackets */
                 }
#line 2466 "y.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 384 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "Begin subcircuit '%s' definition body...", (yyvsp[0].strptr) );
                   SHOWCIRCSTATE( "Pass1,2: before process '.subckt'" );/**/

                   isSubDefBody = TRUE;
                   levelSubDef++;
                   PushCircStack();

                   if( PassNo == 1 ){
                     SubCirc *circThis;
                     cnt_SUBCKT++; /* counter of circuit brackets */
                     circThis = FindSubCirc( listSubCirc,
                                             circDefCurrent/*parent*/, (yyvsp[0].strptr) );
                     if( circThis != NULL ){
                       MsgCompile( msgCIRCDEFEXIST, (yyvsp[0].strptr), (circDefCurrent==NULL)?
                                     msgMainCircuit: circDefCurrent->circname);
                     /*MsgErrorFatal( exitPARSEREXIT, msgPARSEREXIT );/*exit*/
                       /* if not exit here, parser give for this circuit */
                       /* many errors about elements with equal names */
                     }else{ /* not found: add to list */
                       Ydebug( "Pass1: add to list subcirc def '%s', 1st line#", (yyvsp[0].strptr) );
                       listSubCirc = AddSubCirc( listSubCirc,
                                                 circDefCurrent/*parent*/, (yyvsp[0].strptr) );
                       circThis = FindSubCirc( listSubCirc,
                                               circDefCurrent/*parent*/, (yyvsp[0].strptr) );
                     }
                     circDefCurrent = circThis;
                   }/*pass1*/

                   if( PassNo == 2 ){
                     SubCirc *circThis;
                /*   circThis = FindSubCirc( listSubCirc,
                         circDefCurrent/*parent*\, $2 ); /* in current */
                /**/ circThis = FindSubCircHigh( listSubCirc,
                         circDefCurrent/*parent*/, (yyvsp[0].strptr) ); /* in current and in parents */
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
#line 2522 "y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 437 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... end of body subcircuit '%s'.", (yyvsp[-3].strptr) );

                   if( PassNo == 1 ){
                     SHOWCIRCSTATE( "Pass1: before process '.ends'" );/**/
                     PopCircStack();
                     SHOWCIRCSTATE( "Pass1: after restore subcirc states" );/**/

                     if( Flags[flagG].value || Flags[flag1].value || Flags[flag2].value )
                     { MsgDebug( "circDefCurrent: " );
                       OutFullCircName( fileDebug, circDefCurrent );
                       MsgDebug( "\n" );
                     }
                     Ydebug( "Pass1: save last line# for .ENDS '%s'", (yyvsp[-3].strptr) );
                     if( AddSubCircEnd( listSubCirc, circDefCurrent/*->parent*/, (yyvsp[-3].strptr) )
                         == FALSE )
                       MsgCompile( msgERR_ENDS, (yyvsp[-3].strptr) );
                     else { /* test if .subckt and .ends in same file: */
                       SubCirc *cr;
                       cr = FindSubCirc( listSubCirc, circDefCurrent/*->parent*/, (yyvsp[-3].strptr) );
                       if( !strEQ( cr->begfname, cr->endfname ) )
                         MsgCompile( msgCIRCNOTIN1FILE, (yyvsp[-3].strptr) );
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
#line 2590 "y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 501 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... end of subcircuit '%s' detected.", (yyvsp[-5].strptr) ); }
#line 2596 "y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 505 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... subcircuit node '%s'", (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( strEQ( (yyvsp[0].strptr), "0" ) )
                         MsgCompile( msgSUBCIRCPARM0 );
                     /* save local node-parameter for 'circDefCurrent': */
                     ADD_NODE_SUBCIRCPARM( (yyvsp[0].strptr), nodtypSubCirc,
                                           circDefCurrent->circname );
                   }/*pass2*/
                 }
#line 2610 "y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 516 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_SUBCKTSTMT );
                    YERRSKIP;
                 }
#line 2618 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 523 "shpars.y" /* yacc.c:1646  */
    { if( PassNo == 1 ) cnt_ENDS++; /* counter of circuit brackets */ }
#line 2624 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 525 "shpars.y" /* yacc.c:1646  */
    { if( PassNo == 1 ) cnt_ENDS++; /* counter of circuit brackets */ }
#line 2630 "y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 530 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRSUBCALL, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 2638 "y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 534 "shpars.y" /* yacc.c:1646  */
    { namePrepCall = (yyvsp[0].strptr);
                   if( PassNo == 2 ){
                       /* free subcall parameters list: */
                       DelParmSubCall( parmSubCall );
                       parmSubCall = NULL;
                       cntParmSubCall = 0;
                   }
                 }
#line 2651 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 544 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "Call '%s' of subcircuit '%s'", (yyvsp[-3].strptr), (yyvsp[0].strptr) );

                   if( PassNo == 1 ){
                     listX = AddElem( listX, &cntX, (yyvsp[-3].strptr), circDefCurrent ); /* add to list */
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
                         circDefCurrent/*parent*/, (yyvsp[0].strptr) ); /* in current and in parents */
                       if( circPrepCall == NULL ) /* subcirc definition not found */
                         MsgCompile( msgSUBCKTNOTEXIST, (yyvsp[0].strptr) );
                       else{
                         /* called subcirc found: */

                         Ydebug( "Pass2:  process call '%s' of subcircuit '%s'...",
                                                        (yyvsp[-3].strptr),                (yyvsp[0].strptr) );
                         if( Flags[flagG].value ){ /* debug info: */
                           /* from wich surcuit process call: */
                           MsgDebug( "\t\t\tcall from " );
                           OutFullCircName( fileDebug, circDefCurrent );
                           MsgDebug( "\n" );
                           /* debug print fact parameters: */
                           MsgDebug( "\t\t(%ld) nodes-parameters in call '%s' of '%s': ",
                                      cntParmSubCall, (yyvsp[-3].strptr), (yyvsp[0].strptr) );
                           PrintSubParm( parmSubCall );
                         }
                         /* compare number of parameters with subcirc definition: */
                         if( cntParmSubCall != circPrepCall->cntnodesparm){
                           MsgCompile( msgCNTPARMSUBCAL,
                               circPrepCall->cntnodesparm, (yyvsp[-3].strptr), (yyvsp[0].strptr), cntParmSubCall );
                         } else
                           /* rename local nodes: */
                         if( RenameLocalNodes( circPrepCall, circDefCurrent,
                                               parmSubCall ) == FALSE )
                             MsgCompile( msgPARMSETSUBCAL, (yyvsp[-3].strptr) );
                         else { /* generate call: */

                           /* save state: */
/*                         SHOWCIRCSTATE( "Pass2: subcall processing: before saving states" );/**/
                           PushCircStack();
                           /* new state: process of call subcircuit */
                           isSubCall = TRUE;
                           levelSubCall++;
                           circCall = circPrepCall;
                           SHOWCIRCSTATE( "Pass2: subcall processing: after saving states" );/**/

                           AddCallPath( (yyvsp[-3].strptr) ); /* add name to subcall path */

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
#line 2754 "y.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 646 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... subcircuit node '%s'", (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( (yyvsp[0].strptr), nodtypUNKNOWN, namePrepCall );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[0].strptr), nodtypUNKNOWN, namePrepCall );
                     }
                   }/*pass1*/

                   if( PassNo == 2 ){
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       /* append node-parameter to parameters list: */
                       parmSubCall = AddSubParm( parmSubCall, (yyvsp[0].strptr), &cntParmSubCall );
                     }
                   }/*pass2*/
                 }
#line 2778 "y.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 670 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRINDUCTOR, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 2786 "y.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 676 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRCOUPINDUC, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 2794 "y.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 682 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Inductor '%s' = %g Henries",
                             (yyvsp[-3].strptr),(yyvsp[-2].strptr),           (yyvsp[-4].strptr),   (yyvsp[-1].floatnum) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypUNKNOWN, (yyvsp[-4].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypUNKNOWN, (yyvsp[-4].strptr) );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypUNKNOWN, (yyvsp[-4].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypUNKNOWN, (yyvsp[-4].strptr) );
                     }
                     listL = AddElem( listL, &cntL, (yyvsp[-4].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/

                   if( PassNo == 2 ){
                     SHOWCIRCSTATE( "test if generate code for Inductor:" );/**/
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) )
                     { /* generate code: */
                       Ydebug( "Generate code for Inductor '%s'...", (yyvsp[-4].strptr) );
                       PrintElemsTransl( (yyvsp[-4].strptr), circDefCurrent, listL );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s",
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-3].strptr) ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-2].strptr) )
                              );
                       HabELEMout(
                         /*NE=  */GetHabElemName( (yyvsp[-4].strptr), circDefCurrent, listL ),
                         /*KNOT=*/linebuffer,
                         /*PAR= */ftoa((yyvsp[-1].floatnum)),
                         /*IDOP=*/"",
                         /*ISTR=*/""
                                 );
                     }
                   }/*pass2*/
                 }
#line 2839 "y.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 726 "shpars.y" /* yacc.c:1646  */
    { if( toupper( (yyvsp[0].strptr)[0] ) != 'L' ){
                     MsgCompile( msgNEEDINDUCTOR, (yyvsp[0].strptr) );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 2849 "y.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 732 "shpars.y" /* yacc.c:1646  */
    { if( toupper( (yyvsp[0].strptr)[0] ) != 'L' ){
                     MsgCompile( msgNEEDINDUCTOR, (yyvsp[0].strptr) );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 2859 "y.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 738 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "find Coupled Inductor '%s' (%s,%s) = %g Henries",
                                                   (yyvsp[-5].strptr),  (yyvsp[-4].strptr),(yyvsp[-2].strptr),   (yyvsp[0].floatnum) );
                 }
#line 2867 "y.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 746 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[0].strptr), "IC", "" );
                 }
#line 2875 "y.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 749 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... inductor option IC = %g", (yyvsp[0].floatnum) );
                 }
#line 2882 "y.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 756 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRRESISTOR, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 2890 "y.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 762 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Resistor '%s' = %g Ohms",
                             (yyvsp[-2].strptr),(yyvsp[-1].strptr),           (yyvsp[-3].strptr),   (yyvsp[0].floatnum) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypUNKNOWN, (yyvsp[-3].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypUNKNOWN, (yyvsp[-3].strptr) );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypUNKNOWN, (yyvsp[-3].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypUNKNOWN, (yyvsp[-3].strptr) );
                     }
                     listR = AddElem( listR, &cntR, (yyvsp[-3].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                   if( PassNo == 2 ){
                     SHOWCIRCSTATE( "test if generate code for Resistor:" );/**/
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) )
                     { /* generate code: */
                       Ydebug( "Generate code for Resistor '%s'...", (yyvsp[-3].strptr) );
                       PrintElemsTransl( (yyvsp[-3].strptr), circDefCurrent, listR );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s",
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-2].strptr) ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-1].strptr) )
                              );
                       HabELEMout(
                         /*NE=  */GetHabElemName( (yyvsp[-3].strptr), circDefCurrent, listR ),
                         /*KNOT=*/linebuffer,
                         /*PAR= */ftoa((yyvsp[0].floatnum)),
                         /*IDOP=*/"",
                         /*ISTR=*/""
                                 );
                     }
                   }/*pass2*/
                 }
#line 2934 "y.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 804 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Resistor '%s' (model '%s')",
                             (yyvsp[-2].strptr),(yyvsp[-1].strptr),           (yyvsp[-3].strptr),         (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypUNKNOWN, (yyvsp[-3].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypUNKNOWN, (yyvsp[-3].strptr) );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypUNKNOWN, (yyvsp[-3].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypUNKNOWN, (yyvsp[-3].strptr) );
                     }
                     listR = AddElem( listR, &cntR, (yyvsp[-3].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/

                   if( PassNo == 2 ){
		     MsgCompileWarn( warn1_INFO, msgNotProcSemiR, (yyvsp[-3].strptr) );
                   }/*pass2*/
                 }
#line 2959 "y.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 825 "shpars.y" /* yacc.c:1646  */
    {
                   /* in Habala no semiResistor ! */
                 }
#line 2967 "y.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 834 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "L", "W", "TEMP", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[0].strptr), "L", "W", "TEMP", "" );
                 }
#line 2975 "y.tab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 837 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... resistor option '%s' = %g", (yyvsp[-3].strptr), (yyvsp[0].floatnum) );
                 }
#line 2982 "y.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 844 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRCAPACITOR, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 2990 "y.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 850 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Capacitor '%s' = %g Farads",
                             (yyvsp[-2].strptr),(yyvsp[-1].strptr),            (yyvsp[-3].strptr),   (yyvsp[0].floatnum) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypPLUS, (yyvsp[-3].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypMINUS, (yyvsp[-3].strptr) );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypPLUS, (yyvsp[-3].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypMINUS, (yyvsp[-3].strptr) );
                     }
                     listC = AddElem( listC, &cntC, (yyvsp[-3].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 3011 "y.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 867 "shpars.y" /* yacc.c:1646  */
    {
                   if( PassNo == 2 ){
                     SHOWCIRCSTATE( "test if generate code for Capacitor:" );/**/
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) )
                     { /* generate code: */
                       Ydebug( "Generate code for Capacitor '%s'...", (yyvsp[-5].strptr) );
                       PrintElemsTransl( (yyvsp[-5].strptr), circDefCurrent, listC );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s",
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-4].strptr) ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-3].strptr) )
                              );
                       HabELEMout(
                         /*NE=  */GetHabElemName( (yyvsp[-5].strptr), circDefCurrent, listC ),
                         /*KNOT=*/linebuffer,
                         /*PAR= */ftoa((yyvsp[-2].floatnum)),
                         /*IDOP=*/"",
                         /*ISTR=*/""
                                 );
                     }
                   }/*pass2*/
                 }
#line 3041 "y.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 895 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Capacitor '%s' (model '%s')",
                             (yyvsp[-2].strptr),(yyvsp[-1].strptr),            (yyvsp[-3].strptr),         (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypPLUS, (yyvsp[-3].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypMINUS, (yyvsp[-3].strptr) );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypPLUS, (yyvsp[-3].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypMINUS, (yyvsp[-3].strptr) );
                     }
                     listC = AddElem( listC, &cntC, (yyvsp[-3].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/

                   if( PassNo == 2 ){
		     MsgCompileWarn( warn1_INFO, msgNotProcSemiC, (yyvsp[-3].strptr) );
                   }/*pass2*/
                 }
#line 3066 "y.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 916 "shpars.y" /* yacc.c:1646  */
    {
                   /* in Habala no semiCapacitor ! */
                 }
#line 3074 "y.tab.c" /* yacc.c:1646  */
    break;

  case 109:
#line 925 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "L", "W", "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[0].strptr), "L", "W", "IC", "" );
                 }
#line 3082 "y.tab.c" /* yacc.c:1646  */
    break;

  case 110:
#line 928 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... capacitor option '%s' = %g", (yyvsp[-3].strptr), (yyvsp[0].floatnum) );
                 }
#line 3089 "y.tab.c" /* yacc.c:1646  */
    break;

  case 112:
#line 934 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[0].strptr), "IC", "" );
                 }
#line 3097 "y.tab.c" /* yacc.c:1646  */
    break;

  case 113:
#line 937 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... capacitor option IC = %g", (yyvsp[0].floatnum) );
                 }
#line 3104 "y.tab.c" /* yacc.c:1646  */
    break;

  case 114:
#line 944 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRVOLTAGE, (yyvsp[-1].strptr) );
                   YERRSKIP;
                   isVxxx = isIVxxxOpt = FALSE;
                   IxxxName = VxxxName = NULL;
                 }
#line 3114 "y.tab.c" /* yacc.c:1646  */
    break;

  case 116:
#line 952 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Voltage '%s'",
                             (yyvsp[-1].strptr),(yyvsp[0].strptr),          (yyvsp[-2].strptr) );
                   VxxxName = (yyvsp[-2].strptr);
                   isVxxx = TRUE;
                   /* make virtual resistor Ri name: */
                   RiVxxxName = (char *)malloc( ( strlen((yyvsp[-2].strptr)) + 2 ) * sizeof(char) );
                   if( RiVxxxName == NULL )
                     MsgErrorFatal( exitERRNOMEM, msgERRMEMALLOC );
                   else {
                     RiVxxxName[0] = 'R';
                     strcpy( &RiVxxxName[1], (yyvsp[-2].strptr) );
                     RiVxxxName[strlen(RiVxxxName)] = '\0';
                   }
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypPLUS, (yyvsp[-2].strptr) );
                       ADD_NODE_MAIN( (yyvsp[0].strptr), nodtypMINUS, (yyvsp[-2].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypPLUS, (yyvsp[-2].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[0].strptr), nodtypMINUS, (yyvsp[-2].strptr) );
                     }
                     listV = AddElem( listV, &cntV, (yyvsp[-2].strptr), circDefCurrent ); /* add to list */
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
                       Ydebug( "Generate code for Ri for '%s'...", (yyvsp[-2].strptr) );
                       /* _no_ rename virtual Ri: */
                       /* renR = Rename1elem( RiVxxxName, listR, circCall, renR ); */
                       PrintElemsTransl( RiVxxxName, circDefCurrent, listR );
                                    /* out to elements names tranlations file */
                       /* save habala-nodes list to 'linebuffer' */
                       sprintf( linebuffer, "%s, %s",
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-1].strptr) ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[0].strptr) )
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
#line 3178 "y.tab.c" /* yacc.c:1646  */
    break;

  case 117:
#line 1016 "shpars.y" /* yacc.c:1646  */
    { isVxxx = isIVxxxOpt = FALSE;
                   IxxxName = VxxxName = NULL;
                 }
#line 3186 "y.tab.c" /* yacc.c:1646  */
    break;

  case 118:
#line 1024 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRCURRENT, (yyvsp[-1].strptr) );
                   YERRSKIP;
                   isIxxx = isIVxxxOpt = FALSE;
                   IxxxName = VxxxName = NULL;
                 }
#line 3196 "y.tab.c" /* yacc.c:1646  */
    break;

  case 120:
#line 1032 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Current '%s'",
                             (yyvsp[-1].strptr),(yyvsp[0].strptr),          (yyvsp[-2].strptr) );
                   IxxxName = (yyvsp[-2].strptr);
                   isIxxx = TRUE;
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypPLUS, (yyvsp[-2].strptr) );
                       ADD_NODE_MAIN( (yyvsp[0].strptr), nodtypMINUS, (yyvsp[-2].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypPLUS, (yyvsp[-2].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[0].strptr), nodtypMINUS, (yyvsp[-2].strptr) );
                     }
                     listI = AddElem( listI, &cntI, (yyvsp[-2].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                   if( PassNo == 2 ){
                     /* put to 'linebuffer' list of nodes: */
                     sprintf( linebuffer, "%s, %s",
                       GetHabNodeName( isSubCall?circCall->circnodlist
                                                :listNodesMain, (yyvsp[-1].strptr) ),
                       GetHabNodeName( isSubCall?circCall->circnodlist
                                                :listNodesMain, (yyvsp[0].strptr) )
                            );
                   }/*pass2*/
                 }
#line 3226 "y.tab.c" /* yacc.c:1646  */
    break;

  case 121:
#line 1062 "shpars.y" /* yacc.c:1646  */
    { isIxxx = isIVxxxOpt = FALSE;
                   IxxxName = VxxxName = NULL;
                 }
#line 3234 "y.tab.c" /* yacc.c:1646  */
    break;

  case 124:
#line 1069 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... voltage/current (DC) option (%g)", (yyvsp[0].floatnum) );
                   if( PassNo == 2 ){
                     /* get habala parameters: */
                     if( isIxxx )
                       IVxxxParmFromDC( &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                                        (yyvsp[0].floatnum)
                       );
                     else if( isVxxx )
                       IVxxxParmFromDC( &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                                        (yyvsp[0].floatnum)/VxxxRi
                       );
                     /* generate code for option: */
                     MAKEELEM_IVxxx; /* generate code */
                   }/*pass2*/
                   isIVxxxOpt = FALSE;
                 }
#line 3255 "y.tab.c" /* yacc.c:1646  */
    break;

  case 127:
#line 1089 "shpars.y" /* yacc.c:1646  */
    { /* generate code for option: */
                   if( PassNo == 2 && isIVxxxOpt ){ /* valid option */
                     MAKEELEM_IVxxx; /* generate code */
                   }/*pass2*/
                   isIVxxxOpt = FALSE;
                 }
#line 3266 "y.tab.c" /* yacc.c:1646  */
    break;

  case 128:
#line 1097 "shpars.y" /* yacc.c:1646  */
    { /* generate code for option: */
                   if( PassNo == 2 && isIVxxxOpt ){ /* valid option */
                     MAKEELEM_IVxxx; /* generate code */
                   }/*pass2*/
                   isIVxxxOpt = FALSE;
                 }
#line 3277 "y.tab.c" /* yacc.c:1646  */
    break;

  case 129:
#line 1106 "shpars.y" /* yacc.c:1646  */
    { if( CheckKeyword( (yyvsp[0].strptr),
                          "DC", "AC", "DISTOF1", "DISTOF2",
                          "PULSE", "SIN", "EXP", "SFFM", "PWL", "" )
                   ){
                     (yyval.strptr) = (yyvsp[0].strptr);
                   }else{
                     MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[0].strptr),
                          "DC", "AC", "DISTOF1", "DISTOF2",
                          "PULSE", "SIN", "EXP", "SFFM", "PWL", "" );
                   }
                   isIVxxxOpt = FALSE;
                 }
#line 3294 "y.tab.c" /* yacc.c:1646  */
    break;

  case 130:
#line 1122 "shpars.y" /* yacc.c:1646  */
    {
                   if( !CheckKeyword( (yyvsp[0].strptr), "AC", "DISTOF1", "DISTOF2", "" ) )
                     MsgCompile( msgILLOPTION, (yyvsp[0].strptr) );
                   else {
                     Ydebug( "... voltage/current '%s' option (%g,%g)", (yyvsp[0].strptr), 0.0, 0.0 );
                     if( PassNo == 2 ){
                       int DISTOFno;
                       if( CheckKeyword( (yyvsp[0].strptr), "DISTOF1", "" ) )      DISTOFno = 1;
                       else if( CheckKeyword( (yyvsp[0].strptr), "DISTOF2", "" ) ) DISTOFno = 2;
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
#line 3325 "y.tab.c" /* yacc.c:1646  */
    break;

  case 131:
#line 1150 "shpars.y" /* yacc.c:1646  */
    {
                   if( CheckKeyword( (yyvsp[-1].strptr), "AC", "DISTOF1", "DISTOF2", "" ) ){
                     Ydebug( "... voltage/current '%s' option (%g,%g)", (yyvsp[-1].strptr), (yyvsp[0].floatnum), 0.0 );
                     if( PassNo == 2 ){
                       int DISTOFno;
                       if( CheckKeyword( (yyvsp[-1].strptr), "DISTOF1", "" ) )      DISTOFno = 1;
                       else if( CheckKeyword( (yyvsp[-1].strptr), "DISTOF2", "" ) ) DISTOFno = 2;
                       else                                         DISTOFno = 0;
                       if( DISTOFno ){ /* get habala parameters: */
                         if( isIxxx )
                           IVxxxParmFromDISTOF(
                              &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                              DISTOFno, (yyvsp[0].floatnum), 0.0
                           );
                         else if( isVxxx )
                           IVxxxParmFromDISTOF(
                              &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                              DISTOFno, (yyvsp[0].floatnum)/VxxxRi, 0.0 - 90.0 /*ASB000623*/
                           );
                         isIVxxxOpt = TRUE; /* processed option */
                       }
                     }/*pass2*/
                   }else if( CheckKeyword( (yyvsp[-1].strptr), "DC", "" ) ){
                     Ydebug( "... voltage/current '%s' option (%g)", (yyvsp[-1].strptr), (yyvsp[0].floatnum) );
                     if( PassNo == 2 ){
                       /* get habala parameters: */
                       if( isIxxx )
                         IVxxxParmFromDC( &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                                          (yyvsp[0].floatnum)
                         );
                       else if( isVxxx )
                         IVxxxParmFromDC( &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                                          (yyvsp[0].floatnum)/VxxxRi
                         );
                       isIVxxxOpt = TRUE; /* processed option */
                     }
                   }else
                     MsgCompile( msgILLOPTION, (yyvsp[-1].strptr) );
                 }
#line 3369 "y.tab.c" /* yacc.c:1646  */
    break;

  case 132:
#line 1192 "shpars.y" /* yacc.c:1646  */
    {
                   if( !CheckKeyword( (yyvsp[-2].strptr), "AC", "DISTOF1", "DISTOF2", "" ) )
                     MsgCompile( msgILLOPTION, (yyvsp[-2].strptr) );
                   else{
                     Ydebug( "... voltage/current '%s' option (%g,%g)", (yyvsp[-2].strptr), (yyvsp[-1].floatnum), (yyvsp[0].floatnum) );
                     if( PassNo == 2 ){
                       int DISTOFno;
                       if( CheckKeyword( (yyvsp[-2].strptr), "DISTOF1", "" ) )      DISTOFno = 1;
                       else if( CheckKeyword( (yyvsp[-2].strptr), "DISTOF2", "" ) ) DISTOFno = 2;
                       else                                         DISTOFno = 0;
                       if( DISTOFno ){ /* get habala parameters: */
                         if( isIxxx )
                           IVxxxParmFromDISTOF(
                              &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                              DISTOFno, (yyvsp[-1].floatnum), (yyvsp[0].floatnum)
                           );
                         else if( isVxxx )
                           IVxxxParmFromDISTOF(
                              &IVxxxGi, &IVxxxF, &IVxxxFi, &IVxxxJm,
                              DISTOFno, (yyvsp[-1].floatnum)/VxxxRi, (yyvsp[0].floatnum) - 90.0 /*ASB000623*/
                           );
                         isIVxxxOpt = TRUE; /* processed option */
                       }
                     }/*pass2*/
                   }
                 }
#line 3400 "y.tab.c" /* yacc.c:1646  */
    break;

  case 134:
#line 1223 "shpars.y" /* yacc.c:1646  */
    {
                   if( !CheckKeyword( (yyvsp[-3].strptr), "PULSE", "SIN", "SFFM", "EXP", "PWL", "" ) ){
                       MsgAllowKeywords( msgEXPECTFUNNAME, strupr((yyvsp[-3].strptr)),
                           "PULSE()", "SIN()", "SFFM()", "EXP()", "PWL()", "" );
                       SKIPTOEOLN; /*SkipRestLine();/*?*/
                   } else
                       Ydebug( "... voltage/current function %s(%g,%g,...)",
                                                             (yyvsp[-3].strptr),(yyvsp[-1].floatnum),(yyvsp[0].floatnum) );
                 }
#line 3414 "y.tab.c" /* yacc.c:1646  */
    break;

  case 135:
#line 1234 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... end of voltage/current function %s()", (yyvsp[-6].strptr) );
                 }
#line 3421 "y.tab.c" /* yacc.c:1646  */
    break;

  case 140:
#line 1240 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "    ... next function parameter %g", (yyvsp[0].floatnum) ); }
#line 3427 "y.tab.c" /* yacc.c:1646  */
    break;

  case 141:
#line 1245 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRLVCVS, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 3435 "y.tab.c" /* yacc.c:1646  */
    break;

  case 143:
#line 1251 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s,%s) Linear Voltage-Controlled Voltage"
                                        " Source '%s', voltage gain = %g",
                             (yyvsp[-4].strptr),(yyvsp[-3].strptr),(yyvsp[-2].strptr),(yyvsp[-1].strptr),         (yyvsp[-5].strptr),                 (yyvsp[0].floatnum) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-4].strptr), nodtypPLUS, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypMINUS, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypCTRLPLUS, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypCTRLMINUS, (yyvsp[-5].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-4].strptr), nodtypPLUS, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypMINUS, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypCTRLPLUS, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypCTRLMINUS, (yyvsp[-5].strptr) );
                     }
                     listE = AddElem( listE, &cntE, (yyvsp[-5].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 3459 "y.tab.c" /* yacc.c:1646  */
    break;

  case 145:
#line 1274 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRLVCCS, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 3467 "y.tab.c" /* yacc.c:1646  */
    break;

  case 147:
#line 1280 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s,%s) Linear Voltage-Controlled Current Source"
                                        " '%s', transconductance %g mhos",
                             (yyvsp[-4].strptr),(yyvsp[-3].strptr),(yyvsp[-2].strptr),(yyvsp[-1].strptr),  (yyvsp[-5].strptr),                   (yyvsp[0].floatnum) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-4].strptr), nodtypPLUS, (yyvsp[-5].strptr));
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypMINUS, (yyvsp[-5].strptr));
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypCTRLPLUS, (yyvsp[-5].strptr));
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypCTRLMINUS, (yyvsp[-5].strptr));
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-4].strptr), nodtypPLUS, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypMINUS, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypCTRLPLUS, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypCTRLMINUS, (yyvsp[-5].strptr) );
                     }
                     listG = AddElem( listG, &cntG, (yyvsp[-5].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 3491 "y.tab.c" /* yacc.c:1646  */
    break;

  case 149:
#line 1303 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRLCCCS, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 3499 "y.tab.c" /* yacc.c:1646  */
    break;

  case 151:
#line 1309 "shpars.y" /* yacc.c:1646  */
    { if( toupper( (yyvsp[0].strptr)[0] ) != 'V' ){
                     MsgCompile( msgNEEDVOLTAGE, (yyvsp[0].strptr) );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 3509 "y.tab.c" /* yacc.c:1646  */
    break;

  case 152:
#line 1315 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Linear Current-Controlled Current Source"
                                 " '%s', voltage source: '%s', current gain = %g",
                             (yyvsp[-4].strptr),(yyvsp[-3].strptr), (yyvsp[-5].strptr),                   (yyvsp[-2].strptr),                 (yyvsp[0].floatnum) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-4].strptr), nodtypPLUS, (yyvsp[-5].strptr));
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypMINUS, (yyvsp[-5].strptr));
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-4].strptr), nodtypPLUS, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypMINUS, (yyvsp[-5].strptr) );
                     }
                     listF = AddElem( listF, &cntF, (yyvsp[-5].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 3529 "y.tab.c" /* yacc.c:1646  */
    break;

  case 154:
#line 1334 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRLCCVS, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 3537 "y.tab.c" /* yacc.c:1646  */
    break;

  case 156:
#line 1340 "shpars.y" /* yacc.c:1646  */
    { if( toupper( (yyvsp[0].strptr)[0] ) != 'V' ){
                     MsgCompile( msgNEEDVOLTAGE, (yyvsp[0].strptr) );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 3547 "y.tab.c" /* yacc.c:1646  */
    break;

  case 157:
#line 1346 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Linear Current-Controlled Voltage Source '%s',"
                              " voltage source: '%s', transresistance %g ohms",
                             (yyvsp[-4].strptr),(yyvsp[-3].strptr),                                           (yyvsp[-5].strptr),
                                                 (yyvsp[-2].strptr),                  (yyvsp[0].floatnum) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-4].strptr), nodtypPLUS, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypMINUS, (yyvsp[-5].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-4].strptr), nodtypPLUS, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypMINUS, (yyvsp[-5].strptr) );
                     }
                     listH = AddElem( listH, &cntH, (yyvsp[-5].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 3568 "y.tab.c" /* yacc.c:1646  */
    break;

  case 159:
#line 1367 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRNLDS, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 3576 "y.tab.c" /* yacc.c:1646  */
    break;

  case 161:
#line 1373 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Non-linear dependent source '%s'",
                             (yyvsp[-1].strptr),(yyvsp[0].strptr),                              (yyvsp[-2].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypPLUS, (yyvsp[-2].strptr) );
                       ADD_NODE_MAIN( (yyvsp[0].strptr), nodtypMINUS, (yyvsp[-2].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypPLUS, (yyvsp[-2].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[0].strptr), nodtypMINUS, (yyvsp[-2].strptr) );
                     }
                     listB = AddElem( listB, &cntB, (yyvsp[-2].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 3595 "y.tab.c" /* yacc.c:1646  */
    break;

  case 167:
#line 1397 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[-2].strptr), "I", "V", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[-2].strptr), "I", "V", "" );
                   else
                     Ydebug( "... NLDS %s-source: %s=...", (yyvsp[-2].strptr), (yyvsp[-2].strptr) );
                 }
#line 3605 "y.tab.c" /* yacc.c:1646  */
    break;

  case 168:
#line 1412 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRAREXPR );
                   /*yyerrok; yyclearin;/*?*/
                   /*YERRSKIP;/*?*/
                 }
#line 3614 "y.tab.c" /* yacc.c:1646  */
    break;

  case 169:
#line 1422 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "reduced:  ArExpr : Value ;" );
                   /* $$ = $1; */
                 }
#line 3622 "y.tab.c" /* yacc.c:1646  */
    break;

  case 170:
#line 1425 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "reduced:  ArExpr : Identifier ;" );
		 }
#line 3629 "y.tab.c" /* yacc.c:1646  */
    break;

  case 171:
#line 1429 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "reduced:  ArExpr : '-' ArExpr ;" );
                   /* $$ = -$2; */
                 }
#line 3637 "y.tab.c" /* yacc.c:1646  */
    break;

  case 172:
#line 1433 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "reduced:  ArExpr : ArExpr '+' ArExpr ;" );
                   /* $$ = $1 + $3; */
                 }
#line 3645 "y.tab.c" /* yacc.c:1646  */
    break;

  case 173:
#line 1437 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "reduced:  ArExpr : ArExpr '-' ArExpr ;" );
                   /* $$ = $1 - $3; */
                 }
#line 3653 "y.tab.c" /* yacc.c:1646  */
    break;

  case 174:
#line 1441 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "reduced:  ArExpr : ArExpr '*' ArExpr ;" );
                   /* $$ = $1 * $3; */
                 }
#line 3661 "y.tab.c" /* yacc.c:1646  */
    break;

  case 175:
#line 1445 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "reduced:  ArExpr : ArExpr '/' ArExpr ;" );
                   /* $$ = $1 / $3; */
                 }
#line 3669 "y.tab.c" /* yacc.c:1646  */
    break;

  case 176:
#line 1449 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "reduced:  ArExpr : ArExpr '^' ArExpr ;" );
                   /* $$ = pow( $1, $3 ); */
                 }
#line 3677 "y.tab.c" /* yacc.c:1646  */
    break;

  case 177:
#line 1453 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "ArExpr : '(' ArExpr ')' ;" );
                   /* $$ = $2; */
                 }
#line 3685 "y.tab.c" /* yacc.c:1646  */
    break;

  case 178:
#line 1458 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "reduced:  ArExpr : Identifier '(' ArExpr ')' ;" );
                   if( !CheckKeyword( (yyvsp[-3].strptr),
                           "i", "v",
                           "abs",   "asinh", "cosh", "sin",
                           "acos",  "atan",  "exp",  "sinh",
                           "acosh", "atanh", "ln",   "sqrt",
                           "asin",  "cos",   "log",  "tan",
                           "poly5", "junc",
                           "" ) )
                     MsgAllowKeywords( msgALLOW_V_C_FUN, (yyvsp[-3].strptr),
                                       "i()",     "v()",
/* disable wrap message:     "\n\t"/**/"abs()",   "asinh()", "cosh()",
                                       "sin()",   "acos()",  "atan()",
                                       "exp()",   "sinh()",  "acosh()",
/* disable wrap message:     "\n\t"/**/"atanh()", "ln()",    "sqrt()",
                                       "asin()",  "cos()",   "log()",
                                       "tan()",   "poly5()", "junc()",
                                       "" );
                 }
#line 3709 "y.tab.c" /* yacc.c:1646  */
    break;

  case 179:
#line 1478 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "reduced:  ArExpr : Identifier '(' Node ',' Node ')' ;" );
                   if( !CheckKeyword( (yyvsp[-5].strptr), "I", "V", "" ) )
                     MsgAllowKeywords( msgALLOWVOLTCURR2, (yyvsp[-5].strptr),
                           "i(_,_)", "v(_,_)", "" );
                 }
#line 3719 "y.tab.c" /* yacc.c:1646  */
    break;

  case 181:
#line 1486 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgRIGHTBRKREQ );
                   YERRSKIP;
                 }
#line 3727 "y.tab.c" /* yacc.c:1646  */
    break;

  case 182:
#line 1500 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRVSWITCH, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 3735 "y.tab.c" /* yacc.c:1646  */
    break;

  case 184:
#line 1506 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRISWITCH, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 3743 "y.tab.c" /* yacc.c:1646  */
    break;

  case 186:
#line 1512 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s,%s) V-switch '%s' (model '%s'),"
                                                              " status = %s",
                             (yyvsp[-5].strptr),(yyvsp[-4].strptr),(yyvsp[-3].strptr),(yyvsp[-2].strptr),           (yyvsp[-6].strptr),         (yyvsp[-1].strptr),
                                                             ShowKeyword((yyvsp[0].longnum)) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-5].strptr), nodtypPLUS, (yyvsp[-6].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-4].strptr), nodtypMINUS, (yyvsp[-6].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypCTRLPLUS, (yyvsp[-6].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypCTRLMINUS, (yyvsp[-6].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-5].strptr), nodtypPLUS, (yyvsp[-6].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-4].strptr), nodtypMINUS, (yyvsp[-6].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypCTRLPLUS, (yyvsp[-6].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypCTRLMINUS, (yyvsp[-6].strptr) );
                     }
                     listS = AddElem( listS, &cntS, (yyvsp[-6].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 3768 "y.tab.c" /* yacc.c:1646  */
    break;

  case 188:
#line 1535 "shpars.y" /* yacc.c:1646  */
    { if( toupper( (yyvsp[0].strptr)[0] ) != 'V' ){
                     MsgCompile( msgNEEDVOLTAGE, (yyvsp[0].strptr) );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 3778 "y.tab.c" /* yacc.c:1646  */
    break;

  case 189:
#line 1541 "shpars.y" /* yacc.c:1646  */
    {
                     Ydebug( "(%s,%s) I-switch '%s' (voltage '%s', model '%s'),"
                                                                 " status = %s",
                               (yyvsp[-5].strptr),(yyvsp[-4].strptr),           (yyvsp[-6].strptr),           (yyvsp[-3].strptr),         (yyvsp[-1].strptr),
                                                                ShowKeyword((yyvsp[0].longnum)) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-5].strptr), nodtypPLUS, (yyvsp[-6].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-4].strptr), nodtypMINUS, (yyvsp[-6].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-5].strptr), nodtypPLUS, (yyvsp[-6].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-4].strptr), nodtypMINUS, (yyvsp[-6].strptr) );
                     }
                     listW = AddElem( listW, &cntW, (yyvsp[-6].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 3800 "y.tab.c" /* yacc.c:1646  */
    break;

  case 191:
#line 1561 "shpars.y" /* yacc.c:1646  */
    { (yyval.longnum) = tokenUNKNOWN; /*unknown*/ /* $$ = 0; - <?> */ }
#line 3806 "y.tab.c" /* yacc.c:1646  */
    break;

  case 192:
#line 1563 "shpars.y" /* yacc.c:1646  */
    {      if( CheckKeyword( (yyvsp[0].strptr), "ON", "" ) )  (yyval.longnum) = ON;
                   else if( CheckKeyword( (yyvsp[0].strptr), "OFF", "" ) ) (yyval.longnum) = OFF;
                   else MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[0].strptr), "ON", "OFF", "" );
                 }
#line 3815 "y.tab.c" /* yacc.c:1646  */
    break;

  case 193:
#line 1572 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRLLTLINE, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 3823 "y.tab.c" /* yacc.c:1646  */
    break;

  case 195:
#line 1578 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRLTLINE, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 3831 "y.tab.c" /* yacc.c:1646  */
    break;

  case 197:
#line 1584 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRUDRCLINE, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 3839 "y.tab.c" /* yacc.c:1646  */
    break;

  case 199:
#line 1591 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s) Uni Distrib RC Line '%s'"
                                                     " (model '%s', length = %g)",
                             (yyvsp[-7].strptr),(yyvsp[-6].strptr),(yyvsp[-5].strptr),                      (yyvsp[-8].strptr),(yyvsp[-4].strptr),           (yyvsp[-1].floatnum) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-7].strptr), nodtypUNKNOWN, (yyvsp[-8].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-6].strptr), nodtypUNKNOWN, (yyvsp[-8].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-5].strptr), nodtypUNKNOWN, (yyvsp[-8].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-7].strptr), nodtypUNKNOWN, (yyvsp[-8].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-6].strptr), nodtypUNKNOWN, (yyvsp[-8].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-5].strptr), nodtypUNKNOWN, (yyvsp[-8].strptr) );
                     }
                     listU = AddElem( listU, &cntU, (yyvsp[-8].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 3861 "y.tab.c" /* yacc.c:1646  */
    break;

  case 201:
#line 1611 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s,%s) Lossy Transmission line '%s' (model '%s')",
                             (yyvsp[-4].strptr),(yyvsp[-3].strptr),(yyvsp[-2].strptr),(yyvsp[-1].strptr),                          (yyvsp[-5].strptr),         (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-4].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-4].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                     }
                     listO = AddElem( listO, &cntO, (yyvsp[-5].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/

                   if( PassNo == 2 ){
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       Ydebug( "Try to generate code for LTLLINE '%s'...", (yyvsp[-5].strptr) );
                       PrintElemsTransl( (yyvsp[-5].strptr), circDefCurrent, listO );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s, %s, %s",
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, (yyvsp[-4].strptr) ),
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, (yyvsp[-2].strptr) ), /*not 3!*/
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, (yyvsp[-3].strptr) ),
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-1].strptr) )
                              );
                       if( !MkElemOxxx( (yyvsp[-5].strptr), linebuffer, (yyvsp[0].strptr) ) )
                         MsgCompileWarn( warn5_BREAK, msgNOTGENELEM, (yyvsp[-5].strptr) );
                     }
                   }/*pass2*/
                 }
#line 3905 "y.tab.c" /* yacc.c:1646  */
    break;

  case 203:
#line 1654 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s,%s) Lossless Transmission line '%s'",
                             (yyvsp[-4].strptr),(yyvsp[-3].strptr),(yyvsp[-2].strptr),(yyvsp[-1].strptr),                             (yyvsp[-5].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       /* put in main circuit nodes list: */
                       ADD_NODE_MAIN( (yyvsp[-4].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                     } else {
                       /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-4].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypUNKNOWN, (yyvsp[-5].strptr) );
                     }
                     listT = AddElem( listT, &cntT, (yyvsp[-5].strptr), circDefCurrent ); /* add to list */
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
                       MsgCompileWarn( warn3_CONFLICT, msgNOTGENILLPARM, (yyvsp[-5].strptr) );
                       isGenOK = FALSE;
                     }
                     if( isGenOK )
                     { /* generate code: */
                       Ydebug( "Generate code for LLTLINE '%s'...", (yyvsp[-5].strptr) );
                       PrintElemsTransl( (yyvsp[-5].strptr), circDefCurrent, listT );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s, %s, %s",
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-4].strptr) ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-2].strptr) ), /*not 3!*/
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-3].strptr) ),
                         GetHabNodeName( isSubCall?circCall->circnodlist
                                                  :listNodesMain, (yyvsp[-1].strptr) )
                              );
                       sprintf( parmbuffer, "%g, %g", parmZ0, parmLength );
                       HabELEMout(
                         /*NE=  */GetHabElemName( (yyvsp[-5].strptr), circDefCurrent, listT ),
                         /*KNOT=*/linebuffer,
                         /*PAR= */parmbuffer,
                         /*IDOP=*/"",
                         /*ISTR=*/""
                                 );
                     }
                   }/*pass2*/
                 }
#line 3968 "y.tab.c" /* yacc.c:1646  */
    break;

  case 209:
#line 1718 "shpars.y" /* yacc.c:1646  */
    {
                   if( CheckKeyword( (yyvsp[-2].strptr), "Z0", "TD", "F_FREQ", "NL", "" ) )
                     Ydebug( "... lossy trans line option '%s' = %g", (yyvsp[-2].strptr),  (yyvsp[0].floatnum) );
                   if( PassNo == 2 ){
                     if( CheckKeyword( (yyvsp[-2].strptr), "IC", "" ) ){
                       MsgCompile( msgTOOFEWPARM, (yyvsp[-2].strptr) );
                     }else if( CheckKeyword( (yyvsp[-2].strptr), "Z0", "" ) ){
                       if( (yyvsp[0].floatnum) <= 0.0 ){
                         MsgCompile( msgERPARMILLVALUE, (yyvsp[0].floatnum), (yyvsp[-2].strptr) );
                         existZ0 = FALSE;
                       }else if( existZ0 ){
                         MsgCompile( msgPARMALREADYDEF, (yyvsp[-2].strptr) );
                         existZ0 = FALSE;
                       }else{
                         existZ0 = TRUE;
                         parmZ0 = (yyvsp[0].floatnum);
                       }
                     }else if( CheckKeyword( (yyvsp[-2].strptr), "TD", "" ) ){
                       if( (yyvsp[0].floatnum) <= 0.0 ){
                         MsgCompile( msgERPARMILLVALUE, (yyvsp[0].floatnum), (yyvsp[-2].strptr) );
                         existTD = FALSE;
                       }else if( existTD ){
                         MsgCompile( msgPARMALREADYDEF, (yyvsp[-2].strptr) );
                         existTD = FALSE;
                       }else{
                         existTD = TRUE;
                         parmTD = (yyvsp[0].floatnum);
                       }
                     }else if( CheckKeyword( (yyvsp[-2].strptr), "F"/*F_FREQ*/, "" ) ){
                       if( (yyvsp[0].floatnum) <= 0.0 ){
                         MsgCompile( msgERPARMILLVALUE, (yyvsp[0].floatnum), (yyvsp[-2].strptr) );
                         existF_FREQ = FALSE;
                       }else if( existF_FREQ ){
                         MsgCompile( msgPARMALREADYDEF, (yyvsp[-2].strptr) );
                         existF_FREQ = FALSE;
                       }else{
                         existF_FREQ = TRUE;
                         parmF_FREQ = (yyvsp[0].floatnum);
                       }
                     }else if( CheckKeyword( (yyvsp[-2].strptr), "NL", "" ) ){
                       if( (yyvsp[0].floatnum) <= 0.0 ){
                         MsgCompile( msgERPARMILLVALUE, (yyvsp[0].floatnum), (yyvsp[-2].strptr) );
                         existNL = FALSE;
                       } else if( existNL ){
                         MsgCompile( msgPARMALREADYDEF, (yyvsp[-2].strptr) );
                         existNL = FALSE;
                       }else{
                         existNL = TRUE;
                         parmNL = (yyvsp[0].floatnum);
                       }
                     }else{
                       MsgCompile( msgILLOPTION, (yyvsp[-2].strptr) );
                       break;
                     }
                   }/*pass2*/
                 }
#line 4029 "y.tab.c" /* yacc.c:1646  */
    break;

  case 210:
#line 1775 "shpars.y" /* yacc.c:1646  */
    {
                   if( !CheckKeyword( (yyvsp[-8].strptr), "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[-8].strptr), "IC", "" );
                   else
                     Ydebug( "... lossy trans line option IC = %g,%g,%g,%g",
                                                               (yyvsp[-6].floatnum),(yyvsp[-4].floatnum),(yyvsp[-2].floatnum),(yyvsp[0].floatnum) );
                 }
#line 4041 "y.tab.c" /* yacc.c:1646  */
    break;

  case 212:
#line 1786 "shpars.y" /* yacc.c:1646  */
    {
                   if( !CheckKeyword( (yyvsp[-2].strptr), "N", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[-2].strptr), "N", "" );
                   else
                     Ydebug( "... uni distrib RC line option N = %g lumps", (yyvsp[0].floatnum) );
                 }
#line 4052 "y.tab.c" /* yacc.c:1646  */
    break;

  case 213:
#line 1797 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRDIODE, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 4060 "y.tab.c" /* yacc.c:1646  */
    break;

  case 215:
#line 1803 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s) Diode '%s' (model '%s')",
                             (yyvsp[-2].strptr),(yyvsp[-1].strptr),        (yyvsp[-3].strptr),         (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypPLUS, (yyvsp[-3].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypMINUS, (yyvsp[-3].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypPLUS, (yyvsp[-3].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypMINUS, (yyvsp[-3].strptr) );
                     }
                     listD = AddElem( listD, &cntD, (yyvsp[-3].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                   if( PassNo == 2 ){ /* clear AREA befor process options: */
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       if( UpFindModel( (yyvsp[0].strptr) ) == NULL ){ /* find model */
                         MsgCompile( msgMODELNOTFOUND, (yyvsp[0].strptr) );
                       }
                     }
                   }/*pass2*/
                 }
#line 4087 "y.tab.c" /* yacc.c:1646  */
    break;

  case 216:
#line 1829 "shpars.y" /* yacc.c:1646  */
    {
                   if( PassNo == 2 ){
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       Ydebug( "Try to generate code for Diode Schottky '%s'...", (yyvsp[-5].strptr) );
                       PrintElemsTransl( (yyvsp[-5].strptr), circDefCurrent, listD );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s",
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, (yyvsp[-4].strptr) ),
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, (yyvsp[-3].strptr) )
                              );
                       /* test for diode parameter AREA: */
                       if( existDiodArea && parmDiodArea <= 0.0 ){
                         MsgCompileWarn( warn3_CONFLICT, msgPARMILLVALUE, parmDiodArea, msgAREA );
                         parmDiodArea = def_DiodArea;
                       }
                       if( !existDiodArea ) parmDiodArea = def_DiodArea;
                       if( UpFindModel( (yyvsp[-2].strptr) ) != NULL ){
                         if( !MkElemDxxx( (yyvsp[-5].strptr), linebuffer, (yyvsp[-2].strptr), parmDiodArea ) )
                           MsgCompileWarn( warn5_BREAK, msgNOTGENELEM, (yyvsp[-5].strptr) );
                       }
                     }
                   }/*pass2*/
                 }
#line 4118 "y.tab.c" /* yacc.c:1646  */
    break;

  case 222:
#line 1860 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... diode option area = %g", (yyvsp[0].floatnum) );
                   if( PassNo == 2 ){
                     if( existDiodArea ){
                       MsgCompile( msgDIODAREAAGAIN );
                     }else{
                       existDiodArea = TRUE;
                       parmDiodArea  = (yyvsp[0].floatnum);
                     }
                   }/*pass2*/
                 }
#line 4133 "y.tab.c" /* yacc.c:1646  */
    break;

  case 223:
#line 1871 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "OFF", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "OFF", "" );
                   else
                     Ydebug( "... diode option OFF" );
                 }
#line 4143 "y.tab.c" /* yacc.c:1646  */
    break;

  case 224:
#line 1877 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[-2].strptr), "IC", "TEMP", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[-2].strptr), "IC", "TEMP", "" );
                   else
                     Ydebug( "... diode option '%s' = %g", (yyvsp[-2].strptr), (yyvsp[0].floatnum) );
                 }
#line 4153 "y.tab.c" /* yacc.c:1646  */
    break;

  case 225:
#line 1887 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRTRANSISTOR, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 4161 "y.tab.c" /* yacc.c:1646  */
    break;

  case 227:
#line 1893 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s) Bipolar Trasistor '%s', model '%s'",
                             (yyvsp[-3].strptr),(yyvsp[-2].strptr),(yyvsp[-1].strptr),                    (yyvsp[-4].strptr),         (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypBjtC, (yyvsp[-4].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypBjtB, (yyvsp[-4].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypBjtE, (yyvsp[-4].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypBjtC, (yyvsp[-4].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypBjtB, (yyvsp[-4].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypBjtE, (yyvsp[-4].strptr) );
                     }
                     listQ = AddElem( listQ, &cntQ, (yyvsp[-4].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 4182 "y.tab.c" /* yacc.c:1646  */
    break;

  case 229:
#line 1916 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s,%s) Bipolar Trasistor '%s', model '%s'",
                             (yyvsp[-4].strptr),(yyvsp[-3].strptr),(yyvsp[-2].strptr),(yyvsp[-1].strptr),                    (yyvsp[-5].strptr),         (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-4].strptr), nodtypBjtC, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypBjtB, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypBjtE, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypBjtS, (yyvsp[-5].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-4].strptr), nodtypBjtC, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypBjtB, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypBjtE, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypBjtS, (yyvsp[-5].strptr) );
                    }
                     listQ = AddElem( listQ, &cntQ, (yyvsp[-5].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 4205 "y.tab.c" /* yacc.c:1646  */
    break;

  case 231:
#line 1942 "shpars.y" /* yacc.c:1646  */
    { (yyval.strptr) = (yyvsp[0].mixed)->text; }
#line 4211 "y.tab.c" /* yacc.c:1646  */
    break;

  case 232:
#line 1944 "shpars.y" /* yacc.c:1646  */
    { (yyval.strptr) = (yyvsp[0].mixed)->text; }
#line 4217 "y.tab.c" /* yacc.c:1646  */
    break;

  case 237:
#line 1953 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... transistor option area = %g", (yyvsp[0].floatnum) ); }
#line 4223 "y.tab.c" /* yacc.c:1646  */
    break;

  case 238:
#line 1955 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "OFF", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "OFF", "" );
                   else
                     Ydebug( "... transistor option OFF" );
                 }
#line 4233 "y.tab.c" /* yacc.c:1646  */
    break;

  case 239:
#line 1961 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[-2].strptr), "TEMP", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[-2].strptr), "TEMP", "" );
                   else
                     Ydebug( "... transistor option TEMP = %g", (yyvsp[0].floatnum) );
                 }
#line 4243 "y.tab.c" /* yacc.c:1646  */
    break;

  case 240:
#line 1967 "shpars.y" /* yacc.c:1646  */
    {
                   if( !CheckKeyword( (yyvsp[-4].strptr), "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[-4].strptr), "IC", "" );
                   else
                     Ydebug( "... transistor option IC = %g,%g", (yyvsp[-2].floatnum), (yyvsp[0].floatnum) );
                 }
#line 4254 "y.tab.c" /* yacc.c:1646  */
    break;

  case 241:
#line 1978 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRJFET, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 4262 "y.tab.c" /* yacc.c:1646  */
    break;

  case 243:
#line 1984 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s) JFET '%s', model '%s'",
                             (yyvsp[-3].strptr),(yyvsp[-2].strptr),(yyvsp[-1].strptr),       (yyvsp[-4].strptr),         (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypFetD, (yyvsp[-4].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypFetG, (yyvsp[-4].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypFetS, (yyvsp[-4].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypFetD, (yyvsp[-4].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypFetG, (yyvsp[-4].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypFetS, (yyvsp[-4].strptr) );
                     }
                     listJ = AddElem( listJ, &cntJ, (yyvsp[-4].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 4283 "y.tab.c" /* yacc.c:1646  */
    break;

  case 249:
#line 2009 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... jfet option area = %g", (yyvsp[0].floatnum) ); }
#line 4289 "y.tab.c" /* yacc.c:1646  */
    break;

  case 250:
#line 2011 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "OFF", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "OFF", "" );
                   else
                     Ydebug( "... jfet option OFF" );
                 }
#line 4299 "y.tab.c" /* yacc.c:1646  */
    break;

  case 251:
#line 2017 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[-2].strptr), "TEMP", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[-2].strptr), "TEMP", "" );
                   else
                     Ydebug( "... jfet option TEMP = %g", (yyvsp[0].floatnum) );
                 }
#line 4309 "y.tab.c" /* yacc.c:1646  */
    break;

  case 252:
#line 2023 "shpars.y" /* yacc.c:1646  */
    {
                   if( !CheckKeyword( (yyvsp[-4].strptr), "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[-4].strptr), "IC", "" );
                   else
                     Ydebug( "... jfet option IC = %g,%g", (yyvsp[-2].floatnum), (yyvsp[0].floatnum) );
                 }
#line 4320 "y.tab.c" /* yacc.c:1646  */
    break;

  case 253:
#line 2034 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRMESFET, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 4328 "y.tab.c" /* yacc.c:1646  */
    break;

  case 255:
#line 2040 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s) MESFET '%s', model '%s'",
                             (yyvsp[-3].strptr),(yyvsp[-2].strptr),(yyvsp[-1].strptr),         (yyvsp[-4].strptr),         (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypFetD, (yyvsp[-4].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypFetG, (yyvsp[-4].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypFetS, (yyvsp[-4].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypFetD, (yyvsp[-4].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypFetG, (yyvsp[-4].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypFetS, (yyvsp[-4].strptr) );
                     }
                     listZ = AddElem( listZ, &cntZ, (yyvsp[-4].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                   if( PassNo == 2 ){ /* clear AREA befor process options: */
                     parmMesfetArea  = def_MesfetAREA;
                     existMesfetArea = FALSE;
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       if( UpFindModel( (yyvsp[0].strptr) ) == NULL ){ /* find model */
                         MsgCompile( msgMODELNOTFOUND, (yyvsp[0].strptr) );
                       }
                     }
                   }/*pass2*/
                 }
#line 4359 "y.tab.c" /* yacc.c:1646  */
    break;

  case 256:
#line 2070 "shpars.y" /* yacc.c:1646  */
    {
                   if( PassNo == 2 ){
                     if( !isSubDefBody /*subcall in main circuit*/ ||
                        ( isSubDefBody && isSubCall && (circCall == circDefCurrent) ) ){
                       Ydebug( "Try to generate code for MESFET '%s'...", (yyvsp[-6].strptr) );
                       PrintElemsTransl( (yyvsp[-6].strptr), circDefCurrent, listZ );
                                    /* out to elements names tranlations file */
                       sprintf( linebuffer, "%s, %s, %s",
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, (yyvsp[-5].strptr) ),
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, (yyvsp[-4].strptr) ),
                           GetHabNodeName( isSubCall?circCall->circnodlist
                                                    :listNodesMain, (yyvsp[-3].strptr) )
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
                       if( UpFindModel( (yyvsp[-2].strptr) ) != NULL ){
                         if( !MkElemZxxx( (yyvsp[-6].strptr), linebuffer, (yyvsp[-2].strptr), parmMesfetArea ) )
                           MsgCompileWarn( warn5_BREAK, msgNOTGENELEM, (yyvsp[-6].strptr) );
                       }
                     }
                   }/*pass2*/
                 }
#line 4397 "y.tab.c" /* yacc.c:1646  */
    break;

  case 262:
#line 2108 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... mesfet option area = %g", (yyvsp[0].floatnum) );
                   parmMesfetArea  = (yyvsp[0].floatnum);
                   existMesfetArea = TRUE;
                 }
#line 4406 "y.tab.c" /* yacc.c:1646  */
    break;

  case 263:
#line 2113 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "OFF", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "OFF", "" );
                   else
                     Ydebug( "... mesfet option OFF" );
                 }
#line 4416 "y.tab.c" /* yacc.c:1646  */
    break;

  case 264:
#line 2119 "shpars.y" /* yacc.c:1646  */
    {
                   if( !CheckKeyword( (yyvsp[-4].strptr), "IC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[-4].strptr), "IC", "" );
                   else
                     Ydebug( "... mesfet option IC = %g,%g", (yyvsp[-2].floatnum), (yyvsp[0].floatnum) );
                 }
#line 4427 "y.tab.c" /* yacc.c:1646  */
    break;

  case 265:
#line 2130 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERRMOSFET, (yyvsp[-1].strptr) );
                   YERRSKIP;
                 }
#line 4435 "y.tab.c" /* yacc.c:1646  */
    break;

  case 267:
#line 2136 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "(%s,%s,%s,%s) MOSFET '%s' (model '%s')",
                             (yyvsp[-4].strptr),(yyvsp[-3].strptr),(yyvsp[-2].strptr),(yyvsp[-1].strptr),         (yyvsp[-5].strptr),         (yyvsp[0].strptr) );
                   if( PassNo == 1 ){
                     if( !isSubDefBody ){
                       ADD_NODE_MAIN( (yyvsp[-4].strptr), nodtypFetD, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-3].strptr), nodtypFetG, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-2].strptr), nodtypFetS, (yyvsp[-5].strptr) );
                       ADD_NODE_MAIN( (yyvsp[-1].strptr), nodtypFetB, (yyvsp[-5].strptr) );
                     } else { /* put in local subcircuit nodes list: */
                       ADD_NODE_SUBCIRC( (yyvsp[-4].strptr), nodtypFetD, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-3].strptr), nodtypFetG, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-2].strptr), nodtypFetS, (yyvsp[-5].strptr) );
                       ADD_NODE_SUBCIRC( (yyvsp[-1].strptr), nodtypFetB, (yyvsp[-5].strptr) );
                     }
                     listM = AddElem( listM, &cntM, (yyvsp[-5].strptr), circDefCurrent ); /* add to list */
                       /* if circDefCurrent == NULL - process main circuit */
                   }/*pass1*/
                 }
#line 4458 "y.tab.c" /* yacc.c:1646  */
    break;

  case 273:
#line 2162 "shpars.y" /* yacc.c:1646  */
    {
                   if( CheckKeyword( (yyvsp[0].strptr), "OFF", "" ) ){
                       Ydebug( "... mosfet option OFF" );
                   }else if( CheckKeyword( (yyvsp[0].strptr), "L_LEN", "W_WIDTH",
                                               "AD", "AS", "PD", "PS",
                                               "NRD", "NRS", "TEMP", "IC",
                                               "" ) ){
                       MsgCompile( msgTOOFEWPARM, (yyvsp[0].strptr) );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }else{
                       MsgCompile( msgILLOPTION, (yyvsp[0].strptr) );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 4477 "y.tab.c" /* yacc.c:1646  */
    break;

  case 274:
#line 2178 "shpars.y" /* yacc.c:1646  */
    {
                   if( CheckKeyword( (yyvsp[-2].strptr), "L_LEN", "W_WIDTH",
                                         "AD", "AS", "PD", "PS",
                                         "NRD", "NRS", "TEMP",
                                         "" ) ){
                       Ydebug( "... mosfet option '%s' = %g", (yyvsp[-2].strptr), (yyvsp[0].floatnum) );
                   }else if( CheckKeyword( (yyvsp[-2].strptr), "IC", "" ) ){
                       MsgCompile( msgTOOFEWPARM, (yyvsp[-2].strptr) );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }else if( CheckKeyword( (yyvsp[-2].strptr), "OFF", "" ) ){
                       MsgCompile( msgTOOMUCHPARM, (yyvsp[-2].strptr) );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }else{
                       MsgCompile( msgILLOPTION, (yyvsp[-2].strptr) );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 4499 "y.tab.c" /* yacc.c:1646  */
    break;

  case 275:
#line 2197 "shpars.y" /* yacc.c:1646  */
    {
                   if( CheckKeyword( (yyvsp[-4].strptr), "IC", "" ) ){
                       Ydebug( "... mosfet option IC = %g,%g", (yyvsp[-2].floatnum), (yyvsp[0].floatnum) );
                   } else if( CheckKeyword( (yyvsp[-4].strptr), "OFF", "L_LEN", "W_WIDTH",
                                                "AD", "AS", "PD", "PS",
                                                "NRD", "NRS", "TEMP",
                                                "" ) ){
                       MsgCompile( msgTOOMUCHPARM, (yyvsp[-4].strptr) );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }else{
                       MsgCompile( msgILLOPTION, (yyvsp[-4].strptr) );
                    /* SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 4518 "y.tab.c" /* yacc.c:1646  */
    break;

  case 276:
#line 2225 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_CONTROL );
                   YERRSKIP;
                 }
#line 4526 "y.tab.c" /* yacc.c:1646  */
    break;

  case 284:
#line 2240 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_LET );
                   YERRSKIP;
                 }
#line 4534 "y.tab.c" /* yacc.c:1646  */
    break;

  case 287:
#line 2249 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_ALTER );
                   YERRSKIP;
                 }
#line 4542 "y.tab.c" /* yacc.c:1646  */
    break;

  case 289:
#line 2256 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_WRITE );
                   YERRSKIP;
                 }
#line 4550 "y.tab.c" /* yacc.c:1646  */
    break;

  case 291:
#line 2264 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_OPTIONS );
                   YERRSKIP;
                 }
#line 4558 "y.tab.c" /* yacc.c:1646  */
    break;

  case 296:
#line 2273 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".OPTIONS: '%s'", (yyvsp[0].strptr) ); }
#line 4564 "y.tab.c" /* yacc.c:1646  */
    break;

  case 297:
#line 2275 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".OPTIONS: '%s'=%g", (yyvsp[-2].strptr), (yyvsp[0].floatnum) ); }
#line 4570 "y.tab.c" /* yacc.c:1646  */
    break;

  case 298:
#line 2280 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_NODESET );
                   YERRSKIP;
                 }
#line 4578 "y.tab.c" /* yacc.c:1646  */
    break;

  case 303:
#line 2290 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "V", "" ) ){
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "V", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 4588 "y.tab.c" /* yacc.c:1646  */
    break;

  case 304:
#line 2296 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".NODESET: V(%g) = %g", (yyvsp[-3].strptr), (yyvsp[0].floatnum) ); }
#line 4594 "y.tab.c" /* yacc.c:1646  */
    break;

  case 305:
#line 2301 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_IC );
                   YERRSKIP;
                 }
#line 4602 "y.tab.c" /* yacc.c:1646  */
    break;

  case 310:
#line 2311 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "V", "" ) ){
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "V", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 4612 "y.tab.c" /* yacc.c:1646  */
    break;

  case 311:
#line 2317 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".IC: V(%g) = %g", (yyvsp[-3].strptr), (yyvsp[0].floatnum) ); }
#line 4618 "y.tab.c" /* yacc.c:1646  */
    break;

  case 312:
#line 2321 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_AC );
                   YERRSKIP;
                 }
#line 4626 "y.tab.c" /* yacc.c:1646  */
    break;

  case 314:
#line 2327 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".AC: number of points = %ld, start freq = %g, stop freq = %g",
                                                     (yyvsp[-3].longnum),              (yyvsp[-2].floatnum),             (yyvsp[-1].floatnum) );
                 }
#line 4634 "y.tab.c" /* yacc.c:1646  */
    break;

  case 315:
#line 2333 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_DC );
                   YERRSKIP;
                 }
#line 4642 "y.tab.c" /* yacc.c:1646  */
    break;

  case 320:
#line 2343 "shpars.y" /* yacc.c:1646  */
    { if( toupper( (yyvsp[0].strptr)[0] ) != 'V' && toupper( (yyvsp[0].strptr)[0] ) != 'I' ){
                     /* MsgCompile( msgNEEDVOLTAGE, $1 ); /* old: only V... */
                     MsgCompile( msgNEEDVOLTCURR, (yyvsp[0].strptr) ); /*add ASB000303*/
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 4653 "y.tab.c" /* yacc.c:1646  */
    break;

  case 321:
#line 2350 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".DC: src name '%s', start = %g, stop = %g, incr = %g",
                                           (yyvsp[-4].strptr),          (yyvsp[-2].floatnum),        (yyvsp[-1].floatnum),        (yyvsp[0].floatnum) );
                 }
#line 4661 "y.tab.c" /* yacc.c:1646  */
    break;

  case 322:
#line 2356 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_DISTO );
                   YERRSKIP;
                 }
#line 4669 "y.tab.c" /* yacc.c:1646  */
    break;

  case 324:
#line 2362 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".DC: number of points = %ld, start freq = %g, stop freq = %g",
                                                     (yyvsp[-2].longnum),              (yyvsp[-1].floatnum),             (yyvsp[0].floatnum) );
                 }
#line 4677 "y.tab.c" /* yacc.c:1646  */
    break;

  case 326:
#line 2368 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".AC: number of points = %ld, start freq = %g, stop freq = %g, overf = %g",
                                                     (yyvsp[-3].longnum),              (yyvsp[-2].floatnum),             (yyvsp[-1].floatnum),         (yyvsp[0].floatnum) );
                 }
#line 4685 "y.tab.c" /* yacc.c:1646  */
    break;

  case 328:
#line 2375 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_NOISE );
                   YERRSKIP;
                 }
#line 4693 "y.tab.c" /* yacc.c:1646  */
    break;

  case 330:
#line 2382 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".NOISE source '%s', '%s' pts = %g,"
                                                    " fstart = %g, fstop = %g",
                              (yyvsp[-4].strptr),   ShowKeyword((yyvsp[-3].longnum)),       (yyvsp[-2].floatnum),
                                                               (yyvsp[-1].floatnum),         (yyvsp[0].floatnum) );
                 }
#line 4703 "y.tab.c" /* yacc.c:1646  */
    break;

  case 332:
#line 2392 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[0].strptr), "V", "" ) ){
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "V", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                   }
                 }
#line 4713 "y.tab.c" /* yacc.c:1646  */
    break;

  case 333:
#line 2400 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... .noise voltage V(%s)", (yyvsp[-1].strptr) );
                 }
#line 4720 "y.tab.c" /* yacc.c:1646  */
    break;

  case 334:
#line 2403 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... .noise voltage V(%s)-V(%s)", (yyvsp[-3].strptr), (yyvsp[-1].strptr) ); }
#line 4726 "y.tab.c" /* yacc.c:1646  */
    break;

  case 336:
#line 2408 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... .noise option pts_per_summary = %ld", (yyvsp[0].longnum) ); }
#line 4732 "y.tab.c" /* yacc.c:1646  */
    break;

  case 337:
#line 2412 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_OP );
                   YERRSKIP;
                 }
#line 4740 "y.tab.c" /* yacc.c:1646  */
    break;

  case 339:
#line 2418 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".OP statement" ); }
#line 4746 "y.tab.c" /* yacc.c:1646  */
    break;

  case 341:
#line 2423 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_PZ );
                   YERRSKIP;
                 }
#line 4754 "y.tab.c" /* yacc.c:1646  */
    break;

  case 343:
#line 2429 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".PZ: nodes: inp='%s','%s' out='%s','%s',"
                           " type='%s', analysis='%s'",
                                             (yyvsp[-5].strptr),  (yyvsp[-4].strptr),       (yyvsp[-3].strptr),  (yyvsp[-2].strptr),
                       ShowKeyword((yyvsp[-1].longnum)),           ShowKeyword((yyvsp[0].longnum))
                         );
                 }
#line 4765 "y.tab.c" /* yacc.c:1646  */
    break;

  case 345:
#line 2439 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_SENS );
                   YERRSKIP;
                 }
#line 4773 "y.tab.c" /* yacc.c:1646  */
    break;

  case 347:
#line 2445 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".SENS statement" ); }
#line 4779 "y.tab.c" /* yacc.c:1646  */
    break;

  case 349:
#line 2450 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... .sens option DC" ); }
#line 4785 "y.tab.c" /* yacc.c:1646  */
    break;

  case 350:
#line 2452 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... .sens options: AC '%s' poins = %ld, fstart = %g, fstop = %g",
                                                    (yyvsp[-3].longnum),         (yyvsp[-2].longnum),           (yyvsp[-1].floatnum),         (yyvsp[0].floatnum) );
                 }
#line 4793 "y.tab.c" /* yacc.c:1646  */
    break;

  case 351:
#line 2458 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_TF );
                   YERRSKIP;
                 }
#line 4801 "y.tab.c" /* yacc.c:1646  */
    break;

  case 353:
#line 2464 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".TF: input source = '%s'", (yyvsp[0].strptr) ); }
#line 4807 "y.tab.c" /* yacc.c:1646  */
    break;

  case 355:
#line 2469 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_TRAN );
                   YERRSKIP;
                 }
#line 4815 "y.tab.c" /* yacc.c:1646  */
    break;

  case 357:
#line 2474 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".TRAN: tstep = %g, tstop = %g, tstart = 0",
                                           (yyvsp[-2].floatnum),         (yyvsp[-1].floatnum) );
                 }
#line 4823 "y.tab.c" /* yacc.c:1646  */
    break;

  case 358:
#line 2478 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".TRAN: tstep = %g, tstop = %g, tstart = %g",
                                           (yyvsp[-3].floatnum),         (yyvsp[-2].floatnum),          (yyvsp[-1].floatnum) );
                 }
#line 4831 "y.tab.c" /* yacc.c:1646  */
    break;

  case 359:
#line 2482 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".TRAN: tstep = %g, tstop = %g, tstart = %g, tmax = %g",
                                           (yyvsp[-4].floatnum),         (yyvsp[-3].floatnum),          (yyvsp[-2].floatnum),        (yyvsp[-1].floatnum) );
                 }
#line 4839 "y.tab.c" /* yacc.c:1646  */
    break;

  case 362:
#line 2490 "shpars.y" /* yacc.c:1646  */
    {
                   if( !CheckKeyword( (yyvsp[0].strptr), "UIC", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "UIC", "" );
                   else
                     Ydebug( "... .tran option UIC" );
                 }
#line 4850 "y.tab.c" /* yacc.c:1646  */
    break;

  case 363:
#line 2539 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_WIDTH );
                   YERRSKIP;
                 }
#line 4858 "y.tab.c" /* yacc.c:1646  */
    break;

  case 365:
#line 2545 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".WIDTH statement" ); }
#line 4864 "y.tab.c" /* yacc.c:1646  */
    break;

  case 369:
#line 2551 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... .width parameter '%s' = %g", (yyvsp[-2].strptr), (yyvsp[0].floatnum) ); }
#line 4870 "y.tab.c" /* yacc.c:1646  */
    break;

  case 370:
#line 2556 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_HB );
                   YERRSKIP;
                 }
#line 4878 "y.tab.c" /* yacc.c:1646  */
    break;

  case 372:
#line 2561 "shpars.y" /* yacc.c:1646  */
    { if( PassNo == 1 ){
                     exist_HB_F2 = FALSE;
                     parm_HB_F2 = 0.0;
                   }
                   /* may be a lot of .HB-operators in program: */
                 /*if( existHB ) MsgCompile( msgHBALREADYEXIST ); /*rem ASB000320*/
                 }
#line 4890 "y.tab.c" /* yacc.c:1646  */
    break;

  case 373:
#line 2570 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[-3].strptr), "F1", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[-3].strptr), "F1", "" );
                   else {
                     if( !existHB ) existHB = TRUE;
                     Ydebug( ".HB statement: F1=%g, F2=...", (yyvsp[-1].floatnum) );
                     /* parm_HB_F2 fixed in pass 1: not correct:
                     Ydebug( ".HB statement: F1=%g, F2=%g",
                             $5,    exist_HB_F2?parm_HB_F2:0.0 );
                     */
                     existHB00 = FALSE;
                     cntHB = 0;
                     if( PassNo == 1 ) /* save F1-parameter of .HB-operator: */
                         parm_HB_F1 = (yyvsp[-1].floatnum);
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
#line 4918 "y.tab.c" /* yacc.c:1646  */
    break;

  case 374:
#line 2593 "shpars.y" /* yacc.c:1646  */
    { if( PassNo == 2 ){
                     if( !existHB00 ){
                         OutTmp( ", 0,0" ); /* add required 0,0-pair */
                         cntHB++;
                     }
                     OutTmp( ", KN=%d /\n", cntHB );
                   }
                 }
#line 4931 "y.tab.c" /* yacc.c:1646  */
    break;

  case 377:
#line 2606 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[-2].strptr), "F2", "" ) ){
                     MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[-2].strptr), "F2", "" );
                     if( PassNo == 1 ) /* save F2-parameter of .HB-operator: */
                         parm_HB_F2 = 0.0;
                   }else{
                     exist_HB_F2 = TRUE;
                     if( PassNo == 1 ) /* save F2-parameter of .HB-operator: */
                         parm_HB_F2 = (yyvsp[0].floatnum);
                   }
                 }
#line 4946 "y.tab.c" /* yacc.c:1646  */
    break;

  case 379:
#line 2620 "shpars.y" /* yacc.c:1646  */
    { if( PassNo == 2 ) OutTmp( ", " ); }
#line 4952 "y.tab.c" /* yacc.c:1646  */
    break;

  case 381:
#line 2625 "shpars.y" /* yacc.c:1646  */
    {
                   Ydebug( "... .HB pair: %ld, %ld", (yyvsp[-1].longnum), (yyvsp[0].longnum) );    /*$3->$2*/
                   cntHB++;
                   if( (yyvsp[-1].longnum) == 0 && (yyvsp[0].longnum) == 0 ) existHB00 = TRUE;	
                   if( PassNo == 2 ) OutTmp( "%ld,%ld", (yyvsp[-1].longnum), (yyvsp[0].longnum) ); /*$3->$2*/
                 }
#line 4963 "y.tab.c" /* yacc.c:1646  */
    break;

  case 382:
#line 2635 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_HB_OPTIONS );
                   YERRSKIP;
                 }
#line 4971 "y.tab.c" /* yacc.c:1646  */
    break;

  case 384:
#line 2641 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".HB_OPTIONS statement" ); }
#line 4977 "y.tab.c" /* yacc.c:1646  */
    break;

  case 388:
#line 2648 "shpars.y" /* yacc.c:1646  */
    { if( GetOptNo( (yyvsp[-2].strptr) ) == -1 ) /* illegal option */
                     MsgCompile( msgUNKNWN_HB_OPT, (yyvsp[-2].strptr) ); /**/
                   else {
                     Ydebug( "... .HB_OPTIONS decimal parameter '%s' = %ld", (yyvsp[-2].strptr), (yyvsp[0].longnum) );
                     if( PassNo == 1 ) ChangeConfOption( (yyvsp[-2].strptr), 0.0, (yyvsp[0].longnum) );
                   }
                 }
#line 4989 "y.tab.c" /* yacc.c:1646  */
    break;

  case 389:
#line 2656 "shpars.y" /* yacc.c:1646  */
    { if( GetOptNo( (yyvsp[-2].strptr) ) == -1 ) /* illegal option */
                     MsgCompile( msgUNKNWN_HB_OPT, (yyvsp[-2].strptr) ); /**/
                   else {
                     Ydebug( "... .HB_OPTIONS float parameter '%s' = %g", (yyvsp[-2].strptr), (yyvsp[0].floatnum) );
                     if( PassNo == 1 ) ChangeConfOption( (yyvsp[-2].strptr), (yyvsp[0].floatnum), 0 );
                   }
                 }
#line 5001 "y.tab.c" /* yacc.c:1646  */
    break;

  case 390:
#line 2666 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_SAVE );
                   YERRSKIP;
                 }
#line 5009 "y.tab.c" /* yacc.c:1646  */
    break;

  case 392:
#line 2672 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".SAVE statement" ); }
#line 5015 "y.tab.c" /* yacc.c:1646  */
    break;

  case 396:
#line 2678 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... .save vector '%s'", (yyvsp[0].strptr) ); }
#line 5021 "y.tab.c" /* yacc.c:1646  */
    break;

  case 397:
#line 2680 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... .save vector '%s'", (yyvsp[0].strptr) ); }
#line 5027 "y.tab.c" /* yacc.c:1646  */
    break;

  case 399:
#line 2685 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_FOUR );
                   YERRSKIP;
                 }
#line 5035 "y.tab.c" /* yacc.c:1646  */
    break;

  case 401:
#line 2691 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".FOUR: frequency = %g", (yyvsp[-1].floatnum) ); }
#line 5041 "y.tab.c" /* yacc.c:1646  */
    break;

  case 405:
#line 2697 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_PRINT );
                   YERRSKIP;
                 }
#line 5049 "y.tab.c" /* yacc.c:1646  */
    break;

  case 407:
#line 2703 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".PRINT: type of analysis = '%s'", ShowKeyword((yyvsp[-1].longnum)) ); }
#line 5055 "y.tab.c" /* yacc.c:1646  */
    break;

  case 411:
#line 2709 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... .print parameter \"%s\"", (yyvsp[0].strptr) ); }
#line 5061 "y.tab.c" /* yacc.c:1646  */
    break;

  case 412:
#line 2711 "shpars.y" /* yacc.c:1646  */
    { Ydebug( "... .print parameter '%s'", (yyvsp[0].strptr) ); }
#line 5067 "y.tab.c" /* yacc.c:1646  */
    break;

  case 414:
#line 2716 "shpars.y" /* yacc.c:1646  */
    { MsgCompile( msgERR_PLOT );
                   YERRSKIP;
                 }
#line 5075 "y.tab.c" /* yacc.c:1646  */
    break;

  case 416:
#line 2722 "shpars.y" /* yacc.c:1646  */
    { Ydebug( ".PLOT: type of analysis = '%s'", ShowKeyword((yyvsp[-1].longnum)) ); }
#line 5081 "y.tab.c" /* yacc.c:1646  */
    break;

  case 422:
#line 2730 "shpars.y" /* yacc.c:1646  */
    {
                   int kw;
                   kw = LookupKeyword( (yyvsp[0].strptr) );
                   switch( kw ){
                   case DC: case AC: case TRAN: case NOISE: case DISTO: case PZ:
                     (yyval.longnum) = kw;
                     break; 
                   /*
                   case FALSE:
                     MsgCompile( msgNOTRESERVKEY, strupr($1) );
                     break;
                   */ 
                   default:
                     MsgAllowKeywords( msgILLPRNTYPE, strupr((yyvsp[0].strptr)),
                        "DC", "AC", "TRAN", "NOISE", "DISTO", "PZ", "" );
                     break; 
                   }
                 }
#line 5104 "y.tab.c" /* yacc.c:1646  */
    break;

  case 423:
#line 2751 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[-3].strptr), "I","V","VR","VI","VM","VP","VDB", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, strupr((yyvsp[-3].strptr)),
                                 "I","V","VR","VI","VM","VP","VDB", "" );
                   else
                     Ydebug( "... outvar %s(%s)", (yyvsp[-3].strptr), (yyvsp[-1].strptr) );
                 }
#line 5115 "y.tab.c" /* yacc.c:1646  */
    break;

  case 424:
#line 2758 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[-5].strptr), "I","V","VR","VI","VM","VP","VDB", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, strupr((yyvsp[-5].strptr)),
                                 "I","V","VR","VI","VM","VP","VDB", "" );
                   else
                     Ydebug( "... outvar V(%s,%s)", (yyvsp[-5].strptr), (yyvsp[-3].strptr), (yyvsp[-1].strptr) );
                 }
#line 5126 "y.tab.c" /* yacc.c:1646  */
    break;

  case 425:
#line 2786 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[-3].strptr), "V", "I", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[-3].strptr), "V", "I", "" );
                   else
                     Ydebug( "... outvar %s(%s)", (yyvsp[-3].strptr), (yyvsp[-1].strptr) );
                 }
#line 5136 "y.tab.c" /* yacc.c:1646  */
    break;

  case 426:
#line 2792 "shpars.y" /* yacc.c:1646  */
    { if( !CheckKeyword( (yyvsp[-5].strptr), "V", "I", "" ) )
                     MsgAllowKeywords( msgALLOWKEYWORD, (yyvsp[-5].strptr), "V", "I", "" );
                   else
                     Ydebug( "... outvar V(%s,%s)", (yyvsp[-5].strptr), (yyvsp[-3].strptr), (yyvsp[-1].strptr) );
                 }
#line 5146 "y.tab.c" /* yacc.c:1646  */
    break;

  case 427:
#line 2800 "shpars.y" /* yacc.c:1646  */
    { (yyval.strptr) = (yyvsp[0].strptr); }
#line 5152 "y.tab.c" /* yacc.c:1646  */
    break;

  case 428:
#line 2805 "shpars.y" /* yacc.c:1646  */
    { (yyval.strptr) = (yyvsp[0].strptr); }
#line 5158 "y.tab.c" /* yacc.c:1646  */
    break;

  case 429:
#line 2826 "shpars.y" /* yacc.c:1646  */
    { (yyval.strptr) = (yyvsp[0].mixed)->text; }
#line 5164 "y.tab.c" /* yacc.c:1646  */
    break;

  case 430:
#line 2828 "shpars.y" /* yacc.c:1646  */
    { (yyval.strptr) = (yyvsp[0].mixed)->text; }
#line 5170 "y.tab.c" /* yacc.c:1646  */
    break;

  case 431:
#line 2830 "shpars.y" /* yacc.c:1646  */
    { (yyval.strptr) = (yyvsp[0].strptr); }
#line 5176 "y.tab.c" /* yacc.c:1646  */
    break;

  case 432:
#line 2834 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = (yyvsp[0].floatnum);
                         }
#line 5183 "y.tab.c" /* yacc.c:1646  */
    break;

  case 433:
#line 2836 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = (yyvsp[0].floatnum);
                         }
#line 5190 "y.tab.c" /* yacc.c:1646  */
    break;

  case 434:
#line 2838 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = -(yyvsp[0].floatnum);
                         }
#line 5197 "y.tab.c" /* yacc.c:1646  */
    break;

  case 435:
#line 2843 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = (double)(yyvsp[0].longnum);
                         }
#line 5204 "y.tab.c" /* yacc.c:1646  */
    break;

  case 436:
#line 2846 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = (yyvsp[0].floatnum);
                         }
#line 5211 "y.tab.c" /* yacc.c:1646  */
    break;

  case 437:
#line 2849 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = (double)(yyvsp[0].mixed)->longval;
                /*
                MsgDebug( "IDENTINT" );
                SHOWlong( $<mixed>1->longval );
                SHOWfloat( (double)$<mixed>1->longval );
                SHOWfloat( $<floatnum>$ );
                */
                         }
#line 5224 "y.tab.c" /* yacc.c:1646  */
    break;

  case 438:
#line 2858 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = (yyvsp[0].mixed)->floatval;
                         }
#line 5231 "y.tab.c" /* yacc.c:1646  */
    break;

  case 439:
#line 2863 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = (yyvsp[0].floatnum);
                         }
#line 5238 "y.tab.c" /* yacc.c:1646  */
    break;

  case 440:
#line 2865 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = (yyvsp[-1].floatnum);
                         }
#line 5245 "y.tab.c" /* yacc.c:1646  */
    break;

  case 441:
#line 2867 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = -(yyvsp[-1].floatnum);
                         }
#line 5252 "y.tab.c" /* yacc.c:1646  */
    break;

  case 442:
#line 2872 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = (yyvsp[0].floatnum);
                         }
#line 5259 "y.tab.c" /* yacc.c:1646  */
    break;

  case 443:
#line 2875 "shpars.y" /* yacc.c:1646  */
    { (yyval.floatnum) = (yyvsp[0].mixed)->floatval;
                         }
#line 5266 "y.tab.c" /* yacc.c:1646  */
    break;

  case 444:
#line 2880 "shpars.y" /* yacc.c:1646  */
    { (yyval.longnum) = (yyvsp[0].longnum);
                           /* Ydebug( "DecInt = %ld", $$ );/**/
                         }
#line 5274 "y.tab.c" /* yacc.c:1646  */
    break;

  case 445:
#line 2883 "shpars.y" /* yacc.c:1646  */
    { (yyval.longnum) = -(yyvsp[0].longnum);
                           /* Ydebug( "-DecInt = %ld", $$ );/**/
                         }
#line 5282 "y.tab.c" /* yacc.c:1646  */
    break;

  case 446:
#line 2886 "shpars.y" /* yacc.c:1646  */
    { (yyval.longnum) = (yyvsp[0].longnum);
                           /* Ydebug( "+DecInt = %ld", $$ );/**/
                         }
#line 5290 "y.tab.c" /* yacc.c:1646  */
    break;

  case 447:
#line 2892 "shpars.y" /* yacc.c:1646  */
    { (yyval.longnum) = (yyvsp[0].longnum);
                         }
#line 5297 "y.tab.c" /* yacc.c:1646  */
    break;

  case 448:
#line 2895 "shpars.y" /* yacc.c:1646  */
    { (yyval.longnum) = (yyvsp[0].mixed)->longval;
                         }
#line 5304 "y.tab.c" /* yacc.c:1646  */
    break;

  case 449:
#line 2901 "shpars.y" /* yacc.c:1646  */
    {
                   int kw;
                   kw = LookupKeyword( (yyvsp[0].strptr) );
                   switch( kw ){
                   case DEC: case OCT: case LIN:
                     (yyval.longnum) = kw;
                     break; 
                   case FALSE:
                     MsgCompile( msgNOTRESERVKEY, (yyvsp[0].strptr) );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   default:
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "DEC", "OCT", "LIN", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   }
                 }
#line 5326 "y.tab.c" /* yacc.c:1646  */
    break;

  case 450:
#line 2922 "shpars.y" /* yacc.c:1646  */
    {
                   int kw;
                   kw = LookupKeyword( (yyvsp[0].strptr) );
                   switch( kw ){
                   case CUR: case VOL:
                     (yyval.longnum) = kw;
                     break; 
                   case FALSE:
                     MsgCompile( msgNOTRESERVKEY, (yyvsp[0].strptr) );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   default:
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "CUR", "VOL", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   }
                 }
#line 5348 "y.tab.c" /* yacc.c:1646  */
    break;

  case 451:
#line 2948 "shpars.y" /* yacc.c:1646  */
    {
                   int kw;
                   kw = LookupKeyword( (yyvsp[0].strptr) );
                   switch( kw ){
                   case POL: case ZER: case PZ:
                     (yyval.longnum) = kw;
                     break; 
                   case FALSE:
                     MsgCompile( msgNOTRESERVKEY, (yyvsp[0].strptr) );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   default:
                     MsgAllowKeywords( msgALLOWKEYWORD,  (yyvsp[0].strptr), "POL", "ZER", "PZ", "" );
                     SKIPTOEOLN; /*SkipRestLine();*/
                     break; 
                   }
                 }
#line 5370 "y.tab.c" /* yacc.c:1646  */
    break;


#line 5374 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 2968 "shpars.y" /* yacc.c:1906  */


/*-------------------------------------------------------------------------*/
/* ASB000603: all static functions move to SHparsY.C */
/*-------------------------------------------------------------------------*/
