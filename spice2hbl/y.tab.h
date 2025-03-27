/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

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
#line 97 "shpars.y" /* yacc.c:1909  */

    long    longnum;    /* integer value */
    double  floatnum;   /* float value */
    char    *strptr;    /* pointer to identifier */
    value   *mixed;     /* mixed value for number-identifier */

#line 279 "y.tab.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
