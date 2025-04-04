# include <stdio.h>
extern struct {int *yyaa, *yybb; int *yystops;}
	      *yylstate [], **yylsp, **yyolsp;

extern FILE *yyout, *yyin;
extern int yyprevious , *yyfnd;
extern char yyextra[];
extern char yytext[];
extern int yyleng;

extern int yyback( int *, int );
extern int yyinput( void );
extern int yyoutput( int );
extern int yyunput( int );
int yyracc( int );

int yyreject ()
{
	for( ; yylsp < yyolsp; yylsp++)
		yytext[yyleng++] = yyinput();
	if (*yyfnd > 0)
		return(yyracc(*yyfnd++));
	while (yylsp-- > yylstate) {
	       yyunput(yytext[yyleng-1]);
	       yytext[--yyleng] = 0;
	       if (*yylsp != 0 && (yyfnd= (*yylsp)->yystops) && *yyfnd > 0)
		       return(yyracc(*yyfnd++));
	}
	if (yytext[0] == 0)
	       return(0);
	yyoutput(yyprevious = yyinput());
	yyleng=0;
	return(-1);
}

int yyracc(int m)
{
	yyolsp = yylsp;
	if (yyextra[m]) {
	      while (yyback((*yylsp)->yystops, -m) != 1 && yylsp>yylstate) {
		       yylsp--;
		       yyunput(yytext[--yyleng]);
		}
	}
	yyprevious = yytext[yyleng-1];
	yytext[yyleng] = 0;
	return(m);
}
