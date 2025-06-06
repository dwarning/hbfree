#include <stdint.h>
extern int yyunput( int );
void yyless(int x)
{
	extern char yytext[];
	register char *lastch, *ptr;
	extern int yyleng;
	extern int yyprevious;

	lastch = yytext+yyleng;
	if (x>=0 && x <= yyleng)
		ptr = x + yytext;
	else
		ptr = (char*)(uintptr_t) x;
	while (lastch > ptr)
		yyunput(*--lastch);
	*lastch = 0;
	if (ptr >yytext)
		yyprevious = *--lastch;
	yyleng = ptr-yytext;
}
