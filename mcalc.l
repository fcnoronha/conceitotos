D [0-9]
E [Ee][+-]?{D}+

%{
#include <math.h>
#include "mcalc.tab.h"
%}

%%

{D}*[.]?{D}+{E}?	{ yylval.val = yytext; return NUM; }

[ \t]+	/* pula espacos em branco */

#.*		/* comentarios simples */

[(]		{ return OPEN; }
[)]		{ return CLOSE; }
[+]		{ return ADD; }
[-]		{ return SUB; }
[*]		{ return MUL; }
[/]		{ return DIV; }
[?]     { return IF; }

[\n]    {  return END; }

.       { fprintf(stderr, "Entrada ignorada\n-->%s\n", yytext); }

%%

/* Local Variables: */
/* mode: c */
/* End: */
