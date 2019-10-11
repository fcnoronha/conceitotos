D [0-9]
L [a-zA-Z]

%{
#include <math.h>
#include "tradutor.tab.h"
%}

%%

{D}*[.]?{D}+    { yylval.val = strdup(yytext); return NUM; }

[ \t]+  /* pula espacos em branco */

#.*     /* comentarios simples */

CALL    { return CALL; }
:=		{  return ATRIB; }
LET     { return DEF;   }
FUNC    { return FUNC;  }
;       { return SEQ;   }
FIM     { return END; }
{L}+    { yylval.val = strdup(yytext) ;return SYMBOL;}
[(]     { return OPEN; }
[)]     { return CLOSE; }
[{]     { return OPENFUNC; }
[}]     { return CLOSEFUNC; }
[+]     { return ADD; }
[-]     { return SUB; }
[*]     { return MUL; }
[/]     { return DIV; }
[?]     { return IF; }
[\n]    {  return NEWLINE; }



.       { fprintf(stderr, "Entrada ignorada\n-->%s\n", yytext); }

%%
