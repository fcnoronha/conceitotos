/* Calculadora infixa */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
char *oper(char op, char *l, char *r) {
	char *res = malloc(strlen(l)+strlen(r)+6);
	sprintf(res, "(%c %s %s)", op, l, r);
	return res;
}
char *operIf(char *c, char *s, char *n) {
	char *res = malloc(strlen(c)+strlen(s)+strlen(n)+8);
	sprintf(res, "(if %s %s %s)", c, s, n);
	return res;
}
char *callFunc(char *nome, char*arg) {
	char *res = malloc(strlen(nome)+strlen(arg)+12);
	sprintf(res, "(call %s %s)", nome, arg);
	return res;
}
char *dup(char *orig) {
	char *res = malloc(strlen(orig)+1);
	strcpy(res,orig);
	return res;
}
int yylex();
void yyerror(char *);
%}

%union {
	char *val;
}

%token	<val> NUM
%token CALL
%token SYMBOL
%token  ADD SUB MUL DIV PRINT OPEN CLOSE IF
%type	<val> exp
%type <val> callfunc
%type <val> SYMBOL
%token END 0 "end of file"

%left ADD SUB
%left MUL DIV
%left NEG
%left IF

/* Gramatica */
%%

input:
		| 		exp     { puts($1);}
		| 		error  	{ fprintf(stderr, "Entrada inválida\n"); }
;

exp: 			NUM 		{ $$ = dup($1); }
		|       callfunc    { $$ = dup($1);}
		| 		exp ADD exp	{ $$ = oper('+', $1, $3);}
		| 		exp SUB exp	{ $$ = oper('-', $1, $3);}
		| 		exp MUL exp	{ $$ = oper('*', $1, $3);}
		| 		exp DIV exp	{ $$ = oper('/', $1, $3);}
		| 		SUB exp %prec NEG   { $$ = oper('~', $2, "");}
        |       exp IF exp exp      { $$ = operIf($1, $3, $4);}
		| 		OPEN exp CLOSE	    { $$ = dup($2);}

;

callfunc:
				CALL SYMBOL exp			{ $$ = callFunc($2,$3);}
		

%%

void yyerror(char *s) {
  fprintf(stderr,"%s\n",s);
}
