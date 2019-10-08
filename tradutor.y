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

char *atrib(char *nome, char*value) {
    char *res = malloc(strlen(nome)+strlen(value)+16);
    sprintf(res, "(:= %s (%s))", nome, value);
    return res;
}

char *seq(char *exp1, char*exp2) {
    char *res = malloc(strlen(exp1)+strlen(exp2)+16);
    sprintf(res, "(seq (%s) (%s))", exp1, exp2);
    return res;
}

char *funcDef(char *nome, char*arg, char*body) {
    char *res = malloc(strlen(nome)+strlen(arg)+22);
    sprintf(res, "(def %s %s (%s))", nome, arg, body);
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

%token  <val> NUM
%token CALL
%token SYMBOL
%token SEQ
%token DEF
%token ATRIB
%token ADD SUB MUL DIV PRINT OPEN CLOSE IF
%type <val> exp
%type <val> SYMBOL
%type <val> callfunc
%type <val> atrib
%type <val> seq
%type <val> funcdef
%type <val> ATRIB
%type <val> SEQ
%type <val> DEF
%token END 0 "end of file"

%left ADD SUB
%left MUL DIV
%left NEG
%left IF

/* Gramatica */
%%

input:
        |       exp     { puts($1);}
        |       error   { fprintf(stderr, "Entrada inválida\n"); }
;

exp:            NUM         { $$ = dup($1);}
        |       callfunc    { $$ = dup($1);}
        |       funcdef     { $$ = dup($1);}
        |       atrib       { $$ = dup($1);}
        |       seq         { $$ = dup($1);}
        |       exp ADD exp { $$ = oper('+', $1, $3);}
        |       exp SUB exp { $$ = oper('-', $1, $3);}
        |       exp MUL exp { $$ = oper('*', $1, $3);}
        |       exp DIV exp { $$ = oper('/', $1, $3);}
        |       exp IF exp exp      { $$ = operIf($1, $3, $4);}
        |       SUB exp %prec NEG   { $$ = oper('~', $2, "");}
        |       OPEN exp CLOSE      { $$ = dup($2);}
;

callfunc:
                CALL SYMBOL exp     { $$ = callFunc(dup($2), dup($3));}
;

atrib:
                ATRIB SYMBOL exp    { $$ = atrib(dup($2), dup($3));}
;

seq:
                SEQ exp exp         { $$ = seq(dup($2), dup($3));}
;

funcdef:
                DEF SYMBOL exp exp   { $$ = funcDef(dup($2), dup($3), dup($4));}
;

%%

void yyerror(char *s) {
    fprintf(stderr,"%s\n",s);
}
