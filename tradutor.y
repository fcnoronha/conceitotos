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
    sprintf(res, "(:= %s %s)", nome, value);
    return res;
}

char *seq(char *exp1, char*exp2) {
    char *res = malloc(strlen(exp1)+strlen(exp2)+16);
    sprintf(res, "(seq %s %s)", exp1, exp2);
    fprintf(stderr, "%s\n", res);
    return res;
}

char *define(char *symbol, char*value, char*exp) {
    char *res = malloc(strlen(symbol)+strlen(value)+strlen(exp)+26);
    sprintf(res, "(def %s %s %s)", symbol, value, exp);
    return res;
}

char *funcDef(char *nome, char*arg, char*body) {
    char *res = malloc(strlen(nome)+strlen(arg)+strlen(body)+30);
    sprintf(res, "(def %s -1 (:= %s (func %s %s)) )", nome, nome, arg, body);
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
%token OPENFUNC
%token CLOSEFUNC
%token LET
%token FUNC
%token ADD SUB MUL DIV PRINT OPEN CLOSE IF
%type <val> exp
%type <val> SYMBOL
%type <val> callfunc
%type <val> atrib
%type <val> seq
%type <val> funcdef
%type <val> def
%token NEWLINE
%token END 0

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
        
        |       SYMBOL      { $$ = dup($1);}
        |       callfunc    { $$ = dup($1);}
        |       funcdef     { $$ = dup($1);}
        |       atrib       { $$ = dup($1);}
        |       seq         { $$ = dup($1);}
        |       def         { $$ = dup($1);}
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

def:
                DEF SYMBOL ATRIB exp SEQ exp { $$ = define(dup($2), dup($4), dup($6));}
            |   DEF SYMBOL ATRIB exp SEQ NEWLINE exp { $$ = define(dup($2), dup($4), dup($7));}
;

atrib:
                SYMBOL ATRIB OPEN exp CLOSE    { $$ = atrib(dup($1), dup($4));}
;

seq:
                exp SEQ             { $$ = dup($1);}
            |   exp SEQ exp         { $$ = seq(dup($1), dup($3));}
            |   exp SEQ NEWLINE exp { $$ = seq(dup($1), dup($4));}
            |   exp SEQ NEWLINE     { $$ = dup($1);}

;

funcdef:
                FUNC SYMBOL OPEN SYMBOL CLOSE OPENFUNC exp CLOSEFUNC  { $$ = funcDef(dup($2), dup($4), dup($7));}
;



%%

void yyerror(char *s) {
    fprintf(stderr,"%s\n",s);
}
