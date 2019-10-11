#lang plai-typed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NUCLEO DA LINGAUEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type ExprC
    [numC   (n : number)]
    [plusC  (l : ExprC) (r : ExprC)]
    [divC   (l : ExprC) (r : ExprC)]
    [multC  (l : ExprC) (r : ExprC)]
    [varC   (s : symbol ) ] ; variavel
    [appC   (fun : ExprC) (arg : ExprC)] ; agora recebe uma expressão
    [lamC   (arg : symbol) (body : ExprC)] ; nao é mais definido fora
    [ifC    (condicao : ExprC) (sim : ExprC) (nao : ExprC)]
    [setC   (var : symbol) (arg : ExprC)] ; atribuição varivael
    [seqC   (b1 : ExprC) (b2 : ExprC)] ; executa b1 depois b2
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AÇUCAR DA LINGAUEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; funções açucaradas
(define-type ExprS
    [numS    (n : number)]
    [plusS   (l : ExprS) (r : ExprS)]
    [bminusS (l : ExprS) (r : ExprS)]
    [divS    (l : ExprS) (r : ExprS)]
    [uminusS (e : ExprS)]
    [multS   (l : ExprS) (r : ExprS)]
    [varS    (s : symbol)]
    [appS    (fun : ExprS) (arg : ExprS)]
    [ifS     (c : ExprS) (s : ExprS) (n : ExprS)]
    [lamS    (arg : symbol) (body : ExprS)]
    [setS    (var : symbol) (arg : ExprS)]
    [seqS    (b1 : ExprS) (b2 : ExprS)]
    [letS    (id : symbol) (arg : ExprS) (body : ExprS)]
)

; desaçucarador
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)   (numC n)]
    [varS    (s)   (varC s)]
    [lamS    (a b) (lamC a (desugar b))]
    [appS    (fun arg) (appC (desugar fun) (desugar arg))]
    [plusS   (l r) (plusC (desugar l) (desugar r))]
    [multS   (l r) (multC (desugar l) (desugar r))]
    [divS    (l r) (divC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    [setS    (v a) (setC v (desugar a))]
    [seqS    (b1 b2) (seqC (desugar b1) (desugar b2))]
    [letS    (id val expr) (appC  (lamC id (desugar expr)) (desugar val))]
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OPERADORES ARITMETICOS FUNDAMENTAIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; operadores aritmeticos que lidam com o value
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
            (numV (+ (numV-n l) (numV-n r)))]
        [else (error 'num+ "Um dos argumentos não é um número")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
            (numV (* (numV-n l) (numV-n r)))]
        [else (error 'num+ "Um dos argumentos não é um número")]))

(define (num/ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
            (numV (/ (numV-n l) (numV-n r)))]
        [else (error 'num+ "Um dos argumentos não é um número")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VARIAVEIS / MUTAÇÃO / CLOSURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; valor 'maleavel' usado como retorno pelo interpretador
(define-type Value
    [numV (n : number)]
    [closV (arg : symbol) (body : ExprC) (env : Env)]
)

(define-type-alias Location number)

; binds são os 'nos' que compoem o enviroment
(define-type Binding
    [bind (name : symbol) (val : Location)])

; definições necessarias para o enviroment em si
(define-type-alias Env (listof Binding ))
(define mt-env empty) ; enviroment vazio - eMpTy
(define extend-env cons) ; pra dar 'append' no env

; cell são os 'nos' que compoem o store
(define-type Storage
      [cell (location : Location) (val : Value)])

; definições necessarias para o store em si
(define-type-alias Store (listof Storage))
(define mt-store empty) ; store vazio
(define override-store cons) ; pra mudar valor no store

; lookup - busca no enviroment
(define (lookup [for : symbol] [env : Env]) : Location
    (cond
        [(empty? env)
            (error 'lookup (string-append (symbol->string for) " not found"))]
        [else (cond
            [(symbol=? for (bind-name (first env)))
                           (bind-val (first env))]
            [else (lookup for (rest env))])]))

; fetch - busca no store
(define (fetch [l : Location] [sto : Store]) : Value
    (cond
        [(empty? sto) (error 'fetch "position not found")]
            [else (cond
                [(= l (cell-location (first sto)))
                      (cell-val (first sto))]
                [else (fetch l (rest sto))])]))

; retorna a próxima box livre
(define new-loc
   (let ([n (box 0)])
        (lambda ()
           (begin (set-box! n (+ 1 (unbox n))) (unbox n)))))

; novo retorno das funções
(define-type Result
    [v*s (v : Value) (s : Store)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INTERPRETADOR - SOLVER EM SI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; interpretador, calcula o resultado em si
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
    (type-case ExprC a
        [numC (n) (v*s (numV n) sto)]
        [plusC (l r)
            (type-case Result (interp l env sto)
                [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                        [v*s (v-r s-r) (v*s (num+ v-l v-r) s-r)])])]
        [divC  (l r)
            (type-case Result (interp l env sto)
                [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                        [v*s (v-r s-r) (v*s (num/ v-l v-r) s-r)])])]
        [multC (l r)
            (type-case Result (interp l env sto)
                [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                        [v*s (v-r s-r) (v*s (num/ v-l v-r) s-r)])])]
        [ifC (c s n) (if (zero? (numV-n (v*s-v (interp c env sto))))
                            (interp n env sto)
                            (interp s env sto))]
        [appC (f a)
            (type-case Result (interp f env sto)
                [v*s (v-f s-f)
                    (type-case Result (interp a env s-f)
                        [v*s (v-a s-a)
                            (let ([lugar (new-loc)])
                                (interp (closV-body v-f)
                                        (extend-env (bind (closV-arg v-f) lugar)
                                                    (closV-env v-f))
                                        (override-store (cell lugar v-a) s-a)))])])]
        [varC  (n) (v*s (fetch (lookup n env) sto) sto)]
        [lamC (a b) (v*s (closV a b env) sto)]
        [seqC (b1 b2) (type-case Result (interp b1 env sto)
                        [v*s (v-b1 s-b1) (interp b2 env s-b1)])]
        [setC (var val) (type-case Result (interp val env sto)
            [v*s (v-val s-val)
                (let ([onde (lookup var env)]) ; procura varivael
                    (v*s v-val
                        (override-store ; atualiza
                            (cell onde v-val) s-val)))])]
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PARSER - ARVORE DE INTERPRETAÇÃO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; parser, decifra as operações
(define (parse [s : s-expression]) : ExprS
    (cond
        [(s-exp-number? s) (numS (s-exp->number s))]
        [(s-exp-symbol? s) (varS (s-exp->symbol s))] ; simbolo livre
        [(s-exp-list? s)   (let ([sl (s-exp->list s)])
            (case (s-exp->symbol (first sl))
                [(+)    (plusS (parse (second sl)) (parse (third sl)))]
                [(*)    (multS (parse (second sl)) (parse (third sl)))]
                [(/)    (divS (parse (second sl)) (parse (third sl)))]
                [(-)    (bminusS (parse (second sl)) (parse (third sl)))]
                [(~)    (uminusS (parse (second sl)))]
                [(if)   (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(call) (appS (parse (second sl)) (parse (third sl)))]
                [(:=)   (setS (s-exp->symbol (second sl)) (parse (third sl)))]
                [(seq)  (seqS (parse (second sl)) (parse (third sl)))]
                [(def) (letS (s-exp->symbol (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(func) (lamS (s-exp->symbol (second sl)) (parse (third sl)))]
                [else (error 'parse "invalid list input")]))]
    [else (error 'parse (s-exp->string s))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AÇUCAR RACKETIZADO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; aqui seria legal definir uns açucares usando o define.
; coisas que seriam legais:
;   criar variavel e ja definir valor pra ela

(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env mt-store))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXECUTANDO A ROTINA COM AS FUNÇÕES DA LINGUAGEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(numV-n (v*s-v (interpS (read))))
