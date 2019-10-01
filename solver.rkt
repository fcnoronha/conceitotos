#lang plai-typed

; nucleo da linguagem
(define-type ExprC
    [numC   (n : number)]
    [plusC  (l : ExprC) (r : ExprC)]
    [divC   (l : ExprC) (r : ExprC)]
    [multC  (l : ExprC) (r : ExprC)]
    [idC    (s : symbol)]
    [appC   (fun : ExprC) (arg : ExprC)] ; agora recebe uma expressão
    [fdC    (name : symbol) (arg : symbol) (body : ExprC)] ; nao é mais definido fora
    [ifC    (condicao : ExprC) (sim : ExprC) (nao : ExprC)]
)

; funções açucaradas
(define-type ExprS
    [numS    (n : number)]
    [plusS   (l : ExprS) (r : ExprS)]
    [bminusS (l : ExprS) (r : ExprS)]
    [divS    (l : ExprS) (r : ExprS)]
    [uminusS (e : ExprS)]
    [multS   (l : ExprS) (r : ExprS)]
    [idS     (s : symbol)]
    [appS    (fun : ExprS) (arg : ExprS)]
    [ifS     (c : ExprS) (s : ExprS) (n : ExprS)]
    [fdS     (name : symbol) (arg : symbol) (body : ExprS)]
    )

; desaçucarador
(define (desugar [as : ExprS]) : ExprC
    (type-case ExprS as
    [numS    (n)   (numC n)]
    [idS     (s) (idC s)]
    [fdS     (n a b) (fdC n a (desugar b))]
    [appS    (fun arg) (appC (desugar fun) (desugar arg))]
    [plusS   (l r) (plusC (desugar l) (desugar r))]
    [multS   (l r) (multC (desugar l) (desugar r))]
    [divS    (l r) (divC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    ))

; binds são os 'nos' que compoem o enviroment
(define-type Binding
    [bind (name : symbol) (val : Value)])

; definições necessarias para o enviroment em si
(define-type-alias Env (listof Binding ))
(define mt-env empty) ; enviroment vazio - eMpTy
(define extend-env cons) ; pra dar 'append' no env

; valor 'maleavel' usado como retorno pelo interpretador
(define-type Value
    [numV (n : number)]
    [funV (name : symbol) (arg : symbol) (body : ExprC)]
    )

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

; interpretador, calcula o resultado em si
(define (interp [a : ExprC] [env : Env]) : Value
    (type-case ExprC a
        [numC  (n) (numV n)]
        [plusC (l r) (num+ (interp l env) (interp r env))]
        [divC  (l r) (num/ (interp l env) (interp r env))]
        [multC (l r) (num* (interp l env) (interp r env))]
        [ifC   (c s n) (if (zero? (numV-n (interp c env)))
                         (interp n env) (interp s env))]
        [appC  (f a) (local ([define fd (interp f env)]) ; checa se é mesmo funV
            (interp (funV-body fd)
                (extend-env (bind
                    (funV-arg fd) (interp a env)) mt-env)))]
        [idC   (n) (lookup n env)] ; podemos ter isso agora que temos env
        [fdC   (n a b) (funV n a b)] ; função se autorepresenta
    ))

; lookup - busca no enviroment
(define (lookup [for : symbol] [env : Env]) : Value
    (cond
        [(empty? env) (error 'lookup "name not found")]
        [else (cond
            [(symbol=? for (bind-name (first env)))
                           (bind-val (first env))]
            [else (lookup for (rest env))])]))

; obsoleto - não é mais usado
; substituidor de argumento para chamada de uma função
; (define (subst [valor : ExprC] [isso : symbol] [em : ExprC]) : ExprC
;     (type-case ExprC em
;         [numC  (n) em]
;         [idC   (s) (cond
;             [(symbol=? s isso) valor]
;             [else em])]
;         [appC  (f a) (appC f (subst valor isso a))]
;         [plusC (l r) (plusC (subst valor isso l) (subst valor isso r))]
;         [multC (l r) (multC (subst valor isso l) (subst valor isso r))]
;         [divC  (l r) (divC (subst valor isso l) (subst valor isso r))]
;         [ifC   (c s n) (ifC (subst valor isso c)
;             (subst valor isso s) (subst valor isso n))]
;     ))

; parser, decifra as operações
(define (parse [s : s-expression]) : ExprS
    (cond
        [(s-exp-number? s) (numS (s-exp->number s))]
        [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; simbolo livre
        [(s-exp-list? s)
        (let ([sl (s-exp->list s)])
            (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(*) (multS (parse (second sl)) (parse (third sl)))]
                [(/) (divS (parse (second sl)) (parse (third sl)))]
                [(-) (bminusS (parse (second sl)) (parse (third sl)))]
                [(~) (uminusS (parse (second sl)))]
                [(call) (appS (parse (second sl)) (parse (third sl)))]
                [(if) (ifS
                    (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(func) (fdS (s-exp->symbol (second sl))
                             (s-exp->symbol (third sl))
                             (parse         (fourth sl)))] ; corpo
                [else (error 'parse "invalid list input")]
            ))]
    [else (error 'parse (s-exp->string s))]))

; obsoleto
; buscador de funções
; (define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
;     (cond
;         [(empty? fds) (error 'get-fundef "function not found")]
;         [(cons? fds) (cond
;             [(equal? n (fdC-name (first fds))) (first fds)]
;             [else (get-fundef n (rest fds))])]))

; lista de funções
; (define biblioteca [list
;     [fdC 'dobro 'x (plusC [idC 'x] [idC 'x])]
;     [fdC 'quadrado 'y [multC [idC 'y] [idC 'y]]]
;     [fdC 'fatorial 'n (ifC  (idC 'n)
;         (multC (appC 'fatorial (plusC (idC 'n) (numC -1)))
;             (idC 'n))
;         (numC 1))]
;     [fdC 'resposta 'x (numC 42) ]
;     [fdC 'fibo 'n
; 		(ifC (idC 'n) ( ifC (plusC (idC 'n) (numC -1))
;             (plusC (appC 'fibo (plusC (idC 'n) (numC -1)))
;                     (appC 'fibo (plusC (idC 'n) (numC -2))))
;             (numC 1))
;             (numC 0))]])


; Tem que fazer o check e tratamento de erro se é numero ou nao
(numV-n (interp (desugar (parse (read))) mt-env))
