#lang plai-typed

; nucleo da linguagem
(define-type ArithC
    [numC (n : number)]
    [plusC (l : ArithC) (r : ArithC)]
    [divC (l : ArithC) (r : ArithC)]
    [multC (l : ArithC) (r : ArithC)]
    [ifC (condição : ArithC) (sim : ArithC) (nao : ArithC)]
    [idC (s : symbol)]
    [appC (fun : symbol) (arg : ArithC)]
    )

; tipo para uma função
(define-type FunDefC
    [fdC [name : symbol] [arg : symbol] [body : ArithC]]
    )

; funções açucaradas
(define-type ArithS
    [numS    (n : number)]
    [plusS   (l : ArithS) (r : ArithS)]
    [bminusS (l : ArithS) (r : ArithS)]
    [divS (l : ArithS) (r : ArithS)]
    [uminusS (e : ArithS)]
    [multS   (l : ArithS) (r : ArithS)]
    [idS (s : symbol)]
    [appS (fun : symbol) (arg : ArithS)]
    [ifS (c : ArithS) (s : ArithS) (n : ArithS)]
    )

; desaçucarador
(define (desugar [as : ArithS]) : ArithC
    (type-case ArithS as
    [numS    (n)   (numC n)]
    [idS (s) (idC s)]
    [appS    (fun arg) (appC fun (desugar arg))]
    [plusS   (l r) (plusC (desugar l) (desugar r))]
    [multS   (l r) (multC (desugar l) (desugar r))]
    [divS   (l r) (divC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    ))

; interpretador, calcula o resultado em si
(define (interp [a : ArithC] [fds : (listof FunDefC)]) : number
    (type-case ArithC a
        [numC (n) n]
        [plusC (l r) (+ (interp l fds) (interp r fds))]
        [divC (l r) (/ (interp l fds) (interp r fds))]
        [multC (l r) (* (interp l fds) (interp r fds))]
        [ifC (c s n) (if (zero? (interp c fds)) (interp n fds) (interp s fds))]
        [appC (f a) (local ([define fd (get-fundef f fds)])
            (interp (subst a (fdC-arg fd) (fdC-body fd)) fds))]
        [idC (_) (error 'interp "something wrong happened!")]
    ))

; substituidor de argumento para chamada de uma função
(define (subst [valor : ArithC] [isso : symbol] [em : ArithC]) : ArithC
    (type-case ArithC em
        [numC (n) em]
        [idC (s) (cond
            [(symbol=? s isso) valor]
            [else em])]
        [appC (f a) (appC f (subst valor isso a))]
        [plusC (l r) (plusC (subst valor isso l) (subst valor isso r))]
        [multC (l r) (multC (subst valor isso l) (subst valor isso r))]
        [divC (l r) (divC (subst valor isso l) (subst valor isso r))]
        [ifC (c s n) (ifC (subst valor isso c)
            (subst valor isso s) (subst valor isso n))]
    ))

; parser, decifra as operações
(define (parse [s : s-expression]) : ArithS
    (cond
        [(s-exp-number? s) (numS (s-exp->number s))]
        [(s-exp-list? s)
        (let ([sl (s-exp->list s)])
            (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(*) (multS (parse (second sl)) (parse (third sl)))]
                [(/) (divS (parse (second sl)) (parse (third sl)))]
                [(-) (bminusS (parse (second sl)) (parse (third sl)))]
                [(~) (uminusS (parse (second sl)))]
                [(call) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
                [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [else (error 'parse "invalid list input")]
            ))]
    [else (error 'parse (s-exp->string s))]))

; buscador de funções
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
    (cond
        [(empty? fds) (error 'get-fundef "function not found")]
        [(cons? fds) (cond
            [(equal? n (fdC-name (first fds))) (first fds)]
            [else (get-fundef n (rest fds))])]))

; lista de funções 
(define biblioteca [list
    [fdC 'dobro 'x (plusC [idC 'x] [idC 'x])]
    [fdC 'quadrado 'y [multC [idC 'y] [idC 'y]]]
    [fdC 'fatorial 'n (ifC  (idC 'n)
        (multC (appC 'fatorial (plusC (idC 'n) (numC -1)))
            (idC 'n))
        (numC 1))]
    [fdC 'resposta 'x (numC 42) ]
    [fdC 'fibo 'n 
    			(ifC (idC 'n) ( ifC (plusC (idC 'n) (numC -1)) 
                                             (plusC (appC 'fibo (plusC (idC 'n) (numC -1)))
                                                    (appC 'fibo (plusC (idC 'n) (numC -2))))
                                             (numC 1)
                                            )
    							
                            (numC 0))]])

(interp (desugar (parse (read))) biblioteca)
