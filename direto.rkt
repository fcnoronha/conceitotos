#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [divC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
  [ifC (condição : ArithC) (sim : ArithC) (nao : ArithC)])
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ArithC)]

(define-type FunDefC
  [fdC [name : symbol] [arg : symbol] [body : ArithC]]
  )

(define-type ArithS
  [numS    (n : number)]
  [plusS   (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [divS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [multS   (l : ArithS) (r : ArithS)]
  [idS (s : symbol)]
  [ifS (c : ArithS) (s : ArithS) (n : ArithS)])


(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS    (n)   (numC n)]
    [plusS   (l r) (plusC (desugar l) (desugar r))]
    [multS   (l r) (multC (desugar l) (desugar r))]
    [divS   (l r) (divC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    ))


(define (interp [a : ArithC]) [fds : [listof FunDefC] : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [divC (l r) (/ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    [ifC (c s n) (if (zero? (interp c)) (interp n) (interp s))]
    [appC (f a) (local ([define fd(get-fundef f fds)])
                  (interp (subst a
                                 (fdC-arg fd)
                                 (fdC-body fd))
                           fds))]
    [idc (_) (error 'interp "something wrong happened!")]
    ))


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
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define (get-fundef [n : symbol] [fds : (listof DunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "function not found")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(define biblioteca [list
        [fdC 'dobro' 'x (plusC [idC 'x] [idC 'x])]
        [fdC 'quadrado' 'y [multC [idC 'y] [idC 'y]]
        [fdC 'fatorial' 'n (ifC (idC 'n) 
        (multC (appC 'fatorial (plusC (idC 'n) (numC -1))) (idC 'n)) 
        [fdC 'somaQuatro' 'x (plusC (idC 'x) 4)]
        [fdC 'resposta' 'x (42)]
        ]
)


(interp (desugar (parse (read))) biblioteca)
