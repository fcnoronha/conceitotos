# Conceitos em linguagem de programação

Projeto da disciplina MAC0316. Este é o projeto de uma calculadora implementada em racket. A parte da gramatica é feita pelos arquivos `mcalc.`[l|y], que transformam uma expressão aritmetica convencional pela sua representação na *notação polonesa*. A aplicação `direto.rkt` calcula a expressão resultante.

## Grupo

- Carolina Senra Marques - NUSP: 10737101
- Felipe Castro de Noronha - NUSP: 10737032
- Raphael Ribeiro - NUSP: 10281601

## Como buildar

Precisamos instalar as dependencias e criar o executavel.

```terminal
$ sudo apt-get install flex
$ sudo apt-get install bison
$ sudo apt-get install racket
$ raco pkg install plai-typed
$ make all
```

## Como usar

Para calcular uma expressão matematica, basta dar um *pipe* entre `mcalc` e `direto` e depois digitar a expressão. Por exemplo:

```terminal
$ ./mcalc | ./direto
> (6 / 2 + 4)
7

$ ./mcalc | ./direto
> (5 / 2 - 1)
1 1/2

$ ./mcalc | ./direto
> (7 / 8 + 8)
8 7/8

$ ./mcalc | ./direto
> ((4 + 4) ? 2 4)
2

$ ./mcalc | ./direto
> ((4 - 4) ? 2 4)
4

$ ./mcalc | ./direto
> ((4 - 5) ? 0 1)
1

$ ./mcalc | ./direto
> CALL dobro 5
10

$ ./mcalc | ./direto
> CALL quadrado 10
100

$ ./mcalc | ./direto
> CALL fatorial 5
120

$ ./mcalc | ./direto
> CALL resposta 12
42

```
