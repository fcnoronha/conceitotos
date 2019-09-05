# Conceitos em linguagem de programação

Projeto da disciplina MAC0316. Este é o projeto de uma calculadora implementada em racket. A parte da gramatica é feita pelos arquivos `mcalc.`[l|y], que transformam uma expressão aritmetica convencional pela sua representação na *notação polonesa*. A aplicação `direto.rkt` calcula a expressão resultante.

## Grupo

- Carolina Senra Marques - NUSP: 10737101
- Felipe Castro de Noronha - NUSP: 10737032
- Raphael Ribeiro - NUSP:

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
> (5 * 2 + 4)
14

$ ./mcalc | ./direto
> ((4 + 4) ? 2 4)
2

$ ./mcalc | ./direto
> ((4 - 4) ? 2 4)
4
```
