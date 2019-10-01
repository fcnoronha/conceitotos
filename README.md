# Conceitos em linguagem de programação - Arith

Projeto da disciplina MAC0316. Este é o projeto de uma calculadora implementada em racket. A parte da gramatica é feita pelos arquivos `tradutor.`[l|y], que transformam uma expressão aritmetica convencional em sua representação na *notação polonesa*. A aplicação `solver.rkt` calcula o valor resultante da expressao que lhe foi passada.

Optou-se por, alem de incluir o operador divisao e condicionais, criar 5 funcoes: `dobro`, `quadrado`, `fatorial`, `fibo` e `resposta`(*Answer to the Ultimate Question of Life, the Universe, and Everything*). A chamada dessas funcoes segue o formato `(CALL <nome> <arg>)`, como se pode ver nos exemplos. Já a condicional tem a forma `(<cond> ? <caso-sim> <caso-nao>)`, onde qualquer valor para cond diferente de 0 sera interpretado como 'sim'. A divisao pode ser realizada fazendo `(<expressao> / <expressao>)` e pode ser acompanhada de outras operacoes aritmeticas.

## Como buildar

Para executar o projeto precisamos instalar as dependencias e criar o executavel. Basta executar as seguintes linhas:

```terminal
$ sudo apt-get install flex
$ sudo apt-get install bison
$ sudo apt-get install racket
$ raco pkg install plai-typed
$ make all
```

## Segundo compilador

    Vamos deixar as coisas que vao ter que ser incluidas no relatorio aq. Tem que escrever de um jeito mais bonitinho dps

- Foi adicionado um enviroment, #escopoDinamico
- Nomeclatura foi mudada, não é mais arit, e sim expr
- Não temos mais uma bib de funções, agora elas são implementadas no core da linguagem

## Testes

Para calcular uma expressão matematica, basta dar um *pipe* entre `tradutor` e `solver` e depois digitar a expressão. A seguir, temos diversos exemplos da execução do programa:

```terminal
$ ./tradutor | ./solver
> (6 / 2 + 4)
7

$ ./tradutor | ./solver
> (5 / 2 - 1)
1 1/2

$ ./tradutor | ./solver
> (5 / (2 - 1))
5

$ ./tradutor | ./solver
> ((4 + 4) ? 2 4)
2

$ ./tradutor | ./solver
> ((4 - 4) ? 2 4)
4

$ ./tradutor | ./solver
> ((4 - 5) ? 0 1)
0

$ ./tradutor | ./solver
> (CALL dobro 5)
10

$ ./tradutor | ./solver
> (CALL quadrado 10)
100

$ ./tradutor | ./solver
> (CALL fatorial 5)
120

$ ./tradutor | ./solver
> (CALL resposta 12)
42

$ ./tradutor | ./solver
> (CALL fatorial 2 + (CALL fibo 3 + (CALL quadrado 2)))
1307674368000
```

## Grupo

- Carolina Senra Marques - NUSP: 10737101
- Felipe Castro de Noronha - NUSP: 10737032
- Raphael Ribeiro - NUSP: 10281601
