all: tradutor solver

tradutor: tradutor.tab.o lex.yy.o main.o
	gcc -o $@ $^ -lfl

tradutor.tab.o: tradutor.y
	bison -d tradutor.y
	gcc -c tradutor.tab.c

lex.yy.o: tradutor.l
	flex tradutor.l
	gcc -c lex.yy.c

solver: solver.rkt
	raco exe $<

clean:
	rm -f *.o lex.yy.c tradutor.tab.c tradutor.tab.h solver tradutor *~
