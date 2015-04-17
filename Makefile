all:
	ml-yacc Prolog.grm
	ml-lex Prolog.lex
	sml < Main-build.sml

clean:
	rm Main-build.sml Prolog-build.sml
