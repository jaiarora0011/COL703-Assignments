LEXER=lexer
PARSER=grammar
FLASL_INP=arg-inp.flasl
SML_OUT=arg.sml
SML_IN=arg.sml
FLASL_OUT=arg-out.flasl

.PHONY: flasl2ast

flasl2ast:
	sml flasl2ast.sml $(FLASL_INP) $(SML_OUT)


.PHONY: clean

clean:
	@rm -f $(LEXER).lex.sml $(PARSER).grm.desc $(PARSER).grm.sig $(PARSER).grm.sml

.PHONY: test

test:
	sml flasl2ast.sml testcase1.txt arg-1.sml
	sml flasl2ast.sml testcase2.txt arg-2.sml
	sml flasl2ast.sml testcase3.txt arg-3.sml
	sml flasl2ast.sml testcase4.txt arg-4.sml
	sml flasl2ast.sml testcase5.txt arg-5.sml
