LEXER=lexer
PARSER=grammar
FLASL_INP=testcases/god1.flasl
TAB_OUT=output/god1.out
DEPS=proptableau.sml flasl2ast.sml lexer.lex grammar.grm datatypes.sml

TESTCASES = $(wildcard testcases/*.sml)
OUTPUT = $(patsubst testcases/%.sml, output/%.out, $(TESTCASES))

.PHONY: flasl2ast
flasl2ast: $(FLASL_INP)
	sml flasl2ast.sml $(FLASL_INP) $(TAB_OUT)

.PHONY: clean

clean:
	@rm -rf $(LEXER).lex.sml $(PARSER).grm.desc $(PARSER).grm.sig $(PARSER).grm.sml output/*.out .cm

.PHONY: test

test: $(OUTPUT)

output/%.out: testcases/%.sml
	CM_VERBOSE=false sml flasl2ast.sml $< $@
