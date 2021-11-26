TESTCASES = $(wildcard testcases/*.sml)
OUTPUT = $(patsubst testcases/%.sml, output/%.dot, $(TESTCASES))

.PHONY: all
all: test

.PHONY: clean

clean:
	@cd output && rm -rf *.dot *.aux *.log *.pdf *.tex

.PHONY: test

test: $(OUTPUT)

output/%.dot: testcases/%.sml
	CM_VERBOSE=false sml main.sml $< $@
	dot2tex $@ > $@.tex
	cd output && pdflatex $*.dot.tex
