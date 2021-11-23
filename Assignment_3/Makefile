TESTCASES = $(wildcard testcases/*.sml)
OUTPUT = $(patsubst testcases/%.sml, output/%.out, $(TESTCASES))

.PHONY: all
all: test

.PHONY: clean

clean:
	@rm -rf output/*.out

.PHONY: test

test: $(OUTPUT)

output/%.out: testcases/%.sml
	-CM_VERBOSE=false sml main.sml $< $@
