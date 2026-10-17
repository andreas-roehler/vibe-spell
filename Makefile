EMACS ?= emacs
BATCH = $(EMACS) --batch -L .
BATCH_NO_NATIVE = $(BATCH) --eval '(setq native-comp-eln-load-path nil)'

EL = vibe-spell.el vibe-spell-tests.el vibe-spell-bench.el
ELC = $(EL:.el=.elc)

all: $(ELC)

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

clean:
	rm -rf $(ELC) .tmp

# Hunspell is built from the hunspell/ checkout as a static library,
# compiling its sources directly; this needs no autotools run, since
# the only generated headers (hunvisapi.h, hunversion.h) are checked in.
CC ?= cc
CXX ?= c++
HUNSPELL_BUILD = .tmp/hunspell-build
HUNSPELL_LIB = $(HUNSPELL_BUILD)/libhunspell.a
HUNSPELL_MODULES = affentry affixmgr csutil filemgr hashmgr hunspell \
	hunspelltrace hunzip phonet replist suggestmgr
HUNSPELL_OBJS = $(HUNSPELL_MODULES:%=$(HUNSPELL_BUILD)/%.o)

$(HUNSPELL_BUILD)/%.o: hunspell/src/hunspell/%.cxx
	mkdir -p $(@D)
	$(CXX) -O2 -DHUNSPELL_STATIC -c -o $@ $<

$(HUNSPELL_LIB): $(HUNSPELL_OBJS)
	$(AR) rcs $@ $^

.tmp/hunspell-bench-%: hunspell-bench-%.c $(HUNSPELL_LIB)
	$(CC) -O2 -DHUNSPELL_STATIC -Ihunspell/src -o $@ $< $(HUNSPELL_LIB) -lstdc++

hunspell:
	git clone https://github.com/hunspell/hunspell.git $@

nuspell:
	git clone https://github.com/nuspell/nuspell.git $@

gutenberg:
	mkdir -p gutenberg
	curl -L -o gutenberg/lusiadas.txt https://www.gutenberg.org/ebooks/3333.txt.utf-8
	curl -L -o gutenberg/alice.txt https://www.gutenberg.org/ebooks/11.txt.utf-8

.PHONY: tests
tests: hunspell nuspell
	$(BATCH) -l ert -l vibe-spell-tests.el -f ert-run-tests-batch-and-exit

BENCH_LINES ?= 1000

.PHONY: bench-check
bench-check: gutenberg all .tmp/hunspell-bench-check
	mkdir -p .tmp
	head -n $(BENCH_LINES) gutenberg/alice.txt > .tmp/alice-sample.txt
	head -n $(BENCH_LINES) gutenberg/lusiadas.txt > .tmp/lusiadas-sample.txt
	@echo ================================================================================
	@.tmp/hunspell-bench-check en_US .tmp/alice-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench-check en_US .tmp/alice-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench-check en_US .tmp/alice-sample.txt
	@echo ================================================================================
	@.tmp/hunspell-bench-check pt_BR .tmp/lusiadas-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench-check pt_BR .tmp/lusiadas-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench-check pt_BR .tmp/lusiadas-sample.txt

MISSPELLINGS = hunspell/tests/suggestiontest/List_of_common_misspellings.txt
BENCH_SUGGEST_LINES ?= 100

.PHONY: bench-suggest
bench-suggest: all .tmp/hunspell-bench-suggest
	mkdir -p .tmp
	head -n $(BENCH_SUGGEST_LINES) $(MISSPELLINGS) > .tmp/misspellings-sample.txt
	@echo ================================================================================
	@.tmp/hunspell-bench-suggest en_US .tmp/misspellings-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench-suggest \
		en_US .tmp/misspellings-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench-suggest \
		en_US .tmp/misspellings-sample.txt
