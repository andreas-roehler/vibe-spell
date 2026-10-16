EMACS ?= emacs
BATCH = $(EMACS) --batch -L .
BATCH_NO_NATIVE = $(BATCH) --eval '(setq native-comp-eln-load-path nil)'

EL = vibe-spell.el vibe-spell-tests.el vibe-spell-bench.el
ELC = $(EL:.el=.elc)

all: $(ELC)

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

clean:
	rm -rf $(ELC) bin

CC ?= cc
bin/hunspell-bench-check: hunspell-bench-check.c
	mkdir -p bin
	$(CC) -O2 -o $@ $< $$(pkg-config --cflags --libs hunspell)

bin/hunspell-bench-suggest: hunspell-bench-suggest.c
	mkdir -p bin
	$(CC) -O2 -o $@ $< $$(pkg-config --cflags --libs hunspell)

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
bench-check: gutenberg all bin/hunspell-bench-check
	head -n $(BENCH_LINES) gutenberg/alice.txt > gutenberg/alice-sample.txt
	head -n $(BENCH_LINES) gutenberg/lusiadas.txt > gutenberg/lusiadas-sample.txt
	@echo ================================================================================
	@./bin/hunspell-bench-check en_US gutenberg/alice-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench-check en_US gutenberg/alice-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench-check en_US gutenberg/alice-sample.txt
	@echo ================================================================================
	@./bin/hunspell-bench-check pt_BR gutenberg/lusiadas-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench-check pt_BR gutenberg/lusiadas-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench-check pt_BR gutenberg/lusiadas-sample.txt

MISSPELLINGS = hunspell/tests/suggestiontest/List_of_common_misspellings.txt
BENCH_SUGGEST_LINES ?= 200

.PHONY: bench-suggest
bench-suggest: all bin/hunspell-bench-suggest
	mkdir -p gutenberg
	head -n $(BENCH_SUGGEST_LINES) $(MISSPELLINGS) > gutenberg/misspellings-sample.txt
	@echo ================================================================================
	@./bin/hunspell-bench-suggest en_US gutenberg/misspellings-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench-suggest \
		en_US gutenberg/misspellings-sample.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench-suggest \
		en_US gutenberg/misspellings-sample.txt
