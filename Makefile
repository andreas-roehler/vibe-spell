EMACS ?= emacs
BATCH = $(EMACS) --batch -L .

EL = vibe-spell.el vibe-spell-tests.el vibe-spell-bench.el
ELC = $(EL:.el=.elc)

all: $(ELC)

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

clean:
	rm -rf $(ELC) bin

CC ?= cc
bin/hunspell-bench: hunspell-bench.c
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

BENCH_LINES = 1000

.PHONY: bench
bench: gutenberg all bin/hunspell-bench
	head -n $(BENCH_LINES) gutenberg/alice.txt > gutenberg/alice-sample.txt
	head -n $(BENCH_LINES) gutenberg/lusiadas.txt > gutenberg/lusiadas-sample.txt
	@echo ================================================================================
	./bin/hunspell-bench en_US gutenberg/alice-sample.txt
	@echo ================================================================================
	$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench-run en_US gutenberg/alice-sample.txt
	@echo ================================================================================
	./bin/hunspell-bench pt_BR gutenberg/lusiadas-sample.txt
	@echo ================================================================================
	$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench-run pt_BR gutenberg/lusiadas-sample.txt
