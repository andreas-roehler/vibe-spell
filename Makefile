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

.tmp/hunspell-bench: hunspell-bench.c $(HUNSPELL_LIB)
	$(CC) -O2 -DHUNSPELL_STATIC -Ihunspell/src -o $@ $< $(HUNSPELL_LIB) -lstdc++

hunspell:
	git clone https://github.com/hunspell/hunspell.git $@

nuspell:
	git clone https://github.com/nuspell/nuspell.git $@

# Benchmark texts and dictionaries.  The dictionaries come from pinned
# Debian trixie packages, so that the C and Lisp benchmarks load the
# same files on every machine; each target downloads its package to
# .tmp and extracts the .aff/.dic pair (the .dic comes along with the
# .aff).
DATA = data/alice.txt data/lusiadas.txt data/mujeong.txt data/egri.txt \
	data/verwandlung.txt data/en_US.aff data/pt_BR.aff data/ko_KR.aff \
	data/hu_HU.aff data/de_DE.aff

.PHONY: data
data: $(DATA)

data/alice.txt:
	mkdir -p data .tmp
	curl -fL -o .tmp/$(@F) https://www.gutenberg.org/ebooks/11.txt.utf-8
	tail -n +31 .tmp/$(@F) > $@

data/lusiadas.txt:
	mkdir -p data .tmp
	curl -fL -o .tmp/$(@F) https://www.gutenberg.org/ebooks/3333.txt.utf-8
	tail -n +31 .tmp/$(@F) > $@

# First part (chapters 1-20) of Yi Kwang-su's Mujeong, from Korean
# Wikisource; the sed script drops the wiki templates (the page header
# and the paragraph-break markers), leaving plain text.
data/mujeong.txt:
	mkdir -p data
	curl -L -A vibe-spell-bench \
		'https://ko.wikisource.org/w/index.php?title=%EB%AC%B4%EC%A0%95/1%EC%9E%A5~20%EC%9E%A5&action=raw' \
		| sed -e '/^{{.*}}$$/d' -e '/^{{/,/^}}$$/d' > $@

# First volume of Gárdonyi Géza's Az egri csillagok.
data/egri.txt:
	mkdir -p data .tmp
	curl -fL -o .tmp/$(@F) https://www.gutenberg.org/ebooks/76235.txt.utf-8
	tail -n +31 .tmp/$(@F) > $@

data/verwandlung.txt:
	mkdir -p data .tmp
	curl -fL -o .tmp/$(@F) https://www.gutenberg.org/ebooks/22367.txt.utf-8
	tail -n +31 .tmp/$(@F) > $@

DEBIAN_POOL = http://deb.debian.org/debian/pool/main
DEB_en_US = $(DEBIAN_POOL)/s/scowl/hunspell-en-us_2020.12.07-4_all.deb
DEB_pt_BR = $(DEBIAN_POOL)/libr/libreoffice-dictionaries/hunspell-pt-br_25.2.3-1_all.deb
DEB_ko_KR = $(DEBIAN_POOL)/h/hunspell-dict-ko/hunspell-ko_0.7.94-2_all.deb
DEB_hu_HU = $(DEBIAN_POOL)/libr/libreoffice-dictionaries/hunspell-hu_25.2.3-1_all.deb
DEB_de_DE = $(DEBIAN_POOL)/i/igerman98/hunspell-de-de_20161207-15_all.deb
# The file name in the package, where it differs from the target's.
DEB_NAME_ko_KR = ko

data/%.aff:
	mkdir -p data .tmp
	curl -L -o .tmp/$*.deb $(DEB_$*)
	ar p .tmp/$*.deb data.tar.xz \
		| tar -xJO ./usr/share/hunspell/$(or $(DEB_NAME_$*),$*).dic > data/$*.dic
	ar p .tmp/$*.deb data.tar.xz \
		| tar -xJO ./usr/share/hunspell/$(or $(DEB_NAME_$*),$*).aff > $@

.PHONY: tests
tests: hunspell nuspell
	$(BATCH) -l ert -l vibe-spell-tests.el -f ert-run-tests-batch-and-exit

BENCH_LINES ?= 1000
# E.g. BENCH_FLAGS=--suggest to also time suggestions for the
# misspelled words found.
BENCH_FLAGS ?=

.PHONY: bench
bench: $(DATA) all .tmp/hunspell-bench
	@echo ================================================================================
	@.tmp/hunspell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) en_US data/alice.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) en_US data/alice.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) en_US data/alice.txt
# Suggestions for Korean take too long to be worth benchmarking.
ifeq ($(filter --suggest,$(BENCH_FLAGS)),)
	@echo ================================================================================
	@.tmp/hunspell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) ko_KR data/mujeong.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) ko_KR data/mujeong.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) ko_KR data/mujeong.txt
endif
	@echo ================================================================================
	@.tmp/hunspell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) pt_BR data/lusiadas.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) pt_BR data/lusiadas.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) pt_BR data/lusiadas.txt
	@echo ================================================================================
	@.tmp/hunspell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) de_DE data/verwandlung.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) de_DE data/verwandlung.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) de_DE data/verwandlung.txt
	@echo ================================================================================
	@.tmp/hunspell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) hu_HU data/egri.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l vibe-spell-bench.el -f vibe-spell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) hu_HU data/egri.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l vibe-spell-bench.el -f vibe-spell-bench --lines $(BENCH_LINES) $(BENCH_FLAGS) hu_HU data/egri.txt
