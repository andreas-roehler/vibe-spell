/* hunspell-bench-check.c --- apples-to-apples C benchmark for vibe-spell-bench.el
 *
 * Measures how long Hunspell_create() takes to load a dictionary, and
 * how long Hunspell_spell() then takes to check every word of a file,
 * the same way vibe-spell-bench.el measures vibe-spell-create and
 * vibe-spell-check: the file is read into memory once, up front,
 * before any checking is timed; words are then found and checked on
 * the fly, one at a time, rather than first collected into a list; a
 * word is a maximal run of Unicode alphabetic characters, apostrophes
 * and hyphens, decoded from the file's assumed UTF-8 encoding -- the
 * same definition `(rx (one-or-more (any alpha ?' ?-)))' uses in
 * vibe-spell-bench.el.  Each word is converted from UTF-8 to the
 * dictionary's own declared encoding (its .aff file's SET directive,
 * read back via Hunspell_get_dic_encoding) before being checked,
 * since Hunspell_spell expects words already in that encoding and
 * does no conversion of its own; some dictionaries (e.g. pt_BR) are
 * not UTF-8.
 *
 * Usage: hunspell-bench-check DICT-NAME FILE
 * DICT-NAME is looked up as /usr/share/hunspell/DICT-NAME.{aff,dic},
 * matching `vibe-spell-dictionary-path''s default.
 *
 * Build: cc -O2 -o hunspell-bench-check hunspell-bench-check.c \
 *            $(pkg-config --cflags --libs hunspell)
 */

#include <errno.h>
#include <iconv.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <wchar.h>
#include <wctype.h>

#include <hunspell/hunspell.h>

static double
now(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return ts.tv_sec + ts.tv_nsec / 1e9;
}

static int
is_word_char(wchar_t wc)
{
	return iswalpha((wint_t)wc) || wc == L'\'' || wc == L'-';
}

/* Check WORD (WLEN bytes, UTF-8), converting it to the dictionary's
 * own encoding via CD first unless CD is (iconv_t)-1 (dictionary is
 * already UTF-8).  Return non-zero if Hunspell considers it correct. */
static int
check_word(Hunhandle *h, iconv_t cd, char *word, size_t wlen)
{
	word[wlen] = '\0';
	if (cd == (iconv_t)-1)
		return Hunspell_spell(h, word);
	char converted[1024];
	char *in = word, *out = converted;
	size_t inleft = wlen, outleft = sizeof converted - 1;
	iconv(cd, &in, &inleft, &out, &outleft);
	*out = '\0';
	return Hunspell_spell(h, converted);
}

/* Read FILE fully into a NUL-terminated buffer; set *SIZE to its
 * length (not counting the added NUL).  Exits the program on error. */
static char *
read_file(const char *path, long *size)
{
	FILE *f = fopen(path, "rb");
	if (!f) {
		fprintf(stderr, "hunspell-bench-check: %s: %s\n", path, strerror(errno));
		exit(1);
	}
	if (fseek(f, 0, SEEK_END) != 0) {
		fprintf(stderr, "hunspell-bench-check: %s: cannot seek\n", path);
		exit(1);
	}
	long len = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *buf = malloc((size_t)len + 1);
	if (!buf) {
		fprintf(stderr, "hunspell-bench-check: out of memory\n");
		exit(1);
	}
	size_t got = fread(buf, 1, (size_t)len, f);
	fclose(f);
	buf[got] = '\0';
	*size = (long)got;
	return buf;
}

int
main(int argc, char **argv)
{
	if (argc != 3) {
		fprintf(stderr, "usage: %s DICT-NAME FILE\n", argv[0]);
		return 1;
	}
	const char *dict_name = argv[1];
	const char *file_path = argv[2];

	/* Unicode-aware iswalpha() needs a UTF-8 locale. */
	if (!setlocale(LC_CTYPE, "C.UTF-8") && !setlocale(LC_CTYPE, "en_US.UTF-8")
	    && !setlocale(LC_CTYPE, ""))
		fprintf(stderr, "hunspell-bench-check: warning: no UTF-8 locale available\n");

	char aff_path[4096], dic_path[4096];
	snprintf(aff_path, sizeof aff_path, "/usr/share/hunspell/%s.aff", dict_name);
	snprintf(dic_path, sizeof dic_path, "/usr/share/hunspell/%s.dic", dict_name);

	double t0 = now();
	Hunhandle *h = Hunspell_create(aff_path, dic_path);
	double load_seconds = now() - t0;
	if (!h) {
		fprintf(stderr, "hunspell-bench-check: could not load dictionary %s\n", dict_name);
		return 1;
	}

	/* Words are matched out of the UTF-8 input, then converted to
	 * whatever encoding the dictionary itself declares (often not
	 * UTF-8), since Hunspell_spell() does no conversion itself. */
	const char *dic_encoding = Hunspell_get_dic_encoding(h);
	iconv_t cd = (iconv_t)-1;
	if (strcmp(dic_encoding, "UTF-8") != 0) {
		cd = iconv_open(dic_encoding, "UTF-8");
		if (cd == (iconv_t)-1) {
			fprintf(stderr, "hunspell-bench-check: iconv_open UTF-8 -> %s: %s\n",
			        dic_encoding, strerror(errno));
			return 1;
		}
	}

	long file_size;
	char *buf = read_file(file_path, &file_size);

	/* Timing starts here, once the file is already loaded: words are
	 * found and checked on the fly, one at a time, rather than first
	 * collected into a list. */
	t0 = now();
	long word_count = 0;
	long correct_count = 0;
	mbstate_t mbs;
	memset(&mbs, 0, sizeof mbs);
	char *p = buf, *end = buf + file_size;
	char word[1024];
	size_t wlen = 0;
	while (p < end) {
		wchar_t wc;
		size_t n = mbrtowc(&wc, p, (size_t)(end - p), &mbs);
		if (n == (size_t)-1 || n == (size_t)-2) {
			/* invalid or incomplete UTF-8 sequence: skip a byte */
			memset(&mbs, 0, sizeof mbs);
			p++;
			continue;
		}
		if (n == 0)
			n = 1; /* embedded NUL byte */
		if (is_word_char(wc)) {
			if (wlen + n < sizeof word) {
				memcpy(word + wlen, p, n);
				wlen += n;
			}
		} else if (wlen > 0) {
			word_count++;
			if (check_word(h, cd, word, wlen))
				correct_count++;
			wlen = 0;
		}
		p += n;
	}
	if (wlen > 0) {
		word_count++;
		if (check_word(h, cd, word, wlen))
			correct_count++;
	}
	double check_seconds = now() - t0;

	printf("benchmark:  hunspell %s %s\n", dict_name, file_path);
	printf("load dict:  %.3fs\n", load_seconds);
	printf("check:      %ld words, %ld correct\n", word_count, correct_count);
	printf("            %.3fs total\n", check_seconds);
	printf("            %.3fK words/s,\n", word_count / check_seconds / 1000.0);

	if (cd != (iconv_t)-1)
		iconv_close(cd);
	Hunspell_destroy(h);
	free(buf);
	return 0;
}
