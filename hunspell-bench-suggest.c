/* hunspell-bench-suggest.c --- apples-to-apples C benchmark for
 * vibe-spell-bench-suggest
 *
 * Measures how long Hunspell_create() takes to load a dictionary, and
 * how long Hunspell_suggest() then takes to suggest corrections for
 * each misspelling of a tab-separated misspelling/correction word
 * list -- normally
 * hunspell/tests/suggestiontest/List_of_common_misspellings.txt --
 * the same way vibe-spell-bench-suggest measures
 * vibe-spell-create and vibe-spell-suggest.  The file is read into
 * memory and split into pairs once, up front, before any suggesting
 * is timed.  As a rough quality indicator rather than a pass/fail
 * test, this also reports how often the expected correction turns up
 * anywhere in Hunspell_suggest()'s own suggestion list.
 *
 * Both the misspelling (before suggesting) and each returned
 * suggestion (before comparing it against the expected correction)
 * are converted between UTF-8 and the dictionary's own declared
 * encoding (its .aff file's SET directive, read back via
 * Hunspell_get_dic_encoding), since Hunspell_suggest expects and
 * returns words in that encoding and does no conversion of its own;
 * some dictionaries (e.g. pt_BR) are not UTF-8.  The word list itself
 * is assumed UTF-8, matching vibe-spell-bench-suggest.
 *
 * Usage: hunspell-bench-suggest DICT-NAME FILE
 * DICT-NAME is looked up as /usr/share/hunspell/DICT-NAME.{aff,dic},
 * matching `vibe-spell-dictionary-path''s default.
 *
 * Build: make .tmp/hunspell-bench-suggest, which links against a static Hunspell built
 * from the hunspell/ source checkout.
 */

#include <errno.h>
#include <iconv.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <hunspell/hunspell.h>

static double
now(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return ts.tv_sec + ts.tv_nsec / 1e9;
}

/* Convert the NUL-terminated string IN with CD (an open iconv_t, or
 * (iconv_t)-1 for "no conversion needed") into OUT, a buffer of
 * OUTSIZE bytes.  Always NUL-terminates OUT, truncating on overflow. */
static void
convert(iconv_t cd, const char *in, char *out, size_t outsize)
{
	if (cd == (iconv_t)-1) {
		snprintf(out, outsize, "%s", in);
		return;
	}
	char *inp = (char *)in, *outp = out;
	size_t inleft = strlen(in), outleft = outsize - 1;
	iconv(cd, &inp, &inleft, &outp, &outleft);
	*outp = '\0';
}

/* One misspelling/correction pair, both still UTF-8. */
struct pair {
	char *wrong;
	char *right;
};

/* Split BUF (modified in place: NULs are written in to end each
 * field) into misspelling/correction pairs, one per "WRONG\tRIGHT"
 * line; blank lines, comment lines starting with "#", and any other
 * line that is not exactly two tab-separated fields (such as this
 * file's own trailing Wikipedia category marker) are skipped.  Set
 * *COUNT to the number of pairs found; return a malloc'd array of
 * them. */
static struct pair *
read_pairs(char *buf, long *count)
{
	size_t cap = 1024;
	struct pair *pairs = malloc(cap * sizeof *pairs);
	long n = 0;
	char *line = buf, *saveptr = NULL;
	for (line = strtok_r(buf, "\n", &saveptr); line;
	     line = strtok_r(NULL, "\n", &saveptr)) {
		if (line[0] == '\0' || line[0] == '#')
			continue;
		char *tab = strchr(line, '\t');
		if (!tab || strchr(tab + 1, '\t'))
			continue;
		*tab = '\0';
		if ((size_t)n == cap) {
			cap *= 2;
			pairs = realloc(pairs, cap * sizeof *pairs);
		}
		pairs[n].wrong = line;
		pairs[n].right = tab + 1;
		n++;
	}
	*count = n;
	return pairs;
}

/* Read FILE fully into a NUL-terminated, malloc'd buffer.  Exits the
 * program on error. */
static char *
read_file(const char *path)
{
	FILE *f = fopen(path, "rb");
	if (!f) {
		fprintf(stderr, "hunspell-bench-suggest: %s: %s\n", path, strerror(errno));
		exit(1);
	}
	if (fseek(f, 0, SEEK_END) != 0) {
		fprintf(stderr, "hunspell-bench-suggest: %s: cannot seek\n", path);
		exit(1);
	}
	long len = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *buf = malloc((size_t)len + 1);
	if (!buf) {
		fprintf(stderr, "hunspell-bench-suggest: out of memory\n");
		exit(1);
	}
	size_t got = fread(buf, 1, (size_t)len, f);
	fclose(f);
	buf[got] = '\0';
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

	setlocale(LC_CTYPE, "");

	char aff_path[4096], dic_path[4096];
	snprintf(aff_path, sizeof aff_path, "/usr/share/hunspell/%s.aff", dict_name);
	snprintf(dic_path, sizeof dic_path, "/usr/share/hunspell/%s.dic", dict_name);

	double t0 = now();
	Hunhandle *h = Hunspell_create(aff_path, dic_path);
	double load_seconds = now() - t0;
	if (!h) {
		fprintf(stderr, "hunspell-bench-suggest: could not load dictionary %s\n",
		        dict_name);
		return 1;
	}

	/* Hunspell_suggest() takes and returns words in the dictionary's
	 * own declared encoding, often not UTF-8, so the misspelling list
	 * (assumed UTF-8, like vibe-spell-bench-suggest's) is
	 * converted on the way in, and each suggestion is converted back
	 * on the way out, before comparing it against the expected
	 * correction. */
	const char *dic_encoding = Hunspell_get_dic_encoding(h);
	iconv_t to_dict = (iconv_t)-1, from_dict = (iconv_t)-1;
	if (strcmp(dic_encoding, "UTF-8") != 0) {
		to_dict = iconv_open(dic_encoding, "UTF-8");
		from_dict = iconv_open("UTF-8", dic_encoding);
		if (to_dict == (iconv_t)-1 || from_dict == (iconv_t)-1) {
			fprintf(stderr, "hunspell-bench-suggest: iconv_open UTF-8 <-> %s: %s\n",
			        dic_encoding, strerror(errno));
			return 1;
		}
	}

	char *buf = read_file(file_path);
	long pair_count;
	struct pair *pairs = read_pairs(buf, &pair_count);

	/* Timing starts here, once the file is already loaded and split
	 * into pairs: one `Hunspell_suggest' call per misspelling, in
	 * file order. */
	t0 = now();
	long found_count = 0;
	char converted_wrong[1024], converted_sug[1024];
	for (long i = 0; i < pair_count; i++) {
		convert(to_dict, pairs[i].wrong, converted_wrong, sizeof converted_wrong);
		char **slst = NULL;
		int n = Hunspell_suggest(h, &slst, converted_wrong);
		for (int j = 0; j < n; j++) {
			convert(from_dict, slst[j], converted_sug, sizeof converted_sug);
			if (strcmp(converted_sug, pairs[i].right) == 0) {
				found_count++;
				break;
			}
		}
		if (n > 0)
			Hunspell_free_list(h, &slst, n);
	}
	double suggest_seconds = now() - t0;

	printf("benchmark:  hunspell %s %s\n", dict_name, file_path);
	printf("load dict:  %.3fs\n", load_seconds);
	printf("suggest:    %ld words, %ld with the correction suggested (%.1f%%)\n",
	       pair_count, found_count, 100.0 * found_count / (pair_count ? pair_count : 1));
	printf("            %.3fs total\n", suggest_seconds);
	printf("            %.3f words/s,\n", pair_count / suggest_seconds);

	if (to_dict != (iconv_t)-1)
		iconv_close(to_dict);
	if (from_dict != (iconv_t)-1)
		iconv_close(from_dict);
	Hunspell_destroy(h);
	free(pairs);
	free(buf);
	return 0;
}
