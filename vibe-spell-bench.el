;;; vibe-spell-bench.el --- Benchmarks for vibe-spell.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Measures how long `vibe-spell-create' takes to load a dictionary,
;; and how long `vibe-spell-check' then takes to check every word of
;; a word list -- the dictionary name and the word-list file are
;; given as command-line arguments (see `vibe-spell-bench-check'); the
;; word list is normally
;; hunspell/tests/suggestiontest/List_of_common_misspellings.txt, a
;; Wikipedia-sourced list of English misspelling/correction pairs
;; (one pair, tab-separated, per non-comment line; both the
;; misspelling and the correction are checked), but can equally be
;; running prose, such as a plain-text ebook.  The file is read into a
;; temporary buffer once, up front; checking time is then measured
;; over a single forward scan of that buffer that finds each word and
;; checks it immediately, rather than first collecting the words into
;; a list and only then checking them.
;;
;; It also compares the checking time against the external hunspell(1)
;; binary checking the same file (hunspell does its own tokenizing of
;; the file's text, so this does not necessarily check exactly the
;; same words vibe-spell does, but it is checking the same input),
;; and, separately, how long that binary takes to merely start up and
;; load the dictionary, by running it on /dev/null.
;;
;; The two checking figures are not perfectly apples-to-apples:
;; hunspell's is for one process invocation, so it necessarily
;; includes process startup and loading the dictionary (which is why
;; that cost is also reported on its own); vibe-spell's covers only
;; the checking itself, with dictionary loading timed and reported
;; separately, since that mirrors how a long-running Emacs session
;; actually uses it -- loading a dictionary once, then checking many
;; words against it over time.
;;
;; `vibe-spell-bench-suggest' similarly measures how long
;; `vibe-spell-suggest' takes, over the same kind of misspelling list,
;; but called once per misspelling rather than once per word of
;; running prose, since a flat list of real misspellings is a much
;; more representative input for suggestions than words picked out of
;; ordinary text (almost all of which are already correctly spelled).
;; It also reports, as a rough quality indicator rather than a
;; pass/fail test, how often the word list's expected correction
;; turns up anywhere in the suggestion list.
;;
;; This is a batch-only, noninteractive script.  Run with, e.g.:
;;   emacs -batch -L . -l vibe-spell-bench.el -f vibe-spell-bench-check \
;;         en_US hunspell/tests/suggestiontest/List_of_common_misspellings.txt
;;   emacs -batch -L . -l vibe-spell-bench.el -f vibe-spell-bench-suggest \
;;         en_US hunspell/tests/suggestiontest/List_of_common_misspellings.txt
;;
;; Unless `native-comp-eln-load-path' is nil (e.g. because the caller
;; cleared it to force a pure byte-compiled measurement), this makes
;; sure vibe-spell.el is actually running native-compiled code first,
;; synchronously native-compiling (and reloading) it if it is not --
;; useful since plain `emacs -batch -l vibe-spell-bench.el' only ever
;; runs byte-compiled code unless a matching .eln happens to already
;; sit in the native-comp cache.  Either way, which one it ran with is
;; reported in the first line of the output.

;;; Code:

(require 'benchmark)
(require 'vibe-spell)

;; Set it to the default interactive value (batch mode raises it to
;; 1.0).
(setq gc-cons-percentage 0.1)

(defconst vibe-spell-bench--project-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing vibe-spell.el and vibe-spell-bench.el.")

(defun vibe-spell-bench--ensure-native ()
  "Make sure vibe-spell.el runs native-compiled, unless the caller
disabled native compilation by setting `native-comp-eln-load-path' to
nil, in which case this does nothing."
  (when (and native-comp-eln-load-path
             (not (native-comp-function-p (symbol-function 'vibe-spell-check))))
    (if (native-comp-available-p)
        (load (native-compile (expand-file-name "vibe-spell.el" vibe-spell-bench--project-dir)))
      (error "vibe-spell-bench: native compilation is not available"))))

(defun vibe-spell-bench-check (&optional dict-name file)
  "Benchmark loading dictionary DICT-NAME and checking FILE's words.

DICT-NAME and FILE default to the first two leftover command-line
arguments, for use as `emacs -batch -l vibe-spell-bench.el -f
vibe-spell-bench-check DICT-NAME FILE'.  This function is noninteractive
-- it is meant to be run this way, not via \\[execute-extended-command].

Unless `native-comp-eln-load-path' is nil, this ensures vibe-spell.el
runs native-compiled before benchmarking it; see the Commentary above.

Return and (via `message') report (WORD-COUNT CORRECT-COUNT
LOAD-SECONDS LOAD-GCS LOAD-GC-SECONDS CHECK-SECONDS CHECK-GCS
CHECK-GC-SECONDS); the *-SECONDS/-GCS/-GC-SECONDS triples are as
produced by `benchmark-run'."
  (let ((dict-name (or dict-name (pop command-line-args-left)
                        (error "vibe-spell-bench: no dictionary name given")))
        (file (or file (pop command-line-args-left)
                   (error "vibe-spell-bench: no word-list file given")))
        (dict nil)
        (word-count 0)
        (correct-count 0))
    (vibe-spell-bench--ensure-native)
    (pcase-let*
        ((`(,load-seconds ,load-gcs ,load-gc-seconds)
          (benchmark-run (setq dict (vibe-spell-create dict-name))))
         (`(,check-seconds ,check-gcs ,check-gc-seconds)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (benchmark-run
             (while (re-search-forward (rx (one-or-more (any alpha ?' ?-))) nil t)
               (setq word-count (1+ word-count))
               (when (vibe-spell-check dict (match-string-no-properties 0))
                 (setq correct-count (1+ correct-count))))))))
      (message "\
benchmark:  %s %s %s
load dict:  %.3fs, %d GCs taking %.3fs
check:      %d words, %d correct
            %.3fs total, %d GCs taking %.3fs
            %.3fK words/s"
               (if (native-comp-function-p (symbol-function 'vibe-spell-check))
                   "native"
                 "bytecomp")
               dict-name file
               load-seconds load-gcs load-gc-seconds
               word-count correct-count
               check-seconds check-gcs check-gc-seconds
               (/ (/ word-count check-seconds) 1000.0))
      (list word-count correct-count load-seconds load-gcs load-gc-seconds
            check-seconds check-gcs check-gc-seconds))))

(defun vibe-spell-bench--read-misspelling-pairs (file)
  "Return FILE's misspelling/correction pairs as a list of (WRONG . RIGHT).
FILE is tab-separated, one pair per line, like
hunspell/tests/suggestiontest/List_of_common_misspellings.txt; blank
lines, comment lines starting with \"#\", and any other line that does
not split into exactly WRONG and RIGHT (such as that file's own
trailing Wikipedia category marker) are skipped."
  (with-temp-buffer
    (insert-file-contents file)
    (let (pairs)
      (dolist (line (split-string (buffer-string) "\n" t))
        (unless (or (string-empty-p line) (eq (aref line 0) ?#))
          (let ((fields (split-string line "\t")))
            (when (length= fields 2)
              (push (cons (nth 0 fields) (nth 1 fields)) pairs)))))
      (nreverse pairs))))

(defun vibe-spell-bench-suggest (&optional dict-name file)
  "Benchmark loading dictionary DICT-NAME and suggesting corrections for FILE.

DICT-NAME and FILE default to the first two leftover command-line
arguments, for use as `emacs -batch -l vibe-spell-bench.el -f
vibe-spell-bench-suggest DICT-NAME FILE'.  This function is
noninteractive -- it is meant to be run this way, not via
\\[execute-extended-command].

FILE is a tab-separated misspelling/correction word list, normally
hunspell/tests/suggestiontest/List_of_common_misspellings.txt; see
`vibe-spell-bench--read-misspelling-pairs'.  It is read into memory
once, up front; `vibe-spell-suggest' is then timed calling it once per
misspelling, in file order.  Unlike `vibe-spell-bench-check', which only
measures speed, this also reports how often the expected correction
turns up anywhere in the suggestion list, as a rough quality check --
not the strict pass/fail of a test, just a benchmark-friendly number to
watch for regressions.

Unless `native-comp-eln-load-path' is nil, this ensures vibe-spell.el
runs native-compiled before benchmarking it; see the Commentary above.

Return and (via `message') report (PAIR-COUNT FOUND-COUNT LOAD-SECONDS
LOAD-GCS LOAD-GC-SECONDS SUGGEST-SECONDS SUGGEST-GCS
SUGGEST-GC-SECONDS); the *-SECONDS/-GCS/-GC-SECONDS triples are as
produced by `benchmark-run'."
  (let ((dict-name (or dict-name (pop command-line-args-left)
                        (error "vibe-spell-bench: no dictionary name given")))
        (file (or file (pop command-line-args-left)
                   (error "vibe-spell-bench: no word-list file given")))
        (dict nil)
        (found-count 0))
    (vibe-spell-bench--ensure-native)
    (pcase-let*
        ((`(,load-seconds ,load-gcs ,load-gc-seconds)
          (benchmark-run (setq dict (vibe-spell-create dict-name))))
         (pairs (vibe-spell-bench--read-misspelling-pairs file))
         (`(,suggest-seconds ,suggest-gcs ,suggest-gc-seconds)
          (benchmark-run
           (dolist (pair pairs)
             (when (member (cdr pair) (vibe-spell-suggest dict (car pair)))
               (setq found-count (1+ found-count)))))))
      (message "\
benchmark:  %s %s %s
load dict:  %.3fs, %d GCs taking %.3fs
suggest:    %d words, %d with the correction suggested (%.1f%%)
            %.3fs total, %d GCs taking %.3fs
            %.3f words/s"
               (if (native-comp-function-p (symbol-function 'vibe-spell-suggest))
                   "native"
                 "bytecomp")
               dict-name file
               load-seconds load-gcs load-gc-seconds
               (length pairs) found-count
               (/ (* 100.0 found-count) (max 1 (length pairs)))
               suggest-seconds suggest-gcs suggest-gc-seconds
               (/ (length pairs) suggest-seconds))
      (list (length pairs) found-count load-seconds load-gcs load-gc-seconds
            suggest-seconds suggest-gcs suggest-gc-seconds))))

(provide 'vibe-spell-bench)
;;; vibe-spell-bench.el ends here
