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
;; given as command-line arguments (see `vibe-spell-bench-run'); the
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
;; This is a batch-only, noninteractive script.  Run with, e.g.:
;;   emacs -batch -L . -l vibe-spell-bench.el -f vibe-spell-bench-run \
;;         en_US hunspell/tests/suggestiontest/List_of_common_misspellings.txt

;;; Code:

(require 'benchmark)
(require 'vibe-spell)

(defun vibe-spell-bench-run (&optional dict-name file)
  "Benchmark loading dictionary DICT-NAME and checking FILE's words,
alongside the external hunspell(1) binary: checking the same file,
and separately, on /dev/null, merely starting up and loading the
dictionary.  All three use DICT-NAME.

DICT-NAME and FILE default to the first two leftover command-line
arguments, for use as `emacs -batch -l vibe-spell-bench.el -f
vibe-spell-bench-run DICT-NAME FILE'.  This function is noninteractive
-- it is meant to be run this way, not via \\[execute-extended-command].

Return and (via `message') report (WORD-COUNT LOAD-SECONDS LOAD-GCS
LOAD-GC-SECONDS CHECK-SECONDS CHECK-GCS CHECK-GC-SECONDS
HUNSPELL-SECONDS HUNSPELL-STARTUP-SECONDS); the *-SECONDS/-GCS/-GC-
SECONDS triples are as produced by `benchmark-run'."
  (let ((dict-name (or dict-name (pop command-line-args-left)
                        (error "vibe-spell-bench-run: no dictionary name given")))
        (file (or file (pop command-line-args-left)
                   (error "vibe-spell-bench-run: no word-list file given")))
        (dict nil)
        (word-count 0))
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
               (vibe-spell-check dict (match-string-no-properties 0)))))))
      (message "\
vibe-spell-bench:  %d words from %s
load dict:         %.3fs (%d GCs taking %.3fs)
check:             %.3fs (%.0f words/s), %d GCs taking %.3fs
"
               word-count file
               load-seconds load-gcs load-gc-seconds
               check-seconds (/ word-count check-seconds)
               check-gcs check-gc-seconds))))

(provide 'vibe-spell-bench)
;;; vibe-spell-bench.el ends here
