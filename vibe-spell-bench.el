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

(defun vibe-spell-bench-run (&optional dict-name file)
  "Benchmark loading dictionary DICT-NAME and checking FILE's words.

DICT-NAME and FILE default to the first two leftover command-line
arguments, for use as `emacs -batch -l vibe-spell-bench.el -f
vibe-spell-bench-run DICT-NAME FILE'.  This function is noninteractive
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
            %.3fK words/s,
"
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

(provide 'vibe-spell-bench)
;;; vibe-spell-bench.el ends here
