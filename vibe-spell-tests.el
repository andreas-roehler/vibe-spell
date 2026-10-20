;;; vibe-spell-tests.el --- Tests for vibe-spell.el  -*- lexical-binding: t; -*-

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

;; Runs Hunspell's own test dictionaries, found in hunspell/tests/,
;; against `vibe-spell-check'.  For every NAME.aff + NAME.dic pair in
;; that directory that comes with a NAME.good and/or NAME.wrong word
;; list, this defines an ERT test `vibe-spell-test-hunspell-NAME' that
;; loads the dictionary and checks that every word in NAME.good is
;; accepted and every word in NAME.wrong is rejected.  This mirrors
;; the ".good"/".wrong" portion of Hunspell's own tests/test.sh.
;;
;; For every NAME.wrong that also comes with a NAME.sug, this also
;; defines `vibe-spell-test-hunspell-suggest-NAME', which checks that
;; `vibe-spell-suggest' includes at least one of NAME.sug's expected
;; alternatives for each corresponding word of NAME.wrong -- see
;; `vibe-spell-tests--check-suggest' for how the two files are paired
;; up.  This is a
;; looser check than Hunspell's own test.sh, which requires the exact
;; suggestion list hunspell itself produces: `vibe-spell-suggest' is a
;; Nuspell-derived port and is not expected to reproduce Hunspell's own
;; `SuggestMgr' suggestion lists verbatim, only to come up with
;; reasonable corrections, among which the expected one should appear.
;;
;; Hunspell's other kinds of expectations -- NAME.root/.morph
;; (morphological analysis) and NAME.trace (affix trace) -- are not
;; checked, since vibe-spell does not implement morphological
;; analysis.  Dictionaries that `vibe-spell-create' refuses outright
;; (currently, only ones using COMPLEXPREFIXES) fail; see the
;; Commentary in vibe-spell.el for this and other limitations of this
;; port that may cause some dictionaries' good/wrong words, or
;; suggestions, to be misclassified.
;;
;; A handful of the tests this generates are known to fail outright --
;; some because of limitations already documented in vibe-spell.el,
;; others because of genuine Hunspell/Nuspell behavioral divergences,
;; or (more rarely) a gap specific to this port.  Each such test name
;; is listed in `vibe-spell-tests-known-failures' along with why, and
;; is declared with ERT's `:expected-result :failed' (an "xfail") so
;; that it reports as an expected failure rather than an unexpected
;; one; if it ever starts passing, ERT reports that as a failure too
;; (an "unexpected pass"), which is the prompt to remove it from that
;; list.
;;
;; Run with, e.g.:
;;   emacs -batch -L . -l ert -l vibe-spell-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'vibe-spell)

(defconst vibe-spell-tests-project-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing vibe-spell.el and vibe-spell-tests.el.")

(defconst vibe-spell-tests-hunspell-dir
  (expand-file-name "hunspell/tests/" vibe-spell-tests-project-dir)
  "Directory holding Hunspell's own affix/dictionary/word-list tests.")

(unless (file-directory-p vibe-spell-tests-hunspell-dir)
  (error "vibe-spell-tests: no such directory %s" vibe-spell-tests-hunspell-dir))

(defconst vibe-spell-tests-known-failures
  '((vibe-spell-test-hunspell-2970240 .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-2970242 .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-alias3 .
     "COMPLEXPREFIXES is not supported; `vibe-spell-create' refuses the
dictionary, per the Commentary in vibe-spell.el.")
    (vibe-spell-test-hunspell-base_utf .
     "Turkish dotless i (İ/ı) casing; casing uses Emacs's own locale-unaware
upcase/downcase, per the Commentary in vibe-spell.el.")
    (vibe-spell-test-hunspell-break .
     "Tests recursive multi-dash BREAK splitting (\"e-mail-foo\"); Nuspell's
BREAK handling, which this ports, only retries the first occurrence of
each pattern per split attempt, so a word with more than one break
point fails here too.")
    (vibe-spell-test-hunspell-checkcompoundpattern .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-checkcompoundpattern2 .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-checkcompoundpattern3 .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-checkcompoundpattern4 .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-checkcompoundpattern5 .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-checkcompoundpattern6 .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-checkcompoundpattern7 .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-complexprefixes .
     "COMPLEXPREFIXES is not supported; `vibe-spell-create' refuses the
dictionary, per the Commentary in vibe-spell.el.")
    (vibe-spell-test-hunspell-complexprefixes2 .
     "COMPLEXPREFIXES is not supported; `vibe-spell-create' refuses the
dictionary, per the Commentary in vibe-spell.el.")
    (vibe-spell-test-hunspell-complexprefixesutf .
     "COMPLEXPREFIXES is not supported; `vibe-spell-create' refuses the
dictionary, per the Commentary in vibe-spell.el.")
    (vibe-spell-test-hunspell-compoundforbid .
     "COMPOUNDFORBIDFLAG on a direct .dic entry (\"foobaz/Z\") has no effect
on an unrelated derivation of the same string from a different root
+ suffix (\"foo\"+\"baz\"), since that derivation never looks at the
other entry's flags at all; `-check-word-in-compound' mirrors
Nuspell's own `check_word_in_compound' exactly here, so this appears
to be a genuine gap in Nuspell/Hunspell's COMPOUNDFORBIDFLAG
semantics, not something specific to this port.")
    (vibe-spell-test-hunspell-dotless_i .
     "Turkish dotless i (İ/ı) casing (LANG tr); casing uses Emacs's own
locale-unaware upcase/downcase, per the Commentary in vibe-spell.el.")
    (vibe-spell-test-hunspell-gh106 .
     "Tests a Hunspell-specific fix for case-insensitive affix conditions on
PASCAL-cased words; Nuspell's checker, which this ports, never retries
case variants for CAMEL/PASCAL words, so this diverges from Hunspell
upstream too.")
    (vibe-spell-test-hunspell-gh151 .
     "SIMPLIFIEDTRIPLE's extra \"insert the elided letter back and retry\"
fallback is not implemented, per the LLM notes to self in vibe-spell.el.")
    (vibe-spell-test-hunspell-gh353 .
     "Hunspell treats a word made only of WORDCHARS-listed non-ASCII digits
as a number; Nuspell's own `is_number' (which `vibe-spell-check'
mirrors) only recognizes ASCII digits, so this diverges from Hunspell
upstream too.")
    (vibe-spell-test-hunspell-hu .
     "Hungarian COMPOUNDSYLLABLE/SYLLABLENUM (the exception letting a
compound exceed COMPOUNDWORDMAX) and the LANG_hu \"moving rule\" for
COMPOUNDFORBIDFLAG are not implemented, per the LLM notes to self in
vibe-spell.el.")
    (vibe-spell-test-hunspell-limit-multiple-compounding .
     "Tests a Hunspell-specific heuristic rejecting a 3-or-more-part compound
that is a near-typo of an unrelated, longer dictionary word; Nuspell's
checker, which this ports, has no such heuristic.")
    (vibe-spell-test-hunspell-morph .
     "The .good file's multi-word lines (e.g. \"drink eat\") exercise
Hunspell's own CLI, which checks a line word by word; `vibe-spell-check'
only ever checks one word, like `Hunspell_spell' itself, so this test
harness's one-call-per-line reading does not apply here.")
    (vibe-spell-test-hunspell-nepali .
     "Tests ICONV/IGNORE normalization of ZWJ/ZWNJ in Devanagari conjuncts;
this port's substring replacer does not reproduce Hunspell's exact
normalization here.")
    (vibe-spell-test-hunspell-onlyincompound2 .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-opentaal_cpdpat .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-opentaal_cpdpat2 .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-ph2 .
     "A dictionary entry with an embedded space (e.g. \"forbidden root\") is
meant to block the squished-together compound from also being
accepted; this multi-word .dic entry mechanism is not implemented
\(same gap as the wordpair test).")
    (vibe-spell-test-hunspell-simplifiedtriple .
     "SIMPLIFIEDTRIPLE's extra \"insert the elided letter back and retry\"
fallback is not implemented, per the LLM notes to self in vibe-spell.el.")
    (vibe-spell-test-hunspell-timelimit .
     "Hunspell gives up on a compound search after 50 ms (its
TIMELIMIT_MS) and rejects the word; when `vibe-spell-check-time-limit'
runs out, this port accepts it instead.")
    (vibe-spell-test-hunspell-trace_compound .
     "CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not enforced.")
    (vibe-spell-test-hunspell-wordpair .
     "A dictionary entry with an embedded space (e.g. \"compound word\") is
meant to block the squished-together compound (\"compoundword\") from
also being accepted; `-parse-dic-line' stops at the first whitespace,
so this multi-word .dic entry mechanism is not implemented.")
    (vibe-spell-test-hunspell-suggest-base_utf .
     "Turkish dotless i (İ) casing; casing uses Emacs's own locale-unaware
upcase/downcase, per the Commentary in vibe-spell.el.")
    (vibe-spell-test-hunspell-suggest-checksharps .
     "CHECKSHARPS (German sharp s) suggestion casing variants are narrower
than Hunspell's own; Nuspell's test suite marks this one as failing
too.")
    (vibe-spell-test-hunspell-suggest-checksharpsutf .
     "CHECKSHARPS (German sharp s) suggestion casing variants are narrower
than Hunspell's own."); Hunspell drops such a suggestion, Nuspell does not.")
    (vibe-spell-test-hunspell-suggest-nosuggest .
     "NOSUGGEST only filters n-gram suggestions, as in Nuspell, whose
`add_sug_if_correct' ignores it; Nuspell's test suite marks this one
as failing too.")
    (vibe-spell-test-hunspell-suggest-opentaal_keepcase .
     "KEEPCASE casing restoration for a multi-dash PASCAL-cased compound
\(\"TV-word-TV\") only fixes one dash-part at a time, not both together.")
    (vibe-spell-test-hunspell-suggest-ph .
     "Hunspell turns ph: fields of .dic entries (known misspellings of a
word) into suggestions; like Nuspell, this port ignores morphological
fields, and PHONE is not implemented either.")
    (vibe-spell-test-hunspell-suggest-ph2 .
     "Hunspell turns ph: fields of .dic entries (known misspellings of a
word) into suggestions; like Nuspell, this port ignores morphological
fields."))
  "Alist of ERT test name (a symbol) to why it is a known failure.
Tests named here are declared with `:expected-result :failed'; see
`vibe-spell-tests--maybe-xfail'.  A test listed here that starts
passing is itself reported as a failure (an \"unexpected pass\"),
which is ERT's way of prompting this list to be updated.")

(defun vibe-spell-tests--maybe-xfail (test-name)
  "Return the `:expected-result' spec for TEST-NAME (a symbol), or nil.
Looks TEST-NAME up in `vibe-spell-tests-known-failures'; spliced into
a generated `ert-deftest' form when non-nil."
  (when (assq test-name vibe-spell-tests-known-failures)
    '(:expected-result :failed)))

(defun vibe-spell-tests--read-words (file)
  "Return the list of non-blank, whitespace-trimmed lines of FILE.
Return nil if FILE does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t "[ \t\r]+"))))

(defun vibe-spell-tests--check (name)
  "Check hunspell test NAME's .good/.wrong word lists against vibe-spell.
NAME is looked up as NAME.aff/NAME.dic/NAME.good/NAME.wrong under
`vibe-spell-tests-hunspell-dir'.  Return nil if everything checks out,
or (fail . MESSAGE) if `vibe-spell-create' itself failed or some
word was misclassified."
  (cl-block nil
    (let ((vibe-spell-dictionary-path (list vibe-spell-tests-hunspell-dir))
          (good-file (expand-file-name (concat name ".good") vibe-spell-tests-hunspell-dir))
          (wrong-file (expand-file-name (concat name ".wrong") vibe-spell-tests-hunspell-dir))
          (dict nil))
      (condition-case err
          (setq dict (vibe-spell-create name))
        (error
         (cl-return-from nil
           (cons 'fail (format "vibe-spell-create failed: %s" (error-message-string err))))))
      (let ((bad-good (seq-remove (lambda (w) (vibe-spell-check dict w))
                                   (vibe-spell-tests--read-words good-file)))
            (bad-wrong (seq-filter (lambda (w) (vibe-spell-check dict w))
                                    (vibe-spell-tests--read-words wrong-file))))
        (if (or bad-good bad-wrong)
            (cons 'fail
                  (concat
                   (and bad-good
                        (format "Good words rejected as incorrect: %S" bad-good))
                   (and bad-good bad-wrong "\n")
                   (and bad-wrong
                        (format "Wrong words accepted as correct: %S" bad-wrong))))
          nil)))))

(defun vibe-spell-tests--run-case (checker name)
  "Run CHECKER for hunspell test NAME.
CHECKER is `vibe-spell-tests--check' or
`vibe-spell-tests--check-suggest'."
  (pcase (funcall checker name)
    ('nil t)
    (`(fail . ,msg) (ert-fail msg))))

(defun vibe-spell-tests--define-hunspell-tests ()
  "Define one `ert-deftest' per Hunspell test dictionary with word lists."
  (dolist (aff-file (directory-files vibe-spell-tests-hunspell-dir t "\\.aff\\'"))
    (let* ((name (file-name-sans-extension (file-name-nondirectory aff-file)))
           (dic-file (expand-file-name (concat name ".dic") vibe-spell-tests-hunspell-dir))
           (good-file (expand-file-name (concat name ".good") vibe-spell-tests-hunspell-dir))
           (wrong-file (expand-file-name (concat name ".wrong") vibe-spell-tests-hunspell-dir)))
      (when (and (file-exists-p dic-file)
                 (or (file-exists-p good-file) (file-exists-p wrong-file)))
        (let ((test-name (intern (format "vibe-spell-test-hunspell-%s" name))))
          (eval
           `(ert-deftest ,test-name ()
              ,(format "Check hunspell/tests/%s.{good,wrong} with vibe-spell." name)
              ,@(vibe-spell-tests--maybe-xfail test-name)
              (vibe-spell-tests--run-case #'vibe-spell-tests--check ,name))
           t))))))

(vibe-spell-tests--define-hunspell-tests)

;;;; Suggestions

(defun vibe-spell-tests--read-lines (file)
  "Return FILE's lines as a list of strings, or nil if FILE does not exist.
Unlike `vibe-spell-tests--read-words', this keeps each line's
whitespace intact and preserves blank lines, except for trailing
ones, which Hunspell's own test script ignores too."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (mapcar (lambda (l) (string-trim-right l "[ \t\r]+"))
                            (split-string (buffer-string) "\n"))))
        (nreverse (drop-while #'string-empty-p (nreverse lines)))))))

(defun vibe-spell-tests--check-suggest (name)
  "Check hunspell test NAME's .wrong/.sug word lists against vibe-spell.
NAME is looked up as NAME.aff/NAME.dic/NAME.wrong/NAME.sug under
`vibe-spell-tests-hunspell-dir'.  Like Hunspell's own test script,
which keeps only the \"&\" lines of `hunspell -a', this pairs the
.sug lines, in order, with the .wrong words that are misspelled and
get at least one suggestion; a different number of such words is a
failure.  A .sug line may list several Hunspell-acceptable
alternatives, comma-separated; this only requires `vibe-spell-suggest'
to include at least one of them, not to match the whole list or its
order -- `vibe-spell-suggest' is a Nuspell-derived port and is not
expected to reproduce Hunspell's own suggestions verbatim.  Return nil
if everything checks out, or (fail . MESSAGE) if `vibe-spell-create'
itself failed or the suggestions did not match."
  (cl-block nil
    (let ((vibe-spell-dictionary-path (list vibe-spell-tests-hunspell-dir))
          (wrong-file (expand-file-name (concat name ".wrong") vibe-spell-tests-hunspell-dir))
          (sug-file (expand-file-name (concat name ".sug") vibe-spell-tests-hunspell-dir))
          (dict nil))
      (condition-case err
          (setq dict (vibe-spell-create name))
        (error
         (cl-return-from nil
           (cons 'fail (format "vibe-spell-create failed: %s" (error-message-string err))))))
      (let ((sug-lines (vibe-spell-tests--read-lines sug-file))
            (suggested nil))
        (dolist (word (vibe-spell-tests--read-lines wrong-file))
          (unless (vibe-spell-check dict word)
            (let ((got (vibe-spell-suggest dict word)))
              (when got
                (push (cons word got) suggested)))))
        (setq suggested (nreverse suggested))
        (unless (= (length suggested) (length sug-lines))
          (cl-return-from nil
            (cons 'fail (format "%d words got suggestions, but %s.sug has %d lines: %S"
                                 (length suggested) name (length sug-lines) suggested))))
        (let (bad)
          (cl-mapc
           (lambda (entry expected)
             (let ((alternatives (split-string expected ", " t)))
               (unless (cl-intersection alternatives (cdr entry) :test #'string=)
                 (push (format "%S: expected one of %S, got %S"
                               (car entry) alternatives (cdr entry))
                       bad))))
           suggested sug-lines)
          (if bad
              (cons 'fail (string-join (nreverse bad) "\n"))
            nil))))))

(defun vibe-spell-tests--define-hunspell-suggest-tests ()
  "Define one `ert-deftest' per Hunspell test dictionary with a .sug file."
  (dolist (aff-file (directory-files vibe-spell-tests-hunspell-dir t "\\.aff\\'"))
    (let* ((name (file-name-sans-extension (file-name-nondirectory aff-file)))
           (dic-file (expand-file-name (concat name ".dic") vibe-spell-tests-hunspell-dir))
           (wrong-file (expand-file-name (concat name ".wrong") vibe-spell-tests-hunspell-dir))
           (sug-file (expand-file-name (concat name ".sug") vibe-spell-tests-hunspell-dir)))
      (when (and (file-exists-p dic-file) (file-exists-p wrong-file) (file-exists-p sug-file))
        (let ((test-name (intern (format "vibe-spell-test-hunspell-suggest-%s" name))))
          (eval
           `(ert-deftest ,test-name ()
              ,(format "Check hunspell/tests/%s.{wrong,sug} with vibe-spell-suggest." name)
              ,@(vibe-spell-tests--maybe-xfail test-name)
              (vibe-spell-tests--run-case #'vibe-spell-tests--check-suggest ,name))
           t))))))

(vibe-spell-tests--define-hunspell-suggest-tests)

;;;; Personal dictionary

(ert-deftest vibe-spell-test-personal-dictionary ()
  "Check `vibe-spell-add-word' and its interaction with `vibe-spell-create'.
A word added without saving is usable right away but forgotten by a
later `vibe-spell-create'; one added with SAVE is both usable right
away and, because it ends up in the personal dictionary file, still
known to a later `vibe-spell-create'."
  (let* ((tmpdir (make-temp-file "vibe-spell-test-pdict-" t))
         (vibe-spell-dictionary-path (list vibe-spell-tests-hunspell-dir))
         (vibe-spell-personal-dictionary-format (expand-file-name "personal-%s" tmpdir))
         (pdict-file (format vibe-spell-personal-dictionary-format "base")))
    (unwind-protect
        (progn
          (let ((dict (vibe-spell-create "base")))
            (should-not (vibe-spell-check dict "loooked"))
            (should-not (vibe-spell-check dict "zyzzyva"))
            (vibe-spell-add-word dict "loooked")
            (should (vibe-spell-check dict "loooked"))
            (should-not (file-exists-p pdict-file))
            (vibe-spell-add-word dict "zyzzyva" t)
            (should (vibe-spell-check dict "zyzzyva"))
            (should (file-exists-p pdict-file)))
          (let ((dict (vibe-spell-create "base")))
            (should-not (vibe-spell-check dict "loooked"))
            (should (vibe-spell-check dict "zyzzyva"))
            (should (vibe-spell-check dict "created"))
            (should-not (vibe-spell-check dict "texxt"))))
      (delete-directory tmpdir t))))

(ert-deftest vibe-spell-test-remove-word ()
  "Check `vibe-spell-remove-word' as the inverse of `vibe-spell-add-word'.
Removing a word that was never added reports nil; removing one added
without SAVE forgets it right away; removing one added with SAVE both
forgets it right away and, because it is scrubbed from the personal
dictionary file, keeps it forgotten by a later `vibe-spell-create'.
Removing a word a second time reports nil.  A real dictionary word is
left alone, since it has no personal (nil-flags) entry to remove."
  (let* ((tmpdir (make-temp-file "vibe-spell-test-pdict-" t))
         (vibe-spell-dictionary-path (list vibe-spell-tests-hunspell-dir))
         (vibe-spell-personal-dictionary-format (expand-file-name "personal-%s" tmpdir))
         (pdict-file (format vibe-spell-personal-dictionary-format "base")))
    (unwind-protect
        (let ((dict (vibe-spell-create "base")))
          (should-not (vibe-spell-remove-word dict "zyzzyva"))

          (vibe-spell-add-word dict "loooked")
          (should (vibe-spell-remove-word dict "loooked"))
          (should-not (vibe-spell-check dict "loooked"))

          (vibe-spell-add-word dict "zyzzyva" t)
          (should (vibe-spell-check dict "zyzzyva"))
          (should (vibe-spell-remove-word dict "zyzzyva" t))
          (should-not (vibe-spell-check dict "zyzzyva"))
          (should-not (vibe-spell-remove-word dict "zyzzyva" t))

          (should (vibe-spell-check dict "created"))
          (should-not (vibe-spell-remove-word dict "created"))
          (should (vibe-spell-check dict "created"))

          (should-not (vibe-spell-check dict "zyzzyva"))
          (should (with-temp-buffer
                    (insert-file-contents pdict-file)
                    (not (string-search "zyzzyva" (buffer-string))))))
      (delete-directory tmpdir t))))

;;;; Word regexp

(defun vibe-spell-tests--words (dict text)
  "Return the list of words of TEXT, according to `vibe-spell-word-regexp'."
  (let ((re (vibe-spell-word-regexp dict))
        (start 0) words)
    (while (string-match re text start)
      (push (match-string 0 text) words)
      (setq start (match-end 0)))
    (nreverse words)))

(ert-deftest vibe-spell-test-word-regexp ()
  "Check `vibe-spell-word-regexp' against a few WORDCHARS settings."
  (let ((vibe-spell-dictionary-path (list vibe-spell-tests-hunspell-dir)))
    ;; WORDCHARS .'’
    (let ((dict (vibe-spell-create "base_utf")))
      (should (eq (vibe-spell-word-regexp dict) (vibe-spell-word-regexp dict)))
      (should (equal (vibe-spell-tests--words
                      dict "'Quoted' don’t ‘it’ e.g. a Ünïcode-x2 _a_")
                     '("Quoted" "don’t" "it" "e.g." "Ünïcode"))))
    ;; WORDCHARS -–
    (let ((dict (vibe-spell-create "break")))
      (should (equal (vibe-spell-tests--words dict "-foo-bar– don't --")
                     '("foo-bar" "don"))))))

(provide 'vibe-spell-tests)
;;; vibe-spell-tests.el ends here
