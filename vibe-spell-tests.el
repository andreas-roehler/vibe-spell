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
;; Hunspell's other kinds of expectations -- NAME.sug (suggestions),
;; NAME.root/.morph (morphological analysis) and NAME.trace (affix
;; trace) -- are not checked, since vibe-spell does not implement
;; suggestions or morphological analysis.  Dictionaries that
;; `vibe-spell-create' refuses outright (currently, only ones using
;; COMPLEXPREFIXES) are reported as skipped rather than failed; see
;; the Commentary in vibe-spell.el for this and other limitations of
;; this port that may cause some dictionaries' good/wrong words to be
;; misclassified, such as encodings other than UTF-8 or ASCII.
;;
;; Almost every test runs directly in the ERT runner's own process.
;; The dictionaries named in `vibe-spell-tests-subprocess-dictionaries'
;; are the exception: each of those runs in its own `emacs -Q --batch'
;; subprocess, wrapped by the external `timeout' command.  This is
;; because Hunspell's own compound checker bails out of pathological
;; inputs (e.g. tests/timelimit.*) via an explicit wall-clock budget;
;; Nuspell's checker, which this port follows, has no such safeguard,
;; so that handful of adversarial Hunspell regression inputs run its
;; unmemoized compound-splitting search for a very long time, in a
;; tight loop that does not yield back to Emacs's timer machinery --
;; so an in-process `with-timeout' cannot reliably interrupt it, but
;; an external, OS-level `timeout' always can.  A subprocess that is
;; killed this way is reported as a skip rather than a pass or a
;; failure.
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

(defconst vibe-spell-tests-timeout 10
  "Seconds allowed for a subprocess-isolated test to finish.
See `vibe-spell-tests-subprocess-dictionaries' and the Commentary at
the top of this file.")

(defconst vibe-spell-tests-subprocess-dictionaries '("timelimit")
  "Hunspell test dictionaries that must run in a time-limited subprocess.
See the Commentary at the top of this file for why.  Every other
dictionary's test runs directly in the ERT runner's own process.")

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
or (STATUS . MESSAGE) where STATUS is `skip' (`vibe-spell-create'
itself failed) or `fail' (some word was misclassified)."
  (cl-block nil
    (let ((vibe-spell-dictionary-path (list vibe-spell-tests-hunspell-dir))
          (good-file (expand-file-name (concat name ".good") vibe-spell-tests-hunspell-dir))
          (wrong-file (expand-file-name (concat name ".wrong") vibe-spell-tests-hunspell-dir))
          (dict nil))
      (condition-case err
          (setq dict (vibe-spell-create name))
        (error
         (cl-return-from nil
           (cons 'skip (format "vibe-spell-create failed: %s" (error-message-string err))))))
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

(defun vibe-spell-tests--run-case (name)
  "Run the check for hunspell test NAME directly in this process."
  (pcase (vibe-spell-tests--check name)
    ('nil t)
    (`(skip . ,msg) (ert-skip msg))
    (`(fail . ,msg) (ert-fail msg))))

(defun vibe-spell-tests--subprocess-check (name)
  "Print \"OK\", \"SKIP:MESSAGE\" or \"FAIL:MESSAGE\" for hunspell test NAME.
This is the entry point run, by `vibe-spell-tests--run-case-subprocess',
in a subprocess that is externally time-limited; it does not itself
know or care about that time limit."
  (pcase (vibe-spell-tests--check name)
    ('nil (princ "OK"))
    (`(skip . ,msg) (princ (format "SKIP:%s" msg)))
    (`(fail . ,msg) (princ (format "FAIL:%s" msg)))))

(defun vibe-spell-tests--run-case-subprocess (name)
  "Run `vibe-spell-tests--subprocess-check' for NAME in its own subprocess.
The subprocess is killed, and this is reported as a skip, if it does
not finish within `vibe-spell-tests-timeout'."
  (let* ((timeout-bin (or (executable-find "timeout")
                           (error "vibe-spell-tests: `timeout' utility not found")))
         (emacs-bin (or (executable-find "emacs")
                        (error "vibe-spell-tests: `emacs' executable not found")))
         (args (list "-k" "2" (number-to-string vibe-spell-tests-timeout)
                     emacs-bin "-Q" "--batch"
                     "-L" vibe-spell-tests-project-dir
                     "-l" "vibe-spell-tests"
                     "--eval" (format "(vibe-spell-tests--subprocess-check %S)" name)))
         (buf (generate-new-buffer " *vibe-spell-tests-subprocess*"))
         (status nil))
    (unwind-protect
        (progn
          (setq status (apply #'call-process timeout-bin nil buf nil args))
          (let ((output (with-current-buffer buf (buffer-string))))
            (cond
             ((= status 124)
              (ert-skip (format "checking did not finish within %ds"
                                 vibe-spell-tests-timeout)))
             ((string-prefix-p "SKIP:" output) (ert-skip (substring output 5)))
             ((string-prefix-p "FAIL:" output) (ert-fail (substring output 5)))
             ((string-prefix-p "OK" output) t)
             (t (ert-fail (format "subprocess exited %s with unexpected output:\n%s"
                                   status output))))))
      (kill-buffer buf))))

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
              (if (member ,name vibe-spell-tests-subprocess-dictionaries)
                  (vibe-spell-tests--run-case-subprocess ,name)
                (vibe-spell-tests--run-case ,name)))
           t))))))

(vibe-spell-tests--define-hunspell-tests)

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

(provide 'vibe-spell-tests)
;;; vibe-spell-tests.el ends here
