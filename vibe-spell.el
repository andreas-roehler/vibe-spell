;;; vibe-spell.el --- Hunspell-compatible spell checking  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; URL: https://github.com/astoff/vibe-spell
;; Keywords: text
;; Package-Requires: ((emacs "28.1") (compat "31.1"))
;; Version: 0

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

;; An Emacs Lisp port of the word-checking algorithm of Nuspell
;; (https://github.com/nuspell/nuspell), a C++ reimplementation of
;; Hunspell.  Nuspell was chosen over Hunspell as the porting base
;; because its checking code is organized as small, separately named
;; functions with little incidental state, which maps much more
;; directly onto Lisp than Hunspell's.
;;
;; Both checking (`vibe-spell-check') and suggestions
;; (`vibe-spell-suggest') are implemented, the latter following
;; Nuspell's `Suggester' (with occasional reference to Hunspell's own
;; `SuggestMgr' for comparison).  The following simplifications were
;; made relative to upstream Nuspell:
;;
;; - Dictionaries that set COMPLEXPREFIXES (double prefix stripping,
;;   used by a handful of languages) are rejected at `vibe-spell-create'
;;   time.
;; - CHECKCOMPOUNDREP, which consults the REP table, is accepted but
;;   has no effect, same as in the checking-only port below it.
;; - CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not
;;   enforced.
;; - PHONE (phonetic suggestions, a Hunspell-only feature that
;;   Nuspell's own `Suggester' never calls either) is not parsed.
;; - MAXCPDSUGS, SUBSTANDARD, NOSPLITSUGS and SUGSWITHDOTS are not
;;   parsed; Nuspell itself parses these but its `Suggester' never
;;   consults them, so they would have no effect here either.
;; - Nuspell's suggestion-casing-fix loop, which moves a
;;   newly-recased, space-containing CAMEL/PASCAL suggestion to the
;;   front of the list one at a time, is instead done as a single
;;   stable partition (recased suggestions first, in their original
;;   relative order); when more than one suggestion qualifies, their
;;   relative order can come out differently than upstream's.
;; - `-move-char-suggest' and `-distant-swap-suggest' enumerate
;;   candidate character moves/swaps directly instead of Nuspell's
;;   swap-and-revert traversal; the budget used to cap either
;;   algorithm is still consulted once per candidate, as upstream
;;   does, but exhausting it while moving through backward moves,
;;   candidates that upstream's forward-then-backward traversal order
;;   would have tried first may be skipped instead.
;; - SIMPLIFIEDTRIPLE's extra "insert the elided letter back and
;;   retry" fallback is not implemented; CHECKCOMPOUNDTRIPLE itself is.
;; - COMPOUNDSYLLABLE/SYLLABLENUM, the Hungarian-specific syllable
;;   counting rules that let a compound exceed COMPOUNDWORDMAX, are not
;;   implemented; COMPOUNDWORDMAX is always a hard cap.
;; - LANG-specific casing rules (e.g. Turkish dotless i) are not
;;   implemented; casing uses Emacs's own `upcase'/`downcase'.
;; - Morphological fields in .dic and the AM directive are ignored,
;;   same as in Nuspell itself.
;; - FULLSTRIP is parsed but has no effect, matching Nuspell's own
;;   (apparently unused) implementation of it.
;; - Dictionary files are read using Emacs's own file-decoding; the
;;   SET directive's declared encoding is not separately consulted.
;; - The 360-character length limit in `vibe-spell-check' is counted in
;;   characters rather than UTF-8 bytes.  Also, unlike Nuspell, a word
;;   over the limit is treated as correctly spelled rather than as a
;;   misspelling, so that long non-word tokens are not flagged as
;;   errors.
;;
;; For affix stripping, Nuspell tries combinations of up to one prefix
;; and up to two suffixes (or vice versa); this port implements that
;; same set of combinations.  A small number of Nuspell's own
;; performance-only early-exit checks (annotated as such in its
;; sources) are omitted here, since they do not affect correctness.

;;; Code:

(require 'cl-lib)
(require 'xdg)

(defgroup vibe-spell nil
  "Hunspell-compatible spell checking library in pure Emacs Lisp."
  :prefix "vibe-spell-"
  :group 'text)

(defcustom vibe-spell-dictionary-path
  (mapcar (lambda (d) (expand-file-name "hunspell/" d))
          (cons (xdg-data-home) (xdg-data-dirs)))
  "List of directories to search for dictionaries.
Given a dictionary NAME, we expect to find files NAME.aff and NAME.dic
in one of those directories."
  :type '(repeat directory))

(defcustom vibe-spell-personal-dictionary-format
  (list (expand-file-name "vibe-spell/%s" user-emacs-directory)
        (expand-file-name "~/hunspell_%s"))
  "Name pattern of personal directories.
Each string is passed to `format' with the dictionary name as argument.
The first file to be found is used to save additional words.  If none of
the files exist, create one from the first item of the list."
  :type '(choice string (repeat string)))

(defvar -max-word-length
  (1+ (length "\
Donaudampfschiffahrtselektrizitätenhauptbetriebswerkbauunterbeamtengesellschaft"))
  "Maximum word length accepted by the spell checker.
Longer words are always reported as correct.")

;;; Public API

(defun vibe-spell-create (dict)
  "Create a spell checker object for dictionary DICT.
DICT is a dictionary name, such as \"en_US\"; the files DICT.aff and
DICT.dic are looked up under `vibe-spell-dictionary-path'.

See `vibe-spell-check' and `vibe-spell-suggest'; see the Commentary at
the top of this file for the limitations of this port.

Additional words accepted via `vibe-spell-add-word' in a past session
are loaded from the personal dictionary file named by
`vibe-spell-personal-dictionary-format', if it exists."
  (let* ((base (-find-dictionary-base dict))
         (aff-file (concat base ".aff"))
         (dic-file (concat base ".dic")))
    (let ((d (-dict-create))
          ;; Loading briefly allocates a lot (the words table, flag
          ;; lists, ...); let it run without GC interrupting partway.
          (gc-cons-threshold most-positive-fixnum))
      (-parse-aff d aff-file)
      (when (-dict-complex-prefixes d)
        (error "vibe-spell: %s uses COMPLEXPREFIXES, which is not supported" dict))
      (-parse-dic d dic-file)
      (-init-personal-dictionary d dict)
      d)))

(defun vibe-spell-check (dict word)
  "Return non-nil if WORD is spelled correctly according to DICT.
DICT is a spell checker object created by `vibe-spell-create'."
  (or (length> word 100) (-spell dict word)))

(defun vibe-spell-suggest (dict word)
  "Return a list of corrections for WORD based on DICT.
DICT is a spell checker object created by `vibe-spell-create'.  Return
nil, without error, if WORD is too long for suggestions to be
attempted; see the 360-character limit note in the Commentary at the
top of this file."
  (unless (length> word -max-word-length)
    (let ((gc-cons-percentage (max gc-cons-percentage 1.0)))
      (-suggest-priv dict word))))

(defun vibe-spell-add-word (dict word &optional save)
  "Add WORD to DICT, and optionally SAVE to personal dictionary file.
DICT is a spell checker object created by `vibe-spell-create'.  When
SAVE is non-nil, also append WORD to DICT's personal dictionary file
\(see `vibe-spell-personal-dictionary-format'), creating it if needed,
so that it is picked up again by a later `vibe-spell-create'."
  (-dict-add-word dict word nil)
  (when save
    (-save-personal-word (-dict-pdict dict) word)))

(defun vibe-spell-remove-word (dict word &optional save)
  "Remove WORD from DICT, and optionally SAVE that to the personal dictionary file.
DICT is a spell checker object created by `vibe-spell-create'.  This
undoes one `vibe-spell-add-word' call for WORD; an entry for WORD that
came from the dictionary's own .dic file, rather than from
`vibe-spell-add-word', is left alone.  When SAVE is non-nil, also
remove WORD from DICT's personal dictionary file (see
`vibe-spell-personal-dictionary-format'), if it is there, so that a
later `vibe-spell-create' does not load it back.

Return non-nil if WORD was removed, nil if it was not there to begin
with."
  (let ((removed (-dict-remove-word dict word)))
    (when save
      (-remove-personal-word (-dict-pdict dict) word))
    removed))

;;; Implementation

;;;; Helpers

(defmacro -dolist-until (spec &rest body)
  "Like `dolist', but stop when BODY first evaluates to a non-nil value.
When that happens, evaluate RESULT and return it.  If RESULT is omitted,
return the last body value.

\(fn (VAR LIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 2 (length spec) 3)
    (signal 'wrong-number-of-arguments (list '(2 . 3) (length spec))))
  (let ((var (car spec))
        (tail (make-symbol "tail"))
        (result (make-symbol "result")))
    `(let ((,tail ,(nth 1 spec)) ,var ,result)
       (while ,tail
         (setq ,var (car ,tail))
         (setq ,result ,(macroexp-progn body))
         (setq ,tail (if ,result
                         ,(when (length= spec 3)
                            `(prog1 nil
                               (setq ,result ,(macroexp-progn (cddr spec)))))
                       (cdr ,tail))))
       ,result)))

(defun -find-dictionary-base (name)
  "Return the path, sans extension, of dictionary NAME.
Searches `vibe-spell-dictionary-path' for the first directory
containing both a readable NAME.aff and a readable NAME.dic, and
returns that directory's `expand-file-name' of NAME.  Signals an
error if no directory in the list has both files."
  (or (-dolist-until (dir vibe-spell-dictionary-path)
        (let ((base (expand-file-name name dir)))
          (and (file-readable-p (concat base ".aff"))
               (file-readable-p (concat base ".dic"))
               base)))
      (user-error "\
vibe-spell: cannot find %s.aff and %s.dic under `vibe-spell-dictionary-path'"
                  name name)))

;;;; Data structures

(cl-defstruct (-prefix (:constructor -prefix-create))
  flag cross-product stripping appending cont-flags condition)

(cl-defstruct (-suffix (:constructor -suffix-create))
  flag cross-product stripping appending cont-flags condition)

(cl-defstruct (-similarity-group (:constructor -similarity-group-create))
  "One MAP entry: a set of characters/strings considered interchangeable."
  (chars "") (strings nil))

(cl-defstruct (-dict (:constructor -dict-create))
  (words nil)                          ; hash-table: word -> (LOWERCASED . list of flag lists)
  (pdict nil)                          ; personal dictionary file name
  (prefix-table nil)                   ; sorted vector of (KEY . `-prefix'), KEY = appending
  (suffix-table nil)                   ; sorted vector of (KEY . `-suffix'), KEY = (reverse appending)
  (compound-rules nil)                 ; list of patterns; pattern = list of (flag . quant)
  (compound-rules-all-flags nil)
  (break-start nil) (break-end nil) (break-middle nil)
  (flag-aliases nil)                   ; vector of flag lists, alias N at index N-1
  (flag-type 'single-char)
  (ignored-chars "")
  (complex-prefixes nil)
  (fullstrip nil)
  (checksharps nil)
  (forbid-warn nil)
  (compound-onlyin-flag 0)
  (circumfix-flag 0)
  (forbiddenword-flag 0)
  (keepcase-flag 0)
  (need-affix-flag 0)
  (warn-flag 0)
  (compound-flag 0)
  (compound-begin-flag 0)
  (compound-last-flag 0)
  (compound-middle-flag 0)
  (compound-min-length 0)
  (compound-max-word-count 0)
  (compound-permit-flag 0)
  (compound-forbid-flag 0)
  (compound-root-flag 0)
  (compound-force-uppercase 0)
  (compound-check-duplicate nil)
  (compound-check-case nil)
  (compound-check-triple nil)
  ;; suggestion options
  (try-chars "")
  (keyboard-closeness "")
  (rep-whole nil) (rep-start nil) (rep-end nil) (rep-any nil)
  (similarities nil)                   ; list of `-similarity-group'
  (input-iconv nil)                    ; sorted list of (FROM . TO), see `-substr-replace'
  (output-oconv nil)
  (nosuggest-flag 0)
  (max-ngram-suggestions 4)
  (max-diff-factor 5)
  (only-max-diff nil)
  (ngram-candidates nil))              ; cache, see `-ngram-candidates'

(defsubst -homonyms (dict word)
  "Return the list of flag lists of WORD's homonym entries in DICT."
  (cdr (gethash word (-dict-words dict))))

;;;; Flags and flag sets

(defun -decode-flags (str flag-type)
  "Decode flags field text STR into a list of flag integers.
FLAG-TYPE is one of `single-char', `utf-8', `long' or `num', matching
the affix file's FLAG directive (default `single-char')."
  (pcase flag-type
    ((or 'single-char 'utf-8)
     (let (result)
       (dotimes (i (length str))
         (push (aref str i) result))
       (nreverse result)))
    ('long
     (when (oddp (length str))
       (error "vibe-spell: odd number of characters in long flags %S" str))
     (let ((i 0) (n (length str)) result)
       (while (< i n)
         (push (logior (ash (aref str i) 8) (aref str (1+ i))) result)
         (setq i (+ i 2)))
       (nreverse result)))
    ('num
     (mapcar #'string-to-number (split-string str ",")))
    (_ (error "vibe-spell: unknown flag type %S" flag-type))))

(defun -decode-first-flag (str flag-type)
  (let ((flags (-decode-flags str flag-type)))
    (unless flags (error "vibe-spell: missing flag in %S" str))
    (car flags)))

(defun -decode-flags-possible-alias (dict str)
  "Decode a .dic word's flags field STR, honoring flag aliases."
  (let ((aliases (-dict-flag-aliases dict)))
    (if (zerop (length aliases))
        (-decode-flags str (-dict-flag-type dict))
      (let ((idx (string-to-number str)))
        (if (and (> idx 0) (<= idx (length aliases)))
            (aref aliases (1- idx))
          (error "vibe-spell: invalid flag alias %S" str))))))

(defun -flags-contain (flags flag)
  "Return non-nil if list of flag integers FLAGS contains FLAG.
A FLAG of 0 means \"not configured\" and never matches, mirroring
Nuspell, where such flag fields default to 0 when the corresponding
affix-file directive is absent."
  (and (not (zerop flag)) (memq flag flags) t))

(defsubst -hidden-homonym-p (flags)
  "Return non-nil if flag list FLAGS marks a dictionary entry as a hidden homonym.
A hidden homonym is excluded from direct lookup, so that checking the
title-cased form of a word whose canonical form has different casing
(e.g. an acronym) falls through to other checks rather than accepting
it outright; see `-dict-add-word'.  Mirrors Nuspell's
`Aff_Data::HIDDEN_HOMONYM_FLAG', represented here by the literal flag
-1 -- never a valid flag otherwise, since real flags are always
non-negative."
  (memq -1 flags))

(defsubst -cross-valid-p (inner-flags outer-flag)
  "Return non-nil if INNER-FLAGS (a flag list) contains OUTER-FLAG."
  (-flags-contain inner-flags outer-flag))

;;;; Affix conditions
;;
;; An affix condition is a tiny regexp-like pattern: "." matches any
;; character, "[abc]"/"[^abc]" matches a set of characters (with no
;; special meaning for "-", unlike POSIX/Emacs bracket expressions),
;; and any other character matches itself.  It is compiled into a
;; list of specs (one per matched character) and is checked against
;; exactly that many characters at the start (for a prefix) or end
;; (for a suffix) of the root word.

(defun -compile-condition (str)
  "Compile affix condition STR into a list of specs.
Each element is a character (literal match), t (any character, from
\".\"), or (NEGATED . CHARS) for a \"[...]\"/\"[^...]\" bracket
expression."
  (when (string-empty-p str)
    (setq str "."))
  (let ((i 0) (n (length str)) (specs nil))
    (while (< i n)
      (let ((c (aref str i)))
        (cond
         ((eq c ?.)
          (push t specs)
          (setq i (1+ i)))
         ((eq c ?\[)
          (setq i (1+ i))
          (when (>= i n)
            (error "vibe-spell: unterminated bracket in condition %S" str))
          (let (negated)
            (when (eq (aref str i) ?^)
              (setq negated t)
              (setq i (1+ i)))
            (let ((start i))
              (while (and (< i n) (not (eq (aref str i) ?\])))
                (setq i (1+ i)))
              (when (= i start)
                (error "vibe-spell: empty bracket in condition %S" str))
              (when (>= i n)
                (error "vibe-spell: unterminated bracket in condition %S" str))
              (push (cons negated (substring str start i)) specs)
              (setq i (1+ i)))))
         ((eq c ?\])
          (error "vibe-spell: stray closing bracket in condition %S" str))
         (t
          (push c specs)
          (setq i (1+ i))))))
    (nreverse specs)))

(defun -condition-match-spec (spec ch)
  (cond
   ((eq spec t) t)
   ((integerp spec) (eq spec ch))
   ((consp spec)
    (let ((negated (car spec)) (chars (cdr spec)))
      (if negated (not (cl-position ch chars)) (and (cl-position ch chars) t))))))

(defun -condition-match-prefix (specs word)
  "Return non-nil if WORD's first (length SPECS) characters match SPECS."
  (let ((n (length specs)))
    (and (>= (length word) n)
         (let ((idx 0) (ok t))
           (dolist (spec specs)
             (unless (-condition-match-spec spec (aref word idx))
               (setq ok nil))
             (setq idx (1+ idx)))
           ok))))

(defun -condition-match-suffix (specs word)
  "Return non-nil if WORD's last (length SPECS) characters match SPECS."
  (let* ((n (length specs))
         (wlen (length word)))
    (and (>= wlen n)
         (let ((idx (- wlen n)) (ok t))
           (dolist (spec specs)
             (unless (-condition-match-spec spec (aref word idx))
               (setq ok nil))
             (setq idx (1+ idx)))
           ok))))

;;;; Affix helpers

(defun -prefix-not-valid-p (dict mode pfx)
  (let ((cont (-prefix-cont-flags pfx)))
    (or (and (eq mode :full-word)
             (-flags-contain cont (-dict-compound-onlyin-flag dict)))
        (and (eq mode :compound-end)
             (not (-flags-contain cont (-dict-compound-permit-flag dict))))
        (and (not (eq mode :full-word))
             (-flags-contain cont (-dict-compound-forbid-flag dict))))))

(defun -suffix-not-valid-p (dict mode sfx)
  (let ((cont (-suffix-cont-flags sfx)))
    (or (and (eq mode :full-word)
             (-flags-contain cont (-dict-compound-onlyin-flag dict)))
        (and (eq mode :compound-begin)
             (not (-flags-contain cont (-dict-compound-permit-flag dict))))
        (and (not (eq mode :full-word))
             (-flags-contain cont (-dict-compound-forbid-flag dict))))))

(defun -outer-prefix-not-valid-p (dict mode pfx)
  (or (-prefix-not-valid-p dict mode pfx)
      (-flags-contain (-prefix-cont-flags pfx) (-dict-need-affix-flag dict))))

(defun -outer-suffix-not-valid-p (dict mode sfx)
  (or (-suffix-not-valid-p dict mode sfx)
      (-flags-contain (-suffix-cont-flags sfx) (-dict-need-affix-flag dict))))

(defun -circumfix-flags-p (dict cont-flags)
  (-flags-contain cont-flags (-dict-circumfix-flag dict)))

(defun -valid-inside-compound-p (dict mode flags)
  (cond
   ((eq mode :compound-begin)
    (or (-flags-contain flags (-dict-compound-flag dict))
        (-flags-contain flags (-dict-compound-begin-flag dict))))
   ((eq mode :compound-middle)
    (or (-flags-contain flags (-dict-compound-flag dict))
        (-flags-contain flags (-dict-compound-middle-flag dict))))
   ((eq mode :compound-end)
    (or (-flags-contain flags (-dict-compound-flag dict))
        (-flags-contain flags (-dict-compound-last-flag dict))))
   (t t)))

(defun -affix-table-lower-bound (table lo hi pos ch)
  "Return the first index in TABLE[LO,HI) whose key's char at POS is >= CH.
TABLE[LO,HI) must already be sorted by that character."
  (while (< lo hi)
    (let ((mid (/ (+ lo hi) 2)))
      (if (< (aref (car (aref table mid)) pos) ch)
          (setq lo (1+ mid))
        (setq hi mid))))
  lo)

(defun -affix-table-matches (table probe)
  "Return the entries of TABLE whose key is a prefix of PROBE.
TABLE is a vector of (KEY . ENTRY), sorted ascending by KEY, as built
by `-build-affix-table'.  This mirrors Nuspell's
`Prefix_Multiset'/`Suffix_Multiset': instead of scanning every entry
and testing each key against PROBE directly, it narrows a candidate
range of TABLE one character of PROBE at a time, via binary search,
collecting a match whenever a candidate's key is fully consumed."
  (let ((lo 0) (hi (length table)) (len 0) (plen (length probe)) (done nil) result)
    (while (not done)
      ;; TABLE[lo,hi) all share key[0..len) == probe[0..len); yield
      ;; those whose key is now exactly that (nothing left to match).
      (while (and (< lo hi) (= (length (car (aref table lo))) len))
        (push (cdr (aref table lo)) result)
        (setq lo (1+ lo)))
      (if (or (>= lo hi) (= len plen))
          (setq done t)
        ;; Narrow [lo,hi) to the entries whose next character, at
        ;; POS=len, matches PROBE's; everything in it now has a key
        ;; longer than LEN, so that character is always there.
        (setq hi (-affix-table-lower-bound table lo hi len (1+ (aref probe len))))
        (setq lo (-affix-table-lower-bound table lo hi len (aref probe len)))
        (setq len (1+ len))))
    (nreverse result)))

(defun -build-affix-table (entries key-fn)
  "Build a sorted affix table from the list ENTRIES.
KEY-FN extracts the sort/match key string from one entry (the
appending text for prefixes, or its reverse for suffixes); see
`-affix-table-matches'."
  (let ((table (vconcat (mapcar (lambda (e) (cons (funcall key-fn e) e)) entries))))
    (sort table (lambda (a b) (string< (car a) (car b))))))

(defun -all-affix-table-entries (table)
  "Return the `-prefix'/`-suffix' entries of affix TABLE as a plain list.
Unlike `-affix-table-matches', this does not filter by any probe word."
  (mapcar #'cdr (append table nil)))

(defun -iterate-prefixes-of (dict word)
  "Return the `-prefix' entries of DICT whose text could precede WORD."
  (-affix-table-matches (-dict-prefix-table dict) word))

(defun -iterate-suffixes-of (dict word)
  "Return the `-suffix' entries of DICT whose text could follow WORD."
  (-affix-table-matches (-dict-suffix-table dict) (reverse word)))

(defun -prefix-to-root (pfx word)
  (concat (-prefix-stripping pfx)
          (substring word (length (-prefix-appending pfx)))))

(defun -suffix-to-root (sfx word)
  (let ((n (length (-suffix-appending sfx))))
    (concat (substring word 0 (- (length word) n)) (-suffix-stripping sfx))))

(defun -prefix-from-root (pfx root)
  "Apply PFX to ROOT, the opposite direction of `-prefix-to-root'."
  (concat (-prefix-appending pfx) (substring root (length (-prefix-stripping pfx)))))

(defun -suffix-from-root (sfx root)
  "Apply SFX to ROOT, the opposite direction of `-suffix-to-root'."
  (let ((n (length (-suffix-stripping sfx))))
    (concat (substring root 0 (- (length root) n)) (-suffix-appending sfx))))

(defun -prefix-check-condition (pfx word)
  (-condition-match-prefix (-prefix-condition pfx) word))

(defun -suffix-check-condition (sfx word)
  (-condition-match-suffix (-suffix-condition sfx) word))

(defun -prefix-condition-matches-p (pfx word)
  "Like (-prefix-check-condition PFX (-prefix-to-root PFX WORD)), but
without ever building that root string.  Nuspell's dictionaries can
have many affix entries sharing the same `appending' text (so that
`-iterate-prefixes-of' returns many candidates for one WORD), each
telling candidates apart only by its condition; since most such
candidates fail their condition, checking it against a WORD-and-
STRIPPING \"virtual\" root -- indexing directly into WORD and
STRIPPING instead of consing a new string to throw away -- avoids
the allocation for the common case where it was going to be wasted."
  (let* ((specs (-prefix-condition pfx))
         (n (length specs))
         (stripping (-prefix-stripping pfx))
         (strip-len (length stripping))
         (app-len (length (-prefix-appending pfx))))
    (and (>= (+ strip-len (- (length word) app-len)) n)
         (let ((idx 0) (ok t))
           (dolist (spec specs)
             (let ((ch (if (< idx strip-len)
                           (aref stripping idx)
                         (aref word (+ app-len (- idx strip-len))))))
               (unless (-condition-match-spec spec ch)
                 (setq ok nil)))
             (setq idx (1+ idx)))
           ok))))

(defun -suffix-condition-matches-p (sfx word)
  "Like (-suffix-check-condition SFX (-suffix-to-root SFX WORD)), but
without ever building that root string; see
`-prefix-condition-matches-p', which this mirrors."
  (let* ((specs (-suffix-condition sfx))
         (n (length specs))
         (stripping (-suffix-stripping sfx))
         (strip-len (length stripping))
         (app-len (length (-suffix-appending sfx)))
         (wordlen (length word))
         (rootlen (+ (- wordlen app-len) strip-len))
         (boundary (- rootlen strip-len)))
    (and (>= rootlen n)
         (let ((idx (- rootlen n)) (ok t))
           (dolist (spec specs)
             (let ((ch (if (>= idx boundary)
                           (aref stripping (- idx boundary))
                         (aref word idx))))
               (unless (-condition-match-spec spec ch)
                 (setq ok nil)))
             (setq idx (1+ idx)))
           ok))))

;;;; Affix stripping
;;
;; Each of these functions returns (ROOT-WORD . FLAGS), where
;; ROOT-WORD is the dictionary key that was found and FLAGS is the
;; specific flag list of that homonym entry (`eq'-comparable to
;; whatever `-homonyms' returns for it), or nil if no
;; match was found.  This is a simplified version of Nuspell's
;; `Affixing_Result', which also carries the affix objects used; this
;; port has no use for that, since it does not implement Nuspell's
;; syllable/word-count suggestion modifiers.
;;
;; MODE is one of :full-word, :compound-begin, :compound-middle or
;; :compound-end, mirroring Nuspell's `Affixing_Mode'.  The 2- and
;; 3-affix combinations below are only ever used at :full-word (see
;; `-check-simple-word'), matching Nuspell's own usage.

(defun -strip-suffix-only (dict mode word skip-hidden)
  (-dolist-until (se (-iterate-suffixes-of dict word))
    (and (not (or (-outer-suffix-not-valid-p dict mode se)
                  (-circumfix-flags-p dict (-suffix-cont-flags se))))
         (-suffix-condition-matches-p se word)
         (let ((root (-suffix-to-root se word)))
           (-dolist-until (flags (-homonyms dict root))
             (and (-cross-valid-p flags (-suffix-flag se))
                  (not (and (eq mode :full-word)
                            (-flags-contain flags (-dict-compound-onlyin-flag dict))))
                  (not (and skip-hidden (-hidden-homonym-p flags)))
                  (or (-valid-inside-compound-p dict mode flags)
                      (-valid-inside-compound-p dict mode (-suffix-cont-flags se)))
                  (cons root flags)))))))

(defun -strip-prefix-only (dict mode word skip-hidden)
  (-dolist-until (pe (-iterate-prefixes-of dict word))
    (and (not (or (-outer-prefix-not-valid-p dict mode pe)
                  (-circumfix-flags-p dict (-prefix-cont-flags pe))))
         (-prefix-condition-matches-p pe word)
         (let ((root (-prefix-to-root pe word)))
           (-dolist-until (flags (-homonyms dict root))
             (and (-cross-valid-p flags (-prefix-flag pe))
                  (not (and (eq mode :full-word)
                            (-flags-contain flags (-dict-compound-onlyin-flag dict))))
                  (not (and skip-hidden (-hidden-homonym-p flags)))
                  (or (-valid-inside-compound-p dict mode flags)
                      (-valid-inside-compound-p dict mode (-prefix-cont-flags pe)))
                  (cons root flags)))))))

(defun -strip-prefix-then-suffix-commutative (dict mode word skip-hidden)
  "Strip one prefix and one suffix, in either order.
A derived word formed as prefix+root+suffix can have been built by
adding either affix first, as long as the other one's flag allows
cross-product with it; this checks both directions at once, as
Nuspell's \"commutative\" variant does."
  (-dolist-until (pe (-iterate-prefixes-of dict word))
    (and (-prefix-cross-product pe)
         (not (-prefix-not-valid-p dict mode pe))
         (-prefix-condition-matches-p pe word)
         (let ((root1 (-prefix-to-root pe word)))
           (-strip-pfx-then-sfx-comm-2 dict mode pe root1 skip-hidden)))))

(defun -strip-pfx-then-sfx-comm-2 (dict mode pe word skip-hidden)
  (let* ((pe-cont (-prefix-cont-flags pe))
         (has-needaffix-pe (-flags-contain pe-cont (-dict-need-affix-flag dict)))
         (circumfix-pe (-circumfix-flags-p dict pe-cont)))
    (-dolist-until (se (-iterate-suffixes-of dict word))
      (and (-suffix-cross-product se)
           (not (-suffix-not-valid-p dict mode se))
           (let* ((se-cont (-suffix-cont-flags se))
                  (has-needaffix-se (-flags-contain se-cont (-dict-need-affix-flag dict))))
             (and (not (and has-needaffix-pe has-needaffix-se))
                  (eq circumfix-pe (-circumfix-flags-p dict se-cont))
                  (-suffix-condition-matches-p se word)
                  (let ((root (-suffix-to-root se word)))
                    (-dolist-until (flags (-homonyms dict root))
                      (let* ((valid-pe-outer
                              (and (not has-needaffix-pe)
                                   (-cross-valid-p flags (-suffix-flag se))
                                   (or (-cross-valid-p se-cont (-prefix-flag pe))
                                       (-cross-valid-p flags (-prefix-flag pe)))))
                             (valid-se-outer
                              (and (not has-needaffix-se)
                                   (-cross-valid-p flags (-prefix-flag pe))
                                   (or (-cross-valid-p pe-cont (-suffix-flag se))
                                       (-cross-valid-p flags (-suffix-flag se))))))
                        (and (or valid-pe-outer valid-se-outer)
                             (not (and (eq mode :full-word)
                                       (-flags-contain flags (-dict-compound-onlyin-flag dict))))
                             (not (and skip-hidden (-hidden-homonym-p flags)))
                             (or (-valid-inside-compound-p dict mode flags)
                                 (-valid-inside-compound-p dict mode se-cont)
                                 (-valid-inside-compound-p dict mode pe-cont))
                             (cons root flags)))))))))))

(defun -strip-suffix-then-suffix (dict word skip-hidden)
  (-dolist-until (se1 (-iterate-suffixes-of dict word))
    (and (not (or (-outer-suffix-not-valid-p dict :full-word se1)
                  (-circumfix-flags-p dict (-suffix-cont-flags se1))))
         (-suffix-condition-matches-p se1 word)
         (let ((root1 (-suffix-to-root se1 word)))
           (-strip-sfx-then-sfx-2 dict se1 root1 skip-hidden)))))

(defun -strip-sfx-then-sfx-2 (dict se1 word skip-hidden)
  (-dolist-until (se2 (-iterate-suffixes-of dict word))
    (and (-cross-valid-p (-suffix-cont-flags se2) (-suffix-flag se1))
         (not (or (-suffix-not-valid-p dict :full-word se2)
                  (-circumfix-flags-p dict (-suffix-cont-flags se2))))
         (-suffix-condition-matches-p se2 word)
         (let ((root (-suffix-to-root se2 word)))
           (-dolist-until (flags (-homonyms dict root))
             (and (-cross-valid-p flags (-suffix-flag se2))
                  (not (-flags-contain flags (-dict-compound-onlyin-flag dict)))
                  (not (and skip-hidden (-hidden-homonym-p flags)))
                  (cons root flags)))))))

(defun -strip-prefix-then-2-suffixes (dict word skip-hidden)
  (-dolist-until (pe1 (-iterate-prefixes-of dict word))
    (and (-prefix-cross-product pe1)
         (not (-outer-prefix-not-valid-p dict :full-word pe1))
         (-prefix-condition-matches-p pe1 word)
         (let ((root-p (-prefix-to-root pe1 word)))
           (-dolist-until (se1 (-iterate-suffixes-of dict root-p))
             (and (-suffix-cross-product se1)
                  (not (-suffix-not-valid-p dict :full-word se1))
                  (eq (-circumfix-flags-p dict (-prefix-cont-flags pe1))
                      (-circumfix-flags-p dict (-suffix-cont-flags se1)))
                  (-suffix-condition-matches-p se1 root-p)
                  (let ((root1 (-suffix-to-root se1 root-p)))
                    (-strip-pfx-2-sfx-3 dict pe1 se1 root1 skip-hidden))))))))

(defun -strip-pfx-2-sfx-3 (dict pe1 se1 word skip-hidden)
  (-dolist-until (se2 (-iterate-suffixes-of dict word))
    (and (-cross-valid-p (-suffix-cont-flags se2) (-suffix-flag se1))
         (not (or (-suffix-not-valid-p dict :full-word se2)
                  (-circumfix-flags-p dict (-suffix-cont-flags se2))))
         (-suffix-condition-matches-p se2 word)
         (let ((root (-suffix-to-root se2 word)))
           (-dolist-until (flags (-homonyms dict root))
             (and (or (-cross-valid-p (-suffix-cont-flags se1) (-prefix-flag pe1))
                      (-cross-valid-p flags (-prefix-flag pe1)))
                  (-cross-valid-p flags (-suffix-flag se2))
                  (not (-flags-contain flags (-dict-compound-onlyin-flag dict)))
                  (not (and skip-hidden (-hidden-homonym-p flags)))
                  (cons root flags)))))))

(defun -strip-suffix-prefix-suffix (dict word skip-hidden)
  (-dolist-until (se1 (-iterate-suffixes-of dict word))
    (and (-suffix-cross-product se1)
         (not (-outer-suffix-not-valid-p dict :full-word se1))
         (-suffix-condition-matches-p se1 word)
         (let ((root-s (-suffix-to-root se1 word)))
           (-dolist-until (pe1 (-iterate-prefixes-of dict root-s))
             (and (-prefix-cross-product pe1)
                  (not (-prefix-not-valid-p dict :full-word pe1))
                  (-prefix-condition-matches-p pe1 root-s)
                  (let ((root1 (-prefix-to-root pe1 root-s)))
                    (-strip-s-p-s-3 dict se1 pe1 root1 skip-hidden))))))))

(defun -strip-s-p-s-3 (dict se1 pe1 word skip-hidden)
  (-dolist-until (se2 (-iterate-suffixes-of dict word))
    (and (-suffix-cross-product se2)
         (or (-cross-valid-p (-suffix-cont-flags se2) (-suffix-flag se1))
             (-cross-valid-p (-prefix-cont-flags pe1) (-suffix-flag se1)))
         (not (-suffix-not-valid-p dict :full-word se2))
         (let* ((circ-pe1 (-circumfix-flags-p dict (-prefix-cont-flags pe1)))
                (circ-se1 (-circumfix-flags-p dict (-suffix-cont-flags se1)))
                (circ-se2 (-circumfix-flags-p dict (-suffix-cont-flags se2)))
                (ok1 (and (eq circ-pe1 circ-se1) (not circ-se2)))
                (ok2 (and (eq circ-pe1 circ-se2) (not circ-se1))))
           (and (or ok1 ok2)
                (-suffix-condition-matches-p se2 word)
                (let ((root (-suffix-to-root se2 word)))
                  (-dolist-until (flags (-homonyms dict root))
                    (and (or (-cross-valid-p (-suffix-cont-flags se2) (-prefix-flag pe1))
                             (-cross-valid-p flags (-prefix-flag pe1)))
                         (-cross-valid-p flags (-suffix-flag se2))
                         (not (-flags-contain flags (-dict-compound-onlyin-flag dict)))
                         (not (and skip-hidden (-hidden-homonym-p flags)))
                         (cons root flags)))))))))

;;;; Simple (non-compound) word checking

(defun -check-simple-word (dict word skip-hidden)
  "Check WORD as a single (possibly affixed) dictionary entry.
Return (ROOT-WORD . FLAGS) or nil; see the affix-stripping functions
above for the shape of the result."
  (or (-dolist-until (flags (-homonyms dict word))
        (and (not (or (-flags-contain flags (-dict-need-affix-flag dict))
                      (-flags-contain flags (-dict-compound-onlyin-flag dict))
                      (and skip-hidden (-hidden-homonym-p flags))))
             (cons word flags)))
      (-strip-suffix-only dict :full-word word skip-hidden)
      (-strip-prefix-only dict :full-word word skip-hidden)
      (-strip-prefix-then-suffix-commutative dict :full-word word skip-hidden)
      (-strip-suffix-then-suffix dict word skip-hidden)
      (-strip-prefix-then-2-suffixes dict word skip-hidden)
      (-strip-suffix-prefix-suffix dict word skip-hidden)))

;;;; Compound checking

(defun -check-word-in-compound (dict mode word)
  "Check WORD as one part of a compound, in position MODE.
MODE is :compound-begin, :compound-middle or :compound-end."
  (let ((cpd-flag (cond ((eq mode :compound-begin) (-dict-compound-begin-flag dict))
                        ((eq mode :compound-middle) (-dict-compound-middle-flag dict))
                        ((eq mode :compound-end) (-dict-compound-last-flag dict))
                        (t 0))))
    (or (-dolist-until (flags (-homonyms dict word))
          (and (not (or (-flags-contain flags (-dict-need-affix-flag dict))
                        (not (or (-flags-contain flags (-dict-compound-flag dict))
                                 (-flags-contain flags cpd-flag)))
                        (-hidden-homonym-p flags)))
               (cons word flags)))
        (-strip-suffix-only dict mode word t)
        (-strip-prefix-only dict mode word t)
        (-strip-prefix-then-suffix-commutative dict mode word t))))

(defun -char-alpha-p (ch) (memq (get-char-code-property ch 'general-category)
                                '(Ll Lu Lo Lm Lt)))
(defun -char-upper-p (ch) (/= ch (downcase ch)))
(defun -char-lower-p (ch) (/= ch (upcase ch)))

(defun -uppercase-at-compound-boundary-p (word i)
  "Return non-nil if WORD has a letter case change straddling index I."
  (let ((before (and (> i 0) (aref word (1- i))))
        (after (and (< i (length word)) (aref word i))))
    (or (and after (-char-upper-p after) before (-char-alpha-p before))
        (and before (-char-upper-p before) after (-char-alpha-p after)))))

(defun -three-equal-at-boundary-p (word i)
  "Return non-nil if three identical characters straddle index I in WORD."
  (when (and (> i 0) (< i (length word)))
    (let ((b (aref word (1- i))) (c (aref word i)))
      (when (eq b c)
        (or (and (< (1+ i) (length word)) (eq c (aref word (1+ i))))
            (and (> (1- i) 0) (eq (aref word (- i 2)) b)))))))

(defun -check-compound-classic (dict mode word start-pos i num-part allow-bad-forceucase)
  "Try to split WORD[START-POS:] as a compound with its first part ending at I.
Returns the dictionary entry (ROOT-WORD . FLAGS) for the first part on
success, or nil.  MODE is :compound-begin or :compound-middle,
matching the position of the first part."
  (let* ((part1-text (substring word start-pos i))
         (part1-entry (-check-word-in-compound dict mode part1-text)))
    (and part1-entry
         (not (-flags-contain (cdr part1-entry) (-dict-forbiddenword-flag dict)))
         (not (and (-dict-compound-check-triple dict) (-three-equal-at-boundary-p word i)))
         (not (and (-dict-compound-check-case dict) (-uppercase-at-compound-boundary-p word i)))
         (let* ((root-flag (-dict-compound-root-flag dict))
                (num-part1 (+ num-part (if (-flags-contain (cdr part1-entry) root-flag) 1 0)))
                (part2-text (substring word i))
                (part2-entry (-check-word-in-compound dict :compound-end part2-text))
                (success2
                 (and part2-entry
                      (not (-flags-contain (cdr part2-entry) (-dict-forbiddenword-flag dict)))
                      (not (and (-dict-compound-check-duplicate dict)
                                (eq (cdr part1-entry) (cdr part2-entry))))
                      (not (and (/= 0 (-dict-compound-force-uppercase dict))
                                (not allow-bad-forceucase)
                                (-flags-contain (cdr part2-entry) (-dict-compound-force-uppercase dict)))))))
           (if success2
               (let ((num-part2 (+ num-part1 (if (-flags-contain (cdr part2-entry) root-flag) 1 0))))
                 (and (not (and (/= 0 (-dict-compound-max-word-count dict))
                                (>= (1+ num-part2) (-dict-compound-max-word-count dict))))
                      part1-entry))
             (and (-check-compound-split dict :compound-middle word i (1+ num-part1) allow-bad-forceucase)
                  part1-entry))))))

(defun -check-compound-split (dict mode word start-pos num-part allow-bad-forceucase)
  "Try every split point of WORD[START-POS:] respecting COMPOUNDMIN."
  (let* ((min-len (if (/= 0 (-dict-compound-min-length dict))
                      (-dict-compound-min-length dict)
                    3))
         (len (length word)))
    (and (>= (- len start-pos) (* 2 min-len))
         (-dolist-until (i (number-sequence (+ start-pos min-len) (- len min-len)))
           (-check-compound-classic dict mode word start-pos i num-part
                                    allow-bad-forceucase)))))

(defun -compound-rules-has-any-flag-p (dict flags)
  (any (lambda (f) (-flags-contain flags f)) (-dict-compound-rules-all-flags dict)))

(defun -find-compound-rule-word (dict word)
  (-dolist-until (flags (-homonyms dict word))
    (and (not (or (-flags-contain flags (-dict-need-affix-flag dict))
                  (not (-compound-rules-has-any-flag-p dict flags))))
         (cons word flags))))

(defun -match-compound-rule-1 (data pattern)
  "Match flag lists DATA (one per word part) against a compound-rule PATTERN.
PATTERN is a list of (FLAG . QUANT), QUANT one of nil, ?? or ?*."
  (cond
   ((null pattern) (null data))
   (t
    (let* ((node (car pattern)) (flag (car node)) (quant (cdr node))
           (rest-pat (cdr pattern)))
      (cond
       ((eq quant ??)
        (or (-match-compound-rule-1 data rest-pat)
            (and data (-flags-contain (car data) flag)
                 (-match-compound-rule-1 (cdr data) rest-pat))))
       ((eq quant ?*)
        (or (-match-compound-rule-1 data rest-pat)
            (and data (-flags-contain (car data) flag)
                 (-match-compound-rule-1 (cdr data) pattern))))
       (t
        (and data (-flags-contain (car data) flag)
             (-match-compound-rule-1 (cdr data) rest-pat))))))))

(defun -match-compound-rules (dict words-data)
  (any (lambda (pattern) (-match-compound-rule-1 words-data pattern))
       (-dict-compound-rules dict)))

(defun -check-compound-with-rules (dict word words-data start-pos allow-bad-forceucase)
  "Try to split WORD[START-POS:] according to COMPOUNDRULE patterns.
WORDS-DATA is the flag lists of the ancestor parts already matched,
most recent first."
  (let* ((min-len (if (/= 0 (-dict-compound-min-length dict))
                      (-dict-compound-min-length dict)
                    3))
         (len (length word)))
    (and (>= (- len start-pos) (* 2 min-len))
         (-dolist-until (i (number-sequence (+ start-pos min-len) (- len min-len)))
           (let* ((part1-text (substring word start-pos i))
                  (part1-entry (-find-compound-rule-word dict part1-text)))
             (and part1-entry
                  (let* ((words-data1 (cons (cdr part1-entry) words-data))
                         (part2-text (substring word i))
                         (part2-entry (-find-compound-rule-word dict part2-text)))
                    (or (and part2-entry
                             (let ((words-data2 (cons (cdr part2-entry) words-data1)))
                               (and (-match-compound-rules dict (reverse words-data2))
                                    (not (and (/= 0 (-dict-compound-force-uppercase dict))
                                              (not allow-bad-forceucase)
                                              (-flags-contain (cdr part2-entry)
                                                              (-dict-compound-force-uppercase dict))))
                                    part1-entry)))
                        (-check-compound-with-rules dict word words-data1 i allow-bad-forceucase)))))))))

(defun -check-compound (dict word allow-bad-forceucase)
  (or (and (or (/= 0 (-dict-compound-flag dict))
               (/= 0 (-dict-compound-begin-flag dict))
               (/= 0 (-dict-compound-middle-flag dict))
               (/= 0 (-dict-compound-last-flag dict)))
           (-check-compound-split dict :compound-begin word 0 0 allow-bad-forceucase))
      (and (-dict-compound-rules dict)
           (-check-compound-with-rules dict word nil 0 allow-bad-forceucase))))

(defun -check-word (dict word &optional allow-bad-forceucase skip-hidden)
  "Check WORD as a simple word or as a compound.
Return (MATCHED-WORD . FLAGS) or nil; FLAGS may itself be nil for a
dictionary entry with no flags at all, so nil here means WORD was
not found -- never confuse this with a found entry's (possibly nil)
FLAGS, as `cdr' of the result would."
  (or (-check-simple-word dict word skip-hidden)
      (-check-compound dict word allow-bad-forceucase)))

;;;; Casing

(defun -to-title (s)
  (if (string-empty-p s)
      s
    (concat (upcase (substring s 0 1)) (downcase (substring s 1)))))

(defun -classify-casing (s)
  "Classify S as `small', `init-capital', `all-capital', `pascal' or `camel'."
  (let ((case-fold-search nil)
        (upper-rx "[[:upper:]]+")
        (lower-rx "[[:lower:]]"))
    (if (not (string-match upper-rx s))
        'small
      (let ((beg (match-beginning 0)) (end (match-end 0)))
        (if (> beg 0)
            ;; First character isn't upper-case, so `init-capital' and
            ;; `pascal' (which both require that) are out; it's
            ;; `all-capital' if there's no lower-case letter anywhere,
            ;; `camel' otherwise.
            (if (string-match-p lower-rx s) 'camel 'all-capital)
          ;; First character is upper-case.  There's exactly one
          ;; upper-case character in all of S iff this leading run is
          ;; a single character and no further one follows it --
          ;; checking the latter can start right after the run instead
          ;; of rescanning it.
          (if (and (= end 1) (not (string-match-p upper-rx s end)))
              'init-capital
            (if (string-match-p lower-rx s) 'pascal 'all-capital)))))))

(defun -spell-casing (dict s)
  "Dispatch S to the right casing-aware check for DICT.
A leading lower-case letter already rules out `all-capital' and
`init-capital' (both require an upper-case first character), leaving
only `small'/`camel'/`pascal', which all go to `-check-word' anyway --
so most words (ordinary running text) skip `-classify-casing'
entirely."
  (if (or (string-empty-p s) (-char-lower-p (aref s 0)))
      (-check-word dict s)
    (pcase (-classify-casing s)
      ((or 'small 'camel 'pascal) (-check-word dict s))
      ('all-capital (-spell-casing-upper dict s))
      ('init-capital (-spell-casing-title dict s)))))

(defun -spell-casing-upper (dict s)
  (or (-check-word dict s t)
      ;; Apostrophe-separated prefix, e.g. Catalan/French/Italian
      ;; SANT'ELIA -> Sant'+Elia.
      (let ((apos (cl-position ?' s)))
        (and apos (/= apos (1- (length s)))
             (let* ((part1 (downcase (substring s 0 (1+ apos))))
                    (part2 (-to-title (substring s (1+ apos)))))
               (or (-check-word dict (concat part1 part2) t)
                   (progn
                     (setq part1 (-to-title part1))
                     (-check-word dict (concat part1 part2) t))))))
      ;; German sharp s.
      (and (-dict-checksharps dict) (string-search "SS" s)
           (or (-spell-sharps dict (downcase s))
               (-spell-sharps dict (-to-title s))))
      (let* ((s2 (-to-title s))
             (res (-check-word dict s2 t)))
        (and res (not (-flags-contain (cdr res) (-dict-keepcase-flag dict))) res))
      (let* ((s2 (downcase s))
             (res (-check-word dict s2 t)))
        (and res (not (-flags-contain (cdr res) (-dict-keepcase-flag dict))) res))))

(defun -spell-casing-title (dict s)
  (or (-check-word dict s t t)
      (let* ((s2 (downcase s))
             (res (-check-word dict s2 t)))
        (if (and res (-flags-contain (cdr res) (-dict-keepcase-flag dict))
                 (not (and (-dict-checksharps dict) (string-search "ß" s2))))
            nil
          res))))

(defun -spell-sharps (dict base &optional pos n rep)
  "Recursively try replacing \"ss\" with \"ß\" in BASE, German-style."
  (setq pos (or pos 0) n (or n 0) rep (or rep 0))
  (let ((max-sharps 5)
        (found (string-search "ss" base pos)))
    (if (and found (< n max-sharps))
        (or (let ((replaced (concat (substring base 0 found) "ß"
                                    (substring base (+ found 2)))))
              (-spell-sharps dict replaced (1+ found) (1+ n) (1+ rep)))
            (-spell-sharps dict base (+ found 2) (1+ n) rep))
      (and (> rep 0)
           (-check-word dict base t)))))

;;;; Word breaking (BREAK) and top-level checking

(defun -spell-break (dict s depth)
  (let ((res (-spell-casing dict s)))
    (if res
        (let ((flags (cdr res)))
          (not (or (-flags-contain flags (-dict-forbiddenword-flag dict))
                   (and (-dict-forbid-warn dict)
                        (-flags-contain flags (-dict-warn-flag dict))))))
      (and (/= depth 9)
           (or (-dolist-until (pat (-dict-break-start dict))
                 (and (string-prefix-p pat s)
                      (-spell-break dict (substring s (length pat)) (1+ depth))))
               (-dolist-until (pat (-dict-break-end dict))
                 (and (string-suffix-p pat s)
                      (-spell-break dict (substring s 0 (- (length s) (length pat)))
                                    (1+ depth))))
               (-dolist-until (pat (-dict-break-middle dict))
                 (and (not (string-empty-p pat))
                      (let ((i (string-search pat s)))
                        (and i (> i 0) (< i (- (length s) (length pat)))
                             (-spell-break dict (substring s 0 i) (1+ depth))
                             (-spell-break dict (substring s (+ i (length pat)))
                                           (1+ depth)))))))))))

(defun -number-p (s)
  "Return non-nil if S looks like a number (possibly with . , - separators)."
  (if (string-empty-p s)
      nil
    (let ((i 0) (n (length s)) (done nil) (result nil))
      (when (eq (aref s 0) ?-) (setq i 1))
      (while (and (< i n) (not done))
        (let ((start i))
          (while (and (< i n) (<= ?0 (aref s i) ?9)) (setq i (1+ i)))
          (cond
           ((= i start) (setq done t result nil))
           ((= i n) (setq done t result t))
           (t
            (let ((c (aref s i)))
              (if (memq c '(?. ?, ?-))
                  (setq i (1+ i))
                (setq done t result nil)))))))
      result)))

(defun -erase-chars (s chars)
  (if (string-empty-p chars)
      s
    (let (result)
      (dotimes (i (length s))
        (let ((c (aref s i)))
          (unless (cl-position c chars) (push c result))))
      (concat (nreverse result)))))

(defun -string-last-non-dot (s)
  "Return 1+ the index of the last non-dot character of S (0 if none)."
  (let ((i (length s)))
    (while (and (> i 0) (eq (aref s (1- i)) ?.))
      (setq i (1- i)))
    i))

(defun -spell (dict word)
  (setq word (-substr-replace (-dict-input-iconv dict) word))
  (if (string-empty-p word)
      t
    (let ((abbreviation (eq (aref word (1- (length word))) ?.)))
      (when abbreviation
        (setq word (substring word 0 (-string-last-non-dot word))))
      (if (string-empty-p word)
          t
        (if (-number-p word)
            t
          (progn
            (setq word (-erase-chars word (-dict-ignored-chars dict)))
            (let ((ret (-spell-break dict word 0)))
              (if (and (not ret) abbreviation)
                  (-spell-break dict (concat word ".") 0)
                ret))))))))

;;;; Suggestions
;;
;; This is a port of Nuspell's `Suggester', which builds a list of
;; candidate corrections by running many small, independent "one
;; typo" heuristics (`-suggest-low' and its callees) plus a slower
;; dictionary-wide n-gram similarity search (`-ngram-suggest'), then
;; cleans up and reorders the result.  Nuspell's version operates on
;; UTF-8 byte offsets because C++ strings are byte sequences; Emacs
;; strings are already sequences of characters, so this port indexes
;; by character throughout and has no equivalent of Nuspell's
;; UTF-8-boundary bookkeeping.  Likewise, affixing/stripping here
;; builds fresh strings with `substring'/`concat' instead of mutating
;; a buffer in place and reverting it afterwards, since Emacs strings
;; are not mutated in length the way C++ strings are; this is simpler
;; at the cost of some allocation, which is acceptable since
;; suggestions, unlike checking, are not expected to run in a tight
;; loop.

;;;; Suggestion accumulator

(cl-defstruct (-sugs-box (:constructor -sugs-box-create))
  "Mutable suggestion list, built up by the various `-*-suggest' functions.
Mirrors passing a `List_Strings&' around in Nuspell; HEAD/TAIL support
O(1) `-sugs-push' (appending, the common case), and COUNT is kept in
sync so callers do not need to walk the list to learn its length."
  head tail (count 0))

(defun -sugs-push (box word)
  "Append WORD to the end of BOX's suggestion list."
  (let ((cell (cons word nil)))
    (if (-sugs-box-tail box)
        (setcdr (-sugs-box-tail box) cell)
      (setf (-sugs-box-head box) cell))
    (setf (-sugs-box-tail box) cell))
  (incf (-sugs-box-count box)))

(defun -sugs-push-first (box word)
  "Prepend WORD to BOX's suggestion list."
  (let ((cell (cons word (-sugs-box-head box))))
    (setf (-sugs-box-head box) cell)
    (unless (-sugs-box-tail box)
      (setf (-sugs-box-tail box) cell)))
  (incf (-sugs-box-count box)))

(defun -sugs-list (box)
  "Return BOX's current suggestion list, in order."
  (-sugs-box-head box))

(defun -sugs-box-set (box list)
  "Replace BOX's suggestion list wholesale with LIST."
  (setf (-sugs-box-head box) list)
  (setf (-sugs-box-tail box) (last list))
  (setf (-sugs-box-count box) (length list)))

(defun -add-sug-if-correct (dict word box)
  "Push WORD onto BOX if it is a correct, suggestible word of DICT.
Return non-nil iff it was pushed.  Mirrors Nuspell's
`Suggester::add_sug_if_correct'."
  (let ((res (-check-word dict word nil t)))
    (and res
         (not (-flags-contain (cdr res) (-dict-forbiddenword-flag dict)))
         (not (and (-dict-forbid-warn dict) (-flags-contain (cdr res) (-dict-warn-flag dict))))
         (progn (-sugs-push box word) t))))

;;;; Attempt budget, for the algorithms below that are otherwise O(word
;;;; length * affix table size) or worse

(cl-defstruct (-budget (:constructor -budget-create (n)))
  "A shared, mutable count of remaining suggestion attempts."
  n)

(defun -budget-consume (budget)
  "Consume one unit of BUDGET; return non-nil if one was available."
  (when (> (-budget-n budget) 0)
    (decf (-budget-n budget))
    t))

(defun -max-attempts-for-long-algos (dict word)
  "Return DICT's suggestion-attempt budget for WORD.
A heuristic cap, based on the size of DICT's affix tables, on how many
candidates the pricier per-position suggestion heuristics below may
try; without it, a dictionary with unusually many affixes could make
suggesting take an impractically long time.  Mirrors Nuspell's
`Suggester::max_attempts_for_long_alogs'.  Unlike Nuspell, there is no
COMPLEXPREFIXES branch, since `vibe-spell-create' already rejects such
dictionaries; see the Commentary at the top of this file."
  (let* ((p (/ (length (-dict-prefix-table dict)) 20))
         (s (/ (length (-dict-suffix-table dict)) 20))
         (cost (+ 1 p s (* p s) (* s s) (* 2 p s s)))
         (cost (max 250000 (min cost 25000000000)))
         (ret (/ 25000000000 cost)))
    (if (and (or (/= 0 (-dict-compound-flag dict))
                 (/= 0 (-dict-compound-begin-flag dict))
                 (/= 0 (-dict-compound-last-flag dict))
                 (/= 0 (-dict-compound-middle-flag dict)))
             (> (length word) 0))
        (/ ret (length word))
      ret)))

;;;; Small string-surgery helpers shared by the heuristics below

(defun -replace-char-at (word i rep)
  "Return WORD with its character at index I replaced by string REP."
  (concat (substring word 0 i) rep (substring word (1+ i))))

(defun -swap-adjacent (word i)
  "Return WORD with the characters at indices I and I+1 swapped."
  (concat (substring word 0 i)
          (substring word (1+ i) (+ i 2))
          (substring word i (1+ i))
          (substring word (+ i 2))))

(defun -swap-chars (word i j)
  "Return WORD with the characters at indices I and J (I < J) swapped."
  (concat (substring word 0 i)
          (substring word j (1+ j))
          (substring word (1+ i) j)
          (substring word i (1+ i))
          (substring word (1+ j))))

(defun -swap-two-pairs (word i j)
  "Return WORD with (I . I+1) and (J . J+1) each swapped (I+2 <= J)."
  (concat (substring word 0 i)
          (substring word (1+ i) (+ i 2))
          (substring word i (1+ i))
          (substring word (+ i 2) j)
          (substring word (1+ j) (+ j 2))
          (substring word j (1+ j))
          (substring word (+ j 2))))

(defun -move-char-forward (word i j)
  "Return WORD with the character at index I moved to just after J (J > I)."
  (concat (substring word 0 i)
          (substring word (1+ i) (1+ j))
          (substring word i (1+ i))
          (substring word (1+ j))))

(defun -move-char-backward (word i j)
  "Return WORD with the character at index I moved to index J (J < I)."
  (concat (substring word 0 j)
          (substring word i (1+ i))
          (substring word j i)
          (substring word (1+ i))))

(defun -to-title-char-at (s i)
  "Return S with its character at index I upcased; the rest is unchanged."
  (concat (substring s 0 i) (string (upcase (aref s i))) (substring s (1+ i))))

;;;; Single-typo suggestion heuristics
;;
;; Each of these mirrors one of Nuspell's `Suggester::*_suggest'
;; member functions: given WORD (assumed already a correctly-cased
;; input, or lower-cased by the caller) and BOX, it tries variations
;; of WORD that correct one commonly-made kind of typing mistake, and
;; pushes each one that turns out to be a correctly spelled word of
;; DICT onto BOX via `-add-sug-if-correct'.

(defun -uppercase-suggest (dict word box)
  (-add-sug-if-correct dict (upcase word) box))

(defun -try-rep-suggestion (dict candidate box)
  "Push CANDIDATE onto BOX if correct, or if every space-separated part is.
Mirrors Nuspell's `Suggester::try_rep_suggestion'."
  (unless (-add-sug-if-correct dict candidate box)
    (when (and (string-search " " candidate)
               (all (lambda (part) (-check-word dict part nil t))
                    (split-string candidate " " t)))
      (-sugs-push box candidate))))

(defun -rep-suggest (dict word box)
  "Try DICT's REP-table replacements (whole-word, prefix, suffix, any-place).
Mirrors Nuspell's `Suggester::rep_suggest'."
  (dolist (r (-dict-rep-whole dict))
    (when (string= word (car r))
      (-try-rep-suggestion dict (cdr r) box)))
  (dolist (r (-dict-rep-start dict))
    (when (string-prefix-p (car r) word)
      (-try-rep-suggestion dict (concat (cdr r) (substring word (length (car r)))) box)))
  (dolist (r (-dict-rep-end dict))
    (when (string-suffix-p (car r) word)
      (-try-rep-suggestion
       dict (concat (substring word 0 (- (length word) (length (car r)))) (cdr r)) box)))
  (dolist (r (-dict-rep-any dict))
    (let* ((from (car r))
           (to (cdr r))
           (i (and (not (string-empty-p from)) (string-search from word))))
      (while i
        (-try-rep-suggestion
         dict (concat (substring word 0 i) to (substring word (+ i (length from)))) box)
        (setq i (string-search from word (1+ i)))))))

(defun -map-suggest-try (dict word box i old-len rep budget)
  "Replace WORD[I, I+OLD-LEN) with REP, as one `-map-suggest' attempt."
  (when (-budget-consume budget)
    (let ((candidate (concat (substring word 0 i) rep (substring word (+ i old-len)))))
      (-add-sug-if-correct dict candidate box)
      (-map-suggest-1 dict candidate box (+ i (length rep)) budget))))

(defun -map-suggest-try-group (dict word box i budget grp)
  "Try GRP's character/string equivalences at WORD's position I.
See `-map-suggest-1'."
  (let* ((chars (-similarity-group-chars grp))
         (strings (-similarity-group-strings grp))
         (cp (aref word i))
         (j (cl-position cp chars)))
    (when j
      (dotimes (k (length chars))
        (unless (= k j)
          (-map-suggest-try dict word box i 1 (substring chars k (1+ k)) budget)))
      (dolist (r strings)
        (-map-suggest-try dict word box i 1 r budget)))
    (dolist (f strings)
      (when (and (<= (+ i (length f)) (length word))
                 (string= f (substring word i (+ i (length f)))))
        (dotimes (k (length chars))
          (-map-suggest-try dict word box i (length f) (substring chars k (1+ k)) budget))
        (dolist (r strings)
          (unless (string= r f)
            (-map-suggest-try dict word box i (length f) r budget)))))))

(defun -map-suggest-1 (dict word box i budget)
  "Try MAP (similarity-group) substitutions in WORD from index I onward."
  (when (and (< i (length word)) (> (-budget-n budget) 0))
    (dolist (grp (-dict-similarities dict))
      (-map-suggest-try-group dict word box i budget grp))
    (-map-suggest-1 dict word box (1+ i) budget)))

(defun -map-suggest (dict word box)
  "Try DICT's MAP character/string equivalences throughout WORD.
Mirrors Nuspell's `Suggester::map_suggest'."
  (-map-suggest-1 dict word box 0 (-budget-create (-max-attempts-for-long-algos dict word))))

(defun -adjacent-swap-suggest (dict word box)
  "Try swapping each pair of adjacent characters of WORD.
For 4- and 5-character words, also try the two-simultaneous-swap
combinations Nuspell additionally tries.  Mirrors Nuspell's
`Suggester::adjacent_swap_suggest'."
  (let ((len (length word)))
    (when (> len 0)
      (dotimes (i (1- len))
        (-add-sug-if-correct dict (-swap-adjacent word i) box))
      (pcase len
        (4 (-add-sug-if-correct dict (-swap-two-pairs word 0 2) box))
        (5 (-add-sug-if-correct dict (-swap-two-pairs word 0 3) box)
           (-add-sug-if-correct dict (-swap-two-pairs word 1 3) box))))))

(defun -distant-swap-suggest (dict word box)
  "Try swapping each pair of non-adjacent characters of WORD.
Mirrors Nuspell's `Suggester::distant_swap_suggest'."
  (let ((len (length word)))
    (when (> len 0)
      (let ((budget (-budget-create (-max-attempts-for-long-algos dict word))))
        (dotimes (i len)
          (dotimes (k (max 0 (- len i 2)))
            (when (-budget-consume budget)
              (-add-sug-if-correct dict (-swap-chars word i (+ i 2 k)) box))))))))

(defun -keyboard-suggest (dict word box)
  "Try case-toggling and keyboard-adjacent-key replacements in WORD.
DICT's KEY (keyboard closeness) groups are \"|\"-separated runs of
characters that are physically close together; this tries, for each
character of WORD, replacing it with its upper case, and with its
neighbors (if any) in every KEY group it belongs to.  Mirrors
Nuspell's `Suggester::keyboard_suggest'."
  (let ((kb (-dict-keyboard-closeness dict)))
    (dotimes (j (length word))
      (let* ((c (aref word j)) (upp (upcase c)))
        (when (/= upp c)
          (-add-sug-if-correct dict (-replace-char-at word j (string upp)) box))
        (dotimes (i (length kb))
          (when (eq (aref kb i) c)
            (when (and (> i 0) (/= (aref kb (1- i)) ?|))
              (-add-sug-if-correct dict (-replace-char-at word j (substring kb (1- i) i)) box))
            (when (and (< (1+ i) (length kb)) (/= (aref kb (1+ i)) ?|))
              (-add-sug-if-correct
               dict (-replace-char-at word j (substring kb (1+ i) (+ i 2))) box))))))))

(defun -extra-char-suggest (dict word box)
  "Try removing each single character of WORD.
Mirrors Nuspell's `Suggester::extra_char_suggest'."
  (dotimes (i (length word))
    (-add-sug-if-correct dict (concat (substring word 0 i) (substring word (1+ i))) box)))

(defun -forgotten-char-suggest (dict word box)
  "Try inserting each of DICT's TRY characters at each position of WORD.
Mirrors Nuspell's `Suggester::forgotten_char_suggest'."
  (let ((budget (-budget-create (-max-attempts-for-long-algos dict word)))
        (try-chars (-dict-try-chars dict)))
    (dotimes (t-idx (length try-chars))
      (let ((cp (substring try-chars t-idx (1+ t-idx))))
        (dotimes (i (1+ (length word)))
          (when (-budget-consume budget)
            (-add-sug-if-correct
             dict (concat (substring word 0 i) cp (substring word i)) box)))))))

(defun -move-char-suggest (dict word box)
  "Try moving each character of WORD to each other, non-adjacent position.
Mirrors Nuspell's `Suggester::move_char_suggest'."
  (let* ((len (length word))
         (budget (-budget-create (-max-attempts-for-long-algos dict word))))
    (dotimes (i1 len)
      (dotimes (k (max 0 (- len i1 2)))
        (when (-budget-consume budget)
          (-add-sug-if-correct dict (-move-char-forward word i1 (+ i1 2 k)) box))))
    (dotimes (i1 len)
      (dotimes (k (max 0 (- i1 2)))
        (when (-budget-consume budget)
          (-add-sug-if-correct dict (-move-char-backward word i1 (- i1 2 k)) box))))))

(defun -bad-char-suggest (dict word box)
  "Try replacing each character of WORD with each of DICT's TRY characters.
Mirrors Nuspell's `Suggester::bad_char_suggest'."
  (let ((budget (-budget-create (-max-attempts-for-long-algos dict word)))
        (try-chars (-dict-try-chars dict)))
    (dotimes (t-idx (length try-chars))
      (let ((tc (aref try-chars t-idx)))
        (dotimes (i (length word))
          (unless (eq (aref word i) tc)
            (when (-budget-consume budget)
              (-add-sug-if-correct dict (-replace-char-at word i (string tc)) box))))))))

(defun -doubled-two-chars-suggest (dict word box)
  "Try collapsing an \"XYXYX\" run in WORD down to \"XYX\".
Mirrors Nuspell's `Suggester::doubled_two_chars_suggest'."
  (let ((len (length word)))
    (dotimes (i (max 0 (- len 4)))
      (when (and (eq (aref word i) (aref word (+ i 2)))
                 (eq (aref word (1+ i)) (aref word (+ i 3)))
                 (eq (aref word i) (aref word (+ i 4))))
        (-add-sug-if-correct
         dict (concat (substring word 0 (+ i 3)) (substring word (+ i 5))) box)))))

(defun -two-words-suggest (dict word box)
  "Try splitting WORD into two dictionary words, space- or hyphen-joined.
Mirrors Nuspell's `Suggester::two_words_suggest'."
  (let ((len (length word)) (try-chars (-dict-try-chars dict)))
    (dotimes (i (max 0 (1- len)))
      (let ((w1 (substring word 0 (1+ i))) (w2 (substring word (1+ i))))
        (when (and (-check-simple-word dict w1 t) (-check-simple-word dict w2 t))
          (let ((candidate (concat w1 " " w2)))
            (unless (member candidate (-sugs-list box))
              (-sugs-push box candidate)))
          (when (and (> (1+ i) 1) (> (length w2) 1)
                     (or (cl-position ?a try-chars) (cl-position ?- try-chars)))
            (let ((candidate (concat w1 "-" w2)))
              (unless (member candidate (-sugs-list box))
                (-sugs-push box candidate)))))))))

(defun -suggest-low (dict word box)
  "Try every single-typo heuristic above for WORD, pushing hits onto BOX.
Return non-nil if a \"high quality\" suggestion was found this way, in
which case the caller may skip the much more expensive
`-ngram-suggest'.  Mirrors Nuspell's `Suggester::suggest_low'."
  (let ((old-count (-sugs-box-count box)))
    (-uppercase-suggest dict word box)
    (-rep-suggest dict word box)
    (-map-suggest dict word box)
    (let ((hq (or (/= old-count (-sugs-box-count box))
                  (and (-dict-similarities dict) (-check-word dict word nil t) t))))
      (-adjacent-swap-suggest dict word box)
      (-distant-swap-suggest dict word box)
      (-keyboard-suggest dict word box)
      (-extra-char-suggest dict word box)
      (-forgotten-char-suggest dict word box)
      (-move-char-suggest dict word box)
      (-bad-char-suggest dict word box)
      (-doubled-two-chars-suggest dict word box)
      (-two-words-suggest dict word box)
      hq)))

;;;; N-gram-based suggestion search
;;
;; When none of the heuristics above finds a "high quality" match,
;; `-ngram-suggest' instead scores every word of the dictionary by how
;; similar it looks to the misspelled word (by shared substrings and
;; letters in matching positions, roughly), keeps the 100 best-scoring
;; roots, expands each of those through its possible affixes, scores
;; those expansions too, and returns the best-scoring ones.  This is
;; much more expensive than the heuristics above (it visits every
;; dictionary word) but catches corrections that are not just one
;; small edit away from the misspelling.  Mirrors Nuspell's
;; `Suggester::ngram_suggest' and the free functions above it.

(defconst -max-suggestions 16
  "Hard cap on the number of suggestions `-ngram-suggest' may add.
Mirrors Nuspell's `Aff_Data::MAX_SUGGESTIONS'.")

(defun -kgrams (n a)
  "Return the k-grams of A, for k=1..N, as a vector of vectors.
Element K-1 is the vector of A's K-grams, in order.  The n-gram
similarity functions below accept this as an optional precomputed
A-GRAMS argument: `-ngram-suggest' compares the same misspelled word
against every dictionary word, so extracting its k-grams once per
call, rather than once per comparison, avoids redoing that work (and
its allocations) tens of thousands of times."
  (let* ((len (length a))
         (n (min n len))
         (result (make-vector n nil)))
    (dotimes (k0 n)
      (let ((grams (make-vector (- len k0) nil)))
        (dotimes (i (length grams))
          (aset grams i (substring a i (+ i k0 1))))
        (aset result k0 grams)))
    result))

(defun -ngram-similarity-low-level (n a b &optional a-grams)
  "Return a raw k-gram-overlap similarity score of A against B, k=1..N.
A-GRAMS, if non-nil, is `-kgrams' of A for some N' >= N.
Mirrors Nuspell's `ngram_similarity_low_level'."
  (let ((grams (or a-grams (-kgrams n a)))
        (n (min n (length a)))
        (score 0)
        (k0 0))
    (while (< k0 n)
      (let ((v (aref grams k0)) (k-score 0))
        (dotimes (i (length v))
          (when (string-search (aref v i) b)
            (setq k-score (1+ k-score))))
        (setq score (+ score k-score))
        (setq k0 (if (< k-score 2) n (1+ k0)))))
    score))

(defun -ngram-similarity-weighted-low-level (n a b &optional a-grams)
  "Like `-ngram-similarity-low-level', but penalizing non-matching k-grams.
A-GRAMS is as in `-ngram-similarity-low-level'.
Mirrors Nuspell's `ngram_similarity_weighted_low_level'."
  (let ((grams (or a-grams (-kgrams n a)))
        (score 0))
    (dotimes (k0 (min n (length a)))
      (let* ((v (aref grams k0)) (last (1- (length v))) (k-score 0))
        (dotimes (i (length v))
          (if (string-search (aref v i) b)
              (setq k-score (1+ k-score))
            (setq k-score (1- k-score))
            (when (or (= i 0) (= i last))
              (setq k-score (1- k-score)))))
        (setq score (+ score k-score))))
    score))

(defun -ngram-similarity-longer-worse (n a b &optional a-grams)
  "`-ngram-similarity-low-level', penalized when B is much longer than A.
Mirrors Nuspell's `ngram_similarity_longer_worse'."
  (if (string-empty-p b)
      0
    (let ((score (-ngram-similarity-low-level n a b a-grams))
          (d (- (- (length b) (length a)) 2)))
      (if (> d 0) (- score d) score))))

(defun -ngram-similarity-any-mismatch (n a b &optional a-grams)
  "`-ngram-similarity-low-level', penalized when A and B differ much in length.
Mirrors Nuspell's `ngram_similarity_any_mismatch'."
  (if (string-empty-p b)
      0
    (let ((score (-ngram-similarity-low-level n a b a-grams))
          (d (- (abs (- (length b) (length a))) 2)))
      (if (> d 0) (- score d) score))))

(defun -ngram-similarity-any-mismatch-weighted (n a b &optional a-grams)
  "Weighted version of `-ngram-similarity-any-mismatch'.
Mirrors Nuspell's `ngram_similarity_any_mismatch_weighted'."
  (if (string-empty-p b)
      0
    (let ((score (-ngram-similarity-weighted-low-level n a b a-grams))
          (d (- (abs (- (length b) (length a))) 2)))
      (if (> d 0) (- score d) score))))

(defun -left-common-substring-length (a b)
  "Return the length of A and B's common prefix.
The first character is matched case-insensitively; the rest are not.
Mirrors Nuspell's `left_common_substring_length'."
  (if (or (string-empty-p a) (string-empty-p b))
      0
    (if (and (/= (aref a 0) (aref b 0)) (/= (aref a 0) (downcase (aref b 0))))
        0
      (let ((n (min (length a) (length b))) (i 1))
        (while (and (< i n) (eq (aref a i) (aref b i)))
          (setq i (1+ i)))
        i))))

(defun -longest-common-subsequence-length (a b)
  "Return the length of the longest common subsequence of A and B.
Mirrors Nuspell's `longest_common_subsequence_length'."
  (let ((state (make-vector (max (length b) 1) 0)))
    (dotimes (i (length a))
      (let ((row1-prev 0) (row2-prev 0))
        (dotimes (j (length b))
          (let ((row1-current (aref state j)))
            (if (eq (aref a i) (aref b j))
                (aset state j (1+ row1-prev))
              (aset state j (max row1-current row2-prev)))
            (setq row1-prev row1-current)
            (setq row2-prev (aref state j))))))
    (if (zerop (length b)) 0 (aref state (1- (length b))))))

(defun -count-eq-chars-at-same-pos (a b)
  "Return (NUM . IS-SWAP) for A and B; see Nuspell's `count_eq_chars_at_same_pos'.
NUM is how many characters are equal at the same position in both; when
A and B are the same length and exactly 2 positions differ, IS-SWAP
says whether those two differing characters are a transposition of
each other."
  (let* ((n (min (length a) (length b))) (count 0))
    (dotimes (i n)
      (when (eq (aref a i) (aref b i)) (setq count (1+ count))))
    (let (is-swap m1 m2)
      (when (and (= (length a) (length b)) (= (- n count) 2))
        (-dolist-until (i (number-sequence 0 (1- n)))
          (unless (eq (aref a i) (aref b i))
            (if m1 (progn (setq m2 i) t) (progn (setq m1 i) nil))))
        (setq is-swap (and (eq (aref a m1) (aref b m2)) (eq (aref a m2) (aref b m1)))))
      (cons count is-swap))))

(defun -expand-root-word-for-ngram (dict root flags wrong)
  "Return every word form derivable from ROOT/FLAGS, for `-ngram-suggest'.
WRONG is the original (lower-cased) misspelled word; affixes whose
appending text is absent from it are skipped, as a pruning
optimization only.  Mirrors Nuspell's
`Suggester::expand_root_word_for_ngram'."
  (let ((base (unless (-flags-contain flags (-dict-need-affix-flag dict)) (list root)))
        suffixed cross-ok combo prefixed)
    (when flags
      (dolist (sfx (-all-affix-table-entries (-dict-suffix-table dict)))
        (when (and (-cross-valid-p flags (-suffix-flag sfx))
                   (not (-outer-suffix-not-valid-p dict :full-word sfx))
                   (not (-circumfix-flags-p dict (-suffix-cont-flags sfx)))
                   (string-suffix-p (-suffix-stripping sfx) root)
                   (-suffix-check-condition sfx root)
                   (or (string-empty-p (-suffix-appending sfx))
                       (string-suffix-p (-suffix-appending sfx) wrong)))
          (push (-suffix-from-root sfx root) suffixed)
          (push (-suffix-cross-product sfx) cross-ok)))
      (setq suffixed (nreverse suffixed) cross-ok (nreverse cross-ok))
      (cl-mapc
       (lambda (root-sfx ok)
         (when ok
           (dolist (pfx (-all-affix-table-entries (-dict-prefix-table dict)))
             (when (and (-cross-valid-p flags (-prefix-flag pfx))
                        (not (-outer-prefix-not-valid-p dict :full-word pfx))
                        (not (-circumfix-flags-p dict (-prefix-cont-flags pfx)))
                        (string-prefix-p (-prefix-stripping pfx) root-sfx)
                        (-prefix-check-condition pfx root-sfx)
                        (or (string-empty-p (-prefix-appending pfx))
                            (string-prefix-p (-prefix-appending pfx) wrong)))
               (push (-prefix-from-root pfx root-sfx) combo)))))
       suffixed cross-ok)
      (dolist (pfx (-all-affix-table-entries (-dict-prefix-table dict)))
        (when (and (-cross-valid-p flags (-prefix-flag pfx))
                   (not (-outer-prefix-not-valid-p dict :full-word pfx))
                   (not (-circumfix-flags-p dict (-prefix-cont-flags pfx)))
                   (string-prefix-p (-prefix-stripping pfx) root)
                   (-prefix-check-condition pfx root)
                   (or (string-empty-p (-prefix-appending pfx))
                       (string-prefix-p (-prefix-appending pfx) wrong)))
          (push (-prefix-from-root pfx root) prefixed))))
    (append base suffixed combo prefixed)))

(defun -heap-replace (heap entry)
  "Replace the root of HEAP, a `value<'-ordered min-heap vector, with ENTRY.
The root, HEAP's worst entry, is discarded and ENTRY sifted down to its
place.  Like Python's `heapq.heapreplace'."
  (let ((n (length heap)) (i 0) (done nil))
    (while (not done)
      (let ((child (1+ (* 2 i))))
        (when (and (< (1+ child) n)
                   (value< (aref heap (1+ child)) (aref heap child)))
          (setq child (1+ child)))
        (if (and (< child n) (value< (aref heap child) entry))
            (progn (aset heap i (aref heap child))
                   (setq i child))
          (setq done t))))
    (aset heap i entry)))

(defun -ngram-candidates (dict)
  "Return the dictionary entries `-ngram-suggest' may suggest from.
This is a vector of (WORD LOWER FLAGS), one per homonym entry of DICT's
words table that is not forbidden, hidden, NOSUGGEST or ONLYINCOMPOUND,
in the table's order; LOWER is WORD's lowercased form.  Which entries
qualify depends only on DICT, so this is computed on first use and
cached, rather than re-filtered on every suggestion; `-dict-add-word'
and `-dict-remove-word' clear the cache."
  (or (-dict-ngram-candidates dict)
      (let (acc)
        (maphash
         (lambda (word entry)
           (dolist (flags (cdr entry))
             (unless (or (-flags-contain flags (-dict-forbiddenword-flag dict))
                         (-hidden-homonym-p flags)
                         (-flags-contain flags (-dict-nosuggest-flag dict))
                         (-flags-contain flags (-dict-compound-onlyin-flag dict)))
               (push (list word (car entry) flags) acc))))
         (-dict-words dict))
        (setf (-dict-ngram-candidates dict) (vconcat (nreverse acc))))))

(defun -ngram-suggest (dict wrong-word box)
  "Add n-gram-similarity-based suggestions for WRONG-WORD to BOX.
WRONG-WORD is assumed already lower-cased.  Mirrors Nuspell's
`Suggester::ngram_suggest'."
  (let ((wrong-grams (-kgrams (length wrong-word) wrong-word))
        (candidates (-ngram-candidates dict))
        ;; Keep the best 100 roots in a `-heap-replace' heap of (SCORE
        ;; IDX . VALUE).  IDX increases, so it breaks ties on SCORE in
        ;; favor of the later offer, as a stable sort of a `push'ed list
        ;; would, and a newcomer beats the root iff its SCORE is at least
        ;; as high.  The filler entries have a nil VALUE.
        (top-roots (make-vector 100 (list most-negative-fixnum 0)))
        roots)
    (dotimes (i (length candidates))
      (let* ((c (aref candidates i))
             (score (+ (-left-common-substring-length wrong-word (car c))
                       (-ngram-similarity-longer-worse 3 wrong-word (cadr c) wrong-grams))))
        (when (>= score (car (aref top-roots 0)))
          (-heap-replace top-roots (cons score (cons i c))))))
    ;; Each root is (SCORE WORD LOWER FLAGS), sharing its tail with the
    ;; `-ngram-candidates' entry.
    (setq roots (mapcan (lambda (e) (and (cddr e) (list (cons (car e) (cddr e)))))
                        (sort top-roots :reverse t :in-place t)))

    (let ((threshold 0))
      (dolist (k '(1 2 3))
        (let ((mangled wrong-word) (i k))
          (while (< i (length mangled))
            ;; Can't `aset' an arbitrary character in place: Emacs
            ;; strings are not necessarily uniform-width internally,
            ;; so replacing e.g. a 2-byte-wide character with a
            ;; 1-byte-wide "*" can fail; rebuild instead.
            (setq mangled (concat (substring mangled 0 i) "*" (substring mangled (1+ i))))
            (setq i (+ i 4)))
          (setq threshold (+ threshold
                             (-ngram-similarity-any-mismatch
                              (length wrong-word) wrong-word mangled wrong-grams)))))
      (setq threshold (/ threshold 3))

      (let ((top-guesses (make-vector 200 (list most-negative-fixnum 0)))
            (guess-idx 0)
            guesses)
        (dolist (r roots)
          (pcase-let ((`(,_ ,word ,word-lower ,flags) r))
            (dolist (expanded (-expand-root-word-for-ngram dict word flags wrong-word))
              (let* ((lower (if (eq expanded word) word-lower (downcase expanded)))
                     (score (+ (-left-common-substring-length wrong-word expanded)
                               (-ngram-similarity-any-mismatch
                                (length wrong-word) wrong-word lower wrong-grams))))
                (when (and (>= score threshold)
                           (>= score (car (aref top-guesses 0))))
                  (-heap-replace top-guesses (cons score (cons (incf guess-idx) expanded))))))))
        (setq guesses (mapcan (lambda (e) (and (cddr e) (list (cons (car e) (cddr e)))))
                              (sort top-guesses :reverse t :in-place t)))

        (-dolist-until (g guesses)
          (pcase-let ((`(,score . ,word) g))
            (let* ((lower (downcase word))
                   (lcs (-longest-common-subsequence-length wrong-word lower)))
              (if (and (= (length wrong-word) (length lower))
                       (= (length wrong-word) lcs))
                  (progn (setcar g (+ score 2000)) t)
                (let* ((ngram2 (+ (-ngram-similarity-any-mismatch-weighted 2 wrong-word lower wrong-grams)
                                  (-ngram-similarity-any-mismatch-weighted 2 lower wrong-word)))
                       (ngram4 (-ngram-similarity-any-mismatch 4 wrong-word lower wrong-grams))
                       (left-common (-left-common-substring-length wrong-word lower))
                       (eq-info (-count-eq-chars-at-same-pos wrong-word lower))
                       (new-score (- (* 2 lcs) (abs (- (length wrong-word) (length lower))))))
                  (setq new-score (+ new-score left-common ngram2 ngram4))
                  (when (/= (car eq-info) 0) (setq new-score (1+ new-score)))
                  (when (cdr eq-info) (setq new-score (+ new-score 10)))
                  (when (< (* 5 ngram2)
                           (* (+ (length wrong-word) (length lower))
                              (- 10 (-dict-max-diff-factor dict))))
                    (setq new-score (- new-score 1000)))
                  (setcar g new-score)
                  nil)))))
        (setq guesses (sort guesses (lambda (a b) (> (car a) (car b)))))

        (let* ((more-selective (and guesses (> (car (car guesses)) 1000)))
               (old-num-sugs (-sugs-box-count box))
               (max-sug (min -max-suggestions (+ old-num-sugs (-dict-max-ngram-suggestions dict)))))
          (-dolist-until (g guesses)
            (pcase-let ((`(,score . ,word) g))
              (cond
               ((= (-sugs-box-count box) max-sug) t)
               ((and more-selective (<= score 1000)) t)
               ((and (< score -100)
                     (or (/= old-num-sugs (-sugs-box-count box)) (-dict-only-max-diff dict)))
                t)
               ((any (lambda (sug) (string-search sug word)) (-sugs-list box))
                (< score -100))
               (t (-sugs-push box word) nil)))))))))

;;;; Top-level suggestion entry point

(defun -fix-space-casing-in-sugs (box word)
  "Title-case the second word of BOX's space-containing suggestions.
Any suggestion of BOX that contains a space and whose part after that
space does not already end in the same case as WORD has that part's
first character upcased, and is moved to the front of BOX.  Used for
CAMEL/PASCAL-cased input.  Loosely mirrors the casing-fix loop in
Nuspell's `Suggester::suggest_priv' (ties among several qualifying
suggestions are broken differently; see the Commentary at the top of
this file)."
  (let (fixed rest)
    (dolist (sug (-sugs-list box))
      (let ((space (string-search " " sug)))
        (if (not space)
            (push sug rest)
          (let* ((i (1+ space)) (len (- (length sug) i)))
            (if (and (<= len (length word))
                     (string= (substring sug i) (substring word (- (length word) len))))
                (push sug rest)
              (push (concat (substring sug 0 i) (-to-title-char-at (substring sug i) 0))
                    fixed))))))
    (-sugs-box-set box (append (nreverse fixed) (nreverse rest)))))

(defun -fix-keepcase-sug (dict s)
  "Return S, possibly lower- or title-cased, or nil to drop it.
Used for INIT_CAPITAL/ALL_CAPITAL input to a KEEPCASE/FORBIDDENWORD
dictionary.  Mirrors the inline `is_ok' lambda in Nuspell's
`Suggester::suggest_priv'."
  (cond
   ((string-search " " s) s)
   ((-spell dict s) s)
   ((-spell dict (downcase s)) (downcase s))
   (t (let ((title (-to-title s))) (and (-spell dict title) title)))))

(defun -dedup-preserve-first (list)
  "Return LIST with duplicates removed, keeping each value's first occurrence."
  (let (seen result)
    (dolist (s list)
      (unless (member s seen)
        (push s seen)
        (push s result)))
    (nreverse result)))

(defun -suggest-dash-split (dict box word)
  "If WORD contains a hyphen, also try correcting it piece by piece.
For each \"-\"-separated piece of WORD that is not itself a correctly
spelled word, recursively suggest corrections for that piece alone and
splice each one into WORD in its place, pushing the result onto BOX
unless it is a forbidden word.  Skipped entirely if BOX already has a
suggestion containing a hyphen.  Mirrors the dash-splitting block in
Nuspell's `Suggester::suggest_priv'."
  (when (and (string-search "-" word)
             (not (any (lambda (s) (string-search "-" s)) (-sugs-list box))))
    (let ((i 0) (len (length word)))
      (while (<= i len)
        (let* ((dash (string-search "-" word i))
               (j (or dash len))
               (part (substring word i j)))
          (unless (-spell dict part)
            (dolist (sub (-suggest-priv dict part))
              (let* ((candidate (concat (substring word 0 i) sub (substring word j)))
                     (flg (-check-word dict candidate)))
                (when (or (not flg) (not (-flags-contain (cdr flg) (-dict-forbiddenword-flag dict))))
                  (-sugs-push box candidate)))))
          (setq i (if dash (1+ j) (1+ len))))))))

(defun -suggest-for-casing (dict word casing)
  "Return suggestions for WORD, dispatching on its CASING.
Called from `-suggest-priv' once WORD has had any trailing abbreviation
dots trimmed and its early-exit special case ruled out; CASING is
`-classify-casing' of WORD."
  (let ((box (-sugs-box-create)) (hq-sugs nil))
    (pcase casing
      ('small
       (setq hq-sugs (-suggest-low dict word box)))
      ('init-capital
       (setq hq-sugs (-suggest-low dict word box))
       (setq hq-sugs (or (-suggest-low dict (downcase word) box) hq-sugs)))
      ((or 'camel 'pascal)
       (setq hq-sugs (-suggest-low dict word box))
       (let ((dot (string-search "." word)))
         (when dot
           (let ((after (substring word (1+ dot))))
             (when (eq (-classify-casing after) 'init-capital)
               (-sugs-push-first box (concat (substring word 0 (1+ dot)) " " after))))))
       (when (eq casing 'pascal)
         (let ((buffer (concat (downcase (substring word 0 1)) (substring word 1))))
           (when (-spell dict buffer) (-sugs-push-first box buffer))
           (setq hq-sugs (or (-suggest-low dict buffer box) hq-sugs))))
       (let ((lower (downcase word)))
         (when (-spell dict lower) (-sugs-push-first box lower))
         (setq hq-sugs (or (-suggest-low dict lower box) hq-sugs)))
       (when (eq casing 'pascal)
         (let ((title (-to-title word)))
           (when (-spell dict title) (-sugs-push-first box title))
           (setq hq-sugs (or (-suggest-low dict title box) hq-sugs))))
       (-fix-space-casing-in-sugs box word))
      ('all-capital
       (let ((lower (downcase word)))
         (when (and (/= 0 (-dict-keepcase-flag dict)) (-spell dict lower))
           (-sugs-push-first box lower))
         (setq hq-sugs (-suggest-low dict lower box)))
       (let ((title (-to-title word)))
         (setq hq-sugs (or (-suggest-low dict title box) hq-sugs)))
       (-sugs-box-set box (mapcar #'upcase (-sugs-list box)))))

    (when (and (not hq-sugs) (/= 0 (-dict-max-ngram-suggestions dict)))
      (let* ((buffer (if (eq casing 'small) word (downcase word)))
             (old-list (-sugs-list box))
             (old-len (length old-list)))
        (-ngram-suggest dict buffer box)
        (when (eq casing 'all-capital)
          (-sugs-box-set
           box (append old-list (mapcar #'upcase (nthcdr old-len (-sugs-list box))))))))

    (-suggest-dash-split dict box word)

    (when (memq casing '(init-capital pascal))
      (-sugs-box-set box (mapcar (lambda (s) (-to-title-char-at s 0)) (-sugs-list box))))

    (when (and (memq casing '(init-capital all-capital))
               (or (/= 0 (-dict-keepcase-flag dict)) (/= 0 (-dict-forbiddenword-flag dict))))
      (-sugs-box-set box (delq nil (mapcar (lambda (s) (-fix-keepcase-sug dict s)) (-sugs-list box)))))

    (-sugs-box-set box (-dedup-preserve-first (-sugs-list box)))

    (mapcar (lambda (s) (-substr-replace (-dict-output-oconv dict) s)) (-sugs-list box))))

(defun -suggest-priv (dict word)
  "Return VIBE-SPELL-SUGGEST's suggestions for WORD, as a plain list.
Applies DICT's input ICONV to WORD and trims its trailing abbreviation
dots, if any, then handles Nuspell's `compound_force_uppercase'
early-exit special case before dispatching to `-suggest-for-casing'.
Mirrors Nuspell's `Suggester::suggest_priv'."
  (unless (string-empty-p word)
    (let* ((word (-substr-replace (-dict-input-iconv dict) word))
           (abbreviation (eq (aref word (1- (length word))) ?.))
           (word (if abbreviation (substring word 0 (-string-last-non-dot word)) word)))
      (unless (string-empty-p word)
        (let ((casing (-classify-casing word)))
          (if (and (eq casing 'small)
                   (/= 0 (-dict-compound-force-uppercase dict))
                   (-check-compound dict word t))
              (list (-to-title word))
            (-suggest-for-casing dict word casing)))))))

;;;; Dictionary (.dic) parsing

(defun -parse-dic-line (line)
  "Parse a .dic LINE into (WORD . FLAGS-STRING-OR-NIL), or nil.
Morphological fields, if any, are discarded.  A backslash-escaped
slash within the word is unescaped and kept as a literal character."
  (when (and (> (length line) 0) (eq (aref line (1- (length line))) ?\r))
    (setq line (substring line 0 -1)))
  (let ((i 0) (n (length line)) (word-chars nil) flags-str (done nil))
    (while (and (< i n) (not done))
      (let ((c (aref line i)))
        (cond
         ((and (eq c ?\\) (< (1+ i) n) (eq (aref line (1+ i)) ?/))
          (push ?/ word-chars)
          (setq i (+ i 2)))
         ;; A "/" at the very start of the line is a literal
         ;; character, not a flags separator -- this is what lets a
         ;; word like "/usr/share/myspell/" start with "/" without
         ;; escaping it, and is how a bare "/" is written as a word.
         ((and (eq c ?/) (= i 0))
          (push c word-chars)
          (setq i (1+ i)))
         ((eq c ?/)
          (let ((j (1+ i)))
            (while (and (< j n) (not (memq (aref line j) '(?\s ?\t))))
              (setq j (1+ j)))
            (setq flags-str (substring line (1+ i) j)))
          (setq done t))
         ((memq c '(?\s ?\t))
          (setq done t))
         (t
          (push c word-chars)
          (setq i (1+ i))))))
    (if word-chars
        (cons (concat (nreverse word-chars)) flags-str)
      nil)))

(defun -add-homonym (table word flags)
  "Append a homonym entry with FLAGS to WORD's entry in words TABLE.
A new entry also records WORD's lowercased form, computed once here
rather than on every `-ngram-suggest' call, which needs it for every
dictionary word; it is WORD itself, with no allocation, when WORD is
already all lowercase."
  (let ((entry (gethash word table)))
    ;; Appended, not pushed: homonyms must stay in insertion order, since
    ;; a later one can mask an earlier one for a flag (e.g. FORBIDDENWORD)
    ;; that only some of them carry -- see `-check-simple-word'.
    (if entry
        (setcdr entry (append (cdr entry) (list flags)))
      (puthash word
               (list (if (let ((case-fold-search nil))
                           (string-match-p "[[:upper:]]" word))
                         (downcase word)
                       word)
                     flags)
               table))))

(defun -dict-add-word (dict word flags)
  "Add dictionary entry WORD/FLAGS to DICT.
If WORD is ALL-CAPITAL (with non-empty FLAGS), PASCAL or CAMEL cased,
also add a title-cased duplicate flagged as a hidden homonym, so that
checking the title-cased form of a word whose canonical form has
different casing (e.g. an acronym) falls through to other checks
rather than accepting it outright; see `-hidden-homonym-p'."
  (let* ((table (-dict-words dict))
         (casing (-classify-casing word)))
    (setf (-dict-ngram-candidates dict) nil)
    (-add-homonym table word flags)
    (when (or (and (eq casing 'all-capital) flags)
              (memq casing '(pascal camel)))
      (unless (-flags-contain flags (-dict-forbiddenword-flag dict))
        (let ((title (-to-title word)))
          ;; -1 marks this entry as a hidden homonym; see `-hidden-homonym-p'.
          (-add-homonym table title (cons -1 flags)))))))

(defun -dict-remove-word (dict word)
  "Remove one nil-flags entry of WORD from DICT's words table.
This is the counterpart of `-dict-add-word' called with nil FLAGS,
which is how `vibe-spell-add-word' always calls it; an entry with
non-nil flags, e.g. one loaded from the dictionary's own .dic file, is
left alone even if WORD also has a nil-flags entry alongside it.  Any
hidden-homonym duplicate `-dict-add-word' may have added for WORD
elsewhere is left in place; it has no effect on checking, only on
whether that title-cased spelling is skipped when found directly (see
`-hidden-homonym-p'), so an unused leftover one is harmless.

Return non-nil if such an entry was found and removed."
  (let* ((table (-dict-words dict))
         (entry (gethash word table)))
    (when (memq nil (cdr entry))
      (setf (-dict-ngram-candidates dict) nil)
      (let ((new-list (cl-remove nil (cdr entry) :count 1)))
        (if new-list
            (setcdr entry new-list)
          (remhash word table)))
      t)))

(defun -parse-dic (dict file)
  "Parse .dic FILE into DICT."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    ;; The first line is an approximate word count; use it to presize the
    ;; words table so it isn't repeatedly grown and rehashed as entries
    ;; are added below.
    (let ((count (string-to-number (buffer-substring-no-properties
                                    (pos-bol) (pos-eol)))))
      (setf (-dict-words dict) (make-hash-table :test 'equal :size (max count 65))))
    (forward-line 1)
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties (pos-bol) (pos-eol)))
             (parsed (-parse-dic-line line)))
        (when parsed
          (let* ((word (-erase-chars (car parsed) (-dict-ignored-chars dict)))
                 (flags-str (cdr parsed))
                 (flags (and flags-str (-decode-flags-possible-alias dict flags-str))))
            (unless (string-empty-p word)
              (-dict-add-word dict word flags)))))
      (forward-line 1))))

;;;; Affix (.aff) parsing

(defun -chomp-cr (line)
  (if (and (> (length line) 0) (eq (aref line (1- (length line))) ?\r))
      (substring line 0 -1)
    line))

(defun -array-command-step (cmd-counts command count-token)
  "Advance array-COMMAND state in CMD-COUNTS.
Return `header' when this line declared the array's element count,
`data' when this is a data line to be handled by the caller, or nil
for a data line beyond the declared count (ignored)."
  (let ((state (gethash command cmd-counts 'unset)))
    (cond
     ((eq state 'unset)
      (puthash command (string-to-number count-token) cmd-counts)
      'header)
     ((> state 0)
      (puthash command (1- state) cmd-counts)
      'data)
     (t nil))))

(defun -parse-compound-rule-charwise (str)
  "Parse single-char/UTF-8 COMPOUNDRULE text STR into (FLAG . QUANT) pairs."
  (let ((i 0) (n (length str)) (result nil))
    (while (< i n)
      (let ((flag (aref str i)))
        (setq i (1+ i))
        (let (quant)
          (when (and (< i n) (memq (aref str i) '(?? ?*)))
            (setq quant (aref str i))
            (setq i (1+ i)))
          (push (cons flag quant) result))))
    (nreverse result)))

(defun -parse-compound-rule-grouped (str group-len)
  "Parse long/num COMPOUNDRULE text STR, flags given as \"(...)\" groups."
  (let ((i 0) (n (length str)) (result nil))
    (while (< i n)
      (unless (eq (aref str i) ?\()
        (error "vibe-spell: invalid COMPOUNDRULE %S" str))
      (setq i (1+ i))
      (let ((start i))
        (while (and (< i n) (not (eq (aref str i) ?\))))
          (setq i (1+ i)))
        (let ((flag (if (eq group-len 2)
                        (logior (ash (aref str start) 8) (aref str (1+ start)))
                      (string-to-number (substring str start i)))))
          (setq i (1+ i))                ; skip ")"
          (let (quant)
            (when (and (< i n) (memq (aref str i) '(?? ?*)))
              (setq quant (aref str i))
              (setq i (1+ i)))
            (push (cons flag quant) result)))))
    (nreverse result)))

(defun -parse-compound-rule-line (str flag-type)
  (pcase flag-type
    ((or 'single-char 'utf-8) (-parse-compound-rule-charwise str))
    ('long (-parse-compound-rule-grouped str 2))
    ('num (-parse-compound-rule-grouped str nil))))

(defun -partition-break-patterns (patterns)
  "Partition raw BREAK pattern strings into (START END MIDDLE) lists.
A pattern beginning with \"^\" is a start-of-word pattern (with the
\"^\" stripped); a remaining pattern ending with \"$\" is an
end-of-word pattern (with the \"$\" stripped); everything else is a
middle-of-word pattern.  Empty patterns, and the bare patterns \"^\"
and \"$\", are dropped, matching Nuspell's `Break_Table::order_entries'."
  (let (start end middle)
    (dolist (p patterns)
      (cond
       ((or (string-empty-p p) (string= p "^") (string= p "$")))
       ((eq (aref p 0) ?^) (push (substring p 1) start))
       ((eq (aref p (1- (length p))) ?$) (push (substring p 0 -1) end))
       (t (push p middle))))
    (list (nreverse start) (nreverse end) (nreverse middle))))

(defun -compute-compound-rules-all-flags (rules)
  (let (flags)
    (dolist (pattern rules)
      (dolist (node pattern)
        (cl-pushnew (car node) flags)))
    flags))

;;;; Suggestion-related affix tables (REP, MAP, ICONV/OCONV)

(defun -parse-similarity-group (str)
  "Parse one MAP line STR into a `-similarity-group'.
Text outside parentheses becomes equivalent single characters; a
single character inside parentheses is added the same way, but a
multi-character parenthesized run is instead added as a whole
equivalent string.  Mirrors Nuspell's `Similarity_Group::parse'."
  (let ((i 0) (n (length str)) chars strings (done nil))
    (while (and (< i n) (not done))
      (let ((open (cl-position ?\( str :start i)))
        (if (not open)
            (progn (setq chars (concat chars (substring str i))) (setq done t))
          (setq chars (concat chars (substring str i open)))
          (let ((close (cl-position ?\) str :start (1+ open))))
            (if (not close)
                (setq done t)
              (let ((len (- close open 1)))
                (cond
                 ((= len 1) (setq chars (concat chars (substring str (1+ open) close))))
                 ((> len 1) (push (substring str (1+ open) close) strings)))
                (setq i (1+ close))))))))
    (-similarity-group-create :chars chars :strings (nreverse strings))))

(defun -build-replacement-table (pairs)
  "Partition REP PAIRS, each (FROM . TO), by anchor into 4 lists.
Return (WHOLE-WORD START-WORD END-WORD ANY-PLACE), each a list of
\(FROM . TO) with any \"^\"/\"$\" anchor stripped from FROM.  Entries
whose FROM is empty or a bare \"^\"/\"$\" are dropped.  Mirrors
Nuspell's `Replacement_Table::order_entries'."
  (let (whole start end any)
    (dolist (p pairs)
      (let ((from (car p)) (to (cdr p)))
        (unless (or (string-empty-p from) (member from '("^" "$")))
          (let* ((starts (eq (aref from 0) ?^))
                 (ends (eq (aref from (1- (length from))) ?$))
                 (from (cond ((and starts ends) (substring from 1 -1))
                             (starts (substring from 1))
                             (ends (substring from 0 -1))
                             (t from))))
            (cond
             ((and starts ends) (push (cons from to) whole))
             (starts (push (cons from to) start))
             (ends (push (cons from to) end))
             (t (push (cons from to) any)))))))
    (list (nreverse whole) (nreverse start) (nreverse end) (nreverse any))))

(defun -build-substr-replacer (pairs)
  "Build an ICONV/OCONV substitution table from (FROM . TO) PAIRS.
Entries with an empty FROM are dropped; see `-substr-replace'."
  (cl-remove-if (lambda (p) (string-empty-p (car p))) pairs))

(defun -substr-replacer-find (table s i)
  "Return the (FROM . TO) entry of TABLE whose FROM is the longest match
at position I of S, or nil if none matches there."
  (let (best)
    (dolist (p table)
      (let ((from (car p)))
        (when (and (<= (+ i (length from)) (length s))
                   (or (not best) (> (length from) (length (car best))))
                   (string= from (substring s i (+ i (length from)))))
          (setq best p))))
    best))

(defun -substr-replace (table s)
  "Apply TABLE's longest-match-at-each-position substitutions throughout S.
Mirrors Nuspell's `Substr_Replacer', used for ICONV/OCONV."
  (if (null table)
      s
    (let ((i 0) (len (length s)) parts)
      (while (< i len)
        (let ((m (-substr-replacer-find table s i)))
          (if m
              (progn (push (cdr m) parts) (setq i (+ i (length (car m)))))
            (push (substring s i (1+ i)) parts)
            (setq i (1+ i)))))
      (apply #'concat (nreverse parts)))))

(defun -parse-aff-affix-line (dict command args cmd-affix)
  "Process one PFX/SFX line.  Return a new affix struct, or nil."
  (let* ((flag (-decode-first-flag (car args) (-dict-flag-type dict)))
         (key (cons command flag))
         (state (gethash key cmd-affix)))
    (if (null state)
        (progn
          (puthash key (cons (string= (nth 1 args) "Y") (string-to-number (nth 2 args)))
                   cmd-affix)
          nil)
      (let ((cross (car state)) (remaining (cdr state)))
        (when (> remaining 0)
          (puthash key (cons cross (1- remaining)) cmd-affix)
          (let* ((stripping0 (nth 1 args))
                 (stripping (if (string= stripping0 "0") "" stripping0))
                 (appending-raw (nth 2 args))
                 (slash (cl-position ?/ appending-raw))
                 (appending0 (if slash (substring appending-raw 0 slash) appending-raw))
                 (appending (if (string= appending0 "0") "" appending0))
                 (cont-flags (and slash (-decode-flags-possible-alias
                                         dict (substring appending-raw (1+ slash)))))
                 (condition (-compile-condition (or (nth 3 args) "."))))
            (if (string= command "PFX")
                (-prefix-create :flag flag :cross-product cross
                                :stripping stripping :appending appending
                                :cont-flags cont-flags :condition condition)
              (-suffix-create :flag flag :cross-product cross
                              :stripping stripping :appending appending
                              :cont-flags cont-flags :condition condition))))))))

(defun -parse-aff (dict file)
  "Parse .aff FILE into DICT."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((prefixes nil) (suffixes nil)
          (break-patterns nil) (break-seen nil)
          (compound-rules nil) (flag-aliases nil)
          (rep-pairs nil) (similarities nil)
          (iconv-pairs nil) (oconv-pairs nil)
          (cmd-affix (make-hash-table :test 'equal))
          (cmd-counts (make-hash-table :test 'equal)))
      (while (not (eobp))
        (let* ((line (-chomp-cr (buffer-substring-no-properties (pos-bol) (pos-eol))))
               (trimmed (string-trim-left line)))
          (unless (or (string= trimmed "") (eq (aref trimmed 0) ?#))
            (let* ((tokens (split-string line))
                   (command (upcase (car tokens)))
                   (args (cdr tokens)))
              (cond
               ((or (string= command "PFX") (string= command "SFX"))
                (let ((affix (-parse-aff-affix-line dict command args cmd-affix)))
                  (when affix
                    (if (string= command "PFX")
                        (push affix prefixes)
                      (push affix suffixes)))))
               ((string= command "FLAG")
                (setf (-dict-flag-type dict)
                      (cond ((string= (upcase (car args)) "LONG") 'long)
                            ((string= (upcase (car args)) "NUM") 'num)
                            ((string= (upcase (car args)) "UTF-8") 'utf-8)
                            (t (error "vibe-spell: invalid FLAG type %S" (car args))))))
               ((string= command "IGNORE")
                (setf (-dict-ignored-chars dict) (car args)))
               ((string= command "TRY")
                (setf (-dict-try-chars dict) (car args)))
               ((string= command "KEY")
                (setf (-dict-keyboard-closeness dict) (car args)))
               ((string= command "COMPLEXPREFIXES")
                (setf (-dict-complex-prefixes dict) t))
               ((string= command "FULLSTRIP")
                (setf (-dict-fullstrip dict) t))
               ((string= command "CHECKSHARPS")
                (setf (-dict-checksharps dict) t))
               ((string= command "FORBIDWARN")
                (setf (-dict-forbid-warn dict) t))
               ((string= command "WARN")
                (setf (-dict-warn-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "CIRCUMFIX")
                (setf (-dict-circumfix-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "FORBIDDENWORD")
                (setf (-dict-forbiddenword-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "KEEPCASE")
                (setf (-dict-keepcase-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "NEEDAFFIX")
                (setf (-dict-need-affix-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "NOSUGGEST")
                (setf (-dict-nosuggest-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "MAXNGRAMSUGS")
                (setf (-dict-max-ngram-suggestions dict) (string-to-number (car args))))
               ((string= command "MAXDIFF")
                (let ((v (string-to-number (car args))))
                  (setf (-dict-max-diff-factor dict) (if (> v 10) 5 v))))
               ((string= command "ONLYMAXDIFF")
                (setf (-dict-only-max-diff dict) t))
               ((string= command "COMPOUNDFLAG")
                (setf (-dict-compound-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "COMPOUNDBEGIN")
                (setf (-dict-compound-begin-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "COMPOUNDEND")
                (setf (-dict-compound-last-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "COMPOUNDMIDDLE")
                (setf (-dict-compound-middle-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "ONLYINCOMPOUND")
                (setf (-dict-compound-onlyin-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "COMPOUNDPERMITFLAG")
                (setf (-dict-compound-permit-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "COMPOUNDFORBIDFLAG")
                (setf (-dict-compound-forbid-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "COMPOUNDROOT")
                (setf (-dict-compound-root-flag dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "FORCEUCASE")
                (setf (-dict-compound-force-uppercase dict) (-decode-first-flag (car args) (-dict-flag-type dict))))
               ((string= command "COMPOUNDMIN")
                (let ((v (string-to-number (car args))))
                  (setf (-dict-compound-min-length dict) (if (= v 0) 1 v))))
               ((string= command "COMPOUNDWORDMAX")
                (setf (-dict-compound-max-word-count dict) (string-to-number (car args))))
               ((string= command "CHECKCOMPOUNDCASE")
                (setf (-dict-compound-check-case dict) t))
               ((string= command "CHECKCOMPOUNDDUP")
                (setf (-dict-compound-check-duplicate dict) t))
               ((string= command "CHECKCOMPOUNDTRIPLE")
                (setf (-dict-compound-check-triple dict) t))
               ((string= command "AF")
                (pcase (-array-command-step cmd-counts command (car args))
                  ('data
                   (push (-decode-flags (car args) (-dict-flag-type dict)) flag-aliases)
                   ;; Kept live (not just set once at the end) so that
                   ;; PFX/SFX continuation flags and COMPOUNDRULE lines
                   ;; later in this same file can resolve aliases too.
                   (setf (-dict-flag-aliases dict) (vconcat (reverse flag-aliases))))))
               ((string= command "BREAK")
                (setq break-seen t)
                (pcase (-array-command-step cmd-counts command (car args))
                  ('data (push (car args) break-patterns))))
               ((string= command "COMPOUNDRULE")
                (pcase (-array-command-step cmd-counts command (car args))
                  ('data (push (-parse-compound-rule-line (car args) (-dict-flag-type dict))
                               compound-rules))))
               ((string= command "REP")
                (pcase (-array-command-step cmd-counts command (car args))
                  ('data (push (cons (nth 0 args) (string-replace "_" " " (nth 1 args)))
                               rep-pairs))))
               ((string= command "MAP")
                (pcase (-array-command-step cmd-counts command (car args))
                  ('data (push (-parse-similarity-group (car args)) similarities))))
               ((string= command "ICONV")
                (pcase (-array-command-step cmd-counts command (car args))
                  ('data (push (cons (nth 0 args) (nth 1 args)) iconv-pairs))))
               ((string= command "OCONV")
                (pcase (-array-command-step cmd-counts command (car args))
                  ('data (push (cons (nth 0 args) (nth 1 args)) oconv-pairs))))
               ((member command '("PHONE" "CHECKCOMPOUNDPATTERN" "AM"))
                (-array-command-step cmd-counts command (car args)))
               (t nil)))))
        (forward-line 1))
      ;; As in Nuspell, ignored characters are erased from each affix's
      ;; appended text (but not from its stripped text) once parsing is
      ;; done, since IGNORE may appear anywhere relative to PFX/SFX.
      (dolist (p prefixes)
        (setf (-prefix-appending p) (-erase-chars (-prefix-appending p) (-dict-ignored-chars dict))))
      (dolist (s suffixes)
        (setf (-suffix-appending s) (-erase-chars (-suffix-appending s) (-dict-ignored-chars dict))))
      (setf (-dict-prefix-table dict)
            (-build-affix-table prefixes #'-prefix-appending))
      (setf (-dict-suffix-table dict)
            (-build-affix-table suffixes (lambda (s) (reverse (-suffix-appending s)))))
      (unless break-seen (setq break-patterns '("^-" "-$" "-")))
      (let ((parts (-partition-break-patterns break-patterns)))
        (setf (-dict-break-start dict) (nth 0 parts))
        (setf (-dict-break-end dict) (nth 1 parts))
        (setf (-dict-break-middle dict) (nth 2 parts)))
      (setf (-dict-compound-rules dict) (nreverse compound-rules))
      (setf (-dict-compound-rules-all-flags dict)
            (-compute-compound-rules-all-flags (-dict-compound-rules dict)))
      (setf (-dict-flag-aliases dict) (vconcat (nreverse flag-aliases)))
      (pcase-let ((`(,whole ,start ,end ,any)
                   (-build-replacement-table (nreverse rep-pairs))))
        (setf (-dict-rep-whole dict) whole)
        (setf (-dict-rep-start dict) start)
        (setf (-dict-rep-end dict) end)
        (setf (-dict-rep-any dict) any))
      (setf (-dict-similarities dict) (nreverse similarities))
      (setf (-dict-input-iconv dict) (-build-substr-replacer (nreverse iconv-pairs)))
      (setf (-dict-output-oconv dict) (-build-substr-replacer (nreverse oconv-pairs))))))

;;;; Personal dictionary

(defun -personal-dictionary-file (name)
  "Return the personal dictionary file to use for dictionary NAME.
See `vibe-spell-personal-dictionary-format': this is the first file
named by it that exists, or, if none exists, the file named by its
first element."
  (let ((formats (ensure-list vibe-spell-personal-dictionary-format)))
    (or (-dolist-until (fmt formats)
          (let ((file (format fmt name)))
            (and (file-exists-p file) file)))
        (format (car formats) name))))

(defun -init-personal-dictionary (dict name)
  "Set up DICT's personal dictionary, and load any words already in it.
NAME is the dictionary name passed to `vibe-spell-create'."
  (let ((file (-personal-dictionary-file name)))
    (setf (-dict-pdict dict) file)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((word (-chomp-cr (buffer-substring-no-properties
                                  (pos-bol) (pos-eol)))))
            (unless (string-empty-p word)
              (-dict-add-word dict word nil)))
          (forward-line 1))))))

(defun -save-personal-word (file word)
  "Append WORD as a new line to personal dictionary FILE.
FILE, and its parent directory, are created if they do not exist yet."
  (make-directory (file-name-directory file) t)
  (write-region (concat word "\n") nil file t))

(defun -remove-personal-word (file word)
  "Remove each line equal to WORD from personal dictionary FILE.
Return non-nil if FILE exists and had at least one such line."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (removed)
        (while (not (eobp))
          (if (string= (-chomp-cr (buffer-substring-no-properties (pos-bol) (pos-eol))) word)
              (progn (delete-region (pos-bol) (min (point-max) (1+ (pos-eol))))
                     (setq removed t))
            (forward-line 1)))
        (when removed
          (write-region (point-min) (point-max) file))
        removed))))

;; Local Variables:
;; read-symbol-shorthands: (("-" . "vibe-spell--"))
;; End:

(provide 'vibe-spell)
;;; vibe-spell.el ends here
