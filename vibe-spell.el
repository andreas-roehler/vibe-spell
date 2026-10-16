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
;; Only checking (`vibe-spell-check') is implemented; `vibe-spell-suggest'
;; is not.  Within checking, the following simplifications were made
;; relative to upstream Nuspell, mostly to leave out machinery that
;; exists in Nuspell only to support suggestions:
;;
;; - Dictionaries that set COMPLEXPREFIXES (double prefix stripping,
;;   used by a handful of languages) are rejected at `vibe-spell-create'
;;   time.
;; - ICONV/OCONV, MAP, REP, PHONE, KEY and TRY are not parsed; these
;;   only matter for Nuspell's suggestion generator.  CHECKCOMPOUNDREP,
;;   which also consults the REP table, is accepted but has no effect.
;; - CHECKCOMPOUNDPATTERN is not parsed; such restrictions are not
;;   enforced.
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

;;; Public API

(defun vibe-spell-create (dict)
  "Create a spell checker object for dictionary DICT.
DICT is a dictionary name, such as \"en_US\"; the files DICT.aff and
DICT.dic are looked up under `vibe-spell-dictionary-path'.

This only supports checking; see `vibe-spell-check'.  Suggestions
\(`vibe-spell-suggest') are not implemented.  See the Commentary at
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

(defun vibe-spell-suggest (_dict _word)
  "Return a list of corrections for WORD based on DICT."
  (error "vibe-spell-suggest is not yet implemented"))

(defun vibe-spell-add-word (dict word &optional save)
  "Add WORD to DICT, and optionally SAVE to personal dictionary file.
DICT is a spell checker object created by `vibe-spell-create'.  When
SAVE is non-nil, also append WORD to DICT's personal dictionary file
\(see `vibe-spell-personal-dictionary-format'), creating it if needed,
so that it is picked up again by a later `vibe-spell-create'."
  (-dict-add-word dict word nil)
  (when save
    (-save-personal-word (-dict-pdict dict) word)))

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
                            `(progn
                               (setq ,result ,(macroexp-progn (cddr spec)))
                               nil))
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

(defconst -hidden-homonym-flag -1
  "Sentinel flag marking a dictionary entry as hidden from direct lookup.
Mirrors Nuspell's `Aff_Data::HIDDEN_HOMONYM_FLAG'.  See `-dict-add-word'.")

;;;; Data structures

(cl-defstruct (-prefix (:constructor -prefix-create))
  flag cross-product stripping appending cont-flags condition)

(cl-defstruct (-suffix (:constructor -suffix-create))
  flag cross-product stripping appending cont-flags condition)

(cl-defstruct (-dict (:constructor -dict-create))
  (words nil)                          ; hash-table: word -> list of flag lists
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
  (compound-check-triple nil))

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
  (and (/= flag 0) (memql flag flags) t))

(defun -cross-valid-p (inner-flags outer-flag)
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
;; whatever is stored in the `-dict-words' hash table), or nil if no
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
           (-dolist-until (flags (gethash root (-dict-words dict)))
             (and (-cross-valid-p flags (-suffix-flag se))
                  (not (and (eq mode :full-word)
                            (-flags-contain flags (-dict-compound-onlyin-flag dict))))
                  (not (and skip-hidden (-flags-contain flags -hidden-homonym-flag)))
                  (or (-valid-inside-compound-p dict mode flags)
                      (-valid-inside-compound-p dict mode (-suffix-cont-flags se)))
                  (cons root flags)))))))

(defun -strip-prefix-only (dict mode word skip-hidden)
  (-dolist-until (pe (-iterate-prefixes-of dict word))
    (and (not (or (-outer-prefix-not-valid-p dict mode pe)
                  (-circumfix-flags-p dict (-prefix-cont-flags pe))))
         (-prefix-condition-matches-p pe word)
         (let ((root (-prefix-to-root pe word)))
           (-dolist-until (flags (gethash root (-dict-words dict)))
             (and (-cross-valid-p flags (-prefix-flag pe))
                  (not (and (eq mode :full-word)
                            (-flags-contain flags (-dict-compound-onlyin-flag dict))))
                  (not (and skip-hidden (-flags-contain flags -hidden-homonym-flag)))
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
                    (-dolist-until (flags (gethash root (-dict-words dict)))
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
                             (not (and skip-hidden (-flags-contain flags -hidden-homonym-flag)))
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
           (-dolist-until (flags (gethash root (-dict-words dict)))
             (and (-cross-valid-p flags (-suffix-flag se2))
                  (not (-flags-contain flags (-dict-compound-onlyin-flag dict)))
                  (not (and skip-hidden (-flags-contain flags -hidden-homonym-flag)))
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
           (-dolist-until (flags (gethash root (-dict-words dict)))
             (and (or (-cross-valid-p (-suffix-cont-flags se1) (-prefix-flag pe1))
                      (-cross-valid-p flags (-prefix-flag pe1)))
                  (-cross-valid-p flags (-suffix-flag se2))
                  (not (-flags-contain flags (-dict-compound-onlyin-flag dict)))
                  (not (and skip-hidden (-flags-contain flags -hidden-homonym-flag)))
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
                  (-dolist-until (flags (gethash root (-dict-words dict)))
                    (and (or (-cross-valid-p (-suffix-cont-flags se2) (-prefix-flag pe1))
                             (-cross-valid-p flags (-prefix-flag pe1)))
                         (-cross-valid-p flags (-suffix-flag se2))
                         (not (-flags-contain flags (-dict-compound-onlyin-flag dict)))
                         (not (and skip-hidden (-flags-contain flags -hidden-homonym-flag)))
                         (cons root flags)))))))))

;;;; Simple (non-compound) word checking

(defun -check-simple-word (dict word skip-hidden)
  "Check WORD as a single (possibly affixed) dictionary entry.
Return (ROOT-WORD . FLAGS) or nil; see the affix-stripping functions
above for the shape of the result."
  (or (-dolist-until (flags (gethash word (-dict-words dict)))
        (and (not (or (-flags-contain flags (-dict-need-affix-flag dict))
                      (-flags-contain flags (-dict-compound-onlyin-flag dict))
                      (and skip-hidden (-flags-contain flags -hidden-homonym-flag))))
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
    (or (-dolist-until (flags (gethash word (-dict-words dict)))
          (and (not (or (-flags-contain flags (-dict-need-affix-flag dict))
                        (not (or (-flags-contain flags (-dict-compound-flag dict))
                                 (-flags-contain flags cpd-flag)))
                        (-flags-contain flags -hidden-homonym-flag)))
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
  (-dolist-until (flags (gethash word (-dict-words dict)))
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

(defun -dict-add-word (dict word flags)
  "Add dictionary entry WORD/FLAGS to DICT.
If WORD is ALL-CAPITAL (with non-empty FLAGS), PASCAL or CAMEL cased,
also add a title-cased duplicate flagged as a hidden homonym, so that
checking the title-cased form of a word whose canonical form has
different casing (e.g. an acronym) falls through to other checks
rather than accepting it outright; see `-hidden-homonym-flag'."
  (let* ((table (-dict-words dict))
         (casing (-classify-casing word)))
    (puthash word (cons flags (gethash word table)) table)
    (when (or (and (eq casing 'all-capital) flags)
              (memq casing '(pascal camel)))
      (unless (-flags-contain flags (-dict-forbiddenword-flag dict))
        (let ((title (-to-title word)))
          (puthash title (cons (cons -hidden-homonym-flag flags) (gethash title table))
                   table))))))

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
               ((member command '("MAP" "REP" "PHONE" "ICONV" "OCONV" "CHECKCOMPOUNDPATTERN" "AM"))
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
      (setf (-dict-flag-aliases dict) (vconcat (nreverse flag-aliases))))))

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

;; Local Variables:
;; read-symbol-shorthands: (("-" . "vibe-spell--"))
;; End:

(provide 'vibe-spell)
;;; vibe-spell.el ends here
