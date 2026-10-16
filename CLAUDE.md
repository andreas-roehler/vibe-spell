# vibe-spell.el
- Private symbols start with `-`.  This is expanded via the shorthand mechanism to the usual `vibe-spell--` prefix.
- Public symbols are always written in full and have the `vibe-spell-` prefix.
- Instead of `cl-block`, prefer `-dolist-until`.
- Instead of `(cl-loop for v across arr ...)`, prefer `(dotimes (i (length arr)) (let (v (aref arr i)) ...))`.
- Instead of `(zerop (length s))`, use `string-empty-p`.
- Prefer `pcase` over `cl-case`, `dolist` and variations over simple `cl-loop`.
- Make sure to byte-compile code being benchmarked.
- nuspell and hunspell sources are available in this directory for your reference.

## Target Emacs 31: prefer core functions over cl-lib habits

We target Emacs 31 (see `Package-Requires` in vibe-spell.el), which is new
enough that several operations long reached for via `cl-lib` out of habit now
have a plain, non-`cl-` core equivalent.  Checked against
github.com/emacs-compat/compat (which backports exactly these to older
Emacsen, so its `compat-NN.el` files are a reliable list of what is genuinely
new in version NN) -- prefer the right-hand side below; the file already uses
some of these (e.g. `any`/`all` in `-compound-rules-has-any-flag-p`,
`-match-compound-rules`; `oddp` in `-decode-flags`), so stay consistent:

- `cl-some` / `cl-every` → `any` / `all` (Emacs 31; same `(PRED LIST)` arg order).
- `cl-incf` / `cl-decf` → `incf` / `decf` (Emacs 31).
- `cl-oddp` / `cl-evenp` / `cl-plusp` / `cl-minusp` → `oddp` / `evenp` / `plusp` / `minusp` (Emacs 31).
- `seq-take` / `seq-drop` → `take` / `drop` (Emacs 29 / 30), when the sequence is already a plain list -- skips seq.el's generic-sequence dispatch.
- `(not (eq (gethash k table 'missing) 'missing))`-style sentinel checks → `hash-table-contains-p` (Emacs 31), when only presence matters, not the value.
- `cl-member-if` → `member-if` (Emacs 31; `any` is literally an alias for it).
- A new top-level `-foo-1`/`-foo-2` helper whose only job is a local tail-recursive loop → consider `named-let` (Emacs 28) instead, to keep the loop from leaking into the file's namespace.
- A `while` loop whose condition is "keep going while this binding is non-nil" → `while-let` (Emacs 29).
- Stripping a fixed leading/trailing run of a predicate from a list → `take-while` / `drop-while` (Emacs 31).
