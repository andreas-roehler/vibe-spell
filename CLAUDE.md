# vibe-spell.el
- Private symbols start with `-`.  This is expanded via the shorthand mechanism to the usual `vibe-spell--` prefix.
- Public symbols are always written in full and have the `vibe-spell-` prefix.
- Instead of `cl-block`, prefer `-dolist-until`.
- Instead of `(cl-loop for v across arr ...)`, prefer `(dotimes (i (length arr)) (let (v (aref arr i)) ...))`.
- Instead of `(zerop (length s))`, use `string-empty-p`.
- Prefer `pcase` over `cl-case`, `dolist` and variations over simple `cl-loop`.
- Make sure to byte-compile code being benchmarked.
