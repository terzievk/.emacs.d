# My Emacs config.
Requires `Emacs 29+`(tree-sitter, among other things).

Until Emacs 31 `markdown-ts-mode needs` manual installation, see init file comments.

Don't forget to have `clangd` and `gcc` installed.

## How to run:
Git clone at `$HOME`. Open Emacs. It will do its thing, and voila.

It might take some time - Melpa is kinda slow these days;  10-ish minutes is
kinda normal.

Tip: Run a system update/upgrade before installing. Doesn’t hurt. Not doing so
crashed my build once.

## Q&A?
### Why tree-sitter?
I like navigating with hjkl and have my keyboard sped up(the equivalent of
`xset r rate 160 100`). `c++-mode` lagged when scrolling and `c++-ts-mode` doesn’t.
Plus Emacs is set on the tree-sitter route regardless.

### Why clangd?
Irony mode is no longer actively maintained.

### Why evil?
Emacs pinky…

#### Does it help?
Kinda.
