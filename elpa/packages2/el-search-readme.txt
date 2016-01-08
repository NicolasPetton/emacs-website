Introduction
============


The main user entry point is the command `el-search-pattern'.  It
prompts for a `pcase' pattern and searches the current buffer for
expressions that are matched by it when read.  Point is put at the
beginning of the expression found (unlike isearch).

It doesn't matter how the code is actually formatted.  Comments are
ignored by the search, and strings are treated as objects, their
contents are not being searched.

Example 1: if you enter

   97

at the prompt, this will find any occurrence of the number 97 in
the code, but not 977 or (+ 90 7) or "My string containing 97".
But it will find anything `eq' to 97 after reading, e.g. #x61 or
?a.


Example 2: If you enter the pattern

  `(defvar ,_)

you search for all defvar forms that don't specify an init value.

The following will search for defvar forms with a docstring whose
first line is longer than 70 characters:

  `(defvar ,_ ,_
     ,(and s (guard (< 70 (length (car (split-string s "\n")))))))


Convenience
===========

For expression input, the minibuffer prompts here uses
`emacs-lisp-mode'.

When reading a search pattern in the minibuffer, the input is
automatically wrapped into `(and expr ,(read input)).  So, if you
want to search a buffer for symbols that are defined in "cl-lib",
you can use this pattern

  (guard (and (symbolp expr)
              (when-let ((file (symbol-file expr)))
                (string-match-p "cl-lib\\.elc?$" file))))

without binding the variable `expr'.


Replacing
=========

You can replace expressions with command `el-search-query-replace'.
You are queried for a (pcase) pattern and a replacement expression.
For each match of the pattern, the replacement expression is
evaluated with the bindings created by the pcase matching in
effect, and printed to produce the replacement string.

Example: In some buffer you want to swap the two expressions at the
places of the first two arguments in all calls of function `foo',
so that e.g.

  (foo 'a (* 2 (+ 3 4)) t)

becomes

  (foo (* 2 (+ 3 4)) 'a t).

This will do it:

   M-x el-search-query-replace RET
   `(foo ,a ,b . ,rest) RET
   `(foo ,b ,a . ,rest) RET

Type y to replace a match and go to the next one, r to replace
without moving, SPC to go to the next match and ! to replace all
remaining matches automatically.  q quits.  n is like SPC, so that
y and n work like in isearch (meaning "yes" and "no") if you are
used to that.


Suggested key bindings
======================

   (define-key emacs-lisp-mode-map [(control ?S)] #'el-search-pattern)
   (define-key emacs-lisp-mode-map [(control ?%)] #'el-search-query-replace)

   (define-key isearch-mode-map [(control ?S)] #'el-search-search-from-isearch)
   (define-key isearch-mode-map [(control ?%)] #'el-search-replace-from-isearch)

The bindings in `isearch-mode-map' let you conveniently switch to
elisp searching from isearch.


Bugs, Known Limitations
=======================

- Replacing: in some cases the reader syntax of forms
is changing due to reading+printing.  "Some" because we can treat
that problem in most cases.

- Similarly: Comments are normally preserved (where it makes
sense).  But when replacing like `(foo ,a ,b) -> `(foo ,b ,a)

in a content like

  (foo
    a
    ;;a comment
    b)

the comment will be lost.


 Acknowledgments
 ===============

Thanks to Stefan Monnier for corrections and advice.


TODO:

- implement backward searching and wrapped searching

- improve docstrings

- add more examples

- handle more reader syntaxes, e.g. #n, #n#

- Implement sessions; add multi-file support based on iterators.  A
file list is read in (or the user can specify an iterator as a
variable).  The state in the current buffer is just (buffer
. marker).  Or should this be abstracted into an own lib?  Could be
named "files-session" or so.