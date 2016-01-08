;;; el-search.el --- Expression based incremental search for emacs-lisp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2015 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 29 Jul 2015
;; Keywords: lisp
;; Compatibility: GNU Emacs 25
;; Version: 0.0.3
;; Package-Requires: ((emacs "25"))


;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Introduction
;; ============
;;
;;
;; The main user entry point is the command `el-search-pattern'.  It
;; prompts for a `pcase' pattern and searches the current buffer for
;; expressions that are matched by it when read.  Point is put at the
;; beginning of the expression found (unlike isearch).
;;
;; It doesn't matter how the code is actually formatted.  Comments are
;; ignored by the search, and strings are treated as objects, their
;; contents are not being searched.
;;
;; Example 1: if you enter
;;
;;    97
;;
;; at the prompt, this will find any occurrence of the number 97 in
;; the code, but not 977 or (+ 90 7) or "My string containing 97".
;; But it will find anything `eq' to 97 after reading, e.g. #x61 or
;; ?a.
;;
;;
;; Example 2: If you enter the pattern
;;
;;   `(defvar ,_)
;;
;; you search for all defvar forms that don't specify an init value.
;; 
;; The following will search for defvar forms with a docstring whose
;; first line is longer than 70 characters:
;;
;;   `(defvar ,_ ,_
;;      ,(and s (guard (< 70 (length (car (split-string s "\n")))))))
;;
;;
;; Convenience
;; ===========
;;
;; For expression input, the minibuffer prompts here uses
;; `emacs-lisp-mode'.
;;
;; When reading a search pattern in the minibuffer, the input is
;; automatically wrapped into `(and expr ,(read input)).  So, if you
;; want to search a buffer for symbols that are defined in "cl-lib",
;; you can use this pattern
;;
;;   (guard (and (symbolp expr)
;;               (when-let ((file (symbol-file expr)))
;;                 (string-match-p "cl-lib\\.elc?$" file))))
;;
;; without binding the variable `expr'.
;;
;;
;; Replacing
;; =========
;;
;; You can replace expressions with command `el-search-query-replace'.
;; You are queried for a (pcase) pattern and a replacement expression.
;; For each match of the pattern, the replacement expression is
;; evaluated with the bindings created by the pcase matching in
;; effect, and printed to produce the replacement string.
;;
;; Example: In some buffer you want to swap the two expressions at the
;; places of the first two arguments in all calls of function `foo',
;; so that e.g.
;;
;;   (foo 'a (* 2 (+ 3 4)) t)
;;
;; becomes
;;
;;   (foo (* 2 (+ 3 4)) 'a t).
;;
;; This will do it:
;;
;;    M-x el-search-query-replace RET
;;    `(foo ,a ,b . ,rest) RET
;;    `(foo ,b ,a . ,rest) RET
;;
;; Type y to replace a match and go to the next one, r to replace
;; without moving, SPC to go to the next match and ! to replace all
;; remaining matches automatically.  q quits.  n is like SPC, so that
;; y and n work like in isearch (meaning "yes" and "no") if you are
;; used to that.
;;
;;
;; Suggested key bindings
;; ======================
;;
;;    (define-key emacs-lisp-mode-map [(control ?S)] #'el-search-pattern)
;;    (define-key emacs-lisp-mode-map [(control ?%)] #'el-search-query-replace)
;;
;;    (define-key isearch-mode-map [(control ?S)] #'el-search-search-from-isearch)
;;    (define-key isearch-mode-map [(control ?%)] #'el-search-replace-from-isearch)
;;
;; The bindings in `isearch-mode-map' let you conveniently switch to
;; elisp searching from isearch.
;;
;;
;; Bugs, Known Limitations
;; =======================
;;
;; - Replacing: in some cases the reader syntax of forms
;; is changing due to reading+printing.  "Some" because we can treat
;; that problem in most cases.
;;
;; - Similarly: Comments are normally preserved (where it makes
;; sense).  But when replacing like `(foo ,a ,b) -> `(foo ,b ,a)
;;
;; in a content like
;;
;;   (foo
;;     a
;;     ;;a comment
;;     b)
;;
;; the comment will be lost.
;;
;;
;;  Acknowledgments
;;  ===============
;;
;; Thanks to Stefan Monnier for corrections and advice.
;;
;;
;; TODO:
;;
;; - implement backward searching and wrapped searching
;;
;; - improve docstrings
;;
;; - add more examples
;;
;; - handle more reader syntaxes, e.g. #n, #n#
;;
;; - Implement sessions; add multi-file support based on iterators.  A
;; file list is read in (or the user can specify an iterator as a
;; variable).  The state in the current buffer is just (buffer
;; . marker).  Or should this be abstracted into an own lib?  Could be
;; named "files-session" or so.



;;; Code:

;;;; Requirements

(eval-when-compile
  (require 'subr-x))

(require 'cl-lib)
(require 'elisp-mode)
(require 'thingatpt)


;;;; Configuration stuff

(defgroup el-search nil
  "Expression based search and replace for `emacs-lisp-mode'."
  :group 'lisp)

(defcustom el-search-this-expression-identifier 'exp
  "Name of the identifier referring to the whole expression.
The default value is `expr'.  You can use this variable in the
search prompt to refer to value of the currently searched
expression."
  :type 'symbol)

(defface el-search-match '((((background dark)) (:background "#0000A0"))
			   (t                   (:background "DarkSlateGray1")))
  "Face for highlighting the current match.")


;;;; Helpers

(defun el-search--print (expr)
  (let ((print-quoted t)
        (print-length nil)
        (print-level nil))
    (prin1-to-string expr)))

(defvar el-search-read-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map [(control ?g)] #'abort-recursive-edit)
    (define-key map [up]   nil)
    (define-key map [down] nil)
    (define-key map [(control meta backspace)] #'backward-kill-sexp)
    (define-key map [(control ?S)] #'exit-minibuffer)
    map)
  "Map for reading input with `el-search-read-expression'.")

;; $$$$$FIXME: this should be in Emacs!  There is only a helper `read--expression'.
(defun el-search-read-expression (prompt &optional initial-contents hist default read)
  "Read expression for `my-eval-expression'."
  (minibuffer-with-setup-hook
      (lambda ()
        (emacs-lisp-mode)
        (use-local-map el-search-read-expression-map)
        (setq font-lock-mode t)
        (funcall font-lock-function 1)
        (backward-sexp)
        (indent-sexp)
        (goto-char (point-max)))
    (read-from-minibuffer prompt initial-contents el-search-read-expression-map read
                          (or hist 'read-expression-history) default)))

(defun el-search--read-pattern (prompt &optional default initial-contents read)
  (el-search-read-expression
   prompt initial-contents 'el-search-history
   (or default (when-let ((this-sexp (sexp-at-point)))
                 (concat "'" (el-search--print this-sexp))))
   read))

(defun el-search--end-of-sexp ()
  ;;Point must be at sexp beginning
  (or (scan-sexps (point) 1) (point-max)))

(defun el-search--ensure-sexp-start ()
  "Move point to the beginning of the next sexp if necessary.
Don't move if already at beginning of a sexp.
Point must not be inside a string or comment."
  (let ((not-done t) res)
    (while not-done
      (let ((stop-here nil) syntax-here
            (looking-at-from-back (lambda (regexp n)
                                    (save-excursion
                                      (backward-char n)
                                      (looking-at regexp)))))
        (while (not stop-here)
          (cond
           ((eobp) (signal 'end-of-buffer nil))
           ((looking-at (rx (and (* space) ";"))) (forward-line))
           ((looking-at (rx (+ (or space "\n")))) (goto-char (match-end 0)))
           ((progn (setq syntax-here (syntax-ppss))
                   (or (nth 4 syntax-here) (nth 8 syntax-here)))
            (if (nth 4 syntax-here) (forward-line) (search-forward "\"")))
           
           ;; FIXME: can the rest be done more generically?
           ((and (looking-at (rx (or (syntax symbol) (syntax word))))
                 (not (looking-at "\\_<"))
                 (not (funcall looking-at-from-back ",@" 2)))
            (forward-symbol 1))
           ((or (and (looking-at "'") (funcall looking-at-from-back "#" 1))
                (and (looking-at "@") (funcall looking-at-from-back "," 1)))
            (forward-char))
           (t (setq stop-here t)))))
      (condition-case nil
          (progn
            (setq res (save-excursion (read (current-buffer))))
            (setq not-done nil))
        (error (forward-char))))
    res))

(defun el-search--matcher (pattern &rest body)
  (let ((warning-suppress-log-types '((bytecomp))))
    (byte-compile
     `(lambda (expression)
        (pcase expression
          (,pattern ,@(or body (list t)))
          (_        nil))))))

(defun el-search--match-p (matcher expression)
  (funcall matcher expression))

(defun el-search--wrap-pattern (pattern)
  `(and ,el-search-this-expression-identifier ,pattern))

(defun el-search--search-pattern (pattern &optional noerror)
  "Search elisp buffer with `pcase' PATTERN.
Set point to the beginning of the occurrence found and return
point.  Optional second argument, if non-nil, means if fail just
return nil (no error)."
  ;; For better performance we read complete top-level sexps and test
  ;; for matches.  We enter top-level expressions in the buffer text
  ;; only when the test was successful.
  (let ((matcher (el-search--matcher pattern)) (match-beg nil) (opoint (point)) current-expr)
    (if (catch 'no-match
          (while (not match-beg)
            (condition-case nil
                (setq current-expr (el-search--ensure-sexp-start))
              (end-of-buffer
               (goto-char opoint)
               (throw 'no-match t)))
            (if (el-search--match-p matcher current-expr)
                (setq match-beg (point)
                      opoint (point))
              (forward-char))))
        (if noerror nil (signal 'end-of-buffer nil)))
    match-beg))

(defun el-search--do-subsexps (pos do-fun &optional ret-fun bound)
  ;; bound -> nil means till end of buffer
  (save-excursion
    (goto-char pos)
    (condition-case nil
        (while (< (point) (or bound (point-max)))
          (let* ((this-sexp-end (save-excursion (thing-at-point--end-of-sexp) (point)))
                 (this-sexp-bounds (buffer-substring-no-properties (point) this-sexp-end)))
            (funcall do-fun this-sexp-bounds this-sexp-end))
          (forward-char)
          (el-search--ensure-sexp-start))
      (end-of-buffer))
    (when ret-fun (funcall ret-fun))))

(defun el-search--create-read-map (&optional pos)
  (let ((mapping '()))
    (el-search--do-subsexps
     (or pos (point))
     (lambda (sexp _) (push (cons (read sexp) sexp) mapping))
     (lambda () (nreverse mapping))
     (save-excursion (thing-at-point--end-of-sexp) (point)))))

(defun el-search--repair-replacement-layout (printed mapping)
  (with-temp-buffer
    (insert printed)
    (el-search--do-subsexps
     (point-min)
     (lambda (sexp sexp-end)
       (when-let ((old (cdr (assoc (read sexp) mapping))))
         (delete-region (point) sexp-end)
         (when (string-match-p "\n" old)
           (unless (looking-back "^[[:space:]]*" (line-beginning-position))
             (insert "\n"))
           (unless (looking-at "[[:space:]\)]*$")
             (insert "\n")
             (backward-char)))
         (insert old)))
     (lambda () (buffer-substring (point-min) (point-max))))))


;;;; Highlighting

(defvar-local el-search-hl-overlay nil)

(defvar el-search-keep-hl nil)

(defun el-search-hl-sexp-at-point ()
  (let ((bounds (list (point) (el-search--end-of-sexp))))
    (if (overlayp el-search-hl-overlay)
        (apply #'move-overlay el-search-hl-overlay bounds)
      (overlay-put (setq el-search-hl-overlay (apply #'make-overlay bounds))
                   'face 'el-search-match)))
  (add-hook 'post-command-hook (el-search-hl-post-command-fun (current-buffer)) t))

(defun el-search-hl-remove ()
  (when (overlayp el-search-hl-overlay)
    (delete-overlay el-search-hl-overlay)))

(defun el-search-hl-post-command-fun (buf)
  (lambda ()
    (when (buffer-live-p buf)
      (unless (or el-search-keep-hl
                  (eq this-command 'el-search-query-replace)
                  (eq this-command 'el-search-pattern))
        (with-current-buffer buf
          (el-search-hl-remove)
          (remove-hook 'post-command-hook #'el-search-hl-post-command-fun t))))))


;;;; Core functions

(defvar el-search-history '()
  "List of input strings.")

(defvar el-search-success nil)
(defvar el-search-current-pattern nil)

;;;###autoload
(defun el-search-pattern (pattern)
  "Do incremental elisp search forward."
  (interactive (list (if (and (eq this-command last-command)
                              el-search-success)
                         el-search-current-pattern
                       (let ((pattern
                              (el-search--read-pattern "Find pcase pattern: "
                                                       (car el-search-history)
                                                       nil t)))
                         ;; A very common mistake: input "foo" instead of "'foo"
                         (when (and (symbolp pattern)
                                    (not (eq pattern '_))
                                    (or (not (boundp pattern))
                                        (not (eq (symbol-value pattern) pattern))))
                           (error "Please don't forget the quote when searching for a symbol"))
                         (el-search--wrap-pattern pattern)))))
  (setq el-search-current-pattern pattern)
  (setq el-search-success nil)
  (let ((opoint (point)))
    (when (eq this-command last-command)
      (forward-char))
    (when (condition-case nil
              (el-search--search-pattern pattern)
            (end-of-buffer (message "No match")
                           (goto-char opoint)
                           (el-search-hl-remove)
                           (ding)
                           nil))
      (setq el-search-success t)
      (el-search-hl-sexp-at-point)
      (message "%s" (substitute-command-keys "Type \\[el-search-pattern] to repeat")))))

(defun el-search-search-and-replace-pattern (pattern replacement &optional mapping)
  (let ((replace-all nil) (nbr-replaced 0) (nbr-skipped 0) (done nil)
        (el-search-keep-hl t) (opoint (point))
        (get-replacement (el-search--matcher pattern replacement)))
    (unwind-protect
        (while (and (not done) (el-search--search-pattern pattern t))
          (setq opoint (point))
          (unless replace-all (el-search-hl-sexp-at-point))
          (let* ((read-mapping (el-search--create-read-map))
                 (region (list (point) (el-search--end-of-sexp)))
                 (substring (apply #'buffer-substring-no-properties region))
                 (expr      (read substring))
                 (replaced-this nil)
                 (new-expr  (funcall get-replacement expr))
                 (to-insert (el-search--repair-replacement-layout
                             (el-search--print new-expr) (append mapping read-mapping)))
                 (do-replace (lambda ()
                               (atomic-change-group
                                 (apply #'delete-region region)
                                 (let ((inhibit-message t)
                                       (opoint (point)))
                                   (insert to-insert)
                                   (indent-region opoint (point))
                                   (goto-char opoint)
                                   (el-search-hl-sexp-at-point)))
                               (cl-incf nbr-replaced)
                               (setq replaced-this t))))
            (if replace-all
                (funcall do-replace)
              (while (not (pcase (if replaced-this
                                     (read-char-choice "[SPC ! q]" '(?\ ?! ?q ?n))
                                   (read-char-choice
                                    (concat "Replace this occurence"
                                            (if (or (string-match-p "\n" to-insert)
                                                    (< 40 (length to-insert)))
                                                "" (format " with `%s'" to-insert))
                                            "? [y SPC r ! q]" )
                                    '(?y ?n ?r ?\ ?! ?q)))
                            (?r (funcall do-replace)
                                nil)
                            (?y (funcall do-replace)
                                t)
                            ((or ?\ ?n)
                             (unless replaced-this (cl-incf nbr-skipped))
                             t)
                            (?! (unless replaced-this
                                  (funcall do-replace))
                                (setq replace-all t)
                                t)
                            (?q (setq done t)
                                t)))))
            (unless (or done (eobp)) (forward-char 1)))))
    (el-search-hl-remove)
    (goto-char opoint)
    (message "Replaced %d matches%s"
             nbr-replaced
             (if (zerop nbr-skipped)  ""
               (format "   (%d skipped)" nbr-skipped)))))

(defun el-search-query-replace-read-args (&optional initial-contents)
  (barf-if-buffer-read-only)
  (let* ((from (el-search--read-pattern "Replace from: " nil initial-contents))
         (to   (el-search--read-pattern "Replace with result of evaluation of: " from)))
    (list (el-search--wrap-pattern (read from)) (read to)
          (with-temp-buffer
            (insert to)
            (el-search--create-read-map 1)))))

;;;###autoload
(defun el-search-query-replace (from to &optional mapping)
  "Replace some occurrences of FROM pattern with evaluated TO."
  (interactive (el-search-query-replace-read-args))
  (setq el-search-current-pattern from)
  (barf-if-buffer-read-only)
  (el-search-search-and-replace-pattern from to mapping))

(defun el-search--take-over-from-isearch ()
  (let ((other-end isearch-other-end)
        (input isearch-string))
    (isearch-exit)
    (when (and other-end (< other-end (point)))
      (goto-char other-end))
    input))

;;;###autoload
(defun el-search-search-from-isearch ()
  ;; FIXME: an interesting alternative would be to really integrate it
  ;; with Isearch, using `isearch-search-fun-function'.
  ;; Alas, this is not trivial if we want to transfer our optimizations.
  (interactive)
  (el-search-pattern
   (el-search--read-pattern
    "Find pcase pattern: " nil (concat "'" (el-search--take-over-from-isearch)) t))
  (setq this-command 'el-search-pattern))

;;;###autoload
(defun el-search-replace-from-isearch ()
  (interactive)
  (let ((this-command 'el-search-query-replace))
    (apply #'el-search-query-replace
           (el-search-query-replace-read-args (concat "'" (el-search--take-over-from-isearch))))))

;;;; ChangeLog:

;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	whitespace clean up
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	rename a local variable
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	rename a function
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	new function el-search--end-of-sexp; use it
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	avoid repeated expansion of pcase forms
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	rename a function
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	drop el-search-expression-contains-match-p
;; 
;; 	A correct implementation would have to be more complicated.  We didn't 
;; 	recurse on arrays for example, or we didn't find (2 3) in (1 . (2 3)).
;; 
;; 	And it wasn't that effective either, so I remove it.
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	fix a condition in el-search--do-subsexps
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	el-search: some comment changes
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	rename a function
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	change default of el-search-this-expression-identifier
;; 
;; 2015-10-11  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	avoid looking-back in el-search--goto-next-sexp
;; 
;; 	because it extremely slows it down
;; 
;; 2015-09-22  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	el-search: bump version
;; 
;; 2015-09-22  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	el-search: some comment changes
;; 
;; 2015-09-22  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	el-search: add autoload cookies
;; 
;; 2015-09-22  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	el-search: remove redundant :group specs from custom defs
;; 
;; 2015-09-22  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	el-search: remove redundant package dependency on cl-lib
;; 
;; 2015-09-22  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	el-search: fix two typos
;; 
;; 2015-09-22  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	el-search: fix comment styles
;; 
;; 2015-08-06  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* el-search.el: Add missing footer
;; 
;; 2015-08-05  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* el-search.el: Fix first line convention
;; 
;; 2015-08-05  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	el-search: some small improvements
;; 
;; 2015-08-04  Michael Heerdegen  <michael_heerdegen@web.de>
;; 
;; 	new package el-search for searching elisp
;; 




(provide 'el-search)
;;; el-search.el ends here
