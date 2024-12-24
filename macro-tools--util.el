;;; macro-tools--util.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(eval-when-compile
  (require 'cl-lib)
  (require 's)
  (require 'eieio-core)

  (declare-function eieio-make-clas-predicate "eieio-core")
  )

(defconst macro-tools--util-fmt-as-bool-pair '("T" . "F"))
(defconst macro-tools--util-sym-sep "-")
(defvar   macro-tools--util-kwd-strs (list :example "ex"))

;;;###autoload
(defun unquote! (val)
  "Generalized unquoting function
  Does not recursively unquote.
  "
  (declare (pure t) (side-effect-free t))
  (if (and (consp val) (memq (car val) '(quote function)))
      (cadr val)
    val
    )
  )

;;;###autoload
(defun upfun!(fn)
  " Convert a function mentioned in a macro into something callable "
  ;; todo add an autoloadp case
  (pcase fn
    ('nil nil)
    ((pred functionp)
     fn)
    ((and x (or `(function . (lambda . ,_)) `(function (lambda . ,_ ))))
     (eval x))
    ((and x (or `(function . ,fn) `(function . (,fn))) (guard (functionp fn)))
     fn)
    (x x)
    )
  )

;;;###autoload
(defun fmt-as-bool! (arg)
  " pass in a value, convert it to one of the values in
`macro-tools--util-fmt-as-bool-pair` "
  (if arg (car macro-tools--util-fmt-as-bool-pair) (cdr macro-tools--util-fmt-as-bool-pair))
  )

;;;###autoload
(defun pop-plist-from-body! (body)
  "macros with &key and &rest include the keys in the body,
  (and need &allow-other-keys.
         This strips the kwds and their values off from the body
         "
  (while (and body (listp body) (keywordp (car body))) (pop body) (pop body))
  body
  )

;;;###autoload
(defun ensure-hook! (sym)
  "Give a symbol, get a hook name
eg: blah -> blah-hook
... blah-mode -> blah-mode-hook
... blah-mode-hook -> blah-mode-hook
"
  (let ((symstr (symbol-name (unquote! sym))))
    (if (s-suffix? "-hook" symstr)
        sym
      (intern (format "%s-hook" symstr)))
      )
    )

;;;###autoload
(defun gensym! (&rest names)
  " make a newly interned symbol from the provided name strings/symbols/keywords,
  separated by 'macro-tools--util-sym-sep', looking up keywords in 'macro-tools--util-kwd-strs' "

  (intern (string-join
           (-reject #'null
                    (mapcar #'(lambda (x) (cond
                                           ((null x) nil)
                                           ((and (keywordp x) (plist-member macro-tools--util-kwd-strs x))
                                            (plist-get macro-tools--util-kwd-strs x))
                                           ((keywordp x) nil)
                                           ((symbolp x)
                                            (symbol-name x))
                                           ((stringp x)
                                            x)
                                           (t nil)
                                           )
                                )
                            names
                            )
                    )
           macro-tools--util-sym-sep
           )
          )
  )

(defmacro file! ()
  "Return the file of the file this macro was called. orig from doom."
  '(or (macroexp-file-name) load-file-name buffer-file-name)
)

(defmacro dir! ()
  "Return the directory of the file this macro was called. orig from doom."
  '(file-name-directory (file!))
  )

(provide 'macro-tools--util)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    December 20, 2024
;; Modified:   December 20, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; macro-tools--util.el ends here
