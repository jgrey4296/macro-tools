;;; jg-misc-macros.el -*- lexical-binding: t; no-byte-compile: t; -*-
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

(defconst jg-misc-macros-fmt-as-bool-pair '("T" . "F"))
(defconst jg-misc-macros--sym-sep "-")
(defvar   jg-misc-macros--kwd-strs (list :example "ex"))

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
  " Convert a function mentioned in a macro into something callable


 "
  ;; todo add an autoloadp case
  (pcase fn
    ('nil nil)
    ((pred functionp)
     fn)
    ((and x (or `(function . (lambda . ,_)) `(function (lambda . ,_ ))))
     (eval x))
    ((and x `(function . ,fn) (guard (functionp fn)))
     fn)
    (x x)
    )
  )

;;;###autoload
(defun fmt-as-bool! (arg)
  " pass in a value, convert it to one of the values in
`jg-misc-macros-fmt-as-bool-pair` "
  (if arg (car jg-misc-macros-fmt-as-bool-pair) (cdr jg-misc-macros-fmt-as-bool-pair))
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
  separated by 'jg-misc-macros--sym-sep', looking up keywords in 'jg-misc-macros--kwd-strs' "

  (intern (string-join
           (-reject #'null
                    (mapcar #'(lambda (x) (cond
                                           ((null x) nil)
                                           ((and (keywordp x) (plist-member jg-misc-macros--kwd-strs x))
                                            (plist-get jg-misc-macros--kwd-strs x))
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
           jg-misc-macros--sym-sep
           )
          )
  )


(provide 'jg-misc-macros)

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
;;; jg-misc-macros.el ends here
