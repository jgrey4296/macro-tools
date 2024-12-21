;;; jg-misc-macros.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(eval-when-compile
  (require 's)
  )

(defconst jg-misc-macros-fmt-as-bool-pair '("T" . "F"))

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
  "Prep handler functions by possibly evaluating them"
  (pcase fn
    ('nil nil)
    ((and x `(function (lambda . ,_)))
     (eval x))
    ((and x `(function ,_))
     (eval x))
    ((and x (pred symbolp) (pred symbol-function))
     x)
    (x nil)
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
