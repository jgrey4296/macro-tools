;;; jg-misc-macros.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header


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

(defun unfun!(fn)
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
