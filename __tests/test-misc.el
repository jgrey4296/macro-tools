;;; test-misc.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header
(require 'buttercup)
(require 'jg-misc-macros)

(describe "unquote tests"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "ignores non-quotes"
    (expect (unquote! 2) :to-be 2)
    (expect (unquote! (list 1 2 3 4)) :to-equal (list 1 2 3 4))
    (expect (unquote! (quote (list 1 2 3 4))) :to-equal '(list 1 2 3 4))
    )
  (it "does basic unquoting"
    (expect (unquote! 'blah) :to-be (quote blah)))
  (it "unquotes functions"
    (expect (unquote! #'blah) :to-be (quote blah))
    (expect (unquote! #'blah) :to-equal (quote blah))
    )

)

(describe "upfun tests"
  :var (setup)
  (before-each
    (setf (symbol-function 'setup) #'(lambda (&rest data) nil))
    (spy-on 'setup)
    )
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should pass through function symbols"
    (expect 'setup :not :to-have-been-called)
    (expect (upfun! #'setup) :to-be #'setup)
    (expect (functionp (upfun! #'setup)) :to-be t)
    (expect (upfun! (function setup)) :to-be #'setup)
    (expect (functionp (upfun! (function setup))) :to-be t)
    (expect 'setup :not :to-have-been-called)
    )
  (it "should reject non-function symbols"
    (expect 'setup :not :to-have-been-called)
    (expect (upfun! 'default-directory) :to-be nil)
    (expect 'setup :not :to-have-been-called)
    )
  (it "should eval list lambdas"
    (expect 'setup :not :to-have-been-called)
    (expect (upfun! '(function (lambda () (setup)))) :not :to-be nil)
    (expect (functionp (upfun! '(function (lambda () (setup))))) :to-be t)
    (expect 'setup :not :to-have-been-called)
    )
  )

(describe "fmt-as-bool tests"
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should format to T and F"
    (expect (fmt-as-bool! t) :to-equal "T")
    (expect (fmt-as-bool! nil) :to-equal "F")
    (expect (fmt-as-bool! (list 1 2 3)) :to-equal "T")
    (expect (fmt-as-bool! (list)) :to-equal "F")
    )

)

(describe "pop-list tests"
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should ignore non-plist-prefixs"
    (expect (pop-plist-from-body! '(1 2 3 4)) :to-equal '(1 2 3 4))
    (expect (pop-plist-from-body! 'blah) :to-equal 'blah)
    )
  (it "should remove the pairs of values until no keywords remain at head"
    (expect (pop-plist-from-body! '(:blah val)) :to-equal '())
    (expect (pop-plist-from-body! '(:blah val other 2 3 4)) :to-equal '(other 2 3 4))
    )

)

(describe "ensure-hook tests"
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should return hooks unchanged"
    (expect (ensure-hook! 'lsp-mode-hook) :to-be 'lsp-mode-hook)
    )
  (it "should return hook-symbols otherwise"
    (expect (ensure-hook! 'blah)              :to-be 'blah-hook)
    (expect (ensure-hook! (quote blah))       :to-be 'blah-hook)
    )
  (it "should handle x-mode symbols as well"
    (expect (ensure-hook! 'blah-mode)         :to-be 'blah-mode-hook)
    (expect (ensure-hook! (quote blah-mode))  :to-be 'blah-mode-hook)
    )
)



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
;;; test-misc.el ends here
