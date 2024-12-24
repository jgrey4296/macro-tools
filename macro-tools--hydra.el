;;; hydra-macros.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 's)
  (require 'dash)
  )

(defvar macro-tools--hydra-format-min-column 8)
(defvar macro-tools--hydra-stack nil)

(defun macro-tools--hydra-first-char-format (text &optional index)
  (let ((index (or index 1))
        (empty "    ")
        (pad   "")
        (pad-chr ?^)
        (fmtter (lambda (i x p) (format "%s_%s_%s%s"
                                      (if (< 0 (1- i )) (substring x 0 (1- i)) "")
                                      (substring x (1- i) i)
                                      (substring x i)
                                      p)
                  ))
        )
    (cond
     ((and (symbolp text) (eq 'blank text))
      empty) ;; (concat pad empty))
     ((and (symbolp text) (s-contains? "_" (symbol-name text)))
      (concat (symbol-name text) pad pad))
     ((symbolp text)
      (funcall fmtter index (symbol-name text) pad))
     ((not (stringp text))
      (error "bad arg to hydra char formatter" text))
     ((s-contains? "|" text)
      (concat (string-replace "|" "" text))) ;; pad))
     ((s-contains? "%" text)
      (concat text pad))
     ((not (s-contains? "_" text))
      (funcall fmtter index text pad))
     (t
      (concat text)) ;;(make-string (s-count-matches "_" text) pad-chr)))
     )
    )
  )

(defun macro-tools--hydra-format-str-len (text)
  (max macro-tools--hydra-format-min-column
       (cond ((s-contains? "%" text)
              10)
             (t
              (- (length text) (s-count-matches "_" text)))
             ))
  )

;;;###autoload
(defun macro-tools--hydra-format-columns (&rest columns)
  " format a bunch of lists of symbols into a hydra doc string
    place in a (format %s (macro-tools--hydra-format-columns ....))

format rules:
blank (symbol) -> ' '
symbol         -> _s_ymbol
s_y_mbol       -> s_ymbol
%text          -> %text
|text          -> text
text           -> _t_ext
 "
  (let* ((fmt (mapcar (-partial #'mapcar #'macro-tools--hydra-first-char-format) columns))
         (titles (mapcar #'car columns))
         (max-row (apply #'max (mapcar #'length columns)))
         (pad-char ? )
         (empty-pad-amnt 0)
         (concat-str "")
         padded
         fmt-columns
         header
         )
    (cl-loop for column in fmt
             do
             (let* ((longest (apply #'max (mapcar #'macro-tools--hydra-format-str-len column)))
                    (empty-lines (mapcar (-partial (-compose (-partial #'concat concat-str) #'make-string) longest)
                                         (make-list (max 0 (- max-row (length column))) pad-char)))
                   )
               (push (mapcar #'(lambda (x)
                               (s-pad-right (+ longest (s-count-matches "_" x))
                                            (char-to-string pad-char)
                                            x))
                             (append column empty-lines))
                     padded)
               )
             )
    (setq fmt-columns (mapcar (-rpartial #'string-join " | ")
                              (apply #'-zip-lists (reverse padded))
                              )
          header (car fmt-columns)
          )
    (string-join (append (list header
                               (make-string (- (length header) (* 4 (length columns))) ?-))
                         (cdr fmt-columns))
     "\n")
    )
  )


;;;###autoload
(defun macro-tools--hydra-doc (var)
  (if var 1 0)
  )

;;;###autoload
(defun macro-tools--hydra-push (func)
  (push func macro-tools--hydra-stack)
  )

;;;###autoload
(defun macro-tools--hydra-pop ()
  (interactive)
  (when macro-tools--hydra-stack
    (funcall-interactively (pop macro-tools--hydra-stack))
    )
  )

;;;###autoload
(defun macro-tools--hydra-clear ()
  (interactive)
  (setq macro-tools--hydra-stack nil)
  )

(provide 'macro-tools--hydra)
