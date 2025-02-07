;;; transient.el -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'cl-lib)
  (require 'transient)
  (require 'eieio-core)
  (require 'macro-tools--util)

  (declare-function pop-plist-from-body! "jg-misc-macros")
  (declare-function eieio-make-class-predicate "eieio-core")
  )

(require 'transient)
(cl-assert (featurep 'transient) "transient is needed")
(cl-assert (fboundp 'transient-prefix) "transient prefix is needed")

(defvar macro-tools--transient-quit!
  [
   ""
   [("q" "Quit Local" transient-quit-one)]
   [("Q" "Quit Global" transient-quit-all)]
   [("|" "Quit" transient-quit-all)]
   ]
  " Reusable simple quit for transients "
  )
(defvar macro-tools--transient-hooks nil "transient hook store, to create a one stop shop of hooks")

(defclass macro-tools--transient-group (transient-prefix)
  ((description :initarg :description :initform nil))
  "Prefix Subclassed to hold a description"
  )

(cl-defmethod macro-tools--transient-format-description :before ((obj macro-tools--transient-group))
  "Format the description by calling the next method.  If the result
is nil, then use \"(BUG: no description)\" as the description.
If the OBJ's `key' is currently unreachable, then apply the face
`transient-unreachable' to the complete string."
  (or (funcall (oref obj description))
      (propertize "(JG BUG: no description)" 'face 'error))
)

;;;###autoload
(defun macro-tools--transient-simple-fmt (name key)
  (format "%s  : %s"
          (make-string (max 0 (- 3 (length key))) 32)
          name)
  )

;;;###autoload (defalias 'transient-title-mode-formatter 'transient-mode-fmt)

;;;###autoload
(defun transient-mode-fmt (name mode key)
  (format "%s%s : %s"
          (make-string (max 0 (- 3 (length key))) 32)
          (fmt-as-bool! (when (and (symbolp mode) (boundp mode)) (symbol-value mode)))
          name
          )
  )

;;;###autoload (defalias 'transient-title-var-formatter 'transient-var-fmt)

;;;###autoload
(defun transient-var-fmt (name val key)
  (format "%s%s : %s"
          (make-string (max 0 (- 3 (length key))) 32)
          (fmt-as-bool! val)
          name
          )
  )

;;;###autoload
(defun transient-hook-fmt (name hook mode key)
  (let ((mode-active (fmt-as-bool! (symbol-value mode)))
        (spacer 32)
        (in-hook (fmt-as-bool! (-contains? hook mode)))
        )
    (format "%s(%s∈%s) : %s"
            (make-string (max 0 (- 3 (length key))) spacer)
            mode-active
            in-hook
            name
            )
    )
  )

;;;###autoload
(defun transient-args! ()
  " utility for easily getting all current transient args "
      (transient-args transient-current-command)
  )

;;;###autoload
(defun transient-args? (&optional key)
  "utility for easily testing the args in transient"
  (member (if (symbolp key) (symbol-name key) key) (transient-args transient-current-command))
  )

;;;###autoload
(defun transient-init! (arg)
  " utility for simply setting a transient init value "
  (-partial #'(lambda (val obj)
                (oset obj value val))
            arg)
  )

;;;###autoload (defalias 'transient-make-mode-toggle! 'transient-toggle-mode!)

;;;###autoload (autoload 'transient-toggle-mode! "transient-macros" nil nil t)
(cl-defmacro transient-toggle-mode! (mode () docstr &key desc key heading mode-var)
  " Macro to define a transient suffix for toggling a mode easier "
  (declare (indent defun))
  (let* ((fullname (intern (format "transient-macro-toggle-%s" (symbol-name mode))))
         (name (let ((str (or desc docstr (symbol-name mode))))
                 (when heading
                   (put-text-property 0 (length str) 'face 'transient-heading str))
                 str))
         (desc-fn `(lambda () (transient-mode-fmt
                               ,(or desc docstr name)
                               ,(or mode-var mode)
                               ,key)))
         )
    `(progn
       (transient-define-suffix ,fullname ()
         ,docstr
         :transient t
         ,@(when key (list :key key))
         :description ,desc-fn
         (interactive)
         (,mode 'toggle)
         )
       (quote ,fullname)
       )
     )
  )

;;;###autoload (defalias 'transient-make-var-toggle! 'transient-toggle-var!)

;;;###autoload (autoload 'transient-toggle-var! "transient-macros" nil nil t)
(cl-defmacro transient-toggle-var! (name () docstr &key var desc key)
  " Macro to define a transient suffix for toggling a bool variable "
  (declare (indent defun))
  (let* ((fullname (intern (format "transient-macro-toggle-%s" (symbol-name name))))
         (desc-fn `(lambda () (transient-var-fmt
                               ,(or desc docstr (symbol-name name))
                               ,var
                               ,key)))
         )
    `(progn
       (defvar ,var nil)
       (transient-define-suffix ,fullname ()
         ,docstr
         :transient t
         :description ,desc-fn
         ,@(when key (list :key key))
         (interactive)
         (setq ,var (not ,var))
         )
       (quote ,fullname)
       )
    )
)

;;;###autoload (autoload 'transient-toggle-hook! "transient-macros" nil nil t)
(cl-defmacro transient-toggle-hook! (name () docstr &key desc key hook fn global)
  "Make a transient toggle to add/remove `fn' to/from `hook'"
  (declare (indent defun))
  (let* ((fullname (intern (format "transient-macro-toggle-hook-%s" name)))
         (hook-targets (ensure-hook! (unquote! hook) :plural t))
         (desc-fn `(lambda () (transient-hook-fmt ,(or desc docstr (symbol-name name))
                                                  (eval (car (list ,@hook-targets)))
                                                  ,fn
                                                  ,key)))
         (globalhook (when global (gensym! 'transient-global-hook-add fullname)))
         (xsym (gensym))
         (ysym (gensym))
         (presym (gensym))
         )
    `(progn
       (transient-define-suffix ,fullname (arg)
         ,docstr
         :transient t
         :description ,desc-fn
         ,@(when key (list :key key))
         (interactive "P")
         (if arg
           (cl-loop for x in (list ,@hook-targets)
                    if (-contains? (eval x) ,fn)
                    do (remove-hook x ,fn)
                    and do (funcall ,fn -1)
                    else
                    do (add-hook x ,fn)
                    )
           (funcall ,fn 'toggle)
           )
         )
       ,(when global
           `(progn
              (defun ,globalhook (,presym ,xsym ,ysym)
                (transient-guarded-append! ,presym (quote ,fullname) (,xsym ,ysym))
                )
              (add-hook 'macro-tools--transient-hooks (function ,globalhook))
              )
           )
       (quote ,fullname)
       )
    )
  )

;;;###autoload (defalias 'transient-make-call! 'transient-call!)
;;;###autoload (autoload 'transient-call! "transient-macros" nil nil t)
(cl-defmacro transient-call! (name () docstr &body body &key key desc interactive (transient t) no-buff &allow-other-keys)
  " create a transient suffix of `name`
with a string or format call, which executes the body
 "
  (declare (indent defun))
  (let* ((fullname (intern (format "transient-macro-call-%s" (if (stringp name) name
                                                           (symbol-name name)))))
         (clean-body (pop-plist-from-body! body))
         (body-call (if interactive
                        `((call-interactively ,@clean-body))
                      clean-body))
         (wrapped-call (if no-buff
                           body-call
                         `((with-current-buffer (or transient--original-buffer
                                                   (current-buffer))
                            ,@body-call))))
        )
    `(progn
       (transient-define-suffix ,fullname ()
         ,docstr
         :transient ,transient
         ,@(when key (list :key key))
         :description (lambda () ,(or desc docstr name))
         (interactive)
         ,@wrapped-call
         )
       (quote ,fullname)
       )
    )
  )

;;;###autoload (defalias 'transient-make-subgroup! 'transient-subgroup!)
;;;###autoload (autoload 'transient-subgroup! "transient-macros" nil nil t)
(cl-defmacro transient-subgroup! (name () docstring &body body &key key (desc nil) rows &allow-other-keys)
  " Make prefix subgroup bound to const `name`, as the triple (keybind descr prefix-call),
which can then be included in other transient-prefixes as just `name`
with text properties to mark it so

auto-wraps the body if rows is nil, ie: each provided vector is a column


TODO: kwd based row and column assembly
ie: :row [:col [] :col [] :col []] :row []
 "
  (declare (indent defun))
  (let* ((prefix-name (gensym! name 'body))
         (descfn-name (gensym! name 'descfn))
         (source (macroexp-file-name))
         (desc-result (pcase (upfun! (or desc (symbol-name name)))
                        ((and str (pred stringp))
                         (put-text-property 0 (length str) 'face 'transient-heading str)
                         str)
                        ((and fn (pred functionp))
                         `(let ((result (funcall (function ,fn))))
                            (put-text-property 0 (length result) 'face 'transient-heading result)
                            result
                            ))
                        ))
         (clean-body (pop-plist-from-body! body))
         (wrapped-body (if rows
                           clean-body
                         (list `[:description ,descfn-name
                                 ,@clean-body
                                 ])))
         )
    (put descfn-name 'source source)
    (put prefix-name 'source source)
    `(progn
       ;; As Value for use in the triple
       (set (quote ,descfn-name) (lambda nil (interactive) "Generated desc fn for transient" ,desc-result))
       ;; As Fun for use in group :description
       (fset (quote ,descfn-name) ,descfn-name)
       (transient-define-prefix ,prefix-name ()
         ,docstring
         ,@wrapped-body
         macro-tools--transient-quit!
         )
       (defconst ,name
         (list
          ,key
          (quote ,prefix-name)
          :description ,descfn-name
          )
         )
       (quote ,name)
       )
    )
  )

;;;###autoload (autoload 'transient-guarded-insert! "transient-macros" nil nil t)
(cl-defmacro transient-guarded-insert! (prefix suffix (&rest loc) &key (col-len 4))
  "Insert a subgroup into prefix, but in a new column if necessary"
  (declare (indent defun))
  (let* ((loc-end (cl-concatenate 'list loc '(-1)))
         (new-row `(transient-append-suffix ,prefix (quote ,loc-end) ,suffix))
         (new-col `(transient-append-suffix ,prefix (quote ,loc) `[ ,,suffix ] ))
         )
    ;; manually construct the backquote list,
    ;; better controlling pcase patterns
    (backquote-list* 'pcase
                     `(transient-get-suffix ,prefix (quote ,loc))
                     (list ;; patterns
                      (list ;; pattern 1
                       (list 'and
                             (list '\` [1 transient-column nil ,x])
                             (list 'guard '(not (null x)))
                             (list 'guard '(< (length x) 4)))
                       ;; p1 result
                       new-row
                       ''new-row
                       )
                      ;; p2
                      `(_ ,new-col 'new-col)
                      )
                     )
    )
  )

;;;###autoload (autoload 'transient-guarded-append! "transient-macros" nil nil t)
(cl-defmacro transient-guarded-append! (prefix suffix (&rest loc) &key (col-len 3))
  "Insert a single suffix into a subgroup"
  (declare (indent defun))
  (let* ((loc-list (cl-concatenate 'list loc))
         (loc-end (cl-concatenate 'list loc-list '(-1)))
         (target (pcase suffix
                   ((pred consp)
                    (cadr suffix))
                   (_ suffix)))
         (new-row `(transient-append-suffix (cadr ,prefix) (mapcar #'eval (list ,@loc-end)) (quote (,target))))
         (new-col (backquote-list* 'transient-append-suffix
                                   `(cadr ,prefix)
                                   (list
                                    `(mapcar #'eval (list ,@loc-list))
                                   `[(,target)]
                                   )
                                   ))
         )

    (backquote-list* 'pcase
                     `(transient-get-suffix (cadr ,prefix) (mapcar #'eval (list ,@loc-list)))
                     (list ;; patterns
                      (list ;; p1
                       (list 'and
                             (list '\` [1 transient-column nil ,xsym])
                             t
                             (list 'guard `(not (null xsym)))
                             (list 'guard `(< (length xsym) 4))
                             )

                       ;; result
                       new-row
                       ''new-row
                       )
                      ;; fallback
                      `(_
                        ,new-col
                        'new-col)
                      )
       )
    )
  )

;;;###autoload (autoload 'transient-setup-hook! "transient-macros" nil nil t)
(cl-defmacro transient-setup-hook! (name () &optional docstr &rest body)
  "Create a function to trigger rebuilding of a transient,
also calling any registered addition hooks.

ie: (progn (build-base-transient) (run-hooks 'base-transient-addition-hook))
"
  (declare (indent defun))
  (let* ((builder-fn-sym (gensym! name "builder"))
         (hook-sym (gensym! name "hook"))
         (hookdoc (format "Hook for functions which modify %s, run when %s is called" name builder-fn-sym))
         (docstring (if (stringp docstr)
                        docstr
                      (push docstr body)
                      (format "Macro Generated %s transient builder" name)))
         )
    `(progn
       (defvar ,hook-sym nil ,hookdoc)
       (defun ,builder-fn-sym (&optional arg)
         ,docstring
         (interactive "P")
         (message "-- Building Base %s" (quote ,name))
         ,@body
         (unless arg
           (message "-- Running %s Extension hooks" (quote ,name))
           (run-hooks (quote ,hook-sym))
           )
         (message "-- Finished %s" (quote ,name))
         )
       )
    )
  )

(provide 'macro-tools--transient)
