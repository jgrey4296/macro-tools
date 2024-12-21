;;; transient.el -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'cl-lib)
  (require 'transient)
  (require 'eieio-core)
  (require 'jg-misc-macros)

  (declare-function pop-plist-from-body! "jg-misc-macros")
  (declare-function eieio-make-class-predicate "eieio-core")

  )

(defvar transient-quit!
  [
   ""
   [("q" "Quit Local" transient-quit-one)]
   [("Q" "Quit Global" transient-quit-all)]
   [("|" "Quit" transient-quit-all)]
   ]
  " Reusable simple quit for transients "
  )

(defclass transient-macro--group (transient-prefix)
  ((description :initarg :description :initform nil))
  "Prefix Subclassed to hold a description"
  )

(cl-defmethod transient-format-description :before ((obj transient-macro--group))
  "Format the description by calling the next method.  If the result
is nil, then use \"(BUG: no description)\" as the description.
If the OBJ's `key' is currently unreachable, then apply the face
`transient-unreachable' to the complete string."
  (or (funcall (oref obj description))
      (propertize "(JG BUG: no description)" 'face 'error))
)

;;;###autoload
(defun transient-simple-formatter (name key)
  (format "%s  : %s"
          (make-string (max 0 (- 3 (length key))) 32)
          name)
  )

;;;###autoload (defalias 'transient-title-mode-formatter 'transient-mode-fmt)

;;;###autoload
(defun transient-mode-fmt (name mode key)
  (format "%s%s : %s"
          (make-string (max 0 (- 3 (length key))) 32)
          (fmt-as-bool! (when (boundp mode) (symbol-value mode)))
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
(cl-defmacro transient-toggle-hook! (name () docstr &key desc key hook fn)
  "Make a transient toggle to add/remove `fn' to/from `hook'"
  (declare (indent defun))
  (let* ((fullname (intern (format "transient-macro-toggle-hook-%s" name)))
         (hook-target (ensure-hook! (unquote! hook)))
         (desc-fn `(lambda () (transient-hook-fmt ,(or desc docstr (symbol-name name))
                                                  ,hook-target
                                                  ,fn
                                                  ,key)))
         )
    `(progn
       (transient-define-suffix ,fullname (arg)
         ,docstr
         :transient t
         :description ,desc-fn
         ,@(when key (list :key key))
         (interactive "P")
         (if arg
             (funcall ,fn 'toggle)
           (if (not (-contains? ,hook-target ,fn))
               (add-hook (quote ,hook-target) ,fn)
             (remove-hook (quote ,hook-target) ,fn)
             (funcall ,fn -1))
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
(cl-defmacro transient-subgroup! (name () docstring &body body &key key (desc nil) &allow-other-keys)
  " Make prefix subgroup bound to const `name`, as the triple (keybind descr prefix-call),
which can then be included in other transient-prefixes as just `name`
with text properties to mark it so
'
 "
  (declare (indent defun))
  (let ((prefix (gensym))
        (docfn (gensym))
        (doc (pcase (or desc (symbol-name name))
               ((and str (pred stringp))
                (put-text-property 0 (length str) 'face 'transient-heading str)
                str)
               ((and fn (pred functionp))
                `(let ((result (funcall ,fn)))
                   (put-text-property 0 (length result) 'face 'transient-heading result)
                   result
                   ))
               ))
        (clean-body (pop-plist-from-body! body))
        )
    `(progn
       (transient-define-prefix ,prefix ()
         ,docstring
         ,@clean-body
         transient-quit!
         )
       (defun ,docfn nil ,doc)
       (defconst ,name (list ,key (quote ,docfn) (quote ,prefix)))
       (quote ,name)
       )
    )
  )

(provide 'transient-macros)
