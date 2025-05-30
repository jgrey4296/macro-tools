;;; related-files.el -*- lexical-binding: t; -*-

;; Macros for easily building related-files functions for projectile
;; TODO make separate related-dired fn
(eval-when-compile
  (require 'cl-lib)
  (require 'f)
  (require 'macro-tools--util)
)

(defconst macro-tools--related-symbol-separator ":")

(defconst macro-tools--related-doc-str "Macro generated related-files-fn for projectile. ")


(defmacro macro-tools--related-proj-detail (key)
  (declare (indent defun))
  (unless (member (unquote! key) '(project-file marker-files compilation-dir compile configure install package run src-dir test test-dir test-prefix test-suffix related-files-fn))
    (error "Bad Project Key" key))
  `(when-let ((proj (alist-get (projectile-project-type) projectile-project-types)))
     (plist-get proj ,key))
  )

;;;###autoload
(cl-defmacro make-related! (proj-type &rest body
                                      &key (files nil) (tests nil) (binds nil)
                                      &allow-other-keys
                                      )
  " Provide projectile with various :kinds of related file.
by default provides bindings of:
root, fbase, fname, fparent, project

files can be relative to project root, will be expanded later
"
  (declare (indent defun))
  (let ((funcname (gensym! "related-files" proj-type))
        )
    `(fset (function ,funcname)
      (lambda (path)
        ,(format macro-tools--related-doc-str)
        (interactive "b")
        (let* ((path (if (bufferp path) (buffer-file-name path) path))
               (root (projectile-project-root))
               (fbase   (f-base path))
               (fname   (f-filename path))
               (fparent (f-parent path))
               (fparent2 (f-parent fparent))
               (project  (macro-tools--related-proj-detail 'project-file))

               ,@binds

               ,@tests

               result
               )

          (setq result (append
                        ;; Defaults
                        (list :root root
                              :notes "notes.md"
                              :pylint "pylint.toml"
                              :reqs "requirements.txt")
                        (when project
                          (list :config project))
                        ,@(cl-loop for entry in files
                                   if (plist-get entry :when)
                                   collect (list 'when (plist-get entry :when) `(list ,(car entry) ,(plist-get entry (car entry))))
                                   else collect `(list ,@entry)
                                   )
                        ))
          (if (interactive-p)
              (ivy-read (format "Related Files for %s: " fname) result)
            result
            )
          )
        )
      )
    )
)

(provide 'macro-tools--related)
