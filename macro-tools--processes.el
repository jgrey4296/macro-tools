;;; macro-tools--processes.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defun process-sentinel! (target proc status)
  (unless (process-live-p proc)
     (let ((text (with-current-buffer (process-buffer proc)
                   (buffer-string))))
       (with-current-buffer target 'display-buffer-pop-up-window nil
                            (goto-char (point-min))
                            (insert (format "--- Proc: %s\n" (process-name proc)))
                            (insert text)
                            (insert (format "\n--- End Proc: %s.\n" (process-name proc)))
                            )
       )
     ;; (with-current-buffer (process-buffer proc)
     ;;   (erase-buffer))
     (kill-buffer (process-buffer proc))
     )
  )

(defmacro with-process-wrap! (buffer &rest body)
  "A Macro for setting up a process with a sentinel"
  (let ((procs (make-symbol "tempprocs"))
        (sent (make-symbol "sentinel"))
        )
    `(progn
       (with-current-buffer (get-buffer-create ,buffer)
         (erase-buffer))
       (let ((,procs ,@body)
             (,sent (-partial #'jg-process-sentinel! ,buffer))
             )
         (cond ((listp ,procs)
                (message "Applying sentinel to processes %s" ,procs)
                (mapc (-rpartial #'set-process-sentinel ,sent) ,procs))
               (t
                (message "Applying single sentinel %s" ,procs)
                (set-process-sentinel ,procs ,sent))
               )
         (save-selected-window
           (display-buffer ,buffer 'display-buffer-pop-up-window))
         )
       )
    )
  )

(provide 'macro-tools--processes)

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 11, 2025
;; Modified:   January 11, 2025
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
;;; macro-tools--processes.el ends here
