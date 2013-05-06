(in-package :jada)

(define-condition invalid-input (error)
  ((input :reader input :initarg :input)))

(defun get-user-input ()
  (format *query-io* "> ")
  (force-output)
  (read-line *query-io*))

(defun run ()
  (loop (execute (create-command (get-user-input)))))
