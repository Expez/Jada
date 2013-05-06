(in-package :jada)

(define-condition invalid-input (error)
  ((input :reader input :initarg :input)))

(defvar *log* (make-array 100 :fill-pointer 0 :adjustable t)
  "The log holding all our log entries.")

(defvar *food-db (make-hash-table :test #'equal)
  "Database holding entries for various food items.")

(defun create-command (input)
  (cond
    ((eql (char input 0) #\w)) (create-add-weight-command input)
    (t (error invalid-input :input input))))

(defun get-user-input ()
  (format *query-io* "> ")
  (force-output)
  (read-line *query-io*))

(defun run ()
  (loop (execute (create-command (get-user-input)))))
