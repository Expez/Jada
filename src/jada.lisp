(in-package :jada)

(define-condition invalid-input (error)
  ((input :reader input :initarg :input)))

(define-condition invalid-food-name (invalid-input)
  ((name :reader name :initarg name)))

(defun get-user-input ()
  (format *query-io* "> ")
  (force-output)
  (read-line *query-io*))

(defgeneric print-error-message (condition)
  (:documentation "Prints the error message associated with the condition."))

(defmethod print-error-message ((c invalid-input))
  (print-msg "Invalid input: ~a~%" (input c)))

(defmethod print-error-message ((c invalid-food-name))
  (print-msg "Food '~a' not found in database.~%" (input c)))

(defun print-msg (msg &rest data)
  (apply #'format *query-io* msg data))

(defun run ()
  (loop
     (handler-case (execute (create-command (get-user-input)))
       (invalid-input (c)
         (print-error-message c)))))
