(in-package :jada)

(defun parse-input (input)
  "Parse user input and return a function and the arguments given."
  (let ((tokens (cl-ppcre:split "[ \t\n]+" input)))
    (cond ((equal (car tokens) "w")
           (values #'weight (list (read-from-string (second tokens))))))))

(defun weight (w)
  "Adds entry for today's weight."
  w)

(defun safely-read-from-string (str &rest read-from-string-args)
  "Read an expression from the string STR, with *READ-EVAL* set
to NIL. Any unsafe expressions will be replaced by NIL in the
resulting S-Expression."
  (let ((*read-eval* nil))
    (ignore-errors
      (apply 'read-from-string str read-from-string-args))))
