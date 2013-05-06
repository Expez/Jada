(in-package :jada)

(defclass command ()
  ()
  (:documentation "A command class, holding all the data necessary to run the
command the user requested."))

(defclass log-weight (command)
  ((weight :reader weight :initarg :weight)))

(defun tokenize (input)
  "Parse user input and return a function and the arguments given."
  (cl-ppcre:split "[ ]+" (string-downcase input)))

(defun safely-read-from-string (str &rest read-from-string-args)
  "Read an expression from the string STR, with *READ-EVAL* set
to NIL. Any unsafe expressions will be replaced by NIL in the
resulting S-Expression."
  (let ((*read-eval* nil))
    (ignore-errors
      (apply 'read-from-string str read-from-string-args))))

(defun create-log-weight-command (input)
  (let ((tokens (tokenize input)))
    (if (= (length tokens) 2)
        (let ((weight (safely-read-from-string (second tokens))))
          (if (and (not (null weight))
                   (numberp weight))
              (make-instance 'log-weight :weight weight)
              (error 'invalid-input :input input)))
        (error 'invalid-input :input input))))

(defun create-command (input)
     (cond
       ((eql (char input 0) #\w)) (create-add-weight-command input)
       (t (error invalid-input :input input))))

(defgeneric execute (c)
  (:documentation "Executes the user-specified command."))

(defmethod execute ((log-weight command))
  )
