(in-package :jada)

(defclass command ()
  ()
  (:documentation "A command class, holding all the data necessary to run the
command the user requested."))

(defmacro def-command (name &rest slots)
  `(defclass ,name (command)
     ,(loop
         for slot in slots collecting
           `(,slot :reader ,slot :initarg ,(intern (symbol-name slot) :keyword)))))

(def-command log-weight weight)

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

(defun create-add-food-command (input)
  (let ((tokens (tokenize input)))
    (if (= (length tokens) 5)
        (progn
          (mapc #'safely-read-from-string tokens)
          (if-let ((name (first tokens))
                   (kcal (second tokens))
                   (prot (third tokens))
                   (fat (fourth tokens))
                   (carbs (fifth tokens)))
            (make-instance 'food :name name :kcal kcal
                           :prot prot :fat fat :carbs carbs)
            (error 'invalid-input :input input)))
        (error 'invalid-input :input input))))

(defun create-command (input)
  (cond
    ((eql (char input 0) #\w) (create-log-weight-command input))
    ((eql (char input 0) #\f) (create-add-food-command input))
    (t (error 'invalid-input :input input))))

(defgeneric execute (c)
  (:documentation "Executes the user-specified command."))

(defmethod execute ((log-weight-command command))
  (with-accessors ((new-weight weight)) log-weight-command
    (setf (weight (today)) new-weight))
  (save-log))

