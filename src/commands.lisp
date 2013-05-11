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
(def-command quit)
(def-command add-food food)

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

(defun verify-num-tokens (input num-expected-tokens)
  "Checks if we've been passed the correct number of tokens.
Raises an error if not."
  (unless (= (length (tokenize input)) num-expected-tokens)
    (error 'invalid-input :input input)))

(defun create-log-weight-command (input)
  (verify-num-tokens input 2)
  (let* ((tokens (tokenize input))
         (weight (safely-read-from-string (second tokens))))
    (if (and (not (null weight))
             (numberp weight))
        (make-instance 'log-weight :weight weight)
        (error 'invalid-input :input input))))

(defun create-add-food-command (input)
  (verify-num-tokens input 5)
  (let ((tokens (rest (tokenize input))))
    (mapc #'safely-read-from-string tokens)
    (let* ((name (first tokens))
             (kcal (second tokens))
             (prot (third tokens))
             (fat (fourth tokens))
             (carbs (fifth tokens))
             (food (make-instance 'food :name name :kcal kcal
                                                    :prot prot :fat fat
                                                    :carbs carbs)))
      (make-instance 'add-food :food food))))

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

(defmethod execute ((add-food-command command))
  (with-accessors ((food food)) add-food-command
    (setf (get-hash (name food)) food)))

