(in-package :jada)

(defclass command ()
  ()
  (:documentation "A command class, holding all the data necessary to run the
command the user requested."))

(defmacro def-command (name &rest slots)
  "Macro to create a class representing an executable command.  With readers for
the given slots."
  `(defclass ,name (command)
     ,(loop
         for slot in slots collecting
           `(,slot :reader ,slot :initarg ,(intern (symbol-name slot) :keyword)))))

(def-command log-weight weight)
(def-command quit)
(def-command add-food food)
(def-command eat food)

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
  (verify-num-tokens input 6)
  (let ((tokens (rest (tokenize input))))
    (mapc #'safely-read-from-string tokens)
    (let* ((name (first tokens))
             (kcal (second tokens))
             (prot (third tokens))
             (fat (fourth tokens))
             (carbs (fifth tokens))
             (food (create-food name kcal prot fat carbs)))
      (make-instance 'add-food :food food))))

(defun create-quit-command (input)
  (verify-num-tokens input 1)
  (make-instance 'quit))

(defun create-eat-command (input)
  (verify-num-tokens input 2)
  (let* ((food-name (second (tokenize input)))
         (food (lookup-food food-name)))
    (unless food
      (error 'invalid-food-name food-name))
    (make-instance 'eat :food food)))

(defun create-command (input)
  (cond
    ((eql (char input 0) #\w) (create-log-weight-command input))
    ((eql (char input 0) #\a) (create-add-food-command input))
    ((eql (char input 0) #\q) (create-quit-command input))
    ((eql (char input 0) #\e) (create-eat-command input))
    (t (error 'invalid-input :input input))))

(defgeneric execute (c)
  (:documentation "Executes the user-specified command."))

(defmethod execute ((log-weight-command log-weight))
  (with-accessors ((new-weight weight)) log-weight-command
    (let ((todays-log-entry (today)))
      (set-weight todays-log-entry new-weight))))

(defmethod execute ((add-food-command add-food))
  (with-accessors ((food food)) add-food-command
    (add-food food)))

(defmethod execute ((eat-food-command eat))
  (with-accessors ((food food)) eat-food-command
    (log-meal food)))

(defmethod execute ((quit-command quit))
  (sb-ext:exit))
