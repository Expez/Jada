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
(def-command eat food amount)
(def-command barf puke amount)
(def-command ls)
(def-command display food)
(def-command remaining)
(def-command set-protocol protocol)
(def-command today)
(def-command no)

(defun tokenize (input &key start)
  "Parse user input and return a function and the arguments given."
  (cl-ppcre:split "[ ]+" (string-downcase input) :start start))

(defun safely-read-from-string (str &rest read-from-string-args)
  "Read an expression from the string STR, with *READ-EVAL* set
to NIL. Any unsafe expressions will be replaced by NIL in the
resulting S-Expression."
  (let ((*read-eval* nil))
    (ignore-errors
      (apply 'read-from-string str read-from-string-args))))

(defun verify-num-tokens (input num-expected-tokens &key (or nil or-p))
  "Checks if we've been passed the correct number of tokens.
Raises an error if not."
  (let* ((length (length (tokenize input)))
         (max-length (if or-p (max or num-expected-tokens) num-expected-tokens)))
    (unless (<= length max-length)
      (error 'invalid-input :input input))))

(defun create-log-weight-command (input)
  (verify-num-tokens input 2)
  (let* ((tokens (tokenize input))
         (weight (safely-read-from-string (second tokens))))
    (if (and (not (null weight))
             (numberp weight))
        (make-instance 'log-weight :weight weight)
        (error 'invalid-input :input input))))

(defun create-add-food-command (input)
  (let* ((food-string (subseq input (1+ (position #\Space input))))
         (food (food-from-string food-string)))
    (make-instance 'add-food :food food)))

(defun create-quit-command (input)
  (verify-num-tokens input 1)
  (make-instance 'quit))

(defun string-to-symbol (s)
  (intern (string-upcase s)))

(defun input-sans-command (input)
  (subseq input (1+ (search " " input))))

(defun extract-multiplier (input)
  (let ((tokens (tokenize input)))
    (if (= (length tokens) 2)
        1
        (safely-read-from-string (second tokens)))))

(defun create-eat-command (input)
  (verify-num-tokens input 2 :or 3)
  (flet ()
    (let* ((food-name (extract-food-name (input-sans-command input)))
           (multiplier (extract-multiplier input))
           (food (lookup-food food-name)))
      (unless food
        (error 'invalid-food-name food-name))
      (make-instance 'eat :food food :amount multiplier))))

(defun create-display-command (input)
  (verify-num-tokens input 2)
  (let* ((food-name (extract-food-name (input-sans-command input)))
         (food (lookup-food food-name)))
    (make-instance 'display :food food)))

(defun create-barf-command (input)
  (verify-num-tokens input 2 :or 3)
  (let* ((food-name (extract-food-name (input-sans-command input)))
         (amount (extract-multiplier input))
         (food (lookup-food food-name))
         (puke (as-puke food)))
    (unless food
      (error 'invalid-food-name food-name))
    (make-instance 'barf :puke puke :amount amount)))

(defun create-remaining-command (input)
  (verify-num-tokens input 1)
  (make-instance 'remaining))

(defun create-ls-command (input)
  (verify-num-tokens input 1)
  (make-instance 'ls))

(defun create-set-protocol-command (input)
  (verify-num-tokens input 2)
  (let ((protocol (extract-food-name (input-sans-command input))))
    (make-instance 'set-protocol :protocol protocol)))

(defun create-today-command (input)
  (verify-num-tokens input 1)
  (make-instance 'today))

(defun create-null-command ()
  (make-instance 'no))

(defun create-command (input)
  (cond
    ((equal input "") (create-null-command))
    ((eql (char input 0) #\w) (create-log-weight-command input))
    ((eql (char input 0) #\a) (create-add-food-command input))
    ((eql (char input 0) #\q) (create-quit-command input))
    ((eql (char input 0) #\e) (create-eat-command input))
    ((eql (char input 0) #\b) (create-barf-command input))
    ((eql (char input 0) #\l) (create-ls-command input))
    ((eql (char input 0) #\p) (create-display-command input))
    ((eql (char input 0) #\t) (create-today-command input))
    ((equal (string-downcase (subseq input 0 2)) "pr")
     (create-set-protocol-command input))
    ((eql (char input 0) #\r) (create-remaining-command input))
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
  (with-accessors ((food food) (amount amount)) eat-food-command
    (log-meal food amount)))

(defmethod execute ((barf-command barf))
  (with-accessors ((puke puke) (amount amount)) barf-command
    (log-meal puke amount)))

(defmethod execute ((ls-command ls))
  (print-food-db))

(defmethod execute ((display-command display))
  (with-accessors ((food food)) display-command
    (print-food food)))

(defmethod execute ((remaining-command remaining))
  (print-remaining-macros))

(defmethod execute ((today-command today))
  (print-info-about-today)
  (format *query-io* "~%")
  (print-remaining-macros))

(defmethod execute ((set-protocol-command set-protocol))
  (with-accessors ((protocol protocol)) set-protocol-command
    (set-protocol (today) protocol)))

(defmethod execute ((null-command no))
  )

(defmethod execute ((quit-command quit))
  (sb-ext:exit))
