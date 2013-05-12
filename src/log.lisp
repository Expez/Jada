(in-package :jada)

(defvar *log* (make-array 100 :fill-pointer 0 :adjustable t)
  "The log holding all our log entries.")

(defvar *log-file*
  (if (and (boundp '*test*) *test*)
      "test-log"
      "~/.jada/log"))

(defun create-log-entry (&optional date weight protocol kcal prot fat carbs)
  "Creates a log entry using default values from the previous log entry for
 tdee and protocol."
  (let* ((prototype-entry (list :date (current-date) :weight 0
                                :protocol '+20-20 :kcal 0 :prot 0 :fat 0
                                :carbs 0))
         (prev-entry (or (most-recent-log-entry) prototype-entry))
         (date (or date (current-date)))
         (new-entry (list :date date :weight weight :protocol protocol
                          :kcal kcal :prot prot :fat fat :carbs carbs)))
    (initialize-log-entry new-entry prev-entry)))

(defun initialize-log-entry (new-entry prototype-entry)
  (loop for (key value) on new-entry by #'cddr
     if (not (or (eql key :protocol) (eql key :date) (eql key :tdee)))
     do (unless (getf new-entry key)
          (setf (getf new-entry key) 0))
     else
     do (unless (getf new-entry key)
          (setf (getf new-entry key) (getf prototype-entry key)))
     finally (return new-entry)))

(defmacro create-getters (slots)
  `(progn
     ,@(loop for slot in slots collecting
            `(defun ,(intern (concatenate 'string "GET-" (symbol-name slot)))
                 (log-entry)
               (getf log-entry ,(intern (symbol-name slot) :keyword))))))

(defmacro create-setters (slots)
  `(progn
     ,@(loop for slot in slots collecting
            `(defun ,(intern (concatenate 'string "SET-" (symbol-name slot)))
                 (log-entry value)
               (setf (getf log-entry ,(intern (symbol-name slot) :keyword))
                     value)
               (save-log)))))

(create-getters (date weight protocol kcal prot fat carbs))
(create-setters (date weight protocol kcal prot fat carbs))

(defun current-date ()
  "Returns a plist with :day :month :year representing the current
  date."
  (multiple-value-bind (s m h day month year) (get-decoded-time)
    (declare (ignore s m h))
    (list :day day :month month :year year)))

(defun log-meal (food)
  (let ((log-entry (today))
        (food (remf food :name)))
    (loop
       for (key delta) on food by #'cddr
       for current-value = (getf log-entry key) do
         (setf (getf log-entry key) (+ current-value delta))
       finally (update-todays-entry log-entry))))

(defun update-todays-entry (new-entry)
  (unless (= (length *log*) 0)
    (vector-pop *log*))
  (vector-push-extend new-entry *log*)
  (save-log))

(defun most-recent-log-entry ()
  (if (= (length *log*) 0)
      nil
      (elt *log* (- (length *log*) 1))))

(defun add-log-entry (entry)
  (vector-push-extend entry *log*)
  (save-log))

(defun today ()
  "Returns the log entry matching today"
  (let ((date (getf (most-recent-log-entry) :date)))
    (if (equal date (current-date))
        (most-recent-log-entry)
        (progn
          (add-log-entry (create-log-entry))
          (most-recent-log-entry)))))

(defun save-log ()
  "Saves the log to disk"
  (with-open-file (out (ensure-directories-exist *log-file*) :direction
                       :output :if-exists :supersede)
    (with-standard-io-syntax
      (print *log* out))))

(defun load-log ()
  "Loads the log from disk"
  (with-open-file (in *log-file* :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
        (let ((log (read in)))
          (setf (fill-pointer *log*) (length log))
          (loop for e across log
               for i = 0 then (incf i) do
               (setf (elt *log* i) e)))))))
