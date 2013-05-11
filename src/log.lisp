(in-package :jada)

(defvar *log* (make-array 100 :fill-pointer 0 :adjustable t)
  "The log holding all our log entries.")

(defvar *log-file* "~/.jada/log")

(defun create-log-entry (date weight protocol kcal prot fat carbs)
  (list :date date :weight weight :protocol protocol
        :kcal kcal :prot prot :fat fat :carbs carbs))

(defmacro create-readers (slots)
  `(progn
    ,@(loop for slot in slots collecting
           `(defun ,(intern (concatenate 'string "GET-" (symbol-name slot)))
                (log-entry)
              (getf log-entry ,(intern (symbol-name slot) :keyword))))))

(create-readers (list date weight protocol kcal prot fat carbs))

(defmacro create-setters (slots)
  `(progn
    ,@(loop for slot in slots collecting
                `(defun ,(intern (concatenate 'string "SET-" (symbol-name slot)))
                     (log-entry value)
                   (setf (getf log-entry ,(intern (symbol-name slot) :keyword))
                         value)))))

(create-setters (list date weight protocol kcal prot fat carbs))

(defun current-date ()
  "Returns a plist with :day :month :year representing the current
  date."
  (multiple-value-bind (s m h day month year) (get-decoded-time)
    (declare (ignore s m h))
    (list :day day :month month :year year)))

(defun most-recent-log-entry ()
  (first *log*))

(defun add-log-entry (entry)
  (vector-push-extend entry *log*)
  (save-log))

(defun today ()
  "Returns the log entry matching today"
  (let  ((date (getf (most-recent-log-entry) :date)))
    (if (equal date (current-date))
        (most-recent-log-entry)
        (progn
          (push (make-instance 'log-entry :date (current-date)) *log*)
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
        (setf *log* (read in))))))
