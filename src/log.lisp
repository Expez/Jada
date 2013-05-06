(in-package :jada)

(defvar *log* nil
  "The log holding all our log entries.")

(defvar *log-file* "~/.jada/jada.log")

(defclass log-entry ()
  ((weight   :accessor weight   :initarg :weight)
   (protocol :accessor protocol :initarg :protocol)
   (kcal     :accessor kcal     :initarg :kcal)
   (prot     :accessor prot     :initarg :prot)
   (fat      :accessor fat      :initarg :fat)
   (carbs    :accessor carbs    :initarg :carbs)
   (date     :accessor date     :initarg :date)))

(defun current-date ()
  "Returns a plist with :day :month :year representing the current
  date."
  (multiple-value-bind (s m h day month year) (get-decoded-time)
    (declare (ignore s m h))
    (list :day day :month month :year year)))

(defun most-recent-log-entry ()
  (first *log*))

(defun add-log-entry (entry)
  (push entry *log*))

(defun today ()
  "Returns the log entry matching today"
  (with-slots (date) (most-recent-log-entry)
    (if (equal date (current-date))
        (most-recent-log-entry)
        (progn
          (push (make-instance 'log-entry :date (current-date)) *log*)
          (most-recent-log-entry)))))

(defun save-log ()
  "Saves the log to disk"
  (ensure-directories-exist "~/.jada/" :verbose nil)
  (with-open-file (out *log-file* :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print *log* out))))

(defun load-log ()
  "Loads the log from disk"
  (with-open-file (in *log-file* :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
       (setf *log* (read in))))))
