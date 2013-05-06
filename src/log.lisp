(in-package :jada)

(defvar *log* nil
  "The log holding all our log entries.")

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

(defun latest-log-entry ()
  (first *log*))

(defun add-log-entry (entry)
  (push entry *log*))

