(in-package :jada)

(defvar *food-db (make-hash-table :test #'equal)
  "Database holding entries for various food items.")

(defvar *food-file* "~/.jada/foods")
(defclass food ()
  ((name  :accessor name  :initarg :name)
   (kcal  :accessor kcal  :initarg :kcal)
   (prot  :accessor prot  :initarg :prot)
   (fat   :accessor fat   :initarg :fat)
   (carbs :accessor carbs :initarg :carbs)))

(defmethod same ((f1 food) (f2 food))
  (mapc (lambda (v1 v2) (equalp v1 v2))
        (list (name f1) (name f2) (kcal f1) (prot f1)  (fat f1) (carbs f1))
        (list (name f2) (name f2) (kcal f2) (prot f2)  (fat f2) (carbs f2))))

(defun save-food-db ()
  "Saves the food db to disk"
  (ensure-directories-exist "~/.jada/" :verbose nil)
  (with-open-file (out *food-file* :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print *food-db* out))))

(defun load-food-db ()
  "Loads the food db from disk"
  (with-open-file (in *food-file* :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
       (setf *food-db* (read in))))))
