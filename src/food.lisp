(in-package :jada)

(defvar *food-db (make-hash-table :test #'equal)
  "Database holding entries for various food items.")

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
