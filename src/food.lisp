(in-package :jada)

(defvar *food-db (make-hash-table :test #'equal)
  "Database holding entries for various food items.")

(defclass food ()
  ((name  :accessor name  :initarg :name)
   (kcal  :accessor kcal  :initarg :kcal)
   (prot  :accessor prot  :initarg :prot)
   (fat   :accessor fat   :initarg :fat)
   (carbs :accessor carbs :initarg :carbs)))

