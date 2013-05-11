(in-package :jada)


(defvar *food-db* (make-hash-table :test #'equal)
  "Database holding entries for various food items.")

(defvar *food-file* "~/.jada/foods")

(defun create-food (name kcal prot fat carbs)
  (pairlis (list :name :kcal :prot :fat :carbs)
           (list name kcal prot fat carbs)))

(defun save-food-db ()
  "Saves the food db to disk"
  (with-open-file (out (ensure-directories-exist *food-file*)
                       :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print *food-db* out))))

(defun load-food-db ()
  "Loads the food db from disk"
  (with-open-file (in *food-file* :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
       (setf *food-db* (read in))))))

(defun add-food (food)
  "Adds food to DB."
  (setf (gethash (assoc :name food) *food-db*) food))

(defun get-food (name)
  "Get food from DB."
  (gethash name *food-db*))

(defun food-name (food)
  (assoc :name food))

(defun food-kcal (food)
  (assoc :kcal food))

(defun food-prot (food)
  (assoc :prot food))

(defun food-fat (food)
  (assoc :fat food))

(defun food-carbs (food)
  (assoc :carbs food))
