(in-package :jada)

(defvar *food-db* (make-hash-table)
  "Database holding entries for various food items.")

(defvar *food-file* "~/.jada/foods")

(defun create-food (name kcal prot fat carbs)
  (let ((food (list :name name :kcal kcal :prot prot :fat fat :carbs carbs)))
   (mapc (lambda (e) (unless (keywordp e) (check-type e number))) (cddr food))
   food))

(defun food-from-string (s)
  (let* ((tokens (mapcar #'read-from-string (tokenize s)))
         (name (first tokens))
         (kcal (second tokens))
         (prot (third tokens))
         (fat (fourth tokens))
         (carbs (fifth tokens)))
    (create-food name kcal prot fat carbs)))

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
  (setf (gethash (getf food :name) *food-db*) food)
  (save-food-db))

(defun as-puke (food)
  "Used with the barf command, aka undo for meals.  The value of a
  food as puke has the same magnitude, but all elements are negated."
  (mapc (lambda (entry) (when (numberp entry) (* -1 entry))) food))

(defun lookup-food (name)
  "Get food from DB."
  (gethash name *food-db*))

(defun food-name (food)
  (getf food :name))

(defun food-kcal (food)
  (getf food :kcal))

(defun food-prot (food)
  (getf food :prot))

(defun food-fat (food)
  (getf food :fat))

(defun food-carbs (food)
  (getf food :carbs))
