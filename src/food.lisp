(in-package :jada)

(defvar *food-db* (make-hash-table)
  "Database holding entries for various food items.")

(defvar *food-file* "~/.jada/foods")

(defun create-food (name kcal prot fat carbs)
  (let* ((food (list :name name :kcal kcal :prot prot :fat fat :carbs carbs))
         (food-sans-name (cddr food)))
    (mapc (lambda (food-element)
            (unless (keywordp food-element)
              (check-type food-element number))) food-sans-name)
    food))

(defun food-from-string (s)
  (let* ((name (read-from-string (string-right-trim '(#\Space) (cl-ppcre:scan-to-strings "[A-z-ÅåÆæØø ]+" s))))
         (start (1+ (length (symbol-name name))))
         (tokens (mapcar #'read-from-string (tokenize s :start start)))
         (kcal (first tokens))
         (prot (second  tokens))
         (fat (third tokens))
         (carbs (fourth tokens)))
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

(defun print-food-db ()
  "Displays content of the DB."
  (format *query-io* "Name kcal fat prot carbs~%~%")
  (maphash-values #'print-food *food-db*))

(defun print-food (food)
  (let ((name (food-name food))
        (kcal (food-kcal food))
        (prot (food-prot food))
        (fat (food-fat food) )
        (carbs (food-carbs food)))
    (format *query-io* "~a ~4,1f ~3,1f ~3,1f ~3,1f~%" name kcal prot fat carbs)))

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
