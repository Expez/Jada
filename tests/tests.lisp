(defpackage #:jada-tests
  (:use #:common-lisp #:jada #:5am))

(in-package #:jada-tests)

(def-suite jada)

(in-suite jada)

(test create-log-weight-command
  (let ((cmd (create-command "weight 83")))
    (is (= (jada::weight cmd) 83))))

(defun string->food (s)
  (let* ((tokens (mapc #'read-from-string (jada::tokenize s)))
         (name (first tokens))
         (kcal (second tokens))
         (prot (third tokens))
         (fat (fourth tokens))
         (carbs (fifth tokens)))
    (jada::create-food name kcal prot fat carbs)))

(defvar *food-string* "pizza 1500 50 47 103")

(test create-add-food-command
  (let* ((cmd (create-command (concatenate 'string "add " *food-string*)))
         (food (jada::food cmd))
         (expected (string->food *food-string*)))
    (is (equal food expected))))

(test save-and-load-food-db
  (setf jada::*food-file* "food")
  (let ((food (string->food *food-string*)))
    (execute (create-command (concatenate 'string "add " *food-string*)))
   (clrhash jada::*food-db*)
   (jada::load-food-db)
   (when (cl-fad:file-exists-p "food")
    (delete-file "food"))
   (is (equal (get-food (food-name food)) food))))

(test save-and-load-log
  (setf jada::*log-file* "log")
  (execute (create-command "weight 83"))
  (setf jada::*log* nil)
  (jada::load-log)
  (when (cl-fad:file-exists-p "log")
    (delete-file "log"))
  (is (equal (get-weight (jada::today)) 83)))

(run!)
