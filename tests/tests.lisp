(defpackage #:jada-tests
  (:use #:common-lisp #:jada #:5am))

(in-package #:jada-tests)

(def-suite jada)

(in-suite jada)

(test create-log-weight-command
  (let ((cmd (jada::create-log-weight-command "weight 83")))
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
  (let* ((cmd (jada::create-add-food-command (concatenate 'string "add "
                                                          *food-string*)))
         (food (jada::food cmd))
         (expected (string->food *food-string*)))
    (is (equal food expected))))

(test save-and-load-food-db
  (setf jada::*food-file* "food")
  (let ((food (string->food *food-string*)))
    (jada::execute (jada::create-add-food-command (concatenate 'string "add "
                                                               *food-string*)))
   (clrhash jada::*food-db*)
   (jada::load-food-db)
   (when (cl-fad:file-exists-p "food")
    (delete-file "food"))
   (is (equal (jada::get-food (jada::food-name food)) food))))

(run!)
