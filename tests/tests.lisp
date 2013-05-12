(defpackage #:jada-tests
  (:use #:common-lisp #:jada #:5am))

(defvar jada::*test* t)

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

(defvar *pizza-string* "pizza 1500 50 47 103")

(defun add-pizza-to-db ()
  (execute (create-command (concatenate 'string "add " *pizza-string*))))

(defvar *pizza* (string->food *pizza-string*))

(test create-add-food-command
  (let* ((cmd (create-command (concatenate 'string "add " *pizza-string*)))
         (food (jada::food cmd))
         (expected (string->food *pizza-string*)))
    (is (equal food expected))))

(test save-and-load-food-db
  (cl-fad:with-open-temporary-file (file)
    (let ((food-file (cl-fad:pathname-as-file file)))
      (setf jada::*food-file* food-file)
      (add-pizza-to-db)
      (clrhash jada::*food-db*)
      (jada::load-food-db)
      (is (equal (lookup-food (food-name *pizza*)) *pizza*)))))

(test save-and-load-log
  (cl-fad:with-open-temporary-file (file)
    (let ((log-file (cl-fad:pathname-as-file file)))
      (setf jada::*log-file* log-file)
      (execute (create-command "weight 83"))
      (setf jada::*log* (make-array 100 :adjustable t :fill-pointer 0))
      (jada::load-log)
      (is (equal (get-weight (jada::today)) 83)))))

(test create-ate-command
  (let ((cmd (jada::create-ate-command "ate pizza")))
    (is (equal (jada::food cmd) *pizza*))))

(test ate-command
  (add-pizza-to-db)
  (execute (create-command "ate pizza"))
  (is (= 1 1)))

(test create-log-entry
  (let ((default-entry (jada::create-log-entry))
        (expected (jada::create-log-entry (jada::current-date)
                                          0 'jada::+20-20 0 0 0 0)))
    (is (equal default-entry expected))))

(run!)

