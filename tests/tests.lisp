(defpackage #:jada-tests
  (:use #:common-lisp #:jada #:5am))

(in-package #:jada-tests)

(def-suite jada)

(in-suite jada)

(defun string->food (s)
  (let* ((tokens (mapc #'read-from-string (jada::tokenize s)))
         (name (first tokens))
         (kcal (second tokens))
         (prot (third tokens))
         (fat (fourth tokens))
         (carbs (fifth tokens)))
    (jada::create-food name kcal prot fat carbs)))

(defvar *pizza-string* "pizza 1500 50 47 103")

(defun perform (command-string)
  (execute (create-command command-string)))

(defun add-pizza-to-db ()
  (perform (concatenate 'string "add " *pizza-string*)))

(defvar *pizza* (string->food *pizza-string*))

(test add-food-command
  (add-pizza-to-db)
  (is (equal (lookup-food "pizza") *pizza*)))

(test log-weight-command
  (execute (create-command "weight 83"))
  (is (= (get-weight (today)) 83)))

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

(test eat-command
  (add-pizza-to-db)
  (execute (create-command "eat pizza"))
  (let ((log (jada::today))
        (food (remf *pizza* :name)))
    (is (loop
           for (key value) on food by #'cddr
           always (= (getf log key) (getf food key))))))

(test barf-command )
(run!)

