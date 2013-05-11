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
    (make-instance 'jada::food :name name :kcal kcal
                   :prot prot :fat fat
                   :carbs carbs)))

(test create-add-food-command
  (let* ((food-string "pizza 1500 50 47 103")
         (cmd (jada::create-add-food-command food-string))
         (food (jada::food cmd))
         (expected (string->food food-string)))
    (is (jada::same food expected))))

(run!)
