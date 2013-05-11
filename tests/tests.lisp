(defpackage #:jada-tests
  (:use #:common-lisp #:jada #:5am))

(in-package #:jada-tests)

(def-suite jada)

(in-suite jada)

(test create-log-weight-command
      (let ((cmd (jada::create-log-weight-command "weight 83")))
        (is (= (jada::weight cmd) 83))))

;; (test create-add-weight-command
;;   (let ((cmd (create-add-food-command "pizza 1500 50 47 103")))
;;     (is (and
;;          (equal (name )))))
;;   )

(run!)
