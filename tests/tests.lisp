(in-package :jada)

(defpackage #:jada-tests
  (:use #:common-lisp #:jada #:5am))

(in-package #:jada-tests)

(def-suite jada)

(in-package :jada-tests)

(in-suite jada)

(test create-log-weight-command
      (let ((cmd (jada::create-log-weight-command "weight 83")))
        (is (= (slot-value cmd 'jada::weight) 83))))

(run!)
