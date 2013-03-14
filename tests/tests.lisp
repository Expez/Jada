(in-package :jada)

(defpackage #:jada-tests
  (:use #:common-lisp #:jada #:5am))

(in-package #:jada-tests)

(def-suite jada)

(in-package :jada-tests)

(in-suite jada)

(test parse-input
  "Test the parse-input function."
      (multiple-value-bind (fn args) (jada::parse-input "w 83")
        (is (eq fn #'jada::weight))
        (is (= 83 (car args)))))
