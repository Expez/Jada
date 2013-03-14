(defpackage #:jada-tests-asd
  (:use #:cl #:asdf))

(in-package #:jada-tests-asd)

(defsystem #:jada-tests
    :name "jada tests"
    :version "0.0.0"
    :license "BSD"
    :description "Tests for jada"
    :serial t
    :pathname "tests"
    :components ((:file "suite")
                 (:file "tests")))
