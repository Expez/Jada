(defpackage #:jada-asd
  (:use #:cl #:asdf))

(in-package #:jada-asd)

(defsystem #:jada
    :name "jada"
    :version "0.0.0"
    :license "BSD"
    :description "jada is just another diet aid."
    :serial t
    :pathname "src"
    :depends-on (:cl-ppcre)
    :components ((:file "jada")))
