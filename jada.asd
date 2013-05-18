;;;; yada.asd

(defsystem #:jada
    :name "jada"
    :version "0.0.0"
    :license "BSD"
    :description "jada is just another diet aid."
    :serial t
    :pathname "src"
    :depends-on (:cl-ppcre :alexandria :cl-fad)
    :components ((:file "package")
                 (:file "jada")
                 (:file "log")
                 (:file "food")
                 (:file "commands")
                 (:file "protocol"))
      :in-order-to ((test-op (load-op :jada-tests)))
      :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :jada-tests)
                             :jada-tests)))

(defsystem #:jada-tests
    :name "jada tests"
    :version "0.0.0"
    :license "BSD"
    :description "Tests for jada"
    :depends-on (:fiveam)
    :pathname "tests"
    :components ((:file "tests")))
