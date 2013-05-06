(in-package :jada)

(def-suite jada)

(in-suite jada)

(test create-log-weight-command
      (let ((cmd (create-log-weight-command "weight 83")))
        (is (= (weight cmd) 83))))

(run!)
