(defpackage #:jada-tests
  (:use #:common-lisp #:jada #:5am))

(in-package #:jada-tests)

(def-suite jada)

(in-suite jada)

(defvar *pizza-string* "pizza 1000 50 25 100")

(defun perform (command-string)
  (execute (create-command command-string)))

(defun add-pizza-to-db ()
  (perform (concatenate 'string "add " *pizza-string*)))

(defvar *pizza* (jada::food-from-string *pizza-string*))

(defmacro with-scaffolding (&body body)
  "Creates temporary files for log and food db"
  `(cl-fad:with-open-temporary-file (log-file)
     (cl-fad:with-open-temporary-file (food-file)
       (let ((log-file (cl-fad:pathname-as-file log-file))
             (food-file (cl-fad:pathname-as-file food-file)))
         (setf jada::*log-file* log-file)
         (setf jada::*food-file* food-file)
         (progn ,@body)))))

(with-scaffolding
  (test add-food-command
    (add-pizza-to-db)
    (is (equal (lookup-food 'pizza) *pizza*)))

  (test log-weight-command
    (execute (create-command "weight 83"))
    (is (= (get-weight (today)) 83))))

(with-scaffolding
  (test save-and-load-food-db
    (add-pizza-to-db)
    (clrhash jada::*food-db*)
    (jada::load-food-db)
    (is (equal (lookup-food (food-name *pizza*)) *pizza*)))

  (test save-and-load-log
    (perform "weight 83")
    (setf jada::*log* (make-array 100 :adjustable t :fill-pointer 0))
    (jada::load-log)
    (is (equal (get-weight (jada::today)) 83))))

(test eat-command
  (with-scaffolding
    (add-pizza-to-db)
    (perform "eat pizza")
    (let ((log (jada::today))
          (food (remf *pizza* :name)))
      (is (loop
             for (key value) on food by #'cddr
             always (= (getf log key) (getf food key)))))))

(test fractional-eating
  (with-scaffolding
    (add-pizza-to-db)
    (let ((log (jada::today))
          (food (remf *pizza* :name))
          (fraction (random 5.0)))
      (perform (concatenate 'string "eat " (write-to-string fraction) " pizza"))
      (is (loop
             for (key value) on food by #'cddr
             always (= (getf log key) (* fraction (getf food key))))))))

(test barf-command
  (with-scaffolding
    (add-pizza-to-db)
    (let ((fraction (random 5.0)))
      (perform (concatenate 'string "eat " (write-to-string fraction) " pizza"))
      (perform (concatenate 'string "barf" (write-to-string fraction) " pizza"))
      (is (= 0 (get-kcal (today)))))))

(test leangains-macros
  (with-scaffolding
    (jada::set-tdee (jada::today) 1000)
    (jada::set-total-prot (jada::today) 100)
    (jada::set-workout-day (jada::today) nil)
    (let ((macros (leangains-macros '+0-0)))
      (with-slots (jada::prot jada::kcal jada::kcal
                              jada::carbs jada::carbs
                              jada::fat jada::fat) macros
        (is (= jada::prot 100))
        (is (= jada::kcal  jada::kcal 1000))
        (is (= jada::carbs (/ (* 0.25 (- 1000 400)) 4)))
        (is (= jada::fat (/ (* 0.75 (- 1000 400)) 9)))))

    (jada::set-workout-day (jada::today) t)
    (let ((macros (leangains-macros '+0-0)))
      (with-slots (jada::prot jada::fat jada::carbs) macros
        (is (= jada::carbs (/ (* 0.75 (- 1000 400)) 4)))
        (is (= jada::fat (/ (* 0.25 (- 1000 400)) 9)))))))

(test set-protocol
  (with-scaffolding
    (perform "protocol +20-20")
    (is (equal (jada::get-protocol (jada::today)) 'jada::+20-20))))

(run!)
