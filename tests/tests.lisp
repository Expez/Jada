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
       (let ((jada::*log* (make-array 100 :adjustable t :fill-pointer 0))
             (jada::*food-db* (make-hash-table))
             (jada::*food-file* (cl-fad:pathname-as-file food-file))
             (jada::*log-file* (cl-fad:pathname-as-file log-file)))
         (progn ,@body)))))

(with-scaffolding
  (test add-food-command
    (add-pizza-to-db)
    (is (equal (lookup-food 'pizza) *pizza*)))

  (test log-weight-command
    (perform "weight 83")
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
          (food (cddr *pizza*)))
      (is (loop
             for (key value) on food by #'cddr
             always (= (getf log key) value))))))

(defun float= (f1 f2 tolerance)
  "Compares the floats `f1' and `f2' returns T if they are within
  `tolerance' of one another."
  (let* ((abs-max (max (abs f1) (abs f2)))
         (abs-min (min (abs f1) (abs f2)))
         (difference (- abs-max abs-min)))
    (<=  difference tolerance)))

(test fractional-eating
  (with-scaffolding
    (add-pizza-to-db)
    (let ((log (jada::today))
          (food (cddr *pizza*))
          (fraction (random 5.0)))
      (perform (concatenate 'string "eat " (write-to-string fraction) " pizza"))
      (is (loop
             for (key value) on food by #'cddr
             always (float= (getf log key) (* fraction value) 0.01))))))

(test barf-command
  (with-scaffolding
    (add-pizza-to-db)
    (let ((fraction (random 5.0)))
      (perform (concatenate 'string "eat " (write-to-string fraction) " pizza"))
      (perform (concatenate 'string "barf " (write-to-string fraction) " pizza"))
      (is (float= (get-kcal (today)) 0 0.01)))))

(test leangains-macros
  (with-scaffolding
    (jada::set-tdee (jada::today) 1000)
    (jada::set-total-prot (jada::today) 100)
    (jada::set-workout-day (jada::today) nil)
    (let ((macros (leangains-macros '+10-10)))
      (with-accessors ((prot jada::prot) (kcal jada::kcal) (carbs jada::carbs)
                       (fat jada::fat)) macros
        (is (= prot 100))
        (is (= kcal 900))
        (is (= carbs (/ (* 0.25 (- 900 400)) 4)))
        (is (= fat (/ (* 0.75 (- 900 400)) 9)))))

    (jada::set-workout-day (jada::today) t)
    (let ((macros (leangains-macros '+10-10)))
      (with-accessors ((prot jada::prot) (kcal jada::kcal) (carbs jada::carbs)
                       (fat jada::fat)) macros
        (is (= kcal 1100))
        (is (= carbs (/ (* 0.75 (- 1100 400)) 4)))
        (is (= fat (/ (* 0.25 (- 1100 400)) 9)))))))

(test set-protocol
  (with-scaffolding
    (perform "protocol +20-20")
    (is (equal (jada::get-protocol (jada::today)) '+20-20))))

(test remaining
  (with-scaffolding
    (add-pizza-to-db)
    (jada::set-tdee (jada::today) 1000)
    (jada::set-total-prot (jada::today) 100)
    (jada::set-workout-day (jada::today) nil)
    (perform "eat pizza")
    (let* ((remaining-macros (jada::remaining (jada::macros 'jada::+0-0)))
           (remaining-kcal (jada::kcal remaining-macros))
           (remaining-prot (jada::prot remaining-macros))
           (remaining-fat (jada::fat remaining-macros))
           (remaining-carbs (jada::carbs remaining-macros)))
      (is (= remaining-kcal 0))
      (is (= remaining-prot 50))
      (is (= remaining-carbs (- (/ (* 0.25 (- 1000 400)) 4) (food-carbs *pizza*))))
      (is (= remaining-fat (- (/ (* 0.75 (- 1000 400)) 9) (food-fat *pizza*)))))

    (jada::set-workout-day (jada::today) t)
    (let* ((remaining-macros (jada::remaining (jada::macros 'jada::+20-20)))
           (remaining-kcal (jada::kcal remaining-macros))
           (remaining-prot (jada::prot remaining-macros))
           (remaining-fat (jada::fat remaining-macros))
           (remaining-carbs (jada::carbs remaining-macros)))
      (is (= remaining-kcal 200))
      (is (= remaining-prot 50))
      (is (= remaining-carbs (- (/ (* 0.75 (- 1200 400)) 4) (food-carbs *pizza*))))
      (is (= remaining-fat (- (/ (* 0.25 (- 1200 400)) 9) (food-fat *pizza*)))))))

(run!)
