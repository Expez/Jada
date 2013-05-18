;;; Calculations relating to the various protocols.
(in-package :jada)

(defclass macros ()
  ((kcal  :accessor kcal  :initarg :kcal)
   (prot  :accessor prot  :initarg :prot)
   (carbs :accessor carbs :initarg :carbs)
   (fat   :accessor fat   :initarg :fat)))

(defun macros (protocol)
  "Calculates the macro stats based on the given `protocol'"
  (cond
    ((leangainsp protocol) (leangains-macros protocol))
    (t (error 'unknown-protocol protocol))))

(defun leangainsp (protocol-name)
  "Parses a leangains protocol of form +20-20 and returns a `protocol'."
  (let* ((str (symbol-name protocol-name)))
    (when (/= (length str) 6)
      (return-from leangainsp nil))
    (when-let ((plus-or-minus (or (char= (char str 0) #\+)
                                  (char= (char str 0) #\-)))
               (workout-day (numberp (safely-read-from-string (subseq str 1 3))))
               (plus-or-minus- (or (char= (char str 3) #\+)
                                   (char= (char str 3) #\-)))
               (rest-day (numberp (safely-read-from-string (subseq str 4)))))
      t)))

(defun leangains-macros (protocol-name)
  "Returns the macros for the leangains protocol matching `protocol-name'"
  (let* ((str (symbol-name protocol-name))
         ;; The scalars are the factors to scale the tdee by, e.g for +20
         ;; it is 1.2
         (end (min (or (position #\+ str :start 1) (position #\- str :start 1))))
         (rest-day-scalar (+ 1 (/ (read-from-string str nil nil :end end) 100)))
         (start (1+ end))
         (workout-day-scalar (+ 1 (/ (read-from-string str nil nil :start start) 100)))
         (rest-day-kcal (* rest-day-scalar (get-tdee (today))))
         (workout-day-kcal (* workout-day-scalar (get-tdee (today))))

         (prot (get-total-prot (today)))
         ;; 4kcal / g protein and carbs 9 for fat
         ;; protein is same on rest and workout days
         (rest-day-kcal-remaining (- rest-day-kcal (* 4 prot)))
         (workout-day-kcal-remaining (- workout-day-kcal (* 4 prot)))

         ;; 25% remaining energy from fat on workout days,
         ;; and 75% for carbs and vice versa on rest days.
         (rest-day-fat (/ (* 0.75 rest-day-kcal-remaining) 9))
         (rest-day-carbs (/ (* 0.25 rest-day-kcal-remaining) 4))
         (workout-day-fat (/ (* 0.25 workout-day-kcal-remaining) 9))
         (workout-day-carbs (/ (* 0.75 workout-day-kcal-remaining) 4)))

    (if (get-workout-day (today))
        (make-instance 'macros
                       :kcal workout-day-kcal
                       :prot prot
                       :carbs  workout-day-carbs
                       :fat workout-day-fat)
        (make-instance 'macros
                       :kcal rest-day-kcal
                       :prot prot
                       :carbs  rest-day-carbs
                       :fat rest-day-fat))))

