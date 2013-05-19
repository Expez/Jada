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
    (when (or (< (length str) 4)
              (> (length str) 6))
      (return-from leangainsp nil))
    (when-let* ((end (min (or (position #\+ str :start 1) (position #\- str :start 1))))
                (workout-day (numberp (+ 1 (/ (read-from-string str nil nil :end end) 100))))
                (start (1+ end))
                (plus-or-minus- (or (char= (char str end) #\+)
                                    (char= (char str end) #\-)))
                (rest-day (numberp (+ 1 (/ (read-from-string str nil nil :start start) 100)))))
      t)))

(defun leangains-macros (protocol-name)
  "Returns the macros for the leangains protocol matching `protocol-name'"
  (let* ((str (symbol-name protocol-name))
         ;; The scalars are the factors to scale the tdee by, e.g for +20
         ;; it is 1.2
         (end (min (or (position #\+ str :start 1) (position #\- str :start 1))))
         (workout-day-scalar (+ 1 (/ (read-from-string str nil nil :end end) 100)))
         (start end)
         (rest-day-scalar (+ 1 (/ (read-from-string str nil nil :start start) 100)))
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
                       :carbs workout-day-carbs
                       :fat workout-day-fat)
        (make-instance 'macros
                       :kcal rest-day-kcal
                       :prot prot
                       :carbs rest-day-carbs
                       :fat rest-day-fat))))

(defgeneric remaining (macros)
  (:documentation "Returns a macro class representing todays remaining macros."))

(defmethod remaining ((macros macros))
  (with-slots (kcal prot carbs fat) macros
    (let* ((kcal-consumed (get-kcal (today)))
           (fat-consumed (get-fat (today)))
           (prot-consumed (get-prot (today)))
           (carbs-consumed (get-carbs (today))))
      (make-instance 'macros
                     :kcal (- kcal kcal-consumed)
                     :fat (- fat fat-consumed)
                     :prot (- prot prot-consumed)
                     :carbs (- carbs carbs-consumed)))))

(defun print-remaining-macros ()
  (let* ((protocol (get-protocol (today)))
         (remaining-macros (remaining (macros protocol)))
         (kcal (kcal remaining-macros))
         (fat (fat remaining-macros))
         (prot (prot remaining-macros))
         (carbs (carbs remaining-macros)))
    (format *query-io* "Total macros remaining: ~%")
    (format *query-io* "Kcal: ~d~%" kcal)
    (format *query-io* "Prot: ~d~%" prot)
    (format *query-io* "Fat: ~d~%" fat)
    (format *query-io* "Carbs: ~d~%" carbs)))
