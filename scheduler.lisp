;; A program to schedule conference talks in tracks
;; This program explores a possible solution using
;; greedy best fit search.
;; After loading this file eval: (print-all-tracks (make-all-tracks *input1*))

(defparameter *input1*
  '(("Writing Fast Tests Against Enterprise Rails" 60)
    ("Overdoing it in Python" 45)
    ("Lua for the Masses" 30)
    ("Ruby Errors from Mismatched Gem Versions" 45)
    ("Common Ruby Errors" 45)
    ("Rails for Python Developers" lightning)
    ("Communicating Over Distance" 60)
    ("Accounting-Driven Development" 45)
    ("Woah" 30)
    ("Sit Down and Write" 30)
    ("Pair Programming vs Noise" 45)
    ("Rails Magic" 60)
    ("Ruby on Rails: Why We Should Move On" 60)
    ("Clojure Ate Scala (on my project)" 45)
    ("Programming in the Boondocks of Seattle" 30)
    ("Ruby vs. Clojure for Back-End Development" 30)
    ("Ruby on Rails Legacy App Maintenance" 60)
    ("A World Without HackerNews" 30)
    ("User Interface CSS in Rails Apps" 30)))

(defconstant MORNING-SLOT 180)
(defconstant EVENING-SLOT 240)
(defconstant LUNCH 60)

(defun process-talks (input)
  "Process input for lightning and then sort DESC according to talk length"
  (sort
   (mapcar #'(lambda (i)
               (if (eq (second i) 'lightning)
                   (list (first i) 5)
                   i)) input)
   #'(lambda (a b)
       (> (second a) (second b)))))

(defun fit-in-slot (slot-length talk)
  "Checks if a talk can fit in a slot. If no returns nil, else returns the remaining time"
  (if (<= (second talk) slot-length)
      (- slot-length (second talk))
      nil))

(defun make-slot (slot talks)
  "Fits the talks in slot in a slot and returns the slot talks and remaining talks"
  (let ((slot-talks nil))
    (dolist (talk talks)
      (let ((fit (fit-in-slot slot talk)))
        (when (not (null fit))
          (setf slot fit)
          (push talk slot-talks))))
    (list slot-talks
          (set-difference talks slot-talks :test #'equal))))

(defun make-track (talks)
  "Make a track and return the remaining talks"
  (let* ((first-slot (make-slot MORNING-SLOT (process-talks talks)))
         (lunch-slot (list (list "Lunch" LUNCH)))
         (networking-slot '(("Networking" 0)))
         (second-slot (make-slot EVENING-SLOT (process-talks (second first-slot)))))
    (list
     (append (first first-slot)
             lunch-slot
             (first second-slot)
             networking-slot)
     (process-talks
      (set-difference talks (append (first first-slot)
                                    (first second-slot)) :test #'equal)))))

(defun make-all-tracks (talks &optional (tracks nil))
  "Make all tracks consuming all talks"
  (cond
    ((null talks) (reverse tracks))
    (t (let ((track-data (make-track talks)))
         (make-all-tracks (second track-data)
                          (cons (first track-data)
                                tracks))))))

(defun add-minutes (cur-time minutes)
  "A time is represented by a tuple (H M) 24 hour format. This function adds minutes and creates a new time"
  (let* ((cur-hour (+ (first cur-time)
                      (coerce (/ (second cur-time) 60) 'short-float)))
         (new-hour (+ cur-hour (/ minutes 60)))
         (h (floor new-hour))
         (m (ceiling (* 60 (rem new-hour h)))))
    (list h m)))

(defun format-track (track)
  (let ((next-time '(9 0))
        (talks nil))
    (dolist (talk track)
      (push (list (first talk)
                  next-time
                  (second talk))
            talks)
      (setq next-time (add-minutes next-time (second talk))))
    (reverse talks)))

(defun format-tracks (tracks)
  (mapcar #'format-track tracks))

(defun print-track (track)
  (mapcar #'(lambda (talk)
              (format t
                      "~A, starting at: ~A:~A (duration ~A min)~%"
                      (first talk)
                      (first (second talk))
                      (second (second talk))
                      (third talk))) track))

(defun print-all-tracks (tracks)
  (let ((formatted-tracks (format-tracks tracks))
        (track-num 1))
    (dolist (track formatted-tracks)
      (format t "Track ~A~%" track-num)
      (incf track-num)
      (print-track track)
      (format t "~%"))))
