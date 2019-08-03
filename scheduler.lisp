;; A program to schedule conference talks in tracks
;; This program explores a possible solution using
;; greedy best fit search.
;; After loading this file eval: (print-all-tracks (make-all-tracks *input1*))

;; Output:
;; Track 1
;; Rails Magic, starting at: 9:0 (duration 60 min)
;; Communicating Over Distance, starting at: 10:0 (duration 60 min)
;; Writing Fast Tests Against Enterprise Rails, starting at: 11:0 (duration 60 min)
;; Lunch, starting at: 12:0 (duration 60 min)
;; User Interface CSS in Rails Apps, starting at: 13:0 (duration 30 min)
;; Pair Programming vs Noise, starting at: 13:30 (duration 45 min)
;; Clojure Ate Scala (on my project), starting at: 14:15 (duration 45 min)
;; Ruby on Rails: Why We Should Move On, starting at: 15:0 (duration 60 min)
;; Ruby on Rails Legacy App Maintenance, starting at: 16:0 (duration 60 min)
;; Networking, starting at: 17:0 (duration 30 min)

;; Track 2
;; Overdoing it in Python, starting at: 9:0 (duration 45 min)
;; Ruby Errors from Mismatched Gem Versions, starting at: 9:45 (duration 45 min)
;; Common Ruby Errors, starting at: 10:30 (duration 45 min)
;; Accounting-Driven Development, starting at: 11:15 (duration 45 min)
;; Lunch, starting at: 12:0 (duration 60 min)
;; Rails for Python Developers, starting at: 13:0 (duration 5 min)
;; A World Without HackerNews, starting at: 13:5 (duration 30 min)
;; Ruby vs. Clojure for Back-End Development, starting at: 13:35 (duration 30 min)
;; Programming in the Boondocks of Seattle, starting at: 14:5 (duration 30 min)
;; Sit Down and Write, starting at: 14:35 (duration 30 min)
;; Woah, starting at: 15:5 (duration 30 min)
;; Lua for the Masses, starting at: 15:35 (duration 30 min)
;; Networking, starting at: 16:5 (duration 30 min)

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
(defconstant NETWORKING 30)

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
          (set-difference (process-talks talks) slot-talks :test #'equal))))

(defun make-track (talks)
  "Make a track and return the remaining talks"
  (let* ((first-slot (make-slot MORNING-SLOT (process-talks talks)))
         (lunch-slot (list 'lunch (list "Lunch" LUNCH)))
         (networking-slot (list 'networking (list "Networking" NETWORKING)))
         (second-slot (make-slot EVENING-SLOT (process-talks (second first-slot)))))
    (list
     (list (cons 'morning (first first-slot))
           lunch-slot
           (cons 'evening (first second-slot))
           networking-slot)
     (process-talks
      (set-difference (process-talks talks)
                      (append (first first-slot)
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
    (dolist (slot track)
      (cond
        ((eq (first slot) 'morning) (setq next-time '(9 0)))
        ((eq (first slot) 'lunch) (setq next-time '(12 0)))
        ((eq (first slot) 'evening) (setq next-time '(13 0)))
        ((eq (first slot) 'networking) (when (< (first next-time) 16)
                                         (setq next-time '(4 0)))))
      (dolist (talk (cdr slot))
        (push (list (first talk)
                    next-time
                    (second talk))
              talks)
        (setq next-time (add-minutes next-time (second talk)))))
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
