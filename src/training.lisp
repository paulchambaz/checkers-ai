(in-package :checkers-ai)

(defun init-gen (n)
  (let ((gen (make-list n)))
    (dotimes (i n)
      (setf (nth i gen) (make-ai (pick-random) (pick-random))))
    gen))

(defun pick-random ()
  (- (random 2.0) 1.0))

(defun make-ai (w1 w2)
  (list w1 w2))

(defvar *gen* nil)
; (defvar *gen* (list (make-ai 1 -1)
;                     (make-ai 10 0)))

(defun get-ai (n gen)
  (nth n gen))

(defun get-weight (n ai)
  (nth n ai))

(defun match (white black)
  (let ((state (make-state (init-board) 0 -1)))
    (dotimes (i 100)
      (ia-turn state white)
      (ia-turn state black))
    (when (> (utility state 0 white) (utility state 1 black))
      (format t "White won~%"))
    (when (< (utility state 0 white) (utility state 1 black))
      (format t "Black won~%"))))
