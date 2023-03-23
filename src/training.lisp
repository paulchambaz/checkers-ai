(in-package :checkers-ai)

(defun init-gen (n)
  (let ((gen (make-list n)))
    (dotimes (i n)
      (setf (nth i gen) (list
                          30    ; our pawns
                          -30   ; their pawns
                          100   ; our kings
                          -100  ; their kings
                          0    ; our mobility
                          -0   ; their mobility
                          15    ; our safe kings
                          -15   ; their safe kings
                          )))
    gen))

; (defun init-gen (n)
;   (let ((gen (make-list n)))
;     (setf (nth 1 gen) (list 30 -30 100 -100 0 -0 15 -15))
;     (setf (nth 0 gen) (list -30 30 -100 100 0 -0 -15 15))
;     gen
;   ))

(defun pick-random ()
  (- (random 2.0) 1.0))

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
