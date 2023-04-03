(in-package :checkers-ai)

(defun make-ai (dna elo)
  (list dna elo)
)

(defun make-dna (w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20)
  (list w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20))

(defun get-dna-ai (ai)
  (nth 0 ai)
)

(defun get-elo-ai (ai)
  (nth 1 ai)
)

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
                          0    ; our safe kings
                          0   ; their safe kings
                          )))
    gen))


(defun init-first-gen ()
  (let ((gen nil))
    (dotimes (i gen-size)
      (push
        (make-ai (make-dna
            (random-range -1.0 1.0) ; 0
            (random-range -1.0 1.0) ; 1
            (random-range -1.0 1.0) ; 2
            (random-range -1.0 1.0) ; 3
            (random-range -1.0 1.0) ; 4
            (random-range -1.0 1.0) ; 5
            (random-range -1.0 1.0) ; 6
            (random-range -1.0 1.0) ; 7
            (random-range -1.0 1.0) ; 8
            (random-range -1.0 1.0) ; 9
            (random-range -1.0 1.0) ; 10
            (random-range -1.0 1.0) ; 11
            (random-range -1.0 1.0) ; 12
            (random-range -1.0 1.0) ; 13
            (random-range -1.0 1.0) ; 14
            (random-range -1.0 1.0) ; 15
            (random-range -1.0 1.0) ; 16
            (random-range -1.0 1.0) ; 17
            (random-range -1.0 1.0) ; 18
            (random-range -1.0 1.0) ; 19
          ) 400)
        gen))
    gen))

(defun generate-new-generation (orig-gen)
  (let ((gen (stable-sort orig-gen)) (new-gen nil))
    (dotimes (i gen-size)
      (let ((father (pick-random-gen gen)) (mother (pick-random-gen gen)))
        (push (generate-new-ai father mother) new-gen)
      )
    )
    new-gen
  )
)

(defun generate-new-ai (father mother)
  (make-ai (mutate-dna (cross-over-dnas father mother)) (cross-elo father mother)))


(defun cross-over-dnas (father mother)
  (let ((point (* 20 (/ (get-elo-ai father) (+ (get-elo-ai father) (get-elo-ai mother))))) (direction (random 1.0)))
    (if (< direction .5)
      (append (subseq (get-dna-ai father) 0 (round point)) (subseq (get-dna-ai mother) (+ (round point) 1) 19))
      (append (subseq (get-dna-ai mother) 0 (round point)) (subseq (get-dna-ai father) (+ (round point) 1) 19))
    )
  )
)

(defun mutate-dna (dna)
  (mapcar (lambda (weight) (+ (random-range -0.1 0.1) weight (* weight (random-range -0.1 0.1)))) dna)
)

(defun cross-elo (father mother)
  (/ (+ (get-elo-ai father) (get-elo-ai mother)) 2)
)

(defun simulate-gen (orig-gen)
  (let ((generation (copy-gen orig-gen)))
    (dotimes (j set-number)
      (dolist (ai generation)
        (let ((opponent (pick-opponent ai generation)))
          (let ((res (simulate-set ai opponent)))
            (setf (nth 1 ai) (elo ai opponent res))
            (setf (nth 1 opponent) (elo opponent ai (- 1 res)))
          )
        )
      )
    )
  )
)

(defun copy-gen (gen)
)

(defun copy-ai (ai)
  (make-ai (copy-list (get-dna-ai ai)) (get-elo-ai ai))
)

(defun pick-opponent (ai gen)
  (random gen)
)

(defun simulate-set (white black)
  1
)

(defun expected-score (rating-a rating-b)
  (/ 1 (+ 1 (expt 10 (/ (- rating-b rating-a) 400)))))

(defun elo (rating-a rating-b res)
  (+ rating-a (* 20 (- res (expected-score rating-a rating-b)))))

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

; TODO this needs to be rewritten as it is pretty cringe for now - we really do need to reach a terminal condition in order to be sure we are done
; we can set an arbitrary number of turn to something like 512 - in which case the match will be a draw - just in case we reach some long matches and 
(defun match (white black)
  (let ((state (make-state (init-board) 0 -1)))
    (dotimes (i 100)
      (ai-turn state white)
      (ai-turn state black))
    (when (> (utility state 0 white) (utility state 1 black))
      1)
    (when (< (utility state 0 white) (utility state 1 black))
      0)))

(defun random-range (a b)
  (+ a (coerce (random (coerce (- b a) 'float)) 'float)))
