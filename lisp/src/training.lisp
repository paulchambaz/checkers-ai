(in-package :checkers-ai)

(defun make-ai (dna elo)
  (list dna elo))

(defun make-dna (w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20)
  (list w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20))

(defun ai-dna (ai)
  (nth 0 ai)
)

(defun ai-elo (ai)
  (nth 1 ai)
)

(defun init-gen ()
  (let ((gen nil))
    (dotimes (i gen-size)
      (push
        (make-ai (make-dna
            30                      ; 0
            -30                     ; 1
            100                     ; 2
            -100                    ; 3
            0                       ; 4
            0                       ; 5
            0                       ; 6
            0                       ; 7
            0                       ; 8
            0                       ; 9
            0                       ; 10
            0                       ; 11
            0                       ; 12
            0                       ; 13
            0                       ; 14
            0                       ; 15
            0                       ; 16
            0                       ; 17
            0                       ; 18
            0                       ; 19
          ) 400)
        gen))
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
  (let ((point (* 20 (/ (ai-elo father) (+ (ai-elo father) (ai-elo mother))))) (direction (random 1.0)))
    (if (< direction .5)
      (append (subseq (ai-dna father) 0 (round point)) (subseq (ai-dna mother) (+ (round point) 1) 19))
      (append (subseq (ai-dna mother) 0 (round point)) (subseq (ai-dna father) (+ (round point) 1) 19))
    )
  )
)

(defun mutate-dna (dna)
  (mapcar (lambda (weight) (+ (random-range -0.1 0.1) weight (* weight (random-range -0.1 0.1)))) dna)
)

(defun cross-elo (father mother)
  (/ (+ (ai-elo father) (ai-elo mother)) 2)
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
  (make-ai (copy-list (ai-dna ai)) (ai-elo ai))
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

(defun pick-random ()
  (- (random 2.0) 1.0))

(defvar *gen* nil)

(defun get-ai (n gen)
  (nth n gen))

(defun get-weight (n ai)
  (nth n ai))

; TODO: this needs to be rewritten as it is pretty cringe for now - we really do need to reach a terminal condition in order to be sure we are done
; we can set an arbitrary number of turn to something like 512 - in which case the match will be a draw - just in case we reach some long matches
(defun match (white black)
  (let ((state (init-state)))
    (dotimes (i 100)
      (ia-turn state white)
      (ia-turn state black))
    (if (> (utility state 0 white) (utility state 1 black)) 1 0)))

(defun random-range (a b)
  (+ a (coerce (random (coerce (- b a) 'float)) 'float)))
