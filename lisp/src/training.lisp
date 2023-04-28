(in-package :checkers-ai)

(defstruct ai
  dna
  elo)

(defun make-dna (w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20 w21 w22 w23 w24 w25 w26 w27 w28 w29 w30)
  (list w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20 w21 w22 w23 w24 w25 w26 w27 w28 w29 w30))

(defun copy-gen (orig-gen)
  (mapcar #'copy-ai orig-gen))

(defun equal-ai (ai1 ai2)
  (and (equal (ai-dna ai1) (ai-dna ai2))
       (equal (ai-elo ai1) (ai-elo ai2))))

(defun init-gen ()
  (let ((gen nil))
    (dotimes (i gen-size)
      (push
        (make-ai
          :dna (make-dna
                 (random-range 50.0 70.0)    ; pawn count for player 1
                 (random-range -70.0 -50.0)  ; pawn count for player 2
                 (random-range 80.0 100.0)   ; king count for player 1 
                 (random-range -100.0 -80.0) ; king count for player 2
                 (random-range 30.0 50.0)    ; mobility for player 1
                 (random-range -50.0 -30.0)  ; mobility for player 2
                 (random-range 40.0 60.0)    ; eating mobility for player 1
                 (random-range -60.0 -40.0)  ; eating mobility for player 2
                 (random-range 20.0 40.0)    ; center count for player 1
                 (random-range -40.0 -20.0)  ; center count for player 2
                 (random-range 10.0 30.0)    ; front count for player 1
                 (random-range -30.0 -10.0)  ; front count for player 2
                 (random-range 10.0 30.0)    ; back count for player 1
                 (random-range -30.0 -10.0)  ; back count for player 2
                 (random-range 15.0 35.0)    ; left count for player 1
                 (random-range -35.0 -15.0)  ; left count for player 2
                 (random-range 15.0 35.0)    ; right count for player 1
                 (random-range -35.0 -15.0)  ; right count for player 2
                 (random-range 10.0 30.0)    ; side count for player 1
                 (random-range -30.0 -10.0)  ; side count for player 2
                 (random-range 10.0 30.0)    ; diagonal count for player 1
                 (random-range -30.0 -10.0)  ; diagonal count for player 2
                 (random-range 60.0 80.0)    ; king safety for player 1
                 (random-range -80.0 -60.0)  ; king safety for player 2
                 (random-range 40.0 60.0)    ; pawn safety for player 1
                 (random-range -60.0 -40.0)  ; pawn safety for player 2
                 (random-range 30.0 50.0)    ; support count for player 1
                 (random-range -50.0 -30.0)  ; support count for player 2
                 (random-range -40.0 -20.0)  ; jump count for player 1
                 (random-range 20.0 40.0)    ; jump count for player 2
                 )
          :elo (random-range 100.0 2000.0))
        gen))
    gen))

; TODO: fix the big bad bug - 30 minutes

; TODO: fix the thing which bugs in training for the ai - do everything step by step

; TODO: write the function to get the average generation
; which involves simulating the gen, sorting it by elo, then storing it to a
; file - this can take a long time

; TODO: write what is necessary to have easy medium and hard difficulty
; - easy limit depth to 5, do not use opening or endgame databse
; - medium pick the average ai, do not use opening or endgame database, use 3 seconds max
; - hard use the best ai, use the opening and the endgame database, use 10 seconds max
; this should take 20 minutes

(defun save-gen (gen filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (ai gen)
      (format stream "~{~a,~}~a~%" (ai-dna ai) (ai-elo ai)))))

(defun load-gen (filename)
  (let ((gen nil))
    (with-open-file (stream filename
                            :direction :input
                            :if-does-not-exist :error)
      (loop for line = (read-line stream nil)
            while line
            do (let ((tokens (split-string line ",")))
                 (push (make-ai
                         :dna (make-dna (nth 0 tokens)
                                        (nth 1 tokens)
                                        (nth 2 tokens)
                                        (nth 3 tokens)
                                        (nth 4 tokens)
                                        (nth 5 tokens)
                                        (nth 6 tokens)
                                        (nth 7 tokens)
                                        (nth 8 tokens)
                                        (nth 9 tokens)
                                        (nth 10 tokens)
                                        (nth 11 tokens)
                                        (nth 12 tokens)
                                        (nth 13 tokens)
                                        (nth 14 tokens)
                                        (nth 15 tokens)
                                        (nth 16 tokens)
                                        (nth 17 tokens)
                                        (nth 18 tokens)
                                        (nth 19 tokens)
                                        (nth 20 tokens)
                                        (nth 21 tokens)
                                        (nth 22 tokens)
                                        (nth 23 tokens)
                                        (nth 24 tokens)
                                        (nth 25 tokens)
                                        (nth 26 tokens)
                                        (nth 27 tokens)
                                        (nth 28 tokens)
                                        (nth 29 tokens))
                         :elo (nth 30 tokens)) gen))))
    gen))

(defun evolution-step (gen n)
  ; TODO: add random for each subsequent step
  ; (setf *random-state* (make-random-state t))
  (format t "starting generation: ~a~%" n)
  ; (format t "length of the current generation: ~a~%" (length gen))
  ; (format t "~a~%" gen)
  (let* ((simulated-gen (simulate-gen gen))
         (new-gen (generate-new-generation simulated-gen))
         (prev-champion (get-champion gen))
         (new-champion (get-champion simulated-gen)))
    (when (not (equal-ai prev-champion new-champion))
      (evolution-step new-gen (+ n 1)))))

(defun simulate-gen (orig-gen)
  (let ((generation (copy-gen orig-gen)))
    (dotimes (j set-number)
      (dolist (ai generation)
        (let* ((opponent (pick-opponent ai generation))
               (res (simulate-set ai (nth opponent generation))))
          (setf (ai-elo ai) (elo (ai-elo ai) (ai-elo (nth opponent generation)) res))
          (setf (ai-elo (nth opponent generation)) (elo (ai-elo (nth opponent generation)) (ai-elo ai) (- 1 res))))))
    generation))

(defun simulate-set (ai1 ai2)
  (if (< (random 1.0) .5)
    (match ai1 ai2)
    (match ai2 ai1)))

(defun match (white black)
  1 ; in this case white always win
)

(defun pick-opponent (ai gen)
  (let* ((sorted-gen (sort (copy-gen gen) #'< :key #'ai-elo))
         (elo (ai-elo ai))
         (index (position ai sorted-gen :test #'equal-ai))
         (without-ai (remove ai sorted-gen :test #'equal-ai))
         (start (* (floor index 10) 10))
         (end (min (+ start 10) (length without-ai)))
         (candidates (subseq without-ai start end))
         (opponent (nth (random (length candidates)) candidates)))
    (position opponent gen :test #'equal-ai)))

(defun generate-new-generation (orig-gen)
  (let ((new-gen nil)
        (champion (get-champion orig-gen)))
    (dotimes (i (- (length orig-gen) 1))
      (let ((father (pick-random-gen orig-gen))
            (mother (pick-random-gen orig-gen)))
        (push (generate-new-ai father mother) new-gen)))
    (push champion new-gen)
  new-gen))

(defun get-champion (gen)
  (reduce (lambda (a b) (if (> (ai-elo a) (ai-elo b)) a b)) gen))

(defun pick-random-gen (gen)
  (let* ((total-elo (reduce #'+ gen :key #'ai-elo))
         (random-elo (random total-elo))
         (accumulated-elo 0))
    (loop for ai in gen
          do (incf accumulated-elo (ai-elo ai))
          if (>= accumulated-elo random-elo)
          return ai)))

(defun generate-new-ai (father mother)
  (make-ai :dna (mutate-dna (cross-over-dnas father mother))
           :elo (cross-elo father mother)))

(defun mutate-dna (dna)
  (mapcar (lambda (weight) (+ (random-range -0.1 0.1) weight (* weight (random-range -0.1 0.1)))) dna))

(defun cross-over-dnas (father mother)
  (let ((point (* 30 (/ (ai-elo father) (+ (ai-elo father) (ai-elo mother)))))
        (direction (random 1.0)))
    (if (< direction .5)
      (append (subseq (ai-dna father) 0 (round point)) (subseq (ai-dna mother) (round point)))
      (append (subseq (ai-dna mother) 0 (round point)) (subseq (ai-dna father) (round point))))))

(defun cross-elo (father mother)
  (/ (+ (ai-elo father) (ai-elo mother)) 2))

(defun expected-score (rating-a rating-b)
  (/ 1 (+ 1 (expt 10 (/ (- rating-b rating-a) 400)))))

(defun elo (rating-a rating-b res)
  (+ rating-a (* 20 (- res (expected-score rating-a rating-b)))))

(defun random-range (a b)
  (+ a (coerce (random (coerce (- b a) 'float)) 'float)))
