(in-package :checkers-ai)

(defstruct ai
  dna
  elo)

(defun make-dna (w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20 w21 w22 w23 w24 w25 w26 w27 w28 w29 w30)
  "Creates the dna of a certain ai"
  (list w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20 w21 w22 w23 w24 w25 w26 w27 w28 w29 w30))

(defun copy-gen (orig-gen)
  "Deep copies an ai generation"
  (mapcar #'copy-ai orig-gen))

(defun equal-ai (ai1 ai2)
  "Deep equal two ais"
  (and (equal (ai-dna ai1) (ai-dna ai2))
       (equal (ai-elo ai1) (ai-elo ai2))))

(defun dna-distance (ai1 ai2)
  "Computes the distance between two ais dna"
  (let ((dna1 (ai-dna ai1))
        (dna2 (ai-dna ai2)))
    (reduce #'+
            (mapcar #'abs
                    (mapcar #'-
                            dna1
                            dna2)))))

(defun generation-variation (gen)
  "Computes the total variation in two generations"
  (let ((champion (get-champion gen)))
    (reduce #'+
            (mapcar (lambda (ai)
                      (dna-distance ai champion))
                    gen))))

(defun init-gen ()
  "Initializes the 1st generation of ais"
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
          :elo 1000.0)
        gen))
    gen))

(defun gaussian (a)
  "Creates a gaussian random number from a psoedo standard deviation"
  (+ (* a (random-range -1.0 1.0))
     (* a (random-range -1.0 1.0))
     (* a (random-range -1.0 1.0))
     (* a (random-range -1.0 1.0))
     (* a (random-range -1.0 1.0))
     (* a (random-range -1.0 1.0))
     (* a (random-range -1.0 1.0))
     (* a (random-range -1.0 1.0))
     (* a (random-range -1.0 1.0))
     (* a (random-range -1.0 1.0))))

(defun get-gen-from-champion (champion)
  "Create a new generation from a champion"
  (let ((gen nil))
    (dotimes (i 64)
      (push
        (make-ai
          :dna (make-dna
                 (+ (nth 0 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 1 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 2 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 3 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 4 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 5 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 6 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 7 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 8 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 9 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 10 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 11 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 12 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 13 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 14 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 15 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 16 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 17 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 18 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 19 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 20 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 21 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 22 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 23 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 24 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 25 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 26 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 27 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 28 (ai-dna champion)) (gaussian 0.5))
                 (+ (nth 29 (ai-dna champion)) (gaussian 0.5))
                 )
          :elo (ai-elo champion))
        gen))
    gen))

(defun get-average-ai (champion filename)
  "Compute the tournament for the average ai"
  (let ((gen (get-gen-from-champion champion))
        (sim-gen (simulate-gen gen))
        (sorted-gen (sort (copy-gen sim-gen) #'< :key #'ai-elo)))
    (save-gen sorted-gen sorted-gen filename)))

(defun save-gen (gen filename)
  "Save an ai generation to a file"
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (ai gen)
      (format stream "~{~a,~}~a~%" (ai-dna ai) (ai-elo ai)))))


; TODO: fix this
(defun parse-number (line)
  "Parse a number from a line"
  (loop
    :with n := (length line)
    :for pos := 0 :then chars
    :while (< pos n)
    :for (float chars) := (multiple-value-list
            (read-from-string line nil nil :start pos))
    :collect float))

(defun load-gen (filename)
  "Load a generation from a file"
  (let ((gen nil))
    (with-open-file (stream filename
                            :direction :input
                            :if-does-not-exist :error)
      (loop for line = (read-line stream nil)
            while line
            do (let ((tokens (split-string line ",")))
                 (push (make-ai
                         :dna (make-dna (nth 0 (parse-number(nth 0 tokens)))
                                        (nth 0 (parse-number (nth 1 tokens)))
                                        (nth 0 (parse-number (nth 2 tokens)))
                                        (nth 0 (parse-number (nth 3 tokens)))
                                        (nth 0 (parse-number (nth 4 tokens)))
                                        (nth 0 (parse-number (nth 5 tokens)))
                                        (nth 0 (parse-number (nth 6 tokens)))
                                        (nth 0 (parse-number (nth 7 tokens)))
                                        (nth 0 (parse-number (nth 8 tokens)))
                                        (nth 0 (parse-number (nth 9 tokens)))
                                        (nth 0 (parse-number (nth 10 tokens)))
                                        (nth 0 (parse-number (nth 11 tokens)))
                                        (nth 0 (parse-number (nth 12 tokens)))
                                        (nth 0 (parse-number (nth 13 tokens)))
                                        (nth 0 (parse-number (nth 14 tokens)))
                                        (nth 0 (parse-number (nth 15 tokens)))
                                        (nth 0 (parse-number (nth 16 tokens)))
                                        (nth 0 (parse-number (nth 17 tokens)))
                                        (nth 0 (parse-number (nth 18 tokens)))
                                        (nth 0 (parse-number (nth 19 tokens)))
                                        (nth 0 (parse-number (nth 20 tokens)))
                                        (nth 0 (parse-number (nth 21 tokens)))
                                        (nth 0 (parse-number (nth 22 tokens)))
                                        (nth 0 (parse-number (nth 23 tokens)))
                                        (nth 0 (parse-number (nth 24 tokens)))
                                        (nth 0 (parse-number (nth 25 tokens)))
                                        (nth 0 (parse-number (nth 26 tokens)))
                                        (nth 0 (parse-number (nth 27 tokens)))
                                        (nth 0 (parse-number (nth 28 tokens)))
                                        (nth 0 (parse-number (nth 29 tokens))))
                         :elo (nth 30 tokens)) gen))))
    gen))

(defun evolution-step (gen n)
  "Compute a generation in the genetic algorithm from a generation"
  (compute-diagonals)
  (setf *random-state* (make-random-state t))
  (format t "starting generation: ~a~%" n)
  (let* ((simulated-gen (simulate-gen gen))
         (test (format t "finished simulation~%"))
         (new-gen (generate-new-generation simulated-gen))
         (test (format t "finished mutation~%"))
         (prev-champion (get-champion gen))
         (new-champion (get-champion simulated-gen)))
    ; (format t "generation variation: ~a~%" (generation-variation new-gen))
    (save-gen new-gen (format nil "weights/~a.csv" n))
    (when (or (not (equal-ai prev-champion new-champion)) (> (generation-variation new-gen) 400.0))
      (evolution-step new-gen (+ n 1)))))

(defun simulate-gen (orig-gen)
  "Simulate an entire generation tournament"
  (let ((generation (copy-gen orig-gen)))
    (dotimes (j set-number)
      (dolist (ai generation)
        (let* ((opponent (pick-opponent ai generation))
               (res (simulate-set ai (nth opponent generation))))
          (setf (ai-elo ai) (elo (ai-elo ai) (ai-elo (nth opponent generation)) res))
          (setf (ai-elo (nth opponent generation)) (elo (ai-elo (nth opponent generation)) (ai-elo ai) (- 1 res))))))
    generation))

(defun simulate-set (ai1 ai2)
  "Simulate a set between two ais"
  (if (< (random 1.0) .5)
    (match ai1 ai2)
    (match ai1 ai2)))

(defun ai-turn (state ai)
  "Simulate a turn for an ai"
  (let ((move (ai-search state +search-depth+ (ai-dna ai))))
    (result move state)))

(defun match (white black)
  "Match between two ais"
  (let* ((state (init-state)))
    (dotimes (i 256)
      (ai-turn state white)
      (let* ((actions (actions state))
             (terminal (terminal-test state actions +white+))
             (utility (terminal-utility-pair-utility terminal)))
        (if (terminal-utility-pair-terminal terminal)
          (cond ((equal utility 1) (return-from match 1))
                ((equal utility -1) (return-from match 0))
                ((equal utility 0) (return-from match 0.5)))))
      (ai-turn state black)
      (let* ((actions (actions state))
             (terminal (terminal-test state actions +white+))
             (utility (terminal-utility-pair-utility terminal)))
        (if (terminal-utility-pair-terminal terminal)
          (cond ((equal utility 1) (return-from match 1))
                ((equal utility -1) (return-from match 0))
                ((equal utility 0) (return-from match 0.5))))))
    0.5))

(defun pick-opponent (ai gen)
  "Picks an opponent for the ai in the relevant ladder"
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
  "Creates a new generation from a sorted original generation"
  (let ((new-gen nil)
        (champion (get-champion orig-gen)))
    (dotimes (i (- (length orig-gen) 1))
      (let ((father (pick-random-gen orig-gen))
            (mother (pick-random-gen orig-gen)))
        (push (generate-new-ai father mother) new-gen)))
    (push champion new-gen)
  new-gen))

(defun get-champion (gen)
  "Get the champion of a generation"
  (reduce (lambda (a b) (if (> (ai-elo a) (ai-elo b)) a b)) gen))

(defun pick-random-gen (gen)
  "Pick a random member of a generation"
  (let* ((total-elo (reduce #'+ gen :key #'ai-elo))
         (random-elo (random total-elo))
         (accumulated-elo 0))
    (loop for ai in gen
          do (incf accumulated-elo (ai-elo ai))
          if (>= accumulated-elo random-elo)
          return ai)))

(defun generate-new-ai (father mother)
  "Generates a new ai from two parents"
  (make-ai :dna (mutate-dna (cross-over-dnas father mother))
           :elo (cross-elo father mother)))

(defun mutate-dna (dna)
  "Mutates the dna of two ais"
  (mapcar (lambda (weight) (+ (random-range -0.1 0.1) weight (* weight (random-range -0.1 0.1)))) dna))

(defun cross-over-dnas (father mother)
  "Crosses over the dnas of two ais"
  (let ((point (* 30 (/ (ai-elo father) (+ (ai-elo father) (ai-elo mother)))))
        (direction (random 1.0)))
    (if (< direction .5)
      (append (subseq (ai-dna father) 0 (round point)) (subseq (ai-dna mother) (round point)))
      (append (subseq (ai-dna mother) 0 (round point)) (subseq (ai-dna father) (round point))))))

(defun cross-elo (father mother)
  "Crosses the elo of two ais"
  (/ (+ (ai-elo father) (ai-elo mother)) 2.0))

(defun expected-score (rating-a rating-b)
  "Computes the expectore score for ELO from the previous ELO"
  (/ 1.0 (+ 1.0 (expt 10 (/ (- rating-b rating-a) 400.0)))))

(defun elo (rating-a rating-b res)
  "Computes the ELO after the end of a match"
  (max 100.0 (+ rating-a (* 20 (- res (expected-score rating-a rating-b))))))

(defun random-range (a b)
  "Creates a random float between a and b"
  (+ a (coerce (random (coerce (- b a) 'float)) 'float)))
