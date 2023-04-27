(in-package :checkers-ai)

(defstruct utility-move-pair utility move)
(defstruct outcome-move-pair outcome move)

(defvar *endgame-db-mutex* (sb-thread:make-mutex :name "edb"))

(defun ai-search (state depth ai)
  (let* ((actions (actions state))
         (openings (load-opening "data/opening.csv"))
         (state-hash (state-hash state))
        (endgame-database (make-hash-table :test #'state-test :hash-function #'state-hash)))
    (let ((opening-action (gethash state-hash openings)))
      (if opening-action
        opening-action
        (if (equal (list-length actions) 1)
          (nth 0 actions)
          (iterative-alpha-beta-search state depth ai endgame-database))))))

(defun iterative-alpha-beta-search (state depth ai endgame-database)
  (if (equal (to-move state) +white+)
    (format t "white ")
    (format t "black "))
  (let* ((result (make-utility-move-pair :utility nil :move nil))
         (player (to-move state))
         (max-time (+ (get-internal-run-time) (* +search-time+ 1000000)))
         (max-depth 0)
         (history (make-hash-table :test #'action-test :hash-function #'action-hash))
         (killer-moves (make-array (+ depth 1) :initial-element (make-array 2 :initial-element (make-array 2 :initial-element nil)))))
    (block outer-loop
           (dotimes (n depth)
             (let ((res (max-value state most-negative-fixnum most-positive-fixnum player (+ n 1) max-time ai history killer-moves endgame-database)))
               ; we have a guaranted win or lose in n moves so we can just return
               (when (< (get-internal-run-time) max-time)
                 (setf result res)
                 (setf max-depth (+ n 1))
                 (when (or (equal (utility-move-pair-utility res) (* 1 +win-utility+))
                           (equal (utility-move-pair-utility res) (* -1 +win-utility+)))
                   (return-from outer-loop))))))
    (format t "move: ~a value: ~a depth: ~a~%" (utility-move-pair-move result) (utility-move-pair-utility result) max-depth)
    (utility-move-pair-move result)))

(defun max-value (state alpha beta player depth max-time ai history killer-moves endgame-database)
  ; TODO: we will always assume we are reading from the pov of white
  ; so when we read we need to make sure that if white looses and we
  ; are playing white we return -1 and if white wins and we are
  ; playing white we return 1, opposite for black!
  (let ((actions (actions state))
        (endgame (sb-thread:with-mutex (*endgame-db-mutex*) (gethash state endgame-database))))
    (if (and endgame (not (equal (outcome-move-pair-outcome endgame) 0)))
      (return-from max-value (make-utility-move-pair :utility (* (outcome-move-pair-outcome endgame)
                                                                 (if (equal player +white+) 1 -1) +win-utility+)
                                                     :move (outcome-move-pair-move endgame)))
      (if (or (> (get-internal-run-time) max-time) (equal depth 0))
        (return-from max-value (make-utility-move-pair :utility (utility state actions player ai) :move nil))
        (let ((terminal (terminal-test state actions player)))
          (if (terminal-utility-pair-terminal terminal)
            (return-from max-value (make-utility-move-pair :utility (* (terminal-utility-pair-utility terminal) +win-utility+) :move nil))
            (let ((best (make-utility-move-pair :utility most-negative-fixnum :move nil))
                  (sorted-actions (order-moves actions player depth history killer-moves)))
              (dolist (move sorted-actions)
                (let* ((next-state (result move state))
                       (current (if (equal (to-move next-state) player)
                                  (max-value next-state alpha beta player (- depth 1) max-time ai history killer-moves endgame-database)
                                  (min-value next-state alpha beta player (- depth 1) max-time ai history killer-moves endgame-database))))
                  (when (> (utility-move-pair-utility current) (utility-move-pair-utility best))
                    (setf (utility-move-pair-utility best) (utility-move-pair-utility current))
                    (setf (utility-move-pair-move best) move)
                    (setf alpha (max alpha (utility-move-pair-utility best))))
                  (when (>= (utility-move-pair-utility best) beta)
                    (update-history move history)
                    (update-killer-moves move (to-move next-state) depth killer-moves history)
                    (return-from max-value best))))
              best)))))))

(defun min-value (state alpha beta player depth max-time ai history killer-moves endgame-database)
  (let ((actions (actions state))
        (endgame (sb-thread:with-mutex (*endgame-db-mutex*) (gethash state endgame-database))))
    (if (and endgame (not (equal (outcome-move-pair-outcome endgame) 0)))
      (return-from min-value (make-utility-move-pair :utility (* (outcome-move-pair-outcome endgame)
                                                                 (if (equal player +white+) 1 -1) +win-utility+)
                                                     :move (outcome-move-pair-move endgame)))
      (if (or (> (get-internal-run-time) max-time) (equal depth 0))
        (return-from min-value (make-utility-move-pair :utility (utility state actions player ai) :move nil))
        (let* ((terminal (terminal-test state actions player)))
          (if (terminal-utility-pair-terminal terminal)
            (return-from min-value (make-utility-move-pair :utility (* (terminal-utility-pair-utility terminal) +win-utility+) :move nil))
            (let ((best (make-utility-move-pair :utility most-positive-fixnum :move nil))
                  (sorted-actions (order-moves actions player depth history killer-moves)))
              (dolist (move sorted-actions)
                (let* ((next-state (result move state))
                       (current (if (equal (to-move next-state) player)
                                  (max-value next-state alpha beta player (- depth 1) max-time ai history killer-moves endgame-database)
                                  (min-value next-state alpha beta player (- depth 1) max-time ai history killer-moves endgame-database))))
                  (when (< (utility-move-pair-utility current) (utility-move-pair-utility best))
                    (setf (utility-move-pair-utility best) (utility-move-pair-utility current))
                    (setf (utility-move-pair-move best) move)
                    (setf beta (min beta (utility-move-pair-utility best))))
                  (when (<= (utility-move-pair-utility best) alpha)
                    (update-history move history)
                    (update-killer-moves move (to-move next-state) depth killer-moves history)
                    (return-from min-value best))))
              best)))))))

(defun order-moves (actions player depth history killer-moves)
  ; sort the actions based on their history
  (let ((sorted-actions (stable-sort (if (equal player +white+) actions (reverse actions))
                                     (lambda (a1 a2) (compare-frequencies a1 a2 history))))
        (current-killer-moves (aref (aref killer-moves depth) player))
        (killer-moves-actions '())
        (non-killer-moves-actions '()))

    ; get the actions present in the current killer moves list
    (loop for action across current-killer-moves
          do (when (and action (member action sorted-actions))
               (push action killer-moves-actions)))
   
    ; get the actions not present in the current killer move list
    (dolist (action sorted-actions)
      (unless (member action killer-moves-actions)
        (push action non-killer-moves-actions)))

    ; append the two lists together
    (append (reverse killer-moves-actions) (reverse non-killer-moves-actions))))

(defun update-history (action history)
  (let ((frequency (gethash action history)))
    (if frequency
        (setf (gethash action history) (+ frequency 1))
        (setf (gethash action history) 0))))

(defun update-killer-moves (action player depth killer-moves history)
  (let* ((current-killer-moves (aref (aref killer-moves depth) player))
         (action-value (gethash action history)))
    (cond
      ((or (null (aref current-killer-moves 0))
           (> action-value (gethash (aref current-killer-moves 0) history)))
       (setf (aref current-killer-moves 1) (aref current-killer-moves 0))
       (setf (aref current-killer-moves 0) action))
      ((or (null (aref current-killer-moves 1))
           (> action-value (gethash (aref current-killer-moves 1) history)))
       (setf (aref current-killer-moves 1) action)))))

(defun compare-frequencies (a1 a2 history)
  (> (gethash a1 history 0) (gethash a2 history 0)))

(defun utility (state actions player ai)
  "Utility function of the algorithm"
  (let ((opponent (switch-player player)))
  (+ (* (get-weight 0 ai) (count-pawn state player))
     (* (get-weight 1 ai) (count-pawn state opponent))
     (* (get-weight 2 ai) (count-king state player))
     (* (get-weight 3 ai) (count-king state opponent))
     )))

(defun count-pawn (state player)
  "Counts the number of pawns of a player"
  (if (= player +white+)
    (count +white-pawn+ (state-board state))
    (count +black-pawn+ (state-board state))))

(defun count-king (state player)
  "Counts the number of kings of a player"
  (if (= player +white+)
      (count +white-king+ (state-board state))
      (count +black-king+ (state-board state))))

(defun count-mobility (actions)
  "Counts the number of moves available to a player"
    (list-length actions))

; (defun count-safe-checkers (state player)
;   "Count the number of safe kings the player has"
;   (

; TODO: we need to write the list of all ways to evaluate the board

(defun compute-opening (p-actions filename)
  (compute-diagonals)
  (let* ((state (init-state))
         (prior-actions (subseq p-actions 0 (1- (length p-actions))))
         (last-action (nth 0 (last p-actions))))
    (dolist (p-action prior-actions)
      (let* ((actions (actions state))
             (from (select-from (nth 0 p-action) actions))
             (to (select-to (nth 1 p-action) from))
             (action (nth 0 to)))
        (setf state (result action state))))
    (let* ((actions (actions state))
           (from (select-from (nth 0 last-action) actions))
           (to (select-to (nth 1 last-action) from))
           (action (nth 0 to)))
      (save-opening state action filename))))

(defun save-opening (state action filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~a,~a,~a,~a,~a~%"
            (state-hash state)
            (action-from action)
            (action-to action)
            (action-player action)
            (action-eaten action))))

(defun load-opening (filename)
  (let ((action-map (make-hash-table :test 'equal)))
    (with-open-file (stream filename
                            :direction :input
                            :if-does-not-exist :error)
      (loop for line = (read-line stream nil)
            while line
            do (let* ((tokens (split-string line ","))
                      (state-hash (parse-integer (nth 0 tokens)))
                      (from (parse-integer (nth 1 tokens)))
                      (to (parse-integer (nth 2 tokens)))
                      (player (parse-integer (nth 3 tokens)))
                      (eaten (parse-integer (nth 4 tokens)))
                      (new-action (make-action :from from :to to :player player :eaten eaten)))
                 (setf (gethash state-hash action-map) new-action))))
    action-map))

(defun save-endgame (state outcome filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (let ((state-hash (state-hash state))
          (t-outcome (outcome-move-pair-outcome outcome))
          (action (outcome-move-pair-move outcome)))
      (if action
        (format stream "~a,~a,~a,~a,~a,~a~%"
                state-hash
                t-outcome
                (action-from action)
                (action-to action)
                (action-player action)
                (action-eaten action))
        (format stream "~a,~a~%"
                state-hash
                t-outcome)))))

(defun load-endgame (filename)
  (let ((endgame-database (make-hash-table :test #'state-test :hash-function #'state-hash)))
    (with-open-file (stream filename
                            :direction :input
                            :if-does-not-exist :error)
      (loop for line = (read-line stream nil)
            while line
            do (let* ((tokens (split-string line ","))
                      (state-hash (parse-integer (nth 0 tokens)))
                      (outcome (parse-integer (nth 1 tokens)))
                      (from (parse-integer (nth 2 tokens)))
                      (to (parse-integer (nth 3 tokens)))
                      (player (parse-integer (nth 4 tokens)))
                      (eaten (parse-integer (nth 5 tokens)))
                      (new-move (make-action :from from
                                               :to to
                                               :player player
                                               :eaten eaten))
                      (new-outcome (make-outcome-move-pair :outcome outcome
                                                           :move new-move)))
                 (setf (gethash state-hash endgame-database) new-outcome))))
    endgame-database))

(defun generate-permutations (n max)
  (labels ((permute (lst m)
             (if (= m 1)
                 (mapcar #'list lst)
                 (loop for elem in lst append
                       (mapcar (lambda (sub-perm) (cons elem sub-perm))
                               (permute (remove elem lst) (- m 1)))))))
    (remove-duplicates
     (mapcar (lambda (p)
               (sort (mapcar #'board-index p) #'<))
             (permute (loop for i from 0 to (- max 1) collect i) n))
     :test #'equal)))

(defun generate-boards (squares board)
  (if (null squares)
    (list board)
    (let ((new-boards '()))
      (dotimes (piece-type 4)
        (let ((new-board (copy-list board)))
          (setf (nth (car squares) new-board) (+ piece-type 1))
          (setf new-boards
                (nconc new-boards
                       (generate-boards (cdr squares) new-board)))))
      new-boards)))

(defun generate-states (n)
  ; first we generate all possible permutations of the pieces
  (let ((permutations (generate-permutations n 32))
        (states '()))
    ; then for each permutations
    (dolist (p permutations)
      ; enumerate the players
      (dotimes (player 2)
        ; create the board
        (dolist (board (generate-boards p (make-list 64 :initial-element 0)))
          (dolist (eating p)
            (when (equal (mod (nth eating board) 2) player)
              (push (make-state :board board
                                :player player
                                :eating eating
                                :previous nil
                                :countdown 32) states)))
          (push (make-state :board board
                            :player player
                            :eating -1
                            :previous nil
                            :countdown 32) states))))
    states))


(defun endgame-search (state depth ai endgame-database)
  (let* ((result (make-utility-move-pair :utility nil :move nil))
         (max-time (+ (get-internal-run-time) most-positive-fixnum))
         (player (to-move state))
         (history (make-hash-table :test #'action-test :hash-function #'action-hash))
         (killer-moves (make-array 33 :initial-element
                                   (make-array 2 :initial-element
                                               (make-array 2 :initial-element nil)))))
     (dotimes (n depth)
       (let ((res (max-value state most-negative-fixnum most-positive-fixnum
                             player (+ n 1) max-time ai history killer-moves endgame-database)))
         (when (< (get-internal-run-time) max-time)
           (setf result res)
           (when (equal (utility-move-pair-utility res) (* 1 +win-utility+)) 
             (return-from endgame-search
                          (make-outcome-move-pair :outcome 1
                                                  :move (utility-move-pair-move res))))
           (when (equal (utility-move-pair-utility res) (* -1 +win-utility+))
             (return-from endgame-search
                          (make-outcome-move-pair :outcome -1
                                                  :move (utility-move-pair-move res)))))))
     (make-outcome-move-pair :outcome 0 :move (utility-move-pair-move result))))

(defun endgame-compute (n filename)
  (compute-diagonals)
  (let ((states (generate-states n))
        (ai (nth 0 (init-gen)))
        (endgame-database (make-hash-table :test #'state-test :hash-function #'state-hash))
        (threads nil))
    (dolist (depth '(8))
      (dolist (state states)
        (push (sb-thread:make-thread #'process-state :arguments (list state depth ai endgame-database)) threads))
      (mapc #'sb-thread:join-thread threads))
    (dolist (state states)
      (let ((result (gethash state endgame-database)))
        (when result (save-endgame state result filename))))))

(defun process-state (state depth ai-dna endgame-database)
  (let* ((actions (actions state))
         (terminal (terminal-test state actions +white+))
         (utility (terminal-utility-pair-utility terminal)))
    (if (terminal-utility-pair-terminal terminal)
      (sb-thread:with-mutex (*endgame-db-mutex*)
                            (setf (gethash state endgame-database)
                                  (make-outcome-move-pair :outcome utility
                                                          :move nil)))
      (let ((result (endgame-search state depth (ai-dna (nth 0 ais)) endgame-database)))
        (if (or (equal (outcome-move-pair-outcome result) 1)
                (equal (outcome-move-pair-outcome result) -1))
          (setf (gethash state endgame-database) result)
          (when (equal depth 32) (sb-thread:with-mutex (*endgame-db-mutex*)
                                                       (setf (gethash state endgame-database) result))))))))
