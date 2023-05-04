(in-package :checkers-ai)

(defstruct utility-move-pair utility move)
(defstruct outcome-move-pair outcome move)
(defstruct state-outcome-move-pair state outcome move)

(defvar *endgame-db-mutex* (sb-thread:make-mutex :name "edb"))

(defun ai-search (state depth search-time ai opening-database endgame-database)
  (let* ((actions (actions state))
         (openings opening-database)
         (state-hash (state-hash state))
         (endgame-database endgame-database))
    (let ((opening-action (gethash state-hash openings)))
      (if opening-action
        opening-action
        (if (equal (list-length actions) 1)
          (nth 0 actions)
          (iterative-alpha-beta-search state depth search-time ai endgame-database))))))

(defun iterative-alpha-beta-search (state depth search-time ai endgame-database)
  (if (equal (to-move state) +white+)
    (format t "white ")
    (format t "black "))
  (let* ((result (make-utility-move-pair :utility nil :move nil))
         (player (to-move state))
         (max-time (+ (get-internal-run-time) (* search-time 1000000)))
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
  (+ (* (nth 0 ai) (count-pawn state player))
     (* (nth 1 ai) (count-pawn state opponent))
     (* (nth 2 ai) (count-king state player))
     (* (nth 3 ai) (count-king state opponent))
     (* (nth 4 ai) (count-mobility state actions player))
     (* (nth 5 ai) (count-mobility state actions opponent))
     (* (nth 6 ai) (count-eating-mobility state actions player))
     (* (nth 7 ai) (count-eating-mobility state actions opponent))
     (* (nth 8 ai) (count-center-piece state player))
     (* (nth 9 ai) (count-center-piece state opponent))
     (* (nth 10 ai) (count-front-piece state player))
     (* (nth 11 ai) (count-front-piece state opponent))
     (* (nth 12 ai) (count-back-piece state player))
     (* (nth 13 ai) (count-back-piece state opponent))
     (* (nth 14 ai) (count-left-piece state player))
     (* (nth 15 ai) (count-left-piece state opponent))
     (* (nth 16 ai) (count-right-piece state player))
     (* (nth 17 ai) (count-right-piece state opponent))
     (* (nth 18 ai) (count-side-piece state player))
     (* (nth 19 ai) (count-side-piece state opponent))
     (* (nth 20 ai) (count-diagonal-piece state player))
     (* (nth 21 ai) (count-diagonal-piece state opponent))
     (* (nth 22 ai) (count-safe-king state player))
     (* (nth 23 ai) (count-safe-king state opponent))
     (* (nth 24 ai) (count-safe-pawn state player))
     (* (nth 25 ai) (count-safe-pawn state opponent))
     (* (nth 26 ai) (count-supported-piece state player))
     (* (nth 27 ai) (count-supported-piece state player))
     (* (nth 28 ai) (count-jumps state player))
     (* (nth 29 ai) (count-jumps state player)))))

(defun count-pawn (state player)
  "Counts the number of pawns of a player"
  (if (equal player +white+)
    (count +white-pawn+ (state-board state))
    (count +black-pawn+ (state-board state))))

(defun count-king (state player)
  "Counts the number of kings of a player"
  (if (equal player +white+)
      (count +white-king+ (state-board state))
      (count +black-king+ (state-board state))))

(defun count-mobility (state actions player)
  "Counts the number of moves available to a player if it is their turn"
  (if (equal (to-move state) player)
    (length actions)
    0))

(defun count-eating-mobility (state actions player)
  "Count the number of eating moves available to a player if it is their turn"
  (if (equal (to-move state) player)
    (length (select-eating actions))
    0))

(defun count-center-piece (state player)
  "Count the number of pieces in the center"
  (if (equal player +white+)
    (let ((sub-board (select-subboard (state-board state) (list 26 28 35 37))))
      (+ (count +white-pawn+ sub-board) (count +white-king+ sub-board)))
    (let ((sub-board (select-subboard (state-board state) (list 26 28 35 37))))
      (+ (count +black-pawn+ sub-board) (count +black-king+ sub-board)))))

(defun count-front-piece (state player)
  "Count the number of piece in the front"
  (if (equal player +white+)
    (let ((sub-board (select-subboard (state-board state) (list 1 3 5 7 8 10 12 14))))
      (+ (count +white-pawn+ sub-board) (count +white-king+ sub-board)))
    (let ((sub-board (select-subboard (state-board state) (list 49 51 53 55 56 58 60 62))))
      (+ (count +black-pawn+ sub-board) (count +black-king+ sub-board)))))

(defun count-back-piece (state player)
  "Count the number of piece in the back"
  (if (equal player +white+)
    (let ((sub-board (select-subboard (state-board state) (list 49 51 53 55 56 58 60 62))))
      (+ (count +white-pawn+ sub-board) (count +white-king+ sub-board)))
    (let ((sub-board (select-subboard (state-board state) (list 1 3 5 7 8 10 12 14))))
      (+ (count +black-pawn+ sub-board) (count +black-king+ sub-board)))))

(defun count-left-piece (state player)
  "Count the number of piece in the left"
  (if (equal player +white+)
    (let ((sub-board (select-subboard (state-board state) (list 40 42 49 56 58))))
      (+ (count +white-pawn+ sub-board) (count +white-king+ sub-board)))
    (let ((sub-board (select-subboard (state-board state) (list 5 7 14 21 23))))
      (+ (count +black-pawn+ sub-board) (count +black-king+ sub-board)))))

(defun count-right-piece (state player)
  "Count the number of piece in the right"
  (if (equal player +white+)
    (let ((sub-board (select-subboard (state-board state) (list 46 53 55 62))))
      (+ (count +white-pawn+ sub-board) (count +white-king+ sub-board)))
    (let ((sub-board (select-subboard (state-board state) (list 1 8 10 17))))
      (+ (count +black-pawn+ sub-board) (count +black-king+ sub-board)))))

(defun count-side-piece (state player)
  "Count the number of piece in the side"
  (if (equal player +white+)
    (let ((sub-board (select-subboard (state-board state) (list 7 8 23 24 39 40 55 56))))
      (+ (count +white-pawn+ sub-board) (count +white-king+ sub-board)))
    (let ((sub-board (select-subboard (state-board state) (list 7 8 23 24 39 40 55 56))))
      (+ (count +black-pawn+ sub-board) (count +black-king+ sub-board)))))

(defun count-diagonal-piece (state player)
  "Count the number of piece on the diagonal"
  (if (equal player +white+)
    (let ((sub-board (select-subboard (state-board state) (list 7 14 21 28 35 42 49 56))))
      (+ (count +white-pawn+ sub-board) (count +white-king+ sub-board)))
    (let ((sub-board (select-subboard (state-board state) (list 7 14 21 28 35 42 49 56))))
      (+ (count +black-pawn+ sub-board) (count +black-king+ sub-board)))))

(defun count-safe-king (state player)
  "Count the number of king that are safe"
  ; TODO: add king safety
  0)

(defun count-safe-pawn (state player)
  "Count the number of safe pawns"
  ; TODO: add pawn safety
  0)

(defun count-supported-piece (state player)
  "Count the number of supported piece"
  ; TODO: add supported
  0)

(defun count-jumps (state player)
  "Count the number of jumps"
  ; TODO: add jumps
  0)

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

(defun save-state-outcome-move-pair (state-outcome-move-pair stream)
  (let* ((state (state-outcome-move-pair-state state-outcome-move-pair))
         (board (state-board state))
         (outcome (state-outcome-move-pair-outcome state-outcome-move-pair))
         (move (state-outcome-move-pair-move state-outcome-move-pair)))
    (format stream "~a~a~a~a,~a~a~a~a,~a~a~a~a,~a~a~a~a,~a~a~a~a,~a~a~a~a,~a~a~a~a,~a~a~a~a,~a,~a,~a~a~%"
            (nth 1 board)
            (nth 3 board)
            (nth 5 board)
            (nth 7 board)
            (nth 8 board)
            (nth 10 board)
            (nth 12 board)
            (nth 14 board)
            (nth 17 board)
            (nth 19 board)
            (nth 21 board)
            (nth 23 board)
            (nth 24 board)
            (nth 26 board)
            (nth 28 board)
            (nth 30 board)
            (nth 33 board)
            (nth 35 board)
            (nth 37 board)
            (nth 39 board)
            (nth 40 board)
            (nth 42 board)
            (nth 44 board)
            (nth 46 board)
            (nth 49 board)
            (nth 51 board)
            (nth 53 board)
            (nth 55 board)
            (nth 56 board)
            (nth 58 board)
            (nth 60 board)
            (nth 62 board)
            (state-player state)
            (state-eating state)
            outcome
            (if move
              (format nil ",~a,~a,~a,~a"
                      (action-from move)
                      (action-to move)
                      (action-player move)
                      (action-eaten move))
              ""))))

(defun load-state-outcome-move-pair (line)
  (let* ((tokens (split-string line ","))
         (row-1 (nth 0 tokens))
         (row-2 (nth 1 tokens))
         (row-3 (nth 2 tokens))
         (row-4 (nth 3 tokens))
         (row-5 (nth 4 tokens))
         (row-6 (nth 5 tokens))
         (row-7 (nth 6 tokens))
         (row-8 (nth 7 tokens))
         (player (parse-integer (nth 8 tokens)))
         (eating (parse-integer (nth 9 tokens)))
         (outcome (parse-integer (nth 10 tokens))))
    (let ((board (list 0 (parse-integer (string (char row-1 0))) 0 (parse-integer (string (char row-1 1))) 0 (parse-integer (string (char row-1 2))) 0 (parse-integer (string (char row-1 3)))
                       (parse-integer (string (char row-2 0))) 0 (parse-integer (string (char row-2 1))) 0 (parse-integer (string (char row-2 2))) 0 (parse-integer (string (char row-2 3))) 0
                       0 (parse-integer (string (char row-3 0))) 0 (parse-integer (string (char row-3 1))) 0 (parse-integer (string (char row-3 2))) 0 (parse-integer (string (char row-3 3)))
                       (parse-integer (string (char row-4 0))) 0 (parse-integer (string (char row-4 1))) 0 (parse-integer (string (char row-4 2))) 0 (parse-integer (string (char row-4 3))) 0
                       0 (parse-integer (string (char row-5 0))) 0 (parse-integer (string (char row-5 1))) 0 (parse-integer (string (char row-5 2))) 0 (parse-integer (string (char row-5 3)))
                       (parse-integer (string (char row-6 0))) 0 (parse-integer (string (char row-6 1))) 0 (parse-integer (string (char row-6 2))) 0 (parse-integer (string (char row-6 3))) 0
                       0 (parse-integer (string (char row-7 0))) 0 (parse-integer (string (char row-7 1))) 0 (parse-integer (string (char row-7 2))) 0 (parse-integer (string (char row-7 3)))
                       (parse-integer (string (char row-8 0))) 0 (parse-integer (string (char row-8 1))) 0 (parse-integer (string (char row-8 2))) 0 (parse-integer (string (char row-8 3))) 0)))
      (make-state-outcome-move-pair
        :state (make-state :board board
                           :player player
                           :eating eating
                           :previous nil
                           :countdown 32)
        :outcome outcome
        :move nil))))

(defun load-endgame (filename)
  (let ((endgame-database (make-hash-table :test #'state-test :hash-function #'state-hash)))
    (with-open-file (stream filename
                            :direction :input
                            :if-does-not-exist :error)
      (loop for line = (read-line stream nil)
            while line
            do (let ((som (load-state-outcome-move-pair line)))
                 (setf (gethash (state-outcome-move-pair-state som) endgame-database)
                       (make-outcome-move-pair :outcome (state-outcome-move-pair-outcome som)
                                               :move (state-outcome-move-pair-move som))))))
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
  (let* ((states (generate-states n))
        (ai (nth 0 (init-gen)))
        (endgame-database (make-hash-table :test #'state-test :hash-function #'state-hash))
        (threads nil)
        (sub-states (divide-states states 5)))
    (dolist (depth '(8 16 32))
      (dolist (sub-state sub-states)
        (push (sb-thread:make-thread #'process-state :arguments (list sub-state depth ai endgame-database)) threads))
      (mapc #'sb-thread:join-thread threads))
    (dolist (state states)
      (let ((result (gethash state endgame-database)))
        (when result (save-endgame state result filename))))))

(defun find-state (hash states)
  (find-if (lambda (state) (equal hash (state-hash state))) states))

(defun endgame-convert (n input-filename output-filename)
  (let* ((endgame-database (load-endgame input-filename))
         (states (generate-states n)))
    (with-open-file (out output-filename
                         :direction :output
                         :if-exists :append)
      (dolist (hash-value endgame-database)
        (let* ((hash (car hash-value))
               (omp (cdr hash-value))
               (state (find-state hash states)))
          (when state
            (save-state-outcome-move-pair
              (make-state-outcome-move-pair :state state
                                            :outcome (outcome-move-pair-outcome omp)
                                            :move (outcome-move-pair-move omp))
              out)))))))

(defun process-state (states depth ai endgame-database)
  (dolist (state states)
    (let* ((actions (actions state))
           (terminal (terminal-test state actions +white+))
           (utility (terminal-utility-pair-utility terminal)))
      (if (terminal-utility-pair-terminal terminal)
        (sb-thread:with-mutex (*endgame-db-mutex*)
                              (setf (gethash state endgame-database)
                                    (make-outcome-move-pair :outcome utility
                                                            :move nil)))
        (let ((result (endgame-search state depth (ai-dna ai) endgame-database)))
          (if (or (equal (outcome-move-pair-outcome result) 1)
                  (equal (outcome-move-pair-outcome result) -1))
            (setf (gethash state endgame-database) result)
            (when (equal depth 32) (sb-thread:with-mutex (*endgame-db-mutex*)
                                                         (setf (gethash state endgame-database) result))))))
      (format t "done processing state~%"))))

(defun divide-states (states n)
  (let* ((len (length states))
         (size (ceiling len n))
         (out '()))
    (dotimes (i (- n 1))
      (push (subseq states (* i size) (* (+ i 1) size)) out))
    (push (subseq states (* (- n 1) size) len) out)
    out))
