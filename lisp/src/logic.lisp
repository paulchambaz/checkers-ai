(in-package :checkers-ai)

(defvar *white-left-diagonals* nil)
(defvar *white-right-diagonals* nil)
(defvar *black-left-diagonals* nil)
(defvar *black-right-diagonals* nil)

(defstruct action
  "An action is a single movement on the board"
  from   ; the square from which the movement starts
  to     ; the square to which the movement ends
  player ; the player whose turn it is after the action
  eaten) ; the index of the piece that has been eaten (-1 when none)

(defstruct state
  "The state holds info about a given position"
  board    ; list of the values at each squares
  player   ; the player that is currently playing 0 for white and 1 for black
  eating   ; the piece that has just eaten and should keep eating (-1 if none)
  previous ; list of the six previous actions the state has done
  countdown) ; counter to draw resets when a piece eats

(defun copy-state (state)
  (make-state :board (copy-list (state-board state))
              :player (state-player state)
              :eating (state-eating state)
              :previous (if (state-previous state) (copy-list (state-previous state)) (state-previous state))
              :countdown (state-countdown state)))

(defun init-board ()
  "Creates a new board and places the pieces"
  ; TODO: half the squares are not used so it would lead to better memory use to
  ; not store them
  ; intializes the board as a list of grid-square^2 elements
  (let ((board (make-list +nb-squares+ :initial-element 0)))
    (dotimes (x (/ +grid-size+ 2))
      (dotimes (y (- (/ +grid-size+ 2) 1))
        ; we set the square at (x,y) to be a black pawn
        (setf (nth (+ (* y +grid-size+) 
                      (+ (* x 2) (mod (+ y 1) 2))) board) +black-pawn+)))
    (dotimes (x (/ +grid-size+ 2))
      (dotimes (y (- (/ +grid-size+ 2) 1))
        ; we set the square at (x,y) to be a white pawn
        (setf (nth (+ (* (- (- +grid-size+ 1) y) +grid-size+)
                      (+ (* x 2) (mod y 2))) board) +white-pawn+)))
    board))

(defun init-state ()
  (make-state :board (init-board)
              :player +white+
              :eating -1
              :previous nil
              :countdown 32))

(defun set-board ()
  "Creates a new board and places random pieces"
  (list 0 0 0 0 0 0 0 0 ; 0
        0 0 0 0 0 0 0 0 ; 1
        0 0 0 0 0 2 0 0 ; 2
        2 0 1 0 2 0 0 0 ; 3
        0 1 0 0 0 0 0 0 ; 4
        0 0 1 0 0 0 0 0 ; 5 
        0 0 0 0 0 2 0 0 ; 6
        0 0 0 0 0 0 0 0 ; 7
  )
)

(defun actions (state)
  "Return the list of actions"
  (if (equal (state-eating state) -1)
      (let ((actions (mapcan (lambda (piece) (get-piece-actions (getf piece :n) state)) (if (equal (state-player state) +white+) (get-whites (state-board state)) (get-blacks (state-board state))))))
        (or (select-eating actions) actions))
      (or (select-eating (get-piece-actions (state-eating state) state))
          (list (make-action :from -1 :to -1 :player (switch-player (state-player state)) :eaten -1)))))

(defun result (action state)
  "Creates a new ret-state from an action on a ret-state"
  (let ((ret-state (copy-state state)))
    (if (not (equal (action-to action) -1))
      (progn 
        ; push action to the list of actions
        (push-previous action ret-state)

        ; change the action of the player
        (setf (state-player ret-state) (action-player action))
        ; (format t "set action~%")

        ; move to piece to square
        (setf (nth (action-to action) (state-board ret-state)) (nth (action-from action) (state-board ret-state)))
        ; (format t "set movement~%")

        ; empty previous square
        (setf (nth (action-from action) (state-board ret-state)) 0)
        ; (format t "empty previous square~%")

        ; eating
        (if (equal (action-eaten action) -1)
          ; set the eating of the ret-state
          (progn
            (setf (state-eating ret-state) -1)
            (setf (state-countdown ret-state) (- (state-countdown ret-state) 1))
          )
          (progn
            (setf (nth (action-eaten action) (state-board ret-state)) 0)
            ; set the eating of the ret-state
            (setf (state-eating ret-state) (action-to action))
            (setf (state-countdown ret-state) 32)
          )
        )
        ; (format t "eating action~%")

        ; white promotion
        (when (and (is-white-pawn (nth (action-to action) (state-board ret-state)))
                   (< (action-to action) +grid-size+))
          (setf (nth (action-to action) (state-board ret-state)) 3)
          (setf (state-player ret-state) +black+)
          (setf (state-eating ret-state) -1)
        )

        ; black promotion
        (when (and (is-black-pawn (nth (action-to action) (state-board ret-state)))
                   (>= (action-to action) (- +nb-squares+ +grid-size+)))
          (setf (nth (action-to action) (state-board ret-state)) 4)
          (setf (state-player ret-state) +white+)
          (setf (state-eating ret-state) -1)
        )
      )
      (progn
        (setf (state-player ret-state) (action-player action))
        (setf (state-eating ret-state) -1)
      )
    )
    ret-state
  )
)

(defstruct terminal-utility-pair terminal utility)

(defun terminal-test (state actions player)
  "Returns a pair (terminal utility) about if the state is terminal"
  ; TODO: rewrite this so player looses if it has no longer any pieces regardless of if it is its turn or not
  (if (equal (state-player state) +white+)
    (progn
      ; white is playing
      (when (equal (list-length (get-blacks (state-board state))) 0)
        ; (format t "black looses since it has no pieces~%")
        ; black looses since it has no pieces
        (return-from terminal-test (if (equal player +white+)
                                     (make-terminal-utility-pair :terminal T :utility 1)
                                     (make-terminal-utility-pair :terminal T :utility -1))))
      (when (equal (list-length actions) 0)
        ; (format t "white looses since it has no actions~%")
        ; white looses since it has no actions
        (return-from terminal-test (if (equal player +white+)
                                     (make-terminal-utility-pair :terminal T :utility -1)
                                     (make-terminal-utility-pair :terminal T :utility 1)))))
    (progn
      ; black is playing
      (when (equal (list-length (get-whites (state-board state))) 0)
        ; (format t "white looses since it has no pieces~%")
        ; white looses since it has no pieces
        (return-from terminal-test (if (equal player +white+)
                                     (make-terminal-utility-pair :terminal T :utility -1)
                                     (make-terminal-utility-pair :terminal T :utility 1))))
      (when (equal (list-length actions) 0)
        ; (format t "black looses since it has no actions~%")
        ; black looses since it has no actions
        (return-from terminal-test (if (equal player +white+)
                                     (make-terminal-utility-pair :terminal T :utility 1)
                                     (make-terminal-utility-pair :terminal T :utility -1))))))

  ; check that we have not been doing the same moves three times in a row
  (when (and (equal (length (state-previous state)) 6)
             (equal (nth 0 (state-previous state))
                    (nth 2 (state-previous state)))
             (equal (nth 0 (state-previous state))
                    (nth 4 (state-previous state)))
             (equal (nth 1 (state-previous state))
                    (nth 3 (state-previous state)))
             (equal (nth 1 (state-previous state))
                    (nth 5 (state-previous state))))
    (return-from terminal-test (make-terminal-utility-pair :terminal T :utility 0)))

    ; check that a piece has been eaten in the last 32 turns
    (when (equal (state-countdown state) 0)
      (return-from terminal-test (make-terminal-utility-pair :terminal T :utility 0)))


  (return-from terminal-test (make-terminal-utility-pair :terminal nil :utility 0)))

(defun black-terminal-test (state)
  (equal (list-length (get-blacks (state-board state))) 0))

(defun white-terminal-test (state)
  (equal (list-length (get-whites (state-board state))) 0))


(defun push-previous (action state)
  (push action (state-previous state))
  (when (> (length (state-previous state)) 6) 
    (setf (state-previous state) (subseq (state-previous state) 0 6))))

(defun white-pawn-actions (n state)
  "Returns the list of white pawn actions"
  (append (white-pawn-eat n state 0 -1)
          (white-pawn-eat n state 0 1)
          (white-pawn-eat n state 1 -1)
          (white-pawn-eat n state 1 1)
          (white-pawn-move n state 0 -1)
          (white-pawn-move n state 0 1)))

(defun white-king-actions (n state)
  "Returns the list of white king actions"
  (append (white-king-eat n state 0 -1)
          (white-king-eat n state 0 1)
          (white-king-eat n state 1 -1)
          (white-king-eat n state 1 1)
          (white-king-move n state 0 -1)
          (white-king-move n state 0 1)
          (white-king-move n state 1 -1)
          (white-king-move n state 1 1)))

(defun black-pawn-actions (n state)
  "Returns the list of black pawn actions"
  (append (black-pawn-eat n state 1 -1)
          (black-pawn-eat n state 1 1)
          (black-pawn-eat n state 0 -1)
          (black-pawn-eat n state 0 1)
          (black-pawn-move n state 1 -1)
          (black-pawn-move n state 1 1)))

(defun black-king-actions (n state)
  "Returns the list of white king actions"
  (append (black-king-eat n state 1 -1)
          (black-king-eat n state 1 1)
          (black-king-eat n state 0 -1)
          (black-king-eat n state 0 1)
          (black-king-move n state 1 -1)
          (black-king-move n state 1 1)
          (black-king-move n state 0 -1)
          (black-king-move n state 0 1)))

; white actions

(defun white-pawn-move (n state direction diagonal)
  "Returns the list of pawn move action"
  (let ((actions nil) (square (get-diagonal n direction diagonal)))
    (when (and square (is-empty (nth square (state-board state))))
      ; (push (make-action n square 1 -1) actions))
      (push (make-action :from n :to square :player +black+ :eaten -1) actions))
    actions))

(defun white-pawn-eat (n state direction diagonal)
  "Returns the list of pawn eat action"
  (let ((actions nil) (square (get-diagonal n direction diagonal)))
    (when square
      (let ((ssquare (get-diagonal square direction diagonal)))
        (when ssquare
          (when (and (is-empty (nth ssquare (state-board state)))
                     (is-black (nth square (state-board state))))
            (push (make-action :from n :to ssquare :player +white+ :eaten square) actions)))))
    actions))

(defun white-king-move (n state direction diagonal)
  "Returns the list of king move action"
  (let ((actions nil))
    (labels ((recursive-move (position)
               (let ((square (get-diagonal position direction diagonal)))
                 (when (and square (is-empty (nth square (state-board state))))
                   (push (make-action :from n :to square :player +black+ :eaten -1) actions)
                   (recursive-move square)))))
      (recursive-move n))
    actions))

(defun white-king-eat (n state direction diagonal)
  "Returns the list of king move action"
  (let ((actions nil))
    (labels ((recursive-eat (position)
               (let ((square (get-diagonal position direction diagonal)))
                 (cond
                   ((and square (is-black (nth square (state-board state))))
                    square)
                   ((and square (is-empty (nth square (state-board state))))
                    (recursive-eat square))
                   (t
                    nil))))
             (recursive-move (position initial)
               (let ((ssquare (get-diagonal position direction diagonal)))
                 (when (and ssquare (is-empty (nth ssquare (state-board state))))
                   (push (make-action :from n :to ssquare :player +white+ :eaten initial) actions)
                   (recursive-move ssquare initial)))))
      (let ((square (recursive-eat n)))
        (when square
          (recursive-move square square)
          actions)))))

; black actions

(defun black-pawn-move (n state direction diagonal)
  "Returns the list of pawn move action"
  (let ((actions nil) (square (get-diagonal n direction diagonal)))
    (when (and square (is-empty (nth square (state-board state))))
      (push (make-action :from n :to square :player +white+ :eaten -1) actions))
    actions))

(defun black-pawn-eat (n state direction diagonal)
  "Returns the list of pawn eat action"
  (let ((actions nil) (square (get-diagonal n direction diagonal)))
    (when square
      (let ((ssquare (get-diagonal square direction diagonal)))
        (when ssquare
          (when (and (is-empty (nth ssquare (state-board state)))
                     (is-white (nth square (state-board state))))
            (push (make-action :from n :to ssquare :player +black+ :eaten square) actions)))))
    actions))

(defun black-king-move (n state direction diagonal)
  "Returns the list of king move action"
  (let ((actions nil))
    (labels ((recursive-move (position)
               (let ((square (get-diagonal position direction diagonal)))
                 (when (and square (is-empty (nth square (state-board state))))
                   (push (make-action :from n :to square :player +white+ :eaten -1) actions)
                   (recursive-move square)))))
      (recursive-move n))
    actions))

(defun black-king-eat (n state direction diagonal)
  "Returns the list of king move action"
  (let ((actions nil))
    (labels ((recursive-eat (position)
               (let ((square (get-diagonal position direction diagonal)))
                 (cond
                   ((and square (is-white (nth square (state-board state))))
                    square)
                   ((and square (is-empty (nth square (state-board state))))
                    (recursive-eat square))
                   (t
                    nil))))
             (recursive-move (position initial)
               (let ((ssquare (get-diagonal position direction diagonal)))
                 (when (and ssquare (is-empty (nth ssquare (state-board state))))
                   (push (make-action :from n :to ssquare :player +black+ :eaten initial) actions)
                   (recursive-move ssquare initial)))))
      (let ((square (recursive-eat n)))
        (when square
          (recursive-move square square)
          actions)))))

(defun get-piece-actions (n state)
  "Returns the list of actions from a single piece"
  ; if it is not on the (state-board state) we exit
  (when (= n -1) (return-from get-piece-actions nil))
  ; if we have to eat again with the same piece, but this piece is not selected
  ; we exit
  (when (and (not (= (state-eating state) -1)) (not (= n (state-eating state))))
    (return-from get-piece-actions nil))
  (let ((actions nil))
    ; white king movements
    (when (is-white-king (nth n (state-board state)))
      (setf actions (append actions (white-king-actions n state))))

    ; white pawn movements
    (when (is-white-pawn (nth n (state-board state)))
      (setf actions (append actions (white-pawn-actions n state))))

    ; black king movements
    (when (is-black-king (nth n (state-board state)))
      (setf actions (append actions (black-king-actions n state))))

    ; black pawn movements
    (when (is-black-pawn (nth n (state-board state)))
      (setf actions (append actions (black-pawn-actions n state))))

    actions))

(defun switch-player (player) (mod (+ player 1) 2))

(defun to-move (state)
  "Returns whos turn it is to play"
  (state-player state))

(defun select-from (from actions)
  "Selects only actions that start at from"
  (remove-if-not #'(lambda (action)
                     (equal (action-from action) from)) actions))

(defun select-to (to actions)
  "Selects only actions that end at to"
  (remove-if-not #'(lambda (action)
                     (= (action-to action) to)) actions))

(defun select-eating (actions)
  "Select only actions that do eat"
  (remove-if-not #'(lambda (action)
                     (not (= (action-eaten action) -1))) actions))

(defun get-pieces (board)
  "Creates a list of pieces on the board with format (:n n :id id)"
  (let ((positions nil))
    (dotimes (n +nb-squares+)
      ; we skip over empty squares
      (when (not (is-empty (nth n board)))
        (push (list :n n :id (nth n board)) positions)))
    positions))

(defun get-whites (board)
  "Creates a list of the white pieces"
  (let ((positions nil))
    (dotimes (n +nb-squares+)
      (when (or (is-white-pawn (nth n board)) (is-white-king (nth n board)))
        (push (list :n n :id (nth n board)) positions)))
    positions))

(defun get-white-pawns (board)
  "Creates a list of the white pawns"
  (let ((positions nil))
    (dotimes (n +nb-squares+)
      (when (is-white-pawn (nth n board))
        (push (list :n n :id (nth n board)) positions)))
    positions))

(defun get-white-kings (board)
  "Creates a list of the white kings"
  (let ((positions nil))
    (dotimes (n +nb-squares+)
      (when (is-white-king (nth n board))
        (push (list :n n :id (nth n board)) positions)))
    positions))

(defun get-blacks (board)
  "Creates a list of the black pieces"
  (let ((positions nil))
    (dotimes (n +nb-squares+)
      (when (or (is-black-pawn (nth n board)) (is-black-king (nth n board)))
        (push (list :n n :id (nth n board)) positions)))
    positions))

(defun get-black-pawns (board)
  "Creates a list of the black pawns"
  (let ((positions nil))
    (dotimes (n +nb-squares+)
      (when (is-black-pawn (nth n board))
        (push (list :n n :id (nth n board)) positions)))
    positions))

(defun get-black-kings (board)
  "Creates a list of the black kings"
  (let ((positions nil))
    (dotimes (n +nb-squares+)
      (when (is-black-king (nth n board))
        (push (list :n n :id (nth n board)) positions)))
    positions))

(defun get-diagonal (n p d)
  "Get the square in diagonal"
  ; n is the starting square
  ; p is the player - 0 for white 1 for black
  ; d is the direction - -1 for left and 1 for right
  (if (= p 0)
      (if (= d -1)
          (nth (- (- +nb-squares+ 1) n) *white-left-diagonals*)
          (nth (- (- +nb-squares+ 1) n) *white-right-diagonals*))
      (if (= d -1)
          (nth (- (- +nb-squares+ 1) n) *black-left-diagonals*)
          (nth (- (- +nb-squares+ 1) n) *black-right-diagonals*))))

(defun compute-diagonals ()
  "Computes the diagonals movement from all squares to all other squares in a
  list"
  (dotimes (n +nb-squares+)
    (push (compute-white-left-diagonal n) *white-left-diagonals*)
    (push (compute-white-right-diagonal n) *white-right-diagonals*)
    (push (compute-black-left-diagonal n) *black-left-diagonals*)
    (push (compute-black-right-diagonal n) *black-right-diagonals*)))

(defun compute-white-left-diagonal (n)
  "Computes the white left diagonals of a square"
  (let ((out (- n 9)))
    ; check that the square is valid
    (if (and (>= out 0) (= (get-line out) (- (get-line n) 1)))
        out)))

(defun compute-white-right-diagonal (n)
  "Computes the white left diagonals of a square"
  (let ((out (- n 7)))
    ; check that the square is valid
    (if (and (>= out 0) (= (get-line out) (- (get-line n) 1)))
        out)))

(defun compute-black-left-diagonal (n)
  "Computes the white left diagonals of a square"
  (let ((out (+ n 9)))
    ; check that the square is valid
    (if (and (< out +nb-squares+) (= (get-line out) (+ (get-line n) 1)))
        out)))

(defun compute-black-right-diagonal (n)
  "Computes the white left diagonals of a square"
  (let ((out (+ n 7)))
    ; check that the square is valid
    (if (and (< out +nb-squares+) (= (get-line out) (+ (get-line n) 1)))
        out)))

(defun board-index (index)
  (let ((row (floor index 4))
        (column (mod index 4)))
    (+ (* row 8) (* 2 column) (- 1 (mod row 2)))))

(defmethod print-object ((obj action) stream)
  (let ((from (action-from obj))
        (to (action-to obj))
        (player (action-player obj))
        (eaten (action-eaten obj)))
    (format stream "Action: ")
    (format stream "from: ~a to: ~a player: ~a eaten: ~a~%"
            from
            to
            (if (equal player +white+)
              "white"
              "black")
            eaten)))

(defmethod print-object ((obj state) stream)
  (let ((board (state-board obj))
        (player (state-player obj))
        (eating (state-eating obj))
        (previous (state-previous obj))
        (countdown (state-countdown obj)))
    (format stream "State: ")
    (format stream "board: ~%")
    (loop for i below 8 do
          (loop for j below 8 do
                (format stream " ~a" (nth (+ (* i 8) j) board)))
          (format stream "~%"))
    (format stream "player: ~a eating: ~a countdown: ~a~%actions: ~a~%"
            (if (equal player +white+)
              "white"
              "black")
            eating
            countdown
            previous)))

(defun action-hash (action)
  (+ (* 1627 (action-from action))
     (* 4969 (action-to action))
     (* 1171 (action-player action))
     (* 6029 (action-eaten action))))

(defun action-test (a1 a2)
  (and (equal (action-from a1) (action-from a2))
       (equal (action-to a1) (action-to a2))
       (equal (action-player a1) (action-player a2))
       (equal (action-eaten a1) (action-eaten a2))))

(defun state-hash (state)
  (+ (* 9421 (state-eating state))
     (* 8609 (state-player state))
     (* 2707 (nth 1 (state-board state)))
     (* 5839 (nth 3 (state-board state)))
     (* 2543 (nth 5 (state-board state)))
     (* 6163 (nth 7 (state-board state)))
     (* 8663 (nth 8 (state-board state)))
     (* 6833 (nth 10 (state-board state)))
     (* 4229 (nth 12 (state-board state)))
     (* 8761 (nth 14 (state-board state)))
     (* 4793 (nth 17 (state-board state)))
     (* 4337 (nth 19 (state-board state)))
     (* 9319 (nth 21 (state-board state)))
     (* 7489 (nth 23 (state-board state)))
     (* 3121 (nth 24 (state-board state)))
     (* 3433 (nth 26 (state-board state)))
     (* 5981 (nth 28 (state-board state)))
     (* 3251 (nth 30 (state-board state)))
     (* 6529 (nth 33 (state-board state)))
     (* 7927 (nth 35 (state-board state)))
     (* 8221 (nth 37 (state-board state)))
     (* 1433 (nth 39 (state-board state)))
     (* 3769 (nth 40 (state-board state)))
     (* 4561 (nth 42 (state-board state)))
     (* 5227 (nth 44 (state-board state)))
     (* 3623 (nth 46 (state-board state)))
     (* 6121 (nth 49 (state-board state)))
     (* 3061 (nth 51 (state-board state)))
     (* 6907 (nth 53 (state-board state)))
     (* 3923 (nth 55 (state-board state)))
     (* 7481 (nth 56 (state-board state)))
     (* 9181 (nth 58 (state-board state)))
     (* 8543 (nth 60 (state-board state)))
     (* 6113 (nth 62 (state-board state)))))

(defun state-test (s1 s2)
  (and (equal (state-board s1) (state-board s2))
       (equal (state-player s1) (state-player s2))
       (equal (state-eating s1) (state-eating s2))
       (equal (state-previous s1) (state-previous s2))
       (equal (state-countdown s1) (state-countdown s2))))
