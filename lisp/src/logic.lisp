(in-package :checkers-ai)

(defvar *white-left-diagonals* nil)
(defvar *white-right-diagonals* nil)
(defvar *black-left-diagonals* nil)
(defvar *black-right-diagonals* nil)

; defining the action
; an action is defined as follows
; the square from which the action starts
; the square to which the action ends
; the player that should play next
; the piece that has eaten (-1 if none)
; TODO the piece that has been eaten (-1 if none)
(defun make-action (from to player eaten) (list from to player eaten))
(defun get-action-from (action) (nth 0 action))
(defun get-action-to (action) (nth 1 action))
(defun get-action-player (action) (nth 2 action))
(defun get-action-eaten (action) (nth 3 action))

; defining a state
; a state is defined as follows
; the board - a list of the values at each squares
; the player - the player that is currently playing 0 for white 1 for black
; the piece that has just eaten and should keep eating (-1 if none)
(defun make-state (board player eating) (list board player eating))
(defun get-state-board (state) (nth 0 state))
(defun get-state-player (state) (nth 1 state))
(defun get-state-eating (state) (nth 2 state))
(defun copy-state (state) (make-state (copy-list (get-state-board state))
                                      (get-state-player state)
                                      (get-state-eating state)))

(defun init-board ()
  "Creates a new board and places the pieces"
  ; TODO half the squares are not used so it would lead to better memory use to
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

(defun set-board ()
  "Creates a new board and places random pieces"
  (list 0 0 0 0 0 3 0 0; 0
        0 0 0 0 0 0 0 0; 1
        0 0 0 0 0 0 0 0; 2
        0 0 0 0 0 0 1 0; 3
        0 0 0 0 0 0 0 1; 4
        0 0 0 0 0 0 1 0; 5 
        0 0 0 0 0 0 0 0; 6
        4 0 1 0 0 0 0 0; 7
  )
)

(defun actions (state)
  "Return the list of actions"
  (if (equal (get-state-eating state) -1)
      (let ((actions (mapcan (lambda (piece) (get-piece-actions (getf piece :n) state)) (if (equal (get-state-player state) +white+) (get-whites (get-state-board state)) (get-blacks (get-state-board state))))))
        (or (select-eating actions) actions))
      (or (select-eating (get-piece-actions (get-state-eating state) state))
          (list (make-action -1 -1 (switch-player (get-state-player state)) -1)))))

(defun result (action state)
  "Creates a new ret-state from an action on a ret-state"
  (let ((ret-state (copy-state state)))
    (if (not (equal (get-action-to action) -1))
      (progn 
        ; set the action of the ret-state
        (setf (nth 1 ret-state) (get-action-player action))

        ; move to piece to square
        (setf (nth (get-action-to action) (nth 0 ret-state)) (nth (get-action-from action) (nth 0 ret-state)))

        ; empty previous square
        (setf (nth (get-action-from action) (nth 0 ret-state)) 0)

        ; eating
        (if (not (equal (get-action-eaten action) -1))
          (progn
            (setf (nth (get-action-eaten action) (nth 0 ret-state)) 0)
            ; set the eating of the ret-state
            (setf (nth 2 ret-state) (get-action-to action))
          )
          ; set the eating of the ret-state
          (setf (nth 2 ret-state) -1)
        )

        ; white promotion
        (when (and (is-white-pawn (nth (get-action-to action) (nth 0 ret-state)))
                   (< (get-action-to action) +grid-size+))
          (setf (nth (get-action-to action) (nth 0 ret-state)) 3)
          (setf (nth 1 ret-state) +black+)
          (setf (nth 2 ret-state) -1)
        )

        ; black promotion
        (when (and (is-black-pawn (nth (get-action-to action) (nth 0 ret-state)))
                   (>= (get-action-to action) (- +nb-squares+ +grid-size+)))
          (setf (nth (get-action-to action) (nth 0 ret-state)) 4)
          (setf (nth 1 ret-state) +white+)
          (setf (nth 2 ret-state) -1)
        )
      )
      (progn
        (setf (nth 1 ret-state) (get-action-player action))
        (setf (nth 2 ret-state) -1)
      )
    )
    ret-state
  )
)

; TODO implement this as it is obviously better
(defun undo (action state)
  "Invert the change of state from an action"
  state)

(defun black-terminal-test (state)
  (equal (list-length (get-blacks (get-state-board state))) 0))

(defun white-terminal-test (state)
  (equal (list-length (get-whites (get-state-board state))) 0))
      

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
    (when (and square (is-empty (nth square (get-state-board state))))
      (push (make-action n square 1 -1) actions))
    actions))

(defun white-pawn-eat (n state direction diagonal)
  "Returns the list of pawn eat action"
  (let ((actions nil) (square (get-diagonal n direction diagonal)))
    (when square
      (let ((ssquare (get-diagonal square direction diagonal)))
        (when ssquare
          (when (and (is-empty (nth ssquare (get-state-board state)))
                     (is-black (nth square (get-state-board state))))
            (push (make-action n ssquare 0 square) actions)))))
    actions))

(defun white-king-move (n state direction diagonal)
  "Returns the list of king move action"
  (let ((actions nil))
    (labels ((recursive-move (position)
               (let ((square (get-diagonal position direction diagonal)))
                 (when (and square (is-empty (nth square (get-state-board state))))
                   (push (make-action n square 1 -1) actions)
                   (recursive-move square)))))
      (recursive-move n))
    actions))

(defun white-king-eat (n state direction diagonal)
  "Returns the list of king move action"
  (let ((actions nil))
    (labels ((recursive-eat (position)
               (let ((square (get-diagonal position direction diagonal)))
                 (cond
                   ((and square (is-black (nth square (get-state-board state))))
                    square)
                   ((and square (is-empty (nth square (get-state-board state))))
                    (recursive-eat square))
                   (t
                    nil))))
             (recursive-move (position initial)
               (let ((ssquare (get-diagonal position direction diagonal)))
                 (when (and ssquare (is-empty (nth ssquare (get-state-board state))))
                   (push (make-action n ssquare 0 initial) actions)
                   (recursive-move ssquare initial)))))
      (let ((square (recursive-eat n)))
        (when square
          (recursive-move square square)
          actions)))))

; black actions

(defun black-pawn-move (n state direction diagonal)
  "Returns the list of pawn move action"
  (let ((actions nil) (square (get-diagonal n direction diagonal)))
    (when (and square (is-empty (nth square (get-state-board state))))
      (push (make-action n square 0 -1) actions))
    actions))

(defun black-pawn-eat (n state direction diagonal)
  "Returns the list of pawn eat action"
  (let ((actions nil) (square (get-diagonal n direction diagonal)))
    (when square
      (let ((ssquare (get-diagonal square direction diagonal)))
        (when ssquare
          (when (and (is-empty (nth ssquare (get-state-board state)))
                     (is-white (nth square (get-state-board state))))
            (push (make-action n ssquare 1 square) actions)))))
    actions))

(defun black-king-move (n state direction diagonal)
  "Returns the list of king move action"
  (let ((actions nil))
    (labels ((recursive-move (position)
               (let ((square (get-diagonal position direction diagonal)))
                 (when (and square (is-empty (nth square (get-state-board state))))
                   (push (make-action n square 0 -1) actions)
                   (recursive-move square)))))
      (recursive-move n))
    actions))

(defun black-king-eat (n state direction diagonal)
  "Returns the list of king move action"
  (let ((actions nil))
    (labels ((recursive-eat (position)
               (let ((square (get-diagonal position direction diagonal)))
                 (cond
                   ((and square (is-white (nth square (get-state-board state))))
                    square)
                   ((and square (is-empty (nth square (get-state-board state))))
                    (recursive-eat square))
                   (t
                    nil))))
             (recursive-move (position initial)
               (let ((ssquare (get-diagonal position direction diagonal)))
                 (when (and ssquare (is-empty (nth ssquare (get-state-board state))))
                   (push (make-action n ssquare 1 initial) actions)
                   (recursive-move ssquare initial)))))
      (let ((square (recursive-eat n)))
        (when square
          (recursive-move square square)
          actions)))))

(defun get-piece-actions (n state)
  "Returns the list of actions from a single piece"
  ; if it is not on the (get-state-board state) we exit
  (when (= n -1) (return-from get-piece-actions nil))
  ; if we have to eat again with the same piece, but this piece is not selected
  ; we exit
  (when (and (not (= (get-state-eating state) -1)) (not (= n (get-state-eating state))))
    (return-from get-piece-actions nil))
  (let ((actions nil))
    ; white king movements
    (when (is-white-king (nth n (get-state-board state)))
      (setf actions (append actions (white-king-actions n state))))

    ; white pawn movements
    (when (is-white-pawn (nth n (get-state-board state)))
      (setf actions (append actions (white-pawn-actions n state))))

    ; black king movements
    (when (is-black-king (nth n (get-state-board state)))
      (setf actions (append actions (black-king-actions n state))))

    ; black pawn movements
    (when (is-black-pawn (nth n (get-state-board state)))
      (setf actions (append actions (black-pawn-actions n state))))

    ; (print n)
    ; (print actions)
    actions))

(defun switch-player (player) (mod (+ player 1) 2))

(defun to-move (state)
  "Returns whos turn it is to play"
  (get-state-player state))

(defun select-from (from actions)
  "Selects only actions that start at from"
  (remove-if-not #'(lambda (action)
                     (= (get-action-from action) from)) actions))

(defun select-to (to actions)
  "Selects only actions that end at to"
  (remove-if-not #'(lambda (action)
                     (= (get-action-to action) to)) actions))

(defun select-eating (actions)
  "Select only actions that do eat"
  (remove-if-not #'(lambda (action)
                     (not (= (get-action-eaten action) -1))) actions))

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

(defun compute-diagonals ()
  "Computes the diagonals movement from all squares to all other squares in a
  list"
  (dotimes (n +nb-squares+)
    (push (compute-white-left-diagonal n) *white-left-diagonals*)
    (push (compute-white-right-diagonal n) *white-right-diagonals*)
    (push (compute-black-left-diagonal n) *black-left-diagonals*)
    (push (compute-black-right-diagonal n) *black-right-diagonals*)))