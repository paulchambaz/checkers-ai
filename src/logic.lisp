(in-package :checkers-ai)

(defvar *white-left-diagonals* nil)
(defvar *white-right-diagonals* nil)
(defvar *black-left-diagonals* nil)
(defvar *black-right-diagonals* nil)

; defining the action
(defun make-action (from to player eating) (list from to player eating))
(defun get-action-from (action) (nth 0 action))
(defun get-action-to (action) (nth 1 action))
(defun get-action-player (action) (nth 2 action))
(defun get-action-eating (action) (nth 3 action))

; defining a state
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

(defun actions (state)
  "Returns the list of actions"
  ; if a piece must eat then we return its actions
  (let ((actions nil))
    (when (not (= (get-state-eating state) -1))
        (setf actions (append actions (get-piece-actions
                                        (get-state-eating state)
                                        state))))
    (when (= (get-state-eating state) -1)

      ; we want to get all the pieces of the player playing and append their
      ; actions to the action list
      (when (= (get-state-player state) 0)
        (dolist (white-pawn 
                  (append (get-white-pawns (get-state-board state))
                          (get-white-kings (get-state-board state))))
          (setf actions (append actions (get-piece-actions
                                          (getf white-pawn :n)
                                          state)))))

      (when (= (get-state-player state) 1)
        (dolist (black-pawn 
                  (append (get-black-pawns (get-state-board state))
                          (get-black-kings (get-state-board state))))
          (setf actions (append actions (get-piece-actions
                                          (getf black-pawn :n)
                                          state))))))

    ; force the player to only use eating actions if they are available
    (let ((eating-actions (select-eating actions)))
      (when eating-actions (setf actions eating-actions))

      ; if there are no actions available then we create one that simply
      ; switches the player
      ; if we need to eat and there are no eating actions then we want to setf
      ; actions to 
      (when (or (null actions)
                (and (not (= (get-state-eating state) -1))
                     (null eating-actions)))
        (setf actions (list (make-action -1 -1
                                         (switch-player (get-state-player state))
                                         -1)))))
    ; (print "end of board")
    ; finally we return the action
    actions))

(defun result (action state)
  "Changes the state from an action"
  (when (not (= (get-action-to action) -1))
    ; TODO find a way to do this access cleanly
    ; simple movement
    (let ((board (nth 0 state)) (player (nth 1 state)) (eating (nth 2 state)))
      (setf (nth (get-action-to action) (nth 0 state))
            (nth (get-action-from action) (nth 0 state)))
      (setf (nth (get-action-from action) (nth 0 state)) 0)
      ; white pawn to white king
      (when (and (is-white-pawn (nth (get-action-to action) (nth 0 state)))
                 (< (get-action-to action) +grid-size+))
        (setf (nth (get-action-to action) (nth 0 state)) 3))
      ; black pawn to black king
      (when (and (is-black-pawn (nth (get-action-to action) (nth 0 state)))
                 (>= (get-action-to action) (- +nb-squares+ +grid-size+)))
        (setf (nth (get-action-to action) (nth 0 state)) 4))
      ; eating
      (when (not (= (get-action-eating action) -1))
        (setf (nth (/ (+ (get-action-from action) (get-action-to action)) 2) (nth 0 state)) 0))
    ))
  ; updating player
  (setf (nth 1 state) (get-action-player action))
  ; updating eating
  (setf (nth 2 state) (get-action-eating action))
  state)

; TODO implement this as it is obviously better
(defun undo (action state)
  "Invert the change of state from an action"
  state)

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
            (push (make-action n ssquare 0 ssquare) actions)))))
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
                 (when square
                   (when (is-empty (nth square (get-state-board state)))
                     (recursive-eat square))
                   (when (and (is-black (nth square (get-state-board state))))
                         square)))))
      (let ((square (recursive-eat n)))
        (when square
          (labels ((recursive-move (position)
                     (let ((ssquare (get-diagonal position direction diagonal)))
                       (when (and ssquare (is-empty (nth ssquare (get-state-board state))))
                         (push (make-action n ssquare 0 ssquare) actions)
                         (recursive-move ssquare)))))
            (recursive-move square))
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
            (push (make-action n ssquare 1 ssquare) actions)))))
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
                 (when square
                   (when (is-empty (nth square (get-state-board state)))
                     (recursive-eat square))
                   (when (and (is-white (nth square (get-state-board state))))
                         square)))))
      (let ((square (recursive-eat n)))
        (when square
          (labels ((recursive-move (position)
                     (let ((ssquare (get-diagonal position direction diagonal)))
                       (when (and ssquare (is-empty (nth ssquare (get-state-board state))))
                         (push (make-action n ssquare 1 ssquare) actions)
                         (recursive-move ssquare)))))
            (recursive-move square))
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
  "Select only actions that eat"
  (remove-if-not #'(lambda (action)
                     (not (= (get-action-eating action) -1))) actions))

(defun select-not-eating (actions)
  "Select only actions that eat"
  (remove-if-not #'(lambda (action)
                     (= (get-action-eating action) -1))) actions)

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
      (when (or (is-black-pawn (nth n board)) (is-white-king (nth n board)))
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
