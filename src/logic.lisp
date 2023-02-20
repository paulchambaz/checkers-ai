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

; TODO we can move and then move even if we cant eat - need to select that there
; are still eating moves - that sounds easy to state

(defun init-board ()
  "Creates a new board and places the pieces"
  ; TODO half the squares are useless so we will need to optimize it
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

; TODO the kings should be dealt with last so we start the exploration with the
; strongest one first

(defun white-left-actions (n state)
  "Returns the list of action for a white pawn to the left"
  (let ((actions nil) (left (get-diagonal n 0 -1)))
    (when left
      (if (is-empty (nth left (get-state-board state)))
          ; add a simple movement to the left
          (push (make-action n left 1 -1) actions)
          (let ((lleft (get-diagonal left 0 -1)))
            (when lleft
              ; we check that we could eat
              (when (and (is-empty (nth lleft (get-state-board state)))
                         (is-black (nth left (get-state-board state))))
                (push (make-action n lleft 0 lleft) actions))))))
    actions))

(defun white-right-actions (n state)
  "Returns the list of action for a white pawn to the right"
  (let ((actions nil) (right (get-diagonal n 0 1)))
    (when right
      (if (is-empty (nth right (get-state-board state)))
          ; add a simple movement to the right
          (push (make-action n right 1 -1) actions)
          (let ((rright (get-diagonal right 0 1)))
            (when rright
              ; we check that we could eat
              (when (and (is-empty (nth rright (get-state-board state)))
                         (is-black (nth right (get-state-board state))))
                (push (make-action n rright 0 rright) actions))))))
    actions))

(defun black-left-actions (n state)
  "Returns the list of action for a black pawn to the left"
  (let ((actions nil) (left (get-diagonal n 1 -1)))
    (when left
      (if (is-empty (nth left (get-state-board state)))
          ; add a simple movement to the left
          (push (make-action n left 0 -1) actions)
          (let ((lleft (get-diagonal left 1 -1)))
            (when lleft
              ; we check that we could eat
              (when (and (is-empty (nth lleft (get-state-board state)))
                         (is-white (nth left (get-state-board state))))
                (push (make-action n lleft 1 lleft) actions))))))
    actions))

(defun black-right-actions (n state)
  "Returns the list of action for a black pawn to the right"
  (let ((actions nil) (right (get-diagonal n 1 1)))
    (when right
      (if (is-empty (nth right (get-state-board state)))
          ; add a simple movement to the right
          (push (make-action n right 0 -1) actions)
          (let ((rright (get-diagonal right 1 1)))
            (when rright
              ; we check that we could eat
              (when (and (is-empty (nth rright (get-state-board state)))
                         (is-white (nth right (get-state-board state))))
                (push (make-action n rright 1 rright) actions))))))
    actions))

(defun get-piece-actions (n state)
  "Returns the list of actions from a single piece"
  ; if it is not on the (get-state-board state) we exit
  (when (= n -1) (return-from get-piece-actions nil))
  ; if we have to eat again with the same piece, but this piece is not selected
  ; we exit
  (when (and (not (= (get-state-eating state) -1)) (not (= n (get-state-eating state))))
    (return-from get-piece-actions nil))
  (let ((actions nil))
    ; white pawn movements
    (when (is-white-pawn (nth n (get-state-board state)))
      (setf actions (append actions (white-left-actions n state)))
      ; (format t "white left : ~a~%" actions)
      (setf actions (append actions (white-right-actions n state)))
      ; (format t "white right : ~a~%" actions)
    )
    ; black pawn movements
    (when (is-black-pawn (nth n (get-state-board state)))
      (setf actions (append actions (black-left-actions n state)))
      ; (format t "black left : ~a~%" actions)
      (setf actions (append actions (black-right-actions n state)))
      ; (format t "black right : ~a~%" actions)
    )
    actions))

(defun switch-player (player) (mod (+ player 1) 2))

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
        (dolist (white-pawn (get-white-pawns (get-state-board state)))
          (setf actions (append actions (get-piece-actions
                                          (getf white-pawn :n)
                                          state)))))

      (when (= (get-state-player state) 1)
        (dolist (black-pawn (get-black-pawns (get-state-board state)))
          (setf actions (append actions (get-piece-actions
                                          (getf black-pawn :n)
                                          state))))))
    ; force the player to only use eating actions if they are available
    ; (format t "~a~%" (select-eating actions))
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
    ; finally we return the action
    actions))

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
