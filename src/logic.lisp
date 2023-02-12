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

; TODO we want to add selectors - in particular - we want to select the pieces
; that are king or pawn and we want to select pieces that are black or white
; this function is a bit too broad - it is useful only for drawing

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

(defun get-piece-actions (n state)
  "Returns the list of actions from a single piece (:fr :to :pl)"
  ; if it is not on the (get-state-board state) we exit
  (when (= n -1) (return-from get-piece-actions nil))
  ; if we have to eat again with the same piece, but this piece is not selected
  ; we exit
  (when (and (not (= (get-state-eating state) -1)) (not (= n (get-state-eating state))))
    (return-from get-piece-actions nil))
  (let ((actions nil))
    (when (is-white-pawn (nth n (get-state-board state)))
        (let ((left (get-diagonal n 0 -1)) (right (get-diagonal n 0 1)))
          (when left
            (if (is-empty (nth left (get-state-board state)))
                (push (make-action n left 1 -1) actions)))
          (when right
            (if (is-empty (nth right (get-state-board state)))
                (push (make-action n right 1 -1) actions)))))
    (when (is-black-pawn (nth n (get-state-board state)))
      (let ((left (get-diagonal n 1 -1)) (right (get-diagonal n 1 1)))
          (when left
            (if (is-empty (nth left (get-state-board state)))
                (push (make-action n left 0 -1) actions)))
          (when right
            (if (is-empty (nth right (get-state-board state)))
                (push (make-action n right 0 -1) actions)))))
   actions))

(defun switch-player (player) (mod (+ player 1) 2))

(defun get-actions (state)
  "Returns the list of actions"
  ; if a piece must eat then we return its actions
  (let ((actions nil))
    (if (not (= (get-state-eating state) -1))
        (setf actions (append actions (get-piece-actions
                                        (get-state-eating state)
                                        state)))
        ; we want to get all the pieces of the player playing and append their
        ; actions to the action list
        (if (= (get-state-player state) 0)
            (dolist (white-pawn (get-white-pawns (get-state-board state)))
              (setf actions (append actions (get-piece-actions
                                              (getf white-pawn :n)
                                              state))))
            (dolist (black-pawn (get-black-pawns (get-state-board state)))
              (setf actions (append actions (get-piece-actions
                                              (getf black-pawn :n)
                                              state))))))

    ; if there are no actions then we add an artifical empty one
    (when (null actions) (push (make-action -1 -1 (switch-player (get-state-player state)) -1) actions))
    actions))

(defun select-from (from actions)
  "Selects only actions that start at from"
  (remove-if-not #'(lambda (action)
                     (= (get-action-from action) from)) actions))

(defun select-to (to actions)
  "Selects only actions that end at to"
  (remove-if-not #'(lambda (action)
                     (= (get-action-to action) to)) actions))

(defun move (action state)
  "Changes the state from an action"
  (format t "action : ~a~%" action)
  (format t "state  : ~a~%" state)
  ; TODO find a way to do this access cleanly
  (let ((board (nth 0 state)) (player (nth 1 state)) (eating (nth 2 state)))
    (setf (nth (get-action-to action) (nth 0 state))
          (nth (get-action-from action) (nth 0 state)))
    (setf (nth (get-action-from action) (nth 0 state))
          0)
    (when (and (is-white-pawn (nth (get-action-to action) (nth 0 state)))
               (< (get-action-to action) +grid-size+))
      (setf (nth (get-action-to action) (nth 0 state))
            3))
    (when (and (is-black-pawn (nth (get-action-to action) (nth 0 state)))
               (>= (get-action-to action) (- +nb-squares+ +grid-size+)))
      (setf (nth (get-action-to action) (nth 0 state))
            4))
    (setf (nth 1 state) (get-action-player action))
    (setf (nth 2 state) (get-action-eating action))
  ))
