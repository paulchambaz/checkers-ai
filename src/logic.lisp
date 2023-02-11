(in-package :checkers-ai)

(defun init-board ()
  "Creates a new board and places the pieces"
  ; TODO this may be better as a 64 element array rather than a 8x8 table
  ; intializes the board as a grid of size grid-size x grid-size
  (let ((board (make-array `(,+grid-size+ ,+grid-size+) :initial-element 0)))
    ; places the black pawn at correct position
    (dotimes (x (/ +grid-size+ 2))
      (dotimes (y (- (/ +grid-size+ 2) 1))
        (setf (aref board (+ (* x 2) (mod (+ y 1) 2)) y) +black-pawn+)))
    ; places the white pawn at correct position
    (dotimes (x (/ +grid-size+ 2))
      (dotimes (y (- (/ +grid-size+ 2) 1))
        (setf (aref board (+ (* x 2) (mod y 2)) (- (- +grid-size+ 1) y)) +white-pawn+)))
    ; finally we return the board
    board))

(defun get-pieces (board)
  "Creates a list of pieces on the board with format (:x x :y  y :id id)"
  (let ((positions nil))
    ; TODO this would be better as just a list
    (dotimes (x (array-dimension board 0))
      (dotimes (y (array-dimension board 1))
        ; we skip over empty squares
        ; TODO i would much rather use is-empty
        (when (not (zerop (aref board x y)))
          (push (list :x x :y y :id (aref board x y)) positions))))
    ; fianlly we return the positions
    positions))
