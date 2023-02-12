(in-package :checkers-ai)

; pixel width of the window
(defconstant +width+ 600)
; pixel height of the window
(defconstant +height+ 600)

; pixel padding size around the board
(defconstant +padding+ 30)
; pixel border size around the board
(defconstant +border+ 5)

; size of the grid, here we use a 8x8 grid
(defconstant +grid-size+ 8)
(defconstant +nb-squares+ (* +grid-size+ +grid-size+))

; pixel size of an individual square off board
(defconstant +size+ (/ (coerce (- +width+ (* 2 (+ +border+ +padding+))) 'float) +grid-size+))

; value of the squares
(defconstant +white-pawn+ 1)
(defconstant +black-pawn+ 2)
(defconstant +white-king+ 3)
(defconstant +black-king+ 4)

; function for identity checking
(defun is-empty (value) (= value 0))
(defun is-white-pawn (value) (= value +white-pawn+))
(defun is-black-pawn (value) (= value +black-pawn+))
(defun is-white-king (value) (= value +white-king+))
(defun is-black-king (value) (= value +black-king+))
(defun is-pawn (value) (or (is-white-pawn value) (is-black-pawn value)))
(defun is-king (value) (or (is-white-king value) (is-black-king value)))

; function for evaluating the board
(defun get-line (n) (floor (/ (coerce n 'float) +grid-size+)))
(defun get-col (n) (floor (mod (coerce n 'float) +grid-size+)))
(defun get-square (x y)
  (when (or (= x -1) (= y -1)) (return-from get-square -1))
  (+ (* y +grid-size+) x))
