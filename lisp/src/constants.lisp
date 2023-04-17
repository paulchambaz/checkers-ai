(in-package :checkers-ai)

; pixel width of the window
(defconstant +width+ 720)
; pixel height of the window
(defconstant +height+ 720)

; pixel padding size around the board
(defconstant +padding+ 30)
; pixel border size around the board
(defconstant +border+ 5)

; size of the grid, here we use a 8x8 grid
(defconstant +grid-size+ 8)
(defconstant +nb-squares+ (* +grid-size+ +grid-size+))

; pixel size of an individual square off board
(defconstant +size+ (/ (coerce (- +width+ (* 2 (+ +border+ +padding+))) 'float) +grid-size+))

; value of the players
(defconstant +white+ 0)
(defconstant +black+ 1)

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
(defun is-white (value) (or (is-white-pawn value) (is-white-king value)))
(defun is-black (value) (or (is-black-pawn value) (is-black-king value)))
(defun is-pawn (value) (or (is-white-pawn value) (is-black-pawn value)))
(defun is-king (value) (or (is-white-king value) (is-black-king value)))

; function for evaluating the board
(defun get-line (n) (floor (/ (coerce n 'float) +grid-size+)))
(defun get-col (n) (floor (mod (coerce n 'float) +grid-size+)))
(defun get-square (x y)
  (when (or (= x -1) (= y -1)) (return-from get-square -1))
  (+ (* y +grid-size+) x))

; search constants
(defconstant +search-depth+ 2)
(defconstant +search-time+ 0.5)
; (defconstant +search-time+ 0.5)

; traning constant
(defconstant gen-size 100)
(defconstant set-number 20)
(defconstant dna-size 20)
(defconstant initial-variation 1.0)
(defconstant generation-variation 0.1)
