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

; pixel size of an individual square off board
(defconstant +size+ (/ (coerce (- +width+ (* 2 (+ +border+ +padding+))) 'float) +grid-size+))

; value of the squares
(defconstant +black-pawn+ 1)
(defconstant +white-pawn+ 2)
(defconstant +black-king+ 3)
(defconstant +white-king+ 4)

; macro for identity checking
(defmacro is-empty (value) (= value 0))
(defmacro is-black-pawn (value) (= value +black-pawn+))
(defmacro is-white-pawn (value) (= value +white-pawn+))
(defmacro is-black-king (value) (= value +black-king+))
(defmacro is-white-king (value) (= value +white-king+))

