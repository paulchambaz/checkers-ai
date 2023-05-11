(in-package :checkers-ai)

(defvar *textures* (make-list 4))
(defvar *mouse* nil)
(defvar *mouse-x* -1)
(defvar *mouse-y* -1)
(defvar *prev-mouse* nil)

(defun clear (renderer)
  "Clears the renderer to a blank screen"
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun get-rect (x y)
  "Gets the rect at coordinate x y of the checker board"
  (sdl2:make-rect (round (+ (* x +size+) (+ +padding+ +border+)))
        (round (+ (* y +size+) (+ +padding+ +border+)))
        (round +size+)
        (round +size+)))

(defun get-pos-dim (p l)
  "Return the checker space axis position of the screen space position"
  (if (or (< p (+ +padding+ +border+)) (>= p (- l (+ +padding+ +border+))))
      -1
      (floor (* +grid-size+ (/ (- p (+ +padding+ +border+))
                               (- l (* 2 (+ +padding+ +border+))))))))

(defun get-pos (mouse-x mouse-y)
  "Return the checker space position of the screen space positition"
  (list (get-pos-dim mouse-x +width+) (get-pos-dim mouse-y +height+)))

(defun draw-checker (renderer)
  "Displays a checker board to the renderer"
  ; first we print the background of the checker board
  (sdl2:set-render-draw-color renderer 25 25 25 255)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 0 0 +width+ +height+))
  (sdl2:set-render-draw-color renderer 235 235 235 255)
  (sdl2:render-fill-rect renderer (sdl2:make-rect +padding+ +padding+ (- +width+ (* 2 +padding+)) (- +height+ (* 2 +padding+))))
  ; then we create the list of rects to draw
  (let* ((blacks nil) (whites nil))
    (dotimes (x +grid-size+)
      (dotimes (y +grid-size+)
        (let ((rect (get-rect x y)))
          (if (or (and (evenp x) (oddp y))
                  (and (oddp x) (evenp y)))
              (push rect whites)
              (push rect blacks)))))
    ; finally we draw the rects in the correct color
    (sdl2:set-render-draw-color renderer 235 235 235 255)
    (dolist (white whites)
      (sdl2:render-fill-rect renderer white))
    (sdl2:set-render-draw-color renderer 25 25 25 255)
    (dolist (black blacks)
      (sdl2:render-fill-rect renderer black))))

(defun draw-hint (n renderer)
  "Draws a hint over the square n of the checkers board"
  (when (= n -1) (return-from draw-hint nil))
  (let ((x (get-col n))
        (y (get-line n)))
    ; changes color for different squares
    (if (or (and (evenp x) (oddp y))
            (and (oddp x) (evenp y)))
        (sdl2:set-render-draw-color renderer 180 210 230 255)
        (sdl2:set-render-draw-color renderer 50 80 100 255))
    ; finally draw the rect to the screen
    (sdl2:render-fill-rect renderer (get-rect x y))))

(defun load-texture (renderer path)
  "Loads the texture from a path to create a texture"
  (sdl2:create-texture-from-surface renderer (sdl2:load-bmp path)))

(defun load-textures (renderer)
  "Loads the textures of the pieces to the renderer"
  (setf (nth 0 *textures*) (load-texture renderer "data/w_pawn.bmp"))
  (setf (nth 1 *textures*) (load-texture renderer "data/b_pawn.bmp"))
  (setf (nth 2 *textures*) (load-texture renderer "data/w_king.bmp"))
  (setf (nth 3 *textures*) (load-texture renderer "data/b_king.bmp")))

(defun get-texture (id)
  "Get a texture from its id"
  (nth id *textures*))

(defun draw-pieces (board renderer)
  "Display the pieces to the screen"
  (let ((pieces (get-pieces board)))
    (dolist (square pieces)
      (let ((x (get-col (getf square :n)))
            (y (get-line (getf square :n))))
      (sdl2:render-copy renderer (get-texture (- (getf square :id) 1)) :dest-rect (get-rect x y))))))

(defun hint-whites (board renderer)
  "Hints all whites"
  (dolist (square (get-whites board))
    (draw-hint (getf square :n) renderer)))

(defun hint-white-pawns (board renderer)
  "Hints all white pawns"
  (dolist (square (get-white-pawns board))
    (draw-hint (getf square :n) renderer)))

(defun hint-white-kings (board renderer)
  "Hints all white kings"
  (dolist (square (get-white-kings board))
    (draw-hint (getf square :n) renderer)))

(defun hint-blacks (board renderer)
  "Hints all blacks"
  (dolist (square (get-blacks board))
    (draw-hint (getf square :n) renderer)))

(defun hint-black-pawns (board renderer)
  "Hints all black pawns"
  (dolist (square (get-black-pawns board))
    (draw-hint (getf square :n) renderer)))

(defun hint-black-kings (board renderer)
  "Hints all black kings"
  (dolist (square (get-black-kings board))
    (draw-hint (getf square :n) renderer)))

(defun hint-actions (actions renderer)
  "Hints all member of an action list"
  (dolist (action actions)
    (draw-hint (action-to action) renderer)))

(defun hint-actions-from (actions renderer)
  "Hints all member of an action list"
  (dolist (action actions)
    (draw-hint (action-from action) renderer)))

(defun hint-actions-to (actions renderer)
  "Hints all member of an action list"
  (dolist (action actions)
    (draw-hint (action-to action) renderer)))

(defun mouse-pressed ()
  "Returns if the mouse has been pressed"
  (and (not (null *mouse*)) (null *prev-mouse*)))
