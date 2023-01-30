; dependencies
(ql:quickload "sdl2")
(require :sdl2)

; TODO put constant in a seperate file for clarity

; display information
(defvar *width* 800)
(defvar *height* 800)
(defvar *padding* 30)
(defvar *border* 5)
(defvar *black* 25)
(defvar *white* 235)
(defvar *grid-size* 8)

; calculates the size of an square for the screen
(defvar *size* (/ (coerce (- *width* (* 2 (+ *border* *padding*))) 'float) *grid-size*))

; make constant for id for easier code readability
(defvar *black-pawn* 1)
(defvar *white-pawn* 2)
(defvar *black-king* 3)
(defvar *white-king* 4)

; textures information for the pieces - pseudo const - set at init of
; renderer then is made imutable
(defvar *textures* (make-list 4))

; initializes the board - in this game a board is a 8x8 matrix if
; integers where 0 is an empty square, 1 is a white pawn, 2 a black
; pawn, 3 a white king and 5 is a black king
(defun init-board ()
  "Creates a new board and places the pieces"
  ; first we create the actual board variable as a grid of size
  ; grid-size x grid-size
  (let ((board (make-array `(,*grid-size* ,*grid-size*) :initial-element 0)))
    ; we use a dotimes loop to iterate over each line until the last to
    ; middle line then we set the value of the board to our value
    (dotimes (x (/ *grid-size* 2))
      (dotimes (y (- (/ *grid-size* 2) 1))
        (setf (aref board (+ (* x 2) (mod (+ y 1) 2)) y) *black-pawn*)))

    ; we do the same for the white pawns
    (dotimes (x (/ *grid-size* 2))
      (dotimes (y (- (/ *grid-size* 2) 1))
        (setf (aref board (+ (* x 2) (mod y 2)) (- (- *grid-size* 1) y)) *white-pawn*)))
    board))

(defun get-pieces (board)
  "Creates a list of pieces on the board with format (x y id)"
  (let ((positions nil))
    (dotimes (x (array-dimension board 0))
      (dotimes (y (array-dimension board 1))
        ; we skip over empty squares
        (when (not (zerop (aref board x y)))
          (push (list :x x :y y :id (aref board x y)) positions))))
    positions))

(defun clear (renderer)
  "Clears the renderer to a black screen"
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun get-rect (x y)
  "Gets the rect at coordinate (x y) of the checker board"
  (sdl2:make-rect (round (+ (* x *size*) (+ *padding* *border*))) (round (+ (* y *size*) (+ *padding* *border*))) (round *size*) (round *size*)))

; (defun get-pos (mouse-x mouse-y)
;   (mouse-x mouse-y))

(defun draw-checker (renderer)
  "Display the checker plate to the renderer"
  ; here we first do a loop to iterate over the i and j we use this loop
  ; to collect the values of the rects into two lists, black and white
  ; once we are done we will display the two lists one after the other

  ; we print the background the squares behind the actual checker board
  (sdl2:set-render-draw-color renderer *black* *black* *black* 255)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 0 0 *width* *height*))
  (sdl2:set-render-draw-color renderer *white* *white* *white* 255)
  (sdl2:render-fill-rect renderer (sdl2:make-rect *padding* *padding* (- *width* (* 2 *padding*)) (- *height* (* 2 *padding*))))

  ; creates the list of rects of the checkers to draw to the screen
  ; simple iteration and mods are taken into account
  (let* ((blacks nil) (whites nil))
    (dotimes (x *grid-size*)
      (dotimes (y *grid-size*)
        (let ((rect (get-rect x y)))
          (if (or (and (evenp x) (oddp y))
                  (and (oddp x) (evenp y)))
            (push rect whites)
            (push rect blacks))))

      ; one we have the list of rects we can simply draw them to
      ; renderer all at once
      (sdl2:set-render-draw-color renderer *white* *white* *white* 255)
      (dolist (white whites)
        (sdl2:render-fill-rect renderer white))
      (sdl2:set-render-draw-color renderer *black* *black* *black* 255)
      (dolist (black blacks)
        (sdl2:render-fill-rect renderer black)))))

(defun draw-hint (x y renderer)
  (sdl2:set-render-draw-color renderer 60 90 180 50)
  (sdl2:render-fill-rect renderer (get-rect x y)))

(defun get-texture (id)
  "Get a texture from its id"
  (nth id *textures*))

(defun draw-pieces (board renderer)
  "Display the pieces to the screen"
  ; uses get-pieces function to extract the list of pieces from the
  ; board
    (let ((pieces (get-pieces board)))
      ; then iterate over that list and draws to screen all pieces
      (dolist (square pieces)
        (sdl2:render-copy renderer (get-texture (- (getf square :id) 1)) :dest-rect (get-rect (getf square :x) (getf square :y))))))

; (defun add-texture (texture)
;   "Adds a texture to the list of textures used"
;   (push texture *textures*))

(defun load-texture (renderer path)
  "Loads the texture from a path to create a texture"
  (sdl2:create-texture-from-surface renderer (sdl2:load-bmp path)))

(defun main ()
  "Main function of the checker ai program"
  ; intializes sdl2
  (sdl2:with-init (:everything)
    (finish-output)
    (sdl2:with-window (win :title "Checkers AI" :w *width* :h *height* :flags '(:shown))
      ; creates the renderer to display with the window
      (sdl2:with-renderer (renderer win :flags '(:accelerated))

      ; loads texture files for use
      (setf (nth 0 *textures*) (load-texture renderer "data/b_pawn.bmp"))
      (setf (nth 1 *textures*) (load-texture renderer "data/w_pawn.bmp"))
      (setf (nth 2 *textures*) (load-texture renderer "data/b_king.bmp"))
      (setf (nth 3 *textures*) (load-texture renderer "data/w_king.bmp"))

      ; intializes the board
      (let ((board (init-board)))

        ; polls events
          (sdl2:with-event-loop (:method :poll)
            (:keyup (:keysym keysym)
              ; when exit event is sent to the program
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (sdl2:push-event :quit)))
            (:idle
              ()
                (let ((mouse (sdl2:mouse-state)))

                  ; TODO i am working on implementing a the mouse hints,
                  ; i want to set the variable mouse to a list that
                  ; contains (:x x :y y)
                  ; then i want to write a function that converts these
                  ; coordinates to grid space coordinates
                  ; finally i want to fix the graphical bug on the
                  ; renderer and just use (draw-hint) i also want to
                  ; fix the opacity issue, or change the color depending
                  ; on the value of the grid or even odd even for that
                  ; matter
                  ; (length mouse)

                  ; (setf mouse (subseq mouse 1 3))

                  ; (format "x: ~a y: ~a~%" mouse)

                  ; drawing functions
                  (clear renderer)
                  (draw-checker renderer)

                  ; (format t "x: ~a y: ~a~%" (get-pos mouse-x mouse-y))

                  (draw-hint 0 1 renderer)

                  (draw-pieces board renderer)
                  (sdl2:render-present renderer)
                  (sdl2:delay 16)))
            (:quit () t)))))))
