(in-package :checkers-ai)

(defun main ()
  (compute-diagonals)
  ; intializes sdl2
  (sdl2:with-init (:everything)
    (finish-output)
    (sdl2:with-window (win :title "Checkers AI" :w +width+ :h +height+ :flags '(:shown))
      ; creates the renderer to display with the window
      (sdl2:with-renderer (renderer win :flags '(:accelerated))

        ; initializes all variable that the program uses
        (load-textures renderer)

        (format t "Welcome to the checkers-ai program, please select a difficulty mode:~%")

        ; (let ((board (init-board)) (player 0) (eating 42))
        (let ((state (make-state (init-board) 0 -1)) (click-state 0) (selected -1) (actions-from nil) (actions-to nil))

        ; polls events
          (sdl2:with-event-loop (:method :poll)
            (:keyup (:keysym keysym)
              ; when exit event is sent to the program
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (sdl2:push-event :quit)))
            (:idle
              ()
                ; store mouse information
             (multiple-value-bind (mouse-x mouse-y mouse-bitmask) (sdl2:mouse-state)

               (setf *mouse* (= (mod mouse-bitmask 2) 1))

               (let ((actions (get-actions state)))

                 (when (mouse-pressed)
                   (setf selected (apply #'get-square `(,@(get-pos mouse-x mouse-y))))
                   (if (= click-state 0)
                     (when (not (= selected -1))
                       (setf actions-from (select-from selected actions))
                         (when (not (null actions-from))
                           (setf click-state 1)))
                     (when (not (= selected -1))
                       (setf actions-to (select-to selected actions-from))
                       (when (not (null actions-to))
                         (move (nth 0 actions-to) state)
                         (format t "~a~%" state)
                         (setf actions-from nil)
                         (setf actions-to nil)
                         (setf click-state 0)))))

               ; time to draw
               (clear renderer)
               (draw-checker renderer)
               (when (= click-state 0) (hint-actions-from actions renderer))
               (when (= click-state 1) (hint-actions-to (select-from selected actions) renderer))

               (draw-pieces (get-state-board state) renderer)
               (sdl2:render-present renderer)

               (setf *prev-mouse* *mouse*)

               (sdl2:delay 16))))
            (:quit () t)))))))
