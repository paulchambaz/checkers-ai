(in-package :checkers-ai)

(defun main ()
  ; intializes sdl2
  (sdl2:with-init (:everything)
    (finish-output)
    (sdl2:with-window (win :title "Checkers AI" :w +width+ :h +height+ :flags '(:shown))
      ; creates the renderer to display with the window
      (sdl2:with-renderer (renderer win :flags '(:accelerated))

        ; initializes all variable that the program uses
        (load-textures renderer)

        (format t "Welcome to the checkers-ai program, please select a difficulty mode:~%")

        (let ((board (init-board)))

        ; polls events
          (sdl2:with-event-loop (:method :poll)
            (:keyup (:keysym keysym)
              ; when exit event is sent to the program
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (sdl2:push-event :quit)))
            (:idle
              ()
                ; store mouse information
                (multiple-value-bind (mouse-x mouse-y) (sdl2:mouse-state)

                  ; time to draw
                  (clear renderer)
                  (draw-checker renderer)
                  (apply #'draw-hint `(,@(get-pos mouse-x mouse-y) ,renderer))
                  (draw-pieces board renderer)
                  (sdl2:render-present renderer)
             (sdl2:delay 16)))
            (:quit () t)))))))
