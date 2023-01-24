(defpackage #:checkers-ai
  (:use :cl))

(in-package #:checkers-ai)

; dependencies
(ql:quickload "sdl2")
(require :sdl2)

(defun clear (renderer)
  "Clears the renderer to a black screen"
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun display-checker (renderer)
  "Display the checker plate to the renderer"
  ; here we first do a loop to iterate over the i and j
  ; we use this loop to collect the values of the rects
  ; into two lists, black and white
  ; once we are done we will display the two lists one 
  ; after the other

  (let* ((blacks nil) (whites nil))
    (dotimes (i 8)
      (dotimes (j 8)
        (let ((rect (sdl2:make-rect (* i 40) (* j 40) 40 40)))
          (if (or (and (evenp i) (oddp j))
                  (and (oddp i) (evenp j)))
            (push rect blacks)
            (push rect whites)))))

    (sdl2:set-render-draw-color renderer 255 0 0 255)
    (sdl2:render-fill-rects renderer (sdl2:make-rect 0 0 40 40))
    ; (sdl2:render-fill-rects renderer blacks 1)
    (sdl2:set-render-draw-color renderer 0 255 0 255)
    ; (sdl2:render-fill-rects renderer whites 1))
)

; initializes sdl2
(sdl2:with-init (:everything)

; prints sdl2 version to make sure everything is working
(format t "Using SDL Library Version: ~D.~D.~D~%"
        sdl2-ffi:+sdl-major-version+
        sdl2-ffi:+sdl-minor-version+
        sdl2-ffi:+sdl-patchlevel+)

(finish-output)

  ; creates a new window
  (sdl2:with-window (win :title "Checkers AI" :flags '(:shown))
    ; creates the renderer to display on the window
    (sdl2:with-renderer (renderer win :flags '(:accelerated))
      ; polls events
      (sdl2:with-event-loop (:method :poll)
        (:keyup (:keysym keysym)
          ; when exit event is sent to the program
          (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
            (sdl2:push-event :quit)))
        (:idle

        ; clear the renderer
        (clear renderer)

        ; print checker board to the renderer
        (display-checker renderer)

        ; print text to the renderer

        ; print images to the renderer

        ; present the renderer to the window
        (sdl2:render-present renderer)
        ; TODO adapt waiting time to provide 60fps
        (sdl2:delay 16))
      (:quit () t)))))
