(in-package :checkers-ai)

(defun player-turn (state actions p-click-state p-selected)
  "Player turn"
  (when (= (action-from (nth 0 actions)) -1)
    (setf (nth 1 state) (switch-player (nth 1 state)))
    (setf (nth 2 state) -1))

  (if (mouse-pressed)
      ; first we get the value of the square pressed
      (progn (setf selected (apply #'get-square
                                   `(,@(get-pos *mouse-x* *mouse-y*))))

             ; if the square is valid
             (if (not (= selected -1))
                 (if (= p-click-state 0)
                     ; if we are at the first part - piece selection
                     (progn (setf actions-from (select-from selected actions))
                            (if (not (null actions-from))
                                (values 1 selected state)
                                (values 0 selected state)))
                     ; if we are at the second part - move selection
                     (progn (setf actions-to (select-to selected actions-from))
                            ; if we are moving to a valid move
                            (if (null actions-to)
                                ; if the move is invalid we reset the turn
                                (progn (setf actions-from nil)
                                       (values 0 selected state))
                                ; if the move is valid we change the state
                                (progn (setf state (result (nth 0 actions-to) state))
                                       (format t "~a~%" state)
                                       ; and reset variables
                                       (setf actions-from nil)
                                       (setf actions-to nil)
                                       (values 0 selected state)))))
                 (values p-click-state p-selected state)))
      (values p-click-state p-selected state)))

(defun ia-turn (state ai)
  "Ia turn"
  (result (ai-search state +search-depth+ ai) state))

(defun end (state actions)
  (when (and (white-terminal-test state))
    (format t "black won no pieces available~%")
    (sdl2:push-event :quit)
    (return-from end T)
    )

  (when (and (black-terminal-test state))
    (format t "white won no pieces available~%")
    (sdl2:push-event :quit)
    (return-from end T)
    )

  (when (and (not actions)
             (equal (state-player state) +white+))
    (format t "black won no moves available~%")
    (sdl2:push-event :quit)
    (return-from end T)
    )

  (when (and (not actions)
             (equal (state-player state) +black+))
    (format t "white won no moves available~%")
    (sdl2:push-event :quit)
    (return-from end T)
    )
)


(defun main ()
  (setf *random-state* (make-random-state t))
  (compute-diagonals)
  (dotimes (i 2)
    (setf *gen* (init-gen 2)))
    ; (match (get-ai 0 *gen*) (get-ai 1 *gen*)))
  ; intializes sdl2
  (sdl2:with-init (:everything)
    (finish-output)
    (sdl2:with-window (win :title "Checkers AI" :w +width+ :h +height+ :flags '(:shown))
      ; creates the renderer to display with the window
      (sdl2:with-renderer (renderer win :flags '(:accelerated))

        ; initializes all variable that the program uses
        (load-textures renderer)

        (format t "Welcome to the checkers-ai program, please select a difficulty mode:~%")

        (let ((state (make-state :board (init-board) :player +white+ :eating -1)) (click-state 0) (selected -1) (actions-from nil) (actions-to nil))

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
               (setf *mouse-x* mouse-x)
               (setf *mouse-y* mouse-y)

               (let ((actions (actions state)))

                 (when (not (end state actions))

                   (clear renderer)
                   (draw-checker renderer)

                   (when (= click-state 0)
                     (hint-actions-from actions renderer))

                   (when (= click-state 1)
                     (hint-actions-to (select-from selected actions) renderer))

                   (draw-pieces (state-board state) renderer)
                   (sdl2:render-present renderer)

                   (if (= (state-player state) 0)

                     ; turn of the player
                     (progn 
                       ; (setf state (ia-turn state (get-ai 1 *gen*))))
                       (multiple-value-bind (t-click-state t-selected t-state) (player-turn state actions click-state selected)
                         (setf click-state t-click-state)
                         (setf selected t-selected)
                         (setf state t-state)))

                     ; turn of the ia
                     (progn
                       ; (setf state (ia-turn state (get-ai 0 *gen*))))
                       (multiple-value-bind (t-click-state t-selected t-state) (player-turn state actions click-state selected)
                         (setf click-state t-click-state)
                         (setf selected t-selected)
                         (setf state t-state)))
                   )


                   (setf actions (actions state))

                   ; time to draw

                   (clear renderer)
                   (draw-checker renderer)

                   (when (= click-state 0)
                     (hint-actions-from actions renderer))

                   (when (= click-state 1)
                     (hint-actions-to (select-from selected actions) renderer))

                   (draw-pieces (state-board state) renderer)
                   (sdl2:render-present renderer)

                   (setf *prev-mouse* *mouse*)

                   (sdl2:delay 16)))))
            (:quit () t)))))))

