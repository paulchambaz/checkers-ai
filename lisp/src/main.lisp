(in-package :checkers-ai)

(defstruct player-turn-res click-state selected state)

(defun player-turn (state actions p-click-state p-selected)
  "Player turn"
  ; this is officially a cringe status moment!
  (when (= (action-from (nth 0 actions)) -1)
    (setf (state-player state) (switch-player (state-player state)))
    (setf (state-eating state) -1))

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
                              (make-player-turn-res :click-state 1 :selected selected :state state)
                              (make-player-turn-res :click-state 0 :selected selected :state state)))
                                ; (values 1 selected state)
                                ; (values 0 selected state)))
                     ; if we are at the second part - move selection
                     (progn (setf actions-to (select-to selected actions-from))
                            ; if we are moving to a valid move
                            (if (null actions-to)
                                ; if the move is invalid we reset the turn
                                (progn (setf actions-from nil)
                                       (make-player-turn-res :click-state 0 :selected selected :state state))
                                       ; (values 0 selected state))
                                ; if the move is valid we change the state
                                (progn (setf state (result (nth 0 actions-to) state))
                                       ; and reset variables
                                       (setf actions-from nil)
                                       (setf actions-to nil)
                                       (make-player-turn-res :click-state 0 :selected selected :state state)))))
                 (make-player-turn-res :click-state p-click-state :selected p-selected :state state)))
      (make-player-turn-res :click-state p-click-state :selected p-selected :state state)))

(defun ia-turn (state ai)
  "Ia turn"
  (let ((move (ai-search state +search-depth+ ai)))
    ; (format t "ia turn is done - move : ~a~%" move)
    (result move state)))

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

        (let ((state (make-state :board (set-board) :player +white+ :eating -1)) (click-state 0) (selected -1) (actions-from nil) (actions-to nil))

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

                (let* ((terminal (terminal-test state actions +white+))
                       (utility (terminal-utility-pair-terminal terminal)))
                  (when (terminal-utility-pair-terminal terminal)
                    (cond ((equal utility most-positive-fixnum) (format t "white wins~%"))
                          ((equal utility most-negative-fixnum) (format t "black wins~%"))
                          ((equal utility 0) (format t "draw~%")))
                    (sdl2:push-event :quit)))

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
                    (setf state (ia-turn state (get-ai 1 *gen*))))
                    ; (let ((res (player-turn state actions click-state selected)))
                    ;   (setf click-state (player-turn-res-click-state res))
                    ;   (setf selected (player-turn-res-selected res))
                    ;   (setf state (player-turn-res-state res))))

                  ; turn of the ia
                  (progn
                    (setf state (ia-turn state (get-ai 0 *gen*))))
                     ; (let ((res (player-turn state actions click-state selected)))
                     ;   (setf click-state (player-turn-res-click-state res))
                     ;   (setf selected (player-turn-res-selected res))
                     ;   (setf state (player-turn-res-state res))))
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

               (sdl2:delay 16))))
          (:quit () t)))))))

