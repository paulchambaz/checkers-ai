(in-package :checkers-ai)

(defun ai-search (state depth ai)
  (let ((actions (actions state)))
    (if (equal (list-length actions) 1)
        ; if only one move is possible no need to search
        (values 0 (nth 0 actions))
        ; else we start iterative alpha beta search
        (iterative-alpha-beta-search state depth ai))))

(defun iterative-alpha-beta-search (state depth ai)
  (let ((value nil) (move nil) (final-depth nil))
    (let ((player (to-move state)) (time (get-internal-run-time)))
      (let ((max-time (+ time (* +search-time+ 1000000))))
        (dotimes (n depth)
          (when (< time max-time)
            (multiple-value-bind (_value _move)
                (max-value (copy-state state) +negative-infinity+ +positive-infinity+ player 0 n time max-time ai)
              (setf time (get-internal-run-time))
              (setf value _value)
              (setf move _move)
              (setf final-depth n))))
        (if (equal player 0)
            (format t "White ")
            (format t "Black "))
        (format t "reached a depth of ~a in ~ams " final-depth (floor (- time (- max-time (* 1 1000000))) 1000))
        (format t "- value: ~a move: ~a~%" value move)
        (values value move)))))

(defun alpha-beta-search (state depth ai)
  (let ((player (to-move state)) (time (get-internal-run-time)))
    (multiple-value-bind (value move)
        ; for default alpha beta search we give it 1M seconds, since it is a
        ; DFS we cannot guarantee that it will return the best result. this
        ; fake limit is just a way to make sure it does - we will not be using
        ; depth with this algorithm that would take longer than 1M seconds
        (max-value (copy-state state) +negative-infinity+ +positive-infinity+ player 0 depth time (+ time (* 1000000 1000000)) ai)
      (values value move))))

(defun max-value (state alpha beta init-player depth max-depth time max-time ai)
  ; return clause
  (if (> time max-time)
      (values (utility state init-player ai) nil)
      (if (= depth max-depth)
          (values (utility state init-player ai) nil)
          (let ((v +negative-infinity+) (move nil))
            ; gets the list of actions
            (dolist (a (actions state))
              (let ((result (result a (copy-state state))))
                ; is it the init-player's turn to play
                (if (= (to-move result) init-player)
                    ; it is
                    (multiple-value-bind (v2 a2)
                        ; recursive call
                        (max-value result alpha beta init-player (+ depth 1) max-depth (get-internal-run-time) max-time ai)
                      (when (> v2 v) 
                        (setf v v2)
                        (setf move a)
                        (setf alpha (max alpha v))
                        )
                      (when (>= v beta) return (values v move))
                      )
                    ; it is not
                    (multiple-value-bind (v2 a2)
                        ; recursive call
                        (min-value result alpha beta init-player (+ depth 1) max-depth (get-internal-run-time) max-time ai)
                      (when (> v2 v) 
                        (setf v v2)
                        (setf move a)
                        (setf beta (min beta v))
                        )
                      (when (<= v alpha) return (values v move))
                      )
                  )
                )
              )
            (values v move)
            )
        )
    )
)

(defun min-value (state alpha beta init-player depth max-depth time max-time ai)
  ; return clause
  (if (> time max-time)
      (values (utility state init-player ai) nil)
      (if (= depth max-depth)
          (values (utility state init-player ai) nil)
          (let ((v +positive-infinity+) (move nil))
            ; gets the list of actions
            (dolist (a (actions state))
              (let ((result (result a (copy-state state))))
                ; is it the init-player's turn to play
                (if (= (to-move result) init-player)
                    ; it is
                    (multiple-value-bind (v2 a2)
                        ; recursive call
                        (max-value result alpha beta init-player (+ depth 1) max-depth (get-internal-run-time) max-time ai)
                      (when (< v2 v) 
                        (setf v v2)
                        (setf move a)
                        (setf alpha (max alpha v))
                        )
                      (when (>= v beta) return (values v move))
                      )
                    ; it is not
                    (multiple-value-bind (v2 a2)
                        ; recursive call
                        (min-value result alpha beta init-player (+ depth 1) max-depth (get-internal-run-time) max-time ai)
                      (when (< v2 v) 
                        (setf v v2)
                        (setf move a)
                        (setf beta (min beta v))
                        )
                      (when (<= v alpha) return (values v move))
                      )
                  )
                )
              )
            (values v move)
            )
        )
    )
)

(defun minimax-search (state depth ai)
  (let ((player (to-move state)))
    (multiple-value-bind (value move)
                         (max-value-minimax (copy-state state) player 0 depth ai)
      (values value move))))

(defun max-value-minimax (state init-player depth max-depth ai)
  ; return clause
  (if (= depth max-depth)
    (values (utility state init-player ai) nil)
    (let ((v +negative-infinity+) (move nil))
      ; gets the list of actions
      (dolist (a (actions state))
        (let ((result (result a (copy-state state))))
          ; is it the init-player's turn to play
          (if (= (to-move result) init-player)
            ; it is
            (multiple-value-bind (v2 a2)
                ; recursive call
                (max-value-minimax result init-player (+ depth 1) max-depth ai)
              (when (> v2 v) 
                (setf v v2)
                (setf move a)
              )
            )
            ; it is not
            (multiple-value-bind (v2 a2)
                ; recursive call
                (min-value-minimax result init-player (+ depth 1) max-depth ai)
              (when (> v2 v) 
                (setf v v2)
                (setf move a)
              )
            )
          )
        )
      )
      (values v move)
    )
  )
)

(defun min-value-minimax (state init-player depth max-depth ai)
  ; return clause
  (if (= depth max-depth)
    (values (utility state init-player ai) nil)
    (let ((v +positive-infinity+) (move nil))
      ; gets the list of actions
      (dolist (a (actions state))
        (let ((result (result a (copy-state state))))
          ; is it the init-player's turn to play
          (if (= (to-move result) init-player)
            ; it is
            (multiple-value-bind (v2 a2)
                ; recursive call
                (max-value-minimax result init-player (+ depth 1) max-depth ai)
              (when (< v2 v) 
                (setf v v2)
                (setf move a)
              )
            )
            ; it is not
            (multiple-value-bind (v2 a2)
                ; recursive call
                (min-value-minimax result init-player (+ depth 1) max-depth ai)
              (when (< v2 v) 
                (setf v v2)
                (setf move a)
              )
            )
          )
        )
      )
      (values v move)
    )
  )
)

(defun utility (state player ai)
  "Utility function of the algorithm"
  ; (format t "utility~%")
  ; (format t "~a~%" ai)
  (+ (* (get-weight 0 ai) (count-pawn state player))
     (* (get-weight 1 ai) (count-pawn state (switch-player player)))
     ))

(defun count-pawn (state player)
  "Counts the number of pieces of a player"
  ; (format t "count-pawn~%")
  (if (= player 0)
    (count +white-pawn+ (get-state-board state))
    (count +black-pawn+ (get-state-board state))
  )
)
