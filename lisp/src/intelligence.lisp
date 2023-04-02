(in-package :checkers-ai)

(defun ai-search (state depth ai)
  (let ((actions (actions state)))
    (if (equal (list-length actions) 1)
      (values 0 (nth 0 actions))
      (iterative-alpha-beta-search state depth ai)
    )
  )
)

(defun iterative-alpha-beta-search (state depth ai)
  (let ((value nil) (move nil) (player (to-move state))
        (max-time (+ (get-internal-run-time) (* +search-time+ 1000000)))
        (max-depth 0) (history (make-hash-table :test #'action-test-function :hash-function #'action-hash-function))
        (killer-moves (make-array depth :initial-element (make-array 2 :initial-element (make-array 2 :initial-element nil)))))
    ; (dotimes (n depth)
    (let ((n 2))
      (when (< (get-internal-run-time) max-time)
        ; (format t  "~a~%" (+ n 1))
        (multiple-value-bind (v m) (max-value state most-negative-fixnum most-positive-fixnum player (+ n 1) max-time ai history killer-moves)
          (setf value v)
          (setf move m)
          (setf max-depth (+ n 1))
        )
      )
    )
    (if (equal (to-move state) +white+)
      (format t "white ")
      (format t "black "))
    (format t "value: ~a depth: ~a~%" value max-depth)
    (values value move)
  )
)

(defun max-value (state alpha beta init-player depth max-time ai history killer-moves)
  (if (or (equal depth 0) (> (get-internal-run-time) max-time))
    (return-from max-value (values (utility state init-player ai) nil))
    (let ((v most-negative-fixnum) (move nil) (actions (order-moves (actions state) (to-move state) depth history killer-moves)))
    ; (let ((v most-negative-fixnum) (move nil) (actions (actions state)))
      ; (if actions
        (progn
          (dolist (a actions)
            (let ((result (result a state)))
              (if (equal (to-move result) init-player)
                (progn
                  (multiple-value-bind (v2 a2) (max-value result alpha beta init-player (- depth 1) max-time ai history killer-moves)
                    (when (> v2 v)
                      (setf v v2)
                      (setf move a)
                      (setf alpha (max alpha v))
                    )
                    (when (>= v beta)
                      (update-history a history)
                      (update-killer-moves a (to-move state) depth killer-moves history)
                      (return-from max-value (values v move))
                    )
                  )
                )
                (progn
                  (multiple-value-bind (v2 a2) (min-value result alpha beta init-player (- depth 1) max-time ai history killer-moves)
                    (when (> v2 v)
                      (setf v v2)
                      (setf move a)
                      (setf alpha (max alpha v))
                    )
                    (when (>= v beta)
                      (update-history a history)
                      (update-killer-moves a (to-move state) depth killer-moves history)
                      (return-from max-value (values v move))
                    )
                  )
                )
              )
            )
          )
          (return-from max-value (values v move))
        ; )
        ; (return-from max-value (if (equal (to-move state) init-player) (values most-negative-fixnum nil) (values most-positive-fixnum nil)))
      )
    )
  )
)

(defun min-value (state alpha beta init-player depth max-time ai history killer-moves)
  (if (or (equal depth 0) (> (get-internal-run-time) max-time))
    (return-from min-value (values (utility state init-player ai) nil))
    (let ((v most-positive-fixnum) (move nil) (actions (order-moves (actions state) (to-move state) depth history killer-moves)))
    ; (let ((v most-positive-fixnum) (move nil) (actions (actions state)))
      ; (if actions
        (progn
          (dolist (a actions)
            (let ((result (result a state)))
              (if (equal (to-move result) init-player)
                (progn
                  (multiple-value-bind (v2 a2) (max-value result alpha beta init-player (- depth 1) max-time ai history killer-moves)
                    (when (< v2 v)
                      (setf v v2)
                      (setf move a)
                      (setf beta (min beta v))
                    )
                    (when (<= v alpha)
                      (update-history a history)
                      (update-killer-moves a (to-move state) depth killer-moves history)
                      (return-from min-value (values v move))
                    )
                  )
                )
                (progn
                  (multiple-value-bind (v2 a2) (min-value result alpha beta init-player (- depth 1) max-time ai history killer-moves)
                    (when (< v2 v)
                      (setf v v2)
                      (setf move a)
                      (setf beta (min beta v))
                    )
                    (when (<= v alpha)
                      (update-history a history)
                      (update-killer-moves a (to-move state) depth killer-moves history)
                      (return-from min-value (values v move))
                    )
                  )
                )
              )
            )
          )
          (return-from min-value (values v move))
        ; )
        ; (return-from min-value (if (equal (to-move state) init-player) (values most-negative-fixnum nil) (values most-positive-fixnum nil)))
      )
    )
  )
)

(defun order-moves (actions player depth history killer-moves)
  (stable-sort (if (equal player +white+) actions (reverse actions))
               (lambda (a1 a2) (compare-frequencies a1 a2 history))))

(defun update-history (action history)
  (let ((frequency (gethash action history)))
    (if frequency
        (setf (gethash action history) (+ frequency 1))
        (setf (gethash action history) 0)
    )
  )
)

(defun update-killer-moves (action player depth killer-moves history)

  ; (format t "action: ~a player: ~a~%" action player)

  (let ((current-killer-moves (aref (aref killer-moves depth) player)) (action-frequency(gethash action history 0)) (replace-index -1) (lowest-frequency most-positive-fixnum))

    (dotimes (i (length current-killer-moves))
      (when (equal (aref current-killer-moves i) action)
        (return-from update-killer-moves)
      )
    )

    (dotimes (i (length current-killer-moves))
      (let ((move (aref current-killer-moves i)))
        (if move
          (let ((move-frequency (gethash move history most-positive-fixnum)))
            (when (< move-frequency lowest-frequency)
              (setf lowest-frequency move-frequency)
              (setf replace-index i)
            )
          )
          (progn
            (setf replace-index i)
            (return)
          )
        )
      )
    )

    (when (>= replace-index 0)
      (setf (aref current-killer-moves replace-index) action)
    )
  )
)

(defun compare-frequencies (a1 a2 history)
  (> (gethash a1 history 0) (gethash a2 history 0)))

(defun action-hash-function (action)
  (+ (* 31 (get-action-from action))
     (* 37 (get-action-to action))
     (* 41 (get-action-player action))
     (* 43 (get-action-eaten action))))

(defun action-test-function (a1 a2)
  (and (equal (get-action-from a1) (get-action-from a2))
       (equal (get-action-to a1) (get-action-to a2))
       (equal (get-action-player a1) (get-action-player a2))
       (equal (get-action-eaten a1) (get-action-eaten a2))))

(defun utility (state player ai)
  "Utility function of the algorithm"
  (let ((state-copy-player (copy-state state))
        (state-copy-ennemy (copy-state state)))
    (setf (nth 1 state-copy-player) player)
    (setf (nth 1 state-copy-ennemy) (switch-player player))
    (let ((actions-player (actions state-copy-player))
          (actions-ennemy (actions state-copy-ennemy)))
      (let ((utility 
              (+ (* (get-weight 0 ai) (count-pawn state player))
                 (* (get-weight 1 ai) (count-pawn state (switch-player player)))
                 (* (get-weight 2 ai) (count-king state player))
                 (* (get-weight 3 ai) (count-king state (switch-player player)))
                 (* (get-weight 4 ai) (count-mobility actions-player))
                 (* (get-weight 5 ai) (count-mobility actions-ennemy))
                 )))
        ; (format t "utility: ~a~%" utility)
        utility))))

(defun count-pawn (state player)
  "Counts the number of pawns of a player"
  (if (= player +white+)
    (count +white-pawn+ (get-state-board state))
    (count +black-pawn+ (get-state-board state))))

(defun count-king (state player)
  "Counts the number of kings of a player"
  (if (= player +white+)
      (count +white-king+ (get-state-board state))
      (count +black-king+ (get-state-board state))))

(defun count-mobility (actions)
  "Counts the number of moves available to a player"
    (list-length actions))

; (defun count-safe-kings (state player)
;   "Count the number of safe kings the player has"
;   (
