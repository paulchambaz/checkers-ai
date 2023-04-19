(in-package :checkers-ai)

; TODO: maybe we could do the over part in there? i mean i really dont know at
; all how to handle this i guess we will just have to do our best
(defstruct utility-move-pair utility move)

(defun ai-search (state depth ai)
  (let ((actions (actions state)))
    ; if there is only one action then no need to start the search tree we can
    ; answer immediately
    (if (equal (list-length actions) 1)
      (nth 0 actions)
      (iterative-alpha-beta-search state depth ai))))

(defun iterative-alpha-beta-search (state depth ai)
  ; TODO: if res is positive or negative infinity that means we have a
  ; guaranteed winning or loosing move in n moves - no need to start the search
  ; tree for deeper values - it's over
  ; it would be great to have a similar ways of handling things with draws but
  ; i don't know how to do that - therefore we will just say it on the screen
  ; and continue the search - i mean if we have a guaranteed draw then it's
  ; really over isn't it?
  (if (equal (to-move state) +white+)
    (format t "white ")
    (format t "black "))
  (let* ((result (make-utility-move-pair :utility nil :move nil))
         (player (to-move state))
         (max-time (+ (get-internal-run-time) (* +search-time+ 1000000)))
         (max-depth 0)
         (history (make-hash-table :test #'action-test-function :hash-function #'action-hash-function))
         ; (killer-moves (make-array depth :initial-element (make-array 2 :initial-element (make-array 2 :initial-element nil))))
         )
    (block outer-loop
           (dotimes (n depth)
             (let ((res (max-value state most-negative-fixnum most-positive-fixnum player (+ n 1) max-time ai history)))
               ; we have a guaranted win or lose in n moves so we can just return
               (when (< (get-internal-run-time) max-time)
                 (setf result res)
                 (setf max-depth (+ n 1))
                 (when (or (equal (utility-move-pair-utility res) 1)
                           (equal (utility-move-pair-utility res) -1))
                   (return-from outer-loop))))))
    (format t "move: ~a value: ~a depth: ~a~%" (utility-move-pair-move result) (utility-move-pair-utility result) max-depth)
    (utility-move-pair-move result)))

; (defun order-moves (actions player depth history killer-moves)

(defun max-value (state alpha beta player depth max-time ai history)
  ; (format t "max-value~%")
  (let ((actions (actions state)))
    (if (or (> (get-internal-run-time) max-time) (equal depth 0))
      (return-from max-value (progn
                               ; (format t "end after computing utility~%")
                               (make-utility-move-pair :utility (utility state actions player ai) :move nil)))
      (let ((terminal (terminal-test state actions player)))
        (if (terminal-utility-pair-terminal terminal)
          ; (print t "test~a")
          (return-from max-value (progn
                                   ; (format t "end after a terminal test~%")
                                   (make-utility-move-pair :utility (* (terminal-utility-pair-utility terminal) 1000000) :move nil)))
          (let ((best (make-utility-move-pair :utility most-negative-fixnum :move nil))
                (sorted-actions (order-moves actions player depth history nil)))
            (dolist (move sorted-actions)
              (let* ((next-state (result move state))
                     (current (if (equal (to-move next-state) player)
                                (max-value next-state alpha beta player (- depth 1) max-time ai history)
                                (min-value next-state alpha beta player (- depth 1) max-time ai history))))
                ; (when (equal depth 14)
                  ; (format t "max-value depth: ~a utility after recursive call: ~a~%" depth (utility-move-pair-utility current)))
                (when (> (utility-move-pair-utility current) (utility-move-pair-utility best))
                  (setf (utility-move-pair-utility best) (utility-move-pair-utility current))
                  (setf (utility-move-pair-move best) move)
                  (setf alpha (max alpha (utility-move-pair-utility best))))
                (when (>= (utility-move-pair-utility best) beta)
                  (update-history move history)
                  (return-from max-value best))))
            best))))))

(defun min-value (state alpha beta player depth max-time ai history)
  ; (format t "min-value~%")
  (let ((actions (actions state)))
    (if (or (> (get-internal-run-time) max-time) (equal depth 0))
      (return-from min-value (progn
                               ; (format t "end after computing utility~%")
                               (make-utility-move-pair :utility (utility state actions player ai) :move nil)))
      (let* ((terminal (terminal-test state actions player)))
        (if (terminal-utility-pair-terminal terminal)
          ; (print t "test~a")
          (return-from min-value (progn
                                   ; (format t "end after a terminal test~%")
                                   (make-utility-move-pair :utility (* (terminal-utility-pair-utility terminal) 1000000) :move nil)))
          (let ((best (make-utility-move-pair :utility most-positive-fixnum :move nil))
                (sorted-actions (order-moves actions player depth history nil)))
            (dolist (move sorted-actions)
              (let* ((next-state (result move state))
                     (current (if (equal (to-move next-state) player)
                                (max-value next-state alpha beta player (- depth 1) max-time ai history)
                                (min-value next-state alpha beta player (- depth 1) max-time ai history))))
                ; (when (equal depth 14)
                ;   (format t "min-value depth: ~a utility after recursive call: ~a~%" depth (utility-move-pair-utility current)))
                (when (< (utility-move-pair-utility current) (utility-move-pair-utility best))
                  (setf (utility-move-pair-utility best) (utility-move-pair-utility current))
                  (setf (utility-move-pair-move best) move)
                  (setf beta (min beta (utility-move-pair-utility best))))
                (when (<= (utility-move-pair-utility best) alpha)
                  (update-history move history)
                  (return-from min-value best))))
            best))))))

; (defun iterative-alpha-beta-search (state depth ai)
;   (let ((value nil) (move nil) (player (to-move state))
;         (max-time (+ (get-internal-run-time) (* +search-time+ 1000000)))
;         (max-depth 0) (history (make-hash-table :test #'action-test-function :hash-function #'action-hash-function))
;         (killer-moves (make-array depth :initial-element (make-array 2 :initial-element (make-array 2 :initial-element nil)))))
;     ; (dotimes (n depth)
;     (let ((n 12))
;       (when (< (get-internal-run-time) max-time)
;         (multiple-value-bind (v m) (max-value state most-negative-fixnum most-positive-fixnum player (+ n 1) max-time ai history killer-moves)
;           (setf value v)
;           (setf move m)
;           (setf max-depth (+ n 1))
;         )
;       )
;     )
;     (if (equal (to-move state) +white+)
;       (format t "white ")
;       (format t "black "))
;     (format t "value: ~a depth: ~a~%" value max-depth)
;     (values value move)
;   )
; )

; (defun max-value (state alpha beta init-player depth max-time ai history killer-moves)
;   (if (or (equal depth 0) (> (get-internal-run-time) max-time))
;     (return-from max-value (values (utility state init-player ai) nil))
;     (let ((v most-negative-fixnum) (move nil) (actions (order-moves (actions state) (to-move state) depth history killer-moves)))
;     ; (let ((v most-negative-fixnum) (move nil) (actions (actions state)))
;       ; (if actions
;         (progn
;           (dolist (a actions)
;             (let ((result (result a state)))
;               (if (equal (to-move result) init-player)
;                 (progn
;                   (multiple-value-bind (v2 a2) (max-value result alpha beta init-player (- depth 1) max-time ai history killer-moves)
;                     (when (> v2 v)
;                       (setf v v2)
;                       (setf move a)
;                       (setf alpha (max alpha v))
;                     )
;                     (when (>= v beta)
;                       (update-history a history)
;                       (update-killer-moves a (to-move state) depth killer-moves history)
;                       (return-from max-value (values v move))
;                     )
;                   )
;                 )
;                 (progn
;                   (multiple-value-bind (v2 a2) (min-value result alpha beta init-player (- depth 1) max-time ai history killer-moves)
;                     (when (> v2 v)
;                       (setf v v2)
;                       (setf move a)
;                       (setf alpha (max alpha v))
;                     )
;                     (when (>= v beta)
;                       (update-history a history)
;                       (update-killer-moves a (to-move state) depth killer-moves history)
;                       (return-from max-value (values v move))
;                     )
;                   )
;                 )
;               )
;             )
;           )
;           (return-from max-value (values v move))
;         ; )
;         ; (return-from max-value (if (equal (to-move state) init-player) (values most-negative-fixnum nil) (values most-positive-fixnum nil)))
;       )
;     )
;   )
; )

; (defun min-value (state alpha beta init-player depth max-time ai history killer-moves)
;   (if (or (equal depth 0) (> (get-internal-run-time) max-time))
;     (return-from min-value (values (utility state init-player ai) nil))
;     (let ((v most-positive-fixnum) (move nil) (actions (order-moves (actions state) (to-move state) depth history killer-moves)))
;     ; (let ((v most-positive-fixnum) (move nil) (actions (actions state)))
;       ; (if actions
;         (progn
;           (dolist (a actions)
;             (let ((result (result a state)))
;               (if (equal (to-move result) init-player)
;                 (progn
;                   (multiple-value-bind (v2 a2) (max-value result alpha beta init-player (- depth 1) max-time ai history killer-moves)
;                     (when (< v2 v)
;                       (setf v v2)
;                       (setf move a)
;                       (setf beta (min beta v))
;                     )
;                     (when (<= v alpha)
;                       (update-history a history)
;                       (update-killer-moves a (to-move state) depth killer-moves history)
;                       (return-from min-value (values v move))
;                     )
;                   )
;                 )
;                 (progn
;                   (multiple-value-bind (v2 a2) (min-value result alpha beta init-player (- depth 1) max-time ai history killer-moves)
;                     (when (< v2 v)
;                       (setf v v2)
;                       (setf move a)
;                       (setf beta (min beta v))
;                     )
;                     (when (<= v alpha)
;                       (update-history a history)
;                       (update-killer-moves a (to-move state) depth killer-moves history)
;                       (return-from min-value (values v move))
;                     )
;                   )
;                 )
;               )
;             )
;           )
;           (return-from min-value (values v move))
;         ; )
;         ; (return-from min-value (if (equal (to-move state) init-player) (values most-negative-fixnum nil) (values most-positive-fixnum nil)))
;       )
;     )
;   )
; )

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
  (let ((current-killer-moves (aref (aref killer-moves depth) player))))
  ; TODO: implement this - the idea for the updating of killer moves once we
  ; have the correct depth and player is this - first you inser the element in
  ; the correct place in the list, this can be done in o(n) time by iterating
  ; over the list and getting the hash, if the hash is lower then we can insert
  ; it - if we still have a nil value we can just put it there
  ; once we have found the index at which we should insert - we can create a
  ; new list by (append (subseq current-killer-moves 0 index) (list action)
  ; (subseq current-killer-moves index (- (length (current-killer-moves) 1))))
  ; this is probably bad because it is an array and not a list but you get the
  ; idea
)

(defun compare-frequencies (a1 a2 history)
  (> (gethash a1 history 0) (gethash a2 history 0)))

(defun action-hash-function (action)
  (+ (* 31 (action-from action))
     (* 37 (action-to action))
     (* 41 (action-player action))
     (* 43 (action-eaten action))))

(defun action-test-function (a1 a2)
  (and (equal (action-from a1) (action-from a2))
       (equal (action-to a1) (action-to a2))
       (equal (action-player a1) (action-player a2))
       (equal (action-eaten a1) (action-eaten a2))))

(defun utility (state actions player ai)
  "Utility function of the algorithm"
  (let ((opponent (switch-player player)))
  (+ (* (get-weight 0 ai) (count-pawn state player))
     (* (get-weight 1 ai) (count-pawn state opponent))
     (* (get-weight 2 ai) (count-king state player))
     (* (get-weight 3 ai) (count-king state opponent))
     )))

(defun count-pawn (state player)
  "Counts the number of pawns of a player"
  (if (= player +white+)
    (count +white-pawn+ (state-board state))
    (count +black-pawn+ (state-board state))))

(defun count-king (state player)
  "Counts the number of kings of a player"
  (if (= player +white+)
      (count +white-king+ (state-board state))
      (count +black-king+ (state-board state))))

(defun count-mobility (actions)
  "Counts the number of moves available to a player"
    (list-length actions))

; (defun count-safe-checkers (state player)
;   "Count the number of safe kings the player has"
;   (
