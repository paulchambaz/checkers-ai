(in-package :checkers-ai/tests/main)

(deftest test-compute-diagonal
  (testing "testing the compute diagonals function"
    (ok (null (checkers-ai::compute-white-left-diagonal 0)))
    (ok (null (checkers-ai::compute-white-left-diagonal 3)))
    (ok (null (checkers-ai::compute-white-left-diagonal 16)))
    (ok (= (checkers-ai::compute-white-left-diagonal 15) 6))
    (ok (= (checkers-ai::compute-white-left-diagonal 10) 1))
    (ok (= (checkers-ai::compute-white-left-diagonal 19) 10))
    (ok (= (checkers-ai::compute-white-left-diagonal 25) 16))
    (ok (= (checkers-ai::compute-white-left-diagonal 59) 50))
    (ok (null (checkers-ai::compute-white-right-diagonal 7)))
    (ok (null (checkers-ai::compute-white-right-diagonal 4)))
    (ok (null (checkers-ai::compute-white-right-diagonal 31)))
    (ok (= (checkers-ai::compute-white-right-diagonal 40) 33))
    (ok (= (checkers-ai::compute-white-right-diagonal 45) 38))
    (ok (= (checkers-ai::compute-white-right-diagonal 21) 14))
    (ok (= (checkers-ai::compute-white-right-diagonal 57) 50))
    (ok (= (checkers-ai::compute-white-right-diagonal 26) 19))
    (ok (null (checkers-ai::compute-black-left-diagonal 63)))
    (ok (null (checkers-ai::compute-black-left-diagonal 31)))
    (ok (null (checkers-ai::compute-black-left-diagonal 59)))
    (ok (= (checkers-ai::compute-black-left-diagonal 34) 43))
    (ok (= (checkers-ai::compute-black-left-diagonal 11) 20))
    (ok (= (checkers-ai::compute-black-left-diagonal 46) 55))
    (ok (= (checkers-ai::compute-black-left-diagonal 5) 14))
    (ok (= (checkers-ai::compute-black-left-diagonal 40) 49))
    (ok (null (checkers-ai::compute-black-right-diagonal 56)))
    (ok (null (checkers-ai::compute-black-right-diagonal 60)))
    (ok (null (checkers-ai::compute-black-right-diagonal 24)))
    (ok (= (checkers-ai::compute-black-right-diagonal 39) 46))
    (ok (= (checkers-ai::compute-black-right-diagonal 4) 11))
    (ok (= (checkers-ai::compute-black-right-diagonal 36) 43))
    (ok (= (checkers-ai::compute-black-right-diagonal 21) 28))
    (ok (= (checkers-ai::compute-black-right-diagonal 5) 12))))

(deftest test-get-diagonal
  (testing "testing the get diagonal function"
    (checkers-ai::compute-diagonals)
    (ok (= (checkers-ai::get-diagonal 60 0 -1) 51))
    (ok (= (checkers-ai::get-diagonal 60 0 1) 53))
    (ok (null (checkers-ai::get-diagonal 60 1 -1)))
    (ok (null (checkers-ai::get-diagonal 60 1 1)))
    (ok (= (checkers-ai::get-diagonal 31 0 -1) 22))
    (ok (null (checkers-ai::get-diagonal 31 0 1)))
    (ok (null (checkers-ai::get-diagonal 31 1 -1)))
    (ok (= (checkers-ai::get-diagonal 31 1 1) 38))
    (ok (null (checkers-ai::get-diagonal 24 0 -1)))
    (ok (= (checkers-ai::get-diagonal 24 0 1) 17))
    (ok (= (checkers-ai::get-diagonal 24 1 -1) 33))
    (ok (null (checkers-ai::get-diagonal 24 1 1)))
    (ok (null (checkers-ai::get-diagonal 4 0 -1)))
    (ok (null (checkers-ai::get-diagonal 4 0 1)))
    (ok (= (checkers-ai::get-diagonal 4 1 -1) 13))
    (ok (= (checkers-ai::get-diagonal 4 1 1) 11))
    (ok (= (checkers-ai::get-diagonal 28 0 -1) 19))
    (ok (= (checkers-ai::get-diagonal 28 0 1) 21))
    (ok (= (checkers-ai::get-diagonal 28 1 -1) 37))
    (ok (= (checkers-ai::get-diagonal 28 1 1) 35))
    (ok (= (checkers-ai::get-diagonal 17 0 -1) 8))
    (ok (= (checkers-ai::get-diagonal 17 0 1) 10))
    (ok (= (checkers-ai::get-diagonal 17 1 -1) 26))
    (ok (= (checkers-ai::get-diagonal 17 1 1) 24))
    (ok (= (checkers-ai::get-diagonal 54 0 -1) 45))
    (ok (= (checkers-ai::get-diagonal 54 0 1) 47))
    (ok (= (checkers-ai::get-diagonal 54 1 -1) 63))
    (ok (= (checkers-ai::get-diagonal 54 1 1) 61))))

(deftest test-white-pawn-move
  (testing "testing white pawn movement"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+white-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+white+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 2 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 26 1 -1)))
          (ok (equal (nth 1 actions) (list 35 28 1 -1))))))))

(deftest test-white-pawn-eat-one
  (testing "testing white pawn eating"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+white-pawn+)
      (setf (nth 26 board) checkers-ai::+black-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+white+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 1 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 17 0 26))))))))

(deftest test-white-pawn-eat-all
  (testing "testing white pawn eating in all directions"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+white-pawn+)
      (setf (nth 26 board) checkers-ai::+black-pawn+)
      (setf (nth 28 board) checkers-ai::+black-pawn+)
      (setf (nth 42 board) checkers-ai::+black-pawn+)
      (setf (nth 44 board) checkers-ai::+black-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+white+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 4 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 17 0 26)))
          (ok (equal (nth 1 actions) (list 35 21 0 28)))
          (ok (equal (nth 2 actions) (list 35 53 0 44)))
          (ok (equal (nth 3 actions) (list 35 49 0 42))))))))

(deftest test-white-pawn-eat-force
  (testing "testing white pawn eating when a specific one should eat"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+white-pawn+)
      (setf (nth 37 board) checkers-ai::+white-pawn+)
      (setf (nth 26 board) checkers-ai::+black-pawn+)
      (setf (nth 30 board) checkers-ai::+black-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+white+ 35)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 1 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 17 0 26))))))))

(deftest test-white-king-move
  (testing "testing white king movement"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 56 board) checkers-ai::+white-king+)
      (let ((state (checkers-ai::make-state board checkers-ai::+white+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 7 (list-length actions)))
          (ok (equal (nth 0 actions) (list 56 7 1 -1)))
          (ok (equal (nth 1 actions) (list 56 14 1 -1)))
          (ok (equal (nth 2 actions) (list 56 21 1 -1)))
          (ok (equal (nth 3 actions) (list 56 28 1 -1)))
          (ok (equal (nth 4 actions) (list 56 35 1 -1)))
          (ok (equal (nth 5 actions) (list 56 42 1 -1)))
          (ok (equal (nth 6 actions) (list 56 49 1 -1))))))))

(deftest test-white-king-move-all
  (testing "testing white pawn movement"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 10 board) checkers-ai::+white-king+)
      (let ((state (checkers-ai::make-state board checkers-ai::+white+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 9 (list-length actions)))
          (ok (equal (nth 0 actions) (list 10 1 1 -1)))
          (ok (equal (nth 1 actions) (list 10 3 1 -1)))
          (ok (equal (nth 2 actions) (list 10 55 1 -1)))
          (ok (equal (nth 3 actions) (list 10 46 1 -1)))
          (ok (equal (nth 4 actions) (list 10 37 1 -1)))
          (ok (equal (nth 5 actions) (list 10 28 1 -1)))
          (ok (equal (nth 6 actions) (list 10 19 1 -1)))
          (ok (equal (nth 7 actions) (list 10 24 1 -1)))
          (ok (equal (nth 8 actions) (list 10 17 1 -1))))))))

(deftest test-white-king-eat
  (testing "testing white pawn eating in all directions"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+white-king+)
      (setf (nth 14 board) checkers-ai::+black-pawn+)
      (setf (nth 17 board) checkers-ai::+black-pawn+)
      (setf (nth 49 board) checkers-ai::+black-pawn+)
      (setf (nth 53 board) checkers-ai::+black-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+white+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 4 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 8 0 17)))
          (ok (equal (nth 1 actions) (list 35 7 0 14)))
          (ok (equal (nth 2 actions) (list 35 62 0 53)))
          (ok (equal (nth 3 actions) (list 35 56 0 49))))))))

(deftest test-white-king-eat-force
  (testing "testing white pawn eating when it is forced to"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+white-king+)
      (setf (nth 23 board) checkers-ai::+white-king+)
      (setf (nth 14 board) checkers-ai::+black-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+white+ 35)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 1 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 7 0 14))))))))

(deftest test-black-pawn-move
  (testing "testing black pawn movement"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+black-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+black+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 2 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 44 0 -1)))
          (ok (equal (nth 1 actions) (list 35 42 0 -1))))))))

(deftest test-black-pawn-eat-one
  (testing "testing black pawn eating"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+black-pawn+)
      (setf (nth 42 board) checkers-ai::+white-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+black+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 1 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 49 1 42))))))))

(deftest test-black-pawn-eat-all
  (testing "testing black pawn eating in all directions"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+black-pawn+)
      (setf (nth 26 board) checkers-ai::+white-pawn+)
      (setf (nth 28 board) checkers-ai::+white-pawn+)
      (setf (nth 42 board) checkers-ai::+white-pawn+)
      (setf (nth 44 board) checkers-ai::+white-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+black+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 4 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 53 1 44)))
          (ok (equal (nth 1 actions) (list 35 49 1 42)))
          (ok (equal (nth 2 actions) (list 35 17 1 26)))
          (ok (equal (nth 3 actions) (list 35 21 1 28))))))))

(deftest test-black-pawn-eat-force
  (testing "testing white pawn eating when a specific one should eat"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+black-pawn+)
      (setf (nth 37 board) checkers-ai::+black-pawn+)
      (setf (nth 42 board) checkers-ai::+white-pawn+)
      (setf (nth 46 board) checkers-ai::+white-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+black+ 35)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 1 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 49 1 42))))))))

(deftest test-black-king-move
  (testing "testing black king movement"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 56 board) checkers-ai::+black-king+)
      (let ((state (checkers-ai::make-state board checkers-ai::+black+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 7 (list-length actions)))
          (ok (equal (nth 0 actions) (list 56 7 0 -1)))
          (ok (equal (nth 1 actions) (list 56 14 0 -1)))
          (ok (equal (nth 2 actions) (list 56 21 0 -1)))
          (ok (equal (nth 3 actions) (list 56 28 0 -1)))
          (ok (equal (nth 4 actions) (list 56 35 0 -1)))
          (ok (equal (nth 5 actions) (list 56 42 0 -1)))
          (ok (equal (nth 6 actions) (list 56 49 0 -1))))))))

(deftest test-black-king-move-all
  (testing "testing black pawn movement"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 10 board) checkers-ai::+black-king+)
      (let ((state (checkers-ai::make-state board checkers-ai::+black+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 9 (list-length actions)))
          (ok (equal (nth 0 actions) (list 10 55 0 -1)))
          (ok (equal (nth 1 actions) (list 10 46 0 -1)))
          (ok (equal (nth 2 actions) (list 10 37 0 -1)))
          (ok (equal (nth 3 actions) (list 10 28 0 -1)))
          (ok (equal (nth 4 actions) (list 10 19 0 -1)))
          (ok (equal (nth 5 actions) (list 10 24 0 -1)))
          (ok (equal (nth 6 actions) (list 10 17 0 -1)))
          (ok (equal (nth 7 actions) (list 10 1 0 -1)))
          (ok (equal (nth 8 actions) (list 10 3 0 -1))))))))

(deftest test-black-king-eat
  (testing "testing black pawn eating in all directions"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+black-king+)
      (setf (nth 14 board) checkers-ai::+white-pawn+)
      (setf (nth 17 board) checkers-ai::+white-pawn+)
      (setf (nth 49 board) checkers-ai::+white-pawn+)
      (setf (nth 53 board) checkers-ai::+white-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+black+ -1)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 4 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 62 1 53)))
          (ok (equal (nth 1 actions) (list 35 56 1 49)))
          (ok (equal (nth 2 actions) (list 35 8 1 17)))
          (ok (equal (nth 3 actions) (list 35 7 1 14)))
          )))))

(deftest test-black-king-eat-force
  (testing "testing black pawn eating when it is forced to"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 35 board) checkers-ai::+black-king+)
      (setf (nth 23 board) checkers-ai::+black-king+)
      (setf (nth 14 board) checkers-ai::+white-pawn+)
      (let ((state (checkers-ai::make-state board checkers-ai::+black+ 35)))
        (let ((actions (checkers-ai::actions state)))
          (ok (equal 1 (list-length actions)))
          (ok (equal (nth 0 actions) (list 35 7 1 14)))
          )))))

(deftest test-result
  (testing "testing we get the correct list of actions"
    (ok (= 1 1))))

(deftest test-white-pawn-move-result
  (testing "testing we can do movements have the correct result"
    (let ((state (checkers-ai::make-state (list 0 0 0 0 0 0 0 0 ; 0 
                                                0 0 0 0 0 0 0 0 ; 1
                                                0 0 0 0 0 0 0 0 ; 2
                                                0 0 0 0 0 0 0 0 ; 3
                                                0 0 0 0 0 0 0 0 ; 4
                                                0 0 0 0 0 0 1 0 ; 5
                                                0 0 0 0 0 0 0 0 ; 6
                                                0 0 0 0 0 0 0 0 ; 7
                                                ) 0 -1)))
      (let ((action (checkers-ai::make-action 46 37 1 -1)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 0 0 0 0 ; 3
                                      0 0 0 0 0 1 0 0 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 1 -1))))
      (let ((action (checkers-ai::make-action 46 39 1 -1)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 1 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 1 -1))))
      )))

(deftest test-white-king-move-result
  (testing "testing we can do movements have the correct result"
    (let ((state (checkers-ai::make-state (list 0 0 0 0 0 0 0 0 ; 0 
                                                0 0 0 0 0 0 0 0 ; 1
                                                0 0 0 0 0 0 0 0 ; 2
                                                0 0 0 0 0 0 0 0 ; 3
                                                0 0 0 0 0 0 0 0 ; 4
                                                0 0 0 0 0 0 2 0 ; 5
                                                0 0 0 0 0 0 0 0 ; 6
                                                0 0 0 0 0 0 0 0 ; 7
                                                ) 0 -1)))
      (let ((action (checkers-ai::make-action 46 37 1 -1)) (result (checkers-ai::result action (checkers-ai::copy-state state))))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 0 0 0 0 ; 3
                                      0 0 0 0 0 2 0 0 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 1 -1))))
      (let ((action (checkers-ai::make-action 46 28 1 -1)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 2 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 1 -1))))
      (let ((action (checkers-ai::make-action 46 19 1 -1)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 2 0 0 0 0 ; 2
                                      0 0 0 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 1 -1))))
      (let ((action (checkers-ai::make-action 46 10 1 -1)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 2 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 1 -1))))
      (let ((action (checkers-ai::make-action 46 1 1 -1)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 2 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 1 -1))))
      (let ((action (checkers-ai::make-action 46 39 1 -1)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 2 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 1 -1))))
      (let ((action (checkers-ai::make-action 46 53 1 -1)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 2 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 1 -1))))
      (let ((action (checkers-ai::make-action 46 60 1 -1)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 2 0 0 0 ; 7
                                      ) 1 -1))))
      (let ((action (checkers-ai::make-action 46 55 1 -1)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 0 0 0 0 0 0 ; 5
                                      0 0 0 0 0 0 0 2 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 1 -1))))
      )))

(deftest test-white-pawn-move-result
  (testing "testing we can do movements have the correct result"
    (let ((state (checkers-ai::make-state (list 0 0 0 0 0 0 0 0 ; 0 
                                                0 0 0 0 0 0 0 0 ; 1
                                                0 0 0 0 0 0 0 0 ; 2
                                                0 0 2 0 2 0 0 0 ; 3
                                                0 0 0 1 0 0 0 0 ; 4
                                                0 0 2 0 2 0 0 0 ; 5
                                                0 0 0 0 0 0 0 0 ; 6
                                                0 0 0 0 0 0 0 0 ; 7
                                                ) 0 -1)))
      (let ((action (checkers-ai::make-action 35 17 0 26)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 1 0 0 0 0 0 0 ; 2
                                      0 0 0 0 2 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 2 0 2 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 17))))
      (let ((action (checkers-ai::make-action 35 21 0 28)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 1 0 0 ; 2
                                      0 0 2 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 2 0 2 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 21))))
      (let ((action (checkers-ai::make-action 35 49 0 42)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 2 0 2 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 0 0 2 0 0 0 ; 5
                                      0 1 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 49))))
      (let ((action (checkers-ai::make-action 35 53 0 44)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 2 0 2 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 2 0 0 0 0 0 ; 5
                                      0 0 0 0 0 1 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 53))))
      )))

(deftest test-white-king-move-result
  (testing "testing we can do movements have the correct result"
    (let ((state (checkers-ai::make-state (list 0 0 0 0 0 0 0 0 ; 0 
                                                0 0 0 0 0 0 0 0 ; 1
                                                0 0 0 0 0 0 0 0 ; 2
                                                0 0 2 0 2 0 0 0 ; 3
                                                0 0 0 3 0 0 0 0 ; 4
                                                0 0 2 0 2 0 0 0 ; 5
                                                0 0 0 0 0 0 0 0 ; 6
                                                0 0 0 0 0 0 0 0 ; 7
                                                ) 0 -1)))
      (let ((action (checkers-ai::make-action 35 17 0 26)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 3 0 0 0 0 0 0 ; 2
                                      0 0 0 0 2 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 2 0 2 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 17))))
      (let ((action (checkers-ai::make-action 35 8 0 26)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      3 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 0 0 2 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 2 0 2 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 8))))
      (let ((action (checkers-ai::make-action 35 21 0 28)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 3 0 0 ; 2
                                      0 0 2 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 2 0 2 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 21))))
      (let ((action (checkers-ai::make-action 35 14 0 28)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 3 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 2 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 2 0 2 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 14))))
      (let ((action (checkers-ai::make-action 35 7 0 28)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 3 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 2 0 0 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 2 0 2 0 0 0 ; 5
                                      0 0 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 7))))
      (let ((action (checkers-ai::make-action 35 49 0 42)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 2 0 2 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 0 0 2 0 0 0 ; 5
                                      0 3 0 0 0 0 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 49))))
      (let ((action (checkers-ai::make-action 35 53 0 44)) (result (checkers-ai::copy-state state)))
        (checkers-ai::result action result)
        (ok (equal result (list (list 0 0 0 0 0 0 0 0 ; 0
                                      0 0 0 0 0 0 0 0 ; 1
                                      0 0 0 0 0 0 0 0 ; 2
                                      0 0 2 0 2 0 0 0 ; 3
                                      0 0 0 0 0 0 0 0 ; 4
                                      0 0 2 0 0 0 0 0 ; 5
                                      0 0 0 0 0 3 0 0 ; 6
                                      0 0 0 0 0 0 0 0 ; 7
                                      ) 0 53))))
      )))


(deftest test-switch-player
  (testing "testing that the switch player works as intended"
    (ok (= (checkers-ai::switch-player checkers-ai::+white+) 1))
    (ok (= (checkers-ai::switch-player checkers-ai::+black+) 0))))

(deftest test-select-from
  (testing "testing that we can select the actions that start from a square"
    (let ((actions (list
                     (checkers-ai::make-action 1 10 checkers-ai::+black+ -1)
                     (checkers-ai::make-action 10 19 checkers-ai::+white+ -1)
                     (checkers-ai::make-action 7 21 checkers-ai::+black+ 14)
                     (checkers-ai::make-action 1 19 checkers-ai::+white+ 10))))
      (let ((actions-from (checkers-ai::select-from 1 actions)))
        (ok (equal 2 (list-length actions-from)))
        (ok (equal (nth 0 actions-from) (list 1 10 1 -1)))
        (ok (equal (nth 1 actions-from) (list 1 19 0 10)))))))

(deftest test-select-to
  (testing "testing that we can select the actions that end at a square"
    (let ((actions (list
                     (checkers-ai::make-action 1 10 checkers-ai::+black+ -1)
                     (checkers-ai::make-action 10 19 checkers-ai::+white+ -1)
                     (checkers-ai::make-action 7 21 checkers-ai::+black+ 14)
                     (checkers-ai::make-action 1 19 checkers-ai::+white+ 10))))
      (let ((actions-to (checkers-ai::select-to 19 actions)))
        (ok (equal 2 (list-length actions-to)))
        (ok (equal (nth 0 actions-to) (list 10 19 0 -1)))
        (ok (equal (nth 1 actions-to) (list 1 19 0 10)))))))

(deftest test-select-eating
  (testing "testing that we can select the actions that eat"
    (let ((actions (list
                     (checkers-ai::make-action 1 10 checkers-ai::+black+ -1)
                     (checkers-ai::make-action 10 19 checkers-ai::+white+ -1)
                     (checkers-ai::make-action 7 21 checkers-ai::+black+ 14)
                     (checkers-ai::make-action 1 19 checkers-ai::+white+ 10))))
      (let ((actions-eat (checkers-ai::select-eating actions)))
        (ok (equal 2 (list-length actions-eat)))
        (ok (equal (nth 0 actions-eat) (list 7 21 1 14)))
        (ok (equal (nth 1 actions-eat) (list 1 19 0 10)))))))

(deftest test-get-pieces
  (testing "testing that we get the correct list of pieces from a board"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 1 board) checkers-ai::+white-pawn+)
      (setf (nth 3 board) checkers-ai::+white-king+)
      (setf (nth 5 board) checkers-ai::+black-pawn+)
      (setf (nth 7 board) checkers-ai::+black-king+)
      (let ((pieces (checkers-ai::get-pieces board)))
        (ok (equal 4 (list-length pieces)))
        (ok (equal (nth 3 pieces) (list :n 1 :id checkers-ai::+white-pawn+)))
        (ok (equal (nth 2 pieces) (list :n 3 :id checkers-ai::+white-king+)))
        (ok (equal (nth 1 pieces) (list :n 5 :id checkers-ai::+black-pawn+)))
        (ok (equal (nth 0 pieces) (list :n 7 :id checkers-ai::+black-king+)))))))

(deftest test-get-whites
  (testing "testing that we get the list of white pieces from a board"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 1 board) checkers-ai::+white-pawn+)
      (setf (nth 3 board) checkers-ai::+white-king+)
      (setf (nth 5 board) checkers-ai::+black-pawn+)
      (setf (nth 7 board) checkers-ai::+black-king+)
      (let ((pieces (checkers-ai::get-whites board)))
        (ok (equal 2 (list-length pieces)))
        (ok (equal (nth 1 pieces) (list :n 1 :id checkers-ai::+white-pawn+)))
        (ok (equal (nth 0 pieces) (list :n 3 :id checkers-ai::+white-king+)))))))

(deftest test-get-white-pawns
  (testing "testing that we get the list of white pawns from a board"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 1 board) checkers-ai::+white-pawn+)
      (setf (nth 3 board) checkers-ai::+white-king+)
      (setf (nth 5 board) checkers-ai::+black-pawn+)
      (setf (nth 7 board) checkers-ai::+black-king+)
      (let ((pieces (checkers-ai::get-white-pawns board)))
        (ok (equal 1 (list-length pieces)))
        (ok (equal (nth 0 pieces) (list :n 1 :id checkers-ai::+white-pawn+)))))))

(deftest test-get-white-kings
  (testing "testing that we get the list of white kings from a board"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 1 board) checkers-ai::+white-pawn+)
      (setf (nth 3 board) checkers-ai::+white-king+)
      (setf (nth 5 board) checkers-ai::+black-pawn+)
      (setf (nth 7 board) checkers-ai::+black-king+)
      (let ((pieces (checkers-ai::get-white-kings board)))
        (ok (equal 1 (list-length pieces)))
        (ok (equal (nth 0 pieces) (list :n 3 :id checkers-ai::+white-king+)))))))

(deftest test-get-blacks
  (testing "testing the we get the list of black pieces from a board"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 1 board) checkers-ai::+white-pawn+)
      (setf (nth 3 board) checkers-ai::+white-king+)
      (setf (nth 5 board) checkers-ai::+black-pawn+)
      (setf (nth 7 board) checkers-ai::+black-king+)
      (let ((pieces (checkers-ai::get-blacks board)))
        (ok (equal 2 (list-length pieces)))
        (ok (equal (nth 1 pieces) (list :n 5 :id checkers-ai::+black-pawn+)))
        (ok (equal (nth 0 pieces) (list :n 7 :id checkers-ai::+black-king+)))))))

(deftest test-get-black-pawns
  (testing "testing that we get the list of black pawns from a board"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 1 board) checkers-ai::+white-pawn+)
      (setf (nth 3 board) checkers-ai::+white-king+)
      (setf (nth 5 board) checkers-ai::+black-pawn+)
      (setf (nth 7 board) checkers-ai::+black-king+)
      (let ((pieces (checkers-ai::get-black-pawns board)))
        (ok (equal 1 (list-length pieces)))
        (ok (equal (nth 0 pieces) (list :n 5 :id checkers-ai::+black-pawn+)))))))

(deftest test-get-black-kings
  (testing "testing that we get the list of black kings from a board"
    (let ((board (make-list checkers-ai::+nb-squares+ :initial-element 0)))
      (setf (nth 1 board) checkers-ai::+white-pawn+)
      (setf (nth 3 board) checkers-ai::+white-king+)
      (setf (nth 5 board) checkers-ai::+black-pawn+)
      (setf (nth 7 board) checkers-ai::+black-king+)
      (let ((pieces (checkers-ai::get-black-kings board)))
        (ok (equal 1 (list-length pieces)))
        (ok (equal (nth 0 pieces) (list :n 7 :id checkers-ai::+black-king+)))))))

