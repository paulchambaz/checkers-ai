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
