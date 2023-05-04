(in-package :checkers-ai/tests/main)

(deftest test-get-line
  (testing "testing getting the line from the number"
    (ok (= (checkers-ai::get-line 0) 0))
    (ok (= (checkers-ai::get-line 1) 0))
    (ok (= (checkers-ai::get-line 7) 0))
    (ok (= (checkers-ai::get-line 8) 1))
    (ok (= (checkers-ai::get-line 15) 1))
    (ok (= (checkers-ai::get-line 19) 2))
    (ok (= (checkers-ai::get-line 28) 3))
    (ok (= (checkers-ai::get-line 37) 4))
    (ok (= (checkers-ai::get-line 46) 5))
    (ok (= (checkers-ai::get-line 55) 6))
    (ok (= (checkers-ai::get-line 56) 7))))

(deftest test-get-col
  (testing "testing getting the column from the number"
    (ok (= (checkers-ai::get-col 0) 0))
    (ok (= (checkers-ai::get-col 1) 1))
    (ok (= (checkers-ai::get-col 7) 7))
    (ok (= (checkers-ai::get-col 8) 0))
    (ok (= (checkers-ai::get-col 10) 2))
    (ok (= (checkers-ai::get-col 19) 3))
    (ok (= (checkers-ai::get-col 28) 4))
    (ok (= (checkers-ai::get-col 37) 5))
    (ok (= (checkers-ai::get-col 46) 6))
    (ok (= (checkers-ai::get-col 55) 7))
    (ok (= (checkers-ai::get-col 56) 0))))
