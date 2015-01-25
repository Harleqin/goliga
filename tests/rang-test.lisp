(in-package #:goliga/tests)

(deftest (test-parse-rang :in goliga-tests) ()
  (is (goliga::rang= (goliga::parse-rang "3 kyu")
                     (goliga::parse-rang "3k")))
  (is (= (goliga::rang-int (goliga::parse-rang "1 Dan")) 0))
  (is (= (goliga::rang-int (goliga::parse-rang "3 dan")) 2))
  (is (= (goliga::rang-int (goliga::parse-rang "12kyu")) -12)))
