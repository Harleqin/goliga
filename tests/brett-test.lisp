(in-package #:goliga/tests)

(deftest (test-parse-result :in goliga-tests) ()
  (is (null (goliga::parse-result nil)))
  (let ((parsed (goliga::parse-result "2:0")))
    (is (typep parsed 'goliga::result))
    (is (= (goliga::result-left-points parsed) 2))
    (is (= (goliga::result-right-points parsed) 0))
    (is (not (goliga::result-left-penalty-p parsed)))
    (is (not (goliga::result-right-penalty-p parsed))))
  (let ((parsed (goliga::parse-result "2:!")))
    (is (typep parsed 'goliga::result))
    (is (= (goliga::result-left-points parsed) 2))
    (is (= (goliga::result-right-points parsed) 0))
    (is (not (goliga::result-left-penalty-p parsed)))
    (is (goliga::result-right-penalty-p parsed)))
  (is (null (goliga::parse-result "2")))
  (is (null (goliga::parse-result "nil")))
  (is (null (goliga::parse-result nil))))

(deftest (test-parse-brett :in goliga-tests) ()
  (let ((parsed (goliga::parse-brett '(goliga::brett 1 3 "1:1" "Mo 13:00"))))
    (is (typep parsed 'goliga::brett))
    (is (= (goliga::brett-left-spieler parsed) 1))
    (is (= (goliga::brett-right-spieler parsed) 3))
    (is (= (goliga::result-left-points (goliga::brett-result parsed)) 1))
    (is (string= (goliga::brett-info parsed) "Mo 13:00"))))
