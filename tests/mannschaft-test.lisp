(in-package #:goliga/tests)

(deftest (test-parse-mannschaft :in goliga-tests) ()
  (let ((parsed (apply #'goliga::parse-event
                       '(goliga::mannschaft "foobar" "Die Fuhigen Bars"
                         (goliga::spieler "Abc Dere" "1 dan")
                         (goliga::spieler "dfsd ADSdf" "3 kyu")
                         (goliga::spieler "KIdfij dskjfd dsfkj" "4 kyu")
                         (goliga::spieler "sadf dsf" "4 kyu")
                         (goliga::spieler "Edds Edddd" "5 kyu")))))
    (is (typep parsed 'goliga::mannschaft))
    (is (string= (goliga::mannschaft-name parsed) "Die Fuhigen Bars"))
    (is (= (length (goliga::mannschaft-spieler parsed)) 5))
    (is (string= (goliga::spieler-name (aref (goliga::mannschaft-spieler parsed)
                                             3))
                 "sadf dsf"))
    (is (= (goliga::rang-int
            (goliga::spieler-rang
             (aref (goliga::mannschaft-spieler parsed) 0)))
           0))))
