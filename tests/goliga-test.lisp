(in-package #:goliga/tests)

(deftest (test-enter-liga :in goliga-tests) ()
  (let ((goliga::*ligen* (make-array 10 :adjustable t :fill-pointer 0))
        (test-data '(goliga::liga bl5 "Saison 2015/2016, 5. Bundesliga" 10
                     (goliga::runde
                      ;; before
                      (goliga::mannschaft "Foobar" "Frankfurter Foobar"
                       (goliga::spieler "Eugen Ick" "1 dan")
                       (goliga::spieler "..." "3 kyu"))
                      (goliga::mannschaft "AGruKi" "Augsburger Gruppenkiste"
                       (goliga::spieler "Alslda dsaf" "1 kyu")
                       (goliga::spieler "Badsf dsafd" "2 kyu")
                       (goliga::spieler "Cadfasdf adsf" "2 kyu")
                       (goliga::spieler "Dadsf dasf" "2 kyu")
                       (goliga::spieler "Efds dsafd" "2 kyu")
                       (goliga::spieler "Fdsaf dsafd" "2 kyu")
                       (goliga::spieler "Gdsdf dsafd" "2 kyu")
                       (goliga::spieler "Hdfew dsafd" "2 kyu"))
                      ;; in
                      (goliga::begegnung "Foobar" "AGruKi"
                       (goliga::brett 0 3 |2:0| "(Mo 18)")
                       (goliga::brett 1 4 |0:2|)
                       (goliga::brett 5 5 |1:1| "(Mo 18)")
                       (goliga::brett 6 8 |1:1|))))))
    (let ((liga (goliga::enter-liga test-data)))
      (is (typep liga 'goliga::liga))
      (is (= (hash-table-count (aref (goliga::liga-mannschaften liga) 0)) 2))
      (is (= (goliga::table-row-mp (gethash "Foobar"
                                            (goliga::tabelle-mannschaft-values
                                             (aref (goliga::liga-tabellen liga)
                                                   1))))
             1)))))
