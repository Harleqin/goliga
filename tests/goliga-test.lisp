(in-package #:goliga/tests)

(deftest (test-enter-liga :in goliga-tests) ()
  (let ((goliga::*ligen* (make-array 10 :adjustable t :fill-pointer 0))
        (test-data '(goliga::liga bl5 "Saison 2015/2016, 5. Bundesliga" 10
                     (goliga::runde
                      ;; before
                      (goliga::mannschaft "Foobar" "Frankfurter Foobar"
                       (goliga::spieler "Eugen Ick" "1 dan")
                       (goliga::spieler "..." "3 kyu"))
                      ;; in
                      (goliga::begegnung "Foobar" "AGruKi"
                       (goliga::brett 0 3 |2:0| "(Mo 18)")
                       (goliga::brett 1 4 |0:2|)
                       (goliga::brett 5 5 |1:1| "(Mo 18)")
                       (goliga::brett 6 8 |1:1|))
                      (goliga::begegnung "A" "B"
                       (goliga::brett)
                       (goliga::brett 3 3)
                       (goliga::brett nil 4))))))
    (is (goliga::enter-liga test-data))))
