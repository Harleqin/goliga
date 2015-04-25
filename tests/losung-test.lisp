(in-package #:goliga/tests)

(deftest (test-determine-farben :in goliga-tests) ()
  (let ((a (make-instance 'goliga::table-row
                          :farben #(:left :left :right)))
        (b (make-instance 'goliga::table-row
                          :farben #(:left :right :right))))
    (multiple-value-bind (left right) (goliga::determine-farben a b)
      (is (eq left b))
      (is (eq right a)))))

(deftest (test-sort-from-combos :in goliga-tests) ()
  (let ((combos '((1 2 234)
                  (3 4 345)
                  (7 5 23423))))
    (is (equalp '(7 5) (goliga::sort-from-combos '(5 7) combos)))
    (is (equalp '(3 4) (goliga::sort-from-combos '(3 4) combos)))))
