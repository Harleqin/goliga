(in-package #:goliga/tests)

(deftest (test-next-tabelle-nil :in goliga-tests) ()
  (let ((tabelle (goliga::next-tabelle
                  nil
                  (list :before
                        (vector (make-instance 'goliga::mannschaft
                                               :kuerzel "foo")
                                (make-instance 'goliga::pre-round-event))))))
    (is (typep tabelle 'goliga::tabelle))
    (is (= (hash-table-count (goliga::tabelle-mannschaft-values tabelle)) 1))))

(deftest (test-next-tabelle-update :in goliga-tests) ()
  (let* ((runde (list :before
                      (vector (make-instance 'goliga::mannschaft
                                             :kuerzel "foo")
                              (make-instance 'goliga::pre-round-event)
                              (make-instance 'goliga::mannschaft
                                             :kuerzel "bar"))
                      :in
                      (vector (goliga::parse-event 'goliga::begegnung
                                                   "foo"
                                                   "bar"
                                                   '(goliga::brett 0 0 "2:0")
                                                   '(goliga::brett 1 1 "0:2")
                                                   '(goliga::brett 2 2 "1:1")
                                                   '(goliga::brett 3 5 "2:!")))))
         (first-tabelle (goliga::next-tabelle nil runde))
         (next-tabelle (goliga::next-tabelle first-tabelle runde)))
    (is next-tabelle)
    ;; TODO: test more
    ))

(deftest (test-update-rows :in goliga-tests) ()
  (let* ((runde (list :before
                      (vector (make-instance 'goliga::mannschaft
                                             :kuerzel "foo")
                              (make-instance 'goliga::pre-round-event)
                              (make-instance 'goliga::mannschaft
                                             :kuerzel "bar"))))
         (begegnung (goliga::parse-event 'goliga::begegnung
                                         "foo"
                                         "bar"
                                         '(goliga::brett 0 0 "2:0")
                                         '(goliga::brett 1 1 "0:2")
                                         '(goliga::brett 2 2 "1:1")
                                         '(goliga::brett 3 5 "2:!")))
         (tabelle (goliga::next-tabelle nil runde))
         (before-left (gethash "foo"
                               (goliga::tabelle-mannschaft-values tabelle)))
         (before-right (gethash "bar"
                                (goliga::tabelle-mannschaft-values tabelle))))
    (multiple-value-bind (after-left after-right)
        (goliga::update-rows before-left before-right begegnung)
      (is (string= (goliga::table-row-mannschaft after-left) "foo"))
      (is (string= (goliga::table-row-mannschaft after-right) "bar"))
      (is (= (goliga::table-row-mp after-left) 2))
      (is (= (goliga::table-row-mp after-right) 0))
      (is (= (goliga::table-row-bp after-left) 5))
      (is (= (goliga::table-row-bp after-right) 3))
      (is (= (goliga::table-row-straf-bp after-right) 1))
      (is (position "foo" (goliga::table-row-gegner after-right)))
      (is (equalp #(:left) (goliga::table-row-farben after-left)))
      (is (equalp #(1 0 0 1) (goliga::table-row-wins after-left))))))

(deftest (test-penalty-points :in goliga-tests) ()
  (is (= (goliga::penalty-points nil 0) 0))
  (is (= (goliga::penalty-points t 0) 2))
  (is (= (goliga::penalty-points t 1) 1))
  (is (= (goliga::penalty-points t 4) 1)))

(deftest (test-calc-mp :in goliga-tests) ()
  (is (every #'=
             (multiple-value-list (goliga::calc-mp 4 4))
             (list 1 1)))
  (is (every #'=
             (multiple-value-list (goliga::calc-mp 4 3))
             (list 2 0)))
  (is (every #'=
             (multiple-value-list (goliga::calc-mp 0 8))
             (list 0 2))))

(deftest (test-calc-straf-mp :in goliga-tests) ()
  (is (= (goliga::calc-straf-mp 0) 0))
  (is (= (goliga::calc-straf-mp 3) 0))
  (is (= (goliga::calc-straf-mp 4) 1))
  (is (= (goliga::calc-straf-mp 5) 1))
  (is (= (goliga::calc-straf-mp 6) 2)))

(deftest (test-copy-table-row :in goliga-tests) ()
  (let* ((orig (make-instance 'goliga::table-row
                              :mannschaft "foo"
                              :mp 0
                              :bp 0
                              :straf-mp 0
                              :straf-bp 0
                              :gegner (make-array 10 :fill-pointer 0)
                              :farben (make-array 10 :fill-pointer 0)
                              :wins (make-array 4 :initial-element 0)))
         (copy (goliga::copy-table-row orig)))
    (is (= (goliga::table-row-mp copy) 0))
    (is (= (fill-pointer (goliga::table-row-gegner copy)) 0))
    (vector-push-extend "bar" (goliga::table-row-gegner copy))
    (is (= (fill-pointer (goliga::table-row-gegner copy)) 1))
    (is (= (fill-pointer (goliga::table-row-gegner orig)) 0))
    (vector-push-extend :left (goliga::table-row-farben copy))
    (is (= (fill-pointer (goliga::table-row-farben copy)) 1))
    (is (= (fill-pointer (goliga::table-row-farben orig)) 0))))
