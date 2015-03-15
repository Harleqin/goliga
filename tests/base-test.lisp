(in-package #:goliga/tests)

(deftest (test-dovector :in goliga-tests) ()
  (let ((receiver '()))
    (is (equal (goliga::dovector (e #(1 2 3) receiver)
                 (push e receiver))
               (list 3 2 1)))))

(deftest (test-copy-seq-expand :in goliga-tests) ()
  (is (equalp (goliga::copy-seq-expand #(1 2 3) 5)
              #(1 2 3 nil nil)))
  (is (equalp (goliga::copy-seq-expand #("a" "b") 1)
              #("a"))))

(deftest (test-zip :in goliga-tests) ()
  (is (equalp (goliga::zip #(1 2 3) #("a" "b" "c" "d"))
              '((1 . "a") (2 . "b") (3 . "c") (nil . "d"))))
  (is (equalp (goliga::zip #(1 2 3) #("a" "b" "c" "d") :innerp t)
              '((1 . "a") (2 . "b") (3 . "c")))))
