(in-package #:goliga/tests)

(deftest (test-register-liga :in goliga-tests) ()
  (let ((goliga::*ligen* (make-array 10
                                     :adjustable t
                                     :fill-pointer 0)))
    (goliga::register-liga 'bltest "Test-Liga" 5)
    (is (= (fill-pointer goliga::*ligen*) 1))
    (let ((liga (aref goliga::*ligen* 0)))
      (is (typep liga 'goliga::liga))
      (is (eq (goliga::liga-name liga) 'bltest))
      (is (string= (goliga::liga-description liga) "Test-Liga"))
      (is (= (length (goliga::liga-runden liga)) 5)))))

(deftest (test-find-liga :in goliga-tests) ()
  (let ((goliga::*ligen* (make-array 10
                                     :adjustable t
                                     :fill-pointer 0)))
    (goliga::register-liga 'bltest "Test-Liga" 5)
    (let ((found (goliga::find-liga 'bltest)))
      (is found))))

(deftest (test-delete-liga :in goliga-tests) ()
  (let ((goliga::*ligen* (make-array 10
                                     :adjustable t
                                     :fill-pointer 0)))
    (goliga::register-liga 'bltest "Test-Liga" 5)
    (let ((found (goliga::find-liga 'bltest)))
      (is found))
    (goliga::delete-liga 'bltest)
    (let ((found (goliga::find-liga 'bltest)))
      (is (not found))
      (is (zerop (fill-pointer goliga::*ligen*))))))
