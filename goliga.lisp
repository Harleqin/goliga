(in-package #:goliga)

(defun load-data (datafile)
  (with-open-file (data-in datafile
                           :direction :input
                           :if-does-not-exist :error)
    (let ((*read-eval* nil))
      (enter-liga (read data-in)))))

(defun enter-liga (liga-data)
  (destructuring-bind (symbol name description n-rounds &rest runden) liga-data
    (assert (eq symbol 'liga))
    (assert (symbolp name))
    (assert (stringp description))
    (assert (typep n-rounds '(integer 0 *)))
    (let ((liga (register-liga name description n-rounds)))
      (loop
        :for runde-data :in runden
        :for i :below n-rounds
        :do (enter-runde liga i runde-data))
      liga)))

(defun enter-runde (liga i runde-data)
  (destructuring-bind (symbol &rest event-data) runde-data
    (assert (eq symbol 'runde))
    (dolist (event-datum event-data)
      (enter-event liga i (apply #'parse-event event-datum)))))
