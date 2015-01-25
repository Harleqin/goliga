(in-package #:goliga)

(defclass brett (round-event)
  ((left-spieler :initarg :left-spieler
                :reader brett-left-spieler)
   (right-spieler :initarg :right-spieler
                 :reader brett-right-spieler)
   (result :initarg :result
           :reader brett-result)
   (info :initarg :info
         :reader brett-info)))

(defun parse-brett (data)
  (destructuring-bind (symbol &optional left-index right-index result info) data
    (assert (eql symbol 'brett))
    (make-instance 'brett
                   :left-spieler left-index
                   :right-spieler right-index
                   :result (parse-result result)
                   :info info)))

(defclass result ()
  ((left-points :initarg :left-points
                :reader result-left-points)
   (right-points :initarg :right-points
                 :reader result-right-points)
   (left-penalty-p :initarg :left-penalty-p
                   :reader result-left-penalty-p)
   (right-penalty-p :initarg :right-penalty-p
                    :reader result-right-penalty-p)))

(defun parse-result (data &aux (result (string data)))
  (when data
    (destructuring-bind (left right) (split-sequence #\: result)
      (let ((left-penalty-p (position #\! left))
            (right-penalty-p (position #\! right)))
        (make-instance 'result
                       :left-points (if left-penalty-p
                                        0
                                        (parse-integer left))
                       :right-points (if right-penalty-p
                                         0
                                         (parse-integer right))
                       :left-penalty-p left-penalty-p
                       :right-penalty-p right-penalty-p)))))
