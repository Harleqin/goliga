(in-package #:goliga)

(defclass spieler ()
  ((name :initarg :name
         :reader spieler-name)
   (rang :initarg :rang
         :reader spieler-rang)))

(defun parse-spieler (data)
  (destructuring-bind (symbol name rang) data
    (assert (eq symbol 'spieler))
    (assert (stringp name))
    (assert (stringp rang))
    (make-instance 'spieler
                   :name name
                   :rang (make-instance 'rang :string rang))))
