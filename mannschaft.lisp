(in-package #:goliga)

(defclass mannschaft (pre-round-event)
  ((name :initarg :name
         :reader mannschaft-name)
   (kuerzel :initarg :kuerzel
            :reader mannschaft-kuerzel)
   (spieler :initform (make-array 10 :fill-pointer 0)
            :reader mannschaft-spieler)
   (staerke :accessor mannschaft-staerke)))

(defmethod parse-event ((event-name (eql 'mannschaft)) &rest args)
  (destructuring-bind (kuerzel name &rest spieler-data) args
    (let ((mannschaft (make-instance 'mannschaft
                                     :kuerzel kuerzel
                                     :name name)))
      (dolist (spieler-datum spieler-data)
        (vector-push-extend (parse-spieler spieler-datum)
                            (mannschaft-spieler mannschaft)))
      (setf (mannschaft-staerke mannschaft)
            (-<> (mannschaft-spieler mannschaft)
                 (subseq 0 5)
                 (map 'vector #'spieler-rang <>)
                 (map 'vector #'rang-int <>)
                 (reduce #'+ <>)))
      mannschaft)))
