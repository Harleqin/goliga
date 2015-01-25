(in-package #:goliga)

(defclass tabelle ()
  ((mannschaft-values :initform (make-hash-table :test #'equal)
                      :reader tabelle-mannschaft-values
                      :documentation
                      "A table of mannschaft-kuerzel to table-row")))

(defclass table-row ()
  ((mannschaft :initarg :mannschaft
               :reader table-row-mannschaft
               :documentation "Mannschaft-kuerzel")
   (mp :initarg :mp
       :initform 0
       :accessor table-row-mp)
   (bp :initarg :bp
       :initform 0
       :accessor table-row-bp)
   (straf-mp :initarg :straf-mp
             :initform 0
             :accessor table-row-straf-mp)
   (straf-bp :initarg :straf-bp
             :initform 0
             :accessor table-row-straf-bp)
   (gegner :initform (make-array 10 :adjustable t :fill-pointer 0)
           :reader table-row-gegner)
   (farben :initform (make-array 10 :adjustable t :fill-pointer 0)
           :reader table-row-farben)
   (wins :initform (make-array +bretter/begegnung+ :initial-element 0)
         :reader table-row-wins
         :documentation "Count the number of wins per brett"))
  (:documentation "The status of a Mannschaft after a certain round."))

(defgeneric next-tabelle (before events)
  (:documentation "Calculates the tabelle based on the tabelle before and the
events since then."))

(defmethod next-tabelle ((before null) events)
  "In this case, we are before the first round.  Events are the events of the
first round, we only need the Mannschaften."
  (let ((tabelle (make-instance 'tabelle)))
    (dolist (event (getf events :before) tabelle)
      (when (typep event 'mannschaft)
        (setf (gethash (mannschaft-kuerzel event)
                       (tabelle-mannschaft-values tabelle))
              (make-instance 'table-row
                             :mannschaft (mannschaft-kuerzel event)))))))

(defmethod next-tabelle ((before tabelle) events
                         &aux (before-ht (tabelle-mannschaft-values before)))
  "In this case there is a previous tabelle.  We can assume that no more
Mannschaften will be added, so we need only results."
  (let* ((tabelle (make-instance 'tabelle))
         (tabelle-ht (tabelle-mannschaft-values tabelle)))
    (dolist (event (getf events :in) tabelle)
      (when (typep event 'begegnung)
        (let* ((left-name (begegnung-left event))
               (right-name (begegnung-right event))
               (before-left (gethash left-name before-ht))
               (before-right (gethash right-name before-ht)))
          (assert before-left)
          (assert before-right)
          (multiple-value-bind (left right)
              (update-rows before-left before-right event)
            (setf (gethash left-name tabelle-ht) left
                  (gethash right-name tabelle-ht) right)))))))

(defun update-rows (before-left before-right begegnung)
  (let ((left (make-instance 'table-row
                             :mannschaft (table-row-mannschaft before-left)))
        (right (make-instance 'table-row
                              :mannschaft (table-row-mannschaft before-right))))
    ))
