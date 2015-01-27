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
           :initarg :gegner
           :reader table-row-gegner)
   (farben :initform (make-array 10 :adjustable t :fill-pointer 0)
           :initarg :farben
           :reader table-row-farben)
   (wins :initform (make-array +bretter/begegnung+ :initial-element 0)
         :initarg :wins
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
    (dovector (event (getf events :before) tabelle)
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
    (dovector (event (getf events :in) tabelle)
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
  (let ((left (copy-table-row before-left))
        (right (copy-table-row before-right)))
    (loop
      :for brett :across (begegnung-bretter begegnung)
      :for i :below +bretter/begegnung+
      :for result := (brett-result brett)
      :sum (result-left-points result) :into left-bp
      :sum (result-right-points result) :into right-bp
      :sum (penalty-points (result-left-penalty-p result) i) :into left-sbp
      :sum (penalty-points (result-right-penalty-p result) i) :into right-sbp
      :do (cond ((> (result-left-points result) (result-right-points result))
                 (incf (aref (table-row-wins left) i)))
                ((> (result-right-points result) (result-left-points result))
                 (incf (aref (table-row-wins right) i))))
      :finally
      (multiple-value-bind (left-mp right-mp) (calc-mp left-bp right-bp)
        (incf (table-row-mp left) left-mp)
        (incf (table-row-bp left) left-bp)
        (incf (table-row-mp right) right-mp)
        (incf (table-row-bp right) right-bp)
        (incf (table-row-straf-bp left) left-sbp)
        (incf (table-row-straf-bp right) right-sbp)
        (setf (table-row-straf-mp left)
              (calc-straf-mp (table-row-straf-bp left))
              (table-row-straf-mp right)
              (calc-straf-mp (table-row-straf-bp right)))
        (vector-push-extend (table-row-mannschaft left)
                            (table-row-gegner right))
        (vector-push-extend (table-row-mannschaft right)
                            (table-row-gegner left))
        (vector-push-extend :left (table-row-farben left))
        (vector-push-extend :right (table-row-farben right))))
    (values left right)))

(defun penalty-points (penalty-p brett-n)
  (* (if penalty-p 1 0)
     (if (zerop brett-n) 2 1)))

(defun calc-mp (left-bp right-bp)
  (let ((sign (signum (- left-bp right-bp))))
    (values (1+ sign) (1+ (- sign)))))

(defparameter *straf-mp-thresholds* (list 4 2))

(defun calc-straf-mp (bp)
  (loop
    :for threshold :from (first *straf-mp-thresholds*)
                   :by (second *straf-mp-thresholds*)
    :while (>= bp threshold)
    :count t))

(defun copy-table-row (row)
  (make-instance 'table-row
                 :mannschaft (table-row-mannschaft row)
                 :mp (table-row-mp row)
                 :bp (table-row-bp row)
                 :straf-mp (table-row-straf-mp row)
                 :straf-bp (table-row-straf-bp row)
                 :gegner (copy-array (table-row-gegner row))
                 :farben (copy-array (table-row-farben row))
                 :wins (copy-array (table-row-wins row))))
