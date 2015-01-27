(in-package #:goliga)

(defclass liga ()
  ((name :initarg :name
         :reader liga-name)
   (description :initarg :description
                :reader liga-description)
   (n-runden :initarg :n-runden)
   (runden :reader liga-runden)
   (mannschaften :reader liga-mannschaften)
   (tabellen :reader liga-tabellen)))

(defmethod initialize-instance :after ((liga liga)
                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value liga 'runden)
        (map-into (make-array (slot-value liga 'n-runden))
                  (lambda ()
                    (list :before
                          (make-array 10
                                      :adjustable t
                                      :fill-pointer 0)
                          :in
                          (make-array 10
                                      :adjustable t
                                      :fill-pointer 0))))
        (slot-value liga 'mannschaften)
        (map-into (make-array (slot-value liga 'n-runden))
                  (lambda ()
                    (make-hash-table :test #'equal))))
  (reset-tabellen liga))

(defgeneric reset-tabellen (liga)
  (:method ((liga liga))
    (setf (slot-value liga 'tabellen)
          (make-array (slot-value liga 'n-runden)))))

(defvar *ligen* (make-array 10
                            :adjustable t
                            :fill-pointer 0))

(defun register-liga (name description n-runden)
  (let ((liga (make-instance 'liga
                             :name name
                             :description description
                             :n-runden n-runden))
        (position (position name *ligen* :key #'liga-name)))
    (if position
        (setf (aref *ligen* position) liga)
        (vector-push-extend liga *ligen*))
    liga))

(defun find-liga (name)
  (find name *ligen* :key #'liga-name))

(defun delete-liga (name)
  (let ((i (position name *ligen* :key #'liga-name)))
    (when i
      (shiftf (aref *ligen* i)
              (aref *ligen* (1- (fill-pointer *ligen*)))
              nil)
      (decf (fill-pointer *ligen*)))))

(defun update-tabellen (liga)
  (reset-tabellen liga)
  (setf (aref (liga-tabellen liga) 0)
        (next-tabelle nil (getf (aref (liga-runden liga) 0) :before)))
  (loop
    :for from :upfrom 0
    :for to :upfrom 1
    :for tabelle := (aref (liga-tabellen liga) from)
    :do (setf (aref (liga-tabellen liga) to)
              (next-tabelle tabelle (getf (aref (liga-runden liga) from) :in))
              (aref (liga-mannschaften liga) to)
              (next-mannschaften liga from to))))

(defun next-mannschaften (liga from to)
  (let ((next (copy-hash-table (aref (liga-mannschaften liga) from)))
        (m-events (getf (aref (liga-runden liga) to) :before)))
    (dovector (m m-events next)
      (when (typep m 'mannschaft)
        (setf (gethash (mannschaft-kuerzel m) next) m)))))
