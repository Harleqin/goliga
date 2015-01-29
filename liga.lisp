(in-package #:goliga)

(defclass liga ()
  ((name :initarg :name
         :reader liga-name)
   (description :initarg :description
                :reader liga-description)
   (n-runden :initarg :n-runden
             :reader liga-n-runden)
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
          (make-array (1+ (slot-value liga 'n-runden))))))

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
        (next-tabelle nil (aref (liga-runden liga) 0)))
  (loop
    :for from :upfrom 0
    :for to :upfrom 1 :below (1+ (liga-n-runden liga))
    :do (setf (aref (liga-tabellen liga) to)
              (next-tabelle (aref (liga-tabellen liga) from)
                            (aref (liga-runden liga) from)))))

(defun update-mannschaften (liga)
  (loop
    :for i :upfrom 0 :below (liga-n-runden liga)
    :do (setf (aref (liga-mannschaften liga) i)
              (next-mannschaften liga :from (1- i) :to i))))

(defun next-mannschaften (liga &key from to)
  (let ((next (if (not (minusp from))
                  (copy-hash-table (aref (liga-mannschaften liga) from))
                  (make-hash-table :test #'equal)))
        (m-events (getf (aref (liga-runden liga) to) :before)))
    (dovector (m m-events next)
      (when (typep m 'mannschaft)
        (setf (gethash (mannschaft-kuerzel m) next) m)))))
