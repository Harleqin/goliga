(in-package #:goliga)

(defclass liga ()
  ((name :initarg :name
         :reader liga-name)
   (description :initarg :description
                :reader liga-description)
   (mannschaften :initform (make-hash-table :test #'equal)
                 :reader liga-mannschaften)
   (n-runden :initarg :n-runden)
   (runden :reader liga-runden)
   (tabellen :reader tabellen)
   (tabellen-dirty :reader tabellen-dirty)))

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
        (slot-value liga 'tabellen)
        (make-array (1+ (slot-value liga 'n-runden)))
        (slot-value liga 'tabellen-dirty)
        (make-array (1+ (slot-value liga 'n-runden))
                    :initial-element nil)))

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
