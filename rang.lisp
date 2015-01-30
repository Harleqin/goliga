(in-package #:goliga)

(defclass rang ()
  ((int :reader rang-int
        :documentation "1 dan is 0, dans positive, kyus negative")
   (string :initarg :string)))

(defmethod initialize-instance :after ((rang rang)
                                       &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (let ((danp (position #\d (slot-value rang 'string) :test #'char-equal))
        (n (parse-integer (slot-value rang 'string) :junk-allowed t)))
    (setf (slot-value rang 'int)
          (if danp
              (1- n)
              (- n)))))

(defun parse-rang (string)
  (make-instance 'rang :string string))

(defun rang= (&rest raenge)
  (every #'= (mapcar #'rang-int raenge)))

(defun format-rang (rang)
  (if (minusp (rang-int rang))
      (format nil "~a kyu"
              (- (rang-int rang)))
      (format nil "~a dan"
              (1+ (rang-int rang)))))
