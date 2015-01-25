(in-package #:goliga)

(defclass begegnung (round-event)
  ((left :initarg :left :reader begegnung-left)
   (right :initarg :right :reader begegnung-right)
   (bretter :initform (make-array +bretter/begegnung+ :initial-element nil)
            :reader begegnung-bretter)))

(defmethod parse-event ((event-name (eql 'begegnung)) &rest args)
  (destructuring-bind (left right &rest brett-data) args
    ;; TODO: Mannschaftsnamen direkt auflösen?
    (let ((begegnung (make-instance 'begegnung
                                    :left left
                                    :right right)))
      (loop
        :for brett-datum :in brett-data
        :for i :below +bretter/begegnung+
        :do (setf (aref (begegnung-bretter begegnung) i)
                  (parse-brett brett-datum)))
      begegnung)))
