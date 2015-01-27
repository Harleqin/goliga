(in-package #:goliga)

(defmacro dovector ((var vector &optional return-form) &body body)
  `(loop
     :for ,var :across ,vector
     :do ,@body
     :finally (return ,return-form)))
