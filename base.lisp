(in-package #:goliga)

(defmacro dovector ((var vector &optional return-form) &body body)
  `(loop :for ,var :across ,vector
         :do ,@body
         :finally (return ,return-form)))

(defun zip (seq0 seq1
            &key
              innerp
            &aux
              (l (funcall (if innerp #'min #'max) (length seq0) (length seq1)))
              (a (copy-seq-expand seq0 l))
              (b (copy-seq-expand seq1 l)))
  (map 'list #'cons a b))

(defun copy-seq-expand (seq length)
  (map-into (make-sequence (class-of seq)
                           length
                           :initial-element nil)
            #'identity
            seq))
