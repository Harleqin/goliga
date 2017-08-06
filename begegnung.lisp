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
      (loop :for brett-datum :in brett-data
            :for i :below +bretter/begegnung+
            :do (setf (aref (begegnung-bretter begegnung) i)
                      (parse-brett brett-datum)))
      begegnung)))

(defun begegnung-punkte (begegnung)
  "Return two values: the brettpunkte of the left and right mannschaft."
  (let ((results (remove nil
                         (map 'list
                              #'brett-result
                              (remove nil
                                      (begegnung-bretter begegnung))))))
    (values (reduce #'+
                    (remove nil (mapcar #'result-left-points results))
                    :initial-value 0)
            (reduce #'+
                    (remove nil (mapcar #'result-right-points results))
                    :initial-value 0))))

(defun format-begegnung-lmo (begegnung mannschaften &key asciip)
  (let* ((left-kuerzel (begegnung-left begegnung))
         (right-kuerzel (begegnung-right begegnung))
         (left (gethash left-kuerzel mannschaften))
         (right (gethash right-kuerzel mannschaften))
         (output (format nil
                         "~a | ~a — ~a\\n\\n~{~a~^\\n~}"
                         (multiple-value-call #'format-begegnung-result-lmo
                           (begegnung-punkte begegnung))
                         (format-kuerzel-lmo left-kuerzel)
                         (format-kuerzel-lmo right-kuerzel)
                         (map 'list
                              (lambda (brett)
                                (format-brett-lmo brett left right))
                              (begegnung-bretter begegnung))))
         (maybe-ascii-escaped-output (if asciip
                                         (ascii-escape output)
                                         output)))
    (if (> (length maybe-ascii-escaped-output) (+ 255 6))
        (concatenate 'string maybe-ascii-escaped-output " *ZU LANG*")
        maybe-ascii-escaped-output)))

(defun ascii-escape (string)
  (reduce (lambda (input re-replacement)
            (regex-replace-all (car re-replacement)
                               input
                               (cdr re-replacement)
                               :preserve-case t))
          '(("ä" . "ae")
            ("ö" . "oe")
            ("ü" . "ue")
            ("ß" . "ss")
            ("è" . "e")
            ("é" . "e")
            ("—" . "-")
            ("„" . "\"")
            ("“" . "\"")
            ("”" . "\""))
          :initial-value string))

(defun format-begegnung-result-lmo (left right)
  (format nil "~a:~a" left right))

(defun format-kuerzel-lmo (kuerzel)
  (if (search "[]" kuerzel)
      kuerzel
      (concatenate 'string kuerzel "[1-4]")))
