(in-package #:goliga)

(defgeneric format-html (obj))

(defmethod format-html ((liga liga))
  (with-html-output-to-string (*standard-output*)
    (:div :class "liga"
          (:h2 (esc (string (liga-name liga))))
          (:p (esc (liga-description liga)))
          (dovector (runde (liga-runden liga))
            (htm ".")))))
