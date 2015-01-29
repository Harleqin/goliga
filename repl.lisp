(in-package #:goliga)

(defun begegnung-interactive (liga runde)
  (let ((mannschafts-kuerzel (hash-table-keys (aref (liga-mannschaften liga)
                                                    runde))))
    (flet ((parse-mannschaft (kuerzel)
             (gethash kuerzel (aref (liga-mannschaften liga) runde))))
      (format *query-io* "~{ ~a~%~}~%"
              (loop
                :for k :in (sort mannschafts-kuerzel #'string-lessp)
                :collect k))
      (format *query-io* "Mannschaften> ")
      (let ((line (read-line *query-io*)))
        (register-groups-bind ((#'parse-mannschaft left right))
            ("(\\S+)\\s+(\\S+)" line)
          (format *query-io* "~a ~a~%" left right))))))
