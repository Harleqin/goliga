(in-package #:goliga)

(defun enter-begegnungen-interactive (liga runde)
  (let ((mannschafts-kuerzel (hash-table-keys (aref (liga-mannschaften liga)
                                                    runde))))
    (flet ((parse-mannschaft (kuerzel)
             (gethash kuerzel (aref (liga-mannschaften liga) runde))))
      (format *query-io* "~{ ~a~%~}~%"
              (loop
                :for k :in (sort mannschafts-kuerzel #'string-lessp)
                :collect k))
      (loop
        :for line := (progn
                       (format *query-io* "Mannschaften> ")
                       (read-line *query-io*))
        :while (plusp (length line))
        :collect (register-groups-bind ((#'parse-mannschaft left right))
                     ("(\\S+)\\s+(\\S+)" line)
                   (loop
                     :for (left-spieler . right-spieler)
                     :in (zip (mannschaft-spieler left)
                              (mannschaft-spieler right))
                     :for i :upfrom 0
                     :do (format *query-io*
                                 "~4a~28a~8@a    ~28a~8@a~%"
                                 i
                                 (format-spieler-name left-spieler)
                                 (format-spieler-rang left-spieler)
                                 (format-spieler-name right-spieler)
                                 (format-spieler-rang right-spieler)))
                   (list* 'begegnung
                          (mannschaft-kuerzel left)
                          (mannschaft-kuerzel right)
                          (loop
                            :repeat +bretter/begegnung+
                            :for line := (split "\\s+" (read-line *query-io*)
                                                :limit 4)
                            :collect (list* 'brett
                                            (maybe-parse-integer (first line))
                                            (maybe-parse-integer (second line))
                                            (nthcdr 2 line)))))))))

(defun edit-begegnung-interactive (liga runde)
  (let ((begegnungen (liga-begegnungen liga runde)))
    (map nil
         (lambda (begegnung i)
           (format *query-io*
                   "~a: ~a – ~a~%"
                   i
                   (begegnung-left begegnung)
                   (begegnung-right begegnung)))
         begegnungen
         (iota (length begegnungen)))))

(defun format-spieler-name (spieler)
  (if spieler
      (spieler-name spieler)
      ""))

(defun format-spieler-rang (spieler)
  (if spieler
      (format-rang (spieler-rang spieler))
      ""))

(defun maybe-parse-integer (n-or-nil)
  (when (and n-or-nil
             (plusp (length n-or-nil))
             (digit-char-p (aref n-or-nil 0)))
    (parse-integer n-or-nil)))
