(in-package #:goliga)

(defun enter-begegnungen-interactive (liga runde)
  (let ((mannschafts-kuerzel (liga-runde-mannschafts-kuerzel liga
                                                             runde)))
    (flet ((parse-mannschaft (kuerzel)
             (liga-runde-mannschaft liga runde kuerzel)))
      (format *query-io* "~{ ~a~%~}~%"
              (loop :for k :in (sort mannschafts-kuerzel #'string-lessp)
                    :collect k))
      (loop :for line := (progn
                           (format *query-io* "Mannschaften> ")
                           (read-line *query-io*))
            :while (plusp (length line))
            :collect (register-groups-bind ((#'parse-mannschaft left right))
                         ("(\\S+)\\s+(\\S+)" line)
                       (loop :for (left-spieler . right-spieler)
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
                              (loop :repeat +bretter/begegnung+
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
             (plusp (length n-or-nil)))
    (cond ((digit-char-p (aref n-or-nil 0))
           (parse-integer n-or-nil))
          ((string-equal n-or-nil "t")
           t)
          (t
           nil))))

(defun format-mannschaft (liga runde kuerzel)
  (let ((mannschaft (liga-runde-mannschaft liga runde kuerzel)))
    (with-output-to-string (s)
      (loop :for spieler :across (mannschaft-spieler mannschaft)
            :for i :upfrom 0
            :do (format s "~a ~a ~a~%"
                        i
                        (format-spieler-name spieler)
                        (format-spieler-rang spieler))))))

;;; Saisonvorbereitung

(defun read-mannschaften-csv (filename)
  "A parser for some dirty csv files generated from the dirty, dirty xlsx files
on the Bundesliga page."
  (flet ((start-of-mannschaft (line)
           (find #\. (first line))))
    (let ((csv (member "Aufstellungen"
                       (fare-csv:read-csv-file filename :external-format :latin1)
                       :key #'first
                       :test #'equal)))
      (loop :for mannschaften
              := (member-if #'start-of-mannschaft csv)
              :then (member-if #'start-of-mannschaft (rest mannschaften))
            :while mannschaften
            :collect (read-mannschaft mannschaften)))))

(defun read-mannschaft (lol)
  (list* 'mannschaft
         (cadar lol)
         (loop :for (n name nil nil nil rank) :in (rest lol)
               :until (find #\. n)
               :if (string/= n "")
                 :collect (list 'spieler
                                (munge-name name)
                                (munge-rank rank)))))

(defun munge-name (raw)
  (if-let ((i (position #\, raw)))
    (concatenate 'string
                 (subseq raw (+ i 2))
                 '(#\space)
                 (subseq raw 0 i))
    raw))

(defun munge-rank (raw)
  (format-rang (parse-rang raw)))

(defun read-mannschaften-data (filename)
  (with-open-file (in filename
                      :direction :input)
    (let ((*read-eval* nil))
      (destructuring-bind (symbol name description n-rounds &rest runden)
          (read in)
        (declare (ignore symbol name description n-rounds))
        (mapcan (lambda (runde)
                  (remove 'mannschaft (rest runde)
                          :key #'first
                          :test-not #'eq))
                runden)))))

(defun rate-mannschaft (m)
  (destructuring-bind (nil id name . spieler) m
    (list id
          name
          (-<>> spieler
                (subseq <> 0 5)
                (mapcar #'third)
                (mapcar #'parse-rang)
                (mapcar #'rang-int)
                (reduce #'+)))))

(defun ordered-mannschaften (ms)
  (-<>> ms
        (mapcar #'rate-mannschaft)
        (sort <> #'< :key #'third)))

(defun falt-paare (ms)
  (assert (evenp (length ms)))
  (let ((l (/ (length ms) 2)))
    (loop :for a :in ms
          :and b :in (reverse ms)
          :repeat l
          :collect (shuffle (list a b)))))

(defun data-out (paare)
  (mapcar (lambda (paar)
            (list* 'begegnung (mapcar #'first paar)))
          paare))

(defun email-out (paare)
  (format t "~{~{~a — ~a~}~%~}"
          (mapcar (lambda (paar)
                    (mapcar #'second paar))
                  paare)))

(defun gather-emails (csv-file)
  (let* ((csv (fare-csv:read-csv-file csv-file :external-format :latin-1))
         (liga-info (member "5.Liga" csv
                            :test #'equal
                            :key #'second))
         (email-column (mapcar #'sixth liga-info)))
    (remove-if-not (lambda (cell)
                     (find #\@ cell))
                   email-column)))

(defun format-aliases (alias emails &optional (stream t))
  (format stream "alias ~a ~{~a \\~%          ~}" alias emails))
