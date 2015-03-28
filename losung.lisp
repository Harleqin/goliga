(in-package #:goliga)

(defun rate-pair (a b)
  "Returns three values: the mannschaft name of the left mannschaft, the
mannschaft name of the right mannschaft, and a rating for this pairing, the
higher the better, nil if they already have played each other."
  (check-type a table-row)
  (check-type b table-row)
  (when (or (position (table-row-mannschaft a) (table-row-gegner b) :test #'equal)
            (position (table-row-mannschaft b) (table-row-gegner a) :test #'equal))
    (return-from rate-pair nil))
  (multiple-value-bind (left right) (determine-farben a b)
    (values (table-row-mannschaft left)
            (table-row-mannschaft right)
            (+ (power-close (effective-mp left)
                            (effective-mp right)
                            :power 2
                            :mult 500)
               (power-close (effective-bp left)
                            (effective-bp right)
                            :power 2
                            :mult 10)
               (rate-farbe left :left 2)
               (rate-farbe right :right 2)))))

(defun determine-farben (a b)
  (let* ((a-diff (farb-diff a))
         (b-diff (farb-diff b))
         (diff (- a-diff b-diff)))
    (cond ((plusp diff) (values b a))
          ((minusp diff) (values a b))
          (t (values-list (shuffle (list a b)))))))

(defun farb-diff (table-row)
  "Returns the farb difference of the table row.  Negative if :right is more
often than :left."
  (- (count :left (table-row-farben table-row))
     (count :right (table-row-farben table-row))))

(defun power-close (left-points right-points &key power mult)
  "Returns a value that is higher if the numbers are close."
  (- (* mult
        (expt (abs (- left-points right-points))
              power))))

(defun rate-farbe (table-row farbe mult)
  "Returns a value that is higher if the farbe is less often in the
table-row-farben."
  (let ((farben (table-row-farben table-row)))
    (* mult (- (length farben)
               (* 2 (count farbe farben))))))

(defgeneric calc-combos (liga runde)
  (:method ((liga liga) (runde integer))
    "Returns two values: first a list of three-element lists containing an index
for the left, an index for the right and the weight (the smaller the better);
second a vector with mannschaft names at the used indices."
    (let* ((table-rows (hash-table-values
                        (tabelle-mannschaft-values (liga-tabelle liga runde))))
           (mannschaften (map 'vector #'table-row-mannschaft table-rows))
           (combos (loop
                     :for a :in table-rows
                     :for bs :on (rest table-rows)
                     :append (loop
                               :for b :in bs
                               :collect (multiple-value-bind (left right rating)
                                            (rate-pair a b)
                                          (when rating
                                            (list (position left mannschaften
                                                            :test #'equal)
                                                  (position right mannschaften
                                                            :test #'equal)
                                                  rating))))))
           (valid-combos (remove-if #'null combos))
           (normalized-combos (normalize-weights valid-combos)))
      (values normalized-combos mannschaften))))

(defun normalize-weights (combos)
  (let ((base (1+ (loop
                    :for combo :in combos
                    :minimize (third combo)))))
    (mapcar (lambda (combo)
              (list (first combo)
                    (second combo)
                    (- (third combo) base)))
            combos)))

(defun make-perl-input (combos)
  (format nil "[~{[~{~a~^,~}]~^,~}]" combos))

(defun parse-perl-output (stream)
  (loop
    :for line := (read-line stream nil)
    :while line
    :when (find #\, line :test #'equal)
    :collect (mapcar #'parse-integer
                     (split-sequence #\, line
                                     :test #'equal))))

(defparameter *perl-lib* "-Ipl/lib")

(defparameter *perl-match* "pl/mwmatch.pl")

(defun make-losung (liga runde)
  (multiple-value-bind (combos mannschaften) (calc-combos liga runde)
    (let* ((perl-input (make-perl-input combos))
           (process (sb-ext:run-program "perl"
                                        (list *perl-lib*
                                              *perl-match*
                                              perl-input)
                                        :search t
                                        :output :stream)))
      (if (zerop (sb-ext:process-exit-code process))
          (with-open-stream (perl-out (sb-ext:process-output process))
            (let ((pairs (parse-perl-output perl-out)))
              (mapcar (lambda (pair)
                        (mapcar (lambda (n)
                                  (aref mannschaften n))
                                pair))
                      pairs)))
          (with-open-stream (perl-err (sb-ext:process-error process))
            (loop
              :for line := (read-line perl-err nil)
              :while line
              :do (print line *error-output*))
            (finish-output))))))

(defun losung-begegnungen (losung)
  (mapcar (lambda (pair)
            (cons 'begegnung pair))
          losung))

(defun print-losung (losung liga runde &optional (stream *standard-output*))
  (mapcar (lambda (pair)
            (format stream "~{~a – ~a~%~}"
                    (mapcar (lambda (kuerzel)
                              (mannschaft-name
                               (gethash kuerzel
                                        (aref (liga-mannschaften liga)
                                              runde))))
                            pair)))
          losung))

(defun compile-losung (liga-file runde)
  (let* ((liga (load-data liga-file))
         (losung (make-losung liga runde)))
    (print-liga-tabelle liga runde)
    (print-losung losung liga runde)
    (losung-begegnungen losung)))
