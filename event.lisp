(in-package #:goliga)

(defclass event () ())

(defclass pre-round-event (event) ())

(defclass round-event (event) ())

(defun %enter-event (liga round event phase)
  (vector-push-extend event
                      (getf (aref (liga-runden liga) round) phase)))

(defgeneric enter-event (liga round event)
  (:documentation "Adds an event to the liga in round round."))

(defmethod enter-event ((liga-name symbol) round event)
  (let ((liga (find-liga liga-name)))
    (assert liga (liga-name) "Liga nicht gefunden: ~s" liga-name)
    (enter-event liga round event)))

(defmethod enter-event ((liga liga) (round integer) (event pre-round-event))
  (%enter-event liga round event :before))

(defmethod enter-event ((liga liga) (round integer) (event round-event))
  (%enter-event liga round event :in))

(defgeneric parse-event (name &rest args)
  (:documentation "Parses a form containing data to create an event."))
