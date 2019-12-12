(defpackage #:ur-game.json
  (:use :cl)
  (:import-from :json
                :encode-json
                :*json-output*)
  (:export
   :json-bool
   :json-bool-p
   :encode-json-select-slots))

(in-package #:ur-game.json)

(defclass json-bool ()
  ((p :initarg :p :initform nil :reader json-bool-p)
   (generalised :initarg :generalised :initform nil))
  (:documentation "An object specifically made to be json-encoded to either true or false."))

(defmethod encode-json ((object json-bool) &optional (stream *json-output*))
  (with-slots (p generalised) object
    (cond
      ((and p generalised) (encode-json p stream))
      (p (princ "true" stream))
      (t (princ "false" stream)))))

(defun encode-json-select-slots (object slots &optional (stream json:*json-output*))
  "Encode a JSON object with the chosen select slots"
  (json:with-object (stream)
    (dolist (slot slots)
      (json:encode-object-member slot (slot-value object slot) stream))))
