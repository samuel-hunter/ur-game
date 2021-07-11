(defpackage #:ur-game.json
  (:use :cl)
  (:import-from :json
                :encode-json
                :*json-output*)
  (:export
   :json-bool
   :json-bool-p))

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
