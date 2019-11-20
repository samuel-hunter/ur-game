(defpackage #:ur-game.config
  (:use :cl)
  (:import-from :uiop
                :getenv)
  (:import-from :alexandria
                :if-let)
  (:export :config))

(in-package #:ur-game.config)

;; *config* is an a-list of alists, where the keys of the toplevel is
;; the execution enviornment (production, dev etc) and the keys of the
;; next alists are config variables.
(defparameter *config*
  '((:common (:http-port . 8080) (:debug . t))
    (:prod (:ws-port . 8082) (:debug . nil))
    (:dev (:ws-port . 8081))))

(defun exec-environment ()
  (if (string-equal (getenv "PRODUCTION")
                    "1")
      :prod
      :dev))

(defun config-environment (environment)
  (cdr (assoc environment *config*)))

(defun config (key)
  (if-let (result (assoc key (config-environment (exec-environment))))
    (cdr result)
    (cdr (assoc key (config-environment :common)))))
