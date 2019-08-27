;;;; ur-game.asd

(asdf:defsystem #:ur-game
  :description "The Royal Game of Ur"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (:alexandria
               :cl-json
               :hunchentoot
               :hunchensocket
               :session-token)
  :components ((:file "engine")
               (:file "repl" :depends-on ("engine"))
               (:file "ur-game" :depends-on ("engine"))))
