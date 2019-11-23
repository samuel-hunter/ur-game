;;;; ur-game.asd

(asdf:defsystem #:ur-game
  :description "The Royal Game of Ur"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:cl-json
               #:hunchentoot
               #:hunchensocket
               #:uiop
               #:session-token)

  :components ((:file "json")
               (:file "engine" :depends-on ("json"))
               (:file "config")
               (:file "ur-game" :depends-on ("engine" "config" "json")))

  :build-operation "program-op"
  :build-pathname "ur-game"
  :entry-point "ur-game:main")
