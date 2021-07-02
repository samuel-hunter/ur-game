;;;; ur-game.asd

(asdf:defsystem #:ur-game
  :description "The Royal Game of Ur"
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:cl-json
               #:cl-ppcre
               #:clack
               #:session-token
               #:uiop
               #:vom
               #:websocket-driver)

  :pathname "src"
  :components ((:file "json")
               (:file "engine")
               (:file "ur-game" :depends-on ("engine" "json"))))
