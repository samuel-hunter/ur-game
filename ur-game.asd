;;;; ur-game.asd

(asdf:defsystem #:ur-game
  :description "The Royal Game of Ur Web Application"
  :author "Samuel Hunter <shunter.xyz>"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:cl-json
               #:cl-ppcre

               #:clack
               #:clack-handler-hunchentoot
               #:lack
               #:lack-middleware-static

               #:session-token
               #:uiop
               #:vom
               #:websocket-driver)

  :pathname "src"
  :components ((:file "json")
               (:file "engine")
               (:file "ur-game" :depends-on ("engine" "json"))))
