;;; ur-game.lisp - Service layer of the Ur Game API.
;;
;; Manages the HTTP server, WebSocket server, and WebSocket message
;; interpretation.
(defpackage #:ur-game
  (:use #:cl
        #:ur-game.engine
        #:ur-game.json
        #:ur-game.session)
  (:export :start))

(in-package #:ur-game)



;; Logging (TODO: set level in configuration.)

(vom:config :ur-game :debug)

;; File Paths

(defun static-directory (app-root)
  (merge-pathnames #P"static/" app-root))

(defun index-filepath (app-root)
  (merge-pathnames #P"index.html" app-root))

;; Path Scanner

(defun make-path-scanner (path-regex)
  "Converts a path regex like /a/b/{something}/c into a regex that captures {...}'s and "
  (let* ((query-replaced-paths (cl-ppcre:regex-replace-all
                                 "{\\w*?}" path-regex "([^/]+?)"))
         (total-path-only (concatenate 'string
                                       "^" query-replaced-paths "$"))
         (scanner (cl-ppcre:create-scanner total-path-only)))
    (lambda (path-info)
      (multiple-value-bind (scanned-p groups)
        (cl-ppcre:scan-to-strings scanner path-info)

        (values scanned-p
                (and groups (coerce groups 'list)))))))

(defun route (path-regex routed-app)
  "Route call to another app on a matching path.

   Paths with a `{...}` in them are read as parameters, and passed into the
   routed app, e.g. an app routed by `/sessions/{token}` will be called with
   the app env and the token."
  (let ((scanner (make-path-scanner path-regex)))
    (lambda (app)
      (lambda (env)
        (multiple-value-bind (match-p arguments)
          (funcall scanner (getf env :path-info))

          (if match-p
              (apply routed-app env arguments)
              (funcall app env)))))))

(defun peek-request (peeking-app)
  (lambda (app)
    (lambda (env)
      (funcall peeking-app env)
      (funcall app env))))

(defun remote-addr (env)
  "Grab the IP from a reverse proxy's X-Real-IP header, or from the remote
   address"
  (or (gethash "x-real-ip" (getf env :headers))
      (getf env :remote-addr)))

(defun app (app-root)
  (lack:builder
    ;; === Middlewares ===
    (peek-request
      (lambda (env)
        (vom:debug1 "RECEIVED request ~S" env)
        (vom:info "~A ~S from ~A"
                  (getf env :request-method)
                  (getf env :request-uri)
                  (remote-addr env))))
    ;; === Websocket Routing ===
    ;; Route /wss/sessions to new-session websocket
    (route "/wss/sessions" 'session-app)
    ;; Route /wss/sessions/{token} to join-session websocket
    (route "/wss/sessions/{token}" 'session-app)
    ;; === Website Routing ===
    ;; Redirect /join/{token} to /#/{token}
    (route "/join/{token}"
           (lambda (env token)
             (declare (ignore env))
             `(302 (:location ,(concatenate 'string "/#/" token)))))
    ;; Serve "index.html" for "/".
    (route "/"
           (constantly `(200 (:content-type "text/html") ,(index-filepath app-root))))
    ;; Static Path
    (:static :path "/static/" :root (static-directory app-root))
    ;; All other routes are invalid.
    (constantly '(404 (:content-type "text/plain") ("Not Found")))))

(defvar *website-handler* nil)

(defun start (&key (app-root (asdf:system-source-directory :ur-game))
                   (port 5000)
                   (use-thread t))
  (when *website-handler* (clack:stop *website-handler*))
  (setf *website-handler*
        (clack:clackup (app app-root)
                       :server :hunchentoot
                       :port port
                       :use-thread use-thread))
  (values))
