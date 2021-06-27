;;; ur-game.lisp - Service layer of the Ur Game API.
;;
;; Manages the HTTP server, WebSocket server, and WebSocket message
;; interpretation.
(defpackage #:ur-game
  (:use :cl :ur-game.engine :ur-game.json)
  (:import-from :alexandria
                :curry
                :when-let
                :if-let
                :switch
                :eswitch)
  (:import-from :json
                :encode-json-plist-to-string
                :decode-json-from-string)
  (:export :start
           :*app*))

(in-package #:ur-game)

;; File Paths

(defparameter +app-root+
  (asdf:system-source-directory :ur-game))
(defparameter +static-path+
  (merge-pathnames #P"static/" +app-root+))
(defparameter +index-path+
  (merge-pathnames #P"index.html" +app-root+))

;; Websocket close codes

(defconstant +ws-code-server-closed+ 1012)
(defconstant +ws-code-opponent-disconnected+ 4000)
(defconstant +ws-code-session-full+ 4002)

(defclass session ()
  ((game :initform nil :accessor game)
   (token :initarg :token :reader token)
   (clients :initform () :accessor clients)))

(defun in-game-p (session)
  (and (game session) t))

(defun game-over-p (session)
  (and (= 2 (length (clients session)))
       (null (game session))))

(defun session-full-p (session)
  (>= (length (clients session)) 2))

(defun add-client (session client)
  "Attempt to add the client to the session, and return whether it was successful."
  (if (session-full-p session)
      (prog1 nil
        (websocket-driver:close-connection
          (ws client) "Session is already full" +ws-code-session-full+))
      (prog1 t
        (push client (clients session)))))

;; NOTE: The default generator from `session-token' grabs randomness
;; from /dev/urandom or /dev/arandom, and therefore doesn't work on
;; Windows.
(defvar *game-token-generator*
  (session-token:make-generator :token-length 10))

(defvar *sessions* (make-hash-table :test 'equal)
  "Hash of sessions fetched by their token.")

(defun start-session ()
  (let* ((token (funcall *game-token-generator*))
         (session (make-instance 'session :token token)))
    (setf (gethash token *sessions*) session)
    session))

(defun find-session (token)
  (gethash token *sessions*))

(defun stop-session (session reason &key (code 1000))
  "Disconnect all clients and remove the game session from memory."
  (loop :for client :in (clients session)
        :do (websocket-driver:close-connection (ws client) reason code))
  (remhash (token session) *sessions*)
  (format t "Stopping game ~A: ~A~%" (token session) reason))

(defclass client ()
  ((color :accessor color)
   (ws :initarg :ws :accessor ws)))

(defun client-turn-p (session client)
  "Return whether it is the client player's turn"
  (eq (turn (game session)) (color client)))

(defun send-message (client message)
  "Send a message to one client."
  (pprint (list :sending :client client :message message))
  (websocket-driver:send-text
    (ws client) (encode-json-plist-to-string message)))

(defun send-message* (client &rest message)
  (send-message client message))

(defun broadcast-message (session message)
  "Send a message to all clients."
  (loop :for client :in (clients session)
        :do (send-message client message)))

(defun broadcast-message* (session &rest message)
  (broadcast-message session message))

(defun start-game (session)
  (setf (game session) (make-instance 'game))
  (let ((clients (alexandria:shuffle (clients session))))
    (setf (color (first clients)) :white
          (color (second clients)) :black)
    (dolist (client clients)
      (send-message* client
                     :op :game-start
                     :color (color client)
                     :game (game session)))))

(defun stop-game (session winner)
  (broadcast-message* session
                      :op :game-over
                      :winner winner
                      :game (game session))
  (setf (game session) nil))

(defun handle-new-connection (session client)
  (unless (add-client session client)
    (return-from handle-new-connection))

  (if (session-full-p session)
      (start-game session)
      (send-message* client
                     :op :game-token
                     :token (token session))))

(defun handle-close-connection (session client &key code reason)
  (declare (ignore client code reason))
  ;; TODO see if (stop-session) disconnecting clients manually triggers this event.
  ;; TODO maybe maintain a session until it has 0 clients.
  (stop-session session "Opponent disconnected"
                :code +ws-code-opponent-disconnected+))

;; text-message-received helper function
(defun send-game-state (session)
  (broadcast-message* session
                      :op :game-state
                      :game (game session)))

(defun handle-message (session client message)
  (let* ((message (decode-json-from-string message))
         (operand (cdr (assoc :op message)))
         (game (game session)))
    (pprint (list :received :client client :message message))
    (switch (operand :test #'string-equal)
      ("heartbeat" (send-message* client :op :ack))
      ("message" (broadcast-message* session
                                     :op :message
                                     :message message
                                     :color (color client)))
      ("rematch" (if (game-over-p session)
                     (start-game session)
                     (send-message* client
                                    :op :error
                                    :reason :not-game-over)))
      ("draw" (if (offer-draw game (color client))
                  (stop-game session nil)
                  (broadcast-message* session
                                      :op :tie
                                      :player (color client))))
      ("forfeit" (broadcast-message* session
                                     :op :forfeit
                                     :player (color client)))
      ("roll" (if (not (client-turn-p session client))
                  (send-message* client
                                 :op :err
                                 :reason :not-your-turn)
                  (multiple-value-bind (successful-p total flips turn-ended-p error-reason)
                    (roll game)
                    (if successful-p
                        (broadcast-message* session
                                            :op :roll
                                            :successful t
                                            :total total
                                            :flips flips
                                            :skip-turn (make-instance
                                                         'json-bool :p turn-ended-p))
                        (send-message* client
                                       :op :roll
                                       :successful (make-instance
                                                     'json-bool :p nil)
                                       :reason error-reason)))))
       ("move" (if (not (client-turn-p session client))
                   (send-message* client
                                  :op :err
                                  :reason :not-your-turn)
                   (multiple-value-bind (successful-p move-type turn-ended-p)
                     (make-move game (cdr (assoc :position message)))
                     (if successful-p
                         (progn
                           (broadcast-message* session
                                               :op :move
                                               :successful t
                                               :move-type move-type
                                               :skip-turn (make-instance
                                                            'json-bool :p turn-ended-p))
                           (if-let (winner (winner game))
                             (stop-game session winner)
                             (send-game-state session)))
                         (send-message* client
                                        :op :move
                                        :successful (make-instance
                                                      'json-bool :p nil)
                                        :reason move-type)))))
       (t (send-message* client
                         :op :err
                         :reason :no-such-operand)))))

(defun set-deathmatch (session-token)
  "For debugging end-game states. Given a session token, set the spare pieces
   of both players to 1."
  (let* ((session (find-session session-token))
         (game (game session)))
    (setf (spare-pieces (white-player game)) 1
          (spare-pieces (black-player game)) 1)
    (send-game-state session)))

(defun session-app (env &optional token)
  (let* ((ws (websocket-driver:make-server env))
         (client (make-instance 'client :ws ws))
         (session (or (find-session token) (start-session))))
    (websocket-driver:on :open ws (curry 'handle-new-connection session client))
    (websocket-driver:on :message ws (curry 'handle-message session client))
    (websocket-driver:on :close ws (curry 'handle-close-connection session client))

    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

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

(defparameter *app*
  (lack:builder
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
           (constantly `(200 (:content-type "text/html") ,+index-path+)))
    ;; Static Path
    (:static :path "/static/" :root +static-path+)
    ;; All other routes are invalid.
    (constantly '(404 (:content-type "text/plain") ("Not Found")))))

(defvar *website-handler* nil)

(defun start ()
  (when *website-handler* (clack:stop *website-handler*))
  (setf *website-handler* (clack:clackup *app*
                                         :server :hunchentoot))
  (values))
