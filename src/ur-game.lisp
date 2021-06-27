;;; ur-game.lisp - Service layer of the Ur Game API.
;;
;; Manages the HTTP server, WebSocket server, and WebSocket message
;; interpretation.
(defpackage #:ur-game
  (:use :cl :ur-game.engine
        :ur-game.config :ur-game.json)
  (:import-from :alexandria
                :when-let
                :if-let
                :switch
                :eswitch)
  (:import-from :json
                :encode-json-plist-to-string
                :decode-json-from-string)
  (:export :start
           :main
           :stop))

(in-package #:ur-game)

(defvar *http-acceptor* nil)
(defvar *ws-acceptor* nil)

(defparameter +app-root+
  (asdf:system-source-directory :ur-game))

(defun static-root ()
  (or (config :htdocs)
      (merge-pathnames #P"htdocs/" +app-root+)))

(defun index-root ()
  (merge-pathnames #P"index.html" (static-root)))

(defconstant +ws-code-server-closed+ 1012)
(defconstant +ws-code-opponent-disconnected+ 4000)
(defconstant +ws-code-session-full+ 4002)

;; NOTE: The default generator from `session-token' grabs randomness
;; from /dev/urandom or /dev/arandom, and therefore doesn't work on
;; Windows.
(defvar *game-token-generator*
  (session-token:make-generator :token-length 10))

(defvar *sessions* (make-hash-table :test 'equal)
  "Hash of games fetched by their token.")

(defun find-session (token)
  (gethash token *sessions*))

(defclass session (hunchensocket:websocket-resource)
  ((game :accessor game)
   (token :initarg :token :reader token))
  (:default-initargs :client-class 'player))

(defun in-game-p (session)
  (if (game session) t nil))

(defun game-over-p (session)
  (and (= 2 (length (hunchensocket:clients session)))
       (null (game session))))

(defun session-full-p (session)
  (>= (length (hunchensocket:clients session)) 2))

(defun client-turn-p (session client)
  "Return whether it is the client player's turn"
  (eq (turn (game session)) (color client)))

(defun stop-session (session reason &key (status 1000))
  "Disconnect all clients and remove the game session from memory."
  (loop :for client :in (hunchensocket:clients session)
        :do (hunchensocket:close-connection client :reason reason :status status))
  (remhash (token session) *sessions*)
  (format t "Stopping game ~A: ~A~%" (token session) reason))

(defclass player (hunchensocket:websocket-client)
  ((color :accessor color)))

(defun start-game (session)
  (setf (game session) (make-instance 'game))
  (let ((clients (alexandria:shuffle (hunchensocket:clients session))))
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

(defun send-message (client message)
  "Send a message to one client."
  (pprint (list :sending :client client :message message))
  (hunchensocket:send-text-message
    client (encode-json-plist-to-string message)))

(defun send-message* (client &rest message)
  (send-message client message))

(defun broadcast-message (session message)
  "Send a message to all clients."
  (loop :for client :in (hunchensocket:clients session)
        :do (send-message client message)))

(defun broadcast-message* (session &rest message)
  (broadcast-message session message))

(defmethod hunchensocket:client-connected ((session session) client)
  (if (= 1 (length (hunchensocket:clients session)))
      (send-message* client
                      :op :game-token
                      :token (token session))
      (start-game session)))

(defmethod hunchensocket:client-disconnected ((session session) client)
  ;; TODO see if (stop-sesion) disconnecting clients manually triggers this event.
  (stop-session session "Opponent disconnected" :status +ws-code-opponent-disconnected+))

;; text-message-received helper function
(defun send-game-state (game-session)
  (broadcast-message* game-session
                      :op :game-state
                      :game (game game-session)))

(defmethod hunchensocket:text-message-received ((session session) client message)
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

(defun new-game-dispatcher (request)
  (when (string= (hunchentoot:script-name request) "/new")
    (let* ((token (funcall *game-token-generator*))
           (session (make-instance 'session :token token)))
      (setf (gethash token *sessions*) session)
      (format t "Created a new game ~A~%" token)
      session)))

(defparameter +join-uri-scanner+
  (cl-ppcre:create-scanner "^/join/([\\w]+)$"))

(defun token-from-uri (uri)
  "Extract {token} from URI `/join/{token}'"
  (multiple-value-bind (scanned groups)
    (cl-ppcre:scan-to-strings +join-uri-scanner+ uri)
    (when scanned (aref groups 0))))

(defun join-game-dispatcher (request)
  (when-let (token (token-from-uri (hunchentoot:script-name request)))
    (when-let (session (find-session token))
      (unless (session-full-p session)
        session))))

(defun invite-link-dispatcher (request)
  (print (hunchentoot:script-name request))
  (when-let (token (token-from-uri (hunchentoot:script-name request)))
    (print :found)
    (hunchentoot:redirect (concatenate 'string "/#/" token))))

(defun stop-acceptor (acceptor)
  "Stop the acceptor if possible."
  (when (and acceptor (hunchentoot:started-p acceptor))
    (hunchentoot:stop acceptor)))

(defun stop ()
  "Close all sessions and stop all acceptors."
  (maphash (lambda (key game)
             (remhash key *sessions*)
             (stop-session game "Server closed" :status +ws-code-server-closed+))
           *sessions*)
  (setf *sessions* (make-hash-table :test 'equal))
  (values (stop-acceptor *http-acceptor*)
          (stop-acceptor *ws-acceptor*)))

(defun set-deathmatch (session-token)
  "For debugging end-game states. Given a session token, set the spare pieces of both players to 1."
  (let* ((session (find-session session-token))
         (game (game session)))
    (setf (spare-pieces (white-player game)) 1
          (spare-pieces (black-player game)) 1)
    (send-game-state session)))

(defun start (&key (http-port (config :http-port)) (ws-port (config :ws-port)))
  (stop)

  (setf hunchentoot:*dispatch-table*
        (list (hunchentoot:create-static-file-dispatcher-and-handler "/" (index-root))
              'invite-link-dispatcher
              (hunchentoot:create-folder-dispatcher-and-handler "/" (static-root)))
        hunchensocket:*websocket-dispatch-table*
        (list 'new-game-dispatcher
              'join-game-dispatcher))

  (unless *http-acceptor*
    (setf *http-acceptor*
          (make-instance 'hunchentoot:easy-acceptor :port http-port)))
  (unless *ws-acceptor*
    (setf *ws-acceptor*
          (make-instance 'hunchensocket:websocket-acceptor :port ws-port)))

  (values (hunchentoot:start *http-acceptor*)
          (hunchentoot:start *ws-acceptor*)))

(defun main ()
  "Start the server as a compiled binary."
  (format t "Starting the Royal Game of Ur...~%")
  (start)
  (loop for thread in (remove-if (lambda (x) (eq x (bordeaux-threads:current-thread)))
                                 (bordeaux-threads:all-threads))
       do (bordeaux-threads:join-thread thread)))
