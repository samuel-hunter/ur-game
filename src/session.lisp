;;; session.lisp - Manages WebSocket connections
;;
;; Manages session registration and WebSocket message interpretation.
(defpackage #:ur-game.session
  (:use #:cl #:ur-game.engine #:ur-game.json)
  (:import-from #:alexandria
                #:curry
                #:if-let)
  (:import-from :json
                #:encode-json-plist-to-string
                #:decode-json-from-string)
  (:export :session-app))

(in-package #:ur-game.session)



;; Logging (TODO: set level in configuration)

(vom:config :ur-game.session :debug)

;; Websocket close codes

(defconstant +ws-code-opponent-disconnected+ 4000)
(defconstant +ws-code-session-full+ 4002)

(defvar *sessions* (make-hash-table :test 'equal))

(defun register-session (session)
  (setf (gethash (slot-value session 'token) *sessions*)
        session))

(defun find-session (token)
  (gethash token *sessions*))

(defun deregister-session (session)
  (remhash (slot-value session 'token) *sessions*))

;; NOTE: The default generator from `session-token' grabs randomness
;; from /dev/urandom or /dev/arandom, and therefore doesn't work on
;; Windows.
(defvar *game-token-generator*
  (session-token:make-generator :token-length 10))

(defclass session ()
  ((game :initform nil :accessor game)
   (token :initform (funcall *game-token-generator*) :reader token)
   (clients :initform () :accessor clients)))

(defmethod initialize-instance :after ((instance session) &key &allow-other-keys)
  ;; Start and register the session
  (register-session instance)
  (vom:info "STARTED session ~A" (slot-value instance 'token)))

(defclass client ()
  ((color :accessor color)
   (ws :initarg :ws :accessor ws)))

(defun client-turn-p (session client)
  "Return whether it is the client player's turn"
  (eq (turn (game session)) (color client)))

(defun session-status (session)
  (with-slots (clients game) session
    (cond
      ((< (length clients) 2) :waiting-for-players)
      (game :in-game)
      (t :game-over))))

(defun quit-session (session reason &key (code 1000))
  "Disconnect all clients and deregister the game session."
  (deregister-session session)
  (dolist (client (clients session))
    (websocket-driver:close-connection (ws client) reason code))
  (vom:notice "STOPPED session ~A: ~A" (token session) reason))

(defun add-client (session client)
  "Attempt to add the client to the session, and return whether it was successful."
  (if (eq :waiting-for-players (session-status session))
      (progn
        (push client (clients session))
        t)
      (progn
        (websocket-driver:close-connection
          (ws client) "Session is already full" +ws-code-session-full+)
        nil)))

(defun send-message (client message)
  "Send a message to one client."
  (vom:debug "SENDING message to client ~S: ~S" (ws client) message)
  (websocket-driver:send-text
    (ws client) (encode-json-plist-to-string message)))

(defun send-message* (client &rest message)
  (send-message client message))

(defun broadcast-message (session message)
  "Send the same message to all clients."
  (dolist (client (clients session))
    (send-message client message)))

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



;; Request handling
(defparameter *request-dispatch*
  (make-hash-table :test 'equal))

(defmacro defrequest (function-name opcode-name lambda-list &body body)
  `(prog1
     (defun ,function-name ,lambda-list ,@body)
     (setf (gethash ,opcode-name *request-dispatch*) ',function-name)))

(defrequest handle-heartbeat "heartbeat" (session client message)
  (declare (ignore session message))
  (send-message* client :op :ack))

(defrequest handle-chat "message" (session client message)
  (broadcast-message* session
                      :op :message
                      :message message
                      :color (color client)))

(defrequest handle-rematch "rematch" (session client message)
  (declare (ignore message))
  (if (eq :game-over (session-status session))
      (start-game session)
      (send-message* client
                     :op :error
                     :reason :not-game-over)))

(defrequest handle-draw "draw" (session client message)
  (declare (ignore message))
  (if (offer-draw (game session) (color client))
      (stop-game session nil)
      (broadcast-message* session
                          :op :tie
                          :player (color client))))

(defrequest handle-forfeit "forfeit" (session client message)
  (declare (ignore message))
  (broadcast-message* session
                      :op :forfeit
                      :player (color client)))

(defrequest handle-roll "roll" (session client message)
  (declare (ignore message))
  (if (not (client-turn-p session client))
      (send-message* client
                     :op :err
                     :reason :not-your-turn)
      (multiple-value-bind (successful-p total flips turn-ended-p error-reason)
        (roll (game session))

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

(defrequest handle-move "move" (session client message)
  (let ((game (game session)))
    (if (not (client-turn-p session client))
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
                  (broadcast-message* session
                                      :op :game-state
                                      :game game)))
              (send-message* client
                             :op :move
                             :successful (make-instance
                                           'json-bool :p nil)
                             :reason move-type))))))

(defun handle-message (session client message)
  (let* ((message (decode-json-from-string message)))
    (vom:debug "RECEIVED message from client ~S: ~S" (ws client) message)
    (if-let ((handler (gethash (cdr (assoc :op message)) *request-dispatch*)))
      (funcall handler session client message)
      (send-message* client
                     :op :err
                     :reason :no-such-operand))))

(defun handle-new-connection (session client)
  (unless (add-client session client)
    (return-from handle-new-connection))

  (if (eq :waiting-for-players (session-status session))
      (send-message* client
                     :op :game-token
                     :token (token session))
      (start-game session)))

(defun handle-close-connection (session client &key code reason)
  (declare (ignore client code reason))
  ;; TODO see if (quit-session) disconnecting clients manually triggers this event.
  ;; TODO maybe maintain a session until it has 0 clients.
  (quit-session session "Opponent disconnected"
                :code +ws-code-opponent-disconnected+))

(defun session-app (env &optional token)
  (let* ((ws (websocket-driver:make-server env))
         (client (make-instance 'client :ws ws))
         (session (or (find-session token) (make-instance 'session))))
    (websocket-driver:on :open ws (curry 'handle-new-connection session client))
    (websocket-driver:on :message ws (curry 'handle-message session client))
    (websocket-driver:on :close ws (curry 'handle-close-connection session client))

    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

