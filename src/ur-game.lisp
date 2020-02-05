(defpackage #:ur-game
  (:use :cl :ur-game.engine
        :ur-game.config :ur-game.json)
  (:import-from :alexandria
                :when-let
                :if-let)
  (:import-from :json
                :encode-json-plist-to-string
                :decode-json-from-string)
  (:export :start
           :main
           :stop))

(in-package #:ur-game)

(defvar *http-acceptor* nil)
(defvar *ws-acceptor* nil)

(defparameter +app-root+ (asdf:system-source-directory :ur-game))

(defun static-root ()
  (or (config :htdocs)
      (merge-pathnames #P "htdocs/" +app-root+)))

(defun index-root ()
  (merge-pathnames #P "index.html" (static-root)))

(defparameter +join-url-scanner+ (cl-ppcre:create-scanner "^/join/([\\w]+)$"))

(defconstant +ws-code-opponent-disconnected+ 4000)

;; NOTE: The default generator from `session-token' grabs randomness
;; form /dev/urandom or /dev/arandom, and therefore doesn't work on
;; Windows.
(defvar *game-token-generator*
  (session-token:make-generator :token-length 10))

(defun log-message (message)
  (when (config :debug)
    (pprint message)))


(defclass player-session (hunchensocket:websocket-client)
  ((color :accessor color)
   (offered-draw :accessor offered-draw)))

(defclass game-session (hunchensocket:websocket-resource)
  ((game :accessor game)
   (token :initarg :token :reader token)
   (white-token :initform (funcall *game-token-generator*))
   (black-token :initform (funcall *game-token-generator*))
   (status :initform :empty :accessor status))
  (:default-initargs :client-class 'player-session))

(defun active-player-session (session)
  "Return the session of the current turn's player."
  (session-player session (turn (game session))))

(defun send-message (client &optional data)
  (log-message (list :sending :client client :message data))
  (hunchensocket:send-text-message
   client (encode-json-plist-to-string data)))

(defun send-message* (client &rest data)
  (send-message client data))

(defun broadcast-message (session &optional data)
  (loop :for client :in (hunchensocket:clients session)
     :do (send-message client data)))

(defun broadcast-message* (session &rest data)
  (broadcast-message session data))

(defun send-game-state (game-session)
  (broadcast-message* game-session
                      :op :game-state
                      :game (game game-session)))

(defvar *games* (make-hash-table :test 'equal))

(defun stop-session (session reason &key (status 1000))
  (setf (status session) :stopped)
  (loop :for client :in (hunchensocket:clients session)
     :do (hunchensocket:close-connection client :reason reason :status status))
  (remhash (token session) *games*)
  (format t "Stopping game ~A: ~A~%" (token session) reason))

(defun start-online-session (session)
  (setf (status session) :playing)
  (setf (game session) (make-instance 'game))
  (let ((clients (alexandria:shuffle (hunchensocket:clients session))))
    (setf (color (first clients)) :white)
    (setf (color (second clients)) :black)

    (dolist (client clients)
      (setf (offered-draw client) nil)
      (send-message* client
                     :op :game-start
                     :color (color client)
                     :game (game session)))))

(defmethod hunchensocket:client-connected ((session game-session) client)
  (ecase (status session)
    (:empty (send-message* client
                           :op :game-token
                           :token (token session))
            (setf (status session) :waiting))
    (:waiting (start-online-session session))))

(defun message-op (message)
  "Return the operand of the message"
  (cdr (assoc :op message)))

(defmethod hunchensocket:client-disconnected ((session game-session) client)
  (unless (eq (status session) :stopped)
    (stop-session session "Opponent disconnected" :status +ws-code-opponent-disconnected+)))

(defun stop-game (session winner)
  (setf (status session) :game-over)
  (broadcast-message* session
                      :op :game-over
                      :winner winner

                      :game (game session)))

(defmethod hunchensocket:text-message-received ((session game-session) client message)
  (let* ((message (decode-json-from-string message))
         (operand (message-op message)))
    (log-message (list :received :client client :message message))
    (cond
      ((string-equal operand "heartbeat")
       (send-message* client :op :ack))
      ((string-equal operand "message")
       (let ((message (cdr (assoc :message message))))
         (broadcast-message* session :op :message
                             :message message
                             :color (color client))))
      ((string-equal operand "rematch")
       (if (eq (status session) :game-over)
           (start-online-session session)
           (send-message* client :op :err :reason :not-game-over)))
      ((not (eq (status session) :playing))
       (send-message* client :op :err :reason :not-playing-yet))
      ((string-equal operand "draw")
       (setf (offered-draw client) t)
       (if (every 'identity (loop :for c :in (hunchensocket:clients session)
                               :collect (offered-draw c)))
           (stop-game session nil)
           (broadcast-message* session :op :tie
                               :player (color client))))
      ((string-equal operand "forfeit")
       (broadcast-message* session :op :forfeit
                           :player (color client))
       (stop-game session (opponent-color (color client))))
      ((not (eq (color client) (turn (game session))))
       (send-message* client :op :err
                      :reason :not-your-turn))
      (t (let* ((game (game session))
                (result (process-action game message)))

           (if (action-successful result)
               (progn
                 ;; A move is made; reset all draw offerings.
                 (loop :for c :in (hunchensocket:clients session)
                    :do (setf (offered-draw c) nil))

                 (broadcast-message session result)
                 (if-let (winner (winner game))
                   (stop-game session winner)
                   (when (getf result :turn-end)
                     (send-game-state session))))
               (send-message client result)))))))

(defun new-game-dispatcher (request)
  (when (string= (hunchentoot:script-name request) "/new")
    (let* ((token (funcall *game-token-generator*))
          (game-session (make-instance 'game-session :token token)))
      (setf (gethash token *games*) game-session)
      (format t "Created new game ~A~%" token)
      game-session)))

(defun token-from-url (url)
  (multiple-value-bind (scanned groups) (cl-ppcre:scan-to-strings +join-url-scanner+ url)
    (when scanned
      (aref groups 0))))

(defun find-session (token)
  (gethash token *games*))

(defun join-game-dispatcher (request)
  (when-let (token (token-from-url (hunchentoot:script-name request)))
    (when-let (game-session (find-session token))
      (when (eq (status game-session) :waiting)
        game-session))))

(defun invite-link-dispatcher (request)
  (print (hunchentoot:script-name request))
  (when-let (token (token-from-url (hunchentoot:script-name request)))
    (print :found)
    (hunchentoot:redirect (concatenate 'string "/#/" token))))

(defun stop-acceptor (acceptor)
  "Stop the acceptor if feasible."
  (when (and acceptor
             (hunchentoot:started-p acceptor))
    (hunchentoot:stop acceptor)))

(defun stop ()
  (maphash (lambda (key game)
             (declare (ignore key))
             (stop-session game "Server closed" :status 1012))
           *games*)
  (setf *games* (make-hash-table :test 'equal))

  (values (stop-acceptor *http-acceptor*)
          (stop-acceptor *ws-acceptor*)))

(defun set-deathmatch (session-token)
  "Given a session token, set the spare pieces of both players to 1. For debug purposes"
  (let* ((session (find-session session-token))
         (game (game session)))
    (setf (spare-pieces (white-player game)) 1)
    (setf (spare-pieces (black-player game)) 1)
    (send-game-state session)))

(defun start (&key (http-port (config :http-port)) (ws-port (config :ws-port)))
  (stop)

  (setf hunchentoot:*dispatch-table*
        (list (hunchentoot:create-static-file-dispatcher-and-handler "/" (index-root))
              'invite-link-dispatcher
              (hunchentoot:create-folder-dispatcher-and-handler "/" (static-root))))
  (setf hunchensocket:*websocket-dispatch-table*
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
