(defpackage #:ur-game
  (:use :cl :ur-game.engine)
  (:import-from :alexandria
                :when-let
                :eswitch)
  (:import-from :json
                :encode-json
                :encode-json-to-string
                :encode-json-plist-to-string
                :decode-json-from-string)
  (:export :start))

(in-package #:ur-game)

(defparameter +http-port+ 8080)
(defparameter +ws-port+ 8081)

(defvar *http-acceptor* nil)
(defvar *ws-acceptor* nil)

(defparameter +app-root+ (asdf:system-source-directory :ur-game))
(defparameter +static-root+ (merge-pathnames #P "static/" +app-root+))
(defparameter +index-root+ (merge-pathnames #P "index.html" +static-root+))

(defparameter +join-url-scanner+ (cl-ppcre:create-scanner "^/join/([\\w]+)$"))

;; NOTE: The default generator from `session-token' grabs randomness
;; form /dev/urandom or /dev/arandom, and therefore doesn't work on
;; Windows.
(defvar *game-token-generator*
  (session-token:make-generator :token-length 10))

(defclass player-session (hunchensocket:websocket-client)
  ((color :accessor color)))

(defclass game-session (hunchensocket:websocket-resource)
  ((game :initform (make-instance 'game) :reader game)
   (token :initarg :token :reader token)
   (white-token :initform (funcall *game-token-generator*))
   (black-token :initform (funcall *game-token-generator*))
   (status :initform :empty :accessor status))
  (:default-initargs :client-class 'player-session))

(defun active-player-session (session)
  "Return the session of the current turn's player."
  (session-player session (turn (game session))))

(defun api-send (client opcode &rest data)
  (hunchensocket:send-text-message
   client (encode-json-plist-to-string (list* :op opcode data))))

(defun game-to-alist (game)
  (with-slots (white-start black-start shared-path white-end black-end
                           white-spare-pieces black-spare-pieces turn) game
    `((:white-start . ,white-start)
      (:black-start . ,black-start)
      (:shared-path . ,shared-path)
      (:white-end . ,white-end)
      (:black-end . ,black-end)
      (:white-spare-pieces . ,white-spare-pieces)
      (:black-spare-pieces . ,black-spare-pieces)
      (:turn . ,turn))))

(defun send-game-state (game-session)
  (let ((game-state (game-to-alist (game game-session))))
    (loop :for client :in (hunchensocket:clients game-session)
       :do (api-send client :game-state :game game-state))))

(defvar *games* (make-hash-table :test 'equal))

(defun stop-session (session reason)
  (setf (status session) :stopped)
  (loop :for client :in (hunchensocket:clients session)
     :do (hunchensocket:close-connection client :reason reason))
  (remhash (token session) *games*)
  (format t "Stopping game ~A: ~A~%" (token session) reason))

(defun start-online-session (session)
  (setf (status session) :playing)
  (let ((clients (hunchensocket:clients session))
        (first-client-white-p (zerop (random 2))))
    (if first-client-white-p
        (progn
          (setf (color (first clients)) :white)
          (setf (color (second clients)) :black))
        (progn
          (setf (color (first clients)) :black)
          (setf (color (second clients)) :white)))
    (loop :for client :in clients
       :do (api-send client :welcome :color (color client)))
    (send-game-state session)))

(defmethod hunchensocket:client-connected ((session game-session) client)
  (ecase (status session)
    (:empty (api-send client :game-token :token (token session))
            (setf (status session) :waiting))
    (:waiting (start-online-session session))))

(defmethod hunchensocket:client-disconnected ((session game-session) client)
  (unless (eq (status session) :stopped)
    (stop-session session "Client disconnected")))

(defmethod hunchensocket:text-message-received ((session game-session) client message)
  (let ((message (decode-json-from-string message)))
    (eswitch ((cdr (assoc :op message)) :test #'string-equal)
      ("heartbeat" (api-send client :ack)))))

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

(defun join-game-dispatcher (request)
  (when-let (token (token-from-url (hunchentoot:script-name request)))
    (when-let (game-session (gethash token *games*))
      (when (eq (status game-session) :waiting)
        game-session))))

(defun stop-acceptor (acceptor)
  "Stop the acceptor if feasible."
  (when (and acceptor
             (hunchentoot:started-p acceptor))
    (hunchentoot:stop acceptor)))

(defun stop ()
  (maphash (lambda (key game)
             (declare (ignore key))
             (stop-session game "Server closed"))
           *games*)
  (setf *games* (make-hash-table :test 'equal))

  (values (stop-acceptor *http-acceptor*)
          (stop-acceptor *ws-acceptor*)))

(defun start ()
  (stop)

  (setf hunchentoot:*dispatch-table*
        (list (hunchentoot:create-static-file-dispatcher-and-handler "/" +index-root+)
              (hunchentoot:create-folder-dispatcher-and-handler "/" +static-root+)))
  (setf hunchensocket:*websocket-dispatch-table*
        (list 'new-game-dispatcher
              'join-game-dispatcher))

  (unless *http-acceptor*
    (setf *http-acceptor*
          (make-instance 'hunchentoot:easy-acceptor :port +http-port+)))
  (unless *ws-acceptor*
    (setf *ws-acceptor*
          (make-instance 'hunchensocket:websocket-acceptor :port +ws-port+)))

  (values (hunchentoot:start *http-acceptor*)
          (hunchentoot:start *ws-acceptor*)))
