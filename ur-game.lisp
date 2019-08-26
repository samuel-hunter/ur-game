(defpackage #:ur-game
  (:use :cl :ur-game.engine)
  (:export :start))

(in-package #:ur-game)

(defparameter +http-port+ 8080)
(defparameter +wss-port+ 8081)
(defvar *acceptor* nil)

(defparameter +app-root+ (asdf:system-source-directory :ur-game))
(defparameter +static-root+ (merge-pathnames #P "static/" +app-root+))
(defparameter +index-root+ (merge-pathnames #P "index.html" +static-root+))

;; NOTE: The default generator from `session-token' grabs randomness
;; form /dev/urandom or /dev/arandom, and therefore doesn't work on
;; Windows.
(defvar *game-token-generator*
  (session-token:make-generator :token-length 10))

(defvar *player-token-generator*
  (session-token:make-generator))

(defclass player-session ()
  ((token :initform (funcall *player-token-generator*) :reader token)
   (last-activity :initform (get-universal-time) :reader last-activity)))

(defclass game-session ()
  ((game :initform (make-instance 'game))
   (token :initform (funcall *game-token-generator*) :reader token)
   (black :initform (make-instance 'player-session))
   (white :initform (make-instance 'player-session))))

(defun session-player (session player)
  (with-slots (black white) session
    (ecase player
      (:black black)
      (:white white))))

(defun active-player-session (session)
  "Return the session of the current turn's player."
  (session-player session (turn (slot-value session 'game))))

(defvar *games* (make-hash-table))

(defun start ()
  (when (and *acceptor* (hunchentoot:started-p *acceptor*))
    (hunchentoot:stop *acceptor*))

  (setf hunchentoot:*dispatch-table* ())
  (push (hunchentoot:create-folder-dispatcher-and-handler "/" +static-root+) hunchentoot:*dispatch-table*)
  (push (hunchentoot:create-static-file-dispatcher-and-handler "/" +index-root+) hunchentoot:*dispatch-table*)

  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port +http-port+))
  (hunchentoot:start *acceptor*))
