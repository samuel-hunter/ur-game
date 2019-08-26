(defpackage #:ur-game.repl
  (:use #:cl :ur-game.engine)
  (:export :start)
  (:import-from :alexandria
                :when-let))

(in-package #:ur-game.repl)

(defun tile-to-char (owner)
  (ecase owner
    (:none #\.)
    (:white #\W)
    (:black #\B)))

(defun write-path (path &key (newline t))
  (loop :for tile :across path
     :do (write-char (tile-to-char tile)))
  (when newline
    (write-char #\Newline))
  nil)

(defun write-game (game)
  (with-slots (white-start black-start white-end black-end shared-path turn last-roll rolledp
                           white-spare-pieces black-spare-pieces) game
    (fresh-line)
    (write-path (reverse white-start) :newline nil)
    (princ "  ")
    (write-path (reverse white-end))
    (write-path shared-path)
    (write-path (reverse black-start) :newline nil)
    (princ "  ")
    (write-path (reverse black-end))
    (format t "White: ~A  Black: ~A~%" white-spare-pieces black-spare-pieces)
    (format t "Turn: ~A~%" (string turn))
    (when rolledp
      (format t "Roll: ~A~%" last-roll))))

(defun game-read ()
  (string-downcase (read-line)))

(defun game-eval (line game)
  (cond
    ((string-equal line "roll") (cons :roll (multiple-value-list (roll game))))
    ((string-equal line "quit") :quit)
    (t (handler-case
           (cons :turn (make-move game (parse-integer line)))
         (parse-error () :bad-input)))))

(defun start ()
  (let ((game (make-instance 'game)))
    (write-game game)
    (loop
       :for result = (game-eval (game-read) game)
       :until (or (eq result :quit) (winner game))
       :do (progn (format t "~A~%" result)
                  (when (not (slot-value game 'rolledp))
                    (write-game game))))
    (when-let (winner (winner game))
      (format t "The winner is ~A~%" winner))))
