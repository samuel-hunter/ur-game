(defpackage #:ur-game
  (:use #:cl))

(in-package #:ur-game)

(defun make-empty-path (length)
  (make-array length :initial-element :none))

;; path lengtsh
(defparameter *start-length* 4)
(defparameter *shared-length* 8)
(defparameter *end-length* 2)

(defparameter *path-length* (+ *start-length* *shared-length* *end-length*))

(defparameter *starting-pieces* 7)


;; Rosette tiles protect pieces from battle and give the player a
;; second turn
(defparameter *rosettes* '(4 8 14))

(defclass game ()
  ((white-start :initform (make-empty-path *start-length*))
   (black-start :initform (make-empty-path *start-length*))
   (shared-path :initform (make-empty-path *shared-length*))
   (white-end :initform (make-empty-path *end-length*))
   (black-end :initform (make-empty-path *end-length*))
   (white-spare-pieces :initform *starting-pieces*)
   (black-spare-pieces :initform *starting-pieces*)
   (turn :initform :white)
   (random-state :initform (make-random-state t))
   (last-roll)
   (rolledp :initform nil)))

(define-condition game-error (error)
  ((message :initarg :message :reader message)))

(defun tile-to-char (owner)
  (ecase owner
    (:none #\*)
    (:white #\W)
    (:black #\B)))

(defun write-path (path &key (newline t))
  (loop :for tile :across path
     :do (write-char (tile-to-char tile)))
  (when newline
    (write-char #\Newline))
  nil)

(defun write-game (game)
  (with-slots (white-start black-start white-end black-end shared-path) game
    (write-path (reverse white-start) :newline nil)
    (princ "  ")
    (write-path (reverse white-end))
    (write-path shared-path)
    (write-path (reverse black-start) :newline nil)
    (princ "  ")
    (write-path (reverse black-end))))

(defun opponent (game)
  "Retun the player waiting for their turn."
  (ecase (slot-value game 'turn)
    (:white :black)
    (:black :white)))

(defun next-turn (game)
  (with-slots (rolledp turn) game
    (setf turn (opponent game))
    (setf rolledp nil)))

(defun roll (game)
  "Toss four coins and sum the total, providing a similar D4 allegedly
played in the original game. Store the sum in the game."
  (with-slots (random-state last-roll rolledp) game
    (when rolledp
      (error 'game-error :message "Player already rolled for the turn."))

    (let* ((flips (loop :repeat 4
                     :collect (random 2 random-state)))
           (total (reduce #'+ flips)))

      ;; If the roll was 0, the current player has lost its turn.
      (if (= total 0)
          (next-turn game)
          (setf rolledp t))
      (setf last-roll total)
      (values total flips))))

(defun player-start (game)
  (with-slots (turn white-start black-start) game
    (ecase turn
      (:white white-start)
      (:black black-start))))

(defun player-end (game)
  (with-slots (turn white-end black-end) game
    (ecase turn
      (:white white-end)
      (:black black-end))))

(defun player-tile (game index)
  "Return ownership of the player's effective tile by index."

  (decf index) ; 1-based; playing from tile 0 means placing a new piece on the table.
  (when (< index *start-length*)
    (return-from player-tile (aref (player-start game) index)))
  (decf index *start-length*)

  (when (< index *shared-length*)
    (return-from player-tile (aref (slot-value game 'shared-path) index)))
  (decf index *shared-length*)

  (aref (player-end game) index))

(defun (setf player-tile) (new-owner game index)
  "Set the ownership of the player's effective tile by index."
  (decf index) ; 1-based; playing from tile 0 means placing a new piece on the table.
  (when (< index *start-length*)
    (return-from player-tile (setf (aref (player-start game) index) new-owner)))
  (decf index *start-length*)

  (when (< index *shared-length*)
    (return-from player-tile (setf (aref (slot-value game 'shared-path) index) new-owner)))
  (decf index *shared-length*)

  (setf (aref (player-end game) index) new-owner))

(defun tile-claimable-p (game index)
  "Return T if the player's effective tile can be claimed."
  (or (eq (player-tile game index) :none)
      (not (or (eq (player-tile game index) (slot-value game 'turn))
               (member index *rosettes*)))))

(defun move-tile (game index)
  (with-slots (rolledp last-roll turn white-spare-pieces black-spare-pieces) game
    (let ((dest-index (+ index last-roll))
          (opponent (opponent game)))
      (unless rolledp
        (error 'game-error :message "Player has not rolled yet"))

      (when (> dest-index (1+ *path-length*))
        (error 'game-error :message "The piece can't move that far"))

      (unless (or (= dest-index (1+ *path-length*))
                  (tile-claimable-p game dest-index))
        (error 'game-error :message "You can't move there"))

      ;; Move hte player
      (when (eq (player-tile game dest-index) opponent)
        (ecase opponent
          (:white (incf white-spare-pieces))
          (:black (incf black-spare-pieces))))

      (unless (eq index 0)
        (setf (player-tile game index) :none))

      (unless (eq dest-index (1+ *path-length*))
        (setf (player-tile game dest-index) turn))
      (next-turn game))))
