(defpackage #:ur-game.engine
  (:use :cl)
  (:import-from :alexandria
                :switch)
  (:export :game
           :winner

           ;; Game actions
           :roll
           :make-move
           :offer-draw

           ;; TODO figure out how to make the app not care about whose turn it is.
           :turn))

(in-package #:ur-game.engine)

(defun make-empty-path (length)
  (make-array length :initial-element :none))

;; path lengths
(defparameter +start-length+ 4)
(defparameter +shared-length+ 8)
(defparameter +end-length+ 2)

(defparameter +path-length+ (+ +start-length+ +shared-length+ +end-length+))

(defparameter +starting-pieces+ 7)


;; Rosette tiles protect pieces from battle and give the player a
;; second turn
(defparameter +rosettes+ '(4 8 14))

(defun rosettep (index)
  "Return whether the tile has a rosette"
  (member index +rosettes+))

(defclass player ()
  ((start-path :initform (make-empty-path +start-length+)
               :accessor start-path)
   (end-path :initform (make-empty-path +end-length+)
             :accessor end-path)
   (spare-pieces :initform +starting-pieces+
                 :accessor spare-pieces)
   (draw-offered :initform nil
                 :accessor draw-offered)))

(defclass game ()
  ((black :initform (make-instance 'player)
          :reader black-player)
   (white :initform (make-instance 'player)
          :reader white-player)
   (shared-path :initform (make-empty-path +shared-length+)
                :accessor shared-path)
   (turn :initform :white
         :accessor turn)
   (random-state :initform (make-random-state t))
   (last-roll :initform nil
              :accessor last-roll)))

;; TODO figure out how to separate this last piece of presentation from the
;; game engine.
(defmethod json:encode-json ((object game) &optional (stream json:*json-output*))
  (with-slots (black white shared-path turn last-roll) object
    (let ((json:*json-output* stream))
      (json:with-object ()
        (json:as-object-member (:board)
          (json:with-object ()
            (json:encode-object-member :black-start (start-path black))
            (json:encode-object-member :white-start (start-path white))
            (json:encode-object-member :shared-middle shared-path)
            (json:encode-object-member :black-end (end-path black))
            (json:encode-object-member :white-end (end-path white))))

        (json:encode-object-member :black-spares (spare-pieces black))
        (json:encode-object-member :white-spares (spare-pieces white))
        (json:encode-object-member :turn turn)
        (json:encode-object-member :last-roll last-roll)))))

(defun opponent-color (color)
  (ecase color
    (:white :black)
    (:black :white)))

(defun game-player (game color)
  (with-slots (white black) game
    (ecase color
      (:white white)
      (:black black))))

(defun active-player (game)
  (game-player game (turn game)))

(defun opponent-player (game)
  "Retun the player waiting for their turn."
  (game-player game (opponent-color (turn game))))

(defun game-phase (game)
  (if (slot-value game 'last-roll)
      :move-phase
      :roll-phase))

(defun player-tile (game index)
  "Return ownership of the player's effective tile by index."

  (decf index) ; 1-based; playing from tile 0 means placing a new piece on the table.

  (let ((player (active-player game))
        (shared-index (- index +start-length+))
        (end-index (- index +start-length+ +shared-length+)))
    (cond
      ;; Return from the players' starting path if within its range
      ((< index +start-length+)
       (aref (start-path player) index))
      ;; Otherwise, return from the shared path if within +that+ range
      ((< shared-index +shared-length+)
       (aref (shared-path game) shared-index))
      ;; Finally, the ending path is the last portion, so alway fall back to
      ;; accessing from here.
    (t (aref (end-path player) end-index)))))

(defun (setf player-tile) (new-owner game index)
  "Set the ownership of the player's effective tile by index."

  (decf index) ; 1-based; playing from tile 0 means placing a new piece on the table.

  (let ((player (active-player game))
        (shared-index (- index +start-length+))
        (end-index (- index +start-length+ +shared-length+)))
    (cond
      ;; Return from the players' starting path if within its range
      ((< index +start-length+)
       (setf (aref (start-path player) index) new-owner))
      ;; Otherwise, return from the shared path if within +that+ range
      ((< shared-index +shared-length+)
       (setf (aref (shared-path game) shared-index) new-owner))
      ;; Finally, the ending path is the last portion, so alway fall back to
      ;; accessing from here.
      (t (setf (aref (end-path player) end-index) new-owner)))))

(defun valid-move (game index)
  "Return two values: Whether the move is valid, and the type of move."
  (with-slots (last-roll turn) game
    (unless (eq :move-phase (game-phase game))
      (return-from valid-move (values nil :not-rolled-yet)))

    (let ((dest-index (+ index last-roll)))
      (cond
        ((< index 0) (values nil :bad-tile))
        ((and (zerop index) (zerop (spare-pieces (active-player game)))) (values nil :no-spare-pieces))
        ((> dest-index (1+ +path-length+)) (values nil :too-far))
        ((and (> index 0) (not (eq (player-tile game index) turn))) (values nil :unowned-tile))
        ((= dest-index (1+ +path-length+)) (values t :completed-piece))
        ((eq (player-tile game dest-index) turn) (values nil :cant-capture-own-tile))
        ((eq (player-tile game dest-index) :none) (values t (if (rosettep dest-index)
                                                                :landed-on-rosette
                                                                :moved-piece)))
        ((rosettep dest-index) (values nil :protected-tile))
        (t (values t :captured-piece))))))

(defun valid-turn-p (game)
  "Return whether the current player can make a valid move"
  (loop for index :from 0 :to +path-length+
     :when (valid-move game index) :do (return t)))

(defun player-tiles (game color)
  "Return a vector of all the players' tiles"
  (let ((player (game-player game color)))
    (concatenate 'vector
                 (start-path player)
                 (shared-path game)
                 (end-path player))))

(defun winner (game)
  "Return the game winner's color, or NIL if the game is still ongoing."
  (flet ((is-player-empty (player)
           (loop :for tile :across (player-tiles game player)
              :when (eq tile player) :do (return nil)
              :finally (return t))))
    (cond
      ((and (zerop (spare-pieces (white-player game)))
            (is-player-empty :white))
       :white)
      ((and (zerop (spare-pieces (black-player game)))
            (is-player-empty :black))
       :black))))

(defun next-turn (game)
  (with-slots (last-roll turn) game
    (setf turn (opponent-color (turn game)))
    (setf last-roll nil)))

(defun offer-draw (game color)
  "Mark the player as having offered a draw, and return whether both players agree."
  (let ((player (game-player game color)))
    (setf (draw-offered player) t)
    (draw-offered (game-player game (opponent-color color)))))

(defun clear-draws (game)
  (setf (draw-offered (game-player game :white)) nil
        (draw-offered (game-player game :black)) nil))

(defun random-roll (&optional (random-state *random-state*))
  (let ((flips (loop :repeat 4
                  :collect (random 2 random-state))))
    (values (reduce #'+ flips) flips)))

(defun roll (game)
  "Toss four coins and sum the total, providing a similar D4 allegedly played
   in the original game. Return 4 values: Whether the move was made, the total
   flips up, the individual flips, whether it caused the turn to skip, and (if
                                                                             applicable)
   the reason why the move failed."
  (with-slots (random-state last-roll) game
    ;; TODO use conditions to go for failure instead. Or maybe a Result monad.
    ;; It makes better sense here.
    (if (not (eq :roll-phase (game-phase game)))
        (values nil nil nil nil :already-rolled)
        (multiple-value-bind (total flips) (random-roll random-state)
          (setf last-roll total)
          (clear-draws game)
          (let ((bad-roll (cond ((= total 0) :flipped-nothing)
                                  ((not (valid-turn-p game)) :no-valid-moves))))
            (when bad-roll (next-turn game))
            (values t total flips bad-roll))))))

(defun reset-roll (game)
  (setf (last-roll game) nil))

(defun move-piece (game index)
  "Move a piece from tile INDEX to the last roll. Return the destination tile's index."
  (with-slots (last-roll turn) game
    (let ((dest-index (+ index last-roll)))
      (if (= index 0)
          (decf (spare-pieces (active-player game)))
          (setf (player-tile game index) :none))

      (when (<= dest-index +path-length+)
        (when (eq (player-tile game dest-index) (opponent-color (turn game)))
          (incf (spare-pieces (opponent-player game))))
        (setf (player-tile game dest-index) turn))
      dest-index)))

(defun make-move (game position)
  "Attempt to move a piece from the given position. Return 3 values: whether
   it's successful, that type of move that was (or would have been) made, and
   whether the turn ended."
  (check-type position integer)
  (multiple-value-bind (successful move-type) (valid-move game position)
    (if (not successful)
        (values nil move-type)
        (progn
          (move-piece game position)
          (clear-draws game)
          (if (eq move-type :landed-on-rosette)
              (reset-roll game)
              (next-turn game))
          (values t move-type (not (eq move-type :landed-on-rosette)))))))
