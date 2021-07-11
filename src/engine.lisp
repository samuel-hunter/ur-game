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



;; path lengths
(defparameter +start-length+ 4)
(defparameter +middle-length+ 8)
(defparameter +end-length+ 2)
(defparameter +path-length+
  (+ +start-length+ +middle-length+ +end-length+))

(defparameter +starting-pieces+ 7)

;; Board Data
(defclass path-segment ()
  ((tile-vector :initarg :tile-vector :reader tile-vector)
   (black-next :initarg :black-next :reader black-next
               :documentation "Next path segment for black checkers")
   (white-next :initarg :white-next :reader white-next
               :documentation "Next path segment for white checkers"))
  (:documentation "A segment of a route that a game checker travels to reach the end."))

(defun next-segment (path-segment color)
  (ecase color
    (:black (black-next path-segment))
    (:white (white-next path-segment))))

(defun route-length (path-segment color)
  "Return the length of the entire route for the given color."
  (loop :for segment := path-segment :then (next-segment segment color)
        :while segment
        :sum (length (tile-vector segment))))

(defun find-tile-vector (n path-segment color)
  "If n is in-bounds, return two values: the vector that contains the nth tile,
   and the offset to access that tile. Otherwise, return nil."
  (when (null path-segment)
    (return-from find-tile-vector))

  (let* ((tile-vector (tile-vector path-segment))
         (length (length tile-vector)))
    (if (< n length)
        (values tile-vector n)
        (find-tile-vector (- n length)
                          (next-segment path-segment color)
                          color))))

(defun nth-tile (n path-segment color)
  "Return the nth tile for the given color's route."
  (multiple-value-bind (vector relative-n)
    (find-tile-vector n path-segment color)

    (when vector
      (aref vector relative-n))))

(defun (setf nth-tile) (new-value n path-segment color)
  "Set the nth tile for the given color's route."
  (multiple-value-bind (vector relative-n)
    (find-tile-vector n path-segment color)

    (when vector
      (setf (aref vector relative-n) new-value))))

(defun route-tiles (path-segment color)
  "Return a vector of all path segments concatenated together."
  (loop :for segment := path-segment :then (next-segment segment color)
        :while segment
        :collect (tile-vector segment) :into vectors
        :finally (return (apply #'concatenate 'vector vectors))))

(defclass board ()
  ((black-start :initarg :black-start)
   (white-start :initarg :white-start)
   (shared-middle :initarg :shared-middle)
   (black-end :initarg :black-end)
   (white-end :initarg :white-end))
  (:documentation "Collection of all tile vectors"))

(defun make-tile-vector (length)
  (make-array length :initial-element :none))

(defun make-board-and-paths ()
  "Return three values: the game board, the black starting path segment, and
   the white starting path segment."
  (let* ((black-start (make-tile-vector +start-length+))
         (white-start (make-tile-vector +start-length+))
         (shared-middle (make-tile-vector +middle-length+))
         (black-end (make-tile-vector +end-length+))
         (white-end (make-tile-vector +end-length+))

         (shared-path-segment
           (make-instance 'path-segment
                          :tile-vector shared-middle
                          :black-next (make-instance 'path-segment
                                                     :tile-vector black-end)
                          :white-next (make-instance 'path-segment
                                                     :tile-vector white-end))))
    (values (make-instance 'board
                           :black-start black-start
                           :white-start white-start
                           :shared-middle shared-middle
                           :black-end black-end
                           :white-end white-end)
            (make-instance 'path-segment
                           :tile-vector black-start
                           :black-next shared-path-segment)
            (make-instance 'path-segment
                           :tile-vector white-start
                           :white-next shared-path-segment))))

(defun rosettep (index)
  "Return whether the tile index has a rosette"
  (member index '(4 8 14)))



(defclass player ()
  ((start-path :initarg :start-path
               :reader start-path)
   (spare-pieces :initform +starting-pieces+
                 :accessor spare-pieces)
   (draw-offered :initform nil
                 :accessor draw-offered)))

(defclass game ()
  ((random-state :initform (make-random-state t))
   (black :reader black-player)
   (white :reader white-player)
   (board :reader board)
   (turn :initform :white
         :accessor turn)
   (last-roll :initform nil
              :accessor last-roll)))

(defmethod initialize-instance :after ((instance game) &key &allow-other-keys)
  (multiple-value-bind (new-board black-path white-path) (make-board-and-paths)
    (with-slots (black white board) instance
      (setf black (make-instance 'player :start-path black-path)
            white (make-instance 'player :start-path white-path)
            board new-board))))

;; TODO figure out how to separate this last piece of presentation from the
;; game engine.
(defmethod json:encode-json ((object game) &optional (stream json:*json-output*))
  (with-slots (black white board shared-path turn last-roll) object
    (let ((json:*json-output* stream))
      (json:with-object ()
        (json:encode-object-member :board board)
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

(defun active-player-tile (game index)
  "Return ownership of the active player's effective tile by index."
  (decf index) ; 1-based; playing from tile 0 means placing a new piece on the table.
  (nth-tile index (start-path (active-player game)) (turn game)))

(defun (setf active-player-tile) (new-value game index)
  "Set the ownership of the active player's effective tile by index."
  (decf index) ; 1-based; playing from tile 0 means placing a new piece on the table.
  (setf (nth-tile index (start-path (active-player game)) (turn game))
        new-value))

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
        ((and (> index 0) (not (eq (active-player-tile game index) turn))) (values nil :unowned-tile))
        ((= dest-index (1+ +path-length+)) (values t :completed-piece))
        ((eq (active-player-tile game dest-index) turn) (values nil :cant-capture-own-tile))
        ((eq (active-player-tile game dest-index) :none) (values t (if (rosettep dest-index)
                                                                :landed-on-rosette
                                                                :moved-piece)))
        ((rosettep dest-index) (values nil :protected-tile))
        (t (values t :captured-piece))))))

(defun valid-turn-p (game)
  "Return whether the current player can make a valid move"
  (loop for index :from 0 :to +path-length+
     :when (valid-move game index) :do (return t)))

(defun player-tiles (game color)
  (route-tiles  (start-path (game-player game color))
                color))

(defun winner (game)
  "Return the game winner's color, or NIL if the game is still ongoing."
  (flet ((is-player-empty (player)
           (loop :for tile :across (player-tiles game player)
                 :never (eq tile player))))
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
          (setf (active-player-tile game index) :none))

      (when (<= dest-index +path-length+)
        (when (eq (active-player-tile game dest-index) (opponent-color (turn game)))
          (incf (spare-pieces (opponent-player game))))
        (setf (active-player-tile game dest-index) turn))
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
