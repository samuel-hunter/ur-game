(defpackage #:ur-game
  (:use #:cl))

(in-package #:ur-game)

(defun make-empty-path (length)
  (make-array length :initial-element :none))

;; path lengtsh
(defparameter +start-length+ 4)
(defparameter +shared-length+ 8)
(defparameter +end-length+ 2)

(defparameter +path-length+ (+ +start-length+ +shared-length+ +end-length+))

(defparameter +starting-pieces+ 7)


;; Rosette tiles protect pieces from battle and give the player a
;; second turn
(defparameter +rosettes+ '(4 8 14))

(defmacro whenlet ((varname cond) &body forms)
  `(let ((,varname ,cond))
     (when ,varname ,@forms)))

(defun rosettep (index)
  "Return whether the tile has a rosette"
  (member index +rosettes+))


(defclass game ()
  ((white-start :initform (make-empty-path +start-length+))
   (black-start :initform (make-empty-path +start-length+))
   (shared-path :initform (make-empty-path +shared-length+))
   (white-end :initform (make-empty-path +end-length+))
   (black-end :initform (make-empty-path +end-length+))
   (white-spare-pieces :initform +starting-pieces+)
   (black-spare-pieces :initform +starting-pieces+)
   (turn :initform :white :reader player-turn)
   (random-state :initform (make-random-state t))
   (last-roll)
   (rolledp :initform nil)))

(defun player-spare-pieces (game)
  (with-slots (turn white-spare-pieces black-spare-pieces) game
    (ecase turn
      (:white white-spare-pieces)
      (:black black-spare-pieces))))

(defun (setf player-spare-pieces) (new-value game)
  (with-slots (turn white-spare-pieces black-spare-pieces) game
    (ecase turn
      (:white (setf white-spare-pieces new-value))
      (:black (setf black-spare-pieces new-value)))))

(defun opponent-spare-pieces (game)
  (with-slots (turn white-spare-pieces black-spare-pieces) game
    (ecase turn
      (:white black-spare-pieces)
      (:black white-spare-pieces))))

(defun (setf opponent-spare-pieces) (new-value game)
  (with-slots (turn white-spare-pieces black-spare-pieces) game
    (ecase turn
      (:white (setf black-spare-pieces new-value))
      (:black (setf white-spare-pieces new-value)))))

(defun opponent (game)
  "Retun the player waiting for their turn."
  (ecase (player-turn game)
    (:white :black)
    (:black :white)))

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

  (let ((shared-index (- index +start-length+))
        (end-index (- index +start-length+ +shared-length+)))
    (cond
      ;; Return from the players' starting path if within its range
      ((< index +start-length+)
       (aref (player-start game) index))
      ;; Otherwise, return from the shared path if within +that+ range
      ((< shared-index +shared-length+)
       (aref (slot-value game 'shared-path) shared-index))
      ;; Finally, the ending path is the last portion, so alway fall back to
      ;; accessing from here.
    (t (aref (player-end game) end-index)))))

(defun (setf player-tile) (new-owner game index)
  "Set the ownership of the player's effective tile by index."

  (decf index) ; 1-based; playing from tile 0 means placing a new piece on the table.

  (let ((shared-index (- index +start-length+))
        (end-index (- index +start-length+ +shared-length+)))
    (cond
      ;; Return from the players' starting path if within its range
      ((< index +start-length+)
       (setf (aref (player-start game) index) new-owner))
      ;; Otherwise, return from the shared path if within +that+ range
      ((< shared-index +shared-length+)
       (setf (aref (slot-value game 'shared-path) shared-index) new-owner))
      ;; Finally, the ending path is the last portion, so alway fall back to
      ;; accessing from here.
      (t (setf (aref (player-end game) end-index) new-owner)))))

(defun valid-move (game index)
  "Return two values: Whether the move is valid, and the type of move."
  (with-slots (rolledp last-roll turn) game
    (let ((dest-index (+ index last-roll)))
      (cond
        ((not rolledp) (values nil :not-rolled-yet))
        ((< index 0) (values nil :bad-tile))
        ((and (zerop index) (zerop (player-spare-pieces game))) (values nil :no-spare-pieces))
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
  (loop for index :from 1 :to +path-length+
     :when (valid-move game index) :do (return t)))

(defun game-winner (game)
  "Return the winner of the game, or NIL if the game is still going."
  (flet ((is-player-empty (path player)
           (loop :for tile :across path
              :when (eq tile player) :do (return nil)
              :finally (return t))))
    (with-slots (white-spare-pieces black-spare-pieces white-start black-start
                                    shared-path white-end black-end) game
      (cond
        ((and (= white-spare-pieces 0)
              (is-player-empty (concatenate 'vector white-start shared-path white-end) :white))
         :white)
        ((and (= black-spare-pieces 0)
              (is-player-empty (concatenate 'vector black-start shared-path black-end) :black))
         :black)))))

(defun next-turn (game)
  (with-slots (rolledp turn) game
    (setf turn (opponent game))
    (setf rolledp nil)))

(defun roll (game)
  "Toss four coins and sum the total, providing a similar D4 allegedly
played in the original game. Store the sum in the game."
  (with-slots (random-state last-roll rolledp) game
    (when rolledp
      (return-from roll :already-rolled))

    (let* ((flips (loop :repeat 4
                     :collect (random 2 random-state)))
           (total (reduce #'+ flips)))

      (setf last-roll total)
      (setf rolledp t)
      (cond
        ;; If the roll was 0, the current player has lost their turn.
        ((= total 0)
         (next-turn game)
         (values total flips :flipped-nothing))
        ;; If the player has no valid moves, they skip their turn.
        ((not (valid-turn-p game))
         (next-turn game)
         (values total flips :no-valid-moves))
        (t
         (values total flips nil))))))

(defun move-tile (game index)
  "Move a tile from INDEX to the last roll. Return the destination tile's index."
  (with-slots (last-roll turn) game
    (let ((dest-index (+ index last-roll)))
      (if (= index 0)
          (decf (player-spare-pieces game))
          (setf (player-tile game index) :none))

      (when (<= dest-index +path-length+)
        (when (eq (player-tile game dest-index) (opponent game))
          (incf (opponent-spare-pieces game)))
        (setf (player-tile game dest-index) turn))
      dest-index)))


;; TODO rewrite make-turn with valid-turn
(defun make-turn (game index)
  (with-slots (rolledp last-roll turn) game
    (multiple-value-bind (is-valid move-type) (valid-move game index)
      (when is-valid
        (let ((dest-index (move-tile game index)))
          (if (rosettep dest-index)
              (setf rolledp nil)
              (next-turn game))))
      (list is-valid move-type))))

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

(defun game-eval (form game)
  (cond
    ((integerp form) (cons :turn (make-turn game form)))
    ((eq form 'roll) (cons :roll (multiple-value-list (roll game))))
    ((eq form 'quit) :quit)
    (t :bad-input)))

(defun game-repl ()
  (let ((game (make-instance 'game)))
    (setf (slot-value game 'white-spare-pieces) 1)
    (write-game game)
    (loop
       :for result = (game-eval (read) game)
       :until (or (eq result :quit) (game-winner game))
       :do (progn (format t "~A~%" result)
                  (when (not (slot-value game 'rolledp))
                    (write-game game))))
    (whenlet (winner (game-winner game))
             (format t "The winner is ~A~%" winner))))
