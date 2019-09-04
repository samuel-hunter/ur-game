(defpackage #:ur-game.engine
  (:use :cl)
  (:export :+start-length+
           :+shared-length+
           :+end-length+
           :+path-length+

           :rosettep

           :game
           :white-start
           :black-start
           :shared-path
           :white-end
           :black-end
           :white-spare-pieces
           :black-spare-pieces
           :turn
           :last-roll
           :rolledp

           :roll
           :player-spare-pieces
           :opponent-spare-pieces
           :make-move
           :winner))

(in-package #:ur-game.engine)

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
   (turn :initform :white :reader turn)
   (random-state :initform (make-random-state t))
   (last-roll :initform nil)))

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
  (ecase (turn game)
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
  (with-slots (last-roll turn) game
    (unless last-roll (return-from valid-move (values nil :not-rolled-yet)))

    (let ((dest-index (+ index last-roll)))
      (cond
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
  (loop for index :from 0 :to +path-length+
     :when (valid-move game index) :do (return t)))

(defun winner (game)
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
  (with-slots (last-roll turn) game
    (setf turn (opponent game))
    (setf last-roll nil)))

(defun random-roll (&optional (random-state *random-state*))
  (let ((flips (loop :repeat 4
                  :collect (random 2 random-state))))
    (values (reduce #'+ flips) flips)))

(defun profile-roll (&key (random-state *random-state*) (iterations 10000))
  "Print two values: an alist of the distribution of rolls, and an alist of the theoretical outcome."
  (let ((nums (make-hash-table)))
    (loop :for x :from 0 :to 4 :do (setf (gethash x nums) 0))
    (loop :repeat iterations
       :do (incf (gethash (random-roll random-state) nums)))
    (format t "Actual:")
    (loop :for x :from 0 :to 4 :do (print (cons x (gethash x nums))))
    (format t "~&~%Expected:")
    (loop
        :for num :from 0 :to 4
        :for appearances :in '(1/16 4/16 6/16 4/16 1/16)
        :do (print (cons num (coerce (* iterations appearances) 'float))))))

(defun profile-roll-2d (&key (random-state *random-state*) (iterations 10000))
  "Return a 2-D distribution of the rolls"
  (let ((nums (make-hash-table :test 'equal)))
    (loop :for x :from 0 :to 4
       :do (loop :for y :from 0 :to 4
              :do (setf (gethash (cons x y) nums) 0)))
    (loop :repeat iterations
       :for x = (random-roll random-state)
       :for y = (random-roll random-state)
       :do (incf (gethash (cons x y) nums)))

    (format t "~&~{~{~8d~}~%~}"
            (loop :for y :from 0 :to 4
               :collect (loop :for x :from 0 :to 4
                           :collect (gethash (cons x y) nums))))

    (format t "~2&~{~{~8d~}~%~}"
            (loop :for y :in '(1/16 4/16 6/16 4/16 1/16)
               :collect (loop :for x :in '(1/16 4/16 6/16 4/16 1/16)
                             :collect (floor (* iterations x y)))))))

(defun roll (game)
  "Toss four coins and sum the total, providing a similar D4 allegedly
played in the original game. Store the sum in the game."
  (with-slots (random-state last-roll) game
    (when last-roll
      (return-from roll (values :already-rolled nil)))

    (multiple-value-bind (total flips) (random-roll random-state)

      (setf last-roll total)
      (cond
        ;; If the roll was 0, the current player has lost their turn.
        ((= total 0)
         (next-turn game)
         (values total t flips :flipped-nothing))
        ;; If the player has no valid moves, they skip their turn.
        ((not (valid-turn-p game))
         (next-turn game)
         (values total t flips :no-valid-moves))
        (t
         (values total t flips nil))))))

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


(defun make-move (game index)
  (with-slots (last-roll turn) game
    (multiple-value-bind (is-valid move-type) (valid-move game index)
      (when is-valid
        (let ((dest-index (move-tile game index)))
          (if (rosettep dest-index)
              (setf last-roll nil)
              (next-turn game))))
      (values move-type is-valid))))
