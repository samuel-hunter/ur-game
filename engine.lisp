(defpackage #:ur-game.engine
  (:use :cl :ur-game.json)
  (:import-from :alexandria
                :switch)
  (:export :+start-length+
           :+shared-length+
           :+end-length+
           :+path-length+

           :rosettep

           :opponent-player

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

           :winner
           :action-successful
           :process-action))

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

(defmethod json:encode-json ((object game) &optional (stream json:*json-output*))
  (encode-json-select-slots object
                            '(white-start black-start shared-path white-end
                              black-end white-spare-pieces black-spare-pieces
                              turn last-roll)
                            stream))

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

(defun opponent-player (player)
  (ecase player
    (:white :black)
    (:black :white)))

(defun opponent (game)
  "Retun the player waiting for their turn."
  (opponent-player (turn game)))

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

(defun player-tiles (game player)
  "Return a vector of all the players' tiles"
  (with-slots (white-start black-start shared-path white-end black-end) game
    (ecase player
      (:white (concatenate 'vector white-start shared-path white-end))
      (:black (concatenate 'vector black-start shared-path black-end)))))

(defun winner (game)
  "Return the winner of the game, or NIL if the game is still going."
  (flet ((is-player-empty (player)
           (loop :for tile :across (player-tiles game player)
              :when (eq tile player) :do (return nil)
              :finally (return t))))
    (with-slots (white-spare-pieces black-spare-pieces) game
      (cond
        ((and (= white-spare-pieces 0)
              (is-player-empty :white))
         :white)
        ((and (= black-spare-pieces 0)
              (is-player-empty :black))
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
      (return-from roll (list :successful (make-instance 'json-bool
                                                         :p nil)
                              :reason :already-rolled)))

    (multiple-value-bind (total flips) (random-roll random-state)

      (setf last-roll total)
      (let ((skip-turn (cond
                         ((= total 0) :flipped-nothing)
                         ((not (valid-turn-p game)) :no-valid-moves))))
        (when skip-turn (next-turn game))
        (list :successful (make-instance 'json-bool
                                         :p t)
              :total total
              :flips flips
              :skip-turn (make-instance 'json-bool
                                        :p skip-turn
                                        :generalised t))))))

(defun move-piece (game index)
  "Move a piece from tile INDEX to the last roll. Return the destination tile's index."
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


(defun make-move (game position)
  (unless (integerp position)
    (return-from make-move (list :successful nil
                                 :reason :invalid-position)))

  (multiple-value-bind (successful move-type) (valid-move game position)
    (let ((successful-json (make-instance 'json-bool :p successful)))
      (if successful
          (progn
            (move-piece game position)
            (if (eq move-type :landed-on-rosette)
                (setf (slot-value game 'last-roll) nil)
                (next-turn game))
            (list :successful successful-json
                  :turn-end t
                  :move-type move-type))
          (list :successful successful-json
                :reason move-type)))))

(defun process-action (game action)
  (switch ((cdr (assoc :op action)) :test 'string-equal)
    ("roll" (list* :op :roll
                   (roll game)))
    ("move" (let ((position (cdr (assoc :position action))))
              (list* :op :move
                     (make-move game position))))
    (otherwise (list :op :err
                     :reason :no-such-operand))))

(defun action-successful (action-result)
  (json-bool-p (getf action-result :successful)))
