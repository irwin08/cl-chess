

(defclass board-state ()
  ((current-move
    :accessor board-state-current-move
    :initarg :current-move
    :initform 'white) ;; 'black or 'white
   (white-q-castle-available
    :accessor board-white-q-castle-available
    :initarg :white-q-castle-available
    :initform nil)
   (white-k-castle-available
    :accessor board-white-k-castle-available
    :initarg :white-k-castle-available
    :initform nil)
   (black-q-castle-available
    :accessor board-black-q-castle-available
    :initarg :black-q-castle-available
    :initform nil)
   (black-k-castle-available
    :accessor board-black-k-castle-available
    :initarg :black-k-castle-available
    :initform nil)
   (board
    :accessor board-state-board
    :initarg :board
    :initform (error "Board not created!"))))

(defun make-board-state (&key current-move white-k-castle-available white-q-castle-available black-q-castle-available black-k-castle-available)
  (make-instance 'board-state
		 :current-move (if current-move current-move 'white)
		 :white-q-castle-available white-q-castle-available
		 :white-k-castle-available white-k-castle-available
		 :black-q-castle-available black-q-castle-available
		 :black-k-castle-available black-k-castle-available
		 :board (start-board)))

(defun start-board ()
  (let (( local-board (make-array '(8 8) :initial-contents
	      `((,(make-rook :color 'white) ,(make-knight :color 'white) ,(make-bishop :color 'white) ,(make-queen :color 'white) ,(make-king :color 'white) ,(make-bishop :color 'white) ,(make-knight :color 'white) ,(make-rook :color 'white))
		(nil ,(make-pawn :color 'white) ,(make-pawn :color 'white) ,(make-pawn :color 'white) ,(make-pawn :color 'white) ,(make-pawn :color 'white) ,(make-pawn :color 'white) ,(make-pawn :color 'white))
		(nil nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil nil)
		(nil nil nil nil nil nil nil nil)
		(,(make-pawn :color 'black) ,(make-pawn :color 'black) ,(make-pawn :color 'black) ,(make-pawn :color 'black) ,(make-pawn :color 'black) ,(make-pawn :color 'black) ,(make-pawn :color 'black) ,(make-pawn :color 'black))
		(,(make-rook :color 'black) ,(make-knight :color 'black) ,(make-bishop :color 'black) ,(make-queen :color 'black) ,(make-king :color 'black) ,(make-bishop :color 'black) ,(make-knight :color 'black) ,(make-rook :color 'black))))))
  (loop for i from 0 to 7 do
	(loop for j from 0 to 7 do
	      (when (aref local-board i j)
		(setf (piece-location (aref local-board i j)) `(,i ,j)))))
    local-board))

(defclass piece ()
  ((color
    :accessor piece-color
    :initarg :color
    :initform (error "No color set!"))
   (location
    :accessor piece-location)))

(defgeneric available-moves (piece board)
  :documentation "Provides a list of available moves to the piece on the given board state")


(defclass rook (piece) ())

(defun make-rook (&key color)
  (make-instance 'rook
		 :color color))

(defmethod available-moves ((piece rook) board)
  (let ((possible-moves '()))
    ;; check up
    (let ((rook-i (first (piece-location piece)))
	  (rook-j (+ 1 (second (piece-location piece)))))
      (loop while (and (< rook-j 8) (not (aref board rook-i rook-j))) do
	(setf possible-moves (cons `(,rook-i ,rook-j) possible-moves))
	(setf rook-j (+ 1 rook-j))
	    )
      (when (and (< rook-j 8) (not (aref board rook-i rook-j)))
	(setf possible-moves (cons `(,rook-i ,rook-j) possible-moves))))

    ;; check down
    (let ((rook-i (first (piece-location piece)))
	  (rook-j (- 1 (second (piece-location piece)))))
      (loop while (and (>= rook-j 0) (not (aref board rook-i rook-j))) do
	(setf possible-moves (cons `(,rook-i ,rook-j) possible-moves))
	(setf rook-j (- 1 rook-j))
	    )
      (when (and (>= rook-j 0) (not (aref board rook-i rook-j)))
	(setf possible-moves (cons `(,rook-i ,rook-j) possible-moves))))

        ;; check left
    (let ((rook-i (- 1 (first (piece-location piece))))
	  (rook-j (second (piece-location piece))))
      (loop while (and (>= rook-i 0) (not (aref board rook-i rook-j))) do
	(setf possible-moves (cons `(,rook-i ,rook-j) possible-moves))
	(setf rook-i (- 1 rook-i))
	    )
      (when (and (>= rook-i 0) (not (aref board rook-i rook-j)))
	(setf possible-moves (cons `(,rook-i ,rook-j) possible-moves))))

        ;; check right
    (let ((rook-i (+ 1 (first (piece-location piece))))
	  (rook-j (second (piece-location piece))))
      (loop while (and (< rook-i 8) (not (aref board rook-i rook-j))) do
	(setf possible-moves (cons `(,rook-i ,rook-j) possible-moves))
	(setf rook-i (+ 1 rook-i))
	    )
      (when (and (< rook-i 8) (not (aref board rook-i rook-j)))
	(setf possible-moves (cons `(,rook-i ,rook-j) possible-moves))))
    
    possible-moves))

(defclass knight (piece) ())

(defun make-knight (&key color)
  (make-instance 'knight
		 :color color))

(defclass bishop (piece) ())

(defun make-bishop (&key color)
  (make-instance 'bishop
		 :color color))

(defclass queen (piece) ())

(defun make-queen (&key color)
  (make-instance 'queen
		 :color color))

(defclass king (piece) ())

(defun make-king (&key color)
  (make-instance 'king
		 :color color))

(defclass pawn (piece)
  ((en-passantable ;; set to true after moving two spaces, then remove the following turn
    :accessor en-passantable
    :initform nil)))

(defun make-pawn (&key color)
  (make-instance 'pawn
		 :color color))


