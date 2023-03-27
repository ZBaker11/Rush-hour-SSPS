
(defclass state ()
	(
	(name :accessor state-name :initarg :name)
	(h-heads :accessor state-h-heads :initarg :h-heads)
	(v-heads :accessor state-v-heads :initarg :v-heads)
	(h-lengths :accessor state-h-lengths :initarg :h-lengths)
	(v-lengths :accessor state-v-lengths :initarg :v-lengths)
	(board :accessor state-board :initarg :board)
	(num-v-vehicles :accessor state-num-v-vehicles :initarg :num-v-vehicles)
	(num-h-vehicles :accessor state-num-h-vehicles :initarg :num-h-vehicles)
	(parent :accessor state-parent)
	(operator :accessor state-operator)
	)
)

(defmethod setup (&aux root h-heads v-heads h-lengths v-lengths)
	(setf h-heads '((2 1) (0 1) (4 2)))
	(setf v-heads '((2 4) (1 3) (0 0)))
	(setf h-lengths '(2 3 3))
	(setf v-lengths '(2 3 3))
	(setf num-v-vehicles 3)
	(setf num-h-vehicles 3)
	(setf board (state-populate h-heads v-heads h-lengths v-lengths num-h-vehicles num-v-vehicles))
	
	;init root
	(setf root (make-instance 'state :name 'root :h-heads h-heads :v-heads v-heads :h-lengths h-lengths :v-lengths v-lengths :board board :num-v-vehicles num-v-vehicles :num-h-vehicles num-h-vehicles))
	;init list of unexplored nodes
	(setf *unexplored* (list root))
	;init list of explored nodes
	(setf *explored* '())
	(format t "SSPS initialized with root node: ~%")
	(display-state root)
	
	(state-gather-left-moves root)
	(display-state root)
	nil
)


(defun state-populate (h-heads v-heads h-lengths v-lengths num-h-vehicles num-v-vehicles)
	; POPULATE BOARD
	(setf tempboard (make-array '(6 6) :element-type 'bit :initial-element 0))
	
	; HORIZONTALS
	(loop for i from 0 below num-h-vehicles do
		(setf vehicle-head-x (car (last (nth i h-heads))))
		(setf vehicle-head-y (first (nth i h-heads)))
		(setf vehicle-length (nth i h-lengths))
			(loop for i from vehicle-head-x to (+ vehicle-head-x vehicle-length -1) do
				(setf (aref tempboard vehicle-head-y i) 1)
			)
	)
	
	; VERTICALS
	(loop for i from 0 below num-v-vehicles do
		(setf vehicle-head-x (car (last (nth i v-heads))))
		(setf vehicle-head-y (first (nth i v-heads)))
		(setf vehicle-length (nth i v-lengths))
			(loop for i from vehicle-head-y to (+ vehicle-head-y vehicle-length -1) do
					(setf (aref tempboard i vehicle-head-x) 1)
			)
	)
	tempboard
)


(defmethod state-gather-up-moves ((s state) &aux vehicle-head-x vehicle-head-y ups)
	(setf ups '())
	(loop for i from 0 below (state-num-v-vehicles s) do 
		(setf vehicle-head-x (car (last (nth i (state-v-heads s)))))
		(setf vehicle-head-y (first (nth i (state-v-heads s))))
		(if (and (> vehicle-head-y 0) (= 0 (aref (state-board s) (- vehicle-head-y 1) vehicle-head-x))) ; legality check
			(setf ups (cons i ups))
		)
	)
	ups
)


(defmethod state-gather-down-moves ((s state) &aux vehicle-head-x vehicle-head-y downs)
	(setf downs '())
	(loop for i from 0 below (state-num-v-vehicles s) do
		(setf vehicle-head-x (car (last (nth i (state-v-heads s)))))
		(setf vehicle-head-y (first (nth i (state-v-heads s))))
		(setf length (nth i (state-v-lengths)))
		(if (and (< (+ vehicle-head-y length -1) 5) (= 0 (aref (state-board s) (+ vehicle-head-y length) vehicle-head-x)))
			(setf downs (cons i downs))
		)
	)
	downs
)


(defmethod state-gather-left-moves ((s state) &aux vehicle-head-x vehicle-head-y lefts)
	(setf lefts '())
	(loop for i from 0 below (state-num-h-vehicles s) do
		(setf vehicle-head-x (car (last (nth i (state-h-heads s)))))
		(setf vehicle-head-y (first (nth i (state-h-heads s))))
		(if (and (> vehicle-head-x 0) (= 0 (aref (state-board s) vehicle-head-y (- vehicle-head-x 1))))
			(setf lefts (cons i lefts))
		)
	)
	lefts
)


(defmethod state-gather-right-moves ((s state) &aux vehicle-head-x vehicle-head-y rights)
	(setf rights '())
	(loop for i from 0 below (state-num-h-vehicles s) do 
		(setf vehicle-head-x (car (last (nth i (state-h-heads s)))))
		(setf vehicle-head-y (first (nth i (state-h-heads s))))
		(setf length (nth i (state-h-lengths s)))
		(if (and (< (+ vehicle-head-x length -1) 5) (= 0 (aref (state-board s) vehicle-head-y (+ vehicle-head-x length))))
			(setf rights (cons i rights))
		)
	)
	rights
)


(defmethod state-move-up ((s state) vehicle &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (nth vehicle (state-v-heads s)))))
	(setf vehicle-head-y (first (nth vehicle (state-v-heads s))))
	(setf length (nth vehicle (state-v-lengths s)))
	
	(setf (aref (state-board s) (- vehicle-head-y 1) vehicle-head-x) 1)
	(setf (aref (state-board s) (+ vehicle-head-y length -1) vehicle-head-x) 0)
	(setf (nth vehicle (state-v-heads s)) (list (- vehicle-head-y 1) vehicle-head-x))
)

(defmethod state-move-down ((s state) vehicle &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (nth vehicle (state-v-heads s)))))
	(setf vehicle-head-y (first (nth vehicle (state-v-heads s))))
	(setf length (nth vehicle (state-v-lengths s)))
	
	(setf (aref (state-board s) (+ vehicle-head-y length) vehicle-head-x) 1)
	(setf (aref (state-board s) vehicle-head-y vehicle-head-x) 0)
	(setf (nth vehicle (state-v-heads s)) (list (+ vehicle-head-y 1) vehicle-head-x))
)

(defmethod state-move-left ((s state) vehicle &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (nth vehicle (state-h-heads s)))))
	(setf vehicle-head-y (first (nth vehicle (state-h-heads s))))
	(setf length (nth vehicle (state-h-lengths s)))
	
	(setf (aref (state-board s) vehicle-head-y (- vehicle-head-x 1)) 1)
	(setf (aref (state-board s) vehicle-head-y (+ vehicle-head-x length -1)) 0)
	(setf (nth vehicle (state-h-heads s)) (list vehicle-head-y (- vehicle-head-x 1)))
)

(defmethod state-move-right ((s state) vehicle &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (nth vehicle (state-h-heads s)))))
	(setf vehicle-head-y (first (nth vehicle (state-h-heads s))))
	(setf length (nth vehicle (state-h-lengths s)))
	
	(setf (aref (state-board s) vehicle-head-y (+ vehicle-head-x length)) 1)
	(setf (aref (state-board s) vehicle-head-y vehicle-head-x) 0)
	(setf (nth vehicle (state-h-heads s)) (list vehicle-head-y (+ vehicle-head-x 1)))
)




(defmethod rootp ((s state))
	(eq (state-name s) 'root)
)

( defmethod goalp ((s state))
	(when (= (car (car h-heads)) 4)
		t 
	)
	nil
)


(defmethod equal-state-p ((s1 state) (s2 state))
	(equal (state-board s1) (state-board s2))
)

