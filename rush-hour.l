(load "c:/clisp/boards.l")
(load "c:/clisp/states.l")

; globals
(setf *letters* nil)
(setf *board* nil)
(setf *exit* '(2 5))


(defun play (&optional board-num) 
	(unless board-num (setf board-num 1))
	(setf *letters* '(r g b y p v i c t a l q s x y z))
	(format t "Letters represent: 
R = red; G = green; B = blue; Y = yellow; P = purple; 
V = violet; I = indigo; C = cyan; T = teal; A = aqua; L = lime~%")
	(make-board board-num t)
)


; vehicle-uf is the user-friendly letter representation of a vehicle
(defun move (vehicle-uf direction &optional repeat &aux vehicle) 
	(unless repeat (setf repeat 1))
	(setf vehicle (get vehicle-uf 'vehicle-ref))
	(loop for x from 0 below repeat do 
		(cond 
			((eq direction 'up)
				(if (member vehicle (gather-up-moves)) 
					(move-up vehicle t)
					(progn
						(format t "This move is illegal. Try a different move.~%")
						(display-board *board* t)
						(when (game-end-p) (format t "You win!~%"))
						(return-from move nil)
					)
				)
			)
			((eq direction 'down)
				(if (member vehicle (gather-down-moves)) 
					(move-down vehicle t)
					(progn
						(format t "This move is illegal. Try a different move.~%")
						(display-board *board* t)
						(when (game-end-p) (format t "You win!~%"))
						(return-from move nil)
					)
				)
			)
			((eq direction 'left)
				(if (member vehicle (gather-left-moves)) 
					(move-left vehicle t)
					(progn
						(format t "This move is illegal. Try a different move.~%")
						(display-board *board* t)
						(when (game-end-p) (format t "You win!~%"))
						(return-from move nil)
					)
				)
			)
			((eq direction 'right)
				(if (member vehicle (gather-right-moves)) 
					(move-right vehicle t)
					(progn
						(format t "This move is illegal. Try a different move.~%")
						(display-board *board* t)
						(when (game-end-p) (format t "You win!~%"))
						(return-from move nil)
					)
				)
			)
		)
	)
	(display-board *board* t)
	(when (game-end-p) (format t "You win!~%"))
)


(defun gather-up-moves (&aux vehicle-head-x vehicle-head-y ups)
	(setf ups '())
	(loop for vehicle in *v-vehicles* do 
		(setf vehicle-head-x (car (last (get vehicle 'head))))
		(setf vehicle-head-y (first (get vehicle 'head)))
		(if (and (> vehicle-head-y 0) (= 0 (aref *board* (- vehicle-head-y 1) vehicle-head-x))) ; legality check
			(setf ups (cons vehicle ups))
		)
	)
	ups
)


(defun gather-down-moves (&aux vehicle-head-x vehicle-head-y downs)
	(setf downs '())
	(loop for vehicle in *v-vehicles* do 
		(setf vehicle-head-x (car (last (get vehicle 'head))))
		(setf vehicle-head-y (first (get vehicle 'head)))
		(setf length (get vehicle 'length))
		(if (and (< (+ vehicle-head-y length -1) 5) (= 0 (aref *board* (+ vehicle-head-y length) vehicle-head-x)))
			(setf downs (cons vehicle downs))
		)
	)
	downs
)


(defun gather-left-moves (&aux vehicle-head-x vehicle-head-y lefts)
	(setf lefts '())
	(loop for vehicle in *h-vehicles* do 
		(setf vehicle-head-x (car (last (get vehicle 'head))))
		(setf vehicle-head-y (first (get vehicle 'head)))
		(if (and (> vehicle-head-x 0) (= 0 (aref *board* vehicle-head-y (- vehicle-head-x 1))))
			(setf lefts (cons vehicle lefts))
		)
	)
	lefts
)


(defun gather-right-moves (&aux vehicle-head-x vehicle-head-y rights)
	(setf rights '())
	(loop for vehicle in *h-vehicles* do 
		(setf vehicle-head-x (car (last (get vehicle 'head))))
		(setf vehicle-head-y (first (get vehicle 'head)))
		(setf length (get vehicle 'length))
		(if (and (< (+ vehicle-head-x length -1) 5) (= 0 (aref *board* vehicle-head-y (+ vehicle-head-x length))))
			(setf rights (cons vehicle rights))
		)
	)
	rights
)


(defun move-up (vehicle &optional no-disp &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (get vehicle 'head))))
	(setf vehicle-head-y (first (get vehicle 'head)))
	(setf length (get vehicle 'length))
	
	(setf (aref *board* (- vehicle-head-y 1) vehicle-head-x) 1)
	(setf (aref *board* (+ vehicle-head-y length -1) vehicle-head-x) 0)
	(setf (get vehicle 'head) (list (- vehicle-head-y 1) vehicle-head-x))
	(unless no-disp (display-board *board*))
)

(defun move-down (vehicle &optional no-disp &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (get vehicle 'head))))
	(setf vehicle-head-y (first (get vehicle 'head)))
	(setf length (get vehicle 'length))
	
	(setf (aref *board* (+ vehicle-head-y length) vehicle-head-x) 1)
	(setf (aref *board* vehicle-head-y vehicle-head-x) 0)
	(setf (get vehicle 'head) (list (+ vehicle-head-y 1) vehicle-head-x))
	(unless no-disp (display-board *board*))
)

(defun move-left (vehicle &optional no-disp &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (get vehicle 'head))))
	(setf vehicle-head-y (first (get vehicle 'head)))
	(setf length (get vehicle 'length))
	
	(setf (aref *board* vehicle-head-y (- vehicle-head-x 1)) 1)
	(setf (aref *board* vehicle-head-y (+ vehicle-head-x length -1)) 0)
	(setf (get vehicle 'head) (list vehicle-head-y (- vehicle-head-x 1)))
	(unless no-disp (display-board *board*))
)

(defun move-right (vehicle &optional no-disp &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (get vehicle 'head))))
	(setf vehicle-head-y (first (get vehicle 'head)))
	(setf length (get vehicle 'length))
	
	(setf (aref *board* vehicle-head-y (+ vehicle-head-x length)) 1)
	(setf (aref *board* vehicle-head-y vehicle-head-x) 0)
	(setf (get vehicle 'head) (list vehicle-head-y (+ vehicle-head-x 1)))
	(unless no-disp (display-board *board*))
)

(defun game-end-p ()
	(if (= (car (last (get 'h0 'head))) 4) ;This 4 would reference *exit* if it wasn't marginally slower
		t 
		nil
	)
)



;;;;; DISPLAY FUNCTIONS ;;;;;

(defun display-board (&optional board uf) 
	(unless board (setf board *board*))
	(destructuring-bind (n m) (array-dimensions board)
		(loop for i from 0 below n do
			(loop for j from 0 below m do
				(setf cell (aref board i j))
				(if uf
					(if (= cell 1)
						(format t "~a " (get (belongs-to i j) 'uf-ref))
						(format t "~a " 0)
					)
					(format t "~a " cell)
				)
			)
			(when (and uf (= i (car *exit*))) 
				(format t " <- move R here")
			)
			(format t "~%")
		)
	)
)

(defun belongs-to (y x &aux solution vehicle-head-x vehicle-head-y length)
	(loop for vehicle in *h-vehicles* do 
		(setf vehicle-head-x (car (last (get vehicle 'head))))
		(setf vehicle-head-y (first (get vehicle 'head)))
		(setf length (get vehicle 'length))
		(when (and (= vehicle-head-y y) (>= x vehicle-head-x) (< x (+ vehicle-head-x length)))
			(return-from belongs-to vehicle)
		)
	)
	(loop for vehicle in *v-vehicles* do 
		(setf vehicle-head-x (car (last (get vehicle 'head))))
		(setf vehicle-head-y (first (get vehicle 'head)))
		(setf length (get vehicle 'length))
		(when (and (= vehicle-head-x x) (>= y vehicle-head-y) (< y (+ vehicle-head-y length)))
			(return-from belongs-to vehicle)
		)
	)
	(format t "Error finding a vehicle in function belongs-to.")
)


(defun display-vehicles ()
	(format t "Horizontal vehicles: ~%")
	(loop for vehicle in *h-vehicles* do 
		(display-vehicle vehicle)
	)
	
	(format t "Vertical vehicles: ~%")
	(loop for vehicle in *v-vehicles* do 
		(display-vehicle vehicle)
	)
)


(defun display-vehicle (vehicle &aux temp-board vehicle-head-x vehicle-head-y length)
	(setf temp-board (make-array '(6 6) :element-type 'bit :initial-element 0))
	(setf vehicle-head-x (car (last (get vehicle 'head))))
	(setf vehicle-head-y (first (get vehicle 'head)))
	(setf vehicle-length (get vehicle 'length))
	
	(cond 
		((member vehicle *h-vehicles*) 
			(format t "Vehicle ~a -- vehicle head: ~a; length: ~a ~%" vehicle (get vehicle 'head) (get vehicle 'length))
			
			(loop for i from vehicle-head-x to (+ vehicle-head-x vehicle-length -1) do
					(setf (aref temp-board vehicle-head-y i) 1)
			)
			
			(display-board temp-board)
			(format t "~%")
		)
		((member vehicle *v-vehicles*) 
			(format t "Vehicle ~a -- vehicle head: ~a; length: ~a ~%" vehicle (get vehicle 'head) (get vehicle 'length))
			
			(loop for i from vehicle-head-y to (+ vehicle-head-y vehicle-length -1) do
					(setf (aref temp-board i vehicle-head-x) 1)
			)
			
			(display-board temp-board)
			(format t "~%")
		)
		(t (format t "Vehicle not found.~%"))
	)
)

