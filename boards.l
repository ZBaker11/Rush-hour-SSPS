
(defun make-board-1 ()
	(setf *exit* '(2 5))
	
	; INIT VEHICLES	
	; head is top left position of the vehicle in form (y, x), where the point (0, 0) is top left corner
	
	(setf *h-vehicles* '(h0 h1 h2))
	(setf (symbol-plist 'h0) (list 'head '(2 1) 'length '2)) 	; H0 is ALWAYS the GOAL VEHICLE
	(setf (symbol-plist 'h1) (list 'head '(0 1) 'length '3))
	(setf (symbol-plist 'h2) (list 'head '(4 2) 'length '3))
	
	(setf *v-vehicles* '(v0 v1 v2))
	(setf (symbol-plist 'v0) (list 'head '(2 4) 'length '2))
	(setf (symbol-plist 'v1) (list 'head '(1 3) 'length '3))
	(setf (symbol-plist 'v2) (list 'head '(0 0) 'length '3))
	
	(populate *h-vehicles* *v-vehicles*)
)



;;;;;;;;;;;;;;;;;;;;; BOARD-MAKING HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-board (&optional board-num uf)
	(cond 
		((not board-num) (make-board-1)) 	; default for no value entered
		((= board-num 1) (make-board-1))
		(t (make-board-1))					; default for unrecognized value entered 
	)
	
	(when uf ; build user friendly associations
		(loop for i from 0 for vehicle in (append *h-vehicles* *v-vehicles*) do 
			(setf (get (nth i *letters*) 'vehicle-ref) vehicle)
			(setf (get vehicle 'uf-ref) (nth i *letters*))
		)
	)
	(display-board *board* uf)
)

(defun populate (h-list v-list)
	; POPULATE BOARD
	(setf *board* (make-array '(6 6) :element-type 'bit :initial-element 0))
	(loop for vehicle in h-list do
		(setf vehicle-head-x (car (last (get vehicle 'head))))
		(setf vehicle-head-y (first (get vehicle 'head)))
		(setf vehicle-length (get vehicle 'length))
		(if (or (< vehicle-head-x 0) (< vehicle-head-y 0) (> (+ vehicle-head-x vehicle-length) 6))
			(format t "ERROR: Vehicle out of bounds~%")
			(loop for i from vehicle-head-x to (+ vehicle-head-x vehicle-length -1) do
				(if (= (aref *board* vehicle-head-y i) 1)
					(format t "ERROR: overlapping vehicles X:~a, Y:~a ~%" vehicle-head-x i)
					(setf (aref *board* vehicle-head-y i) 1)
				)
			)
		)
	)
	
	(loop for vehicle in v-list do
		(setf vehicle-head-x (car (last (get vehicle 'head))))
		(setf vehicle-head-y (first (get vehicle 'head)))
		(setf vehicle-length (get vehicle 'length))
		(if (or (< vehicle-head-x 0) (< vehicle-head-y 0) (> (+ vehicle-head-y vehicle-length) 6))
			(format t "ERROR: Vehicle out of bounds~%")
			(loop for i from vehicle-head-y to (+ vehicle-head-y vehicle-length -1) do
				(if (= (aref *board* i vehicle-head-x) 1)
					(format t "ERROR: overlapping vehicles X:~a, Y:~a ~%" i vehicle-head-y)
					(setf (aref *board* i vehicle-head-x) 1)
				)
			)
		)
	)
	(setf *h-vehicles* h-list)
	(setf *v-vehicles* v-list)
)