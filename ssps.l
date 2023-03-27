(load "c:/clisp/rush-hour.l")


(defmethod solve (&aux kids e-state)
	(if (member *trace-instruction* `(u eu ux eux)) (display-the-unexplored-list))
	(if (member *trace-instruction* `(x ex ux eux)) (display-the-explored-list))
	(cond
		((null *unexplored*)
		(terpri)
		(format t ">>> THERE IS NO SOLUTION.~%")
		(return-from solve NIL)
		)
	)
	(setf e-state (pop *unexplored*))
	(if (member *trace-instruction* `(e ex eu eux)) (display-the-e-state e-state))
	(cond
		((goalp e-state)
			(format t "~%>>> GOT A SOLUTION!")
			(display-solution e-state)
		)
		((exploredp e-state)
			(solve)
		)
		(t
		(push e-state *explored*)
		(setf kids (children-of e-state))
		(setf *unexplored* (append *unexplored* kids))
		(solve)
		)
	)
	nil
)
	
	


(defmethod children-of ((e-state state) &aux kids)

)

(defmethod child-of-down ((s state) vehicle)
	
)

(defmethod child-of-up ((s state) vehicle)

)

(defmethod child-of-left ((s state) vehicle)

)

(defmethod child-of-right ((s state) vehicle)

)




(defmethod state-gather-up-moves ((s state) &aux vehicle-head-x vehicle-head-y ups)
	(setf ups '())
	(loop for i from 0 below (state-num-v s) do 
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
	(loop for i from 0 below (state-num-v s) do
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
	(loop for i from 0 below (state-num-v s) do
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
	(loop for i from 0 below (state-num-v s) do 
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
	(setf (nth vehicle (state-v-heads)) (list (- vehicle-head-y 1) vehicle-head-x))
)

(defmethod state-move-down ((s state) vehicle &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (nth vehicle (state-v-heads s)))))
	(setf vehicle-head-y (first (nth vehicle (state-v-heads s))))
	(setf length (nth vehicle (state-v-lengths s)))
	
	(setf (aref (state-board s) (+ vehicle-head-y length) vehicle-head-x) 1)
	(setf (aref (state-board s) vehicle-head-y vehicle-head-x) 0)
	(setf (nth vehicle (state-v-heads)) (list (+ vehicle-head-y 1) vehicle-head-x))
)

(defmethod state-move-left ((s state) vehicle &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (nth vehicle (state-h-heads s)))))
	(setf vehicle-head-y (first (nth vehicle (state-h-heads s))))
	(setf length (nth vehicle (state-h-lengths s)))
	
	(setf (aref (state-board s) vehicle-head-y (- vehicle-head-x 1)) 1)
	(setf (aref (state-board s) vehicle-head-y (+ vehicle-head-x length -1)) 0)
	(setf (nth vehicle (state-h-heads)) (list vehicle-head-y (- vehicle-head-x 1)))
)

(defmethod state-move-right ((s state) vehicle &aux vehicle-head-x vehicle-head-y length)
	(setf vehicle-head-x (car (last (nth vehicle (state-h-heads s)))))
	(setf vehicle-head-y (first (nth vehicle (state-h-heads s))))
	(setf length (nth vehicle (state-h-lengths s)))
	
	(setf (aref (state-board s) vehicle-head-y (+ vehicle-head-x length)) 1)
	(setf (aref (state-board s) vehicle-head-y vehicle-head-x) 0)
	(setf (nth vehicle (state-h-heads)) (list vehicle-head-y (+ vehicle-head-x 1)))
)


(defmethod display-state ((s state))
	(format t "~A " (state-name s))
	(if (not (rootp s))
		(let ()
			(format t "~A " (state-name (state-parent s)))
			(format t "~A~%" (operator-name (state-operator s)))
		)
		(terpri)
	)
	(display-board (state-board s)) ;TODO test uf
	nil
)

(defmethod display-solution ((s state))
	(cond
		((rootp s)
			(terpri)
		)
		(t
			(display-solution (state-parent s))
			;TODO display operator(format t "~A~%" (state-operator s))
		)
	)
	nil
)