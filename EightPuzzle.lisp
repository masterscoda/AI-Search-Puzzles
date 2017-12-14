;8 Puzzle Problem Solver LISP using A* Search
;Scott Bossard

(setf startState (make-array 9)) ;start state
(setf goalState (make-array 9 :initial-contents '(1 2 3 4 5 6 7 8 0))) ;goal state
(setf explored8P (make-array 0 :fill-pointer t :adjustable t)) ;array of nodes explored
(setf explore8P 0) ;0 means we can explore, 1 means it has been explored so ignore
(setf counter 1) ;0 means we can explore, 1 means it has been explored so ignore
(setf tree8P (make-array 0 :fill-pointer t :adjustable t)) ;array to store states and be prioritized

(defstruct node8P
	state ; state of the puzzle ex: 
	heuristic ; = 3 misplaced tiles
	depth ; = depth of node or search cost g(n)
	parent ; parent node
) ;node


(defun 8P ()
	(format t "########### 8 Puzzle Problem Solver in LISP ############ ~%")
	(format t "################### Using A* Search #################### ~%")
	(format t "######################################################## ~%")
	(format t "Hello! Welcome to the 8 Puzzle problem solver! ~%")
	(format t "~% Please enter the start state, starting from top-left and moving right. NOTE: 0 = Empty Square ~%")
	
	(format t "~% First square? ")
	(setf (aref startState 0) (read))
	(format t "~% Second square? ")
	(setf (aref startState 1) (read))
	(format t "~% Third square? ")
	(setf (aref startState 2) (read))
	(format t "~% Fourth square? ")
	(setf (aref startState 3) (read))
	(format t "~% Fifth square? ")
	(setf (aref startState 4) (read))
	(format t "~% Sixth square? ")
	(setf (aref startState 5) (read))
	(format t "~% Seventh square? ")
	(setf (aref startState 6) (read))
	(format t "~% Eighth square? ")
	(setf (aref startState 7) (read))
	(format t "~% Ninth square? ")
	(setf (aref startState 8) (read))
			
	;Check if input is feasible
	(Feasible8P startState)
	
	(setf heur (GetHeuristic8P startState)) ;heuristic of start state
	
	(FindMoves8P startState) ;find possble moves (children) from the start state
	
	(vector-push-extend (make-node8P :state startState :heuristic heur :depth 0 :parent '#(0 0 0 0 0 0 0 0 0)) tree8P) ;add root node to the tree
	
    (format t "~% Solving... ~%")
	
	(format t " Nodes Expanded: ~%")
	(format t " Node 1: ~S ~%" startState)
	(SolveProblem8P)	;Solve the problem
		
    (DisplayResults8P)
	
	(format t "~% ########### End 8 Puzzle Problem Solver  ############ ~%")
    (format t "######################################################## ~%")
)

(defun Feasible8P (s)
	(setf inversions 0) 
	(loop for i from 0 to 7 ;loop through start state array
		do (loop for j from (+ i 1) to 8
			do(if (and (/= (aref s j) 0) (/= (aref s i) 0) (> (aref s i) (aref s j))) 
				(setf inversions (+ inversions 1))
			)
		)
    )
	(if (oddp inversions) ;8 puzzles are infeasible if the puzzle has an odd number of inversions
		(error "The puzzle you entered is infeasible.")
	)
)

(defun GetHeuristic8P (s)
	(setf matched 0) ;initialize
	;subtract s from goal state if any value is 0 then it is a mamtched tiles
	(loop for i from 0 to (- (length s) 1) 
		
		do(if (equalp 0 (- (aref s i) (aref goalState i))) 
			(setf matched (+ matched 1))
		)
    )
	(setf mismatched (- 9 matched)) ;# mismatched
	mismatched ;return # of mismatched
)

(defun FindMoves8P (s)
	(setf blankSpace (position 0 s)) ;find position of the blank space in array
	; the posible moves allowed for each case of blank space location
	(setf tileMoves (case blankSpace
					((0) (make-array 2 :initial-contents '(1 3)))
					((1) (make-array 3 :initial-contents '(0 2 4)))
					((2) (make-array 2 :initial-contents '(1 5)))
					((3) (make-array 3 :initial-contents '(0 4 6)))
					((4) (make-array 4 :initial-contents '(1 3 5 7)))
					((5) (make-array 3 :initial-contents '(2 4 8)))
					((6) (make-array 2 :initial-contents '(3 7)))
					((7) (make-array 3 :initial-contents '(4 6 8)))
					((8) (make-array 2 :initial-contents '(5 7)))
				)
	)
	tileMoves
)

(defun 8PExplored? (n)
	(setf explore8P 0) ;initialize
	(loop for i from 0 to (- (length explored8P) 1) ;loop through all the explored nodes
		;if node has been explored then global variable explore will be set to 1
		do(if (equalp n (node8P-state (aref explored8P i))) (setf explore8P 1))
    )
)

(defun DisplayResults8P()
	(format t " Best Path: ~%")
	(loop for i from 0 to (- (length explored8P) 1)
		do(format t " Best Path Node ~S: ~S ~%" i (aref explored8P i))
	)
	(format t "~% The optimal path was found at depth ~S ~%" (- (length explored8P) 1))
)


	
(defun SolveProblem8P()
	(setf currentState (vector-pop tree8P)) ;pop from the ordered tree   		
	(vector-push-extend currentState explored8P) ;can now add the node8P to explored	
	
	(if (or (equalp (node8P-state currentState) goalState) (= (node8P-heuristic currentState) 0))
        (format t " Total nodes explored: ~S ~% ~%" counter) ;if heuristic is 0 then finalize 
		(progn 			        	    		        	    
			(setf possibleMoves (FindMoves8P (node8P-state currentState)))			
			(setf blankSpace (position 0 (node8P-state currentState)))
			(loop for i from 0 to (- (length possibleMoves) 1)
				do (setf child (make-array 9))
					(loop for j from 0 to (- (length child) 1)					
		        		do(setf (aref child j) (aref (node8P-state currentState) j))
        			)
				;create the child node
				(setf (aref child (aref possibleMoves i)) 0) 
				(setf (aref child blankSpace) (aref (node8P-state currentState) (aref possibleMoves i))) 								
				(setf heur (GetHeuristic8P child)) ;heuristic for this search is # mismatched tiles 
				(setf dep (node8P-depth currentState)) ;depth of the parent used for g(n)		
				
				(8PExplored? child) ;check if already present in the state tree
				
				(setf counter (+ counter 1)) 				
				(format t " Node ~S: ~S ~%" counter child)
				
				;if child has not been explored then push to search tree
				(if (= explore8P 0)
					(vector-push-extend (make-node8P :state child :heuristic heur :depth (+ dep 1) :parent (node8P-state currentState)) tree8P)	
				)				
			)
			
			;Order the tree for the next best node to be poped first
			;Since A* the tree order is decided by heuristic + depth  f(n) = h(n) + g(n)       	
			(loop for j from (- (length tree8P) 1) downto 0
       				do (loop for k from 0 to j when (< (+(node8P-heuristic (aref tree8P k)) (node8P-depth (aref tree8P k))) (+(node8P-heuristic (aref tree8P j)) (node8P-depth (aref tree8P j)))) 
						do (rotatef (aref tree8P j) (aref tree8P k))                                             
            			)
        	) 	        	
			(SolveProblem8P) ;repeat until we reach goal state
        ) 
    ) 
)	

(8P) ;run the program