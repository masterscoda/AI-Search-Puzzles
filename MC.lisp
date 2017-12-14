;Missionary-Cannibal Problem Solver LISP using A* Search
;Scott Bossard

(defvar m1) ;total amount of missionaries
(defvar c1) ;total amount of cannibals
(defvar cap) ;max capacity of the boat
(setf exploredMC (make-array 0 :fill-pointer t :adjustable t)) ;array of nodes explored
(setf exploreMC 0) ;0 means we can explore, 1 means it has been explored so ignore
(setf treeMC (make-array 0 :fill-pointer t :adjustable t)) ;array to store states and be prioritized
(setf bestPathMC (make-array 0 :fill-pointer t :adjustable t)) ;array to stor the best path to take to reach goal
(setf actionMC (make-array 3)) ;possible action to generate child node

(defstruct nodeMC
	state ; #(missionary, cannibals, boat position)
	heuristic ; = missionary + cannibals
	depth ; = depth of node or path cost g(n)
	parent ; parent node
) ;node


(defun MC ()
	(format t "###### Missionary-Cannibal Problem Solver in LISP ###### ~%")
	(format t "################### Using A* Search #################### ~%")
	(format t "######################################################## ~%")
	(format t "Hello! Welcome to the Missionary-Cannibal problem! ~%")
	(format t "~% How many cannibals? ")
	(setq c1 (read)) ;user input for # of cannibals
	(format t "~% How many missionaries?")
	(setq m1 (read)) ;user input for # of missionaries
	(format t "~% What is boat max capacity?")
	(setq cap (read)) ;user input for boat capacity
	
	;Check if input is valid
	(if (> c1 m1)
		(error "~% What are you doing? All the missionaries died already!!??"))	
	(if (or (= m1 0) (= c1 0))
		(error "~% C'mon give me a real problem to solve!!")
	)

	(setf initialState (make-array 3)) ;array (missionary, cannibal, boat position)
	; set root node to users input
	(setf (aref initialState 0) m1) 
	(setf (aref initialState 1) c1)
	(setf (aref initialState 2) 1)
	
	(vector-push-extend (make-nodeMC :state initialState :heuristic (+ m1 c1) :depth 0 :parent '#(0 0 0)) treeMC) ;add root node to the search tree
	
    (format t "~% Solving... ~%")
	
	(SolveProblemMC)	;Solve the problem
	
	(format t "~% **Note: 1st digit = # of missionaries, 2nd digit = # of cannibals, 3rd digit = position of boat (1 for starting side, 0 for goal side) ~%")
    (format t "~% Starting Side: ~%")
	
    (DisplayResultsMC)
	
	(format t "####### End Missionary-Cannibal Problem Solver  ######## ~%")
    (format t "######################################################## ~%")
)

(defun MCExplored? (n)
	(setf exploreMC 0) ;initialize
	(loop for i from 0 to (- (length exploredMC) 1) ;loop through all the explored nodes
		;if node has been explored then global variable explore will be set to 1
		do(if (equalp n (nodeMC-state (aref exploredMC i))) (setf exploreMC 1))
    )
)

(defun DisplayResultsMC()
	(format t "~% ~S " initialState) ; display root node
	(loop for i from (- (length bestPathMC) 1) downto 0
		do(format t "~% ~S " (aref bestPathMC i)) ;display missionaries, canniobals and boat position for each step in best path
	)
	(format t "~% The optimal path found includes ~S steps ~%" (length bestPathMC))
	(format t "~% Cannibals and Missionaries all on right side of the river! And nobody got hurt! ~%") ;if finished printing the best path then display message
)

(defun FinalizePathMC()
	(setq numExplored (- (length exploredMC) 1)) ;# of explored nodes   
	(vector-push-extend (nodeMC-state (aref exploredMC numExplored)) bestPathMC) ;add last exploredMC node to explored array
    (setq nodeParent (nodeMC-parent (aref exploredMC numExplored))) ;parent of the current state

	;loop through and add the parent of each node to the array. This will in turn display the best path.
	(loop for i from numExplored downto 0
		do(if (= 0 i)
		    (return) ;reached root node
        )
        
		do(if (equalp nodeParent (nodeMC-state (aref exploredMC i))) 
			(progn                   
                (vector-push-extend (nodeMC-state (aref exploredMC i)) bestPathMC);push parent               
                (setq nodeParent (nodeMC-parent (aref exploredMC i))) ;next parent
            )
        )
    )
)
	
(defun SolveProblemMC()
	(setf currentState (vector-pop treeMC)) ;pop from the ordered tree   		
	(vector-push-extend currentState exploredMC) ;can now add the node to explored	
	
	(if (= (nodeMC-heuristic currentState) 0)
        (FinalizePathMC) ;if at goal state or heuristic is 0 then finalize the path taken for printing
		(progn
			(loop for m from 0 to cap 
				do(loop for c from 0 to cap 
			        	do(if (and (or (<= c m) (= 0 m) (= 0 c)) (and (> (+ m c) 0) (< (+ m c) (+ cap 1))))
						(progn                       
							;set the actions to make a child node
                            (setf (aref actionMC 0) m) ;# of missionaries                           
                            (setf (aref actionMC 1) c) ;# of cannibals	
							(setf (aref actionMC 2) 1) 
            				;create child nodes
                            (defvar len (length (nodeMC-state currentState))) ;length of the array
							(setf child (make-array len))
							(loop for i from 0 to (- len 1)
								do(if (= (aref (nodeMC-state currentState) 2) 1)
									;subtract action to child if on left side of river
		        					(setf (aref child i) (- (aref (nodeMC-state currentState) i) (aref actionMC i))) 
									;else add action to child if on right side of river
		        					(setf (aref child i) (+ (aref (nodeMC-state currentState) i)(aref actionMC i)))
        							)
    						)
							(setf heur (+ (aref child 0) (aref child 1))) ;heuristic for this search is (cannibals + missionaries) of the node  
							(setf dep (nodeMC-depth currentState)) ;depth of the parent used for g(n), add 1 later						
							(MCExplored? child) ;check if already present in the state tree to shorten search
							
							;if child is not explores and if child node does not contain more cannibals than missionaries and doesnt exceed the initial values
							(if (and (and (<= (aref child 1) (aref child 0)) (>= m1 (aref child 0)) (>= c1 (aref child 1)) (>= (aref child 1) 0) (>= (aref child 0) 0)) (= exploreMC 0))
								(vector-push-extend (make-nodeMC :state child :heuristic heur :depth (+ dep 1) :parent (nodeMC-state currentState)) treeMC)	;push child node to tree
    						)
                        )
                    	)
                )
            ) 
			
			;Order the tree for the next best node to be poped first
			;Since A* the tree order is decided by heuristic + depth  f(n) = h(n) + g(n)       	
			(loop for j from (- (length treeMC) 1) downto 0
       				do (loop for k from 0 to j when (< (+(nodeMC-heuristic (aref treeMC k)) (nodeMC-depth (aref treeMC k))) (+(nodeMC-heuristic (aref treeMC j)) (nodeMC-depth (aref treeMC j)))) 
						do (rotatef (aref treeMC j) (aref treeMC k))                                             
            			)
        	) 
			          	
			(SolveProblemMC) ;repeat until we reach goal state
        ) 
    ) 
)	

(MC) ;run the program