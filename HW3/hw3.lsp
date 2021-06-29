;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond ((null s) T)
	((atom s) (cond ((isBox s) nil)
			(t T)
		  )
	)
        (t (and (goal-test (car s)) (goal-test (cdr s))))
  )
);end defun



; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;


; get-content (r c)
; Takes a row content r, and a column number c
; Returns the content placed at column c in the row r
;
(defun get-content (r c)
  (cond ((< c 0) wall)
	((null r) wall)
    	((= c 0) (car r))
	(t (get-content (cdr r) (- c 1)))
  )
)



; get-square (s r c)
; Takes a State space s, a row number r, a column number c 
; Returns the content of State S at location (r,c)
;
(defun get-square (s r c)
  (cond ((< r 0) wall)
	((null s) wall)
    	((= r 0) (get-content (car s) c))
	(t (get-square (cdr s) (- r 1) c))
  )
)



; set-content (r c v)
; Takes a row content r, a column number c, and the subsitute content v
; Returns the updated row content
;
(defun set-content (r c v)
  (cond ((= c 0) (cons v (cdr r)))
	(t (cons (car r) (set-content (cdr r) (- c 1) v)))
  )
)



; set-square (s r c v)
; Takes a state space s, a row number r, a column number c, and the substitute content v
; Returns the updated state space
;
(defun set-square (s r c v)
  (cond ((= r 0) (cons (set-content (car s) c v) (cdr s)))
	(t (cons (car s) (set-square (cdr s) (- r 1) c v)))
  )
)



; check-move-ok (next-first next-second)
; Takes the content of the first square and the second square next to the current square
; Returns whether the keeper can move to the first square next to the current square
;
(defun check-move-ok (next-first next-second)
  (cond ((or (isBox next-first) (isBoxStar next-first)) (cond ((or (isWall next-second)(isBox next-second)(isBoxStar next-second)) nil)
							      (t T)
			    )
	)
	(t (cond ((isWall next-first) nil)
		 (t T)
	   )
	)
  )
)



; move-ok (s d keeper-row keeper-column)
; Takes the State space s, direction d, the row keeper-row, the column keeper-column that the keeper is currently at
; Returns whether the keeper can move one square in the specified position
;
(defun move-ok (s d keeper-row keeper-column)
  (cond ((equal d 'up) 	 (check-move-ok (get-square s (- keeper-row 1) keeper-column) 
		                        (get-square s (- keeper-row 2) keeper-column)
		      	 )
	)

  	((equal d 'down) (check-move-ok (get-square s (+ keeper-row 1) keeper-column)
				        (get-square s (+ keeper-row 2) keeper-column)
		 	 )
	)

	((equal d 'left) (check-move-ok (get-square s keeper-row (- keeper-column 1))
				        (get-square s keeper-row (- keeper-column 2))
			 )
	)

	((equal d 'right) (check-move-ok (get-square s keeper-row (+ keeper-column 1))
				         (get-square s keeper-row (+ keeper-column 2))
			  )
	)
  )
)



; try-move (s d)
; Takes the State space s, and the direction d
; Returns the updated state space after the move in the direction specified
;
(defun try-move (s d)
  (let* ((keeper-row (second (getKeeperPosition s 0)))
	 (keeper-column (first (getKeeperPosition s 0)))

	 (up-first-row (- keeper-row 1))
	 (up-first-column keeper-column)
	 (up-second-row (- keeper-row 2))
	 (up-second-column keeper-column)

	 (down-first-row (+ keeper-row 1))
	 (down-first-column keeper-column)
	 (down-second-row (+ keeper-row 2))
	 (down-second-column keeper-column)

	 (left-first-row keeper-row)
	 (left-first-column (- keeper-column 1))
	 (left-second-row keeper-row)
	 (left-second-column (- keeper-column 2))

	 (right-first-row keeper-row)
	 (right-first-column (+ keeper-column 1))
	 (right-second-row keeper-row)
	 (right-second-column (+ keeper-column 2))

	 (keeper-square (get-square s keeper-row keeper-column))

	 (up-first-square (get-square s up-first-row up-first-column))
	 (up-second-square (get-square s up-second-row up-second-column))

	 (down-first-square (get-square s down-first-row down-first-column))
	 (down-second-square (get-square s down-second-row down-second-column))

	 (left-first-square (get-square s left-first-row left-first-column))
	 (left-second-square (get-square s left-second-row left-second-column))

	 (right-first-square (get-square s right-first-row right-first-column))
	 (right-second-square (get-square s right-second-row right-second-column))

	 )

    (cond ((not (move-ok s d keeper-row keeper-column)) nil)
	  (t (cond ((equal d 'up) (cond ((and (isKeeper keeper-square) (isBlank up-first-square))
					 (set-square (set-square s keeper-row keeper-column blank) up-first-row up-first-column keeper)
					)
					((and (isKeeperStar keeper-square) (isBlank up-first-square))
					 (set-square (set-square s keeper-row keeper-column star) up-first-row up-first-column keeper)
					)


					((and (isKeeper keeper-square) (isStar up-first-square))
					 (set-square (set-square s keeper-row keeper-column blank) up-first-row up-first-column keeperstar)
					)
					((and (isKeeperStar keeper-square) (isStar up-first-square))
					 (set-square (set-square s keeper-row keeper-column star) up-first-row up-first-column keeperstar)
					)


					((and (isKeeper keeper-square) (isBox up-first-square) (isBlank up-second-square))
					 (set-square (set-square (set-square s keeper-row keeper-column blank) up-first-row up-first-column keeper) up-second-row up-second-column box)
					)
					((and (isKeeperStar keeper-square) (isBox up-first-square) (isBlank up-second-square))
					 (set-square (set-square (set-square s keeper-row keeper-column star) up-first-row up-first-column keeper) up-second-row up-second-column box)
					)


					((and (isKeeper keeper-square) (isBoxStar up-first-square) (isBlank up-second-square))
					 (set-square (set-square (set-square s keeper-row keeper-column blank) up-first-row up-first-column keeperstar) up-second-row up-second-column box)
					)
					((and (isKeeperStar keeper-square) (isBoxStar up-first-square) (isBlank up-second-square))
					 (set-square (set-square (set-square s keeper-row keeper-column star) up-first-row up-first-column keeperstar) up-second-row up-second-column box)
					)


					((and (isKeeper keeper-square) (isBox up-first-square) (isStar up-second-square))
					 (set-square (set-square (set-square s keeper-row keeper-column blank) up-first-row up-first-column keeper) up-second-row up-second-column boxstar)
					)
					((and (isKeeperStar keeper-square) (isBox up-first-square) (isStar up-second-square))
					 (set-square (set-square (set-square s keeper-row keeper-column star) up-first-row up-first-column keeper) up-second-row up-second-column boxstar)
					)


					((and (isKeeper keeper-square) (isBoxStar up-first-square) (isStar up-second-square))
					 (set-square (set-square (set-square s keeper-row keeper-column blank) up-first-row up-first-column keeperstar) up-second-row up-second-column boxstar)
					)
					((and (isKeeperStar keeper-square) (isBoxStar up-first-square) (isStar up-second-square))
					 (set-square (set-square (set-square s keeper-row keeper-column star) up-first-row up-first-column keeperstar) up-second-row up-second-column boxstar)
					)
				  )
		   )

		   ((equal d 'down) (cond ((and (isKeeper keeper-square) (isBlank down-first-square))
					   (set-square (set-square s keeper-row keeper-column blank) down-first-row down-first-column keeper)
					  )
					  ((and (isKeeperStar keeper-square) (isBlank down-first-square))
					   (set-square (set-square s keeper-row keeper-column star) down-first-row down-first-column keeper)
					  )


				 	  ((and (isKeeper keeper-square) (isStar down-first-square))
					   (set-square (set-square s keeper-row keeper-column blank) down-first-row down-first-column keeperstar)
					  )
					  ((and (isKeeperStar keeper-square) (isStar down-first-square))
					   (set-square (set-square s keeper-row keeper-column star) down-first-row down-first-column keeperstar)
					  )


					  ((and (isKeeper keeper-square) (isBox down-first-square) (isBlank down-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column blank) down-first-row down-first-column keeper) down-second-row down-second-column box)
					  )
				  	  ((and (isKeeperStar keeper-square) (isBox down-first-square) (isBlank down-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column star) down-first-row down-first-column keeper) down-second-row down-second-column box)
					  )


					  ((and (isKeeper keeper-square) (isBoxStar down-first-square) (isBlank down-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column blank) down-first-row down-first-column keeperstar) down-second-row down-second-column box)
					  )
				  	  ((and (isKeeperStar keeper-square) (isBoxStar down-first-square) (isBlank down-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column star) down-first-row down-first-column keeperstar) down-second-row down-second-column box)
					  )


					  ((and (isKeeper keeper-square) (isBox down-first-square) (isStar down-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column blank) down-first-row down-first-column keeper) down-second-row down-second-column boxstar)
					  )
					  ((and (isKeeperStar keeper-square) (isBox down-first-square) (isStar down-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column star) down-first-row down-first-column keeper) down-second-row down-second-column boxstar)
					  )


					  ((and (isKeeper keeper-square) (isBoxStar down-first-square) (isStar down-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column blank) down-first-row down-first-column keeperstar) down-second-row down-second-column boxstar)
					  )
					  ((and (isKeeperStar keeper-square) (isBoxStar down-first-square) (isStar down-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column star) down-first-row down-first-column keeperstar) down-second-row down-second-column boxstar)
					  )
				    )
		   )

		   ((equal d 'left) (cond ((and (isKeeper keeper-square) (isBlank left-first-square))
					   (set-square (set-square s keeper-row keeper-column blank) left-first-row left-first-column keeper)
					  )
					  ((and (isKeeperStar keeper-square) (isBlank left-first-square))
					   (set-square (set-square s keeper-row keeper-column star) left-first-row left-first-column keeper)
					  )


				 	  ((and (isKeeper keeper-square) (isStar left-first-square))
					   (set-square (set-square s keeper-row keeper-column blank) left-first-row left-first-column keeperstar)
					  )
					  ((and (isKeeperStar keeper-square) (isStar left-first-square))
					   (set-square (set-square s keeper-row keeper-column star) left-first-row left-first-column keeperstar)
					  )


					  ((and (isKeeper keeper-square) (isBox left-first-square) (isBlank left-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column blank) left-first-row left-first-column keeper) left-second-row left-second-column box)
					  )
				  	  ((and (isKeeperStar keeper-square) (isBox left-first-square) (isBlank left-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column star) left-first-row left-first-column keeper) left-second-row left-second-column box)
					  )


					  ((and (isKeeper keeper-square) (isBoxStar left-first-square) (isBlank left-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column blank) left-first-row left-first-column keeperstar) left-second-row left-second-column box)
					  )
				  	  ((and (isKeeperStar keeper-square) (isBoxStar left-first-square) (isBlank left-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column star) left-first-row left-first-column keeperstar) left-second-row left-second-column box)
					  )


					  ((and (isKeeper keeper-square) (isBox left-first-square) (isStar left-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column blank) left-first-row left-first-column keeper) left-second-row left-second-column boxstar)
					  )
					  ((and (isKeeperStar keeper-square) (isBox left-first-square) (isStar left-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column star) left-first-row left-first-column keeper) left-second-row left-second-column boxstar)
					  )


					  ((and (isKeeper keeper-square) (isBoxStar left-first-square) (isStar left-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column blank) left-first-row left-first-column keeperstar) left-second-row left-second-column boxstar)
					  )
					  ((and (isKeeperStar keeper-square) (isBoxStar left-first-square) (isStar left-second-square))
					   (set-square (set-square (set-square s keeper-row keeper-column star) left-first-row left-first-column keeperstar) left-second-row left-second-column boxstar)
					  )
				    )
		   )

		   ((equal d 'right) (cond ((and (isKeeper keeper-square) (isBlank right-first-square))
					     (set-square (set-square s keeper-row keeper-column blank) right-first-row right-first-column keeper)
					   )
					   ((and (isKeeperStar keeper-square) (isBlank right-first-square))
					     (set-square (set-square s keeper-row keeper-column star) right-first-row right-first-column keeper)
					   )


				 	   ((and (isKeeper keeper-square) (isStar right-first-square))
					     (set-square (set-square s keeper-row keeper-column blank) right-first-row right-first-column keeperstar)
					   )
					   ((and (isKeeperStar keeper-square) (isStar right-first-square))
					     (set-square (set-square s keeper-row keeper-column star) right-first-row right-first-column keeperstar)
					   )


					   ((and (isKeeper keeper-square) (isBox right-first-square) (isBlank right-second-square))
					     (set-square (set-square (set-square s keeper-row keeper-column blank) right-first-row right-first-column keeper) right-second-row right-second-column box)
					   )
				  	   ((and (isKeeperStar keeper-square) (isBox right-first-square) (isBlank right-second-square))
					     (set-square (set-square (set-square s keeper-row keeper-column star) right-first-row right-first-column keeper) right-second-row right-second-column box)
					   )


					   ((and (isKeeper keeper-square) (isBoxStar right-first-square) (isBlank right-second-square))
					     (set-square (set-square (set-square s keeper-row keeper-column blank) right-first-row right-first-column keeperstar) right-second-row right-second-column box)
					   )
				  	   ((and (isKeeperStar keeper-square) (isBoxStar right-first-square) (isBlank right-second-square))
					     (set-square (set-square (set-square s keeper-row keeper-column star) right-first-row right-first-column keeperstar) right-second-row right-second-column box)
					   )


					   ((and (isKeeper keeper-square) (isBox right-first-square) (isStar right-second-square))
					     (set-square (set-square (set-square s keeper-row keeper-column blank) right-first-row right-first-column keeper) right-second-row right-second-column boxstar)
					   )
					   ((and (isKeeperStar keeper-square) (isBox right-first-square) (isStar right-second-square))
					     (set-square (set-square (set-square s keeper-row keeper-column star) right-first-row right-first-column keeper) right-second-row right-second-column boxstar)
					   )


					   ((and (isKeeper keeper-square) (isBoxStar right-first-square) (isStar right-second-square))
					     (set-square (set-square (set-square s keeper-row keeper-column blank) right-first-row right-first-column keeperstar) right-second-row right-second-column boxstar)
					   )
					   ((and (isKeeperStar keeper-square) (isBoxStar right-first-square) (isStar right-second-square))
					     (set-square (set-square (set-square s keeper-row keeper-column star) right-first-row right-first-column keeperstar) right-second-row right-second-column boxstar)
					   )
				     )
		   ); end equal
	     ); end cond
	  ); end t
    ); end cond
  ); end let
)



; next-states (s)
; Takes the State space s
; Returns a list of possible valid states after one move in all four directions
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 'up) (try-move s 'down) (try-move s 'left) (try-move s 'right)))
	 )
    (cleanUpList result);end
   );end let
  );



; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.

; h0 (s) simply returns 0 for all inputs
;
(defun h0 (s)
  0
)



; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; This heuristic is admissible, because it never overestimate the actual cost of a node
; Since in this case every misplaced box require at least one move to get to the correct spot
; Given that we simply count the number of misplaced box, the actual cost would always be 
; higher or at least the same as our heuristic cost.
;

; h1 (s) takes a State space s
; Returns the number of misplaced boxes in the current state as the result
; This heuristic is admissible since all misplaced boxes need to move at least once to get to a star's position.
; Thus, the estimated cost outputed by h1 is always smaller than the actual cost, hence the definition of admissibility
;
(defun h1 (s)
  (cond ((not s) 0)
	((atom s) (cond ((isBox s) 1)
			(t 0)
		  )
	)
	(t (+ (h1 (car s)) (h1 (cdr s))))
  )
)



; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;



; get-absolute-value (val1 val2)
; Takes two values val1 val2
; Returns the absolute value between the two values
;
(defun get-absolute-value (val1 val2)
  (cond ((> val1 val2) (- val1 val2))
	(t (- val2 val1))
  )
)



; get-manhattan-distance (r1 c1 r2 c2)
; Takes the first object's row r1, column c1, and the second object's row r2, column c2
; Returns the manhattan distance between the two objects
;
(defun get-manhattan-distance (r1 c1 r2 c2)
  (+ (get-absolute-value r1 r2) (get-absolute-value c1 c2))
)



; m_map (m-val m-list)
; Takes one object m-val and map it to every entry in the list m-list
; e.g. (m_map 'one '(two three)) would return ((one two) (one three))
;
(defun m_map (m-val m-list)
  (cond ((null m-list) nil)
	((atom m-list) (list m-val m-list))
	(t (cons (m_map m-val (car m-list)) (m_map m-val (cdr m-list))))
  )
)



; get-all-box-in-one-row (r col)
; Takes a row content r, column number col
; Returns the column numbers of all instances of boxes to the right of column col in the row
;
(defun get-all-box-in-one-row (r col)
  (cond ((null r) nil)
	(t (if (isBox (car r))
	     (cons col (get-all-box-in-one-row (cdr r) (+ col 1))) 
	     (get-all-box-in-one-row (cdr r) (+ col 1))
	   )
	)
  )
)



; get-all-box-position (s row)
; Takes a State space s, a row number row
; Returns the coordinates of all boxes in the state space
;
(defun get-all-box-position (s row)
  (cond ((null s) nil)
	(t (let ((x (get-all-box-in-one-row (car s) 0)))
	     (if x
	         (append (m_map row x) (get-all-box-position (cdr s) (+ row 1)))
		 (get-all-box-position (cdr s) (+ row 1))
	     )
	   )
	)
  )
)



; get-all-star-in-one-row (r col)
; Similar to get-all-box-in-one-row, takes a row content r, column number col
; Returns the column numbers of all instances of stars to the right of column col in the row
;
(defun get-all-star-in-one-row (r col)
  (cond ((null r) nil)
	(t (if (isStar (car r))
	     (cons col (get-all-star-in-one-row (cdr r) (+ col 1))) 
	     (get-all-star-in-one-row (cdr r) (+ col 1))
	   )
	)
  )
)



; get-all-star-position (s row)
; Similar to get-all-box-position, takes a State space s, a row number row
; Returns the coordinates of all stars in the state space
;
(defun get-all-star-position (s row)
  (cond ((null s) nil)
	(t (let ((x (get-all-star-in-one-row (car s) 0)))
	     (if x
	         (append (m_map row x) (get-all-star-position (cdr s) (+ row 1)))
		 (get-all-star-position (cdr s) (+ row 1))
	     )
	   )
	)
  )
)



; get-distance-nearest-star (box-position star-position-list)
; Takes one box coordinate box-position, a list of stars coordinate star-position-list
; Returns the manhattan distance between the box and the nearest star
;
(defun get-distance-nearest-star (box-position star-position-list)
  (cond ((null star-position-list) nil)
	(t (let* ((d1 (get-manhattan-distance (first box-position) (second box-position) (first (car star-position-list)) (second (car star-position-list))))
	          (d2 (get-distance-nearest-star box-position (cdr star-position-list)))
	          )
	     (cond ((null d2) d1)
		   (t (cond ((< d1 d2) d1)
			    (t d2)
		      )
		   )
	     )
	   )
        )
  )
)



; isCoordinate (m_list)
; Takes a list of coordinates in the form '((x1 y1) (x2 y2) ...)
; Returns True if the input is one coordinate in the form '(x1 y1)
;
(defun isCoordinate (m_list)
  (cond ((null m_list) nil)
	((numberp m_list) nil)
	(t (and (numberp (first m_list)) (numberp (second m_list))))
  )
)



; get-total-manhattan-distance (box-position-list star-position-list)
; Takes two lists of coordinates for all boxes in the state space box-position-list and all stars the state space star-position-list
; Returns the manhattan distance of all box and star combinations in the state space
;
(defun get-total-manhattan-distance (box-position-list star-position-list)
  (cond ((null box-position-list) 0)
	((null star-position-list) 0)
	((isCoordinate box-position-list) (get-distance-nearest-star box-position-list star-position-list))
	(t (+ (get-total-manhattan-distance (car box-position-list) star-position-list) (get-total-manhattan-distance (cdr box-position-list) star-position-list)))
  )
)



; h205366842 (s)
; Takes the state space s
; Returns the value for the heuristic, value based on the total manhattan distance for all box and star combination that are nearest
; Since manhanttan distance is the minimal distance it take in the case for a box to arrive at a star,
; and since we are taking the pair of box and star with minimal manhattan distance, this heuristic is admissible
;
(defun h205366842 (s)
  (get-total-manhattan-distance (get-all-box-position s 0) (get-all-star-position s 0))
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
