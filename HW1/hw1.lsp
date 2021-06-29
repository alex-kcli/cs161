; Question 1:
; Function takes an integer argument N
; Function outputs Nth Pedovan number

; Recursively calculate the Pedovan number by assigning PAD 0, 1, 2 as 0
; and (PAD 3) ... as (PAD (n - 2)) + (PAD (n - 3))
; Since PAD(n + 1) = (PAD (n - 1)) + (PAD (n - 2))

(defun PAD (N) 
  (cond ((and ( < N 3 ) ( >= N 0 )) 1)
        (t (+ (PAD ( - N 2 )) (PAD ( - N 3 )))) 
  ) 
)



; Question 2:
; Function takes an integer argument N
; Function outputs the steps required to calculate the Nth Pedovan number

; Recursively calculate the number of steps by assigning SUMS 0, 1, 2 as 0
; and (SUMS 3) ... as (SUMS (n - 2)) + (SUMS (n - 3)) + 1
; Since the number of steps to calculate (SUM (n + 1)) is the number of steps
; it takes to calculate (SUMS (n - 1)) + (SUMS (n - 2)) + 1  

(defun SUMS (N)
  (cond ((and ( < N 3 ) ( >= N 0 )) 0)
        (t (+ ( + 1 (SUMS ( - N 2 ))) (SUMS ( - N 3 ))))
  )
)



; Question 3
; Function take a tree representation in LISP
; Function outputs the same tree but with ? replacing all symbol and number
; in the original tree structure

; Recursively replacing symbol and number in the original tree structure by
; letting atom be ? and other structure be the concatenation of calling 
; ANON on its first element and calling ANON on the rest of the elements

(defun ANON (TREE)
  (cond ((not TREE) nil)
        ((atom TREE) '?)
        (t (cons (ANON (car TREE)) 
                  (ANON (cdr TREE))
           )
        )
   )
)
