; this function was used in professor's random agent
(let ((time (gettimeofday)))
      (set! *random-state*
            (seed->random-state (+ (car time)
                                   (cdr time)))))


; picks a move that will find new data while exploring
; if the space in fron of the agent is not empty, it will turn instead
(define (smart-random-move environment)
    (cond
        ((not (equal? (cadar environment) 'empty)) (random-turn))
        (#t (random-move))
    )
)

; move to the next vegetation on your list, but make sure 
; that there is not something blocking your way
(define (educated-move environment)
	(display "educated-move")
	(newline)
	(newline)
	(cond

		((null? my-vegetations) (random-move))	
	
		; eat when vegetaton is in front of you
		((and (equal? (caaar my-vegetations) 0) (equal? (cadaar my-vegetations) 1)) (eat-choice environment))
	
		; decide how to best move towards vegetation
	
		; Adjacent square to the veggies
		((and (equal? (caaar my-vegetations) -1) (equal? (cadaar my-vegetations) 0)) (turn-towards-veggies "LEFT"))
		((and (equal? (caaar my-vegetations) 1) (equal? (cadaar my-vegetations) 0)) (turn-towards-veggies "RIGHT"))
		((and (equal? (caaar my-vegetations) 0) (equal? (cadaar my-vegetations) -1)) (turn-towards-veggies "AROUND"))		

		; finished one axis, flip to the other
		((and (equal? (caaar my-vegetations) 0) (< (cadaar my-vegetations) -1)) (turn-towards-veggies "AROUND"))
		((and (< (caaar my-vegetations) 0) (equal? (cadaar my-vegetations) 0)) (turn-towards-veggies "LEFT"))
		((and (> (caaar my-vegetations) 0) (equal? (cadaar my-vegetations) 0)) (turn-towards-veggies "RIGHT"))
	
		; move forward when you can
		((and (and (equal? (get-square-info environment 2) 'empty) (equal? (get-square-info environment 6) 'empty)) (equal? (get-square-info environment 12) 'empty)) (move-towards-veggies 3))
		((and (equal? (get-square-info environment 2) 'empty) (equal? (get-square-info environment 6) 'empty)) (move-towards-veggies 2))
		((equal? (get-square-info environment 2) 'empty) (move-towards-veggies 1))
		
		; go around obstacles by going around the left side or right side depending
		;((equal? (get-square-info environment 1) 'empty) (turn-towards-veggies "LEFT"))
		;((equal? (get-square-info environment 3) 'empty) (turn-towards-veggies "RIGHT"))
		
		; stuck with obstacles on right, left, and in front 
		(#t  "STAY"
			;(begin
			;	(set! state "BACKTRACK")
			;	(turn-towards-veggies "AROUND")
			;)
		) 		 		
	)
)

; move agent towards the targeted vegetation
(define (move-towards-veggies amount)
	(display "move-towards-veggies")
	(newline)
	(newline)
	(cond
		((null? my-vegetations) (random-move))
		((and (equal? (cadaar my-vegetations) 1) (> amount 1)) (move-forward 0))  
		((and (equal? (cadaar my-vegetations) 2) (> amount 2)) (move-forward 1))
  		((and (equal? (cadaar my-vegetations) 3) (> amount 3)) (move-forward 2))
		((and (equal? (cadaar my-vegetations) 4) (> amount 4)) (move-foward 3)) 
		(#t (move-forward amount))
	)     
)

; moving forward function
(define (move-forward amount)
	(cond
		((equal? amount 0) "STAY")
		((equal? amount 1) "MOVE-PASSIVE-1")
		((equal? amount 2) "MOVE-PASSIVE-2")
		((equal? amount 3) "MOVE-PASSIVE-3")
	)
)


; turn in the direction you need to in order to reach 
(define (turn-towards-veggies direction)
	(display "turn-towards-veggies")
	(newline)
	(display direction)
	(newline)
	(display my-vegetations)
	(newline)
	(newline)
	(begin
		(cond
			((equal? direction "LEFT") (rotate-left my-vegetations))
			((equal? direction "RIGHT") (rotate-right my-vegetations))
			((equal? direction "AROUND") (rotate-around my-vegetations))
		)
		(cond
			((equal? direction "LEFT") "TURN-LEFT")
            ((equal? direction "RIGHT") "TURN-RIGHT")
            ((equal? direction "AROUND") "TURN-AROUND")
		)
	)
)

(define (rotate-right my-veggies)
	(cond
		((null? my-veggies) '())	
		(#t (set! my-vegetations (cons (list (list (- 0 (cadaar my-veggies)) (caaar my-veggies)) (cadar my-veggies)) (rotate-right-helper (cdr my-veggies)))))
	)
)

; we don't want to keep setting my-vegetations
(define (rotate-right-helper my-veggies)
	(cond
		((null? my-veggies) '())
		(#t (cons (list (list (- 0 (cadaar my-veggies)) (caaar my-veggies)) (cadar my-veggies)) (rotate-right-helper (cdr my-veggies))))
	)
)

(define (rotate-left my-veggies)
	(cond
        ((null? my-veggies) '())
        (#t (set! my-vegetations (cons (list (list (cadaar my-veggies) (- 0 (caaar my-veggies))) (cadar my-veggies)) (rotate-left-helper (cdr my-veggies)))))
    )
)

(define (rotate-left-helper my-veggies)
	(cond
	 	((null? my-veggies) '())
       	(#t (cons (list (list (cadaar my-veggies) (- 0 (caaar my-veggies))) (cadar my-veggies)) (rotate-left-helper (cdr my-veggies))))
	)
)

(define (rotate-around my-veggies)
	(cond
        ((null? my-veggies) '())
        (#t (set! my-vegetations (cons (list (list (- 0 (caaar my-veggies)) (- 0 (cadaar my-veggies))) (cadar my-veggies)) (rotate-around-helper (cdr my-veggies)))))
    )
)

(define (rotate-around-helper my-veggies)
	(cond
		((null? my-veggies) '())
        (#t (cons (list (list (- 0 (caaar my-veggies)) (- 0 (cadaar my-veggies))) (cadar my-veggies)) (rotate-around-helper (cdr my-veggies))))
	)
)

; picks a random move of turning when exploring and something is in front of the agent
; #TESTED#
(define (random-turn)
    (let ((rand (random 3)))
        (cond
            ((equal? rand 0) (turn-towards-veggies "RIGHT"))
            ((equal? rand 1) (turn-towards-veggies "LEFT"))
            ((equal? rand 2) (turn-towards-veggies "AROUND"))           
        )
    )
)

; picks a random move of turning or moving passively while exploring
;  #TESTED#
(define (random-move)
    (let ((rand (random 9)))
        (cond
            ((equal? rand 0) (turn-towards-veggies "RIGHT"))
            ((equal? rand 1) (turn-towards-veggies "LEFT"))
            ((equal? rand 2) (turn-towards-veggies "AROUND"))  
            ((equal? rand 3) "MOVE-PASSIVE-1")
            ((equal? rand 4) "MOVE-PASSIVE-2")
            ((equal? rand 5) "MOVE-PASSIVE-3")
            ((equal? rand 6) "MOVE-PASSIVE-1")
            ((equal? rand 7) "MOVE-PASSIVE-2")
            ((equal? rand 8) "MOVE-PASSIVE-3")
        )
    )
)
