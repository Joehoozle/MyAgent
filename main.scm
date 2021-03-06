(load "movement.scm")
(load "vegetation.scm")
(load "predator.scm")
(load "eat.scm")

; state that the AI is in
(define state "EXPLORE")

; counts the number of turns the agent is exploring. Once it gets to a certain maximum, it will stop exploring and begin normal survival methods.
(define explore-count 0)

; if I spend too much time searching for one veggie, I should switch to the next
(define veggie-count 0)



(define (initialize-agent) "Hey!")

(define (choose-action current-energy previous-events percepts)
    (begin 
		(display "beginning of turn logic")
		(newline)
		(display my-vegetations)
		(newline)
		(display state)
		(newline)

		; explore for the first 10 turns by allowing random moves
        (if (< explore-count 10) (set! explore-count (+ explore-count 1)))
	
		; only try to reach a veggie for 20 turns as it may be blocked
		(if (< veggie-count 20) (set! veggie-count (+ veggie-count 1)))

		; once exploring has stopped, begin surviving
        (if (and (equal? explore-count 10) (equal? state "EXPLORE")) (set! state "SURVIVING"))
		
		(analyze-events previous-events percepts)
		
		 ;if veggie count is too much, move onto next vegetation
		(if (and (equal? veggie-count 20) (not (null? my-vegetations)))
			(begin
				(set! my-vegetations (ate (cdr my-vegetations) (list (car my-vegetations))))
				(set! veggie-count 0)
			)
		)

		(if (equal? state "SURVIVING")
			(let ((rand (random 200)))
        		(cond
            		((equal? rand 21) 
						(begin
							(display "Now switching to explore")
							(newline)
							(set! explore-count (- explore-count 5))
							(set! state "EXPLORE")
        			
						)
					)
				)
    		)
		)

		(analyze-environment percepts)
        (make-choice percepts current-energy)
    )
)

;--------------------------------------------------------------------------------------------------------
; ANALYZE ENVIRONMENT

; analyze the environment square by square
(define (analyze-environment-helper environment current-square)
    (begin
		(display "analyze-environment")
		(newline)
		(newline)
        (let ((content (get-square-info environment current-square)))
			(cond	
				((equal? content 'empty) '())
				((equal? content 'barrier) '())
           		;((equal? (car content) 'predator) (analyze-predator content current-square))
           		((equal? (car content) 'vegetation) (analyze-vegetation content current-square))
            )
        )
    	(cond
			; done observing every square
			((equal? current-square 35) '())
			; still have more to go
			(#t (analyze-environment-helper environment (+ current-square 1)))
		)
	)
)

; wrapper function for analyzing the environment
(define (analyze-environment environment) (analyze-environment-helper environment 1))

;---------------------------------------------------------------------------------------------------------
; ANALYZE EVENTS

(define (analyze-events previous-events environment) 
	(display "analyze-events")
	(newline)
	(newline)
	(begin
		(process-movements previous-events)
		(process-ate previous-events)
		(process-attack previous-events environment)
	)
)

; if you were attacked, react accordingly
(define (process-attack previous-events environment)
    (cond
        ((null? previous-events) #f)
        ((equal? (caar previous-events) 'attacked-by)
            (begin
                (display "process-attack")
				(newline)
				(display (car previous-events))
                (newline)
                (newline)
                (attacked environment (car previous-events)) 
            )
        )
        (#t (process-attack (cdr previous-events) environment))
	)
)

; once you have eaten a vegetation, go to the next one on your list
(define (process-ate previous-events) 
	(cond
		((null? previous-events) #f)
		((and (equal? (caar previous-events) 'ate) (and (equal? (caaar my-vegetations) 0) (equal? (cadaar my-vegetations) 1)))
			(begin
				(display "process-ate")
				(newline)
				(newline)
				(set! veggie-count 0)
				(set! my-vegetations (ate (cdr my-vegetations) (list (car my-vegetations))))
			)
		)
		(#t (process-ate (cdr previous-events)))
	)
) 

; append the front of the vegetation list onto the back
(define (ate front back) 
	(begin
		(display "ate")
		(newline)
		(newline)
		(append front back)
	)
)


; if you moved, process the coordinates of the vegetations list 
(define (process-movements previous-events)
	(cond
		((null? previous-events) #f)
		((equal? (caar previous-events) 'moved) 
			(begin
				(display "process-movements")
				(newline)
				(display (car previous-events))
				(newline)
				(newline)
				(change-vegetation-position my-vegetations (cadar previous-events))
				(set! my-vegetations (drop-vegetations my-vegetations 15))
			)
		)	
		(#t (process-movements (cdr previous-events)))
	)
)

; go through everything in my vegetation list and subtract the appropriate amount
(define (change-vegetation-position my-veggies amount)
	(display "change-vegetation-position")
	(newline)
	(newline)
	(cond
        ((null? my-veggies) '())
        (#t (set! my-vegetations (cons (list (list (caaar my-veggies) (- (cadaar my-veggies) amount)) (cadar my-veggies)) (change-vegetation-position-helper (cdr my-veggies) amount))))
	)
)

(define (change-vegetation-position-helper my-veggies amount)  
	(cond
    	((null? my-veggies) '())
       	(#t (cons (list (list (caaar my-veggies) (- (cadaar my-veggies) amount)) (cadar my-veggies)) (change-vegetation-position-helper (cdr my-veggies) amount)))
    )
)

; drop vegetations that have gotten too far away
(define (drop-vegetations my-veggies distance) 
	(cond
		((null? my-veggies) '())
		((> (+ (caaar my-veggies) (cadaar my-veggies)) distance) (drop-vegetations (cdr my-veggies) distance))
		(#t (cons (car my-veggies) (drop-vegetations (cdr my-veggies) distance)))
	)
)

;---------------------------------------------------------------------------------------------------------
; MAKE CHOICE

(define (make-choice environment current-energy)
    (display "make-choice")
	(newline)
	(newline)
	(cond

       	; begin running away from predator by turning around
		((equal? state "FLIGHT-SPOTTED-0") 
            (begin
                (set! state "FLIGHT-SPOTTED-1")
                (turn-towards-veggies "AROUND")
            )
        )
		
		; move away from predator
        ((equal? state "FLIGHT-SPOTTED-1")
            (begin
                (set! state "EXPLORE")
				(cond
					((and (and (equal? (get-square-info environment 2) 'empty) (equal? (get-square-info environment 6) 'empty)) (equal? (get-square-info environment 12) 'empty)) (move-forward 3))
                    ((and (equal? (get-square-info environment 2) 'empty) (equal? (get-square-info environment 6) 'empty)) (move-forward 2))
                    ((equal? (get-square-info environment 2) 'empty) (move-forward 1))
					(#t
                    	(begin
							(set! state "FLIGHT-SPOTTED-1")
                        	(turn-towards-veggies "LEFT")
                        )
                    )
                )
            )
        ) 

		; turn to the left to avoid obstacle in front
		((equal? state "FLIGHT-ATTACKED-TURN") 
            (begin
                (set! state "FLIGHT-ATTACKED-MOVE")
                (turn-towards-veggies "LEFT")
            )
        )

		; if the vegetation in front is more energy than the last attack, it is worth it to eat it
		((equal? state "FLIGHT-ATTACKED-EAT")
			(begin
             	(set! state "FLIGHT-ATTACKED-TURN")
            	(eat-choice environment current-energy)
			)
		)
	
		; move forward as predator is on left, right, or behind
		((equal? state "FLIGHT-ATTACKED-MOVE")
            (begin
                (set! state "EXPLORE")
				(cond 
                	((and (and (equal? (get-square-info environment 2) 'empty) (equal? (get-square-info environment 6) 'empty)) (equal? (get-square-info environment 12) 'empty)) (move-forward 3))
        			((and (equal? (get-square-info environment 2) 'empty) (equal? (get-square-info environment 6) 'empty)) (move-forward 2))
        			((equal? (get-square-info environment 2) 'empty) (move-forward 1))
					(#t 
						(begin
							(set! state "FLIGHT-ATTACKED-MOVE")
							(turn-towards-veggies "LEFT")
						)
					)
				)
            )
        )

		; after turning left, move forward
		((equal? state "LEFT-AROUND-1") 
			(begin
				(set! state "LEFT-AROUND-2")
				(cond
					((equal? (get-square-info environment 3) 'empty) (move-forward 1))
					((equal? (get-square-info environment 7) 'empty) (move-forward 2))
					((equal? (get-square-info environment 13) 'empty) (move-forward 3))
					(#t (move-forward 1))
				)
			)	
		)

		; turn right and proceed
		((equal? state "LEFT-AROUND-2")
			(begin
				(set! state "SURVIVING")
				(turn-towards-veggies "RIGHT")
			)
		)

		; after turning right, move forward
		((equal? state "RIGHT-AROUND-1") 
			(begin
				(set! state "RIGHT-AROUND-2")
				(cond
                	((equal? (get-square-info environment 3) 'empty) (move-forward 1))
                	((equal? (get-square-info environment 7) 'empty) (move-forward 2))
               		((equal? (get-square-info environment 13) 'empty) (move-forward 3))
					(#t (move-forward 1))
				)
			)	
		)
		
		; turn left to procede
		((equal? state "RIGHT-AROUND-2")
			(begin
				(set! state "SURVIVING")
				(turn-towards-veggies "LEFT")
			)
		)

		; random movement to see environment 
		((equal? state "EXPLORE") (smart-random-move environment))
		; main movement towards vegetations
		((equal? state "SURVIVING") (educated-move environment current-energy))
    )
)
;-----------------------------------------------------------------------------------------------------------
; General Functions

; make sure something is not a barrier or empty so you can car it
(define (not-barrier-empty x) 
	(cond
		((equal? x 'empty) #f)
		((equal? x 'barrier) #f)
		(#t #t)
	)
)

; returns what is front of the agent (just the string of what kind of object it is)
(define (in-front environment) 
	(cond
		((equal? (cadar environment) 'barrier) 'barrier)
		((equal? (cadar environment) 'empty) 'empty)
		(#t (caadar environment))
	)
)

; get info from a specific box in environment view 
(define (get-square-info environment square-number) 
        (cond
            ((equal? square-number 1) (caar environment))
            ((null? (cdar environment)) (get-square-info (cdr environment) (- square-number 1)))
            (#t (get-square-info (cons (cdar environment) (cdr environment)) (- square-number 1)))   
        )
)

; get square info only for veggies, predators, and agents
(define (get-square-info-select environment square-number)
        (cond
            ((and (equal? square-number 1) (equal? (caar environment) 'barrier)) #f)
			((and (equal? square-number 1) (equal? (caar environment) 'empty)) #f)
			((equal? square-number 1) (caar environment))
            ((null? (cdar environment)) (get-square-info-select (cdr environment) (- square-number 1)))
            (#t (get-square-info-select (cons (cdar environment) (cdr environment)) (- square-number 1)))
        )
)

; lookup predator or vegatation id in id-list to see if it is there
(define (lookup-id id-list id-found)
    (cond
        ((null? id-list) #f)
        ((equal? id-found (car id-list)) #t)   
        (#t (lookup-id (cdr id-list) id-found))
    )
)

; lookup the manhatten distance 
(define (manhatten-distance current-square) 
	(cond 
        ((and ( < current-square 4) (> current-square 0)) (+ 0 (abs (- 2 current-square))))  
        ((and ( < current-square 9) (> current-square 3)) (+ 1 (abs (- 6 current-square))))
        ((and ( < current-square 16) (> current-square 8)) (+ 2 (abs (- 12 current-square)))) 
        ((and ( < current-square 25) (> current-square 15)) (+ 3 (abs (- 20 current-square))))
        ((and ( < current-square 36) (> current-square 24)) (+ 4 (abs (- 30 current-square))))        
    )
)

; get x-distance from to current-square
(define (x-distance current-square)
    (cond 
        ((and ( < current-square 4) (> current-square 0)) (- current-square 2))  
        ((and ( < current-square 9) (> current-square 3)) (- current-square 6))
        ((and ( < current-square 16) (> current-square 8)) (- current-square 12)) 
        ((and ( < current-square 25) (> current-square 15)) (- current-square 20))
        ((and ( < current-square 36) (> current-square 24)) (- current-square 30))        
    )
)

; get y-distance to current-square
(define (y-distance current-square)
    (cond 
        ((and ( < current-square 4) (> current-square 0)) 1)   
        ((and ( < current-square 9) (> current-square 3)) 2)
        ((and ( < current-square 16) (> current-square 8)) 3) 
        ((and ( < current-square 25) (> current-square 15)) 4)
        ((and ( < current-square 36) (> current-square 24)) 5)        
    )
)


; helper function to find the size of a list
(define (list-size-helper alist num)
  (cond
      ((null? alist) num)
      (#t (list-size-helper (cdr alist) (+ num 1)))
  )
)

; wrapper function to find the size of a list
(define (list-size alist) (list-size-helper alist 0))
