(load "movement.scm")
(load "vegetation.scm")
(load "predator.scm")

; state that the AI is in
(define state "EXPLORE")

; counts the number of turns the agent is exploring. Once it gets to a certain maximum, it will stop exploring and begin normal survival methods.
(define explore-count 0)



(define (initialize-agent) "Hey!")

(define (choose-action current-energy previous-events percepts)
    (begin 
		(display my-vegetations)
		(newline)
		(display current-minimum-bloom) 
		(newline)
        (if (< explore-count 20) (set! explore-count (+ explore-count 1)))
        (if (equal? explore-count 20) (set! state "SURVIVING"))
		(analyze-events previous-events percepts)
		(analyze-environment percepts)
        (make-choice percepts)
    )
)

;--------------------------------------------------------------------------------------------------------
; ANALYZE ENVIRONMENT

; analyze the environment square by square
(define (analyze-environment-helper environment current-square)
    (begin
        (let ((content (get-square-info environment current-square)))
			(cond	
				((equal? content 'empty) '())
				((equal? content 'barrier) '())
           		;((equal? content 'barrier) (analyze-barrier current-square))
         		;((equal? (car content) 'agent) (analyze-agent (car content)))
           		((equal? (car content) 'predator) (analyze-predator content current-square))
           		((equal? (car content) 'vegetation) (analyze-vegetation content current-square))
            )
        )
    	(cond
			((equal? current-square 35) '())
			(#t (analyze-environment-helper environment (+ current-square 1)))
		)
	)
)

; wrapper function for analyzing the environment
(define (analyze-environment environment) (analyze-environment-helper environment 1))

;---------------------------------------------------------------------------------------------------------
; ANALYZE EVENTS

(define (analyze-events previous-events environment) 
	(begin
		(process-movements previous-events)
		(process-attack previous-events environment)
	)
)

; if you were attacked, react accordingly
(define (process-attack previous-events environment)
    (cond
        ((null? previous-events) #f)
        ((equal? (caar previous-events) 'attacked-by)
            (begin
                (display (car previous-events))
                (newline)
                (newline)
                (attacked environment (car previous-events))
            )
        )
        (#t (process-attack (cdr previous-events) environment))
	)
)

; if you moved, change movement accordingly
(define (process-movements previous-events)
	(cond
		((null? previous-events) #f)
		((equal? (caar previous-events) 'moved) 
			(begin
				(display (car previous-events))
				(newline)
				(newline)
				(change-vegetation-position my-vegetations (cadar previous-events))
			)
		)	
		(#t (process-movements (cdr previous-events)))
	)
)

; go through everything in my vegetation list and subtract the appropriate amount
(define (change-vegetation-position my-veggies amount)
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

;---------------------------------------------------------------------------------------------------------
; MAKE CHOICE

(define (make-choice environment)
    (cond
       	; begin running away sequence from predator
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
                (move-towards-veggies 3)
            )
        ) 

		; sandwhiched between two predators
		((equal? state "FLIGHT-ATTACKED-0") 
            (begin
                (set! state "FLIGHT-SPOTTED-1")
                (turn-towards-veggies "LEFT")
            )
        )
	
		; predator is in front of you
		((equal? state "FLIGHT-ATTACKED-1")
            (begin
                (set! state "FLIGHT-SPOTTED-1")
                (turn-towards-veggies "AROUND")
            )
        )
	
		; predator is to left, right, or behind you
		((equal? state "FLIGHT-ATTACKED-2")
            (begin
                (set! state "FLIGHT-SPOTTED-1")
                (move-towards-veggies 3)
            )
        )

		; if you cannot move through space
		((equal? state "BACKTRACK") 
			(cond
				; turn around and a vegetation is there means that you are trapped inside of 4 vegetations 
				((not (equal? (caddr (get-square-info environment 2)) 0)) (eat environment))  
				((equal? (caddr (get-square-info environment 2)) 0) (turn-towards-veggies "LEFT"))
				
				; backtrack 2 spaces
				(#t 	
					(begin
						(set! state "EXPLORE")
						(move-towards-veggies 3)
					)
				)
			)
		)

		((equal? (in-front environment) 'vegetation) "EAT-PASSIVE")  
        ((equal? state "EXPLORE") (smart-random-move environment))
		((equal? state "SURVIVING") (educated-move environment))
    )
)
;-----------------------------------------------------------------------------------------------------------
; General Functions


 ; returns what is front of the agent
(define (in-front environment) 
	(cond
		((equal? (cadar environment) 'barrier) 'barrier)
		((equal? (cadar environment) 'empty) 'empty)
		(#t (caadar environment))
	)
)

; get info from a specific box in environment view 
; #TESTED#
(define (get-square-info environment square-number) 
        (cond
            ((equal? square-number 1) (caar environment))
            ((null? (cdar environment)) (get-square-info (cdr environment) (- square-number 1)))
            (#t (get-square-info (cons (cdar environment) (cdr environment)) (- square-number 1)))   
        )
)

; lookup predator or vegatation id in id-list to see if it is there
; #TESTED#
(define (lookup-id id-list id-found)
    (cond
        ((null? id-list) #f)
        ((equal? id-found (car id-list)) #t)   
        (#t (lookup-id (cdr id-list) id-found))
    )
)

; lookup the manhatten distance 
; #TESTED#
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
; #TESTED#
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
; #TESTED#
(define (y-distance current-square)
    (cond 
        ((and ( < current-square 4) (> current-square 0)) 0)   
        ((and ( < current-square 9) (> current-square 3)) 1)
        ((and ( < current-square 16) (> current-square 8)) 2) 
        ((and ( < current-square 25) (> current-square 15)) 3)
        ((and ( < current-square 36) (> current-square 24)) 4)        
    )
)


; helper function to find the size of a list
; #TESTED#
(define (list-size-helper alist num)
  (cond
      ((null? alist) num)
      (#t (list-size-helper (cdr alist) (+ num 1)))
  )
)

; wrapper function to find the size of a list
; #TESTED#
(define (list-size alist) (list-size-helper alist 0))
