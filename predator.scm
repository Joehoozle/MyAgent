(define predators '())
;(define current-closest-predator 0)

;----------------------------------------------------------------------------------------
;ANALYSIS FROM PERCEPT

; analyzing a predator in view
(define (analyze-predator predator current-square)
	(display "predator detected")
	(newline)
	(newline)
    (begin
        ; if predator type has not been documented, document it via its id
        (if (not (lookup-id predators (cadr predator))) (set! predators (cons (cadr predator) predators))) 
		
		(display "current predators: ")
		(display predators)
		(newline)
		(newline)
		
		; if predator is within a range where it could scare agent, react to that
        (check-scared current-square)
    )
)


; deciding whether to be scared or not based on where predator is
(define (check-scared current-square)

    ; since predators can only move one space, the manhatten distance is how many turns it takes for them to get to you
    (let ((distance (manhatten-distance current-square)))	
		(begin
        	; if they are 2 turns from being adjacent to you, you have to run
			(if (<= distance 2) (set! state "FLIGHT-SPOTTED-0"))
			; otherwise, you do not need to run
			(if (> distance 2) '())
		)
    )
)

;--------------------------------------------------------------------------------------------
; REACTION FROM ATTACK

; THINK OF EVERY WAY YOU COULD BE APPROACHED BY MULTUPLE PREDATORS OR STUCK

(define (attacked environment event) 
	(display event)
	(newline)
	(display (in-front environment))
	(newline)
	(display (get-square-info environment 2))
	(newline)
	(cond
		; if there is a plant blooming in front of the agent that will net points if eaten and taking another hit, eat it
		((and (and (not-barrier-empty (get-square-info environment 2)) (equal? (car (get-square-info environment 2)) 'vegetation)) (> (caddr (get-square-info environment 2)) (abs (caddr event)))) (set! state "FLIGHT-ATTACKED-EAT"))
	
		; there is an empty path in front of you	
		((equal? (get-square-info environment 2) 'empty) (set! state "FLIGHT-ATTACKED-MOVE"))		

		; there is not an empty path in front of you
		(#t (set! state "FLIGHT-ATTACKED-TURN"))
	)
) 




