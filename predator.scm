(define predators '())
(define current-closest-predator 0)

;----------------------------------------------------------------------------------------
;ANALYSIS

; analyzing a predator in view
(define (analyze-predator predator current-square)
	(display "predator")
	(newline)
	(newline)
    (begin
        ; if predator type has not been documented, document 
        (if (not (lookup-id predators (cadr predator))) (set! predators (cons (cadr predator) predators))) 

		; if predator is within range, state changes to scared
        (check-scared current-square)
    )
)


; deciding whether to be scared or not based on where predator is
(define (check-scared current-square)
    ; since predators can only move one space, the manhatten distance is how many turns it takes for them to get to you
    (let ((distance (manhatten-distance current-square)))
		(begin
        ; if they are 2 turns from being adjacent to you, you have to run
			(if (<= distance 3) (set! state "FLIGHT-SPOTTED-0"))
			(if (> distance 3) '())
		)
    )
)

;--------------------------------------------------------------------------------------------
; REACTION

(define (attacked environment event) 
	(display event)
	(newline)
	(newline)
	(cond
		; you are sandwhiched between two predators
		;((and (equal? (in-front environment) 'predator) (not (equal? (cadr event) (cadr (get-square-info environment 2))))) (set! state "FLIGHT-ATTACKED-0"))

		; predator is in front of you
		;((equal? (cadr event) (cadr (get-square-info environment 2))) (set! state "FLIGHT-ATTACKED-1"))
		
		; predator is to left, right, or behind agent
		(#t (set! state "FLIGHT-ATTACKED-2"))
	)
) 




