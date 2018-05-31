(define predators '())
(define current-closest-predator 0)

;----------------------------------------------------------------------------------------
;ANALYSIS

; analyzing a predator in view
(define (analyze-predator predator current-square)
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
    (let (((manhatten-distance current-square) distance))

        ; if they are 2 turns from being adjacent to you, you have to run
        (if ((<= distance 3)) (set! state "FLIGHT0"))
    )
)

;--------------------------------------------------------------------------------------------
;REACTION TO ATTACK




