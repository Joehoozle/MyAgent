(define predators '())
(define current-closest-predator 0)

; reacting to a predator
(define (analyze-predator predator current-square)
    (begin
        ; if predator type has not been documented, document 
        (if (not (lookup-id (cadr predator))) (set! predators (cons (cadr predator) predators))) 

        ; if predator is within range, state changes to scared
        (check-scary current-square)
        
        ; if this predator is the closest, it is the new one you should flee from
        let((((manhatten-distance current-square) distance))
            (if (< distance current-closest-predator) (set! current-closest-predator distance))
        )
    )
)


; deciding whether to be scared or not based on where predator is
(define (check-scary current-square)
    (begin
        ; if a predator is in sight of view, become scared
        (if (and (< current-square 24) (not (equal? state "SCARED"))) (set! state "SCARED"))
        
        ; if you are scared and the predator moves closer towards you and is within 2 spots of you, enter flight mode
        (let (((manhatten-distance current-square) distance))
            (if (and (and (equal? state "SCARED") (< distance current-closest-predator)) (<= distance 2)) (set! state "FLIGHT"))
        )
    )
)