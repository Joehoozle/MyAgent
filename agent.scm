
; INITIAL: First state is exploring
; HUNGRY: Locked onto a piece of vegetation and going to it
; SCARED: Predator is in view
; FLIGHT: Predator has moved closer (probably locked onto agent) after being scared / agent has been hit by predator



; state that the AI is in
(define state "INITIAL")

; movement needed to get to locked on block, or the current block
(define lock-on '())


; vegetation variables
;
;current vegetation
(define current-vegetation )


; current vegatation's bloom cycle
(define bloom-watch 1)




; (define (initialize-agent)

; )

(define (choose-action energy events environment)
    (cond  
        ; first thing to do is see if there is an immediate threat that is attacking you from a square you cannot see   
        ((analyze-events events) (manic-movement energy)) 
    )
)

; analysis functions
;
;----------------------------------------------------------------------------------------------------------------------




; reacting to a barrier
(define (analyze-barrier current-square))

; analyze the environment
(define (analyze-environment-helper environment current-square)
    (begin
        (let ((content (get-square-info environment current-square)))
            (cond
                ((equal? content 'barrier) (analyze-barrier current-square))
                ;((equal? content 'empty) )
                ((equal? (car content) agent) (analyze-agent (car content)))
                ((equal? (car content) predator) (analyze-predator (car content) current-square))
                ((equal? (car content) vegetation) (analyze-vegetation (car content)))
            )
        )
    )
    (analyze-environment-helper environment (+ current-square 1))
)

; wrapper function for analyzing the environment
(define (analyze-environment environment) (analyze-environment-helper environment 1))


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



;--------------------------------------------------------------------------------------------------------------------------



; movement functions
;
;
; manic-movement 
; (define (manic-movement energy)
;     (begin 
;         ;change state to running away
;         ;move as much as you can with energy
;     )
; )


