
; EXPLORE: Initial exploring mode
; HUNGRY: Locked onto a piece of vegetation and going to it
; FLIGHT: Predator has moved closer (probably locked onto agent) after being scared / agent has been hit by predator



; state that the AI is in
(define state "EXPLORE")

; counts the number of turns the agent is exploring. Once it gets to a certain maximum, it will stop exploring and begin normal survival methods.
(define explore-count 0)


(define (choose-action energy events environment)
    (begin 
        (set! turn-count (+ turn-count 1))
        (analyze-environment environment)
        (analyze-events events)
        (make-choice)
    )
)

;--------------------------------------------------------------------------------------------------------
; ANALYZE ENVIRONMENT

; analyze the environment square by square
(define (analyze-environment-helper environment current-square)
    (begin
        (let ((content (get-square-info environment current-square)))
            (cond
                ;((equal? content 'barrier) (analyze-barrier current-square))
                ;((equal? (car content) 'agent) (analyze-agent (car content)))
                ((equal? (car content) 'predator) (analyze-predator (car content) current-square))
                ((equal? (car content) 'vegetation) (analyze-vegetation (car content) current-square))
            )
        )
    )
    (analyze-environment-helper environment (+ current-square 1))
)

; wrapper function for analyzing the environment
(define (analyze-environment environment) (analyze-environment-helper environment 1))

;---------------------------------------------------------------------------------------------------------
; ANALYZE EVENTS

;---------------------------------------------------------------------------------------------------------
; MAKE CHOICE

; (define (make-choice envionment)
;     (cond
;         ((equal? state "Flight0") 
;             (begin
;                 (set! state "FLIGHT1")
;                 "AROUND"
;             )
;         )
;         ((equal? state "Flight1") 
;             (begin
;                 (set! state "EXPLORE")
;                 "AGRESSIVE-3"
;             )
;         )
;         ((equal? state "EXPLORE") (explore-movement))
        
        
        

;     )

; )











;-----------------------------------------------------------------------------------------------------------
; General Functions


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
