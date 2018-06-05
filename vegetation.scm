; all vegetations seen
(define vegetations '())

; the 3 vegetations I am currently traveling between
; the list for each vegetation is a list of x y coordinates for the 0 direction, and the bloom
(define my-vegetations '())

; the current minimum in my-vegetations
(define current-minimum-bloom 0)


; analyzes viewable vegetation to help pick 3 to travel between
(define (analyze-vegetation vegetation current-square)
	(display "analyze vegetation")
	(newline)
	(newline)
    (begin
		; add veggie to vegetations list if it has not been seen already
        (if (not (lookup-id vegetation (cadr vegetation))) (set! vegetations (cons (list (cadr vegetation) (caddr vegetation)) vegetations)))
        
		(cond	
			
			; if less than 3 veggies have been seen, add veggie to my-vegetations list 
            ((and (< (list-size my-vegetations) 3) (not (veggie-being-tracked my-vegetations current-square))) 
                (begin
					(display "there are less than 3 vegetations being kept track of")
					(newline)
					(newline)
                    (set! my-vegetations (cons (list (list (x-distance current-square) (y-distance current-square)) (caddr vegetation)) my-vegetations))
                    (set! current-minimum-bloom (find-min-bloom my-vegetations))
                )
            )

			; update the bloom values of an existing tracked vegetation if needed
			((check-for-same-veggie my-vegetations vegetation current-square)
				(begin
					(display "performing an update of vegetations being followed...old: ")
					(display my-vegetations)
					(newline)
					(display "new: ")
					(display (update-bloom-value my-vegetations vegetation current-square))
					(newline)
					(newline)	
					(set! my-vegetations (update-bloom-value my-vegetations vegetation current-square))
					(set! current-minimum-bloom (find-min-bloom my-vegetations))	
				)
			)
	
			; if 3 veggies have been seen, see if this veggie is better than the others and add it if it is better than the minimum bloom
            ((and (and (equal? (list-size my-vegetations) 3) (> (caddr vegetation) current-minimum-bloom) (not (veggie-being-tracked my-vegetations current-square))))
                (begin
					(display "Preparing to replace a followed vegetation because one seen is larger")
					(newline)
					(display "This is the bloom/current minimum: ")
					(display (caddr vegetation))
					(newline)
					(display current-minimum-bloom)
					(newline)
					(newline)
                    (set! my-vegetations (remove-bloom my-vegetations current-minimum-bloom))
                    (set! my-vegetations (cons (list (list (x-distance current-square) (y-distance current-square)) (caddr vegetation)) my-vegetations))
                    (set! current-minimum-bloom (find-min-bloom my-vegetations))
                )
            )
        )
    )
)


; function to see if a vegetation is already being tracked
(define (veggie-being-tracked my-veggies current-square) 
	(cond
		((null? my-veggies) #f)
		((equal? (caar my-veggies) (list (x-distance current-square) (y-distance current-square))) #t)
		(#t (veggie-being-tracked (cdr my-veggies) current-square)) 
	)
)



; if a veggie has changed bloom amount (to more, as you want to track max) from previous turn, don't add it as a new veggie. Just update it.
; #TESTED#
(define (check-for-same-veggie my-veggies vegetation current-square)
	(cond
		((null? my-veggies) #f)
		((and (equal? (caar my-veggies) (list (x-distance current-square) (y-distance current-square))) (< (cadar my-veggies) (caddr vegetation))) #t)
		(#t (check-for-same-veggie (cdr my-veggies) vegetation current-square))
	) 
)


; replace old value of vegetation in my-veggies with newly observeved number
; #TESTED#
(define (update-bloom-value my-veggies vegetation current-square)
	(cond 
		((null? my-veggies) '())
        ((equal? (caar my-veggies) (list (x-distance current-square) (y-distance current-square))) (cons (list (list (x-distance current-square) (y-distance current-square)) (caddr vegetation)) (update-bloom-value (cdr my-veggies) vegetation current-square)))
        (#t (cons (car my-veggies) (update-bloom-value (cdr my-veggies) vegetation current-square)))
	)
)

; finds the minimum bloom of a list of vegetations 
; #TESTED#
(define (find-min-bloom-helper passed-vegetations min)
    (cond 
        ((null? passed-vegetations) min)
        ((< (cadar passed-vegetations) min) (find-min-bloom-helper (cdr passed-vegetations) (cadar passed-vegetations)))
        (#t (find-min-bloom-helper (cdr passed-vegetations) min))    
    )
)

; wrapper function for finding the minimum blooming vegetation
; #TESTED$
(define (find-min-bloom passed-vegetations) (find-min-bloom-helper passed-vegetations 10000000))


; function for removing a bloom value. The flag is used to not delete the same value twice.
; #TESTED#
(define (remove-bloom-helper passed-vegetations bloom-value flag)
    (cond 
        ((null? passed-vegetations) '())    
        ((and (equal? (cadar passed-vegetations) bloom-value) (equal? flag 0)) (remove-bloom-helper (cdr passed-vegetations) bloom-value 1))
        (#t (cons (car passed-vegetations) (remove-bloom-helper (cdr passed-vegetations) bloom-value flag)))
    )

)

; wrapper function for removing a bloom value 
; #TESTED#
(define (remove-bloom passed-vegetations bloom-value) (remove-bloom-helper passed-vegetations bloom-value 0))
