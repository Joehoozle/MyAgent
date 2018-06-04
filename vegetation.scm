; all vegetations seen
(define vegetations '())

; the 3 vegetations I am currently traveling between
; the list for each vegetation is a list of x y coordinates for the 0 direction, and the bloom
(define my-vegetations '())

; the vegetation that currently is being traveled to
;(define targeted-vegetation '())

; the current minimum in my-vegetations
(define current-minimum-bloom 0)


; analyzes viewable vegetation to help pick 3 to travel between
(define (analyze-vegetation vegetation current-square)
	(display "veggie")
	(newline)
	(newline)
    (begin
        (if (not (lookup-id vegetation (cadr vegetation))) (set! vegetations (cons (list (cadr vegetation) (caddr vegetation)) vegetations)))
        (cond
            ((and (< (list-size my-vegetations) 3)) 
                (begin
					;
                    (set! my-vegetations (cons (list (list (x-distance current-square) (y-distance current-square)) (caddr vegetation)) my-vegetations))
                    (set! current-minimum-bloom (find-min-bloom my-vegetations))
                )
            )
            ((and (equal? (list-size my-vegetations) 3) (> (caddr vegetation) current-minimum-bloom))
                (begin
                    (set! my-vegetations (remove-bloom my-vegetations current-minimum-bloom))
                    (set! my-vegetations (cons (list (list (x-distance current-square) (y-distance current-square)) (caddr vegetation)) my-vegetations))
                    (set! current-minimum-bloom (find-min-bloom my-vegetations))
                )
            )
        )
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
