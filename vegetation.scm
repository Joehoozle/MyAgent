; all vegetations seen
(define vegetations '())

; the 3 vegetations I am currently traveling between
; the list for each vegetation is an integer for the direction, a list of x y coordinates for the 0 direction, and the bloom
(define my-vegetations '())

(define targeted-vegetation '())

; the current minimum in my-vegetations
(define current-minimum-bloom 0)


; analyzes viewable vegetation to help pick 3 to travel between
(define (analyze-vegetation vegetation current-square)
    (begin
        (if (not (lookup-id vegetation (cadr vegetation))) (set! vegetations (cons (list (cadr vegetation) (caddr vegetation)) vegetations)))
        (cond
            ((and (< (list-size my-vegetations) 3)) 
                (begin
                    (set! my-vegetations (cons (list 0 (list (x-distance current-square) (y-distance current-square)) (caddr vegetation)) my-vegetations))
                    (set! current-minimum-bloom (find-min-bloom my-vegetations))
                )
            )
            ((and (equal? (list-size my-vegetations) 3) (> (caddr vegetation) current-minimum-bloom))
                (begin
                    (remove-bloom (find-min-bloom my-vegetations))
                    (set! my-vegetations (cons (list 0 (list (x-distance current-square) (y-distance current-square)) (caddr vegetation)) my-vegetations))
                    (set! current-minimum-bloom (find-min-bloom my-vegetations))
                )
            )
        )
    )
)
