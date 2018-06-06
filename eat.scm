(define (eat-choice environment current-energy) 
	(display "eat choice")
	(newline)
	(display "square to eat from type: ")
	(display (get-square-info environment 2))
	(newline)
	
	 (let ((agent1 (get-square-info environment 1)) (agent2 (get-square-info environment 6)) (agent3 (get-square-info environment 3)) (agent4 (get-square-info environment 12)) (agent5 (get-square-info environment 20)) (agent6 (get-square-info environment 30)))
		(cond 
			; check for competing agents directly next to veggie
			((or 
				(or 
					(and (not-barrier-empty agent1) (and (equal? (car agent1) 'agent) (equal? (cadddr agent1) 'right))) 
					(and (not-barrier-empty agent2) (and (equal? (car agent2) 'agent) (equal? (cadddr agent2) 'left)))) 
				(and (not-barrier-empty agent3) (and (equal? (car agent3) 'agent) (equal? (cadddr agent3) 'towards)))) 
			(eat-with-agents environment current-energy))

			; extra check for agents within my line of sight that are a move away
            ((or 
				(or 
					(and (not-barrier-empty agent4) (and (equal? (car agent4) 'agent) (equal? (cadddr agent4) 'towards))) 
					(and (not-barrier-empty agent5) (and (equal? (car agent5) 'agent) (equal? (cadddr agent5) 'towards)))) 
				(and (not-barrier-empty agent6) (and (equal? (car agent6) 'agent) (equal? (cadddr agent6) 'towards)))) 
			(eat))

			; if you have no threats, and the current bloom is not the max you have seen it, you should wait
			((not (equal? (caddr (get-square-info environment 2)) (cadar my-vegetations))) "STAY")  
			
			((not (equal? (caddr (get-square-info environment 2)) 0)) (eat))	
		
			(#t "STAY")
		)
	)
)

(define (eat) "EAT-PASSIVE")


; function used to decide whether to eat aggressivly or passively with other agents
(define (eat-with-agents environment current-energy)
	; deciding whether I should eat aggressively
	(let ((agent1 (get-square-info environment 1)) (agent2 (get-square-info environment 6)) (agent3 (get-square-info environment 3)))
		(cond
            ((or 
				(or
					(and 
						(and
							(and
								(not-barrier-empty agent1) 
								(equal? (car agent1) 'agent)
							) 
							(equal? (cadddr agent1) 'right)
						)
						(> (expt (caddr agent1) 2) current-energy)   	 
						
					(and 
						(and
							(and
								(not-barrier-empty agent2) 
								(equal? (car agent2) 'agent)
							) 
							(equal? (cadddr agent2) 'left)
						)
						(> (expt (caddr agent2) 2) current-energy)
					)
 
					(and
						(and 
							(and
								(not-barrier-empty agent3)
								(equal? (car agent3) 'agent) 
							)
							(equal? (cadddr agent3) 'towards)
						)
						(> (expt (caddr agent3) 2) current-energy)
					)
				)
			))
				("EAT-PASSIVELY"))
		
		; either there is no one contesting, or I am stronger than all of them
			(#t "EAT-AGGRESIVE")
		)
	)
)
