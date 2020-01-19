#lang racket

(provide  param1 param2 param3 param4 param5 appartient? exp_atom? rech_form)

(define param1 (lambda(exp) 
                 (car exp)
                 )
  )

(define param2 (lambda(exp) 
                 (second exp)
                 )
  )

(define param3 (lambda(exp) 
                 (third exp)
                   )
  )

(define param4 (lambda(exp) 
                 (fourth exp)
                   )
  )

(define param5 (lambda(exp) 
                 (fifth exp)
                   )
  )



;definition de la fonction permettant de savoir si un element appartient à l'ens des opérateurs
(provide appartient? exp_atom? rech_form)

(define appartient? (lambda (a)
                        (cond 
                        ((or (eqv? '* a) (eqv? '+ a)) #t)
                        (#t #f)
                         ))
  )


;definition de la fonction permettant de savoir si une expression est de la "x + y", x et y étant des atomes

(define exp_atom? (lambda(exp)
                    (cond 
                      ((and (list? exp) (and (not (list? (param1 exp))) (not (list? (param2 exp))) (not (list? (param3 exp))))) #t) 
                      (else #f)
                      ))
  )


;définition de la fonction permettant de savoir s'il sagit de la forme prefixée, infixée ou suffixée

(define rech_form (lambda (exp)
                    (cond 
                       ((appartient? (param1 exp)) 1) 
                       ((appartient? (param2 exp)) 2)
                       ((appartient? (param3 exp)) 3)
                       ))
  )
