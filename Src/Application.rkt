#lang racket/gui

(require "Metier.rkt")
(provide simpli)

;definition de la fonction id, qui permet de savoir si un élément est un id

(define id? (lambda(exp)
              (cond
                ((and (not (number? exp)) (not (list? exp))) #t)
                (#t #f)
                ))
  )


;definition de la fonction de simplification des expressions surparenthesées
                  
(define simp_parenth (lambda(exp)
                       (cond                                                
                         ((and (list?  (cond ((pair? exp) (param1 exp)))) (and (eq? (length exp) 1)  (eq? (length exp) (length (param1 exp))))) (simp_parenth (param1 exp)))
                        (#t (cond 
                              ((and (pair? exp) (eq? (length exp) 1)) (param1 exp))                              
                              (else exp)
                            ))                        
                         ))
  )

;definition de la fonction permettant de determiner le type d'opération (prefixée, infixée ou suffixée) et renvoyant la forme nomalisée

(define operateur (lambda(exp)
                    (cond
                      ((eq? 1 (rech_form exp)) (list (param2 exp) (param1 exp) (param3 exp)))
                      ((eq? 2 (rech_form exp)) exp)
                      ((eq? 3 (rech_form exp)) (list (param1 exp) (param3 exp) (param2 exp)))
                      ))
  )

;definition de la fonction permettant de determiner si une expression additive comporte au moins un nombre.
;si oui, renvoit (#t somme_des_nombres liste_des_atomes)
;sinon renvoit (#f)
(define cont_1n (lambda(exp)
                  (cond
                    ((list? exp) (cond
                                   ((eqv? (param2 (operateur exp)) '+) (cond
                                                                            ((number?  (param1 (operateur exp))) (cond
                                                                                                                         
                                                                                                                          
                                                                                                                          ((id?  (param3 (operateur exp))) (list #t (param1 (operateur exp)) (param3 (operateur exp))))
                                                                                                                          ((list? (param3 (operateur exp))) (cond
                                                                                                                                                              ((param1 (cont_1n (param3 (operateur exp)))) (list #t (addition (param1 (operateur exp)) (param2 (cont_1n (param3 (operateur exp))))) (param3 (cont_1n (param3 (operateur exp))))))
                                                                                                                                                              (#t (list #t (param1 (operateur exp)) (simpli (param3 (operateur exp)))))
                                                                                                                                                              ))
                                                                                                                          (#t (list #t (param1 (operateur exp)) (param3 (operateur exp))))
                                                                                                                          ))
                                                                                                                                                        
                                                                            ((number?  (param3 (operateur exp))) (cond
                                                                                                                         
                                                                                                                          
                                                                                                                          ((id?  (param1 (operateur exp))) (list #t (param3 (operateur exp)) (param1 (operateur exp))))
                                                                                                                          ((list? (param1 (operateur exp))) (cond
                                                                                                                                                              ((param1 (cont_1n (param1 (operateur exp)))) (list #t (addition (param3 (operateur exp)) (param2 (cont_1n (param1 (operateur exp))))) (param3 (cont_1n (param1 (operateur exp))))))
                                                                                                                                                              (#t (list #t (param3 (operateur exp)) (simpli (param1 (operateur exp)))))
                                                                                                                                                              ))
                                                                                                                          (#t (list #t (param3 (operateur exp)) (param1 (operateur exp))))
                                                                                                                          ))
                                                                                                                                   
                                                                            ((id?  (param1 (operateur exp))) (cond 
                                                                                                                 ((list? (simpli (param3 (operateur exp)))) (cond
                                                                                                                                                        ((param1 (cont_1n (simpli (param3 (operateur exp))))) (list #t (param2 (cont_1n (simpli (param3 (operateur exp))))) (simpli (list (param1 (operateur exp)) '+ (param3 (cont_1n (simpli (param3 (operateur exp)))))))))
                                                                                                                                                        (#t (list #f))))
                                                                                                                 ((number? (simpli (param3 (operateur exp)))) (list #t (simpli (param3 (operateur exp))) (param1 (operateur exp))))
                                                                                                                 (#t (list #f))))
                                                                        
                                                                            
                                                                            ((id?  (param3 (operateur exp))) (cond 
                                                                                                                 ((list? (simpli (param1 (operateur exp)))) (cond
                                                                                                                                                        ((param1 (cont_1n (simpli (param1 (operateur exp))))) (list #t (param2 (cont_1n (simpli (param1 (operateur exp))))) (simpli (list (param3 (operateur exp)) '+ (param3 (cont_1n (simpli (param1 (operateur exp)))))))))
                                                                                                                                                        (#t (list #f))))
                                                                                                                 ((number? (simpli (param1 (operateur exp)))) (list #t (simpli (param1 (operateur exp))) (param3 (operateur exp))))
                                                                                                                 (#t (list #f))))
                                                                                                                                                                                                                                                                                                                                                                                                                       
                                                                           
                                                                            (#t (list #f))
                                                                            ))
                                   (#t (list #f))
                                   ))
                    (#t (list #f))
                    ))
  )
  
;definition de la fonction permettant de determiner si une expression est de la forme "(5 * x)"
;si oui renvoit (#t 5 x)
;sinon renvoit (#f)

(define cont_1nm (lambda(exp)
                  (cond
                    ((exp_atom? exp) (cond
                                      ((eqv? (param2 (operateur exp)) '*) (cond
                                                                            ((and (number?  (param1 (operateur exp))) (not (number?  (param3 (operateur exp))))) (list #t (param1 (operateur exp)) (param3 (operateur exp))))
                                                                            ((and (number?  (param3 (operateur exp))) (not (number?  (param1 (operateur exp))))) (list #t (param3 (operateur exp)) (param1 (operateur exp))))
                                                                            (#t (list #f))
                                                                            ))
                                      (#t (list #f))
                                      ))
                    (#t (list #f))
                    ))
  )




;definition de la fonction permettant de determiner si deux expressions additives comporte au moins un nombre chacune
;si oui, renvoit (#t somme_des_nombres liste_des_atomes)
;sinon renvoit (#f)

(define cont_2n (lambda(op1 op2 )
                  (cond
                    ((and (param1 (cont_1n op1)) (param1 (cont_1n op2))) ( list #t (addition (param2 (cont_1n op1)) (param2 (cont_1n op2))) (simpli (list (param3 (cont_1n op1)) '+ (param3 (cont_1n op2))))))
                    (#t (list #f))
                    ))
  )




;definition de la fonction permettant de determiner si une expression est de la forme "(5 * x) * (6 * y)"

(define cont_2nm (lambda(op1 op2 )
                  (cond
                    ((and (param1 (cont_1nm op1)) (param1 (cont_1nm op2))) ( list #t (param2 (cont_1nm op1)) (param2 (cont_1nm op2)) (param3 (cont_1nm op1)) (param3 (cont_1nm op2))))
                    (#t (list #f))
                    ))
  )



;definition de la fonction qui effectue l'opération en fonction de l'opérateur

(define operation (lambda(exp)
                  (cond 
                    ((eqv? '+ (param2 (operateur exp))) (addition  (simp_parenth (param1 (operateur exp))) (simp_parenth (param3 (operateur exp)))))
                    ((eqv? '* (param2 (operateur exp))) (multip  (simp_parenth (param1 (operateur exp))) (simp_parenth (param3 (operateur exp)))))
                    ))
  )
  


;definition de la fonction permettant d'effectuer l'operation d'addition

(define addition (lambda (op1 op2)
                   (cond
                     ((and (number? op1) (number? op2)) (+ op1 op2))
                     ((eq? op1 0) op2)
                     ((eq? op2 0) op1)
                     ((and (eq? op1 op2) (id? op1)) (list 2 '* op1))
                     
                     ((and (number? op1) (param1 (cont_1n op2))) (simpli (list (addition op1  (param2 (cont_1n op2))) '+ (param3 (cont_1n op2)))))
                     ((and (number? op2) (param1 (cont_1n op1))) (simpli (list (addition op2  (param2 (cont_1n op1))) '+ (param3 (cont_1n op1)))))
                     
                     ((and (id? op1) (param1 (cont_1n op2)) (exp_atom? op2)) (cond
                                                               ((eq? op1 (param3 (cont_1n op2))) (list (param2 (cont_1n op2)) '+ (list 2 '* op1)))
                                                               (#t (list op1 '+ op2))
                                                               ))
                     ((and (id? op1) (param1 (cont_1nm op2)) (exp_atom? op2)) (cond
                                                               ((eq? op1 (param3 (cont_1nm op2))) (list (+ 1 (param2 (cont_1nm op2))) '* op1))
                                                               (#t (list op1 '+ op2))
                                                               ))
                                                               

                     ((and (id? op2) (param1 (cont_1n op1)) (exp_atom? op1)) (cond
                                                               ((eq? op2 (param3 (cont_1n op1))) (list (param2 (cont_1n op1)) '+ (list 2 '* op2)))
                                                               (#t  (list op1 '+ op2))
                                                               ))
                     ((and (id? op2) (param1 (cont_1nm op1)) (exp_atom? op1)) (cond
                                                               ((eq? op2 (param3 (cont_1nm op1))) (list (+ 1 (param2 (cont_1nm op1))) '* op2))
                                                               (#t (list op1 '+ op2))
                                                               ))

  
                                                                             
                     ((and (list? op1) (list? op2) (param1 (cont_2n op1 op2))) (simpli (list (addition (param2 (cont_2n op1 op2)) (param3 (cont_2n op1 op2))))))
                     
                     ((or (not (equal? op1 (simpli op1))) (not (equal? op2 (simpli op2)))) (simpli (addition  (simpli op1) (simpli op2))))
                     ((and (not (list? op1)) (exp_atom? op2)) (list op1 '+ op2))
                     ((and (not (list? op2)) (exp_atom? op1)) (list op1 '+ op2))
                     
                     (#t (list op1 '+ op2))
                     ))
  )

;definition de la fonction permettant d'effectuer l'operation de multiplication

(define multip (lambda (op1 op2)
                   (cond
                     ((and (number? op1) (number? op2)) (* op1 op2))
                     ((eq? op1 0) 0)                                         
                     ((eq? op2 0) 0)
                     ((eq? op1 1) (simpli op1))                                         
                     ((eq? op2 1) (simpli op2))
                     ((and (number? op1) (param1 (cont_1nm op2))) (list (multip op1  (param2 (cont_1nm op2))) '* (param3 (cont_1nm op2))))
                     ((and (number? op2) (param1 (cont_1nm op1))) (list (multip op2  (param2 (cont_1nm op1))) '* (param3 (cont_1nm op1))))

                     ((and (number? op1) (param1 (cont_1n op2))) (simpli (list (multip op1  (param2 (cont_1n op2))) '+ (list (param3 (cont_1n op2)) '* op1))))                    
                     ((and (number? op2) (param1 (cont_1n op1))) (simpli (list (multip op2  (param2 (cont_1n op1))) '+ (list (param3 (cont_1n op1)) '* op2))))
                     
                     ((and (exp_atom? op1) (exp_atom? op2) (param1 (cont_2nm op1 op2))) (list (multip (param2 (cont_2nm op1 op2)) (param3 (cont_2nm op1 op2))) '* (list (param4 (cont_2nm op1 op2)) '* (param5 (cont_2nm op1 op2)))))
                     ((or (not (equal? op1 (simpli op1))) (not (equal? op2 (simpli op2)))) (simpli (multip (simpli op1) (simpli op2))))
                     ((and (not (list? op1)) (exp_atom? op2)) (list op1 '* op2))
                     ((and (not (list? op2)) (exp_atom? op1)) (list op1 '* op2))
                     (#t (list op1 '* op2))
                     ))
  )



;definition de la fonction de simplification
(define simpli (lambda (exp) 
                 (cond
                   ((number? (simp_parenth exp)) (simp_parenth exp))
                   ((id? (simp_parenth exp)) (simp_parenth exp))
                   ((list? (simp_parenth exp)) (cond
                                                     ((eqv? (param2 (operateur (simp_parenth exp))) '+) (cond
                                                                                                          ((eqv? (param1 (operateur (simp_parenth exp))) 0) (param3 (operateur (simp_parenth exp))))
                                                                                                          ((eqv? (param3 (operateur (simp_parenth exp))) 0) (param1 (operateur (simp_parenth exp))))
                                                                                                          (#t (operation (simp_parenth exp)))
                                                                                                          ))

                                                     
                                                     
                                                     ((eqv? (param2 (operateur (simp_parenth exp))) '*) (cond
                                                                                                          ((or (eqv? (param1 (operateur (simp_parenth exp))) 0) (eqv? (param3 (operateur (simp_parenth exp))) 0)) 0)
                                                                                                          ((eqv? (param1 (operateur (simp_parenth exp))) 1) (param3 (operateur (simp_parenth exp))))
                                                                                                          ((eqv? (param3 (operateur (simp_parenth exp))) 1) (param1 (operateur (simp_parenth exp))))
                                                                                                          (#t (operation (simp_parenth exp)))
                                                                                                          
                                                                                                          ))
                                                     )
                                                   )
                                                     
                                                     
                   (#t (operation (simp_parenth exp)))
                                                 ))
  )