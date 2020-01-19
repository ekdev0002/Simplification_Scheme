;-------------------------Travaux pratiques de simplification-----------------------------------
;                          KABORE Yabyouré Eric
;---------------------------------IAI ING1 2017------------------------------------------------- 


#lang racket/gui
(require racket/draw)
(require racket/gui)
(require racket/format)
(require racket/draw)
(require "Application.rkt") 


;definition des polices
(define police1(make-object font% 13 'script 'normal 'bold #f 'smoothed))
(define police4(make-object font% 13 'script 'italic 'bold #f))
(define police2(make-object font% 12 'system 'normal 'normal #f))
(define police3(make-object font% 13 'roman 'italic 'bold #f))


;fenetre principale
(define win (new frame% 
                 (label "Simplification Arithmetique")
                 (width 600)
                 (height 200) 
                 (x 450) 
                 (y 250)
                 )
  )


;création de la fenêtre d'accueil
(define fenetre_accueil (new frame%
                    [label "ACCUEIL"]
                    [width 615]
                    [height 415]
                    [x 400]
                    [y 100]
                    [alignment '(center center)]
                    [style '(no-caption no-resize-border)]                                       
                    )
  )

 

;definition de l' image d'accueil
(define texte_accueil(make-object bitmap% "image76.BMP"))
(define img_quitter(make-object bitmap% "quitter.PNG"))
(define img-continuer(make-object bitmap% "cont.BMP"))


;definition du panel de l'image d'accueil
(define panel-msg-acces1(new horizontal-panel%
                  [parent fenetre_accueil]
                  [min-height 400]
                  [alignment '(center bottom)]
                  )
  )



;definition des panels fenetre accueil
(define panel-msg-acces(new horizontal-panel%
                  [parent fenetre_accueil]
                  [alignment '(center bottom)]
                  )
  )

;définition du conteneur du texte d'accueil
(define canevas (instantiate canvas%(panel-msg-acces1)
                  (paint-callback (lambda(canevas dc)(logo_fond_ecran dc))))
  )


;définition de l'afficheur du texte d'accueil
(define (logo_fond_ecran dc) (send dc draw-bitmap texte_accueil 1 1 'solid))



;lancement de la fentre d'accueil
(send fenetre_accueil  show #t)


;definition de la fenetre "quitter"
(define fenetre_quitter (new frame%
                            [label "CONFIRMATION"]                               
                            [width 300]   
                            [height 100]   
                            [x 550]   
                            [y 250]                                      
                            )
  )


;definition des panels pour la fenetre quitter
(define panneau-message (new horizontal-panel%   
                            [parent fenetre_quitter]                             
                            [vert-margin 10]
                            [horiz-margin 10]
                            [alignment '(center top)]   
                            )
  ) 

(define panneau-boutons-quitter (new horizontal-panel%   
                                    [parent fenetre_quitter] 
                                    [vert-margin 10]
                                    [horiz-margin 10]
                                    [alignment '(center bottom)]
                                    )
  )




;bouton et action de "Oui"
(define evt-oui (lambda (o e)
                  (send fenetre_quitter on-exit)
                  (send win on-exit)
                  (send fenetre_accueil on-exit)
                  )
  )

(define bouton-ok (new button%   
                      [label "Oui"]
                      [font police3]
                      [parent panneau-boutons-quitter]                         
                      [callback evt-oui]                            
                      )
  )

;bouton et action de "Non"
(define evt-non (lambda (o e)
                  (send fenetre_quitter on-exit)
                  )
  )

(define bouton-non (new button%   
                       [label "Non"] 
                       [font police3]
                       [parent panneau-boutons-quitter]   
                       [callback evt-non]                            
                       )
  )


;message de confirmation "quitter"
(define le-message (new message%  
                       [label "Etes-vous sûr de vouloir quitter?"]
                       [font police3]
                       [parent panneau-message]                             
                            )
  )

;defnition des evenement lancer et quitter le programe
(define evt-quitter(lambda (o e)
  (send fenetre_quitter show #t)
                     )
  )


(define evt-go (lambda (o e)
                 (send fenetre_accueil on-exit)
                 (send win show #t))
  )


;definition de l'evenement retour à la page d'accuil
(define evt-retour(lambda (o e)
  (send win on-exit)
  (send fenetre_accueil show #t)
                    )
  )



;bouton de "quitter" fenetre accueil

(define quitter1(new button%
                    [label img_quitter]
                    [parent panel-msg-acces]
                    [callback evt-quitter]                    
                    )
  )


;bouton permetant d'accéder au menu prinicipal

(define bouton-go (new button%
                      [label img-continuer]
                      [parent panel-msg-acces]
                      [callback evt-go]                     
                      )
  )





;definition de la fenetre "Aide"
(define fenetre_aide (new frame%
                    [label "AIDE"]
                    [width 640]
                    [height 500]
                    (x 450) 
                    (y 50)
                    [alignment '(center center)]
                    )
  )

;definition de l'evement retour aide
(define evt-retour-aide(lambda (o e)
  (send fenetre_aide on-exit)
                         )
  )


;definition paneau-aide
(define panel-aide (new horizontal-panel%
                  [parent fenetre_aide]
                  [min-height 0]
                  [alignment '(center top)]
                  ))
(define panel-aide1 (new horizontal-panel%
                  [parent fenetre_aide]
                  [min-height 50]
                  [alignment '(center center)]
                  ))
(define panneau-bouton-retour (new vertical-panel%   
                                    [parent fenetre_aide] 
                                    [vert-margin 10]
                                    [horiz-margin 10]
                                    [alignment '(center bottom)]
                                    )
  )


;Definitions du contenu de l'aide

(define canevas1 (new canvas% 
                      [parent panel-aide]
                      [style '(transparent)]
                      [paint-callback
                      (lambda (canvas dc)
                        (send dc set-scale 1 1)
                        (send dc set-text-foreground "blue")
                        (send dc draw-text "Bienvenue sur notre programme Simplification arithmetique" 100 0)
                        (send dc draw-text "Ce programme simplifie pour vous une expression arithmetique et ce qu'elle que soit" 0 60)
                        (send dc draw-text "sa forme:" 0 80)
                        (send dc draw-text "          forme prefixée, forme infixée, forme suffixée ou même un melange des trois." 0 100))
                        
                      
                      ]
                      )
  )

(define canevas2 (new canvas% 
                      [parent panel-aide1]
                      [style '(transparent)]
                      [paint-callback
                      (lambda (canvas dc)
                        (send dc set-scale 1 1)
                        (send dc set-text-foreground "black")
                        (send dc draw-text "Pour commencer : " 05 40)
                        (send dc draw-text "Etape 1 : Veuillez saisir votre expression \"entre parenthèse\" en espacant les chiffres" 30 60)
                        (send dc draw-text "Etape 2 : Ensuite cliquer sur \"simplifier\" pour afficher le resultat " 30 100)
                        (send dc draw-text "Etape 3 : Cliquez sur \"effacer\" pour initialiser les champs et recommencer" 30 140))]
                      )
  )

(define canevas3 (new canvas% 
                      [parent  panneau-bouton-retour]
                      [style '(transparent)]
                      [paint-callback
                      (lambda (canvas dc)
                        (send dc set-scale 1 1)
                        (send dc set-text-foreground "orange")
                        (send dc draw-text "Facile ? Amusez vous bien !!!" 160 20))]
                      )
  )


;definition du retour-aide
(define retour_aide(new button%
                    [label "Cliquez ici pour retourner"]
                    [font police2]
                    [parent panneau-bouton-retour]
                    [callback evt-retour-aide]                    
                    )
  )


(define panel (new horizontal-panel%
                    [parent win]             
                    [alignment '(center top)]
                    [horiz-margin 0]
                    [vert-margin 0]
                    )
  )



(define panel1 (new vertical-panel%
                    [parent panel]             
                    [alignment '(center top)]
                    [horiz-margin 0]
                    [vert-margin 0]
                    )
  )

(define panel2 (new vertical-panel%
                    [parent panel]             
                    [alignment '(center top)]
                    [min-width 400]	 
                    )
  )


(define panel1_1 (new vertical-panel%
                    [parent panel1]             
                    [alignment '(center top)]
                    [horiz-margin 0]
                    [vert-margin 0]
                    )
  )

(define panel1_2 (new vertical-panel%
                    [parent panel1]             
                    [alignment '(center top)]
                    )
  )


(define panel2_1 (new vertical-panel%
                    [parent panel2]             
                    [alignment '(center top)]
                    [horiz-margin 0]
                    [vert-margin 0]
                    ))

(define panel2_2 (new vertical-panel%
                    [parent panel2]             
                    [alignment '(center top)]
                    )
  )


(new message%	 
   	 	[label "Expression"]	 
   	 	[parent panel1_1]
                [horiz-margin 0]
                [vert-margin 22]
                )

(new message%	 
   	 	[label "Résultat"]	 
   	 	[parent panel1_2]
                [horiz-margin 0]
                [vert-margin 0]
                )



;definition champs de text pour recueillir l'expression
(define express (new text-field% 
                     (label "") 
                     (parent panel2_1) 
                     (vert-margin 30)
                     (horiz-margin 0)                 
                     )
  )

(define panel_bouton1 (new horizontal-panel%
                    [parent panel2_1]             
                    [alignment '(center top)]
                    )
  )

;definition du bouton effacer
(define bouton (new button% (label "Effacer")
                    (parent panel_bouton1)
                    [callback (lambda (button event)
                                (send resul set-value "")
                                (send express set-value "")                                                                                                           
                                      )]
                    )
  )

;definition du bouton simplifier
(define bouton2 (new button% (label "Simplifier")
                    (parent panel_bouton1)
                    [callback (lambda (button event)
                                (send resul set-value
                                      (~a (simpli (read (open-input-string (send express get-value)))))                                      
                                      ))]
                    )
  )
           


;definition champs de text pour afficher le resultat

(define resul (new text-field% 
                   (label "")
                   (parent panel2_2)
                   (vert-margin 30) 
                   (horiz-margin 0) 
                   (enabled #f)))

(define panel_bouton2 (new horizontal-panel%
                    [parent panel2_2]             
                    [alignment '(center top)]
                    [min-height 50]                   
                    )
  )

;definition du bouton d'aide
(define bouton3 (new button% (label "Aide")
                     (parent panel_bouton2)
                     (horiz-margin 20) 
                     [callback (lambda (button event)
                                (send fenetre_aide show #t)
                                 )]
                     )
  )


;definition du bouton retour
(define bouton4 (new button% (label "Retour")
                     (horiz-margin 20)
                     (parent panel_bouton2)
                     [callback evt-retour]
                    )
  )

;definition du bouton quitter
(define bouton5 (new button% (label "Quitter")
                    (parent panel_bouton2)
                    (horiz-margin 20) 
                    [callback evt-quitter]
                    )
  )