#lang racket
(require "TDA_elementsList.rkt")
(require "TDA_cardsSet.rkt")
(require "TDA_gamersInfo.rkt")
(require "TDA_gameArea.rkt")
(require "TDA_game.rkt")
(require (only-in math/number-theory prime-power?))

(define el (createElementsAndList "A" "B" "C" "D" "E" "F" "G"))

(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 ))

(define cardsSet (lambda (elements numE maxC rndFn)
                   (let ([nCards (+ (- (* numE numE) numE) 1)] [n (- numE 1)])
                     (if (prime-power? n)
                         (if (<= maxC 0)
                             (cardsSet elements numE nCards rndFn)
                             (if (< (elementsListLenght elements) nCards)
                                 (let ([newElements (insertXElements elements (- nCards (elementsListLenght elements)))])
                                   (insertCard (firstCardGeneration newElements n) (unionCardsSet (nCardGeneration newElements n (- maxC 1)) (n2CardGeneration newElements n (- maxC numE)))))
                                 (insertCard (firstCardGeneration elements n) (unionCardsSet (nCardGeneration elements n (- maxC 1)) (n2CardGeneration elements n (- maxC numE))))))
                         null))))
                   

(define dobble? (lambda (cS)
                  (cond
                    [(emptyCardsSet? cS) #f]
                    [(not (andmap elementsList? cS)) #f]
                    [(not (andmap (lambda (x) (= (elementsListLenght (firstCard cS)) x)) (map elementsListLenght cS))) #f]
                    [else
                     (define recursion (lambda (cS1 cS2)
                                             (define recursion2 (lambda (cS3)
                                                                  (cond
                                                                    [(emptyCardsSet? cS3) (recursion (nextElements cS1) (nextElements cS2))]
                                                                    [(not (oneCommonElement? (firstElement cS1) (firstElement cS3))) #f]
                                                                    [else (recursion2 (nextElements cS3))])))
                                             (if (emptyCardsSet? cS2)
                                                 #t
                                                 (recursion2 cS2))))
                         (recursion cS (nextElements cS))])))

(define numCards (lambda (cS)
                   (apply + (map (lambda (x) 1) cS))))                   

(define nthCard (lambda (cS n)
                  (if (= n 0)
                         (firstCard cS)
                         (nthCard (nextCards cS) (- n 1)))))

(define findTotalCards (lambda (card)
                         (define totalCards (lambda (numE) (+ (- (* numE numE) numE) 1)))
                         (totalCards (elementsListLenght card))))

(define requiredElements (lambda (card)
                         (define totalCards (lambda (numE) (+ (- (* numE numE) numE) 1)))
                         (totalCards (elementsListLenght card))))

(define missingCards (lambda (cS)
                       (if (dobble? cS)
                           (let ([numE (elementsListLenght (firstCard cS))] [elements (elementsCardsSet cS)] )
                             (let ([elements (insertXElements elements (- (+ (- (* numE numE) numE) 1) (elementsListLenght elements)))] [nCards (+ (- (* numE numE) numE) 1)])
                               (define recursion (lambda (cS R)
                                                   (define missingCard (lambda (eL numAeL card)
                                                      (if  (= (elementsListLenght card) numE)
                                                          card
                                                          (if (emptyElementsList? eL)
                                                              null
                                                              (if (and (< (firstElement numAeL) numE) (andmap (lambda (x) (if (<= x 1) #t #f)) (map (commonElements (insertElement (firstElement eL) card)) cS)))
                                                                  (let ([missedCard (missingCard (nextElements eL) (nextElements numAeL) (insertElement (firstElement eL) card))])
                                                                    (if (null? missedCard)
                                                                        (missingCard (nextElements eL) (nextElements numAeL) card)
                                                                        missedCard))
                                                                    (missingCard (nextElements eL) (nextElements numAeL) card))))))
                                                   (define numAppearencesElements (lambda (eL cS) (map (lambda (e) (numCards (filter (lambda (card) (isElementList? card e)) cS))) eL)))
                                                   (if (= (numCards cS) nCards)
                                                       R
                                                       (let ([missedCard (missingCard elements (numAppearencesElements elements cS) null)])
                                                         (recursion (insertCard missedCard cS) (insertCard missedCard R))))))
                               (recursion cS null)))
                           null)))

(define cardsSet->string (lambda (cS)
                           (define recursion (lambda (cS i string)
                                              (if (emptyCardsSet? cS)
                                                  string
                                                  (recursion (nextCards cS) (+ i 1) (string-append* string "\n" "card n°" (number->string i) ": " (nextCards (append* (map (lambda (x) (list ", " x)) (firstCard cS)))))))))
                           (recursion cS 1 "")))

(define game(lambda (numPlayers cardsSet mode rndFn)
               (list (initGamersInfo numPlayers) (setCardsSet emptyGameArea cardsSet) "esperando cartas en mesa" mode rndFn)))

(define stackMode (lambda (cS)
                    (if (or (emptyCardsSet? cS) (emptyCardsSet? (nextCards cS)) )
                        emptyCardsSet
                        (insertCard (firstCard (nextCards cS)) (insertCard (firstCard cS) emptyCardsSet)))))

(define register (lambda (user gm)
                   (let ([gsInfo (getGamersInfo gm)] [gA (getGameArea gm)] [st (status gm)] [mode (getMode gm)] [rndFn (getRandomFn gm)])
                     (setGame (newGamer user gsInfo) gA st mode rndFn))))

(define whoseTurnIsIt? (lambda (gm) (getGamerTurn (getGamersInfo gm))))

(define play (lambda (gm action)
               (if (string=? (status gm) "terminado")
                   gm
                   (if (null? action)
                       (let ([gsInfo (getGamersInfo gm)] [gA (getGameArea gm)] [mode (getMode gm)] [rndFn (getRandomFn gm)])
                         (let ([cSP (mode (getCardsSet gA))])
                           (if (null? cSP)
                               (setGame gsInfo (setCardsInPlay gA cSP) "terminado" mode rndFn)
                               (setGame gsInfo (setCardsInPlay gA cSP) "cartas en mesa" mode rndFn))))
                       (action gm)))))

(define spotIt (lambda (e) (lambda (gm)
                             (if (or (string=? (status gm) "terminado") (not (string=? (status gm) "cartas en mesa")))
                                 gm
                                 (let ([gsInfo (getGamersInfo gm)] [gA (getGameArea gm)] [mode (getMode gm)] [rndFn (getRandomFn gm)])
                                   (if (and (isElementList? (firstCard (getCardsInPlay gA)) e) (isElementList? (firstCard (nextCards (getCardsInPlay gA))) e))
                                       (setGame gsInfo gA "spotIt" mode rndFn)
                                       (setGame gsInfo gA "notSpotIt" mode rndFn)))))))

(define pass (lambda (gm)
               (if (or (string=? (status gm) "terminado") (string=? (status gm) "esperando cartas en mesa"))
                   gm
                   (let ([gsInfo (getGamersInfo gm)] [gA (getGameArea gm)] [st (status gm)] [mode (getMode gm)] [rndFn (getRandomFn gm)])
                     (let ([cS (cardsSetSubstraction (getCardsSet gA) (getCardsInPlay gA))])
                       (if (string=? st "spotIt")
                           (setGame (nextTurn (addScore gsInfo)) (setCardsSet (setCardsInPlay gA null) cS) "esperando cartas en mesa" mode rndFn)
                           (setGame (nextTurn gsInfo) (setCardsSet (setCardsInPlay gA null) (unionCardsSet (reverseCardsSet cS) (getCardsInPlay gA))) "esperando cartas en mesa" mode rndFn)))))))

(define status (lambda (gm) (caddr gm)))

(define score (lambda (gm user) (getGamerScore (getGamersInfo gm) user)))
