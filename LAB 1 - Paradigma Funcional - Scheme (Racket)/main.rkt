#lang racket
(require "TDA_elementsList.rkt")
(require "TDA_cardsSet.rkt")
(require "TDA_gamersInfo.rkt")
(require "TDA_game.rkt")
(require (only-in math/number-theory prime?))

(define el (createElementsAndList "A" "B" "C" "D" "E" "F" "G"))

(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 ))

(define cardsSet (lambda (elements numE maxC rndFn)
                   (let ([nCards (+ (- (* numE numE) numE) 1)] [n (- numE 1)])
                     (if (prime? n)
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
               (list (initGamersInfo numPlayers) cardsSet "En juego" mode rndFn)))

(define register (lambda (user gm)
                   (let ([gsInfo (getGamersInfo gm)] [cS (getCardsSet gm)] [st (status gm)] [mode (getMode gm)] [rndFn (getRandomFn gm)])
                     (setGame (newGamer user gsInfo) cS st mode rndFn))))

(define whoseTurnIsIt? (lambda (gm) (getGamerTurn (getGamersInfo gm))))

(define status (lambda (gm) (caddr gm)))

(define score (lambda (gm user) (getGamerScore (getGamersInfo gm) user)))
