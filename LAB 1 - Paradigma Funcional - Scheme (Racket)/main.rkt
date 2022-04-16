#lang racket
(require "TDA_elementsList.rkt")
(require "TDA_cardsSet.rkt")
(require "TDA_gamersInfo.rkt")
(require "TDA_gameArea.rkt")
(require "TDA_game.rkt")
(require "helperFunctions.rkt")
(require (only-in math/number-theory prime?))

#|
Operacion: Constructor
Descripcion: Agrega un puntaje dado al gamersPoints en la posicion n
Dominio: elementsList X int(numero de elementos) X int (maxima cantidad de cartas) X rndnFn (funcion de aleatorizacion)
Recorrido: cardsSet
Recursion: de Cola
|#                   
(define cardsSet (lambda (elements numE maxC rndFn)
                   (let ([nCards (+ (- (* numE numE) numE) 1)] [n (- numE 1)] [randomN (maxN (rndFn (* numE numE maxC 27)) (* numE (+ (- (* numE numE) numE) 1)))])
                     (if (prime? n)
                         (if (<= maxC 0)
                             (cardsSet elements numE nCards rndFn)
                             (if (< (elementsListLenght elements) nCards)
                                 (let ([newElements (insertXElements elements (- nCards (elementsListLenght elements)))])
                                  (mixCardsSetXtimes (insertCard (firstCardGeneration newElements n) (unionCardsSet (nCardGeneration newElements n (- maxC 1)) (n2CardGeneration newElements n (- maxC numE)))) randomN))
                                 (mixCardsSetXtimes (insertCard (firstCardGeneration elements n) (unionCardsSet (nCardGeneration elements n (- maxC 1)) (n2CardGeneration elements n (- maxC numE)))) randomN)))
                         emptyCardsSet))))

#|
Operacion: Pertenencia
Descripcion: verifica si un conjunto de cartas, es un conjunto valido del juego dobble
Dominio: CardsSet
Recorrido: boolean
Recursion: de Cola
|#
(define dobble? (lambda (cS)
                  (cond
                    [(emptyCardsSet? cS) #f]
                    [(not (prime? (- (elementsListLenght (firstCard cS)) 1))) #f]
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

#|
Operacion: otro
Descripcion: calcula la cantidad de cartas que tiene un cardsSet
Dominio: CardsSet
Recorrido: int
|#
(define numCards (lambda (cS)
                   (apply + (map (lambda (x) 1) cS))))                   

#|
Operacion: Selector
Descripcion: Obtiene la nth carta de un conjunto de cartas
Dominio: CardsSet X int
Recorrido: elementsList (card)
|#
(define nthCard (lambda (cS n)
                  (if (= n 0)
                         (firstCard cS)
                         (nthCard (nextCards cS) (- n 1)))))

#|
Operacion: Otro
Descripcion: calcula la cantidad total de cartas que se deben producir para construir un conjunto completo de cartas
Dominio: elementsList (card)
Recorrido: int
|#
(define findTotalCards (lambda (card)
                         (define totalCards (lambda (numE) (+ (- (* numE numE) numE) 1)))
                         (totalCards (elementsListLenght card))))

#|
Operacion: Otro
Descripcion: calcula la cantidad total de elementos que se necesitan para construir un conjunto valido de cartas
Dominio: elementsList (card)
Recorrido: int
|#
(define requiredElements (lambda (card)
                         (define totalCards (lambda (numE) (+ (- (* numE numE) numE) 1)))
                         (totalCards (elementsListLenght card))))

#|
Operacion: Otro
Descripcion: Busca las cartas que faltan para que un conjunto de cartas sea completo
Dominio: cardsSet
Recorrido: cardsSet
Recursion: de Cola
Funcion con uso de backtracking
|#
(define missingCards (lambda (cS)
                       (if (dobble? cS)
                           (let ([numE (elementsListLenght (firstCard cS))] [elements (elementsCardsSet cS)] )
                             (let ([elements (insertXElements elements (- (+ (- (* numE numE) numE) 1) (elementsListLenght elements)))] [nCards (+ (- (* numE numE) numE) 1)])
                               (define recursion (lambda (cS R)
                                                   (define missingCard (lambda (eL numAeL card)
                                                      (if  (= (elementsListLenght card) numE)
                                                          card
                                                          (if (emptyElementsList? eL)
                                                              emptyElementsList
                                                              (if (and (< (firstElement numAeL) numE) (andmap (lambda (x) (if (<= x 1) #t #f)) (map (commonElements (insertElement (firstElement eL) card)) cS)))
                                                                  (let ([missedCard (missingCard (nextElements eL) (nextElements numAeL) (insertElement (firstElement eL) card))])
                                                                    (if (emptyElementsList? missedCard)   ;Condicion para realizar el backtracking
                                                                        (missingCard (nextElements eL) (nextElements numAeL) card)
                                                                        missedCard))
                                                                  (missingCard (nextElements eL) (nextElements numAeL) card))))))
                                                   (define numAppearencesElements (lambda (eL cS) (map (lambda (e) (numCards (filter (lambda (card) (isElementList? card e)) cS))) eL)))
                                                   (if (= (numCards cS) nCards)
                                                       R
                                                       (let ([missedCard (missingCard elements (numAppearencesElements elements cS) emptyElementsList)])
                                                         (recursion (insertCard missedCard cS) (insertCard missedCard R))))))
                               (recursion cS emptyCardsSet)))
                           emptyCardsSet)))

#|
Operacion: Otro
Descripcion: convierte un conjunto de cartas a una representacion en strings
Dominio: cardsSet
Recorrido: string
Recursion: de Cola
|#
(define cardsSet->string (lambda (cS)
                           (define recursion (lambda (cS i string)
                                              (if (emptyCardsSet? cS)
                                                  string
                                                  (recursion (nextCards cS) (+ i 1) (string-append* string "\n" "card n°" (number->string i) ": " (nextCards (append* (map (lambda (x) (list ", " x)) (firstCard cS)))))))))
                           (recursion cS 1 "")))

#|
Operacion: Constructor
Descripcion: Crea un game (tablero de juego con: gamersInfo, gameArea, estatus, modo de juego, funcion de aleatorizacion)
Dominio: int (numero de jugadores) X cardsSet X function (modo de juego) X function (funcion de aleatorizacion)
Recorrido: game
|#
(define game(lambda (numPlayers cardsSet mode rndFn)
               (list (initGamersInfo numPlayers) (setCardsSet emptyGameArea (mixCardsSetXtimes cardsSet (* numPlayers (quotient (findTotalCards (firstCard cardsSet)) (numCards cardsSet))))) "esperando cartas en mesa" mode rndFn)))

#|
Operacion: Otro
Descripcion: Retira las dos primeras cartas de un conjunto de cartas
Dominio: cardsSet
Recorrido: cardsSet
|#
(define stackMode (lambda (cS)
                    (if (or (emptyCardsSet? cS) (emptyCardsSet? (nextCards cS)))
                        emptyCardsSet
                        (insertCard (firstCard (nextCards cS)) (insertCard (firstCard cS) emptyCardsSet)))))

#|
Operacion: Modificador
Descripcion: registra un jugador en el juego
Dominio: string (jugador) X game
Recorrido: game
|#
(define register (lambda (user gm)
                   (let ([gsInfo (getGamersInfo gm)] [gA (getGameArea gm)] [st (status gm)] [mode (getMode gm)] [rndFn (getRandomFn gm)])
                     (setGame (newGamer user gsInfo) gA st mode rndFn))))

#|
Operacion: Selector
Descripcion: Devuelve el usuario al que le corresponde jugar el turno
Dominio: game
Recorrido: string
|#
(define whoseTurnIsIt? (lambda (gm) (getGamerTurn (getGamersInfo gm))))

#|
Operacion: Otro
Descripcion: permite realizar una jugada a partir de una funcion de juego, la cual en el caso de ser nula esta invoca al modo de juego
Dominio: game X function (accion de juego)
Recorrido: game
|#
(define play (lambda (gm action)
               (let ([gsInfo (getGamersInfo gm)])
                 (if (or (string=? (status gm) "terminado") (= (totalRegisteredGamers gsInfo) 0))
                     gm
                     (if (null? action)
                         (let ([gA (getGameArea gm)] [mode (getMode gm)] [rndFn (getRandomFn gm)])
                           (let ([cSP (mode (getCardsSet gA))])
                             (if (emptyCardsSet? cSP)
                                 (setGame gsInfo (setCardsInPlay gA cSP) "terminado" mode rndFn)
                                 (setGame gsInfo (setCardsInPlay gA cSP) "cartas en mesa" mode rndFn))))
                         (action gm))))))

#|
Action para la funcion Play
Para los 3 modos de juego disponibles
Operacion: Modificador
Descripcion: Esta funcion recibe un elemento a comparar con las cartas en la mesa de un game, cambiando el estado de juego a espoteado o no espotado
Dominio: element X game
Recorrido: game
Funcion currificada
|#
(define spotIt (lambda (e) (lambda (gm)
                             (if (or (string=? (status gm) "terminado") (not (string=? (status gm) "cartas en mesa")))
                                 gm
                                 (let ([gsInfo (getGamersInfo gm)] [gA (getGameArea gm)] [mode (getMode gm)] [rndFn (getRandomFn gm)])
                                   (if (<= 2 (numCards (filter (lambda (elem) (isElementList? elem e)) (getCardsInPlay gA))))
                                       (setGame gsInfo gA "spotIt" mode rndFn)
                                       (setGame gsInfo gA "notSpotIt" mode rndFn)))))))

#|
Action para la funcion Play
Operacion: Modificador
Descripcion: pasa de turno y asigna el puntaje (que representa la cantidad de cartas ganadas o perdidas dependiendo del modo) al jugador que tiene el turno activo
Dominio: game
Recorrido: game
|#
(define pass (lambda (gm)
               (if (or (string=? (status gm) "terminado") (string=? (status gm) "esperando cartas en mesa"))
                   gm
                   (let ([gsInfo (getGamersInfo gm)] [gA (getGameArea gm)] [st (status gm)] [mode (getMode gm)] [rndFn (getRandomFn gm)])
                     (let ([cS (cardsSetSubstraction (getCardsSet gA) (getCardsInPlay gA))])
                       (if (string=? st "spotIt")
                           (setGame (nextTurn (addScore gsInfo (numCards (getCardsInPlay gA)))) (setCardsSet (setCardsInPlay gA emptyCardsSet) cS) "esperando cartas en mesa" mode rndFn)
                           (setGame (nextTurn gsInfo) (setCardsSet (setCardsInPlay gA emptyCardsSet) (unionCardsSet (reverseCardsSet cS) (getCardsInPlay gA))) "esperando cartas en mesa" mode rndFn)))))))

#|
Action para la funcion Play
Para todos los modos de juego disponibles
Operacion: Modificador
Descripcion: cambia el estado del juego a terminado
Dominio: game
Recorrido: game
|#
(define finish (lambda (gm)
                 (let ([gsInfo (getGamersInfo gm)] [gA (getGameArea gm)] [st (status gm)] [mode (getMode gm)] [rndFn (getRandomFn gm)])
                   (setGame gsInfo gA "terminado" mode rndFn))))

#|
Operacion: Selector
Descripcion: Retorna el estado actual del juego
Dominio: game
Recorrido: string
|#
(define status (lambda (gm) (caddr gm)))

#|
Operacion: Selector
Descripcion: Retorna el puntaje de un jugador
Dominio: game X string (jugador)
Recorrido: int
|#
(define score (lambda (gm user) (getGamerScore (getGamersInfo gm) user)))

#|
Operacion: Otro
Descripcion: Convierte la informacion obtenible del juego en una representacion en strings
Dominio: game
Recorrido: string
|#
(define game->string (lambda (gm)
                       (let ([gsInfo (getGamersInfo gm)] [st (status gm)] [gA (getGameArea gm)])
                         (if (string=? "terminado" st)
                             (string-append* "Estatus del juego: " st "\n----" (winnersLosersString gsInfo) "\n----\n" (gamersInfo->string gsInfo) null)
                             (if (string=? "cartas en mesa" st)
                                 (string-append* "Estatus del juego: " st "\nCartas en juego: " (cardsSet->string (getCardsInPlay gA)) "\n" (gamersInfo->string gsInfo) null)
                                 (string-append* "Estatus del juego: " st "\n" (gamersInfo->string gsInfo) null))))))

#|
Operacion: modificador
Descripcion: agrega una carta al conjunto de cartas
Dominio: cardsSet X elementsList (card)
Recorrido: cardsSet
|#
(define addCard (lambda (cS card)
                  (if (dobble? (cons card cS))
                      (cons card cS)
                      cS)))

#|
Operacion: Otro
Descripcion: Retira la primera carta, y una aleatoria de la segunda mitad de un conjunto de cartas (esta segunda mitad representa el mazo de un jugador)
Dominio: cardsSet
Recorrido: cardsSet
|#
(define emptyHandsStackMode (lambda (cS)
                              (if (< (numCards cS) 2)
                                  emptyCardsSet
                                  (let ([nC (numCards cS)] [rE (requiredElements (firstCard cS))])
                                    (insertCard (firstCard cS) (insertCard (nthCard cS (minMaxN (maxN (randomFn (* nC rE 135)) (+ nC rE 14)) (quotient nC 2) (- nC 1 ) )) emptyCardsSet))))))

#|
Operacion: Otro
Descripcion: "Divide" el conjunto de cartas en 3 y retira una carta aleatoria entre cada particion, retornando este conjunto de 3 cartas
Dominio: cardsSet
Recorrido: cardsSet
|#
(define myMode (lambda (cS)
                  (let ([nC (numCards cS)])
                    (if (< nC 3)
                        emptyCardsSet
                        (let ([prt (quotient nC 3)])
                          (insertCard (nthCard  cS (minMaxN (maxN (randomFn (* nC prt 28)) (+ (* prt 22) 12)) 0 (- prt 1)))
                                               (insertCard (nthCard cS (minMaxN (maxN (randomFn (* nC prt 35)) (+ (* prt 7) 1)) prt (- (* prt 2) 1)))
                                                           (insertCard (nthCard cS (minMaxN (maxN (randomFn (* nC prt 35)) (+ (* prt 7) 1)) (* prt 2) (- (* prt 3) 1))) emptyCardsSet))))))))
