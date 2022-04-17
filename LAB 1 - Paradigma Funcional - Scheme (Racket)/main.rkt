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
                   (if (or (> n (numCards cS)) (< n 0))
                       emptyElementsList
                       (firstCard (filter (lambda (card) (= (index-of cS card) n))  cS)))))

#|
Operacion: Otro
Descripcion: calcula la cantidad total de cartas que se deben producir para construir un conjunto completo de cartas
Dominio: elementsList (card)
Recorrido: int
|#
(define findTotalCards (lambda (card)
                         (define totalCards (lambda (numE) (+ (- (* numE numE) numE) 1)))
                         (let ([numElements (elementsListLenght card)])
                           (if (or (= numElements 0) (not (prime? (- numElements 1))))
                               0
                               (totalCards (elementsListLenght card))))))

#|
Operacion: Otro
Descripcion: calcula la cantidad total de elementos que se necesitan para construir un conjunto valido de cartas
Dominio: elementsList (card)
Recorrido: int
|#
(define requiredElements (lambda (card)
                         (define totalElements (lambda (numE) (+ (- (* numE numE) numE) 1)))
                         (let ([numElements (elementsListLenght card)])
                           (if (or (= numElements 0) (not (prime? (- numElements 1))))
                               0
                               (totalElements (elementsListLenght card))))))

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


;Ejemplos de uso de las funciones:


;cardsSet
(define el (createElementsAndList "A" "B" "C" "D" "E" "F" "G")) ;Lista de elementos
(define cardsSet1 (cardsSet el 3 -1 randomFn)) ;Se prueba con 3 elementos por carta y el maximo de cartas que se pueden generar
(define cardsSet2 (cardsSet el 5 -1 randomFn)) ;Se prueba con un numero no primo (- 5 1) = 4, lo que genera un cardsSet vacio
(define cardsSet3 (cardsSet el 8 34 randomFn)) ;Se prueba con 8 elementos y un maximo de cartas establecido en 34, en este caso agrega los elementos faltantes para poder generar un cardsSet

;dobble?
(define dobble1 (dobble? cardsSet1)) ;Set de cartas valido, ya que fue generado anteriormente
(define dobble2 (dobble? cardsSet2)) ;Set de cartas no valido, ya que es vacio
(define dobble3 (dobble? cardsSet3)) ;Otro set de cartas valido, ya definido antes
(define dobble4 (dobble? (insertCard (createElementsAndList "A" "1" "A") cardsSet1))) ;Se le agrega una carta no valida al cardsSet1, por lo tanto es no valido

;numCards
(define numCards1 (numCards cardsSet1)) ;Son 7 cartas, ya que es el maximo generado con 3 elementos
(define numCards2 (numCards cardsSet2)) ;Arroja 0, ya que el cardsSet es vacio
(define numCards3 (numCards cardsSet3)) ;Arroja el numero de cartas establecido de 34

;nthCard
(define nthCard1 (nthCard cardsSet1 0)) ;Arroja la primera carta del cardsSet1
(define nthCard2 (nthCard cardsSet2 3)) ;Arroja una carta vacia (emptyElementsList) ya que el cardsSet es vacio
(define nthCard3 (nthCard cardsSet3 33)) ;Arroja la ultima carta del cardsSet3

;findTotalCards
(define findTotalCards1 (findTotalCards (firstCard cardsSet1))) ;Total de cartas 7
(define findTotalCards2 (findTotalCards '())) ;Carta vacia por lo tanto 0
(define findTotalCards3 (findTotalCards (firstCard cardsSet3))) ;Total Cartas 57, que es el maximo a generar
(define findTotalCards4 (findTotalCards '("A" "B" "C" "D"))) ;Total Cartas 13 para los 4 elementos
(define findTotalCards5 (findTotalCards '("A" "B" "C" "D" "E"))) ;Total cartas 0, ya que no es un numero primo de cartas

;requiredElements
(define requiredElements1 (requiredElements (firstCard cardsSet1))) ;Total de elementos 7
(define requiredElements2 (requiredElements '())) ;Carta vacia por lo tanto 0
(define requiredElements3 (requiredElements (firstCard cardsSet3))) ;Total elementos 57, que son los necesarios
(define requiredElements4 (requiredElements '("A" "B" "C" "D"))) ;Total elementos 13
(define requiredElements5 (requiredElements '("A" "B" "C" "D" "E"))) ;Total elementos 0, ya que no es un numero primo de cartas

;missingCards
(define missingCards1 (missingCards cardsSet1)) ;0 cartas faltantes ya que el cardsSet1 es completo por lo tanto devuelve un emptyCardsSet 
(define missingCards2 (missingCards cardsSet2)) ;0 cartas faltantes ya que el cardsSet2 es vacio por lo tanto devuelve un emptyCardsSet 
(define missingCards3 (missingCards cardsSet3)) ;devuelve las 23 cartas faltantes del cardsSet3

;cardsSet->string
(define cardsSet->string1 (cardsSet->string cardsSet1))
(define cardsSet->string2 (cardsSet->string cardsSet2))
(define cardsSet->string3 (cardsSet->string cardsSet3))

;game
(define game1 (game 4 cardsSet1 stackMode randomFn)) ;game con stackMode
(define game2 (game 3 cardsSet1 emptyHandsStackMode randomFn)) ;game con emptyHandsStackMode
(define game3 (game 5 cardsSet3 myMode randomFn)) ;game con myMode

;stackMode
(define stackMode1 (stackMode cardsSet1)) ;primeras cartas del cardsSet1
(define stackMode2 (stackMode cardsSet2)) ;emptyCardsSet, ya que el cardsSet2 es vacio
(define stackMode3 (stackMode cardsSet3)) ;primeras cartas del cardsSet3

;register
(define register1 (register "user1" (register "user2" (register "user1" game1)))) ;Prueba de registro con mismo usuario, devuelve solo con user1 y user2
(define register2 (register "user4" (register "user3" (register "user2" (register "user1" game2))))) ;prueba de registro con un usuario de mas, devuelve con user1, user2, y user3
(define register3 (register "user3" (register "user2" (register "user1" game3)))) ;Registro normal con 3 de los 5 jugadores posibles a registrar

;whoseTurnIsIt?
(define whoseTurnIsIt1 (whoseTurnIsIt? register1)) ;Turno sin jugadas, por lo tanto arroja el primer jugador registrado
(define whoseTurnIsIt2 (whoseTurnIsIt? (play (play register2 null) pass))) ;Paso un turno, por lo tanto arroja el segundo jugador registrado
(define whoseTurnIsIt3 (whoseTurnIsIt? (play (play (play (play register3 null) pass) null) pass))) ;Paso dos turno, por lo tanto arroja el tercer jugador registrado

;play
(define play1 (play (play (play (play (play (play register1 null) (spotIt "A")) pass) null) pass) null)) ;juego con cartas en mesa, y con distintos tipos de jugadas
(define play2 (play (play (play (play (play (play register2 null) (spotIt "B")) pass) null) (spotIt "F")) finish)) ;juego terminado
(define play3 (play (play (play (play (play (play (play (play (play register2 null) (spotIt "A")) pass) null) (spotIt "C")) null) (spotIt "E")) pass) finish)) ;juego con terminado, y con distintos tipos de jugadas

;status
(define status1 (status play1))
(define status2 (status play2))
(define status3 (status play3))

;score
(define score1 (score play1 "user2"))
(define score2 (score play2 "user1"))
(define score3 (score play3 "user5"))

;game->string
(define game->string1 (game->string play1)) ;Mustra las cartas en juego
(define game->string2 (game->string play2)) ;Muestra el ganador y los perdedores
(define game->string3 (game->string play3)) ;Muestra los "ganadores"

;addCard
(define addCard1 (addCard cardsSet1 (list "A" "B" "C"))) ;Agrega cartas que ya estan presentes, por lo tanto devuelve el cardsSet1
(define addCard2 (addCard (addCard (addCard emptyCardsSet (list "B" "E" "F")) (list "A" "D" "E")) (list "A" "B" "C"))) ;Agrega cartas validas
(define addCard3 (addCard (addCard (addCard emptyCardsSet (list "B" "E" "F")) (list "A" "B" "E")) (list "A" "B" "C"))) ;Se agregan solo las cartas validas

;emptyHandsStackMode
(define emptyHandsStackMode1 (emptyHandsStackMode cardsSet1)) ;primera carta del cardsSet1, y una "random" de la segunda mitad
(define emptyHandsStackMode2 (emptyHandsStackMode cardsSet2)) ;emptyCardsSet, ya que el cardsSet2 es vacio
(define emptyHandsStackMode3 (emptyHandsStackMode cardsSet3)) ;primera carta del cardsSet3, y una "random" de la segunda mitad

;myMode
(define myMode1 (myMode cardsSet1)) ;3 cartas "random" entre 3 particiones del cardsSet1
(define myMode2 (myMode cardsSet2)) ;emptyCardsSet, ya que el cardsSet2 es vacio
(define myMode3 (myMode cardsSet3)) ;3 cartas "random" entre 3 particiones del cardsSet3
