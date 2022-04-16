#lang racket
(require "TDA_gamers.rkt")
(require "TDA_gamersPoints.rkt")
(provide (all-defined-out))

#|
Operacion: Constructor
Descripcion: crea un gamersInfo con n jugadores (numG)
Dominio: Int (numero de jugadores)
Recorrido: gamersInfo
|#
(define initGamersInfo (lambda (numG)
                         (list numG 1 initGamers emptyGamersPoints)))

#|
Operacion: Constructor
Descripcion: crea un gamersInfo
Dominio: int (numero de jugadores) X int (turno) X gamers X gamersPoints
Recorrido: gamersInfo
|#
(define gamersInfo (lambda (numG gTurn g gP)
                     (list numG gTurn g gP)))

#|
Operacion: Selector
Descripcion: obtiene el numero de jugadores
Dominio: gamersInfo
Recorrido: Int (numero de jugadores)
|#
(define getNumG (lambda (gI) (car gI)))

#|
Operacion: Selector
Descripcion: obtiene el turno
Dominio: gamersInfo
Recorrido: Int
|#
(define getTurn (lambda (gI) (car (cdr gI))))

#|
Operacion: Selector
Descripcion: obtiene gamers (la lista de jugadores)
Dominio: gamersInfo
Recorrido: gamers
|#
(define getGamers (lambda (gI) (car (cddr gI))))

#|
Operacion: Selector
Descripcion: obtiene los gamersPoints (puntajes de los jugadores)
Dominio: gamersInfo
Recorrido: gamersPoints
|#
(define getGamersPoints (lambda (gI) (car (cdddr gI))))

#|
Operacion: Selector
Descripcion: obtiene el puntaje de un jugador dado
Dominio: gamersInfo X string (nombre jugador)
Recorrido: Int (puntaje de un jugador)
|#
(define getGamerScore (lambda (gI nickname)
                        (let ([g (getGamers gI)] [gP (getGamersPoints gI)])
                          (nthPoint gP (gamerPos nickname g)))))

#|
Operacion: Selector
Descripcion: obtiene el nombre del jugador al que le toca su turno
Dominio: gamersInfo
Recorrido: string (gamer)
|#
(define getGamerTurn (lambda (gI) (nthGamer (getGamers gI) (getTurn gI))))

#|
Operacion: Otro
Descripcion: calcula el total de jugadores registrados
Dominio: gamersInfo
Recorrido: int
|#
(define totalRegisteredGamers (lambda (gI) (totalGamers (getGamers gI))))

#|
Operacion: Modificador
Descripcion: pasa al siguiente turno de jugador
Dominio: gamersInfo
Recorrido: gamersInfo
|#
(define nextTurn (lambda (gI)
                   (let ([numG (getNumG gI)] [gTurn (getTurn gI)] [g (getGamers gI)] [gP (getGamersPoints gI)])
                     (if (or (= (totalGamers g) 0) (= gTurn (totalGamers g)))
                         (gamersInfo numG 1 g gP)
                         (gamersInfo numG (+ gTurn 1) g gP)))))

#|
Operacion: Modificador
Descripcion: ingresa un nuevo jugador
Dominio: string(gamer) X gamersInfo
Recorrido: gamersInfo
|#
(define newGamer (lambda (nickname gI)
                   (let ([numG (getNumG gI)] [gTurn (getTurn gI)] [g (getGamers gI)] [gP (getGamersPoints gI)])
                     (let ([numRG (totalGamers g)] [newG (registerGamer nickname g)])
                       (if (and (> numG numRG) (> (totalGamers newG) numRG))
                           (gamersInfo numG gTurn newG (registerNewGamerPoint gP))
                           gI)))))

#|
Operacion: Modificador
Descripcion: suma un puntaje dado al jugador que tiene el turno
Dominio: gamersInfo X int (cantidad de puntaje)
Recorrido: gamersInfo
|#                        
(define addScore (lambda (gI scoreNum)
                   (let ([numG (getNumG gI)] [gTurn (getTurn gI)] [g (getGamers gI)] [gP (getGamersPoints gI)])
                     (gamersInfo numG gTurn g (addPoints gP scoreNum gTurn)))))

#|
Operacion: Selector
Descripcion: obtiene el/los ganadores segun el puntaje de los jugadores
Dominio: gamersInfo
Recorrido: gamers
Recursion: de Cola
|#
(define getWinners (lambda (gI)
                     (define recursion (lambda (g gP R max)
                                         (if (emptyGamersPoints? gP)
                                             R
                                             (if (= max (firstPoint gP))
                                                 (recursion (nextGamers g) (nextPoints gP) (insertGamer (firstGamer g) R) max)
                                                 (recursion (nextGamers g) (nextPoints gP) R max)))))
                     (recursion (getGamers gI) (getGamersPoints gI) null (maxPoints (getGamersPoints gI)))))

#|
Operacion: Selector
Descripcion: obtiene el/los perdedores segun el puntaje de los jugadores
Dominio: gamersInfo
Recorrido: gamers
Recursion: de Cola
|#
(define getLosers (lambda (gI)
                    (define recursion (lambda (g gP R max)
                                         (if (emptyGamersPoints? gP)
                                              R
                                             (if (> max (firstPoint gP))
                                                 (recursion (nextGamers g) (nextPoints gP) (insertGamer (firstGamer g) R) max)
                                                 (recursion (nextGamers g) (nextPoints gP) R max)))))
                    (recursion (getGamers gI) (getGamersPoints gI) null (maxPoints (getGamersPoints gI)))))

#|
Operacion: otro
Descripcion: obtiene el/los ganadores y perdedores segun el puntaje de los jugadores y los representa en un string
Dominio: gamersInfo
Recorrido: string
|#
(define winnersLosersString (lambda (gI)
                        (string-append* "\nResultados:" (string-append* "\nGanadores:\n" (cdr (append* (map (lambda (x) (list ", " x)) (getWinners gI))))) "\nPerdedores:\n" (cdr (append* (map (lambda (x) (list ", " x)) (getLosers gI)))))))

#|
Operacion: otro
Descripcion: pasa la informacion del gamersInfo a un string
Dominio: gamersInfo
Recorrido: string
|#
(define gamersInfo->string (lambda (gI)
                             (define gamersNamesPoints->string (lambda (g gP i string)
                                                                 (if (emptyGamers? g)
                                                                     string
                                                                     (gamersNamesPoints->string (nextGamers g) (nextPoints gP) (+ i 1) (string-append* string "\nJugador n°" (number->string i) ": " (firstGamer g)
                                                                                                                                                       ", Puntaje: " (number->string (firstPoint gP)) null)))))
                             (let ([numG (getNumG gI)] [gTurn (getTurn gI)] [g (getGamers gI)] [gP (getGamersPoints gI)])
                               (string-append* "Maximos jugadores en el juego: " (number->string numG)
                                               "\nNumero de jugadores Registrados: " (number->string (totalRegisteredGamers gI))
                                               "\nTurno del jugador: n°" (number->string gTurn)
                                               "\nJugadores Registrados: " (gamersNamesPoints->string g gP 1 "") null))))
