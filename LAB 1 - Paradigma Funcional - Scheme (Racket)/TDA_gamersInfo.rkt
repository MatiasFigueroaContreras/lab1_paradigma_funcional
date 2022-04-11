#lang racket
(require "TDA_gamers.rkt")
(require "TDA_gamersPoints.rkt")
(provide (all-defined-out))

(define initGamersInfo (lambda (numG)
                         (list numG 1 initGamers emptyGamersPoints)))

(define gamersInfo (lambda (numG gTurn g gP)
                     (list numG gTurn g gP)))

(define getNumG (lambda (gI) (car gI)))

(define getTurn (lambda (gI) (car (cdr gI))))

(define getGamers (lambda (gI) (car (cddr gI))))

(define getGamersPoints (lambda (gI) (car (cdddr gI))))

(define getGamerScore (lambda (gI nickname)
                        (let ([g (getGamers gI)] [gP (getGamersPoints gI)])
                          (nthPoint gP (gamerPos nickname g)))))

(define nextTurn (lambda (gI)
                   (let ([numG (getNumG gI)] [gTurn (getTurn gI)] [g (getGamers gI)] [gP (getGamersPoints gI)])
                     (if (or (= (totalGamers g) 0) (= gTurn (totalGamers g)))
                         (gamersInfo numG 1 g gP)
                         (gamersInfo numG (+ gTurn 1) g gP)))))

(define getGamerTurn (lambda (gI) (nthGamer (getGamers gI) (getTurn gI))))

(define newGamer (lambda (nickname gI)
                   (let ([numG (getNumG gI)] [gTurn (getTurn gI)] [g (getGamers gI)] [gP (getGamersPoints gI)])
                     (let ([numRG (totalGamers g)] [newG (registerGamer nickname g)])
                       (if (and (> numG numRG) (> (totalGamers newG) numRG))
                           (gamersInfo numG gTurn newG (registerNewGamerPoint gP))
                           gI)))))
(define addScore (lambda (gI)
                   (let ([numG (getNumG gI)] [gTurn (getTurn gI)] [g (getGamers gI)] [gP (getGamersPoints gI)])
                     (gamersInfo numG gTurn g (addPoints gP 1 gTurn)))))

(define gamersInfo->string (lambda (gI)
                             (define gamersNamesPoints->string (lambda (g gP i string)
                                                                 (if (emptyGamers? g)
                                                                     string
                                                                     (gamersNamesPoints->string (nextGamers g) (nextPoints gP) (+ i 1) (string-append* string "\nJugador n°" (number->string i) ": " (firstGamer g)
                                                                                                                                                       ", Puntaje: " (number->string (firstPoint gP)) null)))))
                             (let ([numG (getNumG gI)] [gTurn (getTurn gI)] [g (getGamers gI)] [gP (getGamersPoints gI)])
                               (string-append* "Maximos jugadores en el juego: " (number->string numG)
                                               "\nNumero de jugadores Registrados: " (number->string (totalGamers g))
                                               "\nTurno del jugador: n°" (number->string gTurn)
                                               "\nJugadores Registrados: " (gamersNamesPoints->string g gP 1 "") null))))



