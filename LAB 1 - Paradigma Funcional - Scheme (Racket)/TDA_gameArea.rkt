#lang racket
(provide (all-defined-out))

#|
Operacion: Constructor
Descripcion: crea un gameArea
Dominio: cardsSet X cardsSet
Recorrido: gameArea
|#
(define gameArea (lambda (cardsInPlay cS) (list cardsInPlay cS)))

#|
Operacion: Constructor
Descripcion: crea un gameArea vacio
Dominio:
Recorrido: gameArea
|#
(define emptyGameArea (list null null))

#|
Operacion: Selector
Descripcion: obtiene el cardsSet que representa las cartas en juego
Dominio: gameArea
Recorrido: cardsSet
|#
(define getCardsInPlay (lambda (gA) (car gA)))

#|
Operacion: Selector
Descripcion: obtiene el cardsSet que representa el total de cartas
Dominio: gameArea
Recorrido: cardsSet
|#
(define getCardsSet (lambda (gA) (cadr gA)))

#|
Operacion: Modificador
Descripcion: cambia el cardsSet que representa las cartas en juego en el gameArea
Dominio: gameArea X cardsSet
Recorrido: gameArea
|#
(define setCardsInPlay (lambda (gA cSP) (gameArea cSP (getCardsSet gA))))

#|
Operacion: Modificador
Descripcion: cambia el cardsSet que representa el total de cartas en el gameArea
Dominio: gameArea X cardsSet
Recorrido: gameArea
|#
(define setCardsSet (lambda (gA cS) (gameArea (getCardsInPlay gA) cS)))
