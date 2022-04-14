#lang racket
(provide (all-defined-out))

#|
Operacion: constructor
Descripcion: crea un game
Dominio: gamersInfo X gameArea X string(status) X procedure/function(mode) X procedure/function (rndfn)
Recorrido: game
|#
(define setGame (lambda (gamersInfo gameArea status mode rndfn) (list gamersInfo gameArea status mode rndfn)))

#|
Operacion: selector
Descripcion: obtiene la informacion de los jugadres(gamersInfo) del juego(game)
Dominio: game
Recorrido: gamersInfo
|#
(define getGamersInfo (lambda (gm) (car gm)))

#|
Operacion: selector
Descripcion: obtiene el area de juego(gameArea) del juego(game)
Dominio: game
Recorrido: gameArea
|#
(define getGameArea (lambda (gm) (cadr gm)))

#|
Operacion: selector
Descripcion: obtiene el modo de juego(mode) del juego(game)
Dominio: game
Recorrido: procedure/function
|#
(define getMode (lambda (gm) (cadddr gm)))

#|
Operacion: selector
Descripcion: obtiene la funcion "random"(rndfn) del juego(game)
Dominio: game
Recorrido: procedure/function
|#
(define getRandomFn (lambda (gm) (car (cddddr gm))))
