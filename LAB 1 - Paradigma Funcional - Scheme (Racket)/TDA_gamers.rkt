#lang racket
(provide (all-defined-out))

#|
Operacion: Constructor
Descripcion: crea una conjunto de jugadores(gamers) vacio
Dominio: 
Recorrido: gamers
|#
(define initGamers null)

#|
Operacion: otro
Descripcion: consulta si un conjunto de jugadores(gamers) es vacio
Dominio: gamers
Recorrido: boolean
|#
(define emptyGamers? (lambda (gamers)
                       (if (null? gamers) #t #f)))

#|
Operacion: Selector
Descripcion: obtiene el primer jugador
Dominio: gamers
Recorrido: string (gamer)
|#
(define firstGamer (lambda (gamers) (car gamers)))

#|
Operacion: Selector
Descripcion: obtiene los siguientes jugadores
Dominio: gamers
Recorrido: gamers
|#
(define nextGamers (lambda (gamers) (cdr gamers)))

#|
Operacion: Modificador
Descripcion: inserta un jugador
Dominio: string(gamer) X gamers
Recorrido: gamers
|#
(define insertGamer (lambda (gamer gamers) (cons gamer gamers)))

#|
Operacion: Otro
Descripcion: compara dos jugadores(gamer)
Dominio: string(gamer) X string(gamer)
Recorrido: boolean
|#
(define gamer=? (lambda (g1 g2) (string=? g1 g2)))

#|
Operacion: Otro
Descripcion: consulta si un jugador pertenece a los jugadores(gamers)
Dominio: string(nickname) X gamers
Recorrido: boolean
|#
(define isGamer? (lambda (nickname gamers)
                   (ormap (lambda (gamer) (if (gamer=? nickname gamer) #t #f)) gamers)))

#|
Operacion: Modificador
Descripcion: registra un jugador en los jugadores(gamers)
Dominio: string(nickname) X gamers
Recorrido: gamers
Recursion: Natural
|#
(define registerGamer (lambda (nickname gamers)
                        (if (isGamer? nickname gamers)
                            gamers
                            (if (emptyGamers? gamers)
                                (insertGamer nickname null)
                                (insertGamer (firstGamer gamers) (registerGamer nickname (nextGamers gamers)))))))

#|
Operacion: selector
Descripcion: obtiene el nth jugador del conjunto de jugadores(gamers)
Dominio: gamers X int
Recorrido: string(gamer)
Recursion: Natural
|#
(define nthGamer (lambda (gamers n)
                   (if (emptyGamers? gamers)
                       ""
                       (if (= n 1)
                           (firstGamer gamers)
                           (nthGamer (nextGamers gamers) (- n 1))))))

#|
Operacion: otro
Descripcion: calcula la cantidad de jugadores
Dominio: gamers
Recorrido: int
|#
(define totalGamers (lambda (gamers)
                      (apply + (map (lambda (x) 1) gamers))))   

#|
Operacion: otro
Descripcion: obtiene la posicion de un jugador
Dominio: string(nickname) X gamers
Recorrido: int
Recursion: de Cola
|#
(define gamerPos (lambda (nickname gamers)
                   (define recursion (lambda (pos gamers)
                                           (if (gamer=? nickname (firstGamer gamers))
                                               pos
                                               (recursion (+ pos 1) (nextGamers gamers)))))
                   (if (isGamer? nickname gamers)
                       (recursion 1 gamers)
                       -1)))
