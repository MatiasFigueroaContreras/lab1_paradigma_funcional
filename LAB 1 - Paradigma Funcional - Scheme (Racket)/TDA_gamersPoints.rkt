#lang racket
(provide (all-defined-out))

#|
Operacion: Constructor
Descripcion: crea un gamersPoints vacio
Dominio: 
Recorrido: gamersPoints
|#
(define emptyGamersPoints null)

#|
Operacion: otro
Descripcion: consulta si un gamersPoints es vacio
Dominio: gamersPoints
Recorrido: boolean
|#
(define emptyGamersPoints? (lambda (gP) (null? gP)))

#|
Operacion: Selector
Descripcion: obtiene el primer valor (puntaje) del gamersPoints
Dominio: gamersPoints
Recorrido: int (puntaje)
|#
(define firstPoint (lambda (gP) (car gP)))

#|
Operacion: Selector
Descripcion: obtiene los siguientes valores del gamersPoints
Dominio: gamersPoints
Recorrido: gamersPoints
|#
(define nextPoints (lambda (gP) (cdr gP)))

#|
Operacion: Modificador
Descripcion: agrega un valor (puntaje) al gamersPoints
Dominio: int(puntaje) X gamersPoints
Recorrido: gamersPoints
|#
(define insertGamerPoints (lambda (p gP) (cons p gP)))

#|
Operacion: Modificador
Descripcion: agrega un nuevo slot de valor (puntaje) al gamersPoints
Dominio: int(puntaje) X gamersPoints
Recorrido: gamersPoints
Recursion: Natural
|#
(define registerNewGamerPoint (lambda (gP)
                                (if (emptyGamersPoints? gP)
                                (insertGamerPoints 0 null)
                                (insertGamerPoints (firstPoint gP) (registerNewGamerPoint (nextPoints gP))))))

#|
Operacion: Selector
Descripcion: busca el puntaje en la nth posicion
Dominio: gamersPoints X int (nth)
Recorrido: int (puntaje)
Recursion: de Cola
|#
(define nthPoint (lambda (gP n)
                   (if (null? gP)
                       -1
                       (if (= n 1)
                           (firstPoint gP)
                           (nthPoint (nextPoints gP) (- n 1))))))

#|
Operacion: Selector
Descripcion: busca el puntaje mayor en el gamersPoints
Dominio: gamersPoints
Recorrido: int (puntaje)
Recursion: de Cola
|#
(define maxPoints (lambda (gP)
                    (define recursion (lambda (gP max)
                                        (if (emptyGamersPoints? gP)
                                            max
                                            (if (< max (firstPoint gP))
                                                (recursion (nextPoints gP) (firstPoint gP))
                                                (recursion (nextPoints gP) max)))))
                    (recursion gP -1)))

#|
Operacion: Modificador
Descripcion: Agrega un puntaje dado al gamersPoints en la posicion n
Dominio: gamersPoints X int (puntaje) X int (n)
Recorrido: gamersPoints
Recursion: Natural
|#
(define addPoints (lambda (gP p n)
                    (if (or (< n 1) (null? gP))
                        gP
                        (if (= n 1)
                            (insertGamerPoints (+ p (firstPoint gP)) (nextPoints gP))
                            (insertGamerPoints (firstPoint gP) (addPoints (nextPoints gP) p (- n 1)))))))

