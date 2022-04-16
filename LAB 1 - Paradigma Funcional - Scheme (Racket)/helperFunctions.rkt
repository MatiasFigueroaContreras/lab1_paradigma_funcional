#lang racket
(provide (all-defined-out))

#|
Descripcion: Funcion de aleatorizacion que genera un numero travez de una semilla
Dominio: int
Recorrido: int
|#
(define m 2147483647)
(define a 1103515245)
(define c 12345)
(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 ))

#|
Funcion de ayuda para la aleatorizacion
Descripcion: Funcion que transforma un numero y retorna otro que no pase un maximo dado
Dominio: int
Recorrido: int
|#
(define maxN (lambda (num max)
               (if (<= num max)
                   num
                   (maxN (quotient num max) max))))

#|
Descripcion: Funcion que transforma un numero y retorna un otro entre un rango (min, max)
Dominio: int
Recorrido: int
|#
(define minMaxN (lambda (num min max)
                  (define rec (lambda (i R)
                    (cond
                      [(= i num) R]
                      [(<= max R) (rec (+ i 1) min)]
                      [else (rec (+ i 1) (+ R 1))])))
                  (if (and (<= min num) (<= num max))
                      num
                      (rec 1 min))))
