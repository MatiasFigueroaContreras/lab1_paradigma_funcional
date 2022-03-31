#lang racket
(provide (all-defined-out))

#|
TDA Element (Referencia a un elemento o simbolo en el juego dobble)
-String
|#

#|
Operacion: Constructor
Descripcion: crea un elemento
dominio: number + string
recorrido: string
|#
(define element (lambda (x) (if (number? x) (number->string x) x)))

#|
Operacion: Otro
Descripcion: Comparacion de dos elementos
dominio: number + string
recorrido: string
|#
(define element=? (lambda (e1 e2) (string=? e1 e2)))
