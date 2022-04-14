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
recorrido: element
|#
(define element (lambda (x) (if (number? x) (number->string x) x)))

#|
Operacion: Pertenencia
Descripcion: consulta si un elemento efectivamente lo es
dominio: element
recorrido: boolean
|#
(define element? (lambda (e) (if (string? e) #t #f)))

#|
Operacion: Otro
Descripcion: Comparacion de dos elementos
dominio: element X element
recorrido: boolean
|#
(define element=? (lambda (e1 e2) (string=? e1 e2)))
