#lang racket
(require "TDA_element.rkt")
(provide (all-defined-out))

#|
TDA elemntsList (lista de elementos, por lo tanto, este puede representar tanto a una carta del juego dobble como otros conjuntos de elementos)
- (list elements)
|#

#|
Operacion: Constructor
Descripcion: crea una lista de elementos
Dominio: elements (puedens ser varios)
Recorrido: elementsList
|#
(define createElementsList (lambda x list x))

#|
Operacion: Constructor
Descripcion: crea una lista de elementos
Dominio: numbers + strings (pueden ser varios)
Recorrido: elementsList
|#
(define createElementsAndList (lambda x (map element x)))

(define createElementsList-NtoM (lambda (start end)
                                  (define recursion (lambda (i eL)
                                                      (if (< i start)
                                                          eL
                                                          (recursion (- i 1) (insertElement (element i) eL)))))
                                  (recursion end null)))


#|
Operacion: Pertenencia
Descripcion: determina si un elemento se encuentra en una lista de elementos
Dominio: elementsList X Element
Recorrido: boolean
Recursion: Cola
|#
(define isElementList? (lambda (eL e)
                     (if (null? eL)
                         #f
                         (if (element=? (firstElement eL) e)
                             #t
                             (isElementList? (nextElements eL) e)))))

#|
Operacion: Selector
Descripcion: Permite obtener el primer elemento de una lista de elementos
Dominio: elementsList
Recorrido: element
|#
(define firstElement (lambda (eL) (car eL)))

#|
Operacion: Selector
Descripcion: Permite obtener los siguientes elementos de una lista de elementos
Dominio: elementsList
Recorrido: elementsList
|#
(define nextElements (lambda (eL) (cdr eL)))

#|
Operacion: Selector
Descripcion: Permite obtener el elemento en una poscion dada (partiendo de la posicion 0)
Dominio: elementsList X (posicion)Int
Recorrido: element
Recursion: Cola
|#
(define nthElement (lambda (eL n)
                     (if (= n 0)
                         (firstElement eL)
                         (nthElement (nextElements eL) (- n 1)))))

(define insertElement (lambda (e eL) (cons e eL)))

(define unionElementsList (lambda (eL1 eL2)
                        (if (null? eL1)
                            eL2
                            (unionElementsList (nextElements eL1) (insertElement (firstElement eL1) eL2)))))

#|
Operacion: Otro
Descripcion: Permite obtener el largo de una lista de elementos
Dominio: elementsList
Recorrido: int
Recursion: Cola
|#
(define elementsListLenght (lambda (eL)
                             (define recursion (lambda (eL l)
                                                 (if (null? eL) l
                                                     (recursion (nextElements eL) (+ l 1)))))
                             (recursion eL 0)))
