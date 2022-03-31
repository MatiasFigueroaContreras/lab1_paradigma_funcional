#lang racket
(require "TDA_element.rkt")

#|
TDA elemntsList (lista de elementos, por lo tanto, este puede representar tanto a una carta del juego dobble como otros conjuntos de elementos)
- list of elements
|#

#|
Operacion: Constructor
Descripcion: crea una lista de elementos
dominio: elements (puedens ser varios)
recorrido: elementsList
|#
(define createElementsList (lambda x list x))

#|
Operacion: Constructor
Descripcion: crea una lista de elementos
dominio: numbers + strings (pueden ser varios)
recorrido: elementsList
|#
(define createElementsAndList (lambda x (map element x)))

#|
Operacion: Pertenencia
Descripcion: determina si un elemento se encuentra en una lista de elementos
dominio: ElementList X Element
recorrido: boolean
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
dominio: ElementList
recorrido: Element
|#
(define firstElement (lambda (eL) (car eL)))

#|
Operacion: Selector
Descripcion: Permite obtener los siguientes elementos de una lista de elementos
dominio: ElementList
recorrido: ElementList
|#
(define nextElements (lambda (eL) (cdr eL)))

#|
Operacion: Selector
Descripcion: Permite obtener el elemento en una poscion dada (partiendo de la posicion 0)
dominio: ElementList X (posicion)Int
recorrido: Element
Recursion: Cola
|#
(define nthElement (lambda (eL n)
                     (if (= n 0)
                         (firstElement eL)
                         (nthElement (nextElements eL) (- n 1)))))

#|
Operacion: Otro
Descripcion: Permite obtener el largo de una lista de elementos
dominio: ElementList
recorrido: int
Recursion: Cola
|#
(define elementsListLenght (lambda (eL)
                             (define recursion (lambda (eL l)
                                                 (if (null? eL) l
                                                     (recursion (nextElements eL) (+ l 1)))))
                             (recursion eL 0)))
