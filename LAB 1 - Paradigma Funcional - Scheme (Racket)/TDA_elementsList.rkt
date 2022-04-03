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
(define createElementsList (lambda x (if (elementsList? x)
                                         x
                                         null)))

#|
Operacion: Constructor
Descripcion: crea una lista de elementos
Dominio: numbers + strings (pueden ser varios)
Recorrido: elementsList
|#
(define createElementsAndList (lambda x (define eL (lazy  (map element x)))
                                (if (elementsList? (force eL))
                                    (force eL)
                                    null)))

(define createElementsList-NtoM (lambda (start end)
                                  (define recursion (lambda (i eL)
                                                      (if (< i start)
                                                          eL
                                                          (recursion (- i 1) (insertElement (element i) eL)))))
                                  (recursion end null)))

(define elementsList? (lambda (eL)
                         (define recursion (lambda (eL1 eL2)
                                             (define recursion2 (lambda (eL3)
                                                                  (cond
                                                                    [(null? eL3) (recursion (nextElements eL1) (nextElements eL2))]
                                                                    [(element=? (firstElement eL1) (firstElement eL3)) #f]
                                                                    [else (recursion2 (nextElements eL3))])))
                                             (if (null? eL2)
                                                 #t
                                                 (recursion2 eL2))))
                         (if (andmap element? eL)
                             (recursion eL (nextElements eL))
                             #f)))

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

(define oneCommonElement? (lambda (card1 card2)
                            (define recursion (lambda (eL1 eL2 count)
                                                (cond
                                                  [(< 1 count) #f]
                                                  [(null? eL1) (if (= count 1) #t #f)]
                                                  [(null? eL2) (recursion (nextElements eL1) card2 count)]
                                                  [(element=? (firstElement eL1) (firstElement eL2)) (recursion eL1 (nextElements eL2) (+ count 1))]
                                                  [else (recursion eL1 (nextElements eL2) count)])))
                            (recursion card1 card2 0)))
