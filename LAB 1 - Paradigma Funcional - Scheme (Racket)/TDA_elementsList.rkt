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

#|
Operacion: Pertenencia
Descripcion: verifica si una lista de elementos efectivamente lo es.
Dominio: elementsList
Recorrido: boolean
|#
(define elementsList? (lambda (eL)
                         (define recursion (lambda (eL1 eL2)
                                             (define recursion2 (lambda (eL3)
                                                                  (cond
                                                                    [(emptyElementsList? eL3) (recursion (nextElements eL1) (nextElements eL2))]
                                                                    [(element=? (firstElement eL1) (firstElement eL3)) #f]
                                                                    [else (recursion2 (nextElements eL3))])))
                                             (if (emptyElementsList? eL2)
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
                     (if (or (emptyElementsList? eL) (not (element? e)))
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

#|
Operacion: Modificador
Descripcion: inserta un elemento a una lista de elemenots
Dominio: element X elementsList
Recorrido: elementsList
|#
(define insertElement (lambda (e eL) (cons e eL)))

#|
Operacion: Otro
Descripcion: une dos listas de elementos
Dominio: elementsList X elementsList
Recorrido: elementsList
Recursion: de Cola
|#
(define unionElementsList (lambda (eL1 eL2)
                             (cond
                               [(emptyElementsList? eL1) eL2]
                               [(isElementList? eL2 (firstElement eL1)) (unionElementsList (nextElements eL1) eL2)]
                               [else (unionElementsList (nextElements eL1) (insertElement (firstElement eL1) eL2))])))


#|
Operacion: Otro
Descripcion: Permite obtener el largo de una lista de elementos
Dominio: elementsList
Recorrido: int
Recursion: Cola
|#
(define elementsListLenght (lambda (eL)
                             (define recursion (lambda (eL l)
                                                 (if (emptyElementsList? eL) l
                                                     (recursion (nextElements eL) (+ l 1)))))
                             (recursion eL 0)))

#|
Operacion: Pertenencia
Descripcion: verifica si una lista de elementos es vacia
Dominio: elementsList
Recorrido: boolean
|#
(define emptyElementsList? (lambda (eL) (null? eL)))

#|
Operacion: Otro
Descripcion: calcula la cantidad de elementos que tienen en comun dos listas de elementos
Dominio: elementsList (X elementsList)
Recorrido: int
Recursion: de Cola
Funcion currificada
|#
(define commonElements (lambda (eL1) (lambda (eL2)
                                       (define counter (lambda (eL1 eL2 count)
                                                         (if (emptyElementsList? eL2)
                                                             count
                                                             (if (isElementList? eL1 (firstElement eL2))
                                                                 (counter eL1 (nextElements eL2) (+ count 1))
                                                                 (counter eL1 (nextElements eL2) count)))))
                                       (counter eL1 eL2 0))))
                               

#|
Operacion: Otro
Descripcion: consulta si dos listas de elementos tienen un solo elemento en comun
Dominio: elementsList X elementsList
Recorrido: boolean
|#
(define oneCommonElement? (lambda (eL1 eL2)
                            (if (= 1 ((commonElements eL1) eL2))
                                #t
                                #f)))

#|
Operacion: Otro
Descripcion: inserta x cantidad de elementos a una lista de elementos, respetando que este elemento no se encuentra ya en esta.
Dominio: elementsList X int
Recorrido: elementsList
Recursion: de Cola
|#
(define insertXElements (lambda (eL x)
                          (define recursion (lambda (i x eL2)
                                       (if (= i x)
                                           (unionElementsList eL eL2)
                                           (if (isElementList? eL (element x))
                                               (recursion (- i 1) (- x 1) eL2)
                                               (recursion i (- x 1) (insertElement (element x) eL2))))))
                          (recursion 0 x null)))

#|
Operacion: Otro
Descripcion: consulta si dos listas de elementos son iguales
Dominio: elementsList X (elementsList)
Recorrido: boolean
Recursion: de Cola
Funcion currificada
|#
(define elementsList=? (lambda (eL1) (lambda (eL2)
                         (define recursion (lambda (eL1 eL2)
                           (if (null? eL1)
                              #t
                              (if (isElementList? eL2 (firstElement eL1))
                              (recursion (nextElements eL1) eL2)
                              #f))))
                          (if (= (elementsListLenght eL1) (elementsListLenght eL2))
                              (recursion eL1 eL2)
                              #f))))