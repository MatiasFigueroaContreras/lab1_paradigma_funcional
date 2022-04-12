#lang racket
(require "TDA_elementsList.rkt")
(provide (all-defined-out))

#|
TDA CardsSet (lista de listas de elementos)
- (list (list elements))
Operaciones complementarias del TDA principal (main.rkt)
|#

#|
SubProblema de cardsSet - constructor  
Operacion: Constructor
Descripcion: crea un set de una carta(elementsList)
Dominio: elementsList X int
Recorrido: cardsSet
Recursion: Cola
|#
(define firstCardGeneration (lambda (elements n)
                              (define recursion(lambda (i card)
                                                 (if (= i 0)
                                                     (insertElement (nthElement elements i) card)
                                                     (recursion (- i 1) (insertElement (nthElement elements i) card)))))
                              (recursion n null)))

#|
SubProblema de cardsSet - constructor  
Operacion: Constructor
Descripcion: crea un set de n cartas(elementsList)
Dominio: elementsList X int X int
Recorrido: cardsSet
Recursion: Cola
|#
(define nCardGeneration (lambda (elements n cardsCount)
                          (define recursion(lambda (j cardsList cardsCount)
                                             (define recursion2(lambda (k card)
                                                                 (if (= k 0)
                                                                     card
                                                                     (recursion2 (- k 1) (insertElement (nthElement elements (-(+(* n j)(+ k 1)) 1)) card)))))
                                              (if (or (= j 0) (<= cardsCount 0))
                                                 cardsList
                                                 (recursion (- j 1) (insertCard (insertElement (nthElement elements 0) (recursion2 n null)) cardsList) (- cardsCount 1)))))
                          (recursion n null cardsCount)))

#|
SubProblema de cardsSet - constructor  
Operacion: Constructor
Descripcion: crea un set de n**2 cartas(elementsList)
Dominio: elementsList X int X int
Recorrido: cardsSet
Recursion: Cola
|#
(define n2CardGeneration (lambda (elements n cardsCount)
                          (define recursion(lambda (i j cardsList cardsCount)
                                             (define recursion2(lambda (k card)
                                                                 (if (= k 0)
                                                                     card
                                                                     (recursion2 (- k 1) (insertElement (nthElement elements (-(+ n 2 (* n (- k 1)) (modulo (+(*(- i 1) (- k 1)) (- j 1)) n)) 1)) card)))))
                                              (if (or (= i 0) (<= cardsCount 0))
                                                  cardsList
                                                  (if (= j 0)
                                                      (recursion (- i 1) n cardsList cardsCount)
                                                      (recursion i (- j 1) (insertCard (insertElement (nthElement elements i) (recursion2 n null)) cardsList) (- cardsCount 1))))))
                          (recursion n n null cardsCount)))

(define emptyCardsSet null)

#|
Operacion: Selector
Descripcion: Permite obtener la primera carta(elementsList) de una mazo de cartas (cardsSet)
Dominio: cardsSet
Recorrido: elementsList
|#
(define firstCard (lambda (cS) (car cS)))

#|
Operacion: Selector
Descripcion: Permite obtener las siguientes cartas(elementsList) de una mazo de cartas (cardsSet)
Dominio: cardsSet
Recorrido: cardsSet
|#
(define nextCards (lambda (cS) (cdr cS)))

#|
Operacion: Modificador
Descripcion: Inserta una carta(elementsList) en un mazo de cartas (cardsSet)
Dominio: elementsList X cardsSet
Recorrido: cardsSet
|#
(define insertCard (lambda (c cS) (cons c cS)))

#|
Operacion: Modificador
Descripcion: Une dos mazos de cartas (cardsSet)
Dominio: cardsSet X cardsSet
Recorrido: cardsSet
Recursion: Cola
|#
(define unionCardsSet (lambda (cS1 cS2)
                        (if (emptyCardsSet? cS1)
                            cS2
                            (unionCardsSet (nextCards cS1) (insertCard (firstCard cS1) cS2)))))

(define emptyCardsSet? (lambda (cS) (null? cS)))

(define elementsCardsSet (lambda (cS)
                           (define recursion (lambda (cS R)
                                               (if (emptyCardsSet? cS)
                                                   R
                                               (recursion (nextElements cS) (unionElementsList R (firstElement cS))))))
                           (recursion cS null)))
