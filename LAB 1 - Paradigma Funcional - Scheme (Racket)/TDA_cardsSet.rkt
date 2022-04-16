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

#|
Operacion: Constructor
Descripcion: crea un set de una carta vacio
Dominio: 
Recorrido: cardsSet
|#
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

#|
Operacion: Pertenencia
Descripcion: consulta si un set de cartas es vacio
Dominio: cardsSet
Recorrido: boolean
|#
(define emptyCardsSet? (lambda (cS) (null? cS)))


#|
Operacion: Otro
Descripcion: devuelve todos los elementos que contiene un set de cartas (sin repeticion de estos)
Dominio: cardsSet
Recorrido: elementsList
Recursion: de Cola
|#
(define elementsCardsSet (lambda (cS)
                           (define recursion (lambda (cS R)
                                               (if (emptyCardsSet? cS)
                                                   R
                                               (recursion (nextElements cS) (unionElementsList R (firstElement cS))))))
                           (recursion cS null)))

#|
Operacion: Otro
Descripcion: hace la resta de dos set de cartas (funciona igual que una resta de conjuntos)
Dominio: cardsSet X cardsSet
Recorrido: cardsSet
Recursion: de Cola
|#
(define cardsSetSubstraction (lambda (cS1 cS2)
                               (define recursion (lambda (cS1 cSR)
                                                   (if (emptyCardsSet? cS1)
                                                       cSR
                                                       (if (emptyCardsSet?  (filter (elementsList=? (firstCard cS1)) cS2))
                                                           (recursion (nextCards cS1) (insertCard (firstCard cS1) cSR))
                                                           (recursion (nextCards cS1) cSR)))))
                               (recursion cS1 null)))

#|
Operacion: Otro
Descripcion: da vuelta la orientacion de un set de cartas
Dominio: cardsSet
Recorrido: cardsSet
|#
(define reverseCardsSet (lambda (cS) (unionCardsSet cS null)))

#|
Operacion: Otro
Descripcion: mezcla un cardsSet
Dominio: cardsSet
Recorrido: cardsSet
Recursion: de Cola
|#
(define mixCardsSet (lambda (cS)
                      (define numCards (lambda (cSn)
                                         (apply + (map (lambda (x) 1) cSn))))
                      (define splitCardsSet(lambda (cS1 n i R)
                        (if (= (quotient n 2) i)
                            (cons R (list cS1))
                            (splitCardsSet (nextCards cS1) n (+ i 1) (cons (firstCard cS1) R)))))
                        (if (< (numCards cS) 2)
                            cS
                            (let ([spCs (splitCardsSet cS (numCards cS) 0 null)])
                              (unionCardsSet (mixCardsSet (cadr spCs)) (mixCardsSet (car spCs)))))))

#|
Operacion: Otro
Descripcion: mezcla un cardsSet x veces
Dominio: cardsSet X int
Recorrido: cardsSet
Recursion: de Cola
|#
(define mixCardsSetXtimes (lambda (cS x)
                            (if (= x 0)
                                cS
                                (mixCardsSetXtimes (mixCardsSet cS) (- x 1)))))