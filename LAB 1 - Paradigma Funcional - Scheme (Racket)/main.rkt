#lang racket
(require "TDA_elementsList.rkt")
(require "TDA_cardsSet.rkt")
(require (only-in math/number-theory prime?))

(define el (createElementsAndList "A" "B" "C" "D" "E" "F" "G"))

(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 ))

(define cardsSet (lambda (elements numE maxC rndFn)
                   (if (prime? (- numE 1))
                   (if (< (elementsListLenght elements) (+ (- (* numE numE) numE) 1))
                       (cardsSet (createElementsList-NtoM 1 (+ (- (* numE numE) numE) 1)) numE maxC rndFn)
                       (if (<= maxC 0)
                           (cardsSet elements numE (+ (- (* numE numE) numE) 1) rndFn)
                           (insertCard (firstCardGeneration elements (- numE 1)) (unionCardsSet (nCardGeneration elements (- numE 1) (- maxC 1)) (n2CardGeneration elements (- numE 1) (- maxC numE))))))
                   null)))

(define numCards (lambda (cS)
                   (define recursion (lambda (cS l)
                                       (if (null? cS) l
                                           (recursion (nextCards cS) (+ l 1)))))
                   (recursion cS 0)))

(define nthCard (lambda (cS n)
                  (if (= n 0)
                         (firstCard cS)
                         (nthElement (nextCards cS) (- n 1)))))

(define findTotalCards (lambda (eL)
                         (define totalCards (lambda (numE) (+ (- (* numE numE) numE) 1)))
                         (totalCards (elementsListLenght eL))))

(define requiredElements (lambda (eL)
                         (define totalCards (lambda (numE) (+ (- (* numE numE) numE) 1)))
                         (totalCards (elementsListLenght eL))))
