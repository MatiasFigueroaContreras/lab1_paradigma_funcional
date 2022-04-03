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

(define dobble? (lambda (cardsSet)
                  (cond
                    [(not (andmap elementsList? cardsSet)) #f]
                    [(not (andmap (lambda (x) (= (elementsListLenght (firstCard cardsSet)) x)) (map elementsListLenght cardsSet))) #f]
                    [else
                     (define recursion (lambda (cS1 cS2)
                                             (define recursion2 (lambda (cS3)
                                                                  (cond
                                                                    [(null? cS3) (recursion (nextElements cS1) (nextElements cS2))]
                                                                    [(not (oneCommonElement? (firstElement cS1) (firstElement cS3))) #f]
                                                                    [else (recursion2 (nextElements cS3))])))
                                             (if (null? cS2)
                                                 #t
                                                 (recursion2 cS2))))
                         (recursion cardsSet (nextElements cardsSet))])))

(define numCards (lambda (cardsSet)
                   (define recursion (lambda (cardsSet l)
                                       (if (null? cardsSet) l
                                           (recursion (nextCards cardsSet) (+ l 1)))))
                   (recursion cardsSet 0)))

(define nthCard (lambda (cardsSet n)
                  (if (= n 0)
                         (firstCard cardsSet)
                         (nthElement (nextCards cardsSet) (- n 1)))))

(define findTotalCards (lambda (card)
                         (define totalCards (lambda (numE) (+ (- (* numE numE) numE) 1)))
                         (totalCards (elementsListLenght card))))

(define requiredElements (lambda (card)
                         (define totalCards (lambda (numE) (+ (- (* numE numE) numE) 1)))
                         (totalCards (elementsListLenght card))))
