#lang racket
(provide (all-defined-out))

(define gameArea (lambda (cardsInPlay cS) (list cardsInPlay cS)))

(define emptyGameArea (list null null))

(define getCardsInPlay (lambda (gA) (car gA)))

(define getCardsSet (lambda (gA) (cadr gA)))

(define setCardsInPlay (lambda (gA cSP) (gameArea cSP (getCardsSet gA))))

(define setCardsSet (lambda (gA cS) (gameArea (getCardsInPlay gA) cS)))
