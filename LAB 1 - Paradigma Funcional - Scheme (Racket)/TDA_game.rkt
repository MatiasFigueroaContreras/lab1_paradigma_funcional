#lang racket
(provide (all-defined-out))

(define setGame (lambda (gamersInfo cS status mode rndfn) (list gamersInfo cS status mode rndfn)))

(define getGamersInfo (lambda (gm) (car gm)))

(define getCardsSet (lambda (gm) (cadr gm)))

(define getMode (lambda (gm) (cadddr gm)))

(define getRandomFn (lambda (gm) (car (cddddr gm))))

