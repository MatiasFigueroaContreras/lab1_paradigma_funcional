#lang racket
(provide (all-defined-out))

(define setGame (lambda (gamersInfo gameArea status mode rndfn) (list gamersInfo gameArea status mode rndfn)))

(define getGamersInfo (lambda (gm) (car gm)))

(define getGameArea (lambda (gm) (cadr gm)))

(define getMode (lambda (gm) (cadddr gm)))

(define getRandomFn (lambda (gm) (car (cddddr gm))))
