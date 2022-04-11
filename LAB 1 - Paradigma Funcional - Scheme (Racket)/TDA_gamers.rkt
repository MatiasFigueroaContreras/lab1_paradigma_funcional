#lang racket
(provide (all-defined-out))

(define initGamers null)

(define emptyGamers? (lambda (gamers)
                       (if (null? gamers) #t #f)))

(define firstGamer (lambda (gamers) (car gamers)))

(define nextGamers (lambda (gamers) (cdr gamers)))

(define insertGamer (lambda (gamer gamers) (cons gamer gamers)))

(define gamer=? (lambda (g1 g2) (string=? g1 g2)))

(define isGamer? (lambda (nickname gamers)
                   (ormap (lambda (gamer) (if (gamer=? nickname gamer) #t #f)) gamers)))

(define registerGamer (lambda (nickname gamers)
                        (if (isGamer? nickname gamers)
                            gamers
                            (if (emptyGamers? gamers)
                                (insertGamer nickname null)
                                (insertGamer (firstGamer gamers) (registerGamer nickname (nextGamers gamers)))))))

(define nthGamer (lambda (gamers n)
                   (if (null? gamers)
                       null
                       (if (= n 1)
                           (firstGamer gamers)
                           (nthGamer (nextGamers gamers) (- n 1))))))

(define totalGamers (lambda (gamers)
                      (apply + (map (lambda (x) 1) gamers))))   

(define gamerPos (lambda (nickname gamers)
                   (define recursion (lambda (pos gamers)
                                           (if (gamer=? nickname (firstGamer gamers))
                                               pos
                                               (recursion (+ pos 1) (nextGamers gamers)))))
                   (if (isGamer? nickname gamers)
                       (recursion 1 gamers)
                       -1)))

(define gamers (registerGamer "gamer4"(registerGamer "gamer3" (registerGamer  "gamer2"(registerGamer "gamer1" initGamers)))))