#lang racket
(provide (all-defined-out))

(define emptyGamersPoints null)

(define gamersPoints (lambda (numGamers)
                      (define recursion (lambda (nG gP)
                                          (if (= nG 0)
                                              gP
                                              (recursion (- nG 1) (cons 0 gP)))))
                      (recursion numGamers null)))

(define emptyGamersPoints? (lambda (gP) (null? gP)))

(define firstPoint (lambda (gP) (car gP)))

(define nextPoints (lambda (gP) (cdr gP)))

(define insertGamerPoints (lambda (p gP) (cons p gP)))

(define registerNewGamerPoint (lambda (gP)
                                (if (emptyGamersPoints? gP)
                                (insertGamerPoints 0 null)
                                (insertGamerPoints (firstPoint gP) (registerNewGamerPoint (nextPoints gP))))))

(define nthPoint (lambda (gP n)
                   (if (null? gP)
                       -1
                       (if (= n 1)
                           (firstPoint gP)
                           (nthPoint (nextPoints gP) (- n 1))))))

(define addPoints (lambda (gP p n)
                    (if (or (< n 1) (null? gP))
                        gP
                        (if (= n 1)
                            (insertGamerPoints (+ p (firstPoint gP)) (nextPoints gP))
                            (insertGamerPoints (firstPoint gP) (addPoints (nextPoints gP) p (- n 1)))))))

