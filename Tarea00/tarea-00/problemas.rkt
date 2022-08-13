#lang racket
;; 1.
(define pi 3.14)

;; 2.
(define (area-circle r)
  (* pi (* r r)))

;; 3.
(define (circle-properties r)
  (list (* pi (* r r)) (* pi (* r 2))))

;; 4.
(define (rectangle-properties rec)
  (define largo (list-ref rec 0)) (define ancho (list-ref rec 1))
  (list (* largo ancho) (* 2(+ largo ancho))))
;; 5.
(define (find-needle* ls num)
 (if (null? ls)
     -1
     (if(eq?(first ls)'needle)
        num
        (find-needle* (rest ls) (+ 1 num)))))
  (define (find-needle ls)
    (find-needle* ls 0))
;; Problema 6

(define (abs x)
  (cond ((>= x 0) x)
        (else (- x))))

;; Problema 7
(define (inclis1 ls)
  (map (lambda (x) (+ 1 x))ls))

;; Problema 8
(define (even? x)
  (integer? (/ x 2)))

;; Problema 9
(define another-add
  (lambda (n m)
    (cond
         [(zero? n) m]
         [else (add1 (another-add (sub1 n) m ))])))

(provide (all-defined-out))

