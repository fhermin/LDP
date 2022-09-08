#lang racket

(define (unit-string? x)
  (and (string? x)
       (= (string-length x) 1)))

(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (first x))
           (= (string-length (first x)) 1)
           (unit-string-list? (rest x)))))

(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

(define (bundle s n)
  (cond
    [(and (> n 0) (list? s))
    (cond
      [(or (null? s) (or (zero? n) (< n 0))) s]
      [else
       (cons (implode (take s n))
             (bundle (drop s n) n))])]
     [else (error 'bundle "n es menor que cero o no pasaste una lista")]))

#|PROBLEMA 3
1. Implementa el procedimiento (take l n) la cuál regresa una lista de los primeros n elementos
de l, o cuantos elementos haya si l tiene menos de n elementos.
2. Implementa el procedimiento (drop l n) la cuál regresa una lista con los elementos de l
excepto los primeros n, o la cadena vacía si l tiene menos de n elementos.|#
(define (take l n)
(cond
[(>= n (length l)) l]
[(= n 0) '()]
[else  (cons (first l) (take (rest l) (- n 1)))]))

(define (drop l n)
(cond
[(>= n (length l)) '()]
[(= n 0) (cons (first l) (drop (rest l) n))]
[else  (drop (rest l) (- n 1))]))
#|PROBLEMA 6
Define el procedimiento list->chunks. Consume una lista l de valores arbitrarios y un
natural n. El resultado es una lista de trozos de tamaño n. Cada trozo representa una sub-secuencia
de elementos en l. Implementa bundle usando list->chunks.
|#
(define (list->chunks ls n)
  (cond
    [(or(empty? ls) (zero? n)) null]
    [else (cons (take ls n) (list->chunks (drop ls n) n))]))

(define (bundle-chunk s n)
 (cond
   [(or (null? s) (or (zero? n) (< n 0))) s]
   [else (let add ([ls (list->chunks s n)])
           (cond
             [(empty? ls) null]
             [else (cons (implode (first ls)) (add (rest ls)))]))]))
#|PROBLEMA 7
Define partition. Toma una cadena s y un natural n. Produce una lista de trozos de
cadenas de tamaño n. Para cadenas no vacías s y enteros positivos n se tiene que
|#

(define (partition s n)
  (cond
    [(<= n 0) null]
    [(> n 0) (if(> n (string-length s))
          (if (eq? (string-length s) 0)
              null
              (list s))
          (cons (substring s 0 n) (partition (substring s n) n)))]
    ))
#|2.2 Recursión que ignora estructura
PROBLEMA 8
Modifica esos procedimientos para poder ordenar en orden ascendente cualquier tipo
de valor a partir de un argumento adicional.
|#
(define (isort ls)
  (if(empty? ls)
     null
     (insert (first ls)(isort (rest ls)))))

(define (insert n ls)
  (cond
    [(empty? ls) (list n)]
    [(<= n (first ls)) (cons n ls)]
    [else (cons (first ls) (insert n (rest ls)))]))

#|PROBLEMA 10
Implementa los procedimientos smallers y largers, son recursivos estructurales.
Uno debe regresar los elementos que son estríctamente menores al pivote y otro debe regresar los
elementos que son estríctamente mayores al pivote.
|#
(define (smallers ls n)
  (if (empty? ls)
      null
      (cond[(> (first ls) n) (smallers (rest ls) n)]
           [(eq? (first ls) n) (smallers (rest ls) n)]
          [else (cons (first ls) (smallers (rest ls) n))])))

(define (largers ls n)
  (if (empty? ls)
      null
      (cond [(> n (first ls)) (largers (rest ls) n)]
            [(eq? (first ls) n) (largers (rest ls) n)]
          [else (cons (first ls) (largers (rest ls) n))])))
#|PROBLEMA 11
|#
(define (quicksort ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
       (append (quicksort (smallers ls pivot))
               (eq ls pivot)
               (quicksort (largers ls pivot)))]))
(define (eq ls n)
  (if (null? ls)
      null
      (cond
          [(eq? (first ls) n) (cons n (eq (rest ls) n))]
          [else (eq (rest ls) n)])))

#|PROBLEMA 12|#
(define (g-quicksort ls n)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
       (append (g-quicksort (filter (lambda (x) (n x pivot)) ls) n)
               (filter (lambda (x) (equal? x pivot)) ls)
               (g-quicksort (filter (lambda (x) (and (not (n x pivot)) (not (equal? x pivot)))) ls) n))]))
#|PROBLEMA 13|#
(define (iquicksort ls)
  (cond
    [(empty? ls) null]
    [(>= 100 (length ls)) (isort ls)]
    [else
     (define pivot (first ls))
       (append (quicksort (smallers ls pivot))
               (eq ls pivot)
               (quicksort (largers ls pivot)))]))
#|PROBLEMA 14|#
(define (smallers-filter ls n)
  (filter (lambda (x) (> n x)) ls))

(define (largers-filter ls n)
  (filter (lambda (x) (> x n)) ls))



(provide (all-defined-out))