#lang racket
(define countdown*
  (lambda (i f)
    (if(<= i 0)
            (list i)
            (cons i(countdown* (- i 1) f)))))

(define (countdown num)
  (countdown* num 0)
  )


(define (insertL*  b c list)
 (if(eq? (list-ref list c) b)   
    (delete-at c list)
    (insertL*  b (+ 1 c) list)
    )
  )
(define(insertL  b list)
  (insertL*  b 0 list))
(define (delete-at k lst)
  (cond ((null? lst)
         '())
        ((zero? k)
         (cdr lst))
        (else
         (cons (car lst)
               (delete-at (sub1 k) (cdr lst))))))


(define (map op list)
  (if (empty? list)
    '()
    (cons (op (first list)) (map op (rest list)))))

(define (filter op list)
  (cond ((empty? list) empty)
        ((op (first list))
         (cons (first list) (filter op (rest list))))
        (else (filter op (rest list)))))

(define (List-index-ofv*  b c list)
 (if(eq? (list-ref list c) b)   
    c
    (List-index-ofv*  b (+ 1 c) list)
    )
  )
(define(List-index-ofv b list)
  (List-index-ofv*  b 0 list))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) list2))))


(define (reverse lst)
 [if (null? lst) null [append (reverse (rest lst))(list [first lst])]])
