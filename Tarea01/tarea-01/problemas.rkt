#lang racket
;;Problema 1
(define countdown*
  (lambda (i f)
    (if(<= i f)
            (list i)
            (cons i(countdown* (- i 1) f)))))

(define (countdown num)
  (countdown* num 0)
  )
;;Problema 2
(define (insertL x y ls)
  (if (empty? ls)
      null
      (if(eq? (first ls) x)
         (cons y (cons x(insertL 'x 'y(rest ls))))
         (cons(first ls)(insertL 'x 'y(rest ls))))))

;;Problema 3
(define (remv-1st*  b c list)
  (if(eq? (length list) c)
     list
 (if(eq? (list-ref list c) b)   
    (delete-at c list)
    (remv-1st*  b (+ 1 c) list)
    )
  ))
(define(remv-1st  b list)
  (remv-1st*  b 0 list))

(define (delete-at k lst)
  (cond ((null? lst)
         '())
        ((zero? k)
         (cdr lst))
        (else
         (cons (car lst)
               (delete-at (sub1 k) (cdr lst))))))

;;Problema 4
(define (map op list)
  (if (empty? list)
    '()
    (cons (op (first list)) (map op (rest list)))))
;;Problema 5
(define (filter op list)
  (cond ((empty? list) empty)
        ((op (first list))
         (cons (first list) (filter op (rest list))))
        (else (filter op (rest list)))))
;;Problema 6
(define (zip list list2)
  (if (or (null? list) (null? list2))
      null
      (cons (cons (first list) (first list2)) (zip (rest list) (rest list2)))))
;;Problema 7
(define (List-index-ofv*  b c list)
 (if(eq? (list-ref list c) b)   
    c
    (List-index-ofv*  b (+ 1 c) list)
    )
  )
(define(list-index-ofv b list)
  (List-index-ofv*  b 0 list))
;;Problema 8
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) list2))))

;;Problema 9
(define (reverse lst)
 [if (null? lst) null [append (reverse (rest lst))(list [first lst])]])

;;Problema 10
(define (repeat list num)
  (if (<= num 1)
     list
     (append list (repeat list (- num 1)))
      )
  )
;;Problema 11
(define (same-lists* list list2 )
(if(and (null? list) (null? list2))
  #t
  (if(or (null? list) (null? list2))
     #f
   (if (equal? (first list) (first list2))
       (same-lists* (rest list) (rest list2))
       #f
     )
   ))
  )
;;Problema 12
#|
   ((w x) (z))
   ((w (x.())) (z.())
   ((w.(x.())).(z.()))
|#
;;Problema 13
(define (binary->natural list)
  (if (null? list)
      0
      (+ (car list) (* 2 (binary->natural (cdr list))))))

;;Problema 14
(define (div* a b num)
  (if (eq? (modulo a b) 0)
     (if(eq? (* b num) a)
      num
     (div* a b (+ 1 num)))
     #f)
  )
(define(div a b)
  (div* a b 0))
;;Problema 15
(define append-map (lambda (pro list)
   (if(null? list)
      list
      (append(pro(first list))(append-map pro(rest list)))
                     )))
;;Problema 16
(define set-difference
  (lambda (list list2)
    (if (empty? list)
        list
        (if(set-difference* (car list) list2)
           (set-difference(cdr list)list2)
            (cons(car list)(set-difference(rest list)list2))
 ))))
(define(set-difference* frst lst)
  (if (empty? lst)
      #f
      (if (eq? frst (car lst))
          #t
          (set-difference* frst (rest lst)))))
;;Problema 17
(define (foldr op bin list)
  (if (null? list)
      bin
      (op(car list)(foldr op bin(cdr list)))))
;;Problema 18
(define(powerset ls)
  (if (empty? ls)
      (list ls)
      (let([ps(powerset(rest ls))])
        (append(p*(first ls)ps)ps))))
(define (p* x ls)
  (if(null? ls)
     null
     (cons(cons x (car ls))
          (p* x (cdr ls)))))
;;Problema 19
(define (cartesian-product lst1 lst2)
  (append
         (map (lambda (x)
                (map (lambda (y)
                       (vector x y))
                     lst2))
              lst1)))

;;Problema 20
(define (insertL-fr x y ls)
  (foldr append(insertL x y ls)      
  '()))

(define (filter-fr pro lst)
  (foldr (lambda (x y)
           (if (pro x)
               (cons x y)
               y))
         '()
         lst))

(define (map-fr pro lst)      
     (foldr (lambda (x y)
              (cons (pro x) y))
            '()
              lst))

(define(append-fr ls ls2)
  (foldr append(append ls ls2)
         '())
  )

(define (reverse-fr ls)
  (foldr append ( reverse ls) '() ))

(define(binary->natural-fr ls)
  (foldr (lambda (x y)
           (+ x (* 2 y)))
         0
         ls))

(define(append-map-fr pro pro2)
  (foldr (lambda (x y)
           (append(pro x)y))
         '()
          pro2))

(define (set-difference-fr l l2)
  (foldr set-difference
         (set-difference l l2)
         '()
         ))

(define (powerset-fr ls)
  (foldr append
         (powerset ls)
         '()
         ))

;;Problema 21
(define snowball
  (letrec
      ((odd-case
        (lambda (fix-odd)
          (lambda (x)
            (cond
              ((and (exact-integer? x) (positive? x) (odd? x))
               (snowball (add1 (* x 3))))
              (else (fix-odd x))))))
       (even-case
        (lambda (fix-even)
          (lambda (x)
            (cond
              ((and (exact-integer? x) (positive? x) (even? x))
               (snowball (/ x 2)))
              (else (fix-even x))))))
       (one-case
        (lambda (fix-one)
          (lambda (x)
            (cond
              ((zero? (sub1 x)) 1)
              (else (fix-one x))))))
       (base
        (lambda (x)
          (error 'error "Invalid value ~s~n" x))))
    (one-case(even-case(odd-case base)))))




(provide (all-defined-out))