#lang plait

(define-type ArithC
  [numC (num : Number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (num : Number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [negS (e : ArithS)]
  [minuS (l : ArithS) (r : ArithS)])


(define (interp [a : ArithC]) : Number
  (type-case ArithC a
    [(numC n) (n)]
    [(plusC l r) (+ (interp (plusC-l a)) (interp (plusC-r a)))]
    [(multC l r) (* (interp (multC-l a)) (interp (multC-r a)))]))

(define (desugar [s : ArithS]) : ArithC
  (type-case ArithS s
    [(numS n) (numC n)]
    [(plusS l r) (plusC (desugar l) (desugar r))]
    [(multS l r) (multC (desugar l) (desugar r))]
    [(minuS l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [(negS x) (multC (numC -1) (desugar x))]))

(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))


(define (parse [s : S-Exp]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(and (s-exp-list? s) (not (equal? s `{})))
     (let ([lst (s-exp->list s)])
       (cond
         [(not (s-exp-symbol? (first lst))) (error 'parse "operacion aritmetica no es prefix.")]
         [(= (length lst) 2)
          (case (s-exp->symbol (first lst))
            ['- (minuS (parse (second lst)))])]
         [(= (length lst) 3)
          (case (s-exp->symbol (first lst))
            ['+ (plusS (parse (second lst)) (parse (third lst)))]
            ['* (multS (parse (second lst)) (parse (third lst)))]
            ['- (negS (parse (second lst)) (parse (third lst)))]
            [else (error 'parse "no se reconoce el simbolo en la operacion aritmetica.")])]
         [else (error 'parse "operacion aritmetica malformada.")]))]
     [else (error 'parse "expresion aritmetica malformada.")]))