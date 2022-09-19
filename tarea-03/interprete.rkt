#lang plait

#|
 |||||||||||||||||||||||||||||||||
 ||||||EVALUADOR NO TOCAR|||||||||
 |||||||||||||||||||||||||||||||||
|#
(define (eval [str : S-Exp]) : Value
(interp (desugar (parse str))))
#|
 EXPRESIONES DE TIPOS DEL LENGUAJE
|#
(define-type Value
  (numV [value : Number])
  (strV [value : String])
  (boolV [value : Boolean])
  (funV [param : Symbol] [body : ExprC]))
#|
 OPERATORS
|#
(define-type Operator
  (plus0)
  (append0)
  (numeq0)
  (streq0))

(define-type ExprC
  (numC [value : Number])
  (strC [value : String])
  (boolC [value : Boolean])
  (idC [name : Symbol])
  (ifC [a : ExprC] [b : ExprC] [c : ExprC])
  (binopC [op : Operator] [left : ExprC] [right : ExprC])
  (funC [param : Symbol] [body : ExprC])
  (appC [func : ExprC] [arg : ExprC]))

(define-type ExprS
  (numS [value : Number])
  (strS [value : String])
  (boolS [value : Boolean])
  (idS [name : Symbol])
  (ifS [a : ExprS] [b : ExprS] [c : ExprS])
  (andS [left : ExprS] [right : ExprS])
  (orS [left : ExprS] [right : ExprS])
  (binopS [op : Operator] [left : ExprS] [right : ExprS])
  (funS [param : Symbol] [body : ExprS])
  (letS [name : Symbol] [value : ExprS] [body : ExprS])
  (appS [func : ExprS] [arg : ExprS]))
;;;;;;;;;;;;;
;; DESUGAR ;;
;;;;;;;;;;;;;

(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [(numS value) (numC value)]
    [(strS value) (strC value)]
    [(boolS value) (boolC value)]
    [(idS name) (idC name)]
    [(ifS a b c) (ifC (desugar a) (desugar b) (desugar c))]
    [(andS left right) (ifC (desugar left) (boolC #t) (desugar right))]
    [(orS left right) (ifC (desugar left) (boolC #t) (desugar right))]
    [(binopS op left right)
     (binopC op (desugar left) (desugar right))]
    [(funS param body) (funC param (desugar body))]
    [(letS name value body) (appC (funC name (desugar body)) (desugar value))]
    [(appS func arg) (appC (desugar func) (desugar arg))]))


;;;;;;;;;;;;
;; INTERP ;;
;;;;;;;;;;;;

(define (interp [e : ExprC]) : Value
  (type-case ExprC e
    [(numC value) (numV value)]
    [(strC value) (strV value)]
    [(boolC value) (boolV value)]
    [(idC name) (numV 0)]
    [(ifC a b c) (numV 0)]
    [(binopC op left right)
     (let ([left (interp left)])
       (let ([right (interp right)])
         (interp-binop op left right)))]
    [(funC param body) (numV 0)]
    [(appC func arg) (numV 0)]))

(define (interp-binop [op : Operator]
                      [left : Value]
                      [right : Value]) : Value
  (type-case Operator op
    [(plusO)
     (if (numV? left)
         (if (numV? right)
             (numV (+ (numV-value left)
                      (numV-value right)))
             (error 'binop "upsi dupsi deisy"))
         (error 'binop "upsi deisy dupsi"))]
    [(appendO) ....]
    [(numeqO) ....]
    [(streqO) ....]))
#|PARSE|#
(define (parse [in : S-Exp]) : ExprS
(cond
[(s-exp-number? in) (parse-number in)]
[(s-exp-string? in) (parse-string in)]
[(s-exp-match? `true in) (boolS #t)]
[(s-exp-match? `false in) (boolS #f)]
[(s-exp-match? `{if ANY ...} in) (parse-if in)]
[(s-exp-match? `{and ANY ...} in) (parse-and in)]
[(s-exp-match? `{or ANY ...} in) (parse-or in)]
[(s-exp-match? `{+ ANY ...} in) (parse-+ in)]
[(s-exp-match? `{++ ANY ...} in) (parse-++ in)]
[(s-exp-match? `{num= ANY ...} in) (parse-num= in)]
[(s-exp-match? `{str= ANY ...} in) (parse-str= in)]
[(s-exp-match? `{fun ANY ...} in) (parse-fun in)]
[(s-exp-match? `{let {SYMBOL ANY} ANY ...} in) (parse-let in)]
[(s-exp-match? `{ANY ...} in) (parse-app in)]
[(s-exp-symbol? in) (parse-id in)]))

#|FUNCTIONS PARSE ()|#
(define (parse-number in)
(numS (s-exp->number in)))

(define (parse-string in)
(strS (s-exp->string in)))

(define (parse-id in)
(idS (s-exp->symbol in)))

(define (parse-if in)
(let ([inlst (s-exp->list in)])
(if (equal? (length inlst) 4)
(ifS (parse (second inlst))
(parse (third inlst))
(parse (fourth inlst)))
(error 'parse ”cantidad incorrecta de argumentos para if”))))

(define (parse-and in)
(let ([inlst (s-exp->list in)])
(if (equal? (length inlst) 3)
(andS (parse (second inlst)) (parse (third inlst)))
(error 'parse ”cantidad incorrecta de argumentos para and”))))

(define (parse-or in)
(let ([inlst (s-exp->list in)])
(if (equal? (length inlst) 3)
(orS (parse (second inlst)) (parse (third inlst)))
(error 'parse ”cantidad incorrecta de argumentos para or”))))

(define (parse-+ in)
(let ([inlst (s-exp->list in)])
(if (equal? (length inlst) 3)
(binopS (plusO) (parse (second inlst)) (parse (third inlst)))
(error 'parse ”cantidad incorrecta de argumentos para +”))))

(define (parse-++ in)
(let ([inlst (s-exp->list in)])
(if (equal? (length inlst) 3)
(binopS (appendO) (parse (second inlst)) (parse (third inlst)))
(error 'parse ”cantidad incorrecta de argumentos para ++”))))

(define (parse-num= in)
(let ([inlst (s-exp->list in)])
(if (equal? (length inlst) 3)
(binopS (numeqO) (parse (second inlst)) (parse (third inlst)))
(error 'parse ”cantidad incorrecta de argumentos para num=”))))

(define (parse-str= in)
(let ([inlst (s-exp->list in)])
(if (equal? (length inlst) 3)
(binopS (streqO) (parse (second inlst)) (parse (third inlst)))
(error 'parse ”cantidad incorrecta de argumentos para str=”))))

(define (parse-fun in)
(cond
[(s-exp-match? `{fun SYMBOL ANY ...} in)
(let ([inlst (s-exp->list in)])
(if (equal? (length inlst) 3)
(funS (s-exp->symbol (second inlst)) (parse (third inlst)))
(error 'parse ”funciones deben tener solo un cuerpo”)))]
[(s-exp-match? `{fun ANY ...} in)
(error 'parse ”parametros a función deben ser símbolos”)]))

(define (parse-let in)
(let ([inlst (s-exp->list in)])
(if (equal? (length inlst) 3)
(letS
(s-exp->symbol (first (s-exp->list (second inlst))))
(parse (second (s-exp->list (second inlst))))
(parse (third inlst)))
(error 'parse ”cantidad incorrecta de argumentos para let”))))

(define (parse-app in)
(let ([inlst (s-exp->list in)])
(if (equal? (length inlst) 2)
(appS (parse (first inlst)) (parse (second inlst)))
(error 'parse ”cantidad incorrecta de argumentos en aplicación de
1! funciones”))))