#lang plait
(require "arithlang.rkt")


(module+ test
  ;;Errores
  (test/exn (eval `()) "expresion aritmetica malformada")
  (test/exn (eval `(+)) "operacion aritmetica malformada")
  (test/exn (eval `(-)) "operacion aritmetica malformada")
  (test/exn (eval `(*)) "operacion aritmetica malformada")
  (test/exn (eval `(n)) "operacion aritmetica malformada")
  (test/exn (eval `(+ 2 5 6)) "operacion aritmetica malformada")
  (test/exn (eval `(- 2 5 6)) "operacion aritmetica malformada")
  (test/exn (eval `(* 2 5 6)) "operacion aritmetica malformada")
  (test/exn (eval `(n 5 6)) "no se reconoce el simbolo en la operacion aritmetica.")
  ;;Suma y Resta
  (test (eval `(+ 2 5)) 7)
  (test (eval `(- 2 5)) -3)
  (test (eval `(* 2 5)) 10)
  (test (eval `(+ 1 1)) 2)
  (test (eval `(+ (+ 1 3) 4)) 8)
  ;;Multiplicación
  (test (eval `(* 1 2)) 2)
  (test (eval `(* -1 2)) -2)
  (test (eval `(* 5 6)) 30)
  (test (eval `(* (* 5 8) 4)) 160)
  (test (eval `(* -2.5 3)) -7.5)
  ;;Restas
  (test (eval `(- 1 1)) 0)
  (test (eval `(- 3 2)) 1)
  ;;Negativos
  (test (eval `(- 6)) -6)
  (test (eval `(- 0)) 0)
  )
  
  