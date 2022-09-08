#lang racket
(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
 (test-case "bundle"
             (check-equal? (bundle (explode "abcdefg") 3)
                           (list "abc" "def" "g"))
             (check-equal? (bundle (explode "abcdefgh") 2)
                           (list "ab" "cd" "ef" "gh"))
             (check-equal? (bundle (explode "abcdefgh") 1)
                           (list "a" "b" "c" "d" "e" "f" "g" "h"))
             (check-equal? (bundle '() 2)
                           '())
             (check-equal? (bundle '("a" "b") 3)
                           (list "ab"))
             (check-exn exn:fail? (thunk (bundle '("") 3)))
             (check-exn exn:fail? (thunk (bundle (explode "abcdefgh") 0))))

 
  (test-case "take"
             (check-equal? (take '(1 2 3 4 5 6) 2)
                           '(1 2))
             (check-equal? (take '("a" "b" "c" "d" "e") 3)
                           '("a" "b" "c"))
             (check-equal? (take '() 3)
                           '())
             (check-equal? (take '(1 2) 7)
                           '(1 2)))

(test-case "drop"
             (check-equal? (drop '(1 2 3 4 5 6) 3)
                           '(4 5 6))
             (check-equal? (drop '("a" "b" "c" "d" "e") 3)
                           '("d" "e"))
             (check-equal? (drop '() 3)
                           '())
             (check-equal? (drop '(1 2) 3)
                           '()))
 (test-case "list->chunks"
             (check-equal? (list->chunks '() 0) '())
             (check-equal? (list->chunks '(2 4 6 8 10 12) 2) '((2 4) (6 8) (10 12)))
             (check-equal? (list->chunks '(2 8 76 89 23 12) 0) '())
             (check-equal? (list->chunks '(2 4 6 8 10 12 14) 3) '((2 4 6) (8 10 12) (14)))
             )

   (test-case "bundle-chunk"
             (check-equal? (bundle-chunk (explode "abcdefg") 3)
                           (list "abc" "def" "g"))
             (check-equal? (bundle-chunk (explode "abcdefgh") 2)
                           (list "ab" "cd" "ef" "gh"))
             (check-equal? (bundle-chunk (explode "abcdefgh") 1)
                           (list "a" "b" "c" "d" "e" "f" "g" "h"))
             (check-equal? (bundle-chunk '() 2)
                           '())
             (check-equal? (bundle-chunk '("a" "b") 3)
                           (list "ab")))

  (test-case "partition"
             (check-equal? (partition "" 3) '())
             (check-equal? (partition "Benito Perez" 0) '())
             (check-equal? (partition "Veneco" 2) '("Ve" "ne" "co"))
             (check-equal? (partition ":)" 3) '(":)")))
             (check-equal? (partition "Venecos" 3) '("Ven" "eco" "s"))

  )
(run-tests pruebas 'verbose)