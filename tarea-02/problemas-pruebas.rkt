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
             (check-equal? (partition ":)" 3) '(":)"))
             (check-equal? (partition "Venecos" 3) '("Ven" "eco" "s")))
  
 (test-case "smallers-filter"
             (check-equal? (smallers-filter '(5 5 5 5) 4) '())
             (check-equal? (smallers-filter '(5 5 5 5) 6) '(5 5 5 5)))
  
  (test-case "largers-filter"
             (check-equal? (largers-filter '(5 5 5 5) 6) '())
             (check-equal? (largers-filter '(5 5 5 5) 4) '(5 5 5 5)))
  
  (test-case "quicksort"
    (check-equal? (quicksort '(1 2 5 2 3 6 7)) '(1 2 2 3 5 6 7))
    (check-equal? (quicksort '(1 4 5 8 3 2 9)) '(1 2 3 4 5 8 9))
  )

 (test-case "g-quicksort"
             (check-equal? (g-quicksort '(2 5 6 4 8 4 5 2 18 4) (lambda (x y) (< x y)))
                  '(2 2 4 4 4 5 5 6 8 18))
             (check-equal? (g-quicksort '(2 5 6 4 8 4 5 2 18 4) (lambda (x y) (> x y)))
                  '(18 8 6 5 5 4 4 4 2 2))
             (check-equal? (g-quicksort '("aa" "a" "aaa" "aaaa" "aaaaaaaa" "aa") (lambda (x y) (> (string-length x) (string-length y))))
                  '("aaaaaaaa" "aaaa" "aaa" "aa" "aa" "a"))
             (check-equal? (g-quicksort '("aa" "a" "aaa" "aaaa" "aaaaaaaa" "aa") (lambda (x y) (< (string-length x) (string-length y))))
                  '("a" "aa" "aa" "aaa" "aaaa" "aaaaaaaa")))
)
  
(run-tests pruebas 'verbose)