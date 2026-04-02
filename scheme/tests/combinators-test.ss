(import (rnrs)
        (src combinators))

(define pass-count 0)
(define fail-count 0)

(define (assert-equal name expected actual)
  "Test assertion: checks equality and reports result."
  (if (equal? expected actual)
      (begin
        (set! pass-count (+ pass-count 1))
        (display "  PASS: ")
        (display name)
        (newline))
      (begin
        (set! fail-count (+ fail-count 1))
        (display "  FAIL: ")
        (display name)
        (display " — expected ")
        (display expected)
        (display ", got ")
        (display actual)
        (newline))))

(define (report)
  "Prints final test summary and exits with appropriate code."
  (newline)
  (display pass-count)
  (display " passed, ")
  (display fail-count)
  (display " failed")
  (newline)
  (when (> fail-count 0) (exit 1)))

(define (square x) (* x x))
(define (add1 x) (+ x 1))
(define (double x) (* x 2))


(display "=== 04-backus: identity and constant ===")
(newline)

(assert-equal
 "04-backus-identity"
 42 (fp-identity 42))

(assert-equal
 "04-backus-constant"
 'always ((fp-constant 'always) 'ignored))


(display "=== 04-backus: compose ===")
(newline)

(assert-equal
 "04-backus-compose-two"
 5 ((fp-compose add1 double) 2))

(assert-equal
 "04-backus-compose-three"
 10 ((fp-compose double add1 double) 2))

(assert-equal
 "04-backus-compose-identity-law"
 7 ((fp-compose fp-identity add1) 6))

(assert-equal
 "04-backus-compose-associativity"
 (let ((f add1) (g double) (h square))
   ((fp-compose (fp-compose f g) h) 3))
 (let ((f add1) (g double) (h square))
   ((fp-compose f (fp-compose g h)) 3)))


(display "=== 04-backus: construct ===")
(newline)

(assert-equal
 "04-backus-construct-two"
 '(4 3) ((fp-construct square add1) 2))

(assert-equal
 "04-backus-construct-three"
 '(9 6 4) ((fp-construct square double add1) 3))


(display "=== 04-backus: insert (reduce) ===")
(newline)

(assert-equal
 "04-backus-insert-sum"
 10 ((fp-insert +) '(1 2 3 4)))

(assert-equal
 "04-backus-insert-product"
 24 ((fp-insert *) '(1 2 3 4)))

(assert-equal
 "04-backus-insert-singleton"
 5 ((fp-insert +) '(5)))


(display "=== 04-backus: apply-to-all (map) ===")
(newline)

(assert-equal
 "04-backus-apply-to-all-square"
 '(1 4 9) ((fp-apply-to-all square) '(1 2 3)))

(assert-equal
 "04-backus-apply-to-all-empty"
 '() ((fp-apply-to-all square) '()))


(display "=== 04-backus: combined — insert + apply-to-all ===")
(newline)

(assert-equal
 "04-backus-insert-apply-to-all-sum-of-squares"
 14 ((fp-insert +)
     ((fp-apply-to-all square) '(1 2 3))))

(assert-equal
 "04-backus-composed-pipeline"
 14 ((fp-compose (fp-insert +) (fp-apply-to-all square))
     '(1 2 3)))


(display "=== 04-backus: map fusion law ===")
(newline)

(let ((xs '(1 2 3 4 5))
      (f add1)
      (g double))
  (assert-equal
   "04-backus-map-fusion"
   ((fp-apply-to-all (fp-compose f g)) xs)
   ((fp-compose (fp-apply-to-all f)
                (fp-apply-to-all g)) xs)))


(display "=== 04-backus: distl, distr ===")
(newline)

(assert-equal
 "04-backus-distl"
 '((a . 1) (a . 2) (a . 3))
 (fp-distl '(a . (1 2 3))))

(assert-equal
 "04-backus-distr"
 '((1 . z) (2 . z) (3 . z))
 (fp-distr '((1 2 3) . z)))


(display "=== 04-backus: transpose ===")
(newline)

(assert-equal
 "04-backus-transpose"
 '((1 4) (2 5) (3 6))
 (fp-transpose '((1 2 3) (4 5 6))))

(assert-equal
 "04-backus-transpose-identity"
 '((1 2 3) (4 5 6))
 (fp-transpose (fp-transpose '((1 2 3) (4 5 6)))))


(report)
