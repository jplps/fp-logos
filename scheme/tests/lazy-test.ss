(import (rnrs)
        (src lazy))

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
  (newline)
  (display pass-count)
  (display " passed, ")
  (display fail-count)
  (display " failed")
  (newline)
  (when (> fail-count 0) (exit 1)))


(display "=== 06-sicp: stream basics ===")
(newline)

(assert-equal
 "06-sicp-stream-null"
 #t (stream-null? stream-null))

(assert-equal
 "06-sicp-stream-cons-car"
 1 (stream-car (stream-cons 1 stream-null)))

(assert-equal
 "06-sicp-stream-cons-cdr-null"
 #t (stream-null? (stream-cdr (stream-cons 1 stream-null))))

(assert-equal
 "06-sicp-stream-take-3"
 '(1 2 3) (stream-take 3 (stream-cons 1 (stream-cons 2
            (stream-cons 3 stream-null)))))


(display "=== 06-sicp: infinite streams ===")
(newline)

(define nats (stream-from 0))

(assert-equal
 "06-sicp-nats-take-5"
 '(0 1 2 3 4) (stream-take 5 nats))

(assert-equal
 "06-sicp-stream-ref-100"
 100 (stream-ref nats 100))


(display "=== 06-sicp: stream-map ===")
(newline)

(define squares (stream-map (lambda (x) (* x x)) nats))

(assert-equal
 "06-sicp-stream-map-squares"
 '(0 1 4 9 16) (stream-take 5 squares))

(define doubled-nats (stream-map + nats nats))

(assert-equal
 "06-sicp-stream-map-two-streams"
 '(0 2 4 6 8) (stream-take 5 doubled-nats))


(display "=== 06-sicp: stream-filter ===")
(newline)

(define evens
  (stream-filter (lambda (x) (= (mod x 2) 0))
                 (stream-from 1)))

(assert-equal
 "06-sicp-stream-filter-evens"
 '(2 4 6 8 10) (stream-take 5 evens))


(display "=== 06-sicp: sieve of eratosthenes ===")
(newline)

(define (sieve s)
  "Sieve of Eratosthenes on an infinite stream."
  (let ((p (stream-car s)))
    (stream-cons
     p
     (sieve (stream-filter
             (lambda (x) (not (= (mod x p) 0)))
             (stream-cdr s))))))

(define primes (sieve (stream-from 2)))

(assert-equal
 "06-sicp-primes-first-10"
 '(2 3 5 7 11 13 17 19 23 29)
 (stream-take 10 primes))

(assert-equal
 "06-sicp-prime-100th"
 547 (stream-ref primes 100))


(display "=== 06-sicp: stream-zip ===")
(newline)

(assert-equal
 "06-sicp-stream-zip"
 '((0 . a) (1 . b) (2 . c))
 (stream-take 3
  (stream-zip nats
              (stream-cons 'a (stream-cons 'b
               (stream-cons 'c stream-null))))))


(display "=== 06-sicp: stream-append ===")
(newline)

(define s1
  (stream-cons 1 (stream-cons 2 stream-null)))
(define s2
  (stream-cons 3 (stream-cons 4 stream-null)))

(assert-equal
 "06-sicp-stream-append"
 '(1 2 3 4) (stream->list (stream-append s1 s2)))


(display "=== 06-sicp: memoization ===")
(newline)

(define call-count 0)

(define counted-stream
  (stream-cons 1
    (begin
      (set! call-count (+ call-count 1))
      (stream-cons 2 stream-null))))

(stream-cdr counted-stream)
(stream-cdr counted-stream)
(stream-cdr counted-stream)

(assert-equal
 "06-sicp-memoization-forced-once"
 1 call-count)


(report)
