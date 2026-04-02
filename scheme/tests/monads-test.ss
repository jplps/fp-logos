(import (rnrs)
        (src monads))

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


(display "=== 10-wadler: maybe monad ===")
(newline)

(assert-equal
 "10-wadler-maybe-just"
 #t (maybe-just? (maybe-just 42)))

(assert-equal
 "10-wadler-maybe-value"
 42 (maybe-value (maybe-just 42)))

(assert-equal
 "10-wadler-maybe-nothing"
 #t (maybe-nothing? maybe-nothing))

(assert-equal
 "10-wadler-maybe-bind-just"
 #t
 (maybe-just?
  (maybe-bind (maybe-just 5)
              (lambda (x) (maybe-just (* x 2))))))

(assert-equal
 "10-wadler-maybe-bind-just-value"
 10
 (maybe-value
  (maybe-bind (maybe-just 5)
              (lambda (x) (maybe-just (* x 2))))))

(assert-equal
 "10-wadler-maybe-bind-nothing"
 #t
 (maybe-nothing?
  (maybe-bind maybe-nothing
              (lambda (x) (maybe-just (* x 2))))))

(assert-equal
 "10-wadler-maybe-chain"
 #t
 (maybe-nothing?
  (maybe-bind (maybe-just 5)
              (lambda (x)
                (maybe-bind maybe-nothing
                            (lambda (y)
                              (maybe-just (+ x y))))))))

(define (safe-div a b)
  "Division returning Maybe."
  (if (= b 0)
      maybe-nothing
      (maybe-just (/ a b))))

(assert-equal
 "10-wadler-maybe-safe-div"
 2
 (maybe-value
  (maybe-bind (safe-div 10 5)
              (lambda (x)
                (maybe-bind (safe-div x 1)
                            (lambda (y)
                              (maybe-just y)))))))

(assert-equal
 "10-wadler-maybe-safe-div-zero"
 #t
 (maybe-nothing?
  (maybe-bind (safe-div 10 0)
              (lambda (x) (maybe-just x)))))


(display "=== 10-wadler: maybe monad laws ===")
(newline)

(assert-equal
 "10-wadler-maybe-laws"
 #t
 (monad-laws-hold?
  maybe-monad maybe-unit maybe-bind
  5
  (lambda (x) (maybe-just (* x 2)))
  (lambda (x) (maybe-just (+ x 1)))))


(display "=== 10-wadler: state monad ===")
(newline)

(assert-equal
 "10-wadler-state-unit"
 '(42 . initial)
 (run-state (state-unit 42) 'initial))

(assert-equal
 "10-wadler-state-get"
 '(100 . 100)
 (run-state state-get 100))

(assert-equal
 "10-wadler-state-put"
 99
 (cdr (run-state (state-put 99) 0)))

(assert-equal
 "10-wadler-state-modify"
 11
 (cdr (run-state (state-modify (lambda (s) (+ s 1)))
                 10)))

(assert-equal
 "10-wadler-state-bind-get-put"
 '(1 . 1)
 (run-state
  (state-bind
   state-get
   (lambda (x)
     (state-bind
      (state-put (+ x 1))
      (lambda (_)
        (state-bind state-get
                    (lambda (y)
                      (state-unit y)))))))
  0))

(assert-equal
 "10-wadler-state-counter"
 '(3 . 3)
 (run-state
  (state-bind
   (state-modify (lambda (s) (+ s 1)))
   (lambda (_)
     (state-bind
      (state-modify (lambda (s) (+ s 1)))
      (lambda (_)
        (state-bind
         (state-modify (lambda (s) (+ s 1)))
         (lambda (_) state-get))))))
  0))


(display "=== 10-wadler: state monad laws ===")
(newline)

(define (state-equal? a b initial)
  "Compares two state computations by running them."
  (equal? (run-state a initial) (run-state b initial)))

(let ((a 5)
      (f (lambda (x)
           (state-bind state-get
                       (lambda (s)
                         (state-unit (+ x s))))))
      (g (lambda (x) (state-unit (* x 2)))))
  (assert-equal
   "10-wadler-state-left-identity"
   #t
   (state-equal? (state-bind (state-unit a) f)
                 (f a)
                 10))
  (assert-equal
   "10-wadler-state-right-identity"
   #t
   (state-equal?
    (state-bind (state-unit a) state-unit)
    (state-unit a)
    10))
  (assert-equal
   "10-wadler-state-associativity"
   #t
   (state-equal?
    (state-bind (state-bind (state-unit a) f) g)
    (state-bind (state-unit a)
                (lambda (x) (state-bind (f x) g)))
    10)))


(display "=== 10-wadler: list monad ===")
(newline)

(assert-equal
 "10-wadler-list-unit"
 '(5) (list-unit 5))

(assert-equal
 "10-wadler-list-bind"
 '(1 -1 2 -2 3 -3)
 (list-bind '(1 2 3)
            (lambda (x) (list x (- x)))))

(assert-equal
 "10-wadler-list-bind-empty"
 '()
 (list-bind '() (lambda (x) (list x))))

(assert-equal
 "10-wadler-list-flatmap"
 '((1 a) (1 b) (2 a) (2 b))
 (list-bind '(1 2)
            (lambda (x)
              (list-bind '(a b)
                         (lambda (y)
                           (list-unit (list x y)))))))


(display "=== 10-wadler: list monad laws ===")
(newline)

(assert-equal
 "10-wadler-list-laws"
 #t
 (monad-laws-hold?
  list-monad list-unit list-bind
  5
  (lambda (x) (list x (* x 2)))
  (lambda (x) (list (+ x 1)))))


(display "=== 10-wadler: monad-map and monad-join ===")
(newline)

(assert-equal
 "10-wadler-monad-map-maybe"
 10
 (maybe-value
  (monad-map maybe-monad
             (lambda (x) (* x 2))
             (maybe-just 5))))

(assert-equal
 "10-wadler-monad-map-list"
 '(2 4 6)
 (monad-map list-monad
            (lambda (x) (* x 2))
            '(1 2 3)))

(assert-equal
 "10-wadler-monad-join-list"
 '(1 2 3 4)
 (monad-join list-monad '((1 2) (3 4))))


(report)
