(import (rnrs)
        (src core))

(define pass-count 0)
(define fail-count 0)

(define (assert-equal name expected actual)
  "Test assertion: checks equality and reports result.
   name : string
   expected, actual : any
   returns : void"
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

(define (fresh-env)
  "Creates a fresh global environment for test isolation."
  (make-env
   (list (cons '+ +)
         (cons '- -)
         (cons '* *)
         (cons '/ /)
         (cons '= =)
         (cons '< <)
         (cons '> >)
         (cons '<= <=)
         (cons '>= >=)
         (cons 'null? null?)
         (cons 'pair? pair?)
         (cons 'cons cons)
         (cons 'car car)
         (cons 'cdr cdr)
         (cons 'list list)
         (cons 'not not)
         (cons 'eq? eq?)
         (cons 'equal? equal?)
         (cons 'display display)
         (cons 'newline newline))
   #f))


(display "=== 02-mccarthy: self-evaluating ===")
(newline)

(assert-equal
 "02-mccarthy-eval-number"
 42 (logos-eval 42 global-env))

(assert-equal
 "02-mccarthy-eval-string"
 "hello" (logos-eval "hello" global-env))

(assert-equal
 "02-mccarthy-eval-boolean"
 #t (logos-eval #t global-env))


(display "=== 02-mccarthy: quote ===")
(newline)

(assert-equal
 "02-mccarthy-eval-quote"
 '(a b c) (logos-eval '(quote (a b c)) global-env))

(assert-equal
 "02-mccarthy-eval-quote-symbol"
 'x (logos-eval '(quote x) global-env))


(display "=== 02-mccarthy: arithmetic ===")
(newline)

(assert-equal
 "02-mccarthy-eval-add"
 3 (logos-eval '(+ 1 2) global-env))

(assert-equal
 "02-mccarthy-eval-nested-arith"
 12 (logos-eval '(+ (* 2 3) (- 8 2)) global-env))


(display "=== 02-mccarthy: if ===")
(newline)

(assert-equal
 "02-mccarthy-eval-if-true"
 1 (logos-eval '(if #t 1 2) global-env))

(assert-equal
 "02-mccarthy-eval-if-false"
 2 (logos-eval '(if #f 1 2) global-env))

(assert-equal
 "02-mccarthy-eval-if-computed"
 'yes (logos-eval '(if (= 1 1) (quote yes) (quote no))
                  global-env))


(display "=== 02-mccarthy: lambda + application ===")
(newline)

(assert-equal
 "02-mccarthy-eval-lambda-simple"
 6 (logos-eval '((lambda (x) (+ x 1)) 5) global-env))

(assert-equal
 "02-mccarthy-eval-lambda-two-args"
 7 (logos-eval '((lambda (x y) (+ x y)) 3 4) global-env))

(assert-equal
 "02-mccarthy-eval-lambda-body"
 3 (logos-eval '((lambda (x) (+ x 1) (+ x 2)) 1)
              global-env))


(display "=== 02-mccarthy: define ===")
(newline)

(let ((env (fresh-env)))
  (logos-eval '(define x 10) env)
  (assert-equal
   "02-mccarthy-eval-define-var"
   10 (logos-eval 'x env)))

(let ((env (fresh-env)))
  (logos-eval '(define (square x) (* x x)) env)
  (assert-equal
   "02-mccarthy-eval-define-proc"
   25 (logos-eval '(square 5) env)))


(display "=== 02-mccarthy: let ===")
(newline)

(assert-equal
 "02-mccarthy-eval-let"
 3 (logos-eval '(let ((x 1) (y 2)) (+ x y))
              global-env))


(display "=== 02-mccarthy: cond ===")
(newline)

(assert-equal
 "02-mccarthy-eval-cond-first"
 'a (logos-eval '(cond (#t (quote a)) (else (quote b)))
               global-env))

(assert-equal
 "02-mccarthy-eval-cond-else"
 'b (logos-eval '(cond (#f (quote a)) (else (quote b)))
               global-env))

(assert-equal
 "02-mccarthy-eval-cond-middle"
 2 (logos-eval '(cond ((= 1 2) 1) ((= 1 1) 2) (else 3))
              global-env))


(display "=== 02-mccarthy: begin ===")
(newline)

(assert-equal
 "02-mccarthy-eval-begin"
 3 (logos-eval '(begin 1 2 3) global-env))


(display "=== 02-mccarthy: recursion ===")
(newline)

(let ((env (fresh-env)))
  (logos-eval
   '(define (factorial n)
      (if (= n 0)
          1
          (* n (factorial (- n 1)))))
   env)
  (assert-equal
   "02-mccarthy-eval-factorial-0"
   1 (logos-eval '(factorial 0) env))
  (assert-equal
   "02-mccarthy-eval-factorial-5"
   120 (logos-eval '(factorial 5) env))
  (assert-equal
   "02-mccarthy-eval-factorial-10"
   3628800 (logos-eval '(factorial 10) env)))

(let ((env (fresh-env)))
  (logos-eval
   '(define (fib n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (fib (- n 1))
                     (fib (- n 2))))))
   env)
  (assert-equal
   "02-mccarthy-eval-fib-10"
   55 (logos-eval '(fib 10) env)))


(display "=== 02-mccarthy: higher-order functions ===")
(newline)

(let ((env (fresh-env)))
  (logos-eval '(define (twice f x) (f (f x))) env)
  (logos-eval '(define (add1 n) (+ n 1)) env)
  (assert-equal
   "02-mccarthy-eval-higher-order"
   7 (logos-eval '(twice add1 5) env)))

(let ((env (fresh-env)))
  (logos-eval
   '(define (make-adder n)
      (lambda (x) (+ n x)))
   env)
  (assert-equal
   "02-mccarthy-eval-closure-capture"
   15 (logos-eval '((make-adder 10) 5) env)))


(display "=== 02-mccarthy: list operations ===")
(newline)

(let ((env (fresh-env)))
  (logos-eval
   '(define (length lst)
      (if (null? lst)
          0
          (+ 1 (length (cdr lst)))))
   env)
  (assert-equal
   "02-mccarthy-eval-length"
   3 (logos-eval '(length (list 1 2 3)) env)))

(let ((env (fresh-env)))
  (logos-eval
   '(define (map f lst)
      (if (null? lst)
          (quote ())
          (cons (f (car lst))
                (map f (cdr lst)))))
   env)
  (logos-eval '(define (double x) (* x 2)) env)
  (assert-equal
   "02-mccarthy-eval-map"
   '(2 4 6) (logos-eval '(map double (list 1 2 3)) env)))


(display "=== 05-sussman-steele: tail-call elimination ===")
(newline)

(let ((env (fresh-env)))
  (logos-eval
   '(define (loop-sum n acc)
      (if (= n 0)
          acc
          (loop-sum (- n 1) (+ acc n))))
   env)
  (assert-equal
   "05-tce-tail-recursive-sum-1000000"
   500000500000
   (logos-eval '(loop-sum 1000000 0) env)))

(let ((env (fresh-env)))
  (logos-eval
   '(define (even? n)
      (if (= n 0) #t (odd? (- n 1))))
   env)
  (logos-eval
   '(define (odd? n)
      (if (= n 0) #f (even? (- n 1))))
   env)
  (assert-equal
   "05-tce-mutual-recursion-even"
   #t (logos-eval '(even? 100000) env))
  (assert-equal
   "05-tce-mutual-recursion-odd"
   #t (logos-eval '(odd? 99999) env)))

(let ((env (fresh-env)))
  (logos-eval
   '(define (count-down n)
      (cond ((= n 0) (quote done))
            (else (count-down (- n 1)))))
   env)
  (assert-equal
   "05-tce-cond-tail-position"
   'done (logos-eval '(count-down 500000) env)))

(let ((env (fresh-env)))
  (logos-eval
   '(define (last-of n)
      (begin 1 2 (if (= n 0) (quote end)
                     (last-of (- n 1)))))
   env)
  (assert-equal
   "05-tce-begin-tail-position"
   'end (logos-eval '(last-of 500000) env)))

(let ((env (fresh-env)))
  (logos-eval
   '(define (let-loop n)
      (let ((m (- n 1)))
        (if (= m 0) (quote done) (let-loop m))))
   env)
  (assert-equal
   "05-tce-let-tail-position"
   'done (logos-eval '(let-loop 500000) env)))


(display "=== 05-sussman-steele: and/or ===")
(newline)

(assert-equal
 "05-and-true"
 3 (logos-eval '(and 1 2 3) global-env))

(assert-equal
 "05-and-false"
 #f (logos-eval '(and 1 #f 3) global-env))

(assert-equal
 "05-and-empty"
 #t (logos-eval '(and) global-env))

(assert-equal
 "05-or-first-true"
 1 (logos-eval '(or 1 2 3) global-env))

(assert-equal
 "05-or-all-false"
 #f (logos-eval '(or #f #f #f) global-env))

(assert-equal
 "05-or-empty"
 #f (logos-eval '(or) global-env))


(report)
