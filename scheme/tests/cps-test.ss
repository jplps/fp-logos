(import (rnrs)
        (src cps)
        (src core))

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

(define (run-cps expr)
  "Runs a CPS-transformed expression with identity continuation."
  ((eval (cps-transform expr) (environment '(rnrs)))
   (lambda (x) x)))


(display "=== 07-steele: CPS atomic ===")
(newline)

(assert-equal
 "07-cps-number"
 42 (run-cps 42))

(assert-equal
 "07-cps-boolean"
 #t (run-cps #t))

(assert-equal
 "07-cps-symbol"
 '+ (run-cps '(quote +)))


(display "=== 07-steele: CPS lambda ===")
(newline)

(let ((cps-form (cps-transform '(lambda (x) x))))
  (assert-equal
   "07-cps-lambda-is-lambda"
   'lambda (car cps-form)))


(display "=== 07-steele: CPS round-trip ===")
(newline)

(define (round-trip name expr env)
  "Verifies direct eval = CPS eval for an expression."
  (let ((direct (logos-eval expr env))
        (cps-result (run-cps expr)))
    (assert-equal name direct cps-result)))

(round-trip "07-cps-roundtrip-add" '(+ 1 2)
            global-env)

(round-trip "07-cps-roundtrip-nested"
            '(+ (* 2 3) (- 10 4))
            global-env)

(round-trip "07-cps-roundtrip-if-true"
            '(if #t 1 2)
            global-env)

(round-trip "07-cps-roundtrip-if-false"
            '(if #f 1 2)
            global-env)

(round-trip "07-cps-roundtrip-if-computed"
            '(if (= 1 1) (+ 10 20) (+ 30 40))
            global-env)


(display "=== 07-steele: CPS lambda application ===")
(newline)

(assert-equal
 "07-cps-lambda-app"
 6 (run-cps '((lambda (x) (+ x 1)) 5)))

(assert-equal
 "07-cps-lambda-two-args"
 7 (run-cps '((lambda (x y) (+ x y)) 3 4)))


(display "=== 07-steele: CPS let ===")
(newline)

(assert-equal
 "07-cps-let-simple"
 3 (run-cps '(let ((x 1) (y 2)) (+ x y))))

(assert-equal
 "07-cps-let-nested-expr"
 10 (run-cps '(let ((x (+ 3 4))) (+ x 3))))


(display "=== 07-steele: CPS begin ===")
(newline)

(assert-equal
 "07-cps-begin"
 3 (run-cps '(begin 1 2 3)))


(display "=== 07-steele: CPS structure ===")
(newline)

(let ((transformed (cps-transform '(+ 1 2))))
  (assert-equal
   "07-cps-output-is-lambda"
   'lambda (car transformed)))

(let ((transformed (cps-transform '(if #t 1 2))))
  (assert-equal
   "07-cps-if-output-is-lambda"
   'lambda (car transformed)))


(report)
