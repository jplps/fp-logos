(import (rnrs)
        (src lambda))

(define pass-count 0)
(define fail-count 0)

(define (assert-equal name expected actual)
  "Test assertion: checks equality and reports result.
   name : string (test identifier)
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


(display "=== 01-church: numerals ===")
(newline)

(assert-equal
 "01-church-zero"
 0 (church->int church-zero))

(assert-equal
 "01-church-succ-zero"
 1 (church->int (church-succ church-zero)))

(assert-equal
 "01-church-add-2-3"
 5 (church->int (church-add (int->church 2) (int->church 3))))

(assert-equal
 "01-church-mult-3-4"
 12 (church->int (church-mult (int->church 3) (int->church 4))))

(assert-equal
 "01-church-mult-0-5"
 0 (church->int (church-mult church-zero (int->church 5))))

(assert-equal
 "01-church-exp-2-3"
 8 (church->int (church-exp (int->church 2) (int->church 3))))

(assert-equal
 "01-church-pred-5"
 4 (church->int (church-pred (int->church 5))))

(assert-equal
 "01-church-pred-0"
 0 (church->int (church-pred church-zero)))

(assert-equal
 "01-church-sub-5-2"
 3 (church->int (church-sub (int->church 5) (int->church 2))))

(assert-equal
 "01-church-sub-2-5-floors-at-zero"
 0 (church->int (church-sub (int->church 2) (int->church 5))))


(display "=== 01-church: property — add/mult vs native 0–20 ===")
(newline)

(let loop ((m 0))
  (when (<= m 20)
    (let inner ((n 0))
      (when (<= n 20)
        (let ((sum (church->int
                    (church-add (int->church m)
                                (int->church n)))))
          (unless (= sum (+ m n))
            (assert-equal
             (string-append "01-church-add-prop-"
                            (number->string m) "-"
                            (number->string n))
             (+ m n) sum)))
        (when (and (<= m 10) (<= n 10))
          (let ((prod (church->int
                       (church-mult (int->church m)
                                    (int->church n)))))
            (unless (= prod (* m n))
              (assert-equal
               (string-append "01-church-mult-prop-"
                              (number->string m) "-"
                              (number->string n))
               (* m n) prod))))
        (inner (+ n 1))))
    (loop (+ m 1))))

(assert-equal
 "01-church-add-mult-property-sweep"
 0 fail-count)


(display "=== 01-church: booleans ===")
(newline)

(assert-equal
 "01-church-if-true"
 'yes (church-if church-true 'yes 'no))

(assert-equal
 "01-church-if-false"
 'no (church-if church-false 'yes 'no))

(assert-equal
 "01-church-and-true-true"
 'yes (church-if (church-and church-true church-true) 'yes 'no))

(assert-equal
 "01-church-and-true-false"
 'no (church-if (church-and church-true church-false) 'yes 'no))

(assert-equal
 "01-church-or-false-true"
 'yes (church-if (church-or church-false church-true) 'yes 'no))

(assert-equal
 "01-church-or-false-false"
 'no (church-if (church-or church-false church-false) 'yes 'no))

(assert-equal
 "01-church-not-true"
 'no (church-if (church-not church-true) 'yes 'no))

(assert-equal
 "01-church-not-false"
 'yes (church-if (church-not church-false) 'yes 'no))


(display "=== 01-church: pairs ===")
(newline)

(assert-equal
 "01-church-pair-car"
 'a (church-car (church-pair 'a 'b)))

(assert-equal
 "01-church-pair-cdr"
 'b (church-cdr (church-pair 'a 'b)))

(assert-equal
 "01-church-pair-nested"
 2 (church->int
    (church-car
     (church-cdr
      (church-pair (int->church 1)
                   (church-pair (int->church 2)
                                (int->church 3)))))))


(display "=== 01-church: zero? predicate ===")
(newline)

(assert-equal
 "01-church-zero?-zero"
 'yes (church-if (church-zero? church-zero) 'yes 'no))

(assert-equal
 "01-church-zero?-nonzero"
 'no (church-if (church-zero? (int->church 3)) 'yes 'no))


(report)
