(import (rnrs)
        (src effects))

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


(display "=== 16-kiselyov: pure effects ===")
(newline)

(assert-equal
 "16-eff-pure-run"
 42 (eff-run (eff-pure 42)))

(assert-equal
 "16-eff-bind-pure"
 10 (eff-run (eff-bind (eff-pure 5)
                       (lambda (x) (eff-pure (* x 2))))))

(assert-equal
 "16-eff-then"
 3 (eff-run (eff-then (eff-pure 1) (eff-pure 3))))


(display "=== 16-kiselyov: reader effect ===")
(newline)

(assert-equal
 "16-eff-reader-ask"
 42
 (eff-run
  (eff-handle 'reader (reader-handler 42)
              reader-ask)))

(assert-equal
 "16-eff-reader-bind"
 43
 (eff-run
  (eff-handle 'reader (reader-handler 42)
              (eff-bind reader-ask
                        (lambda (x)
                          (eff-pure (+ x 1)))))))

(assert-equal
 "16-eff-reader-multiple"
 84
 (eff-run
  (eff-handle 'reader (reader-handler 42)
              (eff-bind reader-ask
                        (lambda (x)
                          (eff-bind reader-ask
                                    (lambda (y)
                                      (eff-pure (+ x y)))))))))


(display "=== 16-kiselyov: state effect ===")
(newline)

(assert-equal
 "16-eff-state-get"
 0
 (eff-run
  (eff-handle 'state (state-handler 0)
              state-get*)))

(assert-equal
 "16-eff-state-put-get"
 42
 (eff-run
  (eff-handle 'state (state-handler 0)
              (eff-bind (state-put* 42)
                        (lambda (_)
                          state-get*)))))

(assert-equal
 "16-eff-state-increment"
 3
 (eff-run
  (eff-handle 'state (state-handler 0)
              (eff-bind (state-put* 1)
                        (lambda (_)
                          (eff-bind state-get*
                                    (lambda (s)
                                      (eff-bind (state-put* (+ s 1))
                                                (lambda (_)
                                                  (eff-bind state-get*
                                                            (lambda (s2)
                                                              (eff-bind (state-put* (+ s2 1))
                                                                        (lambda (_)
                                                                          state-get*)))))))))))))


(display "=== 16-kiselyov: exception effect ===")
(newline)

(assert-equal
 "16-eff-exception-catch"
 '(error . "boom")
 (eff-run
  (eff-handle 'exception exception-handler
              (exception-throw "boom"))))

(assert-equal
 "16-eff-exception-short-circuit"
 '(error . "fail")
 (eff-run
  (eff-handle 'exception exception-handler
              (eff-bind (eff-pure 1)
                        (lambda (_)
                          (eff-bind (exception-throw "fail")
                                    (lambda (_)
                                      (eff-pure 999))))))))

(assert-equal
 "16-eff-no-exception"
 42
 (eff-run
  (eff-handle 'exception exception-handler
              (eff-pure 42))))


(display "=== 16-kiselyov: composed effects ===")
(newline)

(assert-equal
 "16-eff-reader+state"
 52
 (eff-run
  (eff-handle 'reader (reader-handler 10)
              (eff-handle 'state (state-handler 0)
                          (eff-bind reader-ask
                                    (lambda (r)
                                      (eff-bind (state-put* (+ r 42))
                                                (lambda (_)
                                                  state-get*))))))))

(assert-equal
 "16-eff-state+exception-success"
 5
 (eff-run
  (eff-handle 'exception exception-handler
              (eff-handle 'state (state-handler 0)
                          (eff-bind (state-put* 5)
                                    (lambda (_)
                                      state-get*))))))

(assert-equal
 "16-eff-state+exception-failure"
 '(error . "oops")
 (eff-run
  (eff-handle 'exception exception-handler
              (eff-handle 'state (state-handler 0)
                          (eff-bind (state-put* 5)
                                    (lambda (_)
                                      (exception-throw "oops")))))))

(assert-equal
 "16-eff-handler-order-matters"
 '(error . "err")
 (eff-run
  (eff-handle 'state (state-handler 0)
              (eff-handle 'exception exception-handler
                          (eff-bind (state-put* 10)
                                    (lambda (_)
                                      (exception-throw "err")))))))


(report)
