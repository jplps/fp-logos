(library (src lambda)
  (export church-zero church-succ church-add church-mult church-exp
          church-zero? church-pred church-sub
          church-true church-false church-if church-and church-or church-not
          church-pair church-car church-cdr
          church->int int->church)
  (import (rnrs))

  (define church-zero
    (lambda (f)
      "The zero Church numeral: applies f zero times.
       f : procedure
       returns : identity procedure
       O(0)"
      (lambda (x) x)))

  (define (church-succ n)
    "Successor of Church numeral n.
     n : Church numeral
     returns : Church numeral
     O(1)"
    (lambda (f) (lambda (x) (f ((n f) x)))))

  (define (church-add m n)
    "λ-encoded addition: applies m successors after n successors.
     m, n : Church numeral
     returns : Church numeral
     O(m) applications of f"
    (lambda (f) (lambda (x) ((m f) ((n f) x)))))

  (define (church-mult m n)
    "λ-encoded multiplication: composes m with n.
     m, n : Church numeral
     returns : Church numeral
     O(m·n) applications of f"
    (lambda (f) (m (n f))))

  (define (church-exp m n)
    "λ-encoded exponentiation: n applied to m.
     m : Church numeral (base)
     n : Church numeral (exponent)
     returns : Church numeral
     O(m^n) applications of f"
    (n m))

  (define (church-zero? n)
    "Predicate: true iff n is the zero Church numeral.
     n : Church numeral
     returns : Church boolean
     O(1)"
    ((n (lambda (_) church-false)) church-true))

  (define (church-pred n)
    "Predecessor of Church numeral n; pred(0) = 0.
     n : Church numeral
     returns : Church numeral
     O(n)"
    (let ((step (lambda (p)
                  (church-pair (church-cdr p)
                               (church-succ (church-cdr p))))))
      (church-car
       ((n step)
        (church-pair church-zero church-zero)))))

  (define (church-sub m n)
    "λ-encoded subtraction: m minus n, floored at zero.
     m, n : Church numeral
     returns : Church numeral
     O(n·m)"
    ((n church-pred) m))

  (define church-true
    (lambda (a)
      "Church-encoded boolean true: selects first argument.
       a : any
       returns : procedure selecting a over second argument
       O(1)"
      (lambda (_) a)))

  (define church-false
    (lambda (_)
      "Church-encoded boolean false: selects second argument.
       _ : ignored
       returns : identity procedure
       O(1)"
      (lambda (b) b)))

  (define (church-if pred then-val else-val)
    "Conditional on a Church boolean.
     pred : Church boolean
     then-val, else-val : any
     returns : then-val if pred is true, else-val otherwise
     O(1)"
    ((pred then-val) else-val))

  (define (church-and a b)
    "Logical conjunction of Church booleans.
     a, b : Church boolean
     returns : Church boolean
     O(1)"
    ((a b) church-false))

  (define (church-or a b)
    "Logical disjunction of Church booleans.
     a, b : Church boolean
     returns : Church boolean
     O(1)"
    ((a church-true) b))

  (define (church-not a)
    "Logical negation of a Church boolean.
     a : Church boolean
     returns : Church boolean
     O(1)"
    ((a church-false) church-true))

  (define (church-pair a b)
    "Church-encoded pair constructor.
     a, b : any
     returns : Church pair (procedure accepting a selector)
     O(1)"
    (lambda (sel) ((sel a) b)))

  (define (church-car p)
    "First element of a Church pair.
     p : Church pair
     returns : the first element
     O(1)"
    (p church-true))

  (define (church-cdr p)
    "Second element of a Church pair.
     p : Church pair
     returns : the second element
     O(1)"
    (p church-false))

  (define (church->int n)
    "Converts a Church numeral to a native integer.
     n : Church numeral
     returns : exact non-negative integer
     O(n)"
    ((n (lambda (x) (+ x 1))) 0))

  (define (int->church k)
    "Converts a native non-negative integer to a Church numeral.
     k : exact non-negative integer
     returns : Church numeral
     O(k)"
    (if (= k 0)
        church-zero
        (church-succ (int->church (- k 1))))))
