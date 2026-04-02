(library (src monads)
  (export monad-unit monad-bind monad-then
          maybe-monad maybe-unit maybe-bind
          maybe-just maybe-nothing maybe-just? maybe-nothing?
          maybe-value
          state-monad state-unit state-bind
          state-get state-put state-modify run-state
          list-monad list-unit list-bind
          monad-map monad-join
          monad-laws-hold?)
  (import (rnrs))

  (define (monad-unit m v)
    "Wraps a value using monad m's unit operation.
     m : monad record (vector)
     v : any
     returns : monadic value
     O(1)"
    ((vector-ref m 1) v))

  (define (monad-bind m mv f)
    "Sequences monadic computation mv into f using monad m.
     m : monad record
     mv : monadic value
     f : procedure from value to monadic value
     returns : monadic value
     O(1) dispatch + f cost"
    ((vector-ref m 2) mv f))

  (define (monad-then m mv1 mv2)
    "Sequences mv1 then mv2, discarding mv1's result.
     m : monad record
     mv1, mv2 : monadic values
     returns : monadic value
     O(1) dispatch"
    (monad-bind m mv1 (lambda (_) mv2)))

  (define (monad-map m f mv)
    "Applies f inside a monadic value (functor map).
     m : monad record
     f : procedure
     mv : monadic value
     returns : monadic value
     O(1) dispatch"
    (monad-bind m mv (lambda (v) (monad-unit m (f v)))))

  (define (monad-join m mmv)
    "Flattens a nested monadic value.
     m : monad record
     mmv : m (m a)
     returns : m a
     O(1) dispatch"
    (monad-bind m mmv (lambda (x) x)))


  (define maybe-nothing (vector 'nothing))

  (define (maybe-just v)
    "Wraps a value in Just.
     v : any
     returns : maybe value
     O(1)"
    (vector 'just v))

  (define (maybe-just? mv)
    "Predicate: true iff mv is a Just value.
     mv : maybe value
     returns : boolean
     O(1)"
    (and (vector? mv)
         (= (vector-length mv) 2)
         (eq? (vector-ref mv 0) 'just)))

  (define (maybe-nothing? mv)
    "Predicate: true iff mv is Nothing.
     mv : maybe value
     returns : boolean
     O(1)"
    (and (vector? mv)
         (= (vector-length mv) 1)
         (eq? (vector-ref mv 0) 'nothing)))

  (define (maybe-value mv)
    "Extracts the value from a Just.
     mv : Just value
     returns : the wrapped value
     O(1)"
    (vector-ref mv 1))

  (define (maybe-unit v)
    "Maybe unit: wraps in Just.
     v : any
     returns : Just v
     O(1)"
    (maybe-just v))

  (define (maybe-bind mv f)
    "Maybe bind: applies f if Just, propagates Nothing.
     mv : maybe value
     f : procedure from value to maybe value
     returns : maybe value
     O(1) dispatch + f cost"
    (if (maybe-just? mv)
        (f (maybe-value mv))
        maybe-nothing))

  (define maybe-monad
    (vector 'maybe-monad maybe-unit maybe-bind))


  (define (state-unit v)
    "State unit: returns v without changing state.
     v : any
     returns : state computation (procedure s -> (v . s))
     O(1)"
    (lambda (s) (cons v s)))

  (define (state-bind mv f)
    "State bind: threads state through mv then f.
     mv : state computation
     f : value -> state computation
     returns : state computation
     O(1) dispatch + f cost"
    (lambda (s)
      (let ((result (mv s)))
        ((f (car result)) (cdr result)))))

  (define state-get
    (lambda (s)
      "State get: returns current state as value.
       s : state
       returns : (s . s)
       O(1)"
      (cons s s)))

  (define (state-put new-s)
    "State put: replaces current state.
     new-s : new state value
     returns : state computation
     O(1)"
    (lambda (_) (cons (if #f #f) new-s)))

  (define (state-modify f)
    "State modify: applies f to current state.
     f : state -> state
     returns : state computation
     O(1) dispatch + f cost"
    (lambda (s) (cons (if #f #f) (f s))))

  (define (run-state computation initial-state)
    "Runs a state computation with initial state.
     computation : state procedure
     initial-state : any
     returns : (value . final-state) pair
     O(1) dispatch + computation cost"
    (computation initial-state))

  (define state-monad
    (vector 'state-monad state-unit state-bind))


  (define (list-unit v)
    "List unit: wraps v in a singleton list.
     v : any
     returns : (v)
     O(1)"
    (list v))

  (define (list-bind mv f)
    "List bind: flatmaps f across all elements.
     mv : list of values
     f : value -> list of values
     returns : concatenated results
     O(n·m) where n = length mv, m = avg result length"
    (apply append (map f mv)))

  (define list-monad
    (vector 'list-monad list-unit list-bind))


  (define (monad-laws-hold? m unit-fn bind-fn a f g)
    "Verifies the three monad laws for given values.
     m : monad record
     unit-fn : unit procedure
     bind-fn : bind procedure
     a : test value
     f, g : test functions (value -> monadic value)
     returns : boolean (all three laws satisfied)
     O(1) + cost of f and g"
    (let ((left-id
           (equal? (bind-fn (unit-fn a) f)
                   (f a)))
          (right-id
           (equal? (bind-fn (unit-fn a)
                            (lambda (x) (unit-fn x)))
                   (unit-fn a)))
          (assoc
           (equal? (bind-fn (bind-fn (unit-fn a) f) g)
                   (bind-fn (unit-fn a)
                            (lambda (x)
                              (bind-fn (f x) g))))))
      (and left-id right-id assoc))))
