(library (src effects)
  (export eff-pure eff-send eff-run eff-handle
          reader-ask reader-handler
          state-get* state-put* state-handler
          exception-throw exception-handler
          eff-bind eff-then)
  (import (rnrs))

  (define (eff-pure v)
    "Wraps a value as a pure effect computation.
     v : any
     returns : effect record (pure)
     O(1)"
    (vector 'pure v))

  (define (eff-send effect-tag payload)
    "Sends an effect request to the handler chain.
     effect-tag : symbol identifying the effect type
     payload : data for the effect
     returns : effect record (request)
     O(1)"
    (vector 'request effect-tag payload
            (lambda (x) (eff-pure x))))

  (define (eff-pure? e)
    "Predicate: true iff e is a pure value.
     e : effect computation
     returns : boolean
     O(1)"
    (and (vector? e) (eq? (vector-ref e 0) 'pure)))

  (define (eff-request? e)
    "Predicate: true iff e is an effect request.
     e : effect computation
     returns : boolean
     O(1)"
    (and (vector? e) (eq? (vector-ref e 0) 'request)))

  (define (eff-value e)
    "Extracts the value from a pure computation.
     e : pure effect
     returns : wrapped value
     O(1)"
    (vector-ref e 1))

  (define (eff-tag e)
    "Extracts the effect tag from a request.
     e : request effect
     returns : symbol
     O(1)"
    (vector-ref e 1))

  (define (eff-payload e)
    "Extracts the payload from a request.
     e : request effect
     returns : payload data
     O(1)"
    (vector-ref e 2))

  (define (eff-continuation e)
    "Extracts the continuation from a request.
     e : request effect
     returns : procedure from value to effect
     O(1)"
    (vector-ref e 3))

  (define (eff-bind m f)
    "Sequences effect computations.
     m : effect computation
     f : procedure from value to effect computation
     returns : effect computation
     O(1) dispatch"
    (cond
     ((eff-pure? m) (f (eff-value m)))
     ((eff-request? m)
      (vector 'request
              (eff-tag m)
              (eff-payload m)
              (lambda (x)
                (eff-bind ((eff-continuation m) x) f))))
     (else (error 'eff-bind "invalid effect" m))))

  (define (eff-then m1 m2)
    "Sequences m1 then m2, discarding m1's result.
     m1, m2 : effect computations
     returns : effect computation
     O(1)"
    (eff-bind m1 (lambda (_) m2)))

  (define (eff-run computation)
    "Runs a pure computation, extracting its value.
     computation : effect computation (must be pure)
     returns : value
     O(1)"
    (if (eff-pure? computation)
        (eff-value computation)
        (error 'eff-run "unhandled effect"
               (eff-tag computation))))

  (define (eff-handle tag handler computation)
    "Handles effects of a given tag in a computation.
     tag : symbol identifying effects to handle
     handler : procedure (payload continuation -> effect)
     computation : effect computation
     returns : effect computation with tag handled
     O(k) where k = number of handled effects"
    (cond
     ((eff-pure? computation) computation)
     ((eff-request? computation)
      (if (eq? (eff-tag computation) tag)
          (handler (eff-payload computation)
                   (lambda (v)
                     (eff-handle tag handler
                                ((eff-continuation
                                  computation) v))))
          (vector 'request
                  (eff-tag computation)
                  (eff-payload computation)
                  (lambda (v)
                    (eff-handle tag handler
                               ((eff-continuation
                                 computation) v))))))
     (else (error 'eff-handle "invalid effect"
                  computation))))


  (define reader-ask
    (eff-send 'reader 'ask))

  (define (reader-handler env)
    "Creates a handler that provides env for reader-ask.
     env : the environment value to provide
     returns : handler procedure
     O(1)"
    (lambda (payload k)
      (k env)))


  (define state-get*
    (eff-send 'state 'get))

  (define (state-put* v)
    "Sends a state-put effect.
     v : new state value
     returns : effect computation
     O(1)"
    (eff-send 'state (cons 'put v)))

  (define (state-handler initial-state)
    "Creates a stateful handler threading state through effects.
     initial-state : initial state value
     returns : handler procedure
     O(1)"
    (let ((state initial-state))
      (lambda (payload k)
        (cond
         ((eq? payload 'get)
          (k state))
         ((and (pair? payload) (eq? (car payload) 'put))
          (set! state (cdr payload))
          (k (if #f #f)))
         (else
          (error 'state-handler "unknown payload"
                 payload))))))


  (define (exception-throw msg)
    "Sends an exception effect.
     msg : error message/data
     returns : effect computation
     O(1)"
    (eff-send 'exception msg))

  (define (exception-handler payload k)
    "Handles exceptions by returning the error as a value.
     payload : error data
     k : continuation (ignored)
     returns : pure effect wrapping (error . payload)
     O(1)"
    (eff-pure (cons 'error payload))))
