(library (src core)
  (export logos-eval logos-apply
          make-env env-lookup env-extend env-define!
          global-env)
  (import (rnrs)
          (rnrs mutable-pairs))

  (define (make-env bindings parent)
    "Creates an environment frame.
     bindings : alist of (name . value) pairs
     parent : environment or #f for top-level
     returns : environment (pair of bindings and parent)
     O(1)"
    (cons bindings parent))

  (define (env-lookup name env)
    "Looks up name in environment, searching parent frames.
     name : symbol
     env : environment
     returns : value bound to name
     O(depth) worst-case"
    (if (not env)
        (error 'env-lookup "unbound variable" name)
        (let ((binding (assq name (car env))))
          (if binding
              (cdr binding)
              (env-lookup name (cdr env))))))

  (define (env-extend params args env)
    "Extends environment with new bindings from params/args.
     params : list of symbols
     args : list of values (same length as params)
     env : parent environment
     returns : new environment frame
     O(n) where n = length of params"
    (make-env (map cons params args) env))

  (define (env-define! name value env)
    "Mutates the current frame to add or update a binding.
     name : symbol
     value : any
     env : environment
     returns : void
     O(n) where n = bindings in current frame"
    (let ((binding (assq name (car env))))
      (if binding
          (set-cdr! binding value)
          (set-car! env (cons (cons name value)
                              (car env))))))

  (define (make-closure params body env)
    "Constructs a closure record.
     params : list of symbols
     body : list of expressions (implicit begin)
     env : lexical environment
     returns : closure vector
     O(1)"
    (vector 'closure params body env))

  (define (closure? x)
    "Predicate: true iff x is a logos closure.
     x : any
     returns : boolean
     O(1)"
    (and (vector? x)
         (= (vector-length x) 4)
         (eq? (vector-ref x 0) 'closure)))

  (define (closure-params c)
    "Extracts parameter list from closure.
     c : closure
     returns : list of symbols
     O(1)"
    (vector-ref c 1))

  (define (closure-body c)
    "Extracts body expressions from closure.
     c : closure
     returns : list of expressions
     O(1)"
    (vector-ref c 2))

  (define (closure-env c)
    "Extracts captured environment from closure.
     c : closure
     returns : environment
     O(1)"
    (vector-ref c 3))

  (define (make-bounce expr env)
    "Constructs a bounce record for trampolined tail calls.
     expr : expression to evaluate
     env : environment for evaluation
     returns : bounce vector
     O(1)"
    (vector 'bounce expr env))

  (define (bounce? x)
    "Predicate: true iff x is a bounce record.
     x : any
     returns : boolean
     O(1)"
    (and (vector? x)
         (= (vector-length x) 3)
         (eq? (vector-ref x 0) 'bounce)))

  (define (bounce-expr b)
    "Extracts expression from bounce record.
     b : bounce
     returns : expression
     O(1)"
    (vector-ref b 1))

  (define (bounce-env b)
    "Extracts environment from bounce record.
     b : bounce
     returns : environment
     O(1)"
    (vector-ref b 2))

  (define (trampoline result)
    "Drives bounce records to final values.
     result : value or bounce
     returns : final value (never a bounce)
     O(k) where k = number of tail calls"
    (let loop ((r result))
      (if (bounce? r)
          (loop (eval-inner (bounce-expr r) (bounce-env r)))
          r)))

  (define (self-evaluating? expr)
    "Predicate: true for numbers, strings, booleans.
     expr : any
     returns : boolean
     O(1)"
    (or (number? expr)
        (string? expr)
        (boolean? expr)))

  (define (eval-sequence exprs env)
    "Evaluates expressions; returns bounce for the last one.
     exprs : non-empty list of expressions
     env : environment
     returns : value or bounce for last expression
     O(n) where n = length of exprs"
    (if (null? (cdr exprs))
        (make-bounce (car exprs) env)
        (begin
          (trampoline (eval-inner (car exprs) env))
          (eval-sequence (cdr exprs) env))))

  (define (eval-cond clauses env)
    "Evaluates cond clauses; returns bounce for matched body.
     clauses : list of (test expr ...)
     env : environment
     returns : value or bounce
     O(k) where k = number of clauses tested"
    (cond
     ((null? clauses)
      (error 'eval-cond "no matching clause"))
     ((eq? (caar clauses) 'else)
      (eval-sequence (cdar clauses) env))
     ((trampoline (eval-inner (caar clauses) env))
      (eval-sequence (cdar clauses) env))
     (else
      (eval-cond (cdr clauses) env))))

  (define (eval-inner expr env)
    "Core evaluator returning bounces for tail positions.
     expr : s-expression
     env : environment
     returns : value or bounce
     O(n) in expression tree size"
    (cond
     ((self-evaluating? expr) expr)
     ((symbol? expr) (env-lookup expr env))
     ((not (pair? expr))
      (error 'logos-eval "unknown expression type" expr))
     ((eq? (car expr) 'quote) (cadr expr))
     ((eq? (car expr) 'if)
      (if (trampoline (eval-inner (cadr expr) env))
          (make-bounce (caddr expr) env)
          (if (null? (cdddr expr))
              (if #f #f)
              (make-bounce (cadddr expr) env))))
     ((eq? (car expr) 'cond)
      (eval-cond (cdr expr) env))
     ((eq? (car expr) 'and)
      (eval-and (cdr expr) env))
     ((eq? (car expr) 'or)
      (eval-or (cdr expr) env))
     ((eq? (car expr) 'define)
      (if (pair? (cadr expr))
          (let ((name (caadr expr))
                (params (cdadr expr))
                (body (cddr expr)))
            (env-define! name
                         (make-closure params body env)
                         env))
          (env-define! (cadr expr)
                       (trampoline
                        (eval-inner (caddr expr) env))
                       env)))
     ((eq? (car expr) 'lambda)
      (make-closure (cadr expr) (cddr expr) env))
     ((eq? (car expr) 'begin)
      (eval-sequence (cdr expr) env))
     ((eq? (car expr) 'let)
      (let ((bindings (cadr expr))
            (body (cddr expr)))
        (let ((params (map car bindings))
              (args (map (lambda (b)
                           (trampoline
                            (eval-inner (cadr b) env)))
                         bindings)))
          (eval-sequence
           body
           (env-extend params args env)))))
     (else
      (let ((proc (trampoline
                   (eval-inner (car expr) env)))
            (args (map (lambda (a)
                         (trampoline
                          (eval-inner a env)))
                       (cdr expr))))
        (apply-inner proc args)))))

  (define (apply-inner proc args)
    "Applies procedure, returning bounce for closure bodies.
     proc : closure or primitive procedure
     args : list of values
     returns : value or bounce
     O(1) dispatch + body cost"
    (cond
     ((closure? proc)
      (eval-sequence
       (closure-body proc)
       (env-extend (closure-params proc)
                   args
                   (closure-env proc))))
     ((procedure? proc)
      (apply proc args))
     (else
      (error 'logos-apply "not a procedure" proc))))

  (define (eval-and exprs env)
    "Evaluates and-expressions with short-circuit.
     exprs : list of expressions
     env : environment
     returns : value or bounce
     O(n) where n = length of exprs"
    (cond
     ((null? exprs) #t)
     ((null? (cdr exprs))
      (make-bounce (car exprs) env))
     (else
      (let ((val (trampoline
                  (eval-inner (car exprs) env))))
        (if val
            (eval-and (cdr exprs) env)
            #f)))))

  (define (eval-or exprs env)
    "Evaluates or-expressions with short-circuit.
     exprs : list of expressions
     env : environment
     returns : value or bounce
     O(n) where n = length of exprs"
    (cond
     ((null? exprs) #f)
     ((null? (cdr exprs))
      (make-bounce (car exprs) env))
     (else
      (let ((val (trampoline
                  (eval-inner (car exprs) env))))
        (if val
            val
            (eval-or (cdr exprs) env))))))

  (define (logos-eval expr env)
    "Evaluates an expression with proper tail-call elimination.
     expr : s-expression
     env : environment
     returns : value
     O(n) in expression tree size"
    (trampoline (eval-inner expr env)))

  (define (logos-apply proc args)
    "Applies a procedure to evaluated arguments.
     proc : closure or primitive procedure
     args : list of values
     returns : result of application
     O(1) dispatch + body evaluation cost"
    (trampoline (apply-inner proc args)))

  (define (make-global-env)
    "Constructs the global environment with primitives.
     returns : environment
     O(1)"
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
           (cons 'modulo mod)
           (cons 'remainder
                 (lambda (a b)
                   (- a (* b (truncate (/ a b))))))
           (cons 'null? null?)
           (cons 'pair? pair?)
           (cons 'number? number?)
           (cons 'symbol? symbol?)
           (cons 'boolean? boolean?)
           (cons 'cons cons)
           (cons 'car car)
           (cons 'cdr cdr)
           (cons 'list list)
           (cons 'length length)
           (cons 'append append)
           (cons 'not not)
           (cons 'eq? eq?)
           (cons 'equal? equal?)
           (cons 'display display)
           (cons 'newline newline))
     #f))

  (define global-env (make-global-env)))
