(library (src cps)
  (export cps-transform cps-primitives)
  (import (rnrs))

  (define cps-primitives
    '(+ - * / = < > <= >= modulo remainder
      null? pair? number? symbol? boolean?
      cons car cdr list length append
      not eq? equal? display newline))

  (define (primitive-call? expr)
    "Predicate: true if expr is a call to a known primitive.
     expr : s-expression
     returns : boolean
     O(n) where n = number of primitives"
    (and (pair? expr)
         (symbol? (car expr))
         (memq (car expr) cps-primitives)
         #t))

  (define gensym-counter 0)

  (define (fresh-var prefix)
    "Generates a fresh variable symbol.
     prefix : string
     returns : unique symbol
     O(1)"
    (set! gensym-counter (+ gensym-counter 1))
    (string->symbol
     (string-append prefix
                    (number->string gensym-counter))))

  (define (atomic? expr)
    "Predicate: true for expressions needing no continuation.
     expr : s-expression
     returns : boolean
     O(1)"
    (or (number? expr)
        (string? expr)
        (boolean? expr)
        (symbol? expr)
        (and (pair? expr)
             (eq? (car expr) 'quote))))

  (define (cps-atomic expr)
    "CPS-transforms an atomic expression (identity).
     expr : atomic s-expression
     returns : CPS-equivalent expression
     O(1)"
    expr)

  (define (cps-transform expr)
    "Transforms an expression into continuation-passing style.
     expr : s-expression in direct style
     returns : CPS lambda expecting a continuation argument
     O(n) in AST node count"
    (set! gensym-counter 0)
    (cps-expr expr))

  (define (cps-expr expr)
    "Recursive CPS transformation of an expression.
     expr : s-expression
     returns : procedure of one argument (continuation)
     O(n) in AST node count"
    (cond
     ((atomic? expr)
      (let ((k (fresh-var "k")))
        `(lambda (,k) (,k ,(cps-atomic expr)))))
     ((and (pair? expr) (eq? (car expr) 'lambda))
      (cps-lambda (cadr expr) (cddr expr)))
     ((and (pair? expr) (eq? (car expr) 'if))
      (cps-if (cadr expr) (caddr expr)
              (if (null? (cdddr expr)) #f
                  (cadddr expr))))
     ((and (pair? expr) (eq? (car expr) 'begin))
      (cps-begin (cdr expr)))
     ((and (pair? expr) (eq? (car expr) 'let))
      (cps-let (cadr expr) (cddr expr)))
     ((primitive-call? expr)
      (cps-prim-application (car expr) (cdr expr)))
     ((pair? expr)
      (cps-application (car expr) (cdr expr)))
     (else
      (error 'cps-expr "unsupported expression" expr))))

  (define (cps-lambda params body)
    "CPS-transforms a lambda expression.
     params : list of symbols
     body : list of body expressions
     returns : CPS lambda expression
     O(n) in body size"
    (let ((k (fresh-var "k"))
          (kc (fresh-var "k")))
      `(lambda (,k)
         (,k (lambda (,@params ,kc)
               ,(cps-body body kc))))))

  (define (cps-body exprs k)
    "CPS-transforms a body (sequence), threading continuation.
     exprs : list of expressions
     k : continuation variable
     returns : CPS expression
     O(n) in number of expressions"
    (if (null? (cdr exprs))
        (cps-tail (car exprs) k)
        (let ((v (fresh-var "v")))
          (cps-tail (car exprs)
                    `(lambda (,v)
                       ,(cps-body (cdr exprs) k))))))

  (define (cps-tail expr k)
    "CPS-transforms expr in tail position with continuation k.
     expr : s-expression
     k : continuation (symbol or expression)
     returns : CPS application expression
     O(n) in AST size"
    (cond
     ((atomic? expr)
      `(,k ,(cps-atomic expr)))
     ((and (pair? expr) (eq? (car expr) 'lambda))
      (let ((params (cadr expr))
            (body (cddr expr))
            (kc (fresh-var "k")))
        `(,k (lambda (,@params ,kc)
               ,(cps-body body kc)))))
     ((and (pair? expr) (eq? (car expr) 'if))
      (cps-tail-if (cadr expr) (caddr expr)
                   (if (null? (cdddr expr)) #f
                       (cadddr expr))
                   k))
     ((and (pair? expr) (eq? (car expr) 'begin))
      (cps-tail-begin (cdr expr) k))
     ((and (pair? expr) (eq? (car expr) 'let))
      (cps-tail-let (cadr expr) (cddr expr) k))
     ((primitive-call? expr)
      (cps-tail-prim (car expr) (cdr expr) k))
     ((pair? expr)
      (cps-tail-app (car expr) (cdr expr) k))
     (else
      (error 'cps-tail "unsupported expression" expr))))

  (define (cps-if test then-expr else-expr)
    "CPS-transforms an if expression in non-tail position.
     test, then-expr, else-expr : s-expressions
     returns : CPS expression
     O(n)"
    (let ((k (fresh-var "k")))
      `(lambda (,k)
         ,(cps-tail-if test then-expr else-expr k))))

  (define (cps-tail-if test then-expr else-expr k)
    "CPS-transforms an if in tail position.
     test : s-expression (condition)
     then-expr, else-expr : s-expressions
     k : continuation
     returns : CPS expression
     O(n)"
    (if (atomic? test)
        `(if ,(cps-atomic test)
             ,(cps-tail then-expr k)
             ,(if else-expr
                  (cps-tail else-expr k)
                  `(,k (if #f #f))))
        (let ((v (fresh-var "v")))
          (cps-tail test
                    `(lambda (,v)
                       (if ,v
                           ,(cps-tail then-expr k)
                           ,(if else-expr
                                (cps-tail else-expr k)
                                `(,k (if #f #f)))))))))

  (define (cps-begin exprs)
    "CPS-transforms a begin expression in non-tail position.
     exprs : list of expressions
     returns : CPS expression
     O(n)"
    (let ((k (fresh-var "k")))
      `(lambda (,k)
         ,(cps-tail-begin exprs k))))

  (define (cps-tail-begin exprs k)
    "CPS-transforms begin in tail position.
     exprs : list of expressions
     k : continuation
     returns : CPS expression
     O(n)"
    (cps-body exprs k))

  (define (cps-let bindings body)
    "CPS-transforms a let in non-tail position.
     bindings : list of (name expr)
     body : list of expressions
     returns : CPS expression
     O(n)"
    (let ((k (fresh-var "k")))
      `(lambda (,k)
         ,(cps-tail-let bindings body k))))

  (define (cps-tail-let bindings body k)
    "CPS-transforms let in tail position.
     bindings : list of (name expr)
     body : body expressions
     k : continuation
     returns : CPS expression
     O(n)"
    (if (null? bindings)
        (cps-body body k)
        (let ((name (caar bindings))
              (init (cadar bindings))
              (rest (cdr bindings)))
          (if (atomic? init)
              `(let ((,name ,(cps-atomic init)))
                 ,(cps-tail-let rest body k))
              (cps-tail init
                        `(lambda (,name)
                           ,(cps-tail-let
                             rest body k)))))))

  (define (cps-prim-application op args)
    "CPS-transforms a primitive call in non-tail position.
     op : primitive operator symbol
     args : list of argument expressions
     returns : CPS expression
     O(n)"
    (let ((k (fresh-var "k")))
      `(lambda (,k)
         ,(cps-tail-prim op args k))))

  (define (cps-tail-prim op args k)
    "CPS-transforms a primitive call in tail position.
     op : primitive operator symbol
     args : argument expressions
     k : continuation
     returns : CPS expression
     O(n) in number of arguments"
    (cps-collect
     args '()
     (lambda (vals)
       `(,k (,op ,@vals)))))

  (define (cps-application fn args)
    "CPS-transforms a function application in non-tail.
     fn : operator expression
     args : list of argument expressions
     returns : CPS expression
     O(n)"
    (let ((k (fresh-var "k")))
      `(lambda (,k)
         ,(cps-tail-app fn args k))))

  (define (cps-tail-app fn args k)
    "CPS-transforms application in tail position.
     fn : operator expression
     args : argument expressions
     k : continuation
     returns : CPS expression
     O(n) in number of arguments"
    (cps-collect
     (cons fn args) '()
     (lambda (vals)
       `(,(car vals) ,@(cdr vals) ,k))))

  (define (cps-collect exprs acc builder)
    "Sequentializes evaluation of expressions for CPS.
     exprs : list of expressions to evaluate
     acc : accumulated evaluated values (reversed)
     builder : procedure taking list of values, returns expr
     O(n) where n = length of exprs"
    (if (null? exprs)
        (builder (reverse acc))
        (let ((e (car exprs))
              (rest (cdr exprs)))
          (if (atomic? e)
              (cps-collect rest
                          (cons (cps-atomic e) acc)
                          builder)
              (let ((v (fresh-var "v")))
                (cps-tail e
                          `(lambda (,v)
                             ,(cps-collect
                               rest
                               (cons v acc)
                               builder)))))))))
