(import (rnrs)
        (src macros)
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

(define (eval-with-macros expr)
  "Expands macros then evaluates in a fresh environment."
  (let ((menv (make-macro-env))
        (env (make-env
              (list (cons '+ +) (cons '- -)
                    (cons '* *) (cons '= =)
                    (cons '< <) (cons '> >)
                    (cons 'not not)
                    (cons 'null? null?)
                    (cons 'cons cons)
                    (cons 'car car) (cons 'cdr cdr)
                    (cons 'list list)
                    (cons 'display display)
                    (cons 'newline newline))
              #f)))
    (let loop ((exprs (if (and (pair? expr)
                               (eq? (car expr) 'begin))
                          (cdr expr)
                          (list expr)))
               (result (if #f #f)))
      (if (null? exprs)
          result
          (let ((expanded (macro-expand (car exprs) menv)))
            (loop (cdr exprs)
                  (logos-eval expanded env)))))))


(display "=== 08-kohlbecker: basic expansion ===")
(newline)

(let ((menv (make-macro-env)))
  (macro-define!
   'my-if
   (make-syntax-rules-transformer
    '()
    '(((_ test then else-branch)
       (cond (test then) (else else-branch)))))
   menv)
  (let ((expanded (macro-expand
                   '(my-if #t 1 2)
                   menv)))
    (assert-equal
     "08-macro-basic-expansion"
     '(cond (#t 1) (else 2))
     expanded)))


(display "=== 08-kohlbecker: my-let via syntax-rules ===")
(newline)

(assert-equal
 "08-macro-my-let"
 3
 (eval-with-macros
  '(begin
     (define-syntax my-let
       (syntax-rules ()
         ((_ ((var val) ...) body ...)
          ((lambda (var ...) body ...) val ...))))
     (my-let ((x 1) (y 2)) (+ x y)))))


(display "=== 08-kohlbecker: my-and / my-or ===")
(newline)

(assert-equal
 "08-macro-my-and-true"
 3
 (eval-with-macros
  '(begin
     (define-syntax my-and
       (syntax-rules ()
         ((_) #t)
         ((_ x) x)
         ((_ x rest ...)
          (if x (my-and rest ...) #f))))
     (my-and 1 2 3))))

(assert-equal
 "08-macro-my-and-false"
 #f
 (eval-with-macros
  '(begin
     (define-syntax my-and
       (syntax-rules ()
         ((_) #t)
         ((_ x) x)
         ((_ x rest ...)
          (if x (my-and rest ...) #f))))
     (my-and 1 #f 3))))

(assert-equal
 "08-macro-my-or-first"
 1
 (eval-with-macros
  '(begin
     (define-syntax my-or
       (syntax-rules ()
         ((_) #f)
         ((_ x) x)
         ((_ x rest ...)
          (let ((t x))
            (if t t (my-or rest ...))))))
     (my-or 1 2 3))))

(assert-equal
 "08-macro-my-or-fallthrough"
 3
 (eval-with-macros
  '(begin
     (define-syntax my-or
       (syntax-rules ()
         ((_) #f)
         ((_ x) x)
         ((_ x rest ...)
          (let ((t x))
            (if t t (my-or rest ...))))))
     (my-or #f #f 3))))


(display "=== 08-kohlbecker: when/unless ===")
(newline)

(assert-equal
 "08-macro-when-true"
 42
 (eval-with-macros
  '(begin
     (define-syntax when
       (syntax-rules ()
         ((_ test body ...)
          (if test (begin body ...) #f))))
     (when #t 1 2 42))))

(assert-equal
 "08-macro-when-false"
 #f
 (eval-with-macros
  '(begin
     (define-syntax when
       (syntax-rules ()
         ((_ test body ...)
          (if test (begin body ...) #f))))
     (when #f 1 2 42))))

(assert-equal
 "08-macro-unless"
 'ok
 (eval-with-macros
  '(begin
     (define-syntax unless
       (syntax-rules ()
         ((_ test body ...)
          (if (not test) (begin body ...) #f))))
     (unless #f (quote ok)))))


(display "=== 08-kohlbecker: swap (non-hygiene test) ===")
(newline)

(assert-equal
 "08-macro-nested-expansion"
 '(1 2)
 (eval-with-macros
  '(begin
     (define-syntax my-let
       (syntax-rules ()
         ((_ ((var val) ...) body ...)
          ((lambda (var ...) body ...) val ...))))
     (define-syntax my-list2
       (syntax-rules ()
         ((_ a b)
          (my-let ((x a) (y b)) (list x y)))))
     (my-list2 1 2))))


(display "=== 08-kohlbecker: hygiene ===")
(newline)

(assert-equal
 "08-macro-hygiene-or-capture"
 7
 (eval-with-macros
  '(begin
     (define-syntax my-or
       (syntax-rules ()
         ((_) #f)
         ((_ x) x)
         ((_ x rest ...)
          (let ((t x))
            (if t t (my-or rest ...))))))
     (let ((t 7))
       (my-or #f t)))))


(report)
