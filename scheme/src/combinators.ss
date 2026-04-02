(library (src combinators)
  (export fp-compose fp-construct fp-insert fp-apply-to-all
          fp-constant fp-identity fp-distl fp-distr
          fp-transpose)
  (import (rnrs))

  (define (fp-identity x)
    "Identity function: returns its argument unchanged.
     x : any
     returns : x
     O(1)"
    x)

  (define (fp-constant k)
    "Returns a function that always produces k.
     k : any
     returns : procedure ignoring its argument, returning k
     O(1)"
    (lambda (_) k))

  (define (fp-compose . fns)
    "Right-to-left composition of one or more functions.
     fns : one or more procedures
     returns : procedure applying fns right-to-left
     O(n) where n = number of functions"
    (let ((reversed (reverse fns)))
      (lambda (x)
        (let loop ((fs reversed) (acc x))
          (if (null? fs)
              acc
              (loop (cdr fs) ((car fs) acc)))))))

  (define (fp-construct . fns)
    "Applies each function to the same argument, collecting results.
     fns : one or more procedures
     returns : procedure producing a list of results
     O(n) where n = number of functions"
    (lambda (x)
      (map (lambda (f) (f x)) fns)))

  (define (fp-insert f)
    "Reduces a list with f (right-fold for two-arg f).
     f : procedure of two arguments
     returns : procedure from non-empty list to value
     O(n) where n = length of list"
    (lambda (lst)
      (if (null? (cdr lst))
          (car lst)
          (f (car lst)
             ((fp-insert f) (cdr lst))))))

  (define (fp-apply-to-all f)
    "Applies f to every element of a list.
     f : procedure of one argument
     returns : procedure from list to list
     O(n) where n = length of list"
    (lambda (lst)
      (map f lst)))

  (define (fp-distl pair)
    "Distributes left: given (a . (b1 b2 ...)), produces ((a . b1) (a . b2) ...).
     pair : pair of (value . list)
     returns : list of pairs
     O(n) where n = length of second element"
    (let ((a (car pair))
          (bs (cdr pair)))
      (map (lambda (b) (cons a b)) bs)))

  (define (fp-distr pair)
    "Distributes right: given ((a1 a2 ...) . b), produces ((a1 . b) (a2 . b) ...).
     pair : pair of (list . value)
     returns : list of pairs
     O(n) where n = length of first element"
    (let ((as (car pair))
          (b (cdr pair)))
      (map (lambda (a) (cons a b)) as)))

  (define (fp-transpose matrix)
    "Transposes a list of lists (matrix).
     matrix : list of lists (all same length)
     returns : transposed list of lists
     O(m·n) where m = rows, n = columns"
    (apply map list matrix)))
