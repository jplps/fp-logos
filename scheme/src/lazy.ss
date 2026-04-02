(library (src lazy)
  (export stream-cons stream-car stream-cdr stream-null stream-null?
          stream-take stream-ref stream-map stream-filter
          stream-zip stream-append stream-from
          stream->list)
  (import (rnrs)
          (rnrs lists))

  (define stream-null
    (vector 'stream-null))

  (define (stream-null? s)
    "Predicate: true iff s is the empty stream.
     s : any
     returns : boolean
     O(1)"
    (and (vector? s)
         (= (vector-length s) 1)
         (eq? (vector-ref s 0) 'stream-null)))

  (define (make-stream-pair head thunk)
    "Constructs a stream pair with a delayed tail.
     head : any (already evaluated)
     thunk : procedure of zero arguments producing a stream
     returns : stream pair vector
     O(1)"
    (vector 'stream-pair head thunk #f))

  (define (stream-pair? s)
    "Predicate: true iff s is a non-empty stream.
     s : any
     returns : boolean
     O(1)"
    (and (vector? s)
         (= (vector-length s) 4)
         (eq? (vector-ref s 0) 'stream-pair)))

  (define-syntax stream-cons
    (syntax-rules ()
      ((_ head tail)
       (make-stream-pair head (lambda () tail)))))

  (define (stream-car s)
    "First element of a stream.
     s : non-empty stream
     returns : value
     O(1)"
    (if (stream-pair? s)
        (vector-ref s 1)
        (error 'stream-car "empty stream")))

  (define (stream-cdr s)
    "Rest of a stream, forcing the thunk and memoizing.
     s : non-empty stream
     returns : stream
     O(1) amortized (forced once, then cached)"
    (if (stream-pair? s)
        (if (vector-ref s 3)
            (vector-ref s 3)
            (let ((tail ((vector-ref s 2))))
              (vector-set! s 3 tail)
              tail))
        (error 'stream-cdr "empty stream")))

  (define (stream-take n s)
    "Takes the first n elements from a stream as a list.
     n : non-negative integer
     s : stream
     returns : list of up to n elements
     O(n)"
    (if (or (= n 0) (stream-null? s))
        '()
        (cons (stream-car s)
              (stream-take (- n 1) (stream-cdr s)))))

  (define (stream-ref s n)
    "Returns the nth element of stream s (zero-indexed).
     s : stream
     n : non-negative integer
     returns : value at position n
     O(n)"
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

  (define (stream-map f . streams)
    "Applies f element-wise across one or more streams.
     f : procedure
     streams : one or more streams
     returns : stream of results
     O(1) per element (lazy)"
    (if (exists stream-null? streams)
        stream-null
        (stream-cons
         (apply f (map stream-car streams))
         (apply stream-map f (map stream-cdr streams)))))

  (define (stream-filter pred s)
    "Filters a stream, keeping elements satisfying pred.
     pred : procedure returning boolean
     s : stream
     returns : stream of matching elements
     O(k) per element where k = elements skipped"
    (cond
     ((stream-null? s) stream-null)
     ((pred (stream-car s))
      (stream-cons (stream-car s)
                   (stream-filter pred (stream-cdr s))))
     (else
      (stream-filter pred (stream-cdr s)))))

  (define (stream-zip s1 s2)
    "Zips two streams into a stream of pairs.
     s1, s2 : streams
     returns : stream of (a . b) pairs
     O(1) per element (lazy)"
    (if (or (stream-null? s1) (stream-null? s2))
        stream-null
        (stream-cons
         (cons (stream-car s1) (stream-car s2))
         (stream-zip (stream-cdr s1)
                     (stream-cdr s2)))))

  (define (stream-append s1 s2)
    "Appends two streams (s1 must be finite).
     s1 : finite stream
     s2 : stream
     returns : concatenated stream
     O(1) per element (lazy)"
    (if (stream-null? s1)
        s2
        (stream-cons
         (stream-car s1)
         (stream-append (stream-cdr s1) s2))))

  (define (stream-from n)
    "Infinite stream of integers starting at n.
     n : integer
     returns : stream n, n+1, n+2, ...
     O(1) per element (lazy)"
    (stream-cons n (stream-from (+ n 1))))

  (define (stream->list s)
    "Converts a finite stream to a list.
     s : finite stream
     returns : list
     O(n) where n = stream length"
    (if (stream-null? s)
        '()
        (cons (stream-car s)
              (stream->list (stream-cdr s))))))
