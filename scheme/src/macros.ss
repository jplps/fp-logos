(library (src macros)
  (export make-macro-env macro-define! macro-expand
          make-syntax-rules-transformer)
  (import (rnrs)
          (rnrs mutable-pairs))

  (define rename-counter 0)

  (define (fresh-rename sym)
    "Generates a fresh renamed symbol for hygiene.
     sym : symbol (original name)
     returns : unique symbol
     O(1)"
    (set! rename-counter (+ rename-counter 1))
    (string->symbol
     (string-append (symbol->string sym)
                    "."
                    (number->string rename-counter))))

  (define (make-macro-env)
    "Creates an empty macro environment.
     returns : mutable alist
     O(1)"
    (list (cons '*macros* '())))

  (define (macro-define! name transformer menv)
    "Registers a macro transformer in the macro environment.
     name : symbol
     transformer : procedure (list -> s-expression)
     menv : macro environment
     returns : void
     O(1)"
    (let ((entry (assq name (cdr menv))))
      (if entry
          (set-cdr! entry transformer)
          (set-cdr! menv
                    (cons (cons name transformer)
                          (cdr menv))))))

  (define (macro-lookup name menv)
    "Looks up a macro transformer by name.
     name : symbol
     menv : macro environment
     returns : transformer or #f
     O(n) where n = number of macros"
    (let ((entry (assq name (cdr menv))))
      (if entry (cdr entry) #f)))

  (define (pattern-match pattern form literals)
    "Matches a syntax-rules pattern against a form.
     pattern : s-expression pattern with literals and vars
     form : s-expression to match
     literals : list of literal symbols
     returns : alist of bindings or #f on failure
     O(n) in pattern size"
    (cond
     ((and (symbol? pattern)
           (memq pattern literals))
      (if (and (symbol? form) (eq? form pattern))
          '()
          #f))
     ((symbol? pattern)
      (if (eq? pattern '_)
          '()
          (list (cons pattern form))))
     ((and (pair? pattern) (pair? form)
           (has-ellipsis? pattern))
      (match-ellipsis pattern form literals))
     ((and (pair? pattern) (pair? form))
      (let ((head-binds (pattern-match
                         (car pattern) (car form)
                         literals))
            (tail-binds (pattern-match
                         (cdr pattern) (cdr form)
                         literals)))
        (if (and head-binds tail-binds)
            (append head-binds tail-binds)
            #f)))
     ((and (null? pattern) (null? form)) '())
     ((equal? pattern form) '())
     (else #f)))

  (define (has-ellipsis? pattern)
    "Predicate: true if pattern contains ... at second position.
     pattern : s-expression
     returns : boolean
     O(1)"
    (and (pair? pattern)
         (pair? (cdr pattern))
         (eq? (cadr pattern) '...)))

  (define (match-ellipsis pattern form literals)
    "Matches an ellipsis pattern against repeated forms.
     pattern : (sub-pattern ... . rest)
     form : list of forms
     literals : literal symbols
     returns : alist of bindings or #f
     O(n·m)"
    (let ((sub-pat (car pattern))
          (rest-pat (cddr pattern)))
      (let loop ((remaining form) (collected '()))
        (let ((rest-match (pattern-match
                           rest-pat remaining literals)))
          (if rest-match
              (let ((merged (merge-ellipsis-bindings
                             (reverse collected))))
                (if merged
                    (append merged rest-match)
                    #f))
              (if (null? remaining)
                  #f
                  (let ((sub-match
                         (pattern-match
                          sub-pat (car remaining)
                          literals)))
                    (if sub-match
                        (loop (cdr remaining)
                              (cons sub-match collected))
                        #f))))))))

  (define (merge-ellipsis-bindings matches)
    "Merges bindings from repeated ellipsis matches.
     matches : list of binding alists
     returns : alist with list-valued bindings
     O(n·m)"
    (if (null? matches)
        '()
        (let ((names (map car (car matches))))
          (map (lambda (name)
                 (cons name
                       (map (lambda (m)
                              (let ((b (assq name m)))
                                (if b (cdr b)
                                    (error 'merge
                                     "missing binding"
                                     name))))
                            matches)))
               names))))

  (define (template-substitute template bindings)
    "Substitutes bindings into a template.
     template : s-expression with pattern variables
     bindings : alist from pattern-match
     returns : expanded s-expression
     O(n) in template size"
    (cond
     ((symbol? template)
      (let ((b (assq template bindings)))
        (if b (cdr b) template)))
     ((and (pair? template)
           (pair? (cdr template))
           (eq? (cadr template) '...))
      (let ((rest (template-substitute
                   (cddr template) bindings)))
        (append
         (expand-ellipsis-template (car template)
                                   bindings)
         rest)))
     ((pair? template)
      (cons (template-substitute (car template)
                                 bindings)
            (template-substitute (cdr template)
                                 bindings)))
     (else template)))

  (define (expand-ellipsis-template sub-template bindings)
    "Expands an ellipsis sub-template across list bindings.
     sub-template : template for one iteration
     bindings : alist (some values are lists for ellipsis)
     returns : list of expanded forms
     O(n·m)"
    (let ((vars (template-vars sub-template)))
      (let ((list-bindings
             (filter (lambda (b)
                       (and (memq (car b) vars)
                            (list? (cdr b))))
                     bindings)))
        (if (null? list-bindings)
            '()
            (let ((count (length (cdar list-bindings))))
              (let loop ((i 0) (acc '()))
                (if (= i count)
                    (reverse acc)
                    (let ((iter-bindings
                           (map (lambda (b)
                                  (if (and (memq (car b) vars)
                                           (list? (cdr b)))
                                      (cons (car b)
                                            (list-ref (cdr b) i))
                                      b))
                                bindings)))
                      (loop (+ i 1)
                            (cons (template-substitute
                                   sub-template
                                   iter-bindings)
                                  acc))))))))))

  (define (template-vars template)
    "Collects all symbols from a template.
     template : s-expression
     returns : list of symbols
     O(n)"
    (cond
     ((symbol? template) (list template))
     ((pair? template)
      (append (template-vars (car template))
              (template-vars (cdr template))))
     (else '())))

  (define (pattern-variables pattern literals)
    "Collects all pattern variable names from a pattern.
     pattern : s-expression pattern
     literals : list of literal symbols
     returns : list of symbols
     O(n)"
    (cond
     ((and (symbol? pattern)
           (not (memq pattern literals))
           (not (eq? pattern '_))
           (not (eq? pattern '...)))
      (list pattern))
     ((pair? pattern)
      (append (pattern-variables (car pattern) literals)
              (pattern-variables (cdr pattern) literals)))
     (else '())))

  (define (template-bindings template pat-vars)
    "Finds binding-position symbols in template not from pattern.
     template : s-expression
     pat-vars : list of pattern variable symbols
     returns : list of introduced binding symbols
     O(n)"
    (cond
     ((not (pair? template)) '())
     ((or (eq? (car template) 'let)
          (eq? (car template) 'let*))
      (if (and (pair? (cdr template))
               (pair? (cadr template)))
          (let ((names (map car (cadr template))))
            (append
             (filter (lambda (s)
                       (and (symbol? s)
                            (not (eq? s '...))
                            (not (memq s pat-vars))))
                     names)
             (template-bindings (cddr template)
                                pat-vars)))
          (template-bindings (cdr template) pat-vars)))
     ((eq? (car template) 'lambda)
      (if (pair? (cdr template))
          (let ((params (cadr template)))
            (append
             (filter (lambda (s)
                       (and (symbol? s)
                            (not (eq? s '...))
                            (not (memq s pat-vars))))
                     (if (list? params) params '()))
             (template-bindings (cddr template)
                                pat-vars)))
          '()))
     (else
      (append (template-bindings (car template) pat-vars)
              (template-bindings (cdr template)
                                 pat-vars)))))

  (define (make-syntax-rules-transformer literals clauses)
    "Builds a macro transformer from syntax-rules clauses.
     literals : list of literal symbols
     clauses : list of (pattern template) pairs
     returns : transformer procedure
     O(1) (deferred matching at expansion time)"
    (lambda (form)
      (let loop ((cls clauses))
        (if (null? cls)
            (error 'syntax-rules "no matching clause"
                   form)
            (let* ((pattern (cdar (car cls)))
                   (template (cadar cls))
                   (bindings (pattern-match
                              pattern (cdr form)
                              literals)))
              (if bindings
                  (let* ((pat-vars
                          (pattern-variables
                           (car (car cls)) literals))
                         (introduced
                          (template-bindings
                           template pat-vars))
                         (renames
                          (map (lambda (s)
                                 (cons s (fresh-rename s)))
                               introduced))
                         (hygienic-template
                          (if (null? renames)
                              template
                              (substitute-all
                               template renames))))
                    (template-substitute
                     hygienic-template bindings))
                  (loop (cdr cls))))))))

  (define (alpha-rename expr introduced-names)
    "Renames introduced bindings for hygiene.
     expr : s-expression (expanded macro output)
     introduced-names : list of symbols to rename
     returns : renamed s-expression + rename map
     O(n) in expression size"
    (let ((renames (map (lambda (s)
                          (cons s (fresh-rename s)))
                        introduced-names)))
      (substitute-all expr renames)))

  (define (substitute-all expr renames)
    "Replaces all occurrences per rename map.
     expr : s-expression
     renames : alist of (old . new) symbol pairs
     returns : s-expression with substitutions
     O(n·m)"
    (cond
     ((symbol? expr)
      (let ((r (assq expr renames)))
        (if r (cdr r) expr)))
     ((pair? expr)
      (cons (substitute-all (car expr) renames)
            (substitute-all (cdr expr) renames)))
     (else expr)))

  (define (find-introduced-bindings expanded original-vars)
    "Finds symbols in binding positions not from user code.
     expanded : expanded s-expression
     original-vars : symbols from the macro call site
     returns : list of introduced symbols
     O(n)"
    (let ((bound (collect-bound-vars expanded)))
      (filter (lambda (s)
                (not (memq s original-vars)))
              bound)))

  (define (collect-bound-vars expr)
    "Collects symbols in binding positions.
     expr : s-expression
     returns : list of symbols
     O(n)"
    (cond
     ((not (pair? expr)) '())
     ((eq? (car expr) 'lambda)
      (append (cadr expr)
              (collect-bound-vars-list (cddr expr))))
     ((eq? (car expr) 'let)
      (append (map car (cadr expr))
              (collect-bound-vars-list
               (map cadr (cadr expr)))
              (collect-bound-vars-list (cddr expr))))
     (else (collect-bound-vars-list expr))))

  (define (collect-bound-vars-list exprs)
    "Collects bound vars from a list of expressions.
     exprs : list of s-expressions
     returns : list of symbols
     O(n)"
    (if (not (pair? exprs))
        '()
        (append (collect-bound-vars (car exprs))
                (collect-bound-vars-list (cdr exprs)))))

  (define (collect-user-vars form)
    "Collects all symbols from a macro call form.
     form : s-expression (the macro invocation)
     returns : list of symbols
     O(n)"
    (cond
     ((symbol? form) (list form))
     ((pair? form)
      (append (collect-user-vars (car form))
              (collect-user-vars (cdr form))))
     (else '())))

  (define (macro-expand expr menv)
    "Recursively expands macros in an expression.
     expr : s-expression
     menv : macro environment
     returns : fully expanded s-expression
     O(n) in expression size (per pass)"
    (cond
     ((not (pair? expr)) expr)
     ((eq? (car expr) 'quote) expr)
     ((eq? (car expr) 'define-syntax)
      (let ((name (cadr expr))
            (spec (caddr expr)))
        (if (and (pair? spec)
                 (eq? (car spec) 'syntax-rules))
            (let ((lits (cadr spec))
                  (clauses (cddr spec)))
              (macro-define!
               name
               (make-syntax-rules-transformer
                lits clauses)
               menv)
              '(if #f #f))
            (error 'macro-expand
                   "expected syntax-rules" spec))))
     ((and (symbol? (car expr))
           (macro-lookup (car expr) menv))
      => (lambda (transformer)
           (macro-expand (transformer expr) menv)))
     (else
      (map (lambda (sub) (macro-expand sub menv))
           expr)))))
