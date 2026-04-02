(ns logos.combinator
  "Point-free combinator library and numerical algorithms
   from Hughes, 'Why Functional Programming Matters' (1989).
   Demonstrates modularity via higher-order functions and lazy evaluation.")

(defn fp-compose
  "Right-to-left composition of functions.
   fns : one or more functions
   returns : composed function
   O(n) where n = number of functions"
  [& fns]
  (reduce comp fns))

(defn fp-construct
  "Applies each function to the same argument, collecting results.
   fns : one or more functions
   returns : function producing a vector of results
   O(n) where n = number of functions"
  [& fns]
  (fn [x] (mapv #(% x) fns)))

(defn fp-insert
  "Reduces a sequence with f (right fold).
   f : binary function
   returns : function from non-empty sequence to value
   O(n) where n = length of sequence"
  [f]
  (fn [coll] (reduce f coll)))

(defn fp-apply-to-all
  "Applies f to every element of a sequence.
   f : unary function
   returns : function from sequence to lazy sequence
   O(n) where n = length of sequence"
  [f]
  (fn [coll] (map f coll)))


(defn within
  "Returns the first element of a converging sequence where
   successive elements differ by less than eps.
   eps : tolerance (positive number)
   s : lazy sequence of approximations
   returns : first sufficiently close approximation
   O(k) where k = steps to convergence"
  [eps s]
  (let [a (first s)
        b (second s)]
    (if (< (abs (- a b)) eps)
      b
      (recur eps (rest s)))))

(defn relative-within
  "Like within, but uses relative difference.
   eps : relative tolerance
   s : lazy sequence of approximations
   returns : first relatively close approximation
   O(k) where k = steps to convergence"
  [eps s]
  (let [a (first s)
        b (second s)]
    (if (or (zero? b)
            (< (abs (/ (- a b) b)) eps))
      b
      (recur eps (rest s)))))

(defn sqrt-approximations
  "Lazy sequence of Newton-Raphson approximations to sqrt(n).
   n : positive number
   returns : lazy sequence converging to sqrt(n)
   O(1) per element"
  [n]
  (iterate (fn [x] (/ (+ x (/ n x)) 2.0)) 1.0))

(defn sqrt-within
  "Square root via Newton-Raphson with absolute tolerance.
   n : positive number
   eps : tolerance
   returns : approximation to sqrt(n)
   O(k) where k = steps to convergence"
  [n eps]
  (within eps (sqrt-approximations n)))

(defn differentiate
  "Lazy sequence of central-difference approximations to f'(x).
   f : differentiable function
   x : point of differentiation
   h0 : initial step size
   returns : lazy sequence of approximations
   O(1) per element"
  [f x h0]
  (let [halve (fn step [h]
                (lazy-seq
                 (let [approx (/ (- (f (+ x h)) (f (- x h)))
                                 (* 2.0 h))]
                   (cons approx (step (/ h 2.0))))))]
    (halve h0)))

(defn eliminate-error
  "Eliminates leading error term from a sequence of approximations
   using Richardson extrapolation.
   n : order of error term
   s : lazy sequence of approximations
   returns : improved lazy sequence
   O(1) per element"
  [n s]
  (let [a (first s)
        b (second s)]
    (lazy-seq
     (cons (/ (- (* b (Math/pow 2 n)) a)
              (- (Math/pow 2 n) 1))
           (eliminate-error n (rest s))))))

(defn order
  "Estimates the order of the error term in a sequence.
   s : lazy sequence of approximations (at least 3 elements)
   returns : estimated order (integer), minimum 1
   O(1)"
  [s]
  (let [a (first s)
        b (second s)
        c (nth s 2)
        denom (- b c)]
    (if (zero? denom)
      1
      (max 1 (long (Math/round
                     (/ (Math/log (abs (/ (- a c) denom)))
                        (Math/log 2))))))))

(defn improve
  "Improves a sequence by eliminating its leading error term.
   s : lazy sequence of approximations
   returns : improved lazy sequence
   O(1) per element"
  [s]
  (eliminate-error (order s) s))

(defn super
  "Repeatedly improves a sequence (tableau method).
   s : lazy sequence of approximations
   returns : lazy sequence of successively improved first elements
   O(1) per element"
  [s]
  (lazy-seq
   (cons (first s) (super (improve s)))))


(defn game-tree
  "Builds a game tree from a position using move generator.
   moves-fn : position -> sequence of positions
   pos : starting position
   returns : lazy tree [pos [child-tree ...]]
   O(branching-factor^depth) total, O(1) per node (lazy)"
  [moves-fn pos]
  [pos (map #(game-tree moves-fn %) (moves-fn pos))])

(defn tree-map
  "Applies f to every node label in a tree.
   f : function on labels
   tree : [label children]
   returns : transformed tree
   O(n) in tree size"
  [f [label children]]
  [(f label) (map #(tree-map f %) children)])

(declare minimise)

(defn maximise
  "Computes the minimax value by maximising at this level.
   tree : [value children] game tree with static values
   returns : minimax value
   O(n) in tree size"
  [[label children]]
  (if (empty? children)
    label
    (apply max (map minimise children))))

(defn minimise
  "Computes the minimax value by minimising at this level.
   tree : [value children] game tree with static values
   returns : minimax value
   O(n) in tree size"
  [[label children]]
  (if (empty? children)
    label
    (apply min (map maximise children))))

(defn prune
  "Prunes a game tree to a given depth.
   depth : non-negative integer
   tree : [label children]
   returns : pruned tree
   O(branching-factor^depth)"
  [depth [label children]]
  (if (zero? depth)
    [label []]
    [label (map #(prune (dec depth) %) children)]))

(defn alpha-beta
  "Alpha-beta pruning evaluator for a game tree.
   static-fn : position -> numeric value
   moves-fn : position -> seq of positions
   depth : search depth
   pos : current position
   returns : best value for maximising player
   O(b^(d/2)) average where b = branching, d = depth"
  [static-fn moves-fn depth pos]
  (-> (game-tree moves-fn pos)
      (->> (prune depth))
      (->> (tree-map static-fn))
      maximise))
