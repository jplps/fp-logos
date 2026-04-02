(ns logos.persistent
  "Purely functional data structures from Okasaki (1998).
   Banker's queue, leftist heap, and red-black tree.")


(defn make-queue
  "Creates an empty banker's queue.
   returns : queue record
   O(1)"
  []
  {:front [] :rear [] :front-len 0 :rear-len 0})

(defn queue-empty?
  "Predicate: true iff queue has no elements.
   q : queue
   returns : boolean
   O(1)"
  [{:keys [front-len]}]
  (zero? front-len))

(defn- queue-check
  "Rebalances queue if rear exceeds front.
   q : queue
   returns : balanced queue
   O(n) amortized O(1)"
  [{:keys [front rear front-len rear-len] :as q}]
  (if (> rear-len front-len)
    {:front (into front rear)
     :rear []
     :front-len (+ front-len rear-len)
     :rear-len 0}
    q))

(defn enqueue
  "Adds an element to the rear of the queue.
   q : queue
   x : element
   returns : queue with x added
   O(1) amortized"
  [{:keys [rear rear-len] :as q} x]
  (queue-check
   (assoc q
          :rear (conj rear x)
          :rear-len (inc rear-len))))

(defn queue-peek
  "Returns the front element without removing it.
   q : non-empty queue
   returns : front element
   O(1)"
  [{:keys [front]}]
  (first front))

(defn dequeue
  "Removes the front element from the queue.
   q : non-empty queue
   returns : queue without front element
   O(1) amortized"
  [{:keys [front front-len] :as q}]
  (queue-check
   (assoc q
          :front (subvec front 1)
          :front-len (dec front-len))))

(defn queue->list
  "Converts a queue to a list (front to rear order).
   q : queue
   returns : list of elements
   O(n)"
  [{:keys [front rear]}]
  (concat front rear))


(defn make-heap
  "Creates an empty leftist heap.
   returns : nil (empty heap)
   O(1)"
  []
  nil)

(defn heap-empty?
  "Predicate: true iff heap has no elements.
   h : heap
   returns : boolean
   O(1)"
  [h]
  (nil? h))

(defn- heap-rank
  "Returns the rank (right spine length) of a heap node.
   h : heap or nil
   returns : non-negative integer
   O(1)"
  [h]
  (if (nil? h) 0 (:rank h)))

(defn- make-heap-node
  "Creates a heap node, maintaining leftist property.
   val : element
   left : left subtree
   right : right subtree
   returns : heap node with correct rank
   O(1)"
  [val left right]
  (if (>= (heap-rank left) (heap-rank right))
    {:val val :left left :right right
     :rank (inc (heap-rank right))}
    {:val val :left right :right left
     :rank (inc (heap-rank left))}))

(defn heap-merge
  "Merges two leftist heaps.
   h1, h2 : heaps (nil = empty)
   returns : merged heap
   O(log n) where n = total elements"
  [h1 h2]
  (cond
    (nil? h1) h2
    (nil? h2) h1
    (<= (:val h1) (:val h2))
    (make-heap-node (:val h1) (:left h1)
                    (heap-merge (:right h1) h2))
    :else
    (make-heap-node (:val h2) (:left h2)
                    (heap-merge h1 (:right h2)))))

(defn heap-insert
  "Inserts a value into a leftist heap.
   h : heap
   val : comparable element
   returns : heap with val added
   O(log n)"
  [h val]
  (heap-merge h {:val val :left nil :right nil :rank 1}))

(defn heap-find-min
  "Returns the minimum element of a heap.
   h : non-empty heap
   returns : minimum element
   O(1)"
  [h]
  (:val h))

(defn heap-delete-min
  "Removes the minimum element from a heap.
   h : non-empty heap
   returns : heap without minimum
   O(log n)"
  [h]
  (heap-merge (:left h) (:right h)))

(defn heap->sorted-list
  "Extracts all elements in sorted order.
   h : heap
   returns : sorted list
   O(n log n)"
  [h]
  (if (nil? h)
    []
    (cons (heap-find-min h)
          (lazy-seq (heap->sorted-list
                     (heap-delete-min h))))))


(defn rb-tree
  "Creates an empty red-black tree.
   returns : nil (empty tree)
   O(1)"
  []
  nil)

(defn rb-empty?
  "Predicate: true iff tree is empty.
   t : red-black tree
   returns : boolean
   O(1)"
  [t]
  (nil? t))

(defn rb-member?
  "Searches for a key in the red-black tree.
   t : red-black tree
   k : comparable key
   returns : boolean
   O(log n)"
  [t k]
  (cond
    (nil? t) false
    (< k (:key t)) (recur (:left t) k)
    (> k (:key t)) (recur (:right t) k)
    :else true))

(defn rb-lookup
  "Looks up a key in the red-black tree.
   t : red-black tree
   k : comparable key
   returns : value or nil
   O(log n)"
  [t k]
  (cond
    (nil? t) nil
    (< k (:key t)) (recur (:left t) k)
    (> k (:key t)) (recur (:right t) k)
    :else (:val t)))

(defn- rb-balance
  "Balances a red-black tree node after insertion.
   color : :black or :red
   left : left subtree
   key : node key
   val : node value
   right : right subtree
   returns : balanced tree node
   O(1)"
  [color left key val right]
  (cond
    (and (= color :black)
         (some? left) (= (:color left) :red)
         (some? (:left left)) (= (:color (:left left)) :red))
    {:color :red
     :left (assoc (:left left) :color :black)
     :key (:key left) :val (:val left)
     :right {:color :black :left (:right left)
             :key key :val val :right right}}

    (and (= color :black)
         (some? left) (= (:color left) :red)
         (some? (:right left)) (= (:color (:right left)) :red))
    {:color :red
     :left {:color :black :left (:left left)
            :key (:key left) :val (:val left)
            :right (:left (:right left))}
     :key (:key (:right left)) :val (:val (:right left))
     :right {:color :black :left (:right (:right left))
             :key key :val val :right right}}

    (and (= color :black)
         (some? right) (= (:color right) :red)
         (some? (:left right)) (= (:color (:left right)) :red))
    {:color :red
     :left {:color :black :left left
            :key key :val val
            :right (:left (:left right))}
     :key (:key (:left right)) :val (:val (:left right))
     :right {:color :black :left (:right (:left right))
             :key (:key right) :val (:val right)
             :right (:right right)}}

    (and (= color :black)
         (some? right) (= (:color right) :red)
         (some? (:right right)) (= (:color (:right right)) :red))
    {:color :red
     :left {:color :black :left left
            :key key :val val :right (:left right)}
     :key (:key right) :val (:val right)
     :right (assoc (:right right) :color :black)}

    :else
    {:color color :left left :key key :val val :right right}))

(defn rb-insert
  "Inserts a key-value pair into a red-black tree.
   t : red-black tree
   k : comparable key
   v : value
   returns : balanced red-black tree
   O(log n)"
  [t k v]
  (letfn [(ins [node]
            (cond
              (nil? node)
              {:color :red :left nil :key k :val v :right nil}

              (< k (:key node))
              (rb-balance (:color node)
                          (ins (:left node))
                          (:key node) (:val node)
                          (:right node))

              (> k (:key node))
              (rb-balance (:color node)
                          (:left node)
                          (:key node) (:val node)
                          (ins (:right node)))

              :else
              (assoc node :val v)))]
    (assoc (ins t) :color :black)))

(defn rb->sorted-list
  "Converts a red-black tree to a sorted list of [key val] pairs.
   t : red-black tree
   returns : sorted list of pairs
   O(n)"
  [t]
  (if (nil? t)
    []
    (concat (rb->sorted-list (:left t))
            [[(:key t) (:val t)]]
            (rb->sorted-list (:right t)))))

(defn rb-size
  "Returns the number of elements in a red-black tree.
   t : red-black tree
   returns : non-negative integer
   O(n)"
  [t]
  (if (nil? t)
    0
    (+ 1 (rb-size (:left t)) (rb-size (:right t))))
)
