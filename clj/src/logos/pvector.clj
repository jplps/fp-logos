(ns logos.pvector
  "Simplified 32-way persistent vector using bit-partitioned trie.
   From Hickey, 'A History of Clojure' (2020, HOPL IV).")

(def ^:private bits 5)
(def ^:private width 32)
(def ^:private mask 0x1f)

(defn make-pvector
  "Creates an empty persistent vector.
   returns : pvector record
   O(1)"
  []
  {:cnt 0 :shift bits :root nil :tail []})

(defn pvector-count
  "Returns the number of elements in a persistent vector.
   v : pvector
   returns : non-negative integer
   O(1)"
  [{:keys [cnt]}]
  cnt)

(defn pvector-empty?
  "Predicate: true iff vector has no elements.
   v : pvector
   returns : boolean
   O(1)"
  [{:keys [cnt]}]
  (zero? cnt))

(defn- tail-off
  "Index where the tail segment begins.
   cnt : element count
   returns : non-negative integer
   O(1)"
  [cnt]
  (if (< cnt width)
    0
    (bit-shift-left (unsigned-bit-shift-right (dec cnt) bits)
                    bits)))

(defn- array-for
  "Returns the leaf array containing index i.
   v : pvector
   i : valid index
   returns : vector (leaf node or tail)
   O(log32 n)"
  [{:keys [cnt shift root tail]} i]
  (if (>= i (tail-off cnt))
    tail
    (loop [node root level shift]
      (if (zero? level)
        node
        (recur (nth node (bit-and (unsigned-bit-shift-right i level) mask))
               (- level bits))))))

(defn pvector-nth
  "Returns the element at index i.
   v : pvector
   i : non-negative integer, 0 <= i < (pvector-count v)
   returns : element at index i
   O(log32 n)"
  [{:keys [cnt] :as v} i]
  (when (or (neg? i) (>= i cnt))
    (throw (ex-info "index out of bounds" {:index i :count cnt})))
  (let [node (array-for v i)]
    (nth node (bit-and i mask))))

(defn- new-path
  "Creates a single-branch path from level down to a leaf.
   level : trie level (multiple of bits)
   leaf : leaf node vector
   returns : nested node structure
   O(level / bits)"
  [level leaf]
  (if (zero? level)
    leaf
    (let [node (vec (repeat width nil))]
      (assoc node 0 (new-path (- level bits) leaf)))))

(defn- push-tail
  "Inserts tail-node into the trie at the correct position.
   cnt : current element count
   level : trie level
   parent : parent node vector
   tail-node : leaf vector to insert
   returns : updated parent node
   O(log32 n)"
  [cnt level parent tail-node]
  (let [subidx (bit-and (unsigned-bit-shift-right (dec cnt) level) mask)
        parent (or parent (vec (repeat width nil)))]
    (if (= level bits)
      (assoc parent subidx tail-node)
      (let [child (nth parent subidx)]
        (assoc parent subidx
               (if child
                 (push-tail cnt (- level bits) child tail-node)
                 (new-path (- level bits) tail-node)))))))

(defn pvector-conj
  "Appends an element to the end of the persistent vector.
   v : pvector
   val : element to append
   returns : new pvector with val appended
   O(log32 n) — effectively O(1)"
  [{:keys [cnt shift root tail] :as v} val]
  (if (< (- cnt (tail-off cnt)) width)
    (assoc v :cnt (inc cnt) :tail (conj tail val))
    (let [new-tail [val]
          overflow? (> (unsigned-bit-shift-right (dec cnt) shift)
                       mask)]
      (if overflow?
        (let [new-root (vec (repeat width nil))]
          {:cnt (inc cnt)
           :shift (+ shift bits)
           :root (-> new-root
                     (assoc 0 root)
                     (assoc 1 (new-path shift tail)))
           :tail new-tail})
        {:cnt (inc cnt)
         :shift shift
         :root (push-tail cnt shift root tail)
         :tail new-tail}))))

(defn- do-assoc
  "Path-copies trie to update index i with val.
   level : trie level
   node : trie node
   i : index
   val : new value
   returns : updated node
   O(log32 n)"
  [level node i val]
  (let [subidx (bit-and (unsigned-bit-shift-right i level) mask)]
    (if (zero? level)
      (assoc node subidx val)
      (assoc node subidx
             (do-assoc (- level bits) (nth node subidx) i val)))))

(defn pvector-assoc
  "Associates val at index i in persistent vector v.
   Uses 32-way bit-partitioned trie path copying.
   v : pvector
   i : int, 0 <= i < (pvector-count v)
   val : new value
   returns : updated pvector
   O(log32 n) — effectively O(1) for practical n"
  [{:keys [cnt shift root tail] :as v} i val]
  (when (or (neg? i) (>= i cnt))
    (throw (ex-info "index out of bounds" {:index i :count cnt})))
  (if (>= i (tail-off cnt))
    (assoc v :tail (assoc tail (- i (tail-off cnt)) val))
    (assoc v :root (do-assoc shift root i val))))

(defn pvector->vec
  "Converts a pvector to a standard Clojure vector.
   v : pvector
   returns : vector
   O(n log32 n)"
  [v]
  (mapv #(pvector-nth v %) (range (pvector-count v))))

(defn vec->pvector
  "Converts a standard Clojure vector to a pvector.
   coll : sequential collection
   returns : pvector
   O(n)"
  [coll]
  (reduce pvector-conj (make-pvector) coll))
