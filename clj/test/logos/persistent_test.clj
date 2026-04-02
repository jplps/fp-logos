(ns logos.persistent-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [logos.persistent :as p]))

(deftest queue-basic-test
  (testing "empty queue"
    (is (p/queue-empty? (p/make-queue))))
  (testing "enqueue and peek"
    (is (= 1 (p/queue-peek (p/enqueue (p/make-queue) 1)))))
  (testing "FIFO order"
    (let [q (-> (p/make-queue)
                (p/enqueue 1) (p/enqueue 2) (p/enqueue 3))]
      (is (= [1 2 3] (vec (p/queue->list q))))))
  (testing "dequeue"
    (let [q (-> (p/make-queue)
                (p/enqueue 1) (p/enqueue 2) (p/enqueue 3))
          q2 (p/dequeue q)]
      (is (= 2 (p/queue-peek q2)))
      (is (= [2 3] (vec (p/queue->list q2)))))))

(defspec queue-fifo-property 100
  (prop/for-all [xs (gen/vector gen/small-integer)]
    (let [q (reduce p/enqueue (p/make-queue) xs)]
      (= (seq xs) (seq (p/queue->list q))))))

(defspec queue-enqueue-dequeue-property 100
  (prop/for-all [xs (gen/not-empty (gen/vector gen/small-integer))]
    (let [q (reduce p/enqueue (p/make-queue) xs)]
      (= (first xs) (p/queue-peek q)))))


(deftest heap-basic-test
  (testing "empty heap"
    (is (p/heap-empty? (p/make-heap))))
  (testing "insert and find-min"
    (is (= 1 (p/heap-find-min
              (reduce p/heap-insert (p/make-heap) [3 1 2])))))
  (testing "sorted extraction"
    (let [h (reduce p/heap-insert (p/make-heap) [5 3 8 1 4])]
      (is (= [1 3 4 5 8] (vec (p/heap->sorted-list h)))))))

(defspec heap-sorted-property 100
  (prop/for-all [xs (gen/not-empty (gen/vector gen/small-integer))]
    (let [h (reduce p/heap-insert (p/make-heap) xs)]
      (= (sort xs) (vec (p/heap->sorted-list h))))))

(defspec heap-min-property 100
  (prop/for-all [xs (gen/not-empty (gen/vector gen/small-integer))]
    (let [h (reduce p/heap-insert (p/make-heap) xs)]
      (= (apply min xs) (p/heap-find-min h)))))

(defspec heap-merge-property 50
  (prop/for-all [xs (gen/vector gen/small-integer)
                 ys (gen/vector gen/small-integer)]
    (let [h1 (reduce p/heap-insert (p/make-heap) xs)
          h2 (reduce p/heap-insert (p/make-heap) ys)
          merged (p/heap-merge h1 h2)]
      (= (sort (concat xs ys))
         (vec (p/heap->sorted-list merged))))))


(deftest rb-tree-basic-test
  (testing "empty tree"
    (is (p/rb-empty? (p/rb-tree))))
  (testing "insert and lookup"
    (let [t (-> (p/rb-tree)
                (p/rb-insert 3 "c")
                (p/rb-insert 1 "a")
                (p/rb-insert 2 "b"))]
      (is (= "a" (p/rb-lookup t 1)))
      (is (= "b" (p/rb-lookup t 2)))
      (is (= "c" (p/rb-lookup t 3)))
      (is (nil? (p/rb-lookup t 4)))))
  (testing "member?"
    (let [t (-> (p/rb-tree)
                (p/rb-insert 5 :x)
                (p/rb-insert 3 :y))]
      (is (p/rb-member? t 5))
      (is (not (p/rb-member? t 7)))))
  (testing "sorted order"
    (let [t (-> (p/rb-tree)
                (p/rb-insert 3 "c")
                (p/rb-insert 1 "a")
                (p/rb-insert 2 "b"))]
      (is (= [[1 "a"] [2 "b"] [3 "c"]]
             (p/rb->sorted-list t)))))
  (testing "update existing key"
    (let [t (-> (p/rb-tree)
                (p/rb-insert 1 "old")
                (p/rb-insert 1 "new"))]
      (is (= "new" (p/rb-lookup t 1)))
      (is (= 1 (p/rb-size t))))))

(defn- rb-black-height
  "Returns the black height if consistent, or -1 if violated."
  [t]
  (if (nil? t)
    1
    (let [lh (rb-black-height (:left t))
          rh (rb-black-height (:right t))]
      (if (or (= lh -1) (= rh -1) (not= lh rh))
        -1
        (if (= (:color t) :black) (inc lh) lh)))))

(defn- rb-no-red-red?
  "Returns true if no red node has a red child."
  [t]
  (if (nil? t)
    true
    (let [left-ok (rb-no-red-red? (:left t))
          right-ok (rb-no-red-red? (:right t))]
      (if (= (:color t) :red)
        (and left-ok right-ok
             (or (nil? (:left t))
                 (= (:color (:left t)) :black))
             (or (nil? (:right t))
                 (= (:color (:right t)) :black)))
        (and left-ok right-ok)))))

(defspec rb-balance-property 200
  (prop/for-all [xs (gen/not-empty
                     (gen/vector
                      (gen/tuple gen/small-integer
                                 gen/small-integer)))]
    (let [t (reduce (fn [tree [k v]] (p/rb-insert tree k v))
                    (p/rb-tree)
                    xs)]
      (and (pos? (rb-black-height t))
           (rb-no-red-red? t)
           (= (:color t) :black)))))

(defspec rb-sorted-property 100
  (prop/for-all [xs (gen/not-empty
                     (gen/vector
                      (gen/tuple gen/small-integer
                                 gen/small-integer)))]
    (let [t (reduce (fn [tree [k v]] (p/rb-insert tree k v))
                    (p/rb-tree)
                    xs)
          sorted-keys (map first (p/rb->sorted-list t))]
      (= sorted-keys (sort sorted-keys)))))

(defspec rb-lookup-property 100
  (prop/for-all [xs (gen/not-empty
                     (gen/vector
                      (gen/tuple gen/small-integer
                                 gen/small-integer)))]
    (let [t (reduce (fn [tree [k v]] (p/rb-insert tree k v))
                    (p/rb-tree)
                    xs)
          expected (into {} xs)]
      (every? (fn [[k v]] (= v (p/rb-lookup t k))) expected))))
