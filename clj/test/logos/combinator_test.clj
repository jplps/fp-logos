(ns logos.combinator-test
  (:require [clojure.test :refer [deftest is testing]]
            [logos.combinator :as c]))

(deftest fp-compose-test
  (testing "two functions"
    (is (= 5 ((c/fp-compose inc #(* 2 %)) 2))))
  (testing "three functions"
    (is (= 10 ((c/fp-compose #(* 2 %) inc #(* 2 %)) 2))))
  (testing "identity law"
    (is (= 7 ((c/fp-compose identity inc) 6))))
  (testing "associativity"
    (let [f inc g #(* 2 %) h #(* % %)]
      (is (= ((c/fp-compose (c/fp-compose f g) h) 3)
             ((c/fp-compose f (c/fp-compose g h)) 3))))))

(deftest fp-construct-test
  (testing "two functions"
    (is (= [4 3] ((c/fp-construct #(* % %) inc) 2))))
  (testing "three functions"
    (is (= [9 6 4] ((c/fp-construct #(* % %) #(* 2 %) inc) 3)))))

(deftest fp-insert-test
  (testing "sum"
    (is (= 10 ((c/fp-insert +) [1 2 3 4]))))
  (testing "product"
    (is (= 24 ((c/fp-insert *) [1 2 3 4]))))
  (testing "singleton"
    (is (= 5 ((c/fp-insert +) [5])))))

(deftest fp-apply-to-all-test
  (testing "square"
    (is (= [1 4 9] (vec ((c/fp-apply-to-all #(* % %)) [1 2 3])))))
  (testing "empty"
    (is (= [] (vec ((c/fp-apply-to-all #(* % %)) []))))))

(deftest combined-insert-apply-to-all-test
  (testing "sum of squares"
    (is (= 14 ((c/fp-insert +) ((c/fp-apply-to-all #(* % %)) [1 2 3])))))
  (testing "composed pipeline"
    (is (= 14 ((c/fp-compose (c/fp-insert +) (c/fp-apply-to-all #(* % %)))
               [1 2 3])))))

(deftest map-fusion-test
  (testing "map fusion law"
    (let [f inc
          g #(* 2 %)
          xs [1 2 3 4 5]]
      (is (= (vec ((c/fp-apply-to-all (comp f g)) xs))
             (vec ((comp (c/fp-apply-to-all f)
                         (c/fp-apply-to-all g)) xs)))))))

(deftest sqrt-approximations-test
  (testing "converges toward sqrt(2)"
    (let [approxs (take 10 (c/sqrt-approximations 2))]
      (is (< (abs (- (last approxs) (Math/sqrt 2))) 1e-10))))
  (testing "sqrt-within accuracy"
    (is (< (abs (- (c/sqrt-within 2 1e-12) (Math/sqrt 2))) 1e-12)))
  (testing "sqrt-within for 9"
    (is (< (abs (- (c/sqrt-within 9 1e-10) 3.0)) 1e-10))))

(deftest differentiate-test
  (testing "derivative of x^2 at x=3 is 6"
    (let [approxs (c/differentiate #(* % %) 3.0 1.0)]
      (is (< (abs (- (c/within 1e-6 approxs) 6.0)) 1e-5))))
  (testing "improved differentiation"
    (let [approxs (c/differentiate #(* % %) 3.0 1.0)]
      (is (< (abs (- (first (c/super approxs)) 6.0)) 1e-3)))))

(deftest alpha-beta-test
  (let [moves-fn (fn [pos]
                   (case pos
                     :root [:a :b]
                     :a [:a1 :a2]
                     :b [:b1 :b2]
                     []))
        static-fn {:a1 3 :a2 5 :b1 2 :b2 9}]
    (testing "alpha-beta on known tree: max(min(3,5), min(2,9)) = 3"
      (is (= 3 (c/alpha-beta static-fn moves-fn 2 :root))))))

(deftest prune-test
  (testing "prune to depth 0"
    (is (= [:root []] (c/prune 0 [:root [[:a []] [:b []]]]))))
  (testing "prune to depth 1"
    (is (= [:root [[:a []] [:b []]]]
           (c/prune 1 [:root [[:a [[:c []]]] [:b [[:d []]]]]])))))
