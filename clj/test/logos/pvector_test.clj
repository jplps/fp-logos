(ns logos.pvector-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [logos.pvector :as pv]))

(deftest basic-test
  (testing "empty vector"
    (is (pv/pvector-empty? (pv/make-pvector)))
    (is (zero? (pv/pvector-count (pv/make-pvector)))))
  (testing "conj and nth"
    (let [v (-> (pv/make-pvector)
                (pv/pvector-conj 10)
                (pv/pvector-conj 20)
                (pv/pvector-conj 30))]
      (is (= 3 (pv/pvector-count v)))
      (is (= 10 (pv/pvector-nth v 0)))
      (is (= 20 (pv/pvector-nth v 1)))
      (is (= 30 (pv/pvector-nth v 2)))))
  (testing "out of bounds"
    (is (thrown? clojure.lang.ExceptionInfo
                 (pv/pvector-nth (pv/make-pvector) 0)))))

(deftest assoc-test
  (testing "assoc updates value"
    (let [v (pv/vec->pvector [1 2 3])]
      (is (= 99 (pv/pvector-nth (pv/pvector-assoc v 1 99) 1)))))
  (testing "assoc preserves other elements"
    (let [v (pv/vec->pvector [1 2 3])
          v2 (pv/pvector-assoc v 1 99)]
      (is (= [1 99 3] (pv/pvector->vec v2)))))
  (testing "original unchanged (persistence)"
    (let [v (pv/vec->pvector [1 2 3])
          _ (pv/pvector-assoc v 1 99)]
      (is (= [1 2 3] (pv/pvector->vec v))))))

(deftest roundtrip-test
  (testing "vec->pvector->vec roundtrip"
    (let [xs [1 2 3 4 5]]
      (is (= xs (pv/pvector->vec (pv/vec->pvector xs))))))
  (testing "larger collection"
    (let [xs (vec (range 100))]
      (is (= xs (pv/pvector->vec (pv/vec->pvector xs)))))))

(defspec conj-count-property 100
  (prop/for-all [xs (gen/vector gen/small-integer)]
    (= (count xs)
       (pv/pvector-count (pv/vec->pvector xs)))))

(defspec roundtrip-property 100
  (prop/for-all [xs (gen/vector gen/small-integer)]
    (= xs (pv/pvector->vec (pv/vec->pvector xs)))))

(defspec assoc-nth-property 100
  (prop/for-all [xs (gen/not-empty (gen/vector gen/small-integer))
                 val gen/small-integer]
    (let [v (pv/vec->pvector xs)
          i (mod (Math/abs (hash val)) (count xs))
          v2 (pv/pvector-assoc v i val)]
      (= val (pv/pvector-nth v2 i)))))

(defspec persistence-property 50
  (prop/for-all [xs (gen/not-empty
                     (gen/vector gen/small-integer 2 100))
                 val gen/small-integer]
    (let [v (pv/vec->pvector xs)
          i (mod (Math/abs (hash val)) (count xs))
          v2 (pv/pvector-assoc v i val)]
      (= xs (pv/pvector->vec v)))))
