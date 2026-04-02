(ns logos.dsl-test
  (:require [clojure.test :refer [deftest is testing]]
            [logos.dsl :as d]))

(deftest lit-test
  (testing "literal evaluates to its value"
    (is (= 42 (d/evaluate (d/lit 42) {})))))

(deftest var-test
  (testing "variable lookup"
    (is (= 10 (d/evaluate (d/vr :x) {:x 10}))))
  (testing "unbound variable throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (d/evaluate (d/vr :y) {:x 10})))))

(deftest add-test
  (testing "addition"
    (is (= 5 (d/evaluate (d/add (d/lit 2) (d/lit 3)) {}))))
  (testing "nested addition"
    (is (= 6 (d/evaluate (d/add (d/add (d/lit 1) (d/lit 2))
                                (d/lit 3)) {})))))

(deftest mul-test
  (testing "multiplication"
    (is (= 12 (d/evaluate (d/mul (d/lit 3) (d/lit 4)) {}))))
  (testing "combined add and mul"
    (is (= 14 (d/evaluate (d/add (d/mul (d/lit 3) (d/lit 4))
                                 (d/lit 2)) {})))))

(deftest neg-test
  (testing "negation"
    (is (= -5 (d/evaluate (d/neg (d/lit 5)) {})))))

(deftest let-test
  (testing "let binding"
    (is (= 12 (d/evaluate
               (d/let-expr :x (d/lit 5)
                           (d/add (d/vr :x) (d/lit 7)))
               {}))))
  (testing "nested let"
    (is (= 30 (d/evaluate
               (d/let-expr :x (d/lit 10)
                           (d/let-expr :y (d/lit 20)
                                       (d/add (d/vr :x) (d/vr :y))))
               {})))))

(deftest if-zero-test
  (testing "zero branch"
    (is (= 1 (d/evaluate (d/if-zero (d/lit 0) (d/lit 1) (d/lit 2))
                          {}))))
  (testing "nonzero branch"
    (is (= 2 (d/evaluate (d/if-zero (d/lit 5) (d/lit 1) (d/lit 2))
                          {})))))

(deftest pretty-print-test
  (testing "simple expression"
    (is (= "(2 + 3)" (d/pp (d/add (d/lit 2) (d/lit 3))))))
  (testing "complex expression"
    (is (= "((2 * 3) + (- 1))"
           (d/pp (d/add (d/mul (d/lit 2) (d/lit 3))
                        (d/neg (d/lit 1)))))))
  (testing "let expression"
    (is (= "(let [x 5] (x + 7))"
           (d/pp (d/let-expr :x (d/lit 5)
                             (d/add (d/vr :x) (d/lit 7))))))))

(deftest expression-problem-test
  (testing "adding Mul without modifying existing code"
    (let [expr (d/mul (d/add (d/lit 2) (d/lit 3))
                      (d/lit 4))]
      (is (= 20 (d/evaluate expr {})))
      (is (= "((2 + 3) * 4)" (d/pp expr))))))
