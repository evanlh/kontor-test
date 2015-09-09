(ns kontor-test.core-test
  (:require [clojure.test :refer :all]
            [kontor-test.core :refer :all]))

(def cart-1 (cart-init))
(def cart-2 (cart-init))
(def cart-3 (cart-init))

(deftest test-1
  (testing "TEST 1, ABCDABAA"
    (reset! cart-1 (set-pricing @cart-1 test-pricing))
    (reset! cart-1 (scan @cart-1 test-items-1))
    (is (= (calculate-total @cart-1) 32.40))))

(deftest test-2
  (testing "TEST 2, CCCCCCC"
    (reset! cart-2 (set-pricing @cart-2 test-pricing))
    (reset! cart-2 (scan @cart-2 test-items-2))
    (is (= (calculate-total @cart-2) 7.25))))

(deftest test-3
  (testing "TEST 2, ABCD"
    (reset! cart-3 (set-pricing @cart-3 test-pricing))
    (reset! cart-3 (scan @cart-3 test-items-3))
    (is (= (calculate-total @cart-3) 15.40))))
