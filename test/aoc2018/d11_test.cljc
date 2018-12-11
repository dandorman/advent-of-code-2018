(ns aoc2018.d11-test
  (:require [aoc2018.d11 :refer [power-levels solve-1 solve-2]]
            [clojure.core.matrix :as mat]
            [clojure.test :refer [deftest are is testing]]))


(deftest power-levels-test
  (are [power-level serial-number x   y] (= power-level
                                            (mat/mget
                                              (power-levels serial-number)
                                              (dec y) (dec x)))
         4           8              3   5
        -5          57            122  79
         0          39            217 196
         4          71            101 153))


(deftest part-1
  (testing "example"
    (is (= [21 61] (solve-1 42))))
  (testing "input"
    (is (= [21 76] (solve-1 3031)))))


(comment
  (deftest part-2
    (testing "example"
      (is (= [90 269 16] (solve-2 18)))
      #_(is (= [232 251 12] (solve-2 42))))
    (comment
      (testing "input"
        (is (= [21 76] (solve-1 3031)))))))
