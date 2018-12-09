(ns aoc2018.d09-test
  (:require [aoc2018.d09 :refer [solve]]
            [clojure.test :refer [are deftest is testing]]))


(deftest part-1
  (testing "examples"
    (are [high-score players last-marble] (= high-score (solve players last-marble))
          32         9       25
          8317       10      1618
          146373     13      7999
          2764       17      1104
          54718      21      6111
          37305      30      5807))
  (testing "input"
    (is (= 374690 (solve 477 70851)))))


(comment ; this blows up CircleCI's memory limit
  (deftest part-2
    (testing "input"
      (is (= 3009951158 (solve 477 7085100))))))
