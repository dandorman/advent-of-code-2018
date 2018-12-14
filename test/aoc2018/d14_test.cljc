(ns aoc2018.d14-test
  (:require [aoc2018.d14 :refer [solve-1 solve-2]]
            [clojure.test :refer [are deftest is testing]]))


(deftest part-1
  (testing "example"
    (are [next-ten     take-after] (= next-ten (solve-1 [3 7] #{0 1} take-after))
          "5158916779" 9
          "0124515891" 5
          "9251071085" 18
          "5941429882" 2018))
  (testing "input"
    (is (= "6548103910" (solve-1 [3 7] #{0 1} 768071)))))


(deftest part-2
  (testing "example"
    (are [recipes search-for] (= recipes (solve-2 [3 7] #{0 1} search-for))
          9       "51589"
          5       "01245"
          18      "92510"
          2018    "59414"))
  (comment
    (testing "input"
      (is (= 20198090 (solve-2 [3 7] #{0 1} "768071"))))))
