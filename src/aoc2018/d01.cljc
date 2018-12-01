(ns aoc2018.d01
  (:require [clojure.edn :as edn]))


(defn input->vec [input]
  (edn/read-string (str "[" input "]")))


(def solve-1 (partial reduce +))


(defn solve-2 [numbers]
  (reduce (fn [[freqs freq] n]
            (let [new-freq (+ freq n)]
              (if (contains? freqs new-freq)
                (reduced new-freq)
                [(conj! freqs new-freq) new-freq])))
          [(transient #{}) 0]
          (cycle numbers)))
