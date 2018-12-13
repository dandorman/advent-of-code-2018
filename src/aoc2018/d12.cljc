(ns aoc2018.d12
  (:require [clojure.string :as str]))


(defn parse-plants [input]
  (let [[_ plants] (re-matches #"initial state: ([.#]+)" input)]
    plants))


(defn parse-rule [rules input]
  (let [[rule production] (str/split input #" => ")]
    (assoc rules rule production)))


(defn parse-input [input]
  (let [lines (str/split-lines input)
        plants (parse-plants (first lines))
        rules (reduce parse-rule {} (drop 2 lines))]
    {:rules rules, :plants plants}))


(def spacing "....")


(defn next-gen [rules [plants idx]]
  (let [plants (str spacing plants spacing)]
    (loop [new-plants ""
           i 0]
      (if (= i (- (count plants) 4))
        (let [first-plant (str/index-of new-plants \#)
              last-plant (str/last-index-of new-plants \#)]
          [(subs new-plants first-plant (inc last-plant))
           (+ idx (- first-plant 2))])
        (recur (str new-plants (get rules (subs plants i (+ i 5)) "."))
               (inc i))))))


(defn score-plants [plants idx]
  (reduce (fn [score i]
            (if (= \# (nth plants i))
              (+ score (+ idx i))
              score))
          0
          (range (count plants))))


(defn solve [generations input]
  (let [{:keys [rules plants]} (parse-input input)]
    (loop [generation generations
           plants plants
           idx 0]
      (if (zero? generation)
        (score-plants plants idx)
        (let [[plants new-idx] (next-gen rules [plants idx])]
          (recur (dec generation) plants new-idx))))))


(def solve-1 (partial solve 20))
