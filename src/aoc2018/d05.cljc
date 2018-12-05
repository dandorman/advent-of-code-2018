(ns aoc2018.d05
  (:require [clojure.string :as str]))


(def lower (map char (take 26 (iterate inc (int \a)))))


(def upper (map char (take 26 (iterate inc (int \A)))))


(def change-case (merge (zipmap lower upper) (zipmap upper lower)))


(defn resolve-polymer [polymer]
  (loop [input polymer
         idx 0]
    (if (<= (dec (count input)) idx)
      input
      (if (= (nth input idx) (change-case (nth input (inc idx))))
        (recur
          (str (subs input 0 idx) (subs input (min (+ 2 idx) (count input))))
          (max (dec idx) 0))
        (recur input (inc idx))))))


(defn best-polymer [input]
  (reduce (fn [shortest ch]
            (let [without (str/replace input (re-pattern (str "(?i)" ch)) "")
                  polymer (resolve-polymer without)]
              (if (< (count polymer) (count shortest))
                polymer
                shortest)))
          input
          (into #{} (str/lower-case input))))


(defn solve-1 [input]
  (count (resolve-polymer input)))


(defn solve-2 [input]
  (count (best-polymer input)))
