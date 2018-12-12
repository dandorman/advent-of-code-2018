(ns aoc2018.d12
  (:require [clojure.set :as set]
            [clojure.string :as str]))


(defn parse-plants [input]
  (let [[_ plant-str] (re-matches #"initial state: ([.#]+)" input)]
    (reduce (fn [plants i]
              (if (= \# (nth plant-str i))
                (conj plants i)
                plants))
            (sorted-set)
            (range (count plant-str)))))


(defn parse-rule [rules input]
  (let [[context-str _ result-str] (str/split input #" ")]
    (assoc rules (reduce (fn [context i]
                           (if (= \# (nth context-str i))
                             (conj context (- i 2))
                             context))
                         (sorted-set)
                         (range 5))
                 (if (= result-str "#") true false))))


(defn parse-input [input]
  (let [lines (str/split-lines input)
        plants (parse-plants (first lines))
        rules (reduce parse-rule {} (drop 2 lines))]
    {:rules rules, :plants plants}))


(defn print-plants
  ([plants]
   (print-plants plants (apply min plants) (inc (apply max plants))))
  ([plants start end]
   (doseq [xs (partition-all 5 (range start end))]
     (doseq [x xs]
       (if (contains? plants x)
         (print (if (zero? x) "0" "#"))
         (print (if (zero? x) "_" "."))))
     (print " "))
   (println)))


(defn next-gen [rules plants]
  (persistent!
    (reduce (fn [next-plants pot]
              (let [pot-context (->> (range (- pot 2) (+ pot 3))
                                     (into #{})
                                     (set/intersection plants)
                                     (map #(- % pot))
                                     (into #{}))]
                (if (get rules pot-context)
                  (conj! next-plants pot)
                  next-plants)))
            (transient #{})
            (range (- (apply min plants) 2)
                   (+ (inc (apply max plants)) 3)))))


(defn solve [generations input]
  (let [{:keys [plants rules]} (parse-input input)]
    (reduce + (nth (iterate (partial next-gen rules) plants) generations))))


(def solve-1 (partial solve 20))
