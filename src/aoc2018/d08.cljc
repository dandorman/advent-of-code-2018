(ns aoc2018.d08
  (:require [clojure.edn :as edn]
            [clojure.walk :as walk]))


(defn parse-input [input]
  (edn/read-string (str "[" input "]")))


(defn parse-node [numbers]
  (let [[num-children num-meta & numbers] numbers
        [numbers children] (reduce (fn [[nums kids] _]
                                     (let [[nums kid] (parse-node nums)]
                                       [nums (conj kids kid)]))
                                   [numbers []]
                                   (range num-children))
        [meta numbers] (split-at num-meta numbers)]
    [numbers {:meta meta :children children}]))


(defn parse-tree [numbers]
  (-> numbers parse-node peek))


(defn solve-1 [input]
  (let [tree (-> input parse-input parse-tree)]
    (reduce + (mapcat :meta (tree-seq #(seq (:children %)) :children tree)))))


(defn walk-tree [tree]
  (if (map? tree)
    (let [{:keys [meta children]} tree]
      (if (seq children)
        (assoc tree :value (reduce (fn [sum idx]
                                     (let [child (get children idx)]
                                       (+ sum (get child :value 0))))
                                   0
                                   (map dec meta)))
        (assoc tree :value (reduce + meta))))
    tree))


(defn solve-2 [input]
  (let [tree (-> input parse-input parse-tree)]
    (:value (walk/postwalk walk-tree tree))))
