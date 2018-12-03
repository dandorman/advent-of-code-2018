(ns aoc2018.d03
  (:require [clojure.set :as set]
            [clojure.string :as str]))


(defn parse-claim [line]
  (let [[_ id & dims] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$" line)
        [left top width height] (map #(Integer/parseInt %) dims)]
    {:id id, :left left, :top top, :width width, :height height}))


(defn claim->map [{:keys [left top width height]}]
  (into {}
        (for [x (range left (+ left width))
              y (range top (+ top height))]
          [[x y] 1])))


(defn merge-claims [input]
  (->> (str/split-lines input)
       (map parse-claim)
       (map claim->map)
       (reduce (fn [m c] (merge-with + m c)) {})))


(defn solve-1 [input]
  (->> input
       merge-claims
       vals
       (filter (partial < 1))
       count))


(defn print-claims [claims]
  (doseq [x (range 10)]
    (doseq [y (range 10)]
      (let [loc (get claims [x y] ".")]
        (print loc)))
    (println)))


(defn claim-coords [{:keys [left top width height]}]
  (into #{}
        (for [x (range left (+ left width))
              y (range top (+ top height))]
          [x y])))


(defn index-claims [input]
  (->> input
       str/split-lines
       (map parse-claim)
       (reduce (fn [m c] (assoc! m (:id c) (claim-coords c))) (transient {}))
       persistent!))


(defn index-claims-by-coords [claims]
  (reduce-kv (fn [m id coords]
               (reduce (fn [m coord]
                         (update m coord conj id))
                       m
                       coords))
             {}
             claims))


(defn remove-overlaps [coord->claims]
  (->> coord->claims
       (remove (fn [[_ claims]] (< 1 (count claims))))
       (map first)
       (into #{})))


(defn solve-2 [input]
  (let [claims (index-claims input)
        single-owners (-> claims index-claims-by-coords remove-overlaps)]
    (->> claims
         (filter (fn [[_ coords]] (= coords (set/intersection coords single-owners))))
         ffirst)))
