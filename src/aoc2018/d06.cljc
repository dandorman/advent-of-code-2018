(ns aoc2018.d06
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]))


(defn parse-point [line]
  (edn/read-string (str "[" line "]")))


(defn parse-points [input]
  (into #{} (map parse-point (str/split-lines input))))


(defn boundaries [points]
  (let [[xs ys] (reduce (fn [[xs ys] [x y]] [(conj! xs x) (conj! ys y)]) [(transient []) (transient [])] points)
        min-max (juxt (partial apply min) (partial apply max))
        [x-min x-max] (min-max (persistent! xs))
        [y-min y-max] (min-max (persistent! ys))]
    {:min [x-min y-min], :max [x-max y-max]}))


(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))


(defn closest [points point]
  (let [closest-points (val (first (reduce (fn [distance->points p]
                                             (update distance->points (manhattan-distance point p) conj p))
                                           (sorted-map)
                                           points)))]
    (when (= 1 (count closest-points))
      (first closest-points))))


(defn space-between [points]
  (let [{[x-min y-min] :min, [x-max y-max] :max} (boundaries points)]
    (for [x (range x-min (inc x-max))
          y (range y-min (inc y-max))]
      [x y])))


(defn not-any-on-boundary? [all-points]
  (let [{[x-min y-min] :min, [x-max y-max] :max} (boundaries all-points)]
    (fn [points]
      (not-any? (fn [[x y]]
                  (or (= x x-min) (= x x-max)
                      (= y y-min) (= y y-max)))
                points))))


(defn solve-1 [input]
  (let [points (parse-points input)
        point->area (->> (reduce (fn [m space]
                                   (if-let [closest (closest points space)]
                                      (assoc! m closest (conj (get m closest #{}) space))
                                      m))
                                 (transient {})
                                 (space-between points))
                         persistent!)]
    (->> point->area
         vals
         (filter (not-any-on-boundary? points))
         (map count)
         (apply max))))


(defn middlish [points]
  (let [[xs ys] (reduce (fn [[xs ys] [x y]] [(conj xs x) (conj ys y)]) [[] []] points)]
    [(quot (reduce + xs) (count xs))
     (quot (reduce + ys) (count ys))]))


(defn neighbors [[x y]]
  #{[x (dec y)] [(dec x) y] [(inc x) y] [x (inc y)]})


(defn viable-filter-fn [threshold points]
  (fn [visiting]
    (reduce (fn [total-dist point]
              (let [dist (manhattan-distance visiting point)
                    total-dist (+ total-dist dist)]
                (if (< total-dist threshold)
                  total-dist
                  (reduced nil))))
            0
            points)))


(defn largest-region [threshold points]
  (loop [visiting #{(middlish points)}
         region #{}]
    (if (empty? visiting)
      region
      (let [viable (->> visiting
                        (filter (viable-filter-fn threshold points))
                        (into #{}))
            new-region (set/union region viable)]
        (recur (set/difference (into #{} (mapcat neighbors viable)) new-region)
               new-region)))))


(defn solve-2 [threshold input]
  (->> (parse-points input)
       (largest-region threshold)
       count))
