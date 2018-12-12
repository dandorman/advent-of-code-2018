(ns aoc2018.d11
  (:require [clojure.core.matrix :as m]))


(def size 300)


(defn power-levels [serial-number]
  (let [cells (-> (into [] (repeat size (into [] (range 1 (inc size))))))
        cells' (m/transpose cells)
        rack-ids (m/add cells 10)]
    (-> rack-ids
        (m/mul cells')
        (m/add serial-number)
        (m/mul rack-ids)
        (->> (m/emap #(-> % (quot 100) (rem 10))))
        (m/sub 5))))


(defn solve-1 [serial-number]
  (let [levels (power-levels serial-number)]
    (->> (for [row (range 0 (- size 3))
               col (range 0 (- size 3))]
           [(m/esum (m/submatrix levels row 3 col 3)) [col row]])
         (into (sorted-map))
         last
         last
         (map inc)
         (into []))))


(defn submatrix-indices [min max]
  (for [sub-size (range min max)
        row (range 0 (inc (- size sub-size)))
        col (range 0 (inc (- size sub-size)))]
    [row col sub-size]))


(defn solve-2 [serial-number]
  (let [levels (power-levels serial-number)]
    (->> (pmap (fn [[row col sub-size]]
                 (let [submatrix (m/submatrix levels row sub-size col sub-size)]
                   [(m/esum submatrix) [(inc col) (inc row) sub-size]]))
               (submatrix-indices 3 (inc size)))
         (into (sorted-map))
         last
         last)))
