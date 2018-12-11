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
    (last (reduce (fn [[score _ :as best] [row col sub-size]]
                    (let [submatrix (m/submatrix levels row sub-size col sub-size)
                          current-score (m/esum submatrix)]
                      (if (< score current-score)
                        [current-score [(inc col) (inc row) sub-size]]
                        best)))
                  [0 nil]
                  (submatrix-indices 3 (inc size))))))


(defn solve-x [serial-number]
  (let [levels (power-levels serial-number)]
    (->> (pmap (fn [[row col sub-size]]
                 (let [submatrix (m/submatrix levels row sub-size col sub-size)]
                   [(m/esum submatrix) [(inc col) (inc row) sub-size]]))
               (submatrix-indices 3 (inc size)))
         (into (sorted-map))
         last
         last)))


(comment
  (solve-x 3031))


(comment
  (let [serial-number 3031
        size 300
        cells (-> (into [] (repeat size (into [] (range 1 (inc size))))))
        cells' (m/transpose cells)
        rack-ids (m/add cells 10)
        power-levels (-> rack-ids
                         (m/mul cells')
                         (m/add serial-number)
                         (m/mul rack-ids)
                         (->> (m/emap #(-> % (quot 100) (rem 10))))
                         (m/sub 5))])




  (m/emap (fn [e] (rem (quot e 100) 10)) [[12345]])

  (rem (quot 12345 100) 10)
  (rem (quot 949 100) 10)

  (m/mul [[1 2] [3 4]] [[1 2] [3 4]])


  (m/esum [[1 2] [3 4]]))