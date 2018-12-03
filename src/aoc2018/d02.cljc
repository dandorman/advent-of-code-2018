(ns aoc2018.d02)


(defn checksum [box-ids]
  (let [letter-freqs (->> box-ids
                          (map frequencies)
                          (mapcat (comp set vals))
                          frequencies)
        twos&threes (select-keys letter-freqs [2 3])]
    (apply * (vals twos&threes))))


(defn pairs [box-ids]
  (->> (take-while seq (iterate #(drop 1 %) box-ids))
       (mapcat (fn [[fst & rst]] (map #(vector fst %) rst)))
       (map #(apply map vector %))))


(defn lev [pair]
  (->> pair
       (remove (partial apply =))
       count))


(defn solve-2 [box-ids]
  (let [pairs (pairs box-ids)
        match (first (filter #(= 1 (lev %)) pairs))]
    (->> match
         (filter (partial apply =))
         (map first)
         (apply str))))
