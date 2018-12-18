(ns aoc2018.d17
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]))


(def earth [[\. \. \. \. \. \. \+ \. \. \.]
            [\. \. \. \. \. \. \| \. \. \.]
            [\. \# \. \. \# \. \. \. \. \.]
            [\. \# \. \. \# \. \. \# \. \.]
            [\. \# \. \. \# \. \. \# \. \.]
            [\. \# \. \. \. \. \. \# \. \.]
            [\. \. \. \. \. \. \. \. \. \.]
            [\. \# \# \# \# \# \# \# \. \.]
            [\. \. \. \. \. \. \. \. \. \.]])

(def vectorize-input (comp (partial mapv vec) str/split-lines))

(def ex1 (vectorize-input "......+.......\n......|.....#.\n.#..#.......#.\n.#..#..#......\n.#..#..#......\n.#.....#......\n.#.....#......\n.#######......\n..............\n..............\n....#.....#...\n....#.....#...\n....#.....#...\n....#######...\n"))


(defn move-water [{:keys [earth pending streams], [row col :as point] :point, :as state}]
  (if (nil? point)
    (if-let [stream (first streams)]
      {:earth earth, :pending pending, :streams (disj streams stream), :point stream}
      {:earth earth, :pending pending})
    (let [below [(inc row) col]]
      (if (= \. (get-in earth below))
        (let [earth' (reduce (fn [e p] (assoc-in e p \|)) earth pending)]
          (merge
            state
            {:earth (assoc-in earth' below \|)
             :point below}))
        (if (#{\# \~} (get-in earth below))
          (do
            (println row \, col)
            (let [context (subvec (nth earth row) (dec col) (+ 3 (dec col)))]
              (match
                context

                [\. \| \#]
                (let [point' [row (dec col)]]
                  (println "open left, wall right")
                  (merge
                    state
                    {:earth (-> earth (assoc-in point' \|))
                     :point point'}))

                [\. \| \|]
                (let [point' [row (dec col)]]
                  (println "open left, stream right")
                  (merge
                    state
                    {:earth (-> earth (assoc-in point' \|))
                     :point point'}))

                [\. \| \.]
                (let [point' [row (dec col)]]
                  (println "open left, open right")
                  (merge
                    state
                    {:earth (-> earth (assoc-in point' \|))
                     :streams (conj streams [row (inc col)])
                     :point point'}))

                [\# \| \#]
                (do
                  (println "wall left, wall right")
                  (merge
                    state
                    {:earth   (-> earth (assoc-in point \~))
                     :point   [(dec row) col]}))

                [\# \| _]
                (do
                  (println "wall left, ? right")
                  (merge
                    state
                    {:earth   (-> earth (assoc-in point \~))
                     :pending ((fnil conj #{}) pending point)
                     :point   [row (inc col)]}))

                [\~ \| \|]
                (do
                  (println "water left, stream right")
                  (merge
                    state
                    {:earth   (-> earth (assoc-in point \~))
                     :pending ((fnil conj #{}) pending point)
                     :point   [row (inc col)]}))

                [\~ \| \#]
                (do
                  (println "water left, wall right")
                  (merge
                    state
                    {:earth (-> earth (assoc-in point \~))
                     :pending #{}
                     :point [(dec row) col]}))

                [\~ \| \.]
                (let [point' [row (inc col)]]
                  (println "water left, open right")
                  (merge
                    state
                    {:earth (-> earth
                                (assoc-in point \~)
                                (assoc-in point' \|))
                     :pending ((fnil conj #{}) pending point)
                     :streams (disj streams point')
                     :point point'}))

                [\| \. \.]
                (let [point' [row (dec col)]]
                  (println "stream left, open right")
                  (merge
                    state
                    {:earth (-> earth
                                (assoc-in point \|)
                                (assoc-in point' \|))
                     :streams (-> streams (disj point) (conj point'))
                     :point point'}))

                [\| \| \|]
                (let [point' [row (inc col)]]
                  (println "streams everywhere")
                  (merge
                    state
                    {:point point'}))

                [\| \| \.]
                (let [point' [row (inc col)]]
                  (println context)
                  (merge
                    state
                    {:earth (assoc-in earth point' \|)
                     :point point'}))

                [\| \. \#]
                (let [point' [row (dec col)]]
                  (println "stream left, wall right")
                  (merge
                    state
                    {:point point'}))

                :else
                (do
                  (println "else" context)
                  (dissoc state :streams :point)))))
          (-> state
              (dissoc :point)
              (assoc :note "done?")))))))


(comment
  (first (drop-while #(or (:point %) (seq (:streams %))) (iterate move-water {:earth earth, :streams #{[1 6]}})))


  (let [{earth :earth} (first (drop-while #(or (:point %) (seq (:streams %))) (iterate move-water {:earth ex1, :streams #{[1 6]}})))]
    (reduce + (-> earth flatten frequencies (select-keys [\| \~]) vals)))

  (nth (iterate move-water {:earth earth, :point [1 6]}) 10))
