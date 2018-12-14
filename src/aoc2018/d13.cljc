(ns aoc2018.d13
  (:require [clojure.string :as str]))


(def cart-chars #{\^ \v \< \>})


(def underlying-track {\^ \|, \v \|, \< \-, \> \-})


(def vectorize-input (comp (partial mapv vec) str/split-lines))


(defn parse-input [input]
  (let [track-with-carts (vectorize-input input)]
    (reduce (fn [state row]
              (reduce (fn [{:keys [track] :as state'} col]
                        (let [char (get-in track [row col])]
                          (if (contains? cart-chars char)
                            (-> state'
                                (assoc-in [:track row col] (underlying-track char))
                                (assoc-in [:carts [row col]] {:char char, :intersections 0}))
                            state')))
                      state
                      (range (count (nth track-with-carts row)))))
            {:track track-with-carts, :carts (sorted-map)}
            (range (count track-with-carts)))))


(def move-cart {\> (fn [[row col]] [row (inc col)])
                \< (fn [[row col]] [row (dec col)])
                \^ (fn [[row col]] [(dec row) col])
                \v (fn [[row col]] [(inc row) col])})


(def cart-transitions {\> {\- \>
                           \\ \v
                           \/ \^}
                       \< {\- \<
                           \\ \^
                           \/ \v}
                       \^ {\| \^
                           \\ \<
                           \/ \>}
                       \v {\| \v
                           \\ \>
                           \/ \<}})


(def intersection-turns {\> {1 \^
                             2 \>
                             0 \v}
                         \< {1 \v
                             2 \<
                             0 \^}
                         \^ {1 \<
                             2 \^
                             0 \>}
                         \v {1 \>
                             2 \v
                             0 \<}})


(defn cart-mover [track safely?]
  (fn [new-carts pos {:keys [char intersections] :as cart}]
    (let [new-pos ((move-cart char) pos)
          landing-on (get-in track new-pos)
          landing-on-intersection? (= \+ landing-on)
          new-intersections (cond-> intersections
                                    landing-on-intersection? inc)
          new-char (if landing-on-intersection?
                     (get-in intersection-turns [char (mod new-intersections 3)])
                     (get-in cart-transitions [char (get-in track new-pos)]))]
      (if (contains? new-carts new-pos)
        (if safely?
          (dissoc new-carts new-pos)
          (throw (ex-info "crash!" {:at new-pos})))
        (assoc new-carts new-pos (assoc cart :char new-char :intersections new-intersections))))))


(defn move-carts [{:keys [track carts safely?] :as state}]
  (assoc state
         :carts
         (reduce-kv (cart-mover track safely?)
                    (sorted-map)
                    carts)))


(defn solve-1 [input]
  (try
    (last (iterate move-carts (parse-input input)))
    (catch clojure.lang.ExceptionInfo e
      (let [[y x] (:at (ex-data e))]
        [x y]))))


(defn solve-2 [input]
  (let [state (-> (parse-input input)
                  (assoc :safely? true))
        [y x] (->> (iterate move-carts state)
                   (drop-while (fn [{carts :carts}] (< 1 (count carts))))
                   first
                   :carts
                   ffirst)]
    [y x]))
