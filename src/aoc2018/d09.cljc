(ns aoc2018.d09
  (:refer-clojure :exclude [remove]))


(defn circle []
  {0 {:left 0, :right 0}})


(defn navigate
  ([dir circle marble]
   (navigate dir circle marble 1))
  ([dir circle marble amount]
   (let [next-marble (get-in circle [marble dir])]
     (if (= 1 amount)
       next-marble
       (recur dir circle next-marble (dec amount))))))


(def left (partial navigate :left))

(def right (partial navigate :right))


(defn insert-right [circle marble new-marble]
  (let [old-right (right circle marble)]
    (-> circle
        (assoc-in [marble :right] new-marble)
        (assoc-in [old-right :left] new-marble)
        (assoc new-marble {:left marble, :right old-right}))))


(defn remove [circle marble]
  (let [left (left circle marble)
        right (right circle marble)]
    (-> circle
        (assoc-in [left :right] right)
        (assoc-in [right :left] left))))


(defn print-circle [circle marble]
  (print marble)
  (loop [current-marble (right circle marble)]
    (if (= current-marble marble)
      (println)
      (do
        (print current-marble)
        (recur (right circle current-marble)))))
  circle)


(defn play-marble* [{:keys [circle current-marble marble player players scores]}]
  (if (zero? (mod marble 23))
    (let [target (left circle current-marble 7)
          new-current-marble (right circle target)]
      {:circle (remove circle target)
       :current-marble new-current-marble
       :scores (update scores
                       (get players player)
                       (fnil (partial + target marble) 0))})
    {:circle (insert-right circle (right circle current-marble) marble)
     :current-marble marble}))


(defn play-marble [{:keys [marble player players] :as state}]
  (merge state
         (play-marble* state)
         {:marble (inc marble)
          :player (mod (inc player) (count players))}))


(defn solve [num-players last-marble]
  (-> (iterate play-marble {:circle         (circle)
                            :current-marble 0
                            :marble         1
                            :player         0
                            :players        (into [] (take num-players (iterate inc 1)))
                            :scores         {}})
      (nth last-marble)
      :scores
      vals
      sort
      last))
