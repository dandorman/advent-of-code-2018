(ns aoc2018.d14)


(defn parse-input [input]
  (into [] (map #(Integer/parseInt (str %)) input)))


(defn next-recipes [[recipes elves]]
  (let [active-recipes (map (partial get recipes) elves)
        new-recipe (reduce + active-recipes)
        ingredients ((juxt quot mod) new-recipe 10)
        ingredients (cond->> ingredients (zero? (first ingredients)) (drop 1))
        new-recipes (reduce conj recipes ingredients)
        new-elves (map
                    (fn [e]
                      (mod (inc (+ e (get recipes e)))
                           (count new-recipes)))
                    elves)]
    [new-recipes (into #{} new-elves)]))


(comment
  (nth (iterate next-recipes [[3 7] #{0 1}]) 14)
  (nth (iterate next-recipes [[3 7] #{0 1}]) 6)


  (->> (iterate next-recipes [[3 7] #{0 1}])
       (drop-while (fn [[recipes _]] (< (count recipes) 19)))
       ffirst
       (drop 9)
       (take 10))


  (->> (iterate next-recipes [[3 7] #{0 1}])
       (drop-while (fn [[recipes _]] (< (count recipes) 15)))
       ffirst
       (drop 5)
       (take 10)
       (apply str)))


(defn solve-1 [recipes elves take-after]
  (->> (iterate next-recipes [recipes elves])
       (drop-while (fn [[recipes _]] (< (count recipes) (+ 10 take-after))))
       ffirst
       (drop take-after)
       (take 10)
       (apply str)))


(defn solve-2 [recipes elves input]
  (let [search-for (parse-input input)]
    (loop [recipes recipes
           elves elves
           gen (count recipes)
           juice 20000000]
      (if (zero? juice)
        :boom
        (let [i (max 0 (- (count recipes) (count search-for)))
              j (min (count recipes) (+ i (count search-for)))
              x (subvec recipes i j)
              i' (max 0 (- (count recipes) (inc (count search-for))))
              j' (min (count recipes) (+ i' (count search-for)))
              x' (subvec recipes i' j')]
          (cond
            (= search-for x) i
            (= search-for x') i'
            :else (let [[recipes' elves'] (next-recipes [recipes elves])]
                    (recur recipes' elves' (inc gen) (dec juice)))))))))
