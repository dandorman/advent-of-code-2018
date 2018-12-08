(ns aoc2018.d07
  (:require [clojure.set :as set]
            [clojure.string :as str]))


(def example "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")


(defn parse-line [line]
  (let [[_ a b] (re-matches #".*\b([A-Z])\b.*\b([A-Z])\b.*" line)]
    [a b]))


(defn parse-input [input]
  (reduce (fn [graph [a b]]
            (-> graph
                (update-in [a :out] (fnil conj (sorted-set)) b)
                (update-in [b :in] (fnil conj (sorted-set)) a)))
          (sorted-map)
          (->> input
               str/split-lines
               (map parse-line))))


(defn start [graph]
  (->> graph
       (filter (fn [[_ v]] (-> v :in empty?)))
       (into (sorted-map))))


(defn traverse
  ([graph]
   (traverse graph (start graph) []))
  ([graph at visited]
   (if (empty? at)
     visited
     (let [next-step (-> at first key)
           available (conj (into #{} visited) next-step)
           out (mapcat (comp :out val) at)
           ready (->> out
                      (filter (fn [k] (empty? (set/difference (get-in graph [k :in]) available))))
                      (select-keys graph)
                      (into (sorted-map)))]
       (recur graph
              (-> (merge at ready) (dissoc next-step))
              (conj visited next-step))))))


(defn solve-1 [input]
  (apply str (traverse (parse-input input))))


(def letter->cost (reduce (fn [m i]
                            (assoc m (-> i char str) (inc (- i (int \A)))))
                          (sorted-map)
                          (take 26 (iterate inc (int \A)))))


(defn add-costs [graph min]
  (reduce-kv (fn [m k _]
               (assoc-in m [k :cost] (+ min (letter->cost k))))
             graph
             graph))


(defn dep-cost [graph costs key]
  (->> (get-in graph [key :in])
       (select-keys costs)
       vals
       (reduce +)))


(defn work-step [{:keys [graph costs ready working done max-workers]}]
  (let [{finished true, unfinished false} (group-by #(zero? (get costs %)) working)
        new-done (into [] (concat done finished))
        next-steps (mapcat #(get-in graph [% :out]) finished)
        new-ready (reduce conj ready next-steps)
        new-jobs (take (- max-workers (count unfinished))
                       (filter #(zero? (dep-cost graph costs %)) new-ready))
        new-ready (into (sorted-set) (set/difference new-ready (into #{} new-jobs)))
        new-working (into #{} (concat unfinished new-jobs))
        new-costs (reduce #(update %1 %2 dec) costs new-working)]
    {:graph graph
     :costs new-costs
     :ready new-ready
     :working new-working
     :done new-done
     :max-workers max-workers}))


(defn solve-2 [input max-workers min]
  (let [graph (-> input parse-input (add-costs min))
        costs (->> (keys graph)
                   (select-keys letter->cost)
                   (reduce-kv (fn [m k v] (assoc m k (+ min v))) {}))
        ready (->> graph start (map key) (into (sorted-set)))]
    (->> {:graph       graph
          :costs       costs
          :ready       ready
          :working     #{}
          :done        []
          :max-workers max-workers}
         (iterate work-step)
         (take-while (fn [{:keys [ready working]}]
                       (or (seq ready) (seq working))))
         count
         ;; dec because our initial state gives us an extra "tick"
         dec)))
