(ns aoc2018.d04
  (:require [clojure.set :as set]
            [clojure.string :as str]))


(defn parse-timestamp [ts]
  (let [minute (->> (str/split ts #"\D") last Integer/parseInt)]
    {:minute minute}))


(defmulti parse-desc #(subs % 0 5))


(defmethod parse-desc "Guard"
  [desc]
  {:type ::new-guard
   :guard (let [id (-> desc (str/split #"\s") second)
                id (apply str (drop 1 id))]
            (Integer/parseInt id))})


(defmethod parse-desc "falls"
  [_]
  {:type ::asleep})


(defmethod parse-desc "wakes"
  [_]
  {:type ::awake})


(defn parse-line [line]
  (let [[_ ts desc] (re-matches #"^\[(.*)\] (.*)$" line)]
    (merge (parse-timestamp ts) (parse-desc desc))))


(defn sleeping-map [fell-asleep-at awoke-at]
  (reduce #(assoc %1 %2 1) {} (range fell-asleep-at awoke-at)))


(defn make-history [input]
  (let [lines (-> input str/split-lines sort)]
    (loop [[line & rst] lines
           guard nil
           fell-asleep-at nil
           guard->minutes {}]
      (if line
        (let [{:keys [type] :as parsed} (parse-line line)]
          (case type
            ::new-guard (recur rst
                               (:guard parsed)
                               nil
                               guard->minutes)
            ::asleep (recur rst
                            guard
                            (:minute parsed)
                            guard->minutes)
            ::awake (let [minutes-slept (sleeping-map fell-asleep-at (:minute parsed))]
                      (recur rst
                             guard
                             nil
                             (update guard->minutes guard #(merge-with + % minutes-slept))))))
        guard->minutes))))


(defn sleepiest-guard-by [f guards]
  (->> guards (sort-by f) last))


(defn sleepiest-guard-by-total-minutes [guards]
  (sleepiest-guard-by (fn [[_ minutes]] (reduce + (vals minutes))) guards))


(defn sleepiest-guard-by-sleepiest-minute [guards]
  (sleepiest-guard-by (fn [[_ minutes]] (apply max (vals minutes))) guards))


(defn sleepiest-minute [minutes]
  (->> (set/map-invert minutes)
       (into (sorted-map))
       last
       last))


(defn solve-1 [input]
  (let [[id minutes] (sleepiest-guard-by-total-minutes (make-history input))
        minute (sleepiest-minute minutes)]
    (* id minute)))


(defn solve-2 [input]
  (let [[id minutes] (sleepiest-guard-by-sleepiest-minute (make-history input))
        minute (sleepiest-minute minutes)]
    (* minute id)))
