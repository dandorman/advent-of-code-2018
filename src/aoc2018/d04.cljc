(ns aoc2018.d04
  (:require [clojure.set :as set]
            [clojure.string :as str]))


(defn parse-timestamp [ts]
  (let [[year month day hour minute] (->> (str/split ts #"\D")
                                          (map #(Integer/parseInt %)))]
    {:year year
     :month month
     :day day
     :hour hour
     :minute minute}))


(defmulti parse-desc (fn [first-word _] first-word))


(defmethod parse-desc "Guard"
  [_ desc]
  {:type ::new-guard
   :guard (let [id (-> desc (str/split #"\s") first)
                id (apply str (drop 1 id))]
            (Integer/parseInt id))})


(defmethod parse-desc "falls"
  [_ _]
  {:type ::asleep})


(defmethod parse-desc "wakes"
  [_ _]
  {:type ::awake})


(defn parse-line [line]
  (let [[_ ts first-word desc] (re-matches #"^\[(.*)\] (\S+) (.*)$" line)]
    (merge (parse-timestamp ts)
           (parse-desc first-word desc))))


(defn sleeping-map [fell-asleep-at awoke-at]
  (reduce (fn [acc el] (assoc acc el 1))
          {}
          (range fell-asleep-at awoke-at)))


(defn make-history [input]
  (let [lines (-> input str/split-lines sort)]
    (loop [[line & rst] lines
           guard nil
           fell-asleep-at nil
           acc {}]
      (if line
        (let [{:keys [type] :as parsed} (parse-line line)]
          (case type
            ::new-guard (recur rst (:guard parsed) nil acc)
            ::asleep (recur rst guard (:minute parsed) acc)
            ::awake (let [minutes-slept (sleeping-map fell-asleep-at (:minute parsed))]
                      (recur rst guard nil (update acc guard #(merge-with + % minutes-slept))))))
        acc))))


(defn sleepiest-guard [guards]
  (last (sort-by (fn [[_ m]] (reduce + (vals m))) guards)))


(defn sleepiest-minute [minutes]
  (->> (set/map-invert minutes)
       (into (sorted-map))
       last))


(defn solve-1 [input]
  (let [[id minutes] (sleepiest-guard (make-history input))
        minute (last (sleepiest-minute minutes))]
    (* id minute)))


(defn global-sleepiest-minute [guards]
  (->> guards
       (map (fn [[id minutes]]
              (conj (sleepiest-minute minutes) id)))
       (sort-by first)
       last))


(defn solve-2 [input]
  (let [history (make-history input)
        [_ minute id] (global-sleepiest-minute history)]
    (* minute id)))
