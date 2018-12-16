(ns aoc2018.d15
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))


(def example "#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######")


(def open-floor \.)


(def vectorize-input (comp (partial mapv vec) str/split-lines))


(defn dist [[row-a col-a] [row-b col-b]]
  (+ (Math/abs (- row-a row-b))
     (Math/abs (- col-a col-b))))


(defn parse-input [input]
  (let [cave (vectorize-input input)]
    (-> (reduce (fn [state row]
                  (reduce (fn [{cave :cave :as state'} col]
                            (let [ch (get-in cave [row col])]
                              (if (#{\G \E} ch)
                                (-> state'
                                    (assoc-in [ch [row col]] {:hp 200})
                                    (assoc-in [:creatures [row col]] ch))
                                state')))
                          state
                          (range (count (nth cave row)))))
                {:cave cave
                 :creatures (sorted-map)
                 \E {}
                 \G {}}
                (range (count cave))))))


(defn points-from [[row col]]
  [[(dec row) col]
   [row (dec col)]
   [row (inc col)]
   [(inc row) col]])


(defn clear-points-from [cave start]
  (->> (points-from start)
       (remove #(= \# (get-in cave %)))))


(defn open-points-from [cave point]
  (->> (points-from point)
       (filter #(= \. (get-in cave %)))))


(def example-2 "#######\n#.E...#\n#.....#\n#...G.#\n#######")


(defn move-creature [{:keys [cave] :as state} point max-distance]
  (let [creature (get-in cave point)
        enemy ({\E \G, \G \E} creature)
        all-enemies (->> (get state enemy)
                         keys
                         (mapcat (partial clear-points-from cave))
                         distinct
                         (filter #(= open-floor (get-in cave %)))
                         (into #{}))]
    (when (seq all-enemies)
      (loop [paths [[point]]
             distance max-distance]
        (if (zero? distance)
          nil
          (let [new-paths (mapcat (fn [path]
                                    (let [next-steps (->> (clear-points-from cave (peek path))
                                                          (remove (into #{} path))
                                                          (remove #(= creature (get-in cave %))))]
                                      (when (seq next-steps)
                                        (map #(conj path %) next-steps))))
                                  paths)]
            (if-let [winner (first (filter #(= enemy (get-in cave (peek %))) new-paths))]
              winner
              (when (seq new-paths)
                (recur new-paths (dec distance))))))))))


(defn print-cave [{cave :cave}]
  (doseq [row cave]
    (println (apply str row))))


(defn move [{:keys [cave] :as state} from to]
  (let [creature (get-in cave from)]
    (-> state

        ; from
        (assoc-in (cons :cave from) open-floor)
        (update :creatures dissoc from)
        (update creature dissoc from)

        ; to
        (assoc-in (cons :cave to) creature)
        (update :creatures assoc to creature)
        (update creature assoc to (get-in state [creature from])))))


(def creature->enemy {\E \G, \G \E})


(defn weakest-enemy [{:keys [cave] :as state} point]
  (let [enemy (creature->enemy (get-in cave point))]
    (->> (clear-points-from cave point)
         (select-keys (get state enemy))
         (sort-by (fn [[location {hp :hp}]] [hp location]))
         first)))


(defn attack [{:keys [cave] :as state} from dmg]
  (let [enemy (creature->enemy (get-in cave from))
        [enemy-location _] (weakest-enemy state from)]
    (if enemy-location
      (let [enemy-hp (- (get-in state [enemy enemy-location :hp]) dmg)]
        (if (<= enemy-hp 0)
          (-> state
              (assoc-in (cons :cave enemy-location) open-floor)
              (update enemy dissoc enemy-location)
              (update :creatures dissoc enemy-location))
          (assoc-in state [enemy enemy-location :hp] enemy-hp)))
      state)))


(defn move-creatures [state]
  (reduce-kv (fn [{:keys [cave] :as state'} pos ch]
               (let [start (System/currentTimeMillis)
                     path (move-creature state' pos 16)
                     end (System/currentTimeMillis)
                     dur (- end start)
                     _ (when (< 1000 dur)
                         (println ch \@ pos dur))]
                 (if-not path
                   (attack state' pos 3)
                   (case (count path)
                     2 (do
                         #_(println "attack!")
                         (attack state' pos 3))
                     3 (let [[curr-pos next-pos _] path]
                         #_(println "move & attack!")
                         (-> state'
                             (move curr-pos next-pos)
                             (attack next-pos 3)))
                     (let [[curr-pos next-pos & _] path]
                       #_(println "move!")
                       (move state' curr-pos next-pos))))))
             state
             (:creatures state)))


(def example-3 "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######")


(def input "################################\n#################..#############\n##########..###G..##############\n#######...G..#G..#...###..######\n#######....###......###...######\n#####G..G..###........#...G..###\n#####G......##.....G....G....###\n###....G....###..............#.#\n###.G........#.G...............#\n###............G............#..#\n###G.....G##..................##\n###.......#.E...G...........####\n##..........G.#####............#\n#####........#######...E...E...#\n#####.E.....#########.....#.#.##\n##.#...G....#########.###.#.#.##\n#...........#########.##########\n###......G..#########.##########\n##..........#########.##########\n#....##.G....#######.....#######\n###E.##....E..#####..E....######\n#######.#................#######\n########......#E.....###########\n#########......E.....###########\n##########.........E############\n##########.........#############\n##########..###..###############\n##########..###..###############\n###########..###...#############\n##########...#####....##########\n###########..########...########\n################################")


(defn reconstruct-path [came-from current]
  (loop [path (list current)
         current current]
    (if-let [next (first (get came-from current))]
      (recur (conj path next) next)
      (into [] path))))


(sort-by {:a 3 :b 2} #{:a :b})

(defn a* [{:keys [cave]} start goal]
  (loop [closed #{}
         open #{start}
         came-from {}
         g-score {start 0}
         f-score {start (dist start goal)}
         juice 100000]
    (if (zero? juice)
      :boom #_[closed open came-from g-score f-score]
      (if (seq open)
        (let [current (first (sort-by (juxt f-score identity) open))]
          #_(println :open (sort-by (juxt f-score identity) open))
          (if (= current goal)
            (do
              #_(println :done start "=>" goal ";" g-score)
              (reconstruct-path came-from current))
            (let [open' (disj open current)
                  closed' (conj closed current)
                  neighbors (into #{} (open-points-from cave current))
                  neighbors (set/difference neighbors closed')
                  open' (set/union open' neighbors)
                  stuff (reduce (fn [x neighbor]
                                  (let [g (inc (g-score current))]
                                    (if (< g (get g-score neighbor (inc g)))
                                      (-> x
                                          (update-in [:came-from neighbor] (fnil conj (sorted-set)) current)
                                          (assoc-in [:g-score neighbor] g)
                                          (assoc-in [:f-score neighbor] (+ g (dist neighbor goal))))
                                      x)))
                                {:came-from came-from, :g-score g-score, :f-score f-score}
                                (sort neighbors))]
              (recur closed'
                     open'
                     (:came-from stuff)
                     (:g-score stuff)
                     (:f-score stuff)
                     (dec juice)))))
        nil #_[closed open came-from g-score f-score]))))


(defn path-comparator [goal]
  (fn [a b]
    (compare [(dist (peek a) goal) a] [(dist (peek b) goal) b])))


(defn b* [{:keys [cave]} start goal]
  (loop [visited #{}
         paths (sorted-set-by (path-comparator goal) [start])
         juice 100000]
    (if (zero? juice)
      :boom #_[closed open came-from g-score f-score]
      (if (seq paths)
        (do
          (let [path (first paths)
                current (peek path)]
            (if (= current goal)
              (do
                #_(println :done start "=>" goal ";" g-score)
                path)
              (let [paths' (disj paths path)
                    visited' (conj visited current)
                    neighbors (into #{} (open-points-from cave current))
                    neighbors (set/difference neighbors visited')
                    paths' (reduce (fn [ps n] (conj ps (conj path n))) paths' neighbors)]
                (recur visited'
                       paths'
                       (dec juice))))))
        nil #_[closed open came-from g-score f-score]))))


(comment
  (b* (parse-input "#####\n#...#\n#...#\n#...#\n#####")
      [1 1] [3 3]))


(defn closest-target [{:keys [cave] :as state} point ch]
  (let [enemies (get state (creature->enemy ch))
        open-points (into #{} (mapcat #(clear-points-from cave %) (keys enemies)))
        in-halo? (contains? open-points point)
        open-points (filter #(= \. (get-in cave %)) open-points)
        enemy-halos (cond-> open-points in-halo? (conj point))]
    (println :halos ch \@ point "=>" (sort-by (juxt (partial dist point) identity) enemy-halos))
    #_(println ch enemy-halos (into #{} (mapcat #(open-points-from cave %) (keys enemies))))
    (first (sort-by (juxt (partial dist point) identity) enemy-halos))))


(defn available-targets [{:keys [cave] :as state} point ch]
  (let [enemies (get state (creature->enemy ch))
        open-points (into #{} (mapcat #(clear-points-from cave %) (keys enemies)))
        in-halo? (contains? open-points point)
        open-points (filter #(= \. (get-in cave %)) open-points)
        enemy-halos (cond-> open-points in-halo? (conj point))]
    #_(println :halos ch \@ point "=>" (sort-by (juxt (partial dist point) identity) enemy-halos))
    #_(println :halos ch \@ point "=>" enemy-halos)
    #_(println ch enemy-halos (into #{} (mapcat #(open-points-from cave %) (keys enemies))))
    #_(sort-by (juxt (partial dist point) identity) enemy-halos)
    enemy-halos))


(defn best-path [state point ch]
  (let [targets (sort-by #(dist point %) (available-targets state point ch))
        #__ #_(println ch \@ point "=>" targets)
        paths (keep #(a* state point %) targets)
        #__ #_(println :paths (sort-by (juxt count last second) paths))]
    #_(first (sort-by (juxt count last second) paths))
    (first paths)))




(defn move-one' [state point ch]
  (if-let [target (closest-target state point ch)]
    (do
      #_(println :ohai target (a* state point target))
      (if-let [path (best-path state point ch) #_(a* state point target)]
        (cond
          (= 1 (count path))
          (do
            #_(println :attack)
            (attack state point 3))

          (= 2 (count path))
          (do
            #_(println :move&attack)
            (-> state
                (move point (last path))
                (attack (last path) 3)))

          :else
          (do
            #_(println :move)
            (move state point (second path))))
        state))
    state))

(defn move-one [state point ch]
  (if (get state ch point)
    (if-let [path (best-path state point ch) #_(a* state point target)]
      (cond
        (= 1 (count path))
        (do
          #_(println :attack)
          (attack state point 3))

        (= 2 (count path))
        (do
          #_(println :move&attack)
          (-> state
              (move point (last path))
              (attack (last path) 3)))

        :else
        (do
          #_(println :move)
          (move state point (second path))))
      state)))


(defn move-everybody [{:keys [creatures] :as state}]
  (reduce-kv move-one state creatures))


(def movement-example "#########\n#G..G..G#\n#.......#\n#.......#\n#G..E..G#\n#.......#\n#.......#\n#G..G..G#\n#########")

(def example-4 "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######")
(def example-5 "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######")
(def example-6 "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########")
(def example-7 "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######")
(def example-8 "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######")


(def blargh "################################\n###############.##...###########\n##############..#...G.#..#######\n##############.............#####\n###############....G....G......#\n##########..........#..........#\n##########................##..##\n######...##..G...G.......####..#\n####..G..#G...............####.#\n#######......G....G.....G#####E#\n#######.................E.######\n########..G...............######\n######....G...#####E...G....####\n######..G..G.#######........####\n###.........#########.......E.##\n###..#..#...#########...E.....##\n######......#########.......####\n#####...G...#########.....######\n#####G......#########.....######\n#...#G..G....#######......######\n###...##......#####.......######\n####..##..G........E...E..######\n#####.####.....######...########\n###########..#...####...E.######\n###############...####..#...####\n###############...###...#.E.####\n#####################.#E....####\n#####################.#...######\n###################...##.#######\n##################..############\n##################...###########\n################################")
(def ah-jeez "################################\n#####################...########\n###################....G########\n###################....#########\n#######.##########......########\n#######G#########........#######\n#######G#######.G.........######\n#######.######..G.........######\n#######.......##.G...G.G..######\n########..##..#....G......G#####\n############...#.....G.....#####\n#...#######..........G.#...#####\n#...#######...#####G......######\n##...######..#######G.....#.##.#\n###.G.#####.#########G.........#\n###G..#####.#########.......#.E#\n###..######.#########..........#\n###.......#.#########.....E..E.#\n#####G...#..#########.......#..#\n####.G.#.#...#######.....G.....#\n########......#####...........##\n###########..................###\n##########.................#####\n##########.................#####\n############..E.........E.....##\n############.........E........##\n###############.#............E##\n##################...E..E..##.##\n####################.#E..####.##\n################.....######...##\n#################.#..###########\n################################")
(def foo "#######\n#..E#G#\n#.....#\n#G#...#\n#######")
(def bar "################################\n###############.##...###########\n##############..#...G.#..#######\n##############.............#####\n###############....G....G......#\n##########..........#..........#\n##########................##..##\n######...##..G...G.......####..#\n####..G..#G...............####.#\n#######......G....G.....G#####E#\n#######.................E.######\n########..G...............######\n######....G...#####E...G....####\n######..G..G.#######........####\n###.........#########.......E.##\n###..#..#...#########...E.....##\n######......#########.......####\n#####...G...#########.....######\n#####G......#########.....######\n#...#G..G....#######......######\n###...##......#####.......######\n####..##..G........E...E..######\n#####.####.....######...########\n###########..#...####...E.######\n###############...####..#...####\n###############...###...#.E.####\n#####################.#E....####\n#####################.#...######\n###################...##.#######\n##################..############\n##################...###########\n################################")

(def aa "#######\n#....G#\n###*#.#\n#E..#.#\n#######")
(def ab "#######\n#....G#\n###.#.#\n#E..#.#\n#######")


(def ac "################################\n#################.....##########\n#################..#.###########\n#################.........######\n##################......########\n#################G.GG###########\n###############...#..###########\n###############......G..########\n############..G.........########\n##########.G.....G......########\n##########......#.........#..###\n##########...................###\n#########G..G.#####....E.G.E..##\n######..G....#######...........#\n#######.....#########.........##\n#######..#..#########.....#.####\n##########..#########..G.##..###\n###########G#########...E...E.##\n#########.G.#########..........#\n#########GG..#######.......##.E#\n######.G......#####...##########\n#...##..G..............#########\n#...#...........###..E.#########\n#.G.............###...##########\n#................###############\n##.........E.....###############\n###.#..............#############\n###..G........E.....############\n###......E..........############\n###......#....#E#...############\n###....####.#...##.#############\n################################")
(def ad "################\n#.......G......#\n#G.............#\n#..............#\n#....###########\n#....###########\n#.......EG.....#\n################")
(def ae "################################\n#################.....##########\n#################..#.###########\n#################.........######\n##################......########\n#################G.GG###########\n###############...#..###########\n###############......G..########\n############..G.........########\n##########.G.....G......########\n##########......#.........#..###\n##########...................###\n#########G..G.#####....E.G.E..##\n######..G....#######...........#\n#######.....#########.........##\n#######..#..#########.....#.####\n##########..#########..G.##..###\n###########G#########...E...E.##\n#########.G.#########..........#\n#########GG..#######.......##.E#\n######.G......#####...##########\n#...##..G..............#########\n#...#...........###..E.#########\n#.G.............###...##########\n#................###############\n##.........E.....###############\n###.#..............#############\n###..G........E.....############\n###......E..........############\n###......#....#E#...############\n###....####.#...##.#############\n################################")


(def halp "################################\n#######.G...####################\n#########...####################\n#########.G.####################\n#########.######################\n#########.######################\n#########G######################\n#########.#...##################\n#########.....#..###############\n########...G....###.....########\n#######............G....########\n#######G....G.....G....#########\n######..G.....#####..G...#######\n######...G...#######......######\n#####.......#########....G..E###\n#####.####..#########G...#....##\n####..####..#########..G....E..#\n#####.####G.#########...E...E.##\n#########.E.#########.........##\n#####........#######.E........##\n######........#####...##...#..##\n###...................####.##.##\n###.............#########..#####\n#G#.#.....E.....#########..#####\n#...#...#......##########.######\n#.G............#########.E#E####\n#..............##########...####\n##..#..........##########.E#####\n#..#G..G......###########.######\n#.G.#..........#################\n#...#..#.......#################\n################################")


(def my-state (parse-input input))
(def my-round 0)

(comment
  (println example)


  (move-everybody (parse-input example-3))

  (nth (iterate move-everybody (parse-input example-3)) 47)

  (a* (parse-input example) [1 1] [1 1])


  (a* (parse-input "#####\n#...#\n#...#\n#E..#\n#####") [3 1] [1 3])

  (a* (parse-input "#####\n#...#\n#.#.#\n#E#.#\n#####") [3 1] [3 3])

  (a* (parse-input example-2) [1 2] [2 4])

  (let [state (parse-input example-2)
        goblins (get state \G)
        goblin-halos (into #{} (mapcat #(clear-points-from (:cave state) %) (keys goblins)))
        pos [1 2]]
    (sort-by (juxt (partial dist pos) identity) goblin-halos)
    #_(first (sort-by (fn [[pos _]] pos) (second (first (into (sorted-map) (group-by (fn [[[r c] _]] (+ (Math/abs (- row r)) (Math/abs (- col c)))) goblins)))))))

  (print-cave (parse-input movement-example))

  (print-cave my-state)
  (def my-state (parse-input input))
  (def my-round 0)
  (loop [state my-state
         round my-round]
    (let [elves (get state \E)
          goblins (get state \G)]
      (println round (count elves) (count goblins))
      (if (= 100 round)
        (do
          (alter-var-root #'my-state (constantly state))
          (alter-var-root #'my-round (constantly round))
          (print-cave state)
          #_(pprint (dissoc state :cave)))
        (if (or (empty? elves) (empty? goblins))
          (do
            (alter-var-root #'my-state (constantly state))
            (alter-var-root #'my-round (constantly round))
            (* round (->> [elves goblins] (mapcat vals) (map :hp) (reduce +))))
          (recur (move-everybody state) (inc round)))))))


;; 260850
;; 263625