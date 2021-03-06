(ns aoc2018.d10
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))


(def example "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>")


(def input "position=< 11118, -32416> velocity=<-1,  3>
position=< 43666,  43652> velocity=<-4, -4>
position=< 21928,  54518> velocity=<-2, -5>
position=< 54535, -32422> velocity=<-5,  3>
position=< 54586, -54155> velocity=<-5,  5>
position=< 43670,  54514> velocity=<-4, -5>
position=< 43690, -10686> velocity=<-4,  1>
position=< 32807, -10680> velocity=<-3,  1>
position=< 21924, -32420> velocity=<-2,  3>
position=< 54549, -21547> velocity=<-5,  2>
position=<-32367,  43654> velocity=< 3, -4>
position=< 21941, -32416> velocity=<-2,  3>
position=<-32379,  21915> velocity=< 3, -2>
position=< 32791,  43649> velocity=<-3, -4>
position=< 54569, -21555> velocity=<-5,  2>
position=<-21520, -10687> velocity=< 2,  1>
position=<-43230,  11055> velocity=< 4, -1>
position=< 11110, -32420> velocity=<-1,  3>
position=<-54101, -32418> velocity=< 5,  3>
position=<-21512, -43288> velocity=< 2,  4>
position=< 32835,  11053> velocity=<-3, -1>
position=<-32362,  43647> velocity=< 3, -4>
position=< 21943,  21922> velocity=<-2, -2>
position=< 43658,  32786> velocity=<-4, -3>
position=<-32391,  32789> velocity=< 3, -3>
position=< 43678, -10679> velocity=<-4,  1>
position=<-43234, -43287> velocity=< 4,  4>
position=< 32799,  32785> velocity=<-3, -3>
position=< 43666,  11053> velocity=<-4, -1>
position=<-21531,  11047> velocity=< 2, -1>
position=<-10624,  21913> velocity=< 1, -2>
position=< 43690, -54150> velocity=<-4,  5>
position=<-10627, -10679> velocity=< 1,  1>
position=< 11076,  43656> velocity=<-1, -4>
position=< 32848, -21555> velocity=<-3,  2>
position=<-43276,  11046> velocity=< 4, -1>
position=<-10627,  21918> velocity=< 1, -2>
position=<-21512,  11049> velocity=< 2, -1>
position=<-54140,  21913> velocity=< 5, -2>
position=<-54101,  11048> velocity=< 5, -1>
position=<-21536, -21547> velocity=< 2,  2>
position=< 43671,  21914> velocity=<-4, -2>
position=< 32847,  21921> velocity=<-3, -2>
position=< 43658, -43288> velocity=<-4,  4>
position=< 21941, -54150> velocity=<-2,  5>
position=<-43270,  11055> velocity=< 4, -1>
position=<-21483,  32787> velocity=< 2, -3>
position=<-32411, -32421> velocity=< 3,  3>
position=< 32819,  43655> velocity=<-3, -4>
position=< 21956,  21917> velocity=<-2, -2>
position=<-54085, -21555> velocity=< 5,  2>
position=< 21948,  54515> velocity=<-2, -5>
position=<-54127,  32780> velocity=< 5, -3>
position=<-10621, -54151> velocity=< 1,  5>
position=<-21544, -43280> velocity=< 2,  4>
position=<-54124, -54155> velocity=< 5,  5>
position=<-32359, -32422> velocity=< 3,  3>
position=<-10669,  54518> velocity=< 1, -5>
position=<-54094,  32780> velocity=< 5, -3>
position=<-32358, -21546> velocity=< 3,  2>
position=<-21544,  21920> velocity=< 2, -2>
position=<-54092, -10688> velocity=< 5,  1>
position=<-32391,  11049> velocity=< 3, -1>
position=< 43661,  32780> velocity=<-4, -3>
position=<-10673, -54156> velocity=< 1,  5>
position=<-54117, -10680> velocity=< 5,  1>
position=< 32792,  43651> velocity=<-3, -4>
position=<-10673,  54518> velocity=< 1, -5>
position=< 11113, -10682> velocity=<-1,  1>
position=< 54534,  32780> velocity=<-5, -3>
position=< 32835,  21913> velocity=<-3, -2>
position=<-10677, -32416> velocity=< 1,  3>
position=<-32403, -54149> velocity=< 3,  5>
position=<-10669,  54515> velocity=< 1, -5>
position=<-21487,  54523> velocity=< 2, -5>
position=<-54084,  43655> velocity=< 5, -4>
position=<-10626,  21922> velocity=< 1, -2>
position=< 54530, -10688> velocity=<-5,  1>
position=< 43677, -43285> velocity=<-4,  4>
position=< 43702,  11049> velocity=<-4, -1>
position=<-32379,  32785> velocity=< 3, -3>
position=< 11105, -10680> velocity=<-1,  1>
position=<-21494,  43647> velocity=< 2, -4>
position=< 43658, -10683> velocity=<-4,  1>
position=< 21985, -32413> velocity=<-2,  3>
position=<-21508, -43280> velocity=< 2,  4>
position=< 21981,  21913> velocity=<-2, -2>
position=< 21943,  11046> velocity=<-2, -1>
position=< 11076,  54518> velocity=<-1, -5>
position=<-10643,  54523> velocity=< 1, -5>
position=< 11109,  54523> velocity=<-1, -5>
position=<-43254, -32420> velocity=< 4,  3>
position=< 21924,  32783> velocity=<-2, -3>
position=<-43266, -54152> velocity=< 4,  5>
position=<-21507,  54523> velocity=< 2, -5>
position=<-21511, -43280> velocity=< 2,  4>
position=< 11102, -54156> velocity=<-1,  5>
position=< 32842,  43656> velocity=<-3, -4>
position=<-43234, -32416> velocity=< 4,  3>
position=< 21924,  21913> velocity=<-2, -2>
position=<-21526, -54151> velocity=< 2,  5>
position=<-43257,  54514> velocity=< 4, -5>
position=< 54545,  54517> velocity=<-5, -5>
position=<-43225,  32781> velocity=< 4, -3>
position=< 54527, -32422> velocity=<-5,  3>
position=<-10645, -32414> velocity=< 1,  3>
position=<-21512, -43282> velocity=< 2,  4>
position=<-10629, -10681> velocity=< 1,  1>
position=< 32841,  43647> velocity=<-3, -4>
position=<-54134,  11050> velocity=< 5, -1>
position=< 54577,  54514> velocity=<-5, -5>
position=<-32403, -32421> velocity=< 3,  3>
position=< 21924, -54149> velocity=<-2,  5>
position=<-21525,  32789> velocity=< 2, -3>
position=< 11116, -43284> velocity=<-1,  4>
position=<-54101, -10680> velocity=< 5,  1>
position=< 32847,  11050> velocity=<-3, -1>
position=< 54538,  11047> velocity=<-5, -1>
position=< 54569,  11052> velocity=<-5, -1>
position=< 54566,  32789> velocity=<-5, -3>
position=<-43235,  21922> velocity=< 4, -2>
position=< 11115,  32789> velocity=<-1, -3>
position=<-43238, -21548> velocity=< 4,  2>
position=< 54586,  43656> velocity=<-5, -4>
position=<-10645, -43284> velocity=< 1,  4>
position=<-32383,  43648> velocity=< 3, -4>
position=< 43669, -54156> velocity=<-4,  5>
position=<-32387,  43650> velocity=< 3, -4>
position=< 21966, -43280> velocity=<-2,  4>
position=<-10674,  11050> velocity=< 1, -1>
position=<-32385, -32419> velocity=< 3,  3>
position=< 32847, -21548> velocity=<-3,  2>
position=<-32395,  21913> velocity=< 3, -2>
position=< 32840, -21549> velocity=<-3,  2>
position=< 32847,  43651> velocity=<-3, -4>
position=<-32363, -32413> velocity=< 3,  3>
position=< 54583,  54523> velocity=<-5, -5>
position=< 21942, -32422> velocity=<-2,  3>
position=< 43661, -21555> velocity=<-4,  2>
position=<-21536, -21550> velocity=< 2,  2>
position=<-21493, -43285> velocity=< 2,  4>
position=<-21535, -43289> velocity=< 2,  4>
position=< 11065, -32416> velocity=<-1,  3>
position=< 21967,  11046> velocity=<-2, -1>
position=< 11113,  11047> velocity=<-1, -1>
position=< 43690,  32784> velocity=<-4, -3>
position=<-10621, -10681> velocity=< 1,  1>
position=<-32379,  43648> velocity=< 3, -4>
position=< 54581,  43649> velocity=<-5, -4>
position=<-54085, -10683> velocity=< 5,  1>
position=<-10674,  21917> velocity=< 1, -2>
position=<-32355, -54148> velocity=< 3,  5>
position=<-54085, -32417> velocity=< 5,  3>
position=<-54087, -32422> velocity=< 5,  3>
position=<-43218, -32414> velocity=< 4,  3>
position=< 11057, -21546> velocity=<-1,  2>
position=<-21536,  43649> velocity=< 2, -4>
position=<-32351, -43281> velocity=< 3,  4>
position=< 11077,  32783> velocity=<-1, -3>
position=< 21949,  11051> velocity=<-2, -1>
position=<-54110,  54523> velocity=< 5, -5>
position=< 11081, -32417> velocity=<-1,  3>
position=<-43254,  21919> velocity=< 4, -2>
position=< 32843,  21916> velocity=<-3, -2>
position=< 54578, -54155> velocity=<-5,  5>
position=<-43217, -32421> velocity=< 4,  3>
position=<-54101,  54519> velocity=< 5, -5>
position=<-10669,  32782> velocity=< 1, -3>
position=< 11113, -21547> velocity=<-1,  2>
position=<-21526,  54523> velocity=< 2, -5>
position=< 21985, -32415> velocity=<-2,  3>
position=<-32409,  32784> velocity=< 3, -3>
position=< 43711,  32782> velocity=<-4, -3>
position=< 21981, -54147> velocity=<-2,  5>
position=<-10667,  54518> velocity=< 1, -5>
position=< 54541,  54521> velocity=<-5, -5>
position=< 11078,  54523> velocity=<-1, -5>
position=<-43269, -54156> velocity=< 4,  5>
position=< 32799,  54517> velocity=<-3, -5>
position=<-43262, -21547> velocity=< 4,  2>
position=<-43218, -21555> velocity=< 4,  2>
position=<-43257,  43649> velocity=< 4, -4>
position=<-32379, -43280> velocity=< 3,  4>
position=<-32379, -54149> velocity=< 3,  5>
position=<-32363,  32787> velocity=< 3, -3>
position=<-32399,  32784> velocity=< 3, -3>
position=<-54089,  54515> velocity=< 5, -5>
position=<-54132, -10685> velocity=< 5,  1>
position=<-10621,  11049> velocity=< 1, -1>
position=<-43258,  54514> velocity=< 4, -5>
position=<-43234,  21914> velocity=< 4, -2>
position=< 43675, -10679> velocity=<-4,  1>
position=< 32802,  11046> velocity=<-3, -1>
position=<-32385,  21919> velocity=< 3, -2>
position=< 32792,  54514> velocity=<-3, -5>
position=< 54533,  43656> velocity=<-5, -4>
position=< 43716,  43647> velocity=<-4, -4>
position=< 43690, -43289> velocity=<-4,  4>
position=<-21485,  21922> velocity=< 2, -2>
position=<-54121,  32789> velocity=< 5, -3>
position=<-43268, -43285> velocity=< 4,  4>
position=<-32411, -32413> velocity=< 3,  3>
position=<-43234,  32788> velocity=< 4, -3>
position=< 11101, -43288> velocity=<-1,  4>
position=<-43268, -21555> velocity=< 4,  2>
position=<-43217, -21549> velocity=< 4,  2>
position=< 54533, -21555> velocity=<-5,  2>
position=<-54135,  11046> velocity=< 5, -1>
position=<-43236,  54523> velocity=< 4, -5>
position=< 21927, -21555> velocity=<-2,  2>
position=<-54145, -21551> velocity=< 5,  2>
position=< 11085, -54148> velocity=<-1,  5>
position=<-10653,  21914> velocity=< 1, -2>
position=< 54566,  43656> velocity=<-5, -4>
position=<-32362, -43280> velocity=< 3,  4>
position=<-10629,  43647> velocity=< 1, -4>
position=<-21495,  32786> velocity=< 2, -3>
position=<-32354,  21922> velocity=< 3, -2>
position=<-54089,  54521> velocity=< 5, -5>
position=< 54542, -32413> velocity=<-5,  3>
position=< 54577, -32419> velocity=<-5,  3>
position=< 11065, -21546> velocity=<-1,  2>
position=<-54112,  21922> velocity=< 5, -2>
position=<-10656,  21914> velocity=< 1, -2>
position=< 11077, -43289> velocity=<-1,  4>
position=< 32803, -43285> velocity=<-3,  4>
position=< 32847, -54152> velocity=<-3,  5>
position=< 32823,  43654> velocity=<-3, -4>
position=< 11118, -32417> velocity=<-1,  3>
position=<-43219, -43284> velocity=< 4,  4>
position=<-54112, -54147> velocity=< 5,  5>
position=<-54105,  21920> velocity=< 5, -2>
position=< 32850,  21913> velocity=<-3, -2>
position=< 32791, -21547> velocity=<-3,  2>
position=< 54549,  11053> velocity=<-5, -1>
position=<-54113, -32419> velocity=< 5,  3>
position=<-21486,  21913> velocity=< 2, -2>
position=<-10653,  11055> velocity=< 1, -1>
position=< 43671, -43287> velocity=<-4,  4>
position=< 54549,  54521> velocity=<-5, -5>
position=<-32353, -43280> velocity=< 3,  4>
position=<-32379, -54150> velocity=< 3,  5>
position=<-10677,  54517> velocity=< 1, -5>
position=<-21523,  43647> velocity=< 2, -4>
position=< 11106, -21555> velocity=<-1,  2>
position=< 11109, -54147> velocity=<-1,  5>
position=<-21488,  43652> velocity=< 2, -4>
position=< 43667,  54518> velocity=<-4, -5>
position=<-21483,  11047> velocity=< 2, -1>
position=<-32360,  43651> velocity=< 3, -4>
position=<-54109, -32413> velocity=< 5,  3>
position=<-54113, -10686> velocity=< 5,  1>
position=<-54128, -32413> velocity=< 5,  3>
position=< 32804, -43287> velocity=<-3,  4>
position=<-32395,  54523> velocity=< 3, -5>
position=<-10645, -10683> velocity=< 1,  1>
position=< 21952, -32421> velocity=<-2,  3>
position=< 11065, -32422> velocity=<-1,  3>
position=<-43243, -43280> velocity=< 4,  4>
position=<-43222, -43286> velocity=< 4,  4>
position=< 43674,  43655> velocity=<-4, -4>
position=<-43238,  43654> velocity=< 4, -4>
position=< 21985, -54149> velocity=<-2,  5>
position=< 32808, -54156> velocity=<-3,  5>
position=<-54105,  54522> velocity=< 5, -5>
position=< 21951, -32420> velocity=<-2,  3>
position=<-21519,  54518> velocity=< 2, -5>
position=< 11081, -43289> velocity=<-1,  4>
position=< 43701,  54514> velocity=<-4, -5>
position=< 21945, -54155> velocity=<-2,  5>
position=<-21520,  11055> velocity=< 2, -1>
position=< 43719, -43281> velocity=<-4,  4>
position=<-54113,  54515> velocity=< 5, -5>
position=< 43687,  11046> velocity=<-4, -1>
position=<-10659,  21913> velocity=< 1, -2>
position=< 21980, -54150> velocity=<-2,  5>
position=<-10627,  11055> velocity=< 1, -1>
position=< 21940, -21546> velocity=<-2,  2>
position=< 43660, -43285> velocity=<-4,  4>
position=<-43249, -32413> velocity=< 4,  3>
position=<-32395, -43289> velocity=< 3,  4>
position=<-54121,  32780> velocity=< 5, -3>
position=<-32384, -10681> velocity=< 3,  1>
position=< 43675,  43653> velocity=<-4, -4>
position=< 54549, -32418> velocity=<-5,  3>
position=< 54549, -54148> velocity=<-5,  5>
position=< 43684, -32416> velocity=<-4,  3>
position=< 43674, -10688> velocity=<-4,  1>
position=<-32387, -10680> velocity=< 3,  1>
position=<-32354,  11046> velocity=< 3, -1>
position=<-32375, -32413> velocity=< 3,  3>
position=< 11101,  11046> velocity=<-1, -1>
position=< 11076, -54152> velocity=<-1,  5>
position=< 54576,  43656> velocity=<-5, -4>
position=<-43225,  21914> velocity=< 4, -2>")


(defn parse-line [line]
  (let [[_ pos vel] (re-matches #".*<([^>]+)>.*<([^>]+)>" line)
        [pos-x pos-y] (edn/read-string (str \[ pos \]))
        [vel-x vel-y] (edn/read-string (str \[ vel \]))]
    {:pos {:x pos-x, :y pos-y}
     :vel {:x vel-x, :y vel-y}}))


(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-line)
       (reduce (fn [pos->vel {:keys [pos vel]}]
                 (assoc! pos->vel pos [vel]))
               (transient {}))
       persistent!))


(defn min-max [points]
  (reduce (fn [{{min-x :x, min-y :y} :min
                {max-x :x, max-y :y} :max :as minmax}
               {:keys [x y] :as pos}]
            (if (nil? minmax)
              {:min pos, :max pos}
              (cond-> minmax
                      (< x min-x) (assoc-in [:min :x] x)
                      (< y min-y) (assoc-in [:min :y] y)
                      (> x max-x) (assoc-in [:max :x] x)
                      (> y max-y) (assoc-in [:max :y] y))))
          nil
          (keys points)))


(defn print-points [points]
  (let [{{min-x :x, min-y :y} :min
         {max-x :x, max-y :y} :max} (min-max points)]
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (if (get points {:x x, :y y})
          (print "#")
          (print ".")))
      (println)))
  (println))


(defn point->str [points point]
  (if (get points point) "#" "."))


(defn row->str [points y x-range]
  (->> (reduce (fn [row x]
                 (conj row (point->str points {:x x, :y y})))
               []
               x-range)
       (apply str)))


(defn points->str [points]
  (let [{{min-x :x, min-y :y} :min
         {max-x :x, max-y :y} :max} (min-max points)]
    (->> (reduce (fn [rows y]
                   (conj rows (row->str points y (range min-x (inc max-x)))))
                 []
                 (range min-y (inc max-y)))
         (str/join "\n"))))


(defn update-points [points]
  (-> (reduce-kv (fn [new-points pos vels]
                   (reduce (fn [new-points vel]
                             (let [new-pos (merge-with + pos vel)]
                               (update new-points new-pos (fnil conj []) vel)))
                           new-points
                           vels))
                 {}
                 points)))


(defn reverse-points [points]
  (-> (reduce-kv (fn [new-points pos vels]
                   (reduce (fn [new-points vel]
                             (let [new-pos (merge-with - pos vel)]
                               (update new-points new-pos (fnil conj []) vel)))
                           new-points
                           vels))
                 {}
                 points)))


(defn width [points]
  (let [{{min-x :x} :min, {max-x :x} :max} (min-max points)]
    (- max-x min-x)))


(defn solve-1 [input]
  (let [points (parse-input input)]
    (->> points
         (iterate update-points)
         (partition 2 1)
         (drop-while (fn [[a b]] (< (width b) (width a))))
         ffirst
         points->str)))


(defn solve-2 [input]
  (let [points (parse-input input)]
    (loop [points points
           last-width nil
           step 0]
      (let [{{min-x :x} :min, {max-x :x} :max} (min-max points)
            width (- max-x min-x)]
        (if (nil? last-width)
          (recur (update-points points) width (inc step))
          (if (< last-width width)
            (dec step)
            (recur (update-points points) width (inc step))))))))
