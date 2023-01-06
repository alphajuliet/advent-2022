(ns aoc22.day15
  (:require [aoc22.util :as util]
            [clojure.edn :as edn]
            [instaparse.core :as insta]))

(def testf "data/day15-test.txt")
(def inputf "data/day15-input.txt")

(def parse-sensor-data
  (insta/parser
   "<data> := line+
    <line> := <'Sensor at '> coord <': closest beacon is at '> coord <newline>
    coord := x <', '> y
    x := <'x='> number
    y := <'y='> number
    <number> := #'-?\\d+'
    newline := '\n'
    "))

(defn read-data
  "Create a list of sensor-beacon pairs"
  [f]
  (->> f
       slurp
       parse-sensor-data
       (insta/transform
        {:x edn/read-string
         :y edn/read-string
         :coord vector})
       (partition 2)))

(defn extent
  "Find the min and max of a collection of 2d points"
  [pts]
  (util/T [(apply (juxt min max) (map first pts))
           (apply (juxt min max) (map second pts))]))

(defn distance
  "Manhattan distance metric"
  [[u v] [x y]]
  (+ (abs (- u x))
     (abs (- v y))))

(defn within?
  [sensor beacon point]
  (<= (distance sensor point) (distance sensor beacon)))

(defn line-overlap
  [[sx sy :as sensor] beacon y]
  (let [d (distance sensor beacon)
        b (- d (abs (- sy y)))]
    (range (- sx b) (+ sx b))))

(defn line-coverage
  [y pairs]
  (for [[s b] pairs
        :when (<= (abs (- y (second s))) (distance s b))]
    (line-overlap s b y)))

(defn diamond
  "List the vertices of the bounding diamond centred on the sensor that touches the beacon"
  [sensor beacon]
  (let [d (distance sensor beacon)]
    (list (mapv + sensor [d 0])
          (mapv + sensor [0 d])
          (mapv - sensor [d 0])
          (mapv - sensor [0 d]))))

(defn diagonal
  "Return all the points on the diagonal from p1 to p2"
  [[x1 y1] [x2 y2]]
  (let [ix (if (> x2 x1) 1 -1)
        iy (if (> y2 y1) 1 -1)]
    (for [i (range (inc (abs (- x1 x2))))]
      [(+ x1 (* i ix)) (+ y1 (* i iy))])))

(defn boundary-plus-one
  "List all the coords one unit beyond the edge of the bounding diamond"
  [[sensor beacon]]
  (let [dia0 (diamond sensor beacon)
        dia1 (map (partial mapv +) dia0 [[1 0] [0 1] [-1 0] [0 -1]])]
    (map #(diagonal (first %) (second %))
         (partition 2 1 (conj dia1 (last dia1))))))

(defn get-candidates [pairs limit]
  (->> pairs
       (mapcat boundary-plus-one)
       (apply concat)
       (filter #(and (<= 0 (first %) limit) (<= 0 (second %) limit)))
       distinct))

(defn within-all?
  [point pairs]
  (filter (fn [[sensor beacon]]
            (within? sensor beacon point))
          pairs))

;;------------------------------
(defn part1
  [f y]
  (->> f
       read-data
       (line-coverage y)
       (apply concat)
       set
       count))

(assert (= 26 (part1 testf 10)) "Part 1 fail on test data")

(defn part2
  [f limit]
  (let [pairs (read-data f)
        candidates (get-candidates pairs limit)
        covers (map #(within-all? % pairs) candidates)
        idx (.indexOf covers '())]
    (->> idx
         (nth candidates)
         ((juxt (comp (partial * 4000000) first) second))
         (apply +))))

(assert (= 56000011 (part2 testf 20)))
;; The End
