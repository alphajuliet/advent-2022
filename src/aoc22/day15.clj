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

(defn covered
  "List all the coords covered by the bounding diamond"
  [[xs ys :as sensor] beacon]
  (let [d (distance sensor beacon)]
    (for [x (range (- xs d) (inc (+ xs d)))
          y (range (- ys d) (inc (+ ys d)))
          :let [x0 (abs (- x xs))] ; save cpu?
          :when (and (<= y (+ ys (- d x0)))
                     (>= y (+ ys (- x0 d))))]
      [x y])))

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
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part2 testf)))
;; The End
