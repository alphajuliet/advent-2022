(ns aoc22.day15
  (:require
   [clojure.edn :as edn]
   [instaparse.core :as insta]
   [aoc22.util :as util]))

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
  "Create two lists: sensors and beacons"
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
  [[sensor beacon] point]
  (<= (distance sensor point) (distance sensor beacon)))

(defn line-points
  "All the points on a line in a grid with a given extent"
  [[[x0 _] [x1 _]] y]
  (for [x (range (- x0 1000000) (+ x1 1000000))]
    [x y]))

(defn inclusions
  "Count the number of points on the line y that lie within the range of the sensors"
  [y points]
  (let [ext (extent (apply concat points))
        line (line-points ext y)]
    (for [sb points]
      (filter #(within? sb %) line))))

;;------------------------------
(defn part1
  [f row]
  (->> f
       read-data
       (inclusions row)
       (apply concat)
       distinct
       count
       dec))

;; (assert (= 0 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part2 testf)))
;; The End
