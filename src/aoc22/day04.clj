(ns aoc22.day04
  (:require [aoc22.util :as util]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def testf "data/day04-test.txt")
(def inputf "data/day04-input.txt")

(defn explode-range
  "Explode the range 'x-y' into the range x to y inclusive"
  [s]
  (let [[a b] (map edn/read-string (str/split s #"-"))]
    (range a (inc b))))

(defn read-data
  [f]
  (->> f
       util/import-data
       (map #(str/split % #","))
       (util/mapmap explode-range)
       (map #(sort-by count > %))))

(defn fully-contains?
  "b is fully contained in a?"
  [[a b]]
  (= (set a) (set/union (set a) (set b))))

(defn overlaps?
  "a and b overlap"
  [[a b]]
  (not= #{} (set/intersection (set a) (set b))))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       (map fully-contains?)
       (util/count-if true?)))

(assert (= 2 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data
       (map overlaps?)
       (util/count-if true?)))

(assert (= 4 (part2 testf)))
;; The End
