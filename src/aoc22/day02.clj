(ns aoc22.day02
  (:require [aoc22.util :as util]
            [clojure.core.matrix :as m]
            [clojure.string :as str]))

(def testf "data/day02-test.txt")
(def inputf "data/day02-input.txt")

(defn read-data
  [f]
  (->> f
      util/import-data
      (map #(str/split % #" "))))

(def codes {"A" 0 "B" 1 "C" 2
            "X" 0 "Y" 1 "Z" 2})

(def result-1
  "Our result matrix for them (row) vs us (column) using order R-P-S"
  [[3 6 0]
   [0 3 6]
   [6 0 3]])

(def result-2
  "Desired result matrix in terms of R-P-S"
  [[2 0 1]
   [0 1 2]
   [1 2 0]])

(defn play-round-1
  [[them us]]
  (let [row (get codes them)
        col (get codes us)
        shape-points (inc col)]
    (+ (m/mget result-1 row col) shape-points)))

(defn play-round-2
  [[them outcome]]
  (let [row (get codes them)
        col (get codes outcome)
        play (m/mget result-2 row col)
        shape-points (inc play)]
    (+ (m/mget result-1 row play) shape-points)))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       (map play-round-1)
       (apply +)))

(assert (= 15 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data
       (map play-round-2)
       (apply +)))

(assert (= 12 (part2 testf)))

;; The End
