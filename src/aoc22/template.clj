(ns aoc22.dayXX
  (:require [aoc22.util :as util]))

(def testf "data/dayXX-test.txt")
(def inputf "data/dayXX-input.txt")

(defn read-data
  [f]
  (-> f
      util/import-data))


(defn part1
  [f]
  (-> f 
      read-data))

(defn part2
  [f]
  (-> f
      read-data))

;; The End
