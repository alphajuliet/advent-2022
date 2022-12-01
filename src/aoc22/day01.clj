(ns aoc22.day01
  (:require [aoc22.util :as util]
            [clojure.math :as math]
            [clojure.edn :as edn]))

(def testf "data/day01-test.txt")
(def inputf "data/day01-input.txt")

(defn read-data
  [f]
  (->> f
       util/import-data
       (map edn/read-string)
       (partition-by nil?)
       (remove #(= (list nil) %))))

(defn part1
  [f]
  (->> f
       read-data
       (map #(apply + %))
       (apply max)))

(defn part2
  [f]
  (-> f
      read-data))

;; The End
