(ns aoc22.day01
  (:require [aoc22.util :as util]
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

(assert (= (part1 testf) 24000))

(defn part2
  [f]
  (->> f
       read-data
       (map #(apply + %))
       sort
       reverse
       (take 3)
       (apply +)))

(assert (= (part2 testf) 45000))

;; The End
