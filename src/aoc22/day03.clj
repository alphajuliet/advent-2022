(ns aoc22.day03
  (:require [aoc22.util :as util]
            [clojure.set :as set]))

(def testf  "data/day03-test.txt")
(def inputf "data/day03-input.txt")

(defn split-line
  "Return the first and second half of each collection"
  [s]
  (let [len (/ (count s) 2)]
    ((juxt take drop) len s)))

(defn read-data
  [f]
  (->> f
       util/import-data))

(defn priority
  "Priority of character c"
  [ch]
  (let [i (int ch)]
    (if (> i 90)
      (inc (- i (int \a)))
      (+ 27 (- i (int \A))))))

(defn common-priority
  "Find the priority of the common element of the two collections"
  [[large small]]
  (->> (set/intersection (set large) (set small))
       first
       priority))

(defn common-item
  "Find the common item that occurs in each element of the collection"
  [coll]
  (->> coll
       (map set)
       (apply set/intersection)))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       (map split-line)
       (map common-priority)
       (apply +)))

(assert (= 157 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data
       (partition 3)
       (map common-item)
       (map (comp priority first))
       (apply +)))

(assert (= 70 (part2 testf)))
;; The End
