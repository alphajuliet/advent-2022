(ns aoc22.day20
  (:require [aoc22.util :as util]
            [clojure.edn :as edn]))

(def testf "data/day20-test.txt")
(def inputf "data/day20-input.txt")

(defn read-data
  [f]
  (->> f
       util/import-data
       (mapv edn/read-string)))

(defn delete-at
  "Delete element at position n in the collection"
  [coll n]
  (concat (take n coll) (drop (inc n) coll)))

(defn insert-at
  "Insert a value v at position n in the collection"
  [coll n v]
  (concat (take n coll) (list v) (drop n coll)))

(defn mix-element
  "Mix the element at the given position in the reference collection"
  [ordering index coll]
  (let [shift (nth coll index)
        pos (.indexOf ordering index)
        n (dec (count ordering))
        index' (mod (+ pos shift) n)
        index'' (if (zero? index') n index')]
    (-> ordering
        (delete-at pos)
        (insert-at index'' index))))

(defn mix
  "Mix the collection"
  [coll]
  (let [rng (range (count coll))]
    (->> (reduce (fn [rng' index]
                   (mix-element rng' index coll))
                 rng
                 rng)
         (map #(nth coll %)))))

(defn nth-mod
  "Get the nth element modulo the length of the collection"
  [coll n]
  (nth coll (mod n (dec (count coll)))))

(defn part1
  [fname]
  (let [coll (-> fname read-data mix)
        i0 (.indexOf coll 0)]
    ;; (println "Zero at position" i0)
    (->> (for [i [1000 2000 3000]]
           (nth-mod coll (+ i i0)))
         (apply +))))

(comment (assert (= 3 (part1 testf))))

;; The End