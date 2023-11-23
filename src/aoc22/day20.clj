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
  "Insert a value v at position i in the collection"
  [coll i v]
  (if (zero? i)
    (concat coll (list v))
    (concat (take i coll) (list v) (drop i coll))))
  
(defn mix-element
  "Mix the element at the given position in the reference collection"
  [ordering index coll]
  (let [shift (nth coll index)
        pos (.indexOf ordering index)
        n (dec (count ordering))
        index' (mod (+ pos shift) n)]
    (-> ordering
        (delete-at pos)
        (insert-at index' index))))

(defn mix
  "Mix the ordering using the given numbers"
  [ordering numbers]
  (reduce (fn [ord index]
            (mix-element ord index numbers))
          ordering
          (range (count numbers))))

(defn nth-mod
  "Get the nth element modulo the length of the collection"
  [coll n]
  (nth coll (mod n (count coll))))

(defn- final-result
  [numbers]
  (let [i0 (.indexOf numbers 0)]
    (->> (for [i [1000 2000 3000]]
           (nth-mod numbers (+ i i0)))
         (apply +))))

(defn part1
  [fname]
  (let [numbers (read-data fname)
        ordering (range (count numbers))]
    ;; (println "Zero at position" i0)
    (->> numbers
        (mix ordering)
        (map #(nth numbers %))
        final-result)))

(defn part2
  [fname]
  (let [numbers (->> fname
                     read-data
                     (map #(* % 811589153)))]
    (->> (reduce (fn [ord _] (mix ord numbers))
                 (range (count numbers))
                 (range 10))
         (map #(nth numbers %))
         final-result)))

(comment 
  (assert (= 3 (part1 testf)))
  (assert (= 1623178306 (part2 testf))))

;; The End