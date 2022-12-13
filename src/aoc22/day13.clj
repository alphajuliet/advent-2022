(ns aoc22.day13
  (:require [aoc22.util :as util]
            [instaparse.core :as insta]
            [clojure.edn :as edn]))

(def testf "data/day13-test.txt")
(def inputf "data/day13-input.txt")

(defn read-data
  [f]
  (->> f
       util/import-data
       (map edn/read-string)
       (remove nil?)))

(defn compare-packets
  "Compare two packets according to the rules"
  [a b]
  (cond
    (and (number? a) (number? b)) (compare a b)
    (and (coll? a) (number? b)) (compare-packets a (list b))
    (and (number? a) (coll? b)) (compare-packets (list a) b)
    (and (empty? a) (empty? b)) 0
    (and (empty? a) (seq b)) -1
    (and (seq a) (empty? b)) 1
    (and (coll? a) (coll? b)) (let [x (compare-packets (first a) (first b))]
                                (if (zero? x)
                                  (compare-packets (rest a) (rest b))
                                  x))
    :else -1))

(defn add-dividers
  [coll]
  (conj coll [[2]] [[6]]))

(defn calc-decoder-key
  [coll]
  (let [d1 (inc (.indexOf coll [[2]]))
        d2 (inc (.indexOf coll [[6]]))]
    (* d1 d2)))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       (partition 2)
       (map-indexed #(list (inc %1) (apply compare-packets %2)))
       (filter #(neg? (second %)))
       (map first)
       (apply +)))

(assert (= 13 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data
       add-dividers
       (sort compare-packets)
       calc-decoder-key))

(assert (= 140 (part2 testf)))
;; The End
