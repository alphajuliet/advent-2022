(ns aoc22.day08
  (:require [aoc22.util :as util]
            [clojure.core.matrix :as m]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def testf "data/day08-test.txt")
(def inputf "data/day08-input.txt")

(defn read-data
  "Read the data and convert into a integer matrix"
  [f]
  (->> f
       util/import-data
       (map #(str/split % #""))
       (util/mapmap edn/read-string)))

(defn get-row-col
  "Get the four sight vectors to m[i j]"
  [m i j]
  (let [[rows cols] (m/shape m)
        i' (- rows i)
        j' (- cols j)
        row (m/get-row m i)
        col (m/get-column m j)]
    (list (reverse (take (inc j) row))
          (reverse (take (inc i) col))
          (take-last j' row)
          (take-last i' col))))

(defn visible?
  "Is the tree at m[i j] visible? It must be strictly greater than all the trees in at least one direction"
  [m i j]
  (let [x (m/mget m i j)
        v (get-row-col m i j)]
    (->> v
         (map #(apply max (drop 1 %)))
         (util/count-if #(> x %))
         pos?)))

(defn count-visible
  "Count tress visible from the edges"
  [m]
  (let [[rows cols] (m/shape m)
        perimeter (* 4 (dec rows))]
    (->> (for [i (range 1 (dec rows))
               j (range 1 (dec cols))]
           (visible? m i j))
         (util/count-if true?)
         (+ perimeter))))

(defn scenic-score
  [m i j]
  (let [x (m/mget m i j)
        v (get-row-col m i j)]
    (->> v
         (util/mapmap #(< % x))
         (map #(drop 1 %))
         (map #(util/take-upto false? %))
         (map count)
         (apply *))))

(defn find-most-scenic
  [m]
  (let [[rows cols] (m/shape m)]
    (->> (for [i (range 1 (dec rows))
               j (range 1 (dec cols))]
           (scenic-score m i j))
         (apply max))))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       count-visible))

(assert (= 21 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data
       find-most-scenic))

(assert (= 8 (part2 testf)))
;; The End
