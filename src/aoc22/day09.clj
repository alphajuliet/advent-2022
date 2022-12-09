(ns aoc22.day09
  (:require [aoc22.util :as util]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def testf "data/day09-test.txt")
(def inputf "data/day09-input.txt")

(defn read-data
  "Read input data as a list of maps"
  [f]
  (->> f
       util/import-data
       (map #(str/split % #" "))
       (map #(vector (keyword (first %))
                     (edn/read-string (second %))))))

(defn direction
  "Convert from directions to vectors"
  [dir]
  (case dir
    :R [1 0]
    :U [0 1]
    :L [-1 0]
    :D [0 -1]
    [0 0]))

(defn distance
  "Chebyshev distance"
  [[hx hy] [tx ty]]
  (max (abs (- hx tx)) (abs (- hy ty))))

(defn clamp
  [[x y]]
  [(util/clamp x -1 1) (util/clamp y -1 1)])

(defn move-tail
  "Given the location of the head, move the tail"
  [{:keys [head tail]}]
  (if (<= (distance head tail) 1)
    tail
    (map + tail (clamp (map - head tail)))))

(def init-state
  {:head [0 0]
   :tail [0 0]
   :path []})

(defn move-head-by-one
  "Move the head one space in the direction given"
  [st dirv]
  (as-> st <>
    (update <> :head #(mapv + % dirv))
    (assoc <> :tail (move-tail <>))
    (update <> :path #(conj % (:tail <>)))))

(defn move-head
  "Move the head the full distance along the given path"
  [st [dir distance]]
  (let [dirv (direction dir)]
    (reduce (fn [st' _]
              (move-head-by-one st' dirv))
            st
            (range distance))))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       (reduce move-head init-state)
       :path
       distinct
       count))

(assert (= 13 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part2 testf)))
;; The End
