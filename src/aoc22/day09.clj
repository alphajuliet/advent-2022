(ns aoc22.day09
  (:require [aoc22.util :as util]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def testf "data/day09-test.txt")
(def test2f "data/day09-test2.txt")
(def inputf "data/day09-input.txt")

(defn nested-tail
  "Return the last item, whether nested or not"
  [c]
  (if (util/nested-coll? c)
    (last c)
    c))

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

(def init-state-part1
  {:head [0 0]
   :tail [0 0]
   :path []})

(defn move-1-tail
  "Given the location of the head, move the tail and return its position"
  ;; move-1-tail :: Coord -> Coord -> Coord
  [head tail]
  (if (<= (distance head tail) 1)
    tail
    (mapv + tail (clamp (mapv - head tail)))))

(def init-state-part2
  {:head [0 0]
   :tail (repeat 9 [0 0])
   :path []})

(defn move-n-tails
  [tails]
  (loop [in (rest tails)
         out (vector (first tails))]
    (let [t' (move-1-tail (last out) (first in))]
      (if (empty? (rest in))
        (conj out t')
        ;; else
        (recur (rest in) (conj out t'))))))

(defn move-tail-0
  "Move the tail as either one point, or a vector of points"
  [{:keys [head tail] :as st}]
  ;; move-tail-0 :: State -> Coord | List Coord
  (if (>= (count tail) 3)
    (let [t' (move-1-tail head (first tail))]
      (apply vector (move-n-tails (apply vector t' (rest tail)))))
    ;; else
    (move-1-tail head tail)))

(defn move-head-by-one
  "Move the head one space in the direction given and update the tail"
  [st dirv]
  (as-> st <>
    (update <> :head #(mapv + % dirv))
    (assoc <> :tail (move-tail-0 <>))
    (update <> :path #(conj % (nested-tail (:tail <>))))))

(defn move-head
  "Move the head the full distance along the given path"
  [st [dir distance]]
  (let [dirv (direction dir)]
    (reduce (fn [st' _] (move-head-by-one st' dirv))
            st
            (range distance))))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       (reduce move-head init-state-part1)
       :path
       distinct
       count))

(assert (= 13 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data
       (reduce move-head init-state-part2)
       :path
       distinct
       count))

(assert (= 36 (part2 test2f)))
;; The End
