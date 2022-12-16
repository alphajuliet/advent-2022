(ns aoc22.day14
  (:require [aoc22.util :as util]
            [clojure.core.matrix :as m]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def testf "data/day14-test.txt")
(def inputf "data/day14-input.txt")

(defn read-coord
  "Read a coordinate into a vector"
  [s]
  (->> (str/split s #",")
      (map edn/read-string)
      vec))

(defn read-data
  [f]
  (->> f
       util/import-data
       (map #(str/split % #" -> "))
       (util/mapmap read-coord)))

(defn extent
  "Find the extent of the matrix to fit the points"
  [coll]
  (let [pts (->> coll
                 flatten
                 (partition 2))]
    [(apply (juxt min max) (map first pts))
     (apply (juxt min max) (map second pts))]))

(defn line-points
  "Return all the points in a line from point 1 to point 2"
  [[u1 v1] [u2 v2]]
  (cond
    (and (= u1 u2) (> v2 v1)) (mapv #(vector u1 %) (range v1 (inc v2)))
    (and (= u1 u2) (> v1 v2)) (mapv #(vector u1 %) (range v2 (inc v1)))
    (and (= v1 v2) (> u2 u1)) (mapv #(vector % v1) (range u1 (inc u2)))
    (and (= v1 v2) (> u1 u2)) (mapv #(vector % v1) (range u2 (inc u1)))))

(defn all-line-points
  "All the points in a line running through the given corners"
  [corners]
  (let [a (drop-last 1 corners)
        b (rest corners)]
    (apply concat (map line-points a b))))

(defn zero-matrix
  "Make a matrix of integer 0"
  [r c]
  (-> (* r c)
      (repeat 0)
      (m/reshape [r c])))

(defn all-points
  "Load up a matrix with all the walls. Walls are 1, sand is 2"
  [corners [rex cex]]
  (let [rmin (first rex)
        rows (inc (- (second rex) rmin))
        cols (inc (second cex))
        mat (zero-matrix rows cols)]
    (reduce (fn [m [r c]]
              (m/mset m (- r rmin) c 1))
            mat
            (apply concat (map all-line-points corners)))))

(defn safe-mget
  "Return nil if m/mget is out of bounds"
  [mat r c]
  {:pre [(seq mat)]}
  (let [[rows cols] (m/shape mat)]
    (if (or (neg? r) (neg? c)
            (>= r rows) (>= c cols))
      nil
      ;; else
      (m/mget mat r c))))

(defn move-sand
  "Drop a unit of sand until it stops or exits the sides (nil). Gravity is in the direction of increasing c"
  [mat rmin]
  (loop [st {:m mat, :r (- 500 rmin), :c 0}]
    (let [{:keys [m r c]} st
          st' (cond
                ;; Check spot directly below
                (nil? (safe-mget m r (inc c))) nil
                (zero? (safe-mget m r (inc c))) (-> st
                                                    (update :m #(m/mset % r c 0))
                                                    (update :m #(m/mset % r (inc c) 2))
                                                    (update :c inc))
                ;; Check below and to the left
                (nil? (safe-mget m (dec r) (inc c))) nil
                (zero? (safe-mget m (dec r) (inc c))) (-> st
                                                          (update :m #(m/mset % r c 0))
                                                          (update :m #(m/mset % (dec r) (inc c) 2))
                                                          (update :r dec)
                                                          (update :c inc))
                ;; Check below and to the right
                (nil? (safe-mget m (inc r) (inc c))) nil
                (zero? (safe-mget m (inc r) (inc c))) (-> st
                                                          (update :m #(m/mset % r c 0))
                                                          (update :m #(m/mset % (inc r) (inc c) 2))
                                                          (update :r inc)
                                                          (update :c inc))
                ;; else do nothing
                :else st)]
      (if (nil? st')
        nil
        ;;else
        (if (= (:m st) (:m st'))
          (:m st')
          ;; else
          (recur st'))))
    ))

(defn fill-with-sand
  [corners]
  (let [ex (extent corners)
        rmin (first (first ex))]
    (loop [mat (all-points corners ex)
           count 0]
      (let [mat' (move-sand mat rmin)]
        (if (nil? mat')
          count
          (recur mat' (inc count)))))))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       fill-with-sand))

(assert (= 24 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part2 testf)))
;; The End
