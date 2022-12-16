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

(defn create-matrix
  "Load up a matrix  for part 1 with all the walls. Walls are 1, sand is 2"
  [corners [rex cex]]
  (let [rmin (first rex)
        rows (inc (- (second rex) rmin))
        cols (inc (second cex))
        mat (zero-matrix rows cols)]
    (reduce (fn [m [r c]]
              (m/mset m (- r rmin) c 1))
            mat
            (apply concat (map all-line-points corners)))))

(defn extend-matrix
  "Extend the matrix for part 2"
  [mat]
  (let [[rows cols] (m/shape mat)
        cols' (+ cols 2)
        mat-right (zero-matrix rows 2)
        mat-tb (zero-matrix (+ 6 rows) (+ cols 2))]
    (as-> mat <>
      (m/join-along 1 <> mat-right)
      (m/join mat-tb <> mat-tb)
      (m/set-column <> (inc cols) 1))))

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
  [mat r-offset]
  (loop [st {:m (m/mset mat (- 500 r-offset) 0 2) ; Set the initial unit of sand
             :r (- 500 r-offset)
             :c 0}]
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
                (= 2 (safe-mget m (- 500 r-offset) 0)) nil
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
        r-offset (first (first ex))]
    (loop [mat (create-matrix corners ex)
           count 0]
      (let [mat' (move-sand mat r-offset)]
        (if (nil? mat')
          count
          (recur mat' (inc count)))))))

(defn fill-sand [matrix r-offset]
  (loop [mat matrix
         count 0]
    (let [mat' (move-sand mat r-offset)]
      (if (nil? mat')
        count
        (recur mat' (inc count))))))

(defn fill-with-sand-2
  [corners]
  (let [ex (extent corners)
        m0 (create-matrix corners ex)
        m1 (extend-matrix m0)
        [rows _] (m/shape m0)
        rmin (first (first ex))
        r-offset (- rmin (+ rows 6))]
    (fill-sand m1 r-offset)))

;;------------------------------
;;
(defn part1
  [f]
  (->> f
       read-data
       fill-with-sand))

(assert (= 24 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data
       fill-with-sand-2
       inc))

;; (assert (= 93 (part2 testf)))
;; The End
