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
  "Read in the data as a list of the vertices for each wall"
  ;; read-data :: IO File -> Vector (Vector Coord)
  [f]
  (->> f
       util/import-data
       (map #(str/split % #" -> "))
       (util/mapmap read-coord)))

(defn extent
  "Find the extent of the matrix to fit the points"
  ;; extent :: Vector (Vector Coord) -> [Coord Coord]
  [coll]
  (let [pts (->> coll
                 flatten
                 (partition 2))]
    [(apply (juxt min max) (map first pts))
     (apply (juxt min max) (map second pts))]))

(defn line-points
  "Return all the points in a horizontal or vertical line segment from point 1 to point 2"
  ;; line-points :: [Coord Coord] -> Vector Coord
  [[u1 v1] [u2 v2]]
  (cond
    (and (= u1 u2) (> v2 v1)) (mapv #(vector u1 %) (range v1 (inc v2)))
    (and (= u1 u2) (> v1 v2)) (mapv #(vector u1 %) (range v2 (inc v1)))
    (and (= v1 v2) (> u2 u1)) (mapv #(vector % v1) (range u1 (inc u2)))
    (and (= v1 v2) (> u1 u2)) (mapv #(vector % v1) (range u2 (inc u1)))
    :else (assert false "Neither a horizontal nor vertical line")))

(defn path-points
  "All the points in a line running through the given list of vertices"
  ;; all-line-points :: Vector Vector Coords -> List List Coords
  [vertices]
  (let [a (drop-last 1 vertices)
        b (rest vertices)]
    (apply concat (mapv line-points a b))))

(defn zero-matrix
  "Make a matrix of integer 0"
  [r c]
  (-> (* r c)
      (repeat 0)
      (m/reshape [r c])))

(defn create-matrix
  "Load up a matrix with all the walls. Walls are 1, sand is 2"
  [corners [[rmin rmax] [_ cmax]]]
  (let [rows (inc (- rmax rmin))
        cols (inc cmax)
        mat (zero-matrix rows cols)]
    (reduce (fn [m [r c]] (m/mset m (- r rmin) c 1))
            mat
            (apply concat (map path-points corners)))))

(defn extend-matrix
  "Extend the matrix for part 2"
  [mat]
  (let [[rows cols] (m/shape mat)
        mat-right (zero-matrix rows 2) ; Add two more cols to provide the floor
        mat-tb (zero-matrix (+ 250 rows) (+ cols 2))] ; Extend the matrix 'enough' on top and bottom
    (as-> mat <>
      (m/join-along 1 <> mat-right)     ; add matrix to RHS
      (m/join mat-tb <> mat-tb)         ; add matrix to top and bottom
      (m/set-column <> (inc cols) 1)))) ; add the floor to the far right edge

(defn safe-mget
  "Return nil if m/mget is out of bounds"
  [mat r c]
  {:pre [(seq mat)]}
  (let [[rows cols] (m/shape mat)]
    (if (or (neg? r) (neg? c) (>= r rows) (>= c cols))
      nil
      (m/mget mat r c))))

(defn drop-unit-sand
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
                ;; See if the sand input is backed up
                (= 2 (safe-mget m (- 500 r-offset) 0)) nil
                ;; else do nothing
                :else st)]
      (if (nil? st')
        nil
        ;;else
        (if (= (:m st) (:m st'))
          (:m st')
          ;; else
          (recur st'))))))

(defn fill-with-sand-part1
  "Fill with sand"
  [corners]
  (let [ex (extent corners)
        r-offset (first (first ex))]
    (loop [mat (create-matrix corners ex)
           count 0]
      (let [mat' (drop-unit-sand mat r-offset)]
        (if (nil? mat')
          count
          (recur mat' (inc count)))))))

(defn fill-sand
  "Drop units of sand until it's full"
  [matrix r-offset]
  (loop [mat matrix
         counter 0]
    (if-let [mat' (drop-unit-sand mat r-offset)]
      (recur mat' (inc counter))
      counter)))

(defn fill-with-sand-part2
  [corners]
  (let [[[rmin _] [_ _] :as ex] (extent corners)
        m0 (create-matrix corners ex)
        m1 (extend-matrix m0)
        [rows _] (m/shape m0)
        r-offset (- rmin (+ rows 250))] ;; because we extended by this number of rows
    (fill-sand m1 r-offset)))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       fill-with-sand-part1))

(assert (= 24 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data
       fill-with-sand-part2
       inc))

(assert (= 93 (part2 testf)))
;; The End
