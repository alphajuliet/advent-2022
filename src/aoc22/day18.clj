(ns aoc22.day18 
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def testf "data/day18-test.txt")
(def inputf "data/day18-input.txt")

(defn read-data
  "Read in the cube coords"
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(edn/read-string (str "(" % ")")))))

(defn drop-nth
  "Drop the nth element of a collection"
  [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn vec+
  [a b]
  (map + a b))

(defn faces
  "Return the six adjacent coords of the cube centred at coord"
  [coord]
  (let [dirs [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]]
    (map #(vec+ coord %) dirs)))

(defn generate-faces
"Generate all the faces for each cube"
  [coords]
  (for [c coords]
    {:coord c, :faces (faces c)}))

(defn adjacent-faces
  [faces]
  (let [n (count faces)]
    (for [i (range n)
          j (range n)
          :when (and (> i j)
                     (contains? (:faces (nth faces j)) (:coord (nth faces i))))]
      (list i j))))

(defn count-excluding
  "Sum the distinct elements after dropping the nth entry in each"
  [n coll]
  (->> coll
       (map #(drop-nth n %))
       distinct
       count))

(defn part1
  [f]
  (let [coords (read-data f)]
    (->> [0 1 2]
         (map #(count-excluding % coords))
         (apply +)
         (* 2))))

(comment
  (assert (= 64 (part1 testf))))

;; The End