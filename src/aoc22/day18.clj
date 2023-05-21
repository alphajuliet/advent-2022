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

(defn faces
  "Return the six adjacent coords of the cube centred at coord"
  [coord]
  (let [dirs [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]]
    (map #(map + coord %) dirs)))

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
          :when (and (> j i)
                     (contains? (set (:faces (nth faces j)))
                                (:coord (nth faces i))))]
      (list i j))))

(defn part1
  [f]
  (let [coords (->> f read-data)
        n (count coords)
        faces (generate-faces coords)]
    (- (* n 6)
       (* 2 (count (adjacent-faces faces))))))

(comment
  (assert (= 64 (part1 testf))))

;; The End