(ns aoc22.day18
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.set :as set]))

(def testf "data/day18-test.txt")
(def inputf "data/day18-input.txt")

(def third #(nth % 2))

(defn read-data
  "Read in the cube coords"
  [f]
  (->> f
       slurp
       str/split-lines
       (map #(edn/read-string (str "(" % ")")))))

(defn bounding-box
  "Find the extent of the cubes"
  [coords]
  (let [x-min (apply min (map first coords))
        x-max (apply max (map first coords))
        y-min (apply min (map second coords))
        y-max (apply max (map second coords))
        z-min (apply min (map third coords))
        z-max (apply max (map third coords))]
    [[x-min y-min z-min] [x-max y-max z-max]]))

(defn is-inside?
  [[x y z] [[xmin ymin zmin] [xmax ymax zmax]]]
  (and (<= xmin x xmax)
       (<= ymin y ymax)
       (<= zmin z zmax)))

(defn neighbours
  "Return the six adjacent coords of the cube centred at coord"
  [coord]
  (let [dirs [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]]
    (map #(map + coord %) dirs)))

(defn generate-faces
"Generate all the faces for each cube"
  [coords]
  (for [c coords]
    {:coord c, :faces (neighbours c)}))

(defn adjacent-faces
  [faces]
  (let [n (count faces)]
    (for [i (range n)
          j (range n)
          :when (and (> j i)
                     (contains? (set (:faces (nth faces j)))
                                (:coord (nth faces i))))]
      (list i j))))

(defn cube-points
  "Given two ordered 3D coordinates, generate all points inside the cube they define plus, plus a layer one unit in thickness."
  [[c1 c2]]
  (let [ranges (map #(range (dec %1) (+ 2 %2)) c1 c2)]
    (apply (fn [xs ys zs]
             (for [x xs
                   y ys
                   z zs]
               [x y z]))
           ranges)))

(defn enclosing-points
  [bbox]
  (let [extent (apply (partial map #(list (dec %1) (+ 2 %2))) bbox)]
    (for [x (first extent)
          y (second extent)
          z (third extent)]
      [x y z])))

(defn count-enclosing-layer
  [[c1 c2]]
  (let [[x y z] (mapv (comp inc -) c2 c1)]
    (- (* (+ x 2) (+ y 2) (+ z 2))
       (* x y z))))

(defn exposed-cubes
  "Externally flood fill a 3D space defined by a list of coordinates, and return the found cubes."
  [cubes]
  (let [visited (atom (set cubes))
        bbox (bounding-box cubes)
        queue (atom #{(first bbox)})
        found (atom #{})]
    (while (seq @queue)
      (let [current (first @queue)]
        (swap! queue disj current)      ; Remove it from the queue
        (if (not (@visited current))
          (swap! visited conj current)    ; Add to visited
          (swap! queue into (filter #(is-inside? % bbox) (neighbours current))))))
    @visited))

(defn part1
  [f]
  (let [coords (->> f read-data)
        n (count coords)
        faces (generate-faces coords)]
    (- (* n 6)
       (* 2 (count (adjacent-faces faces))))))

(defn part2
  [f]
  (let [coords (->> f read-data)
        bbox (bounding-box coords)
        exposed (exposed-cubes coords)
        ;; adj-spaces (adjacent-faces (generate-faces spaces))
        ]
    exposed
    #_(map count [coords adj-faces spaces adj-spaces])
    #_(- (* 6 (count coords))       ; all cube faces
       (* 2 (count adj-faces))    ; minus common faces
       (* 6 (- (count spaces) (count-enclosing-layer bbox)))       ; minus faces of unreachable spaces
       (* -2 (count adj-spaces))  ; add back in common faces of adjacent spaces
       )))

(comment
  (assert (= 64 (part1 testf)))
  (assert (= 58 (part2 testf)))
  (assert (= 4282 (part1 inputf))))

;; The End
