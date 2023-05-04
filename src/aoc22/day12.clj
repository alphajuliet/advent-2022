(ns aoc22.day12
  (:require [aoc22.util :as util]
            [clojure.core.matrix :as m]
            [clojure.string :as str]
            [clojure.set :as set]))

(def testf "data/day12-test.txt")
(def inputf "data/day12-input.txt")

(defn read-data
  [f]
  (->> f
        util/import-data
        (map #(str/replace % "S" "a"))
        (map #(str/replace % "E" "z"))
        (map #(str/split % #""))
        (util/mapmap #(inc (- (int (first %)) (int \a))))))

(defn legal-position?
  [[r c] [i j]]
  (and (nat-int? i) (nat-int? j) (< i r) (< j c)))

(defn mfind
  "Find the first occurrence of val in mat"
  [mat val]
  (let [[_ c] (m/shape mat)
        pos (-> mat
                m/to-vector
                (.indexOf val))]
    ((juxt quot mod) pos c)))

(defn neighbours
  [dim [i j]]
  (let [deltas [[1 0] [0 1] [-1 0] [0 -1]]]
    (->> deltas
         (mapv #(mapv + [i j] %))
         (filter (partial legal-position? dim)))))

(defn not-in?
  [a b]
  (vec (set/difference (set b) (set a))))

(defn allowed-steps
  "Allowed adjacent squares not in the path."
  [mat [r c] path]
  (let [x (m/mget mat r c)
        dim (m/shape mat)
        nn (neighbours dim [r c])
        vv (mapv #(vector % (apply m/mget mat %)) nn)]
    (->> vv
         (filter #(<= (- (second %) x) 1))
         (mapv first)
         (not-in? path))))

(defn children [mat loc path]
  (allowed-steps mat loc path))

(defn remove-previous-states
  [new-states frontier visited]
  (set/difference
   (set new-states)
   (set/union (set frontier) (set visited))))

(def max-calls 10000)

(defn bfs
  "Breadth-first search of a path from start to end in the matrix."
  ;; https://nicmcphee.github.io/intro-to-evolutionary-computation/pages/03-clean-up-search-implementation.html
  [start end mat]
  (loop [frontier [start]
         visited #{}
         num-calls 0]
    (let [next-node (first frontier)]
      (cond
        (= next-node end) visited
        (= num-calls max-calls) :max-calls-reached
        :else
        (recur (concat
                (remove-previous-states (children mat next-node visited) frontier visited)
                (rest frontier))
               (conj visited next-node)
               (inc num-calls))))))

(defn start-end
  [f]
  (case f
    :test [testf [0 0] [2 5]]
    :input [inputf [20 0] [36 10]]))

;;------------------------------
(defn part1
  [target]
  (let [[f start end] (start-end target)]
    (->> f
         read-data
         (bfs start end)
         #_count)))

;; (assert (= 31 (part1 :test)))

(defn part2
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part2 testf)))
;; The End
