(ns aoc22.day12
  (:require [aoc22.util :as util]
            [clojure.core.matrix :as m]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [clojure.math :as math]
            [clojure.set :as set]))

(def testf "data/day12-test.txt")
(def inputf "data/day12-input.txt")

(defn read-data
  [f]
  (let []
    (->> f
        util/import-data
        (map #(str/replace % "S" "`"))
        (map #(str/replace % "E" "{"))
        (map #(str/split % #""))
        (util/mapmap #(inc (- (int (first %)) (int \a)))))))

(defn safe-mget
  "If [i j] is out of bounds then return the default value"
  [m [i j] default]
  (let [[r c] (m/shape m)]
    (if (or (neg? i) (neg? j) (>= i r) (>= j c))
      default
      (m/mget m i j))))

(defn mfind
  "Find the first occurrence of val in mat"
  [mat val]
  (let [[r c] (m/shape mat)
        pos (-> mat
                m/to-vector
                (.indexOf val))]
    (vector (quot pos c)
            (mod pos c))))

(defn neighbours
  [[r c]]
  (let [deltas [[1 0] [0 1] [-1 0] [0 -1]]]
    (mapv #(mapv + [r c] %) deltas)))

(defn not-in?
  [a b]
  (vec (set/difference (set b) (set a))))

(defn allowed-steps
  "Allowed adjacent squares not in the path. Assume monotonically increasing for now"
  [mat [r c] path]
  (let [x (m/mget mat r c)
        nn (neighbours [r c])
        vv (mapv #(vector % (safe-mget mat % 100)) nn)]
    (->> vv
         (filter #(<= 0 (- (second %) x) 1))
         (mapv first)
         (not-in? path))))

(defn goal?
  [mat [r c]]
  (= (m/mget mat r c) 4))

(defn leaf?
  [mat loc path]
  (empty? (allowed-steps mat loc path)))

(defn mat-val
  [mat [r c]]
  (m/mget mat r c))

(defn children [mat loc path]
  (allowed-steps mat loc path))

(defn queue [& vals]
  (apply conj clojure.lang.PersistentQueue/EMPTY vals))

(defn search
  "Depth-first search"
  [mat]
  (loop [q (queue {:loc (mfind mat 0) :path []})]
    (when-let [{:keys [loc path]} (peek q)]
      (cond
        (goal? mat loc) path
        (leaf? mat loc path) (recur (pop q))
        :else
        (let [new-path (concat [loc] path) ;depth-first search, otherwise `conj path loc`
              wrap (fn [loc] {:loc loc :path new-path})]
          (recur (->> (children mat loc new-path)
                      (map wrap)
                      (apply conj (pop q)))))))))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       search
       count))

;; (assert (= 31 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part2 testf)))
;; The End
