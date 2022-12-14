(ns aoc22.day10
  (:require [aoc22.util :as util]
            [clojure.core.match :as m]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def test1f "data/day10-test1.txt")
(def test2f "data/day10-test2.txt")
(def inputf "data/day10-input.txt")

(defn decode
  "Decode each line"
  [line]
  (m/match [line]
           [([instr] :seq)] [(keyword instr)]
           [([instr arg] :seq)] [(keyword instr) (edn/read-string arg)]))

(defn read-data
  [f]
  (->> f
       util/import-data
       (map #(str/split % #"\s"))
       (map decode)))

(defn execute
  [st instr]
  (case (first instr)
    :noop (conj st (last st))
    :addx (-> st
              (conj (last st))
              (conj (+ (last st) (second instr))))))

(defn get-samples
  [trace]
  (let [t (range 19 220 40)]
    (map #(* (inc %) (nth trace %)) t)))

(defn draw-pixel
  [cycle sprite-centre]
  (if (<= (dec sprite-centre) (mod cycle 40) (inc sprite-centre))
    1
    ;;else
    0))

(defn- num->pixel
  [n]
  (if (zero? n) \. \#))

(defn draw-image
  [trace]
  (->> trace
       (map-indexed draw-pixel)
       (map num->pixel)
       (partition 40)
       (map #(str/join "" %))))

;;------------------------------
(defn part1
  [f]
  (let [init-state [1]]
    (->> f
        read-data
        (reduce execute init-state)
        get-samples
        (apply +))))

(assert (= 13140 (part1 test2f)))

(defn part2
  [f]
  (let [init-state [1]]
    (->> f
         read-data
         (reduce execute init-state)
         draw-image)))

;; The End
