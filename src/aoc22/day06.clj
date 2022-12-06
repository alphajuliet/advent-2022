(ns aoc22.day06
  (:require [aoc22.util :as util]))

(def test1f "data/day06-test1.txt")
(def test2f "data/day06-test2.txt")
(def test3f "data/day06-test3.txt")
(def test4f "data/day06-test4.txt")
(def test5f "data/day06-test5.txt")
(def inputf "data/day06-input.txt")

(defn read-data
  [f]
  (->> f
       util/import-data
       first))

(defn start-marker
  "Find the first marker position of n unique consecutive characters"
  [n stream]
  (loop [i n
         p (partition n 1 stream)]
    (if (= n (-> p first set count))
      i
      (recur (inc i) (rest p)))))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       (start-marker 4)))

(assert (= 7 (part1 test1f)))
(assert (= 5 (part1 test2f)))
(assert (= 6 (part1 test3f)))
(assert (= 10 (part1 test4f)))
(assert (= 11 (part1 test5f)))

(defn part2
  [f]
  (->> f
       read-data
       (start-marker 14)))

(assert (= 19 (part2 test1f)))
(assert (= 23 (part2 test2f)))
(assert (= 23 (part2 test3f)))
(assert (= 29 (part2 test4f)))
(assert (= 26 (part2 test5f)))
;; The End
