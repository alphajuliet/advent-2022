(ns aoc22.day22
  (:require [clojure.edn :as edn]
            [instaparse.core :as insta]
            [aoc22.util :as util]
            [clojure.string :as str]))

(def testf "data/day22-test.txt")
(def inputf "data/day22-input.txt")

(defn find-columns
  [board]
  (map (fn [s]
         (let [indices (keep-indexed (fn [i x] (if (not= x \space) i nil)) s)]
           [(first indices) (last indices)]))
       board))

(defn- pad-board
  "Pad the board with spaces to make a regular-sized board"
  [board]
  (let [max-cols (apply max (map count board))]
    (map #(util/right-pad % max-cols) board)))

(defn find-walls
  "Find the locations of all the walls marked with a hash"
  [board]
  (mapcat (fn [row s]
            (map (fn [col] [row col])
                 (keep-indexed (fn [i x] (if (= x \#) i nil)) s)))
          (range)
          board))

(defn discover-board
  "Find the left and right edges of the contiguous contents of each row"
  [board]
  (let [rowexts (find-columns board)
        colexts (find-columns (util/T (pad-board board)))
        walls (find-walls board)]
    {:rowexts rowexts
     :colexts colexts
     :walls walls}))

(defn parse-path
  "Parse the path string into a list of instructions"
  [p]
  (let [parser (insta/parser
                "<path> := (fwd turn) + fwd
                fwd := #'\\d+'
                <turn> := 'R' | 'L' ")]
    (insta/transform
     {:fwd edn/read-string}
     (parser p))))

(defn part1
  [f]
  (let [data (util/import-data f)
        [board path] ((juxt (partial drop-last 2) last) data)]
    (discover-board board)))

;; The End