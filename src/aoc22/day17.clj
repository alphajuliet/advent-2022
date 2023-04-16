(ns aoc22.day17
  (:require [aoc22.util :as util]
            [clojure.string :as str]))

(def testf "data/day17-test.txt")
(def inputf "data/day17-input.txt")

(defn read-data
  [f]
  (-> f
      slurp
      (str/split #"")
      butlast))

(def rocks
  "List each rock as the initial value"
  [[0x1e]
   [0x08 0x1c 0x08]
   [0x1c 0x04 0x04]
   [0x10 0x10 0x10 0x10]])

(defn push-left
  "Rotate left iff the MSB is 0"
  [x]
  (if (bit-test x 6)
    x
    (bit-shift-left x 1)))

(defn push-right
  "Rotate right iff the LSB is 0"
  [x]
  (if (bit-test x 0)
    x
    (bit-shift-right x 1)))

(defn move-piece
  "Move a piece left or right, if it can"
  [dir piece]
  (case dir
    "<" (if (some #(bit-test % 6) piece)
          piece
          (mapv push-left piece))
    ">" (if (some #(bit-test % 0) piece)
          piece
          (mapv push-right piece))
    :else piece))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part2 testf)))
;; The End
