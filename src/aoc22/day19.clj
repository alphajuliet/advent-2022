(ns aoc22.day19
  (:require [aoc22.util :as util]
            [instaparse.core :as insta]))

(def testf "data/day19-test.txt")
(def inputf "data/day19-input.txt")

(def blueprint-parser
  (insta/parser
   "<blueprint> := (number|<other>)+
    <number> := #'\\d+'
    other := #'\\D'
    newline := '\\n'"))

(defn create-blueprint
  "Create a blue print matrix: each row is the robot, each column is the good required."
  [v]
  [[(nth v 1) 0 0 0]
   [(nth v 2) 0 0 0]
   [(nth v 3) (nth v 4) 0 0]
   [(nth v 5) 0 (nth v 6) 0]])

(defn read-data
  "Read in the blueprints and return the data as vectors"
  [f]
  (->> f
       util/import-data
       (map blueprint-parser)
       (util/mapmap #(Integer/parseInt %))
       (map create-blueprint)))

(defn produce-goods
  ;; Produce new goods based on number of robots
  [{:keys [robots goods] :as state}]
  (update state :goods #(map + % robots)))

(defn create-robot?
  "Can we create a specific robot from the goods available, according to the given blueprint?"
  [{:keys [goods]} blueprint robot]
  (every? true? (map >= goods (nth blueprint robot))))

(defn goods-produced
  "How many goods are produced after a given number of ticks"
  [state blueprint robot nticks]
  (if (zero? nticks)
    state
    ;; else
    (let [state' (produce-goods state)
          max-geodes (atom 0)]
      (doseq [r (range 4)]
        (when (create-robot? state' blueprint r)
          (let [state'' (-> state'
                            (update-in [robots r] inc)
                            (update :goods #(map - % (nth blueprint r))))
                geodes-produced (goods-produced state'' blueprint robot (dec nticks))]
            (swap! max-geodes max geodes-produced))))
      @max-geodes)))


(defn step
  "Step forward one tick"
  [state]
  (produce-goods state))

(defn run-steps
  "Run n steps from the initiate state"
  [n state]
  (reduce (fn [st _] (step st))
          state
          (range n)))

(defn init-state
  "Initial state"
  []
  {:robots [1 0 0 0]
   :goods [0 0 0 0]})

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
