(ns aoc22.day16
  (:require [aoc22.util :as util]
            [instaparse.core :as insta]
            [clojure.edn :as edn]
            [ubergraph.alg :as alg]
            [ubergraph.core :as uber]))

(def testf "data/day16-test.txt")
(def inputf "data/day16-input.txt")

(def parse-valve
  (insta/parser
   "<line> := <'Valve '> valve <' has flow rate='> rate <#'[a-z ;]+'> link
    valve := #'[A-Z]{2}'
    rate := #'\\d+'
    link := valve (<', '> valve)*
    newline := '\n'
    "))

(defn read-valve
  [line]
  (->> line
       parse-valve
       (insta/transform
        {:valve keyword
         :rate edn/read-string
         :link vector})))

(defn read-data
  [f]
  (->> f
      util/import-data
       (map read-valve)))

(defn- add-valve
  [g [src weight dests]]
  (-> g
      (uber/add-nodes-with-attrs [src {:pressure weight}])
      (uber/add-edges* (mapv #(vector src %) dests))))

(defn create-graph
  [valves]
  (reduce add-valve
          (uber/graph)
          valves))

(defn non-zero-nodes
  "Filter out the nodes with zero pressure contribution apart from :AA"
  [g]
  (as-> g <>
    (uber/nodes <>)
    (map #(uber/node-with-attrs g %) <>)
    (remove #(zero? (:pressure (second %))) <>)
    (conj <> (uber/node-with-attrs g :AA))))

(defn distances
  "Find distances between all pairs of nodes as a list of maps"
  [g]
  (let [nn (map first (non-zero-nodes g))]
    (for [start nn
          end nn
          :when (not= start end)]
      {:s start
       :e end
       :c (:cost (alg/shortest-path g start end))})))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       create-graph))

;; (assert (= 0 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part2 testf)))
;; The End
