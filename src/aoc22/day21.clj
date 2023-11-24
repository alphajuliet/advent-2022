(ns aoc22.day21
  (:require [aoc22.util :as util]
            [clojure.edn :as edn]
            [instaparse.core :as insta]))

(def testf "data/day21-test.txt")
(def inputf "data/day21-input.txt")

(def monkey-parser
  (insta/parser
   "<monkey> := (node <newline>)+
    node := name <':'> <space> (expr|number)
    <expr> := add|sub|mul|div
    name := #'[a-z]{4}'
    add := name <space> <'+'> <space> name
    sub := name <space> <'-'> <space> name
    mul := name <space> <'*'> <space> name
    div := name <space> <'/'> <space> name
    number := #'\\d+'
    space := #'\\s+'
    newline := '\\n'"))

(defn read-data
  "Read in the blueprints and return the data as vectors"
  [f]
  (->> f
       slurp
       monkey-parser))

(defn transform-tree
  [tree]
  (->> tree
       #_{:clj-kondo/ignore [:unresolved-var]}
       (insta/transform
        {:number edn/read-string
        :name symbol})))

(defn eval-tree
  "Recursively traverse the tree and evaluate each node"
  [nodes node-name]
  (let [node (first (filter #(= (second %) node-name) nodes))
        operation (last node)
        operations {:add +
                    :sub -
                    :mul *
                    :div /}]
    (if (number? operation)
      operation
      ((get operations (first operation))
       (eval-tree nodes (second operation))
       (eval-tree nodes (last operation))))))
         
(defn part1
  [f]
  (-> f
      read-data
      transform-tree
      (eval-tree 'root)))

(assert (= 152 (part1 testf)))

;; The End