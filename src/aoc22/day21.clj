(ns aoc22.day21
  (:require [clojure.edn :as edn]
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
  "Read in the data and return as vectors"
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


(defn- make-adj-list
  "Make an adjacency list from the tree"
  [tree]
  (into {} (map (fn [[_ k v]]
                  [k (if (vector? v)
                       (vec (rest v))
                       [])])
                tree)))


(defn- dfs
  "Find the path from start to end, given a node adjacency list"
  [adj-list start end]
  (let [visited (atom #{})
        parent (atom {start nil})]
    (loop [stack [start]]
      (when-let [vertex (first stack)]
        (cond
          (= vertex end) (loop [path [] node vertex]
                           (if node
                             (recur (cons node path) (@parent node))
                             path))
          (not (@visited vertex)) (do
                                    (swap! visited conj vertex)
                                    (doseq [child (adj-list vertex)]
                                      (when-not (@visited child)
                                        (swap! parent assoc child vertex)))
                                    (recur (concat (adj-list vertex) (rest stack))))
          :else (recur (rest stack)))))))

(defn find-path
  "Find the path through the three from start to end"
  [tree start end]
  (-> tree
      make-adj-list 
      (dfs start end)
      rest))

(defn other
  "Return the other element in a vector of length two"
  [v a]
  (if (= a (first v))
    (last v)
    (first v)))

(defn get-branches
  [tree node]
  (-> tree
      make-adj-list
      node))
         
(defn- get-target 
  "Find the target value for the other branch of the tree"
  [tree node]
  (let [branches (get-branches tree 'root)]
   (eval-tree tree (other branches node))))

(defn update-tree 
  "Update the node value in the tree"
  [tree node value]
  (map #(if (= (second %) node) [:node node value] %) 
       tree))

(defn find-leaf-value
  "Use the bisection method to solve for the leaf value"
  [tree leaf root desired-root-value]
  (let [epsilon 0.001
        max-iterations 1000
        [low high] [0.0 (* 3 desired-root-value)]]
    (loop [low low
           high high
           iteration 0]
      (let [mid (/ (+ low high) 2)
            tree-updated (update-tree tree leaf mid)
            root-value (eval-tree tree-updated root)]
        (cond
          (or (<= (Math/abs (- root-value desired-root-value)) epsilon)
              (>= iteration max-iterations)) (int mid)
          (< root-value desired-root-value) (recur mid high (inc iteration))
          :else (recur low mid (inc iteration)))))))

(defn part1
  [f]
  (-> f
      read-data
      transform-tree
      (eval-tree 'root)))

(defn part2
  [f]
  (let [tree (-> f read-data transform-tree)
        path (find-path tree 'root 'humn)
        target-value (get-target tree (first path))] 
    (println "Target value:" target-value)
    (find-leaf-value tree 'humn (first path) (bigint target-value))))

(assert (= 152 (part1 testf)))
(assert (= 301 (part2 testf)))

;; The End