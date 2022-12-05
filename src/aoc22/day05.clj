(ns aoc22.day05
  (:require [aoc22.util :as util]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def testf "data/day05-test.txt")
(def inputf "data/day05-input.txt")

(defn read-data
  "Read the data in as two lists for the initial placement and the instructions"
  [f]
  (as-> f <>
    (slurp <>)
    (str/split <> #"\n\n")
    (map str/split-lines <>)))

(def placement
  "Create a parser for the initial crate placement"
  (insta/parser
   "<P> := (EMPTY | CRATE) (<SPACE> (EMPTY | CRATE))+
    SPACE := ' '
    <EMPTY> := '   '
    <CRATE> := <'['> ID <']'>
    <ID> := #\"[A-Z]\" "))

(def instruction
  (insta/parser
   "<I> := <'move '> NUMBER <' from '> NUMBER <' to '> NUMBER
    <NUMBER> := #\"\\d+\" "))

(defn gap->nil
  [s]
  (if (= s "   ") nil s))

(defn parse-placement
  "Parse a placement line"
  [s]
  (->> s
       placement
       (mapv gap->nil)))

(defn initial-stacks
  "Get the initial placement of crates as a collection of strings"
  [crates]
  (->> crates
       (mapv parse-placement)
       util/T
       (mapv #(apply concat %))
       (mapv str/join)))

(defn parse-instruction
  "Parse an instruction"
  [s]
  (->> s
       instruction
       (map edn/read-string)
       (zipmap [:num :src :dest])))

(defn move
  "Move a single crate from src to dest"
  [src dest]
  (let [x (subs src 0 1)]
    [(subs src 1)
     (str x dest)]))

(defn execute
  "Execute an instruction on the state"
  [state {:keys [num src dest]}]
  (reduce (fn [st _]
            (let [[src' dest'] (move (nth st (dec src)) (nth st (dec dest)))]
              (-> st
                  (assoc (dec src) src')
                  (assoc (dec dest) dest'))))
          state
          (range num)))

;;------------------------------
(defn part1
  [f]
  (let [[crates instructions] (read-data f)
        s0 (initial-stacks (butlast crates))
        instr (map parse-instruction instructions)]
    (->> instr
         (reduce #(execute %1 %2) s0)
         (map first)
         (apply str))))

(assert (= "CMZ" (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part1 testf)))
;; The End
