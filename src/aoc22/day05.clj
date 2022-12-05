(ns aoc22.day05
  (:require [aoc22.util :as util]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [instaparse.core :as insta]))

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
  "Create a parser for each line of the initial stacks "
  (insta/parser
   "<P> := (EMPTY | CRATE) (<SPACE> (EMPTY | CRATE))+
    SPACE := ' '
    <EMPTY> := '   '
    <CRATE> := <'['> ID <']'>
    <ID> := #\"[A-Z]\" "))

(def instruction
  "A parser for an instruction line"
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
       (map edn/read-string)))

(defn process-input
  [f]
  (let [[crates instructions] (read-data f)
        s0 (initial-stacks (butlast crates))
        instr (map parse-instruction instructions)]
    [s0 instr]))

(defn move
  "Move a single crate from src to dest"
  [src dest]
  (let [x (subs src 0 1)]
    [(subs src 1)
     (str x dest)]))

(defn execute-9000
  "Move num crates individually from src to dest"
  [state [num src dest]]
  (let [i (dec src)
        j (dec dest)]
    (reduce (fn [st _]
              (let [[src' dest'] (move (nth st i) (nth st j))]
               (-> st
                   (assoc i src')
                   (assoc j dest'))))
           state
           (range num))))

(defn execute-9001
  "Move num crates in one stack from src to dest"
  [state [num src dest]]
  (let [i (dec src)
        j (dec dest)
        x (subs (nth state i) 0 num)]
    (-> state
        (update i #(subs % num))
        (update j #(str x %)))))

(defn arrange-crates
  "Arrange the crates using the given crane operation"
  [initial-state instr op]
  (->> instr
       (reduce #(op %1 %2) initial-state)
       (map first)
       (apply str)))

;;------------------------------
(defn part1
  [f]
  (let [[initial-state instr] (process-input f)]
    (arrange-crates initial-state instr execute-9000)))

(assert (= "CMZ" (part1 testf)))

(defn part2
  [f]
  (let [[initial-state instr] (process-input f)]
    (arrange-crates initial-state instr execute-9001)))

(assert (= "MCD" (part2 testf)))
;; The End
