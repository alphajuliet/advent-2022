(ns aoc22.day07
  (:require [aoc22.util :as util]
            [clojure.edn :as edn]
            [instaparse.core :as insta]
            [meander.epsilon :as e]
            [meander.strategy.epsilon :as r]
            [com.rpl.specter :as sp]))

(def testf "data/day07-test.txt")
(def inputf "data/day07-input.txt")

(defn read-data
  [f]
  (->> f
       slurp))

(def log-parser
  "Parse the log file"
  (insta/parser
   "<log> := query+
    query := request response+

    <request> := chdir ls
    chdir := cd+
    <cd> := <'$ cd '> #'[a-z./]+' <newline>
    <ls> := <'$ ls'> <newline>

    <response> := (dir | file) <newline>
    dir := <'dir '> name
    file := size <' '> name

    name := #'[a-z./]+'
    size := #'\\d+'
    newline := '\\n'"))

(defn update-wd
  "Update the working directory"
  ;; update-wd :: Coll -> String -> Coll
  [wd dir-name]
  (case dir-name
    ".." (drop-last wd)
    "/" '()
    (conj wd dir-name)))

(defn compress-wd
  [queries]
  )

(defn read-log
  "Parse the log file and convert strings to numbers"
  [log]
  (->> log
       log-parser
       (insta/transform
        {:size #(vector :size (edn/read-string %))})))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       read-log))

;; (assert (= 0 (part1 testf)))

(defn part2
  [f]
  (->> f
       read-data))

;; (assert (= 0 (part2 testf)))
;; The End
