(ns aoc22.day07
  (:require [clojure.edn :as edn]
            [instaparse.core :as insta]))

(def testf "data/day07-test.txt")
(def inputf "data/day07-input.txt")

(defn read-data
  [f]
  (->> f
       slurp))

(def log-parser
  (insta/parser
   "<log> := (line <newline>)+
    <line> := cd | <ls> | dir | file
    cd := <'$ cd '> name
    ls := <'$ ls'>
    dir := <'dir '> name
    file := size <' '> name

    name := #'[a-z./]+'
    size := #'\\d+'
    newline := '\\n'"))

(defn transform-log
  [log]
  (->> log
       (insta/transform
        {:size #(edn/read-string %)
         :name identity})))

(defn chdir
  [cwd dir]
  (case dir
    ".." (vec (butlast cwd))
    "/" (vector dir)
    ;; else
    (conj cwd dir)))

(defn add-dir
  [cmd cwd fs]
  (-> fs
      (assoc (conj cwd (second cmd)) {:dir 0})))

(defn add-file
  [cmd cwd fs]
  (as-> fs <>
    (assoc <> (conj cwd (last cmd)) {:file (second cmd)})
    (loop [fs_ <>
           path cwd]
      (if (empty? path)
        fs_
        ;; else
        (recur (update-in fs_ [path :dir] #(+ % (second cmd)))
               (vec (butlast path)))))))

(defn create-fs
  "Create a file system as a map of paths"
  ;; Props to https://github.com/hunkyjimpjorps/AdventOfCode/blob/main/2022/day-07/day-07.rkt
  [log]
  (loop [[cmd & cmds] log
         cwd ["/"]
         fs {["/"] {:dir 0}}]
    (case (first cmd)
      nil fs
      :cd (recur cmds (chdir cwd (second cmd)) fs)
      :dir (recur cmds cwd (add-dir cmd cwd fs))
      :file (recur cmds cwd (add-file cmd cwd fs))
      ;; else
      (assert false)
      )))

(defn read-log
  "Parse the log file and convert strings to numbers"
  [log]
  (->> log
       log-parser
       transform-log))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       read-log
       create-fs
       vals
       (filter :dir)
       (map (comp first vals))
       (filter #(<= % 100000))
       (apply +)))

(assert (= 95437 (part1 testf)))

(defn part2
  [f]
  (let [fs (->> f
             read-data
             read-log
             create-fs)
        free-up (- 30000000 (- 70000000 (get-in fs [["/"] :dir])))]
    (->> fs
         vals
         (filter :dir)
         (map (comp first vals))
         (filter #(>= % free-up))
         (apply min))))

(assert (= 24933642 (part2 testf)))
;; The End
