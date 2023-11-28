(ns aoc22.day22
  (:require [clojure.edn :as edn]
            [instaparse.core :as insta]
            [aoc22.util :as util]))

(def testf "data/day22-test.txt")
(def inputf "data/day22-input.txt")

(defn find-columns
  "Find the left and right edges of the contiguous contents of each column"
;; find-columns : Board -> [(Int, Int)]
  [board]
  (map (fn [s]
         (let [indices (keep-indexed (fn [i x] (if (not= x \space) i nil)) s)]
           [(first indices) (last indices)]))
       board))

(defn- pad-board
  "Pad the board with spaces to make a regular-sized board"
  ;; pad-board : Board -> Board
  [board]
  (let [max-cols (apply max (map count board))]
    (map #(util/right-pad % max-cols) board)))

(defn find-walls
  "Find the locations of all the walls marked with a hash"
  ;; find-walls : Board -> [(Int, Int)]
  [board]
  (mapcat (fn [row s]
            (map (fn [col] [row col])
                 (keep-indexed (fn [i x] (if (= x \#) i nil)) s)))
          (range)
          board))

(defn discover-board
  "Find the left and right edges of the contiguous contents of each row"
  ;; discover-board : Board -> Board
  [rows]
  (let [rowexts (find-columns rows)
        colexts (find-columns (util/T (pad-board rows)))
        walls (find-walls rows)]
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
    #_{:clj-kondo/ignore [:unresolved-var]}
    (insta/transform
     {:fwd edn/read-string}
     (parser p))))

(defn wrap-edges
  "Wrap around the edges of the board"
  ;; wrap : Posn -> Board -> Posn
  [{:keys [rc] :as posn} {:keys [rowexts colexts]}]
  (let [[row col] rc
        [rowmin rowmax] rowexts
        [colmin colmax] colexts]
    (cond
      (< col colmin) [row (inc colmax)]
      (> col colmax) [row (dec colmin)]
      (< row rowmin) [(inc rowmax) col]
      (> row rowmax) [(dec rowmin) col]
      :else posn)))

(defn step-forward
  "Move forward one step in the current direction. Stop if we hit a wall. Wrap around edges."
  ;; move-fwd : Posn -> Board -> Posn
  [{:keys [rc dir] :as posn} {:keys [walls] :as board}]
  (let [[dr dc] (case dir
                  0 [1 0]
                  1 [0 1]
                  2 [-1 0]
                  3 [0 -1])
        [newrow newcol] (wrap-edges (map + rc [dr dc]) board)]
    (if (util/coll-contains? walls [newrow newcol])
      posn
      (update posn :rc #(map + % [dr dc])))))

(defn- move-forward
  "Move forward, if possible, by the number of steps given"
  ;; move-forward : Posn -> Int -> Board -> Posn
  [posn steps board]
  (reduce (fn [p _]
            (let [p' (step-forward p board)]
              (if (= p p')
                (reduced p)
                p')))
          posn
          (range steps)))

(defn move
  "Move in the given direction"
  ;; move : Posn -> Instr -> Board -> Posn
  [{:keys [dir] :as posn} instr board]
  (cond (number? instr) (move-forward posn instr board)
        :else (let [next-dir (case instr
                               "R" (mod (inc dir) 4)
                               "L" (mod (dec dir) 4))]
                (assoc posn :dir next-dir))))

(defn navigate-board
  "Navigate the board and return the final position"
  ;; navigate-board : Board -> Path -> Posn
  [{:keys [rowexts] :as board} path]
  (let [init-posn {:rc [0 (get-in (vec rowexts) [0 0])] :dir 0}]
    (reduce (fn [posn instr]
              (move posn instr board))
            init-posn
            path)))

(defn part1
  "Solve part 1"
  [f]
  (let [data (util/import-data f)
        board (discover-board (drop-last 2 data))
        path (parse-path (last data))]
    ;; (set! clojure.core/*print-length* 3)
    (navigate-board board path)))

;; The End