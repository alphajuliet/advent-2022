(ns aoc22.day17
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]))

(def testf "data/day17-test.txt")
(def inputf "data/day17-input.txt")

(def condj 
"Doesn't conj a nil value"
  ((remove nil?) conj))

(defn read-data
  [f]
  (-> f
      slurp
      (str/split #"")
      butlast))

(def shapes
  "Capture each rock as a list of row vectors"
  {:beam  [[1 1 1 1]]
   :plus  [[0 1 0] [1 1 1] [0 1 0]]
   :L     [[1 1 1] [0 0 1] [0 0 1]]
   :stick [[1] [1] [1] [1]]
   :block [[1 1] [1 1]]})

(def shape-order [:beam :plus :L :stick :block])

(defn enumerate-shape 
  "Convert a shape into a set of filled RC coordinates at a given origin"
  ;; enumerate-shape : [[Integer]] -> [Integer Integer]
  [shape r0 c0]
  (let [[rmax cmax] (m/shape shape)]
    (for [r (range rmax)
          c (range cmax)
          :when (= 1 (m/mget shape r c))]
      [(+ r r0) (+ c c0)])))

(defn enumerate-tile
  "Wrapper for enumerate-shape"
  [{:keys [shape r c]}]
  (enumerate-shape (shape shapes) r c))

(defn enumerate-active-tile
  "Return the coords of the active tile"
  [{:keys [active]}]
  (let [{:keys [shape r c]} active]
    (enumerate-shape (shape shapes) r c)))

(defn draw-shape
  "Update the field with the latest shape position"
  ;; draw-shape : State -> Number -> State
  [state val]
  (let [{:keys [shape r c]} (:active state)
        coords (enumerate-shape (shape shapes) r c)
        f' (reduce (fn [f [r c]]
                     (m/mset f r c val))
                   (:field state)
                   coords)]
    (assoc state :field f')))

(defn stack-height
  "Calculate the height of the stack of shapes in the playing field"
  [{:keys [:field]}]
  (->> field
       m/rows
       (map m/zero-matrix?)
       (take-while false?)
       count))

(defn extend-field
  "Grow the field as needed"
  [state]
  (let [[rows cols] (m/shape (:field state))
        h (stack-height state)
        margin 7]
    (if (< rows (+ h margin))
      (update state :field #(m/reshape % [(+ rows margin) cols]))
      ;;else
      state)))

(defn add-new-shape
  "Add a new shape to the top, with x=2 and y=max+3. Increase the size of the matrix as required."
  [state]
  (let [shape-name (get shape-order (:next-shape state))
        h (stack-height state)
        shape-rc {:shape shape-name :c 2 :r (+ h 3)}]
    (-> state
        extend-field
        (update :played #(condj % (:active state)))
        (assoc :active shape-rc)
        (draw-shape 1)
        (update :next-shape #(mod (inc %) 5)))))

(defn collision?
  "Check if the active piece overlaps an existing piece, or if it's outside the bounds of the field."
  [state]
  (let [coords (enumerate-active-tile state)
        filled-coords (apply concat (map enumerate-tile (take-last 30 (:played state))))]
    (or (some #(contains? (set coords) %) filled-coords)
        (not (every? #(<= 0 (second %) 6) coords)))))

(defn- update-active-shape
  "Update the location of the active piece"
  [state direction]
  (let [st' (case direction
              "<" (update-in state [:active :c] #(max 0 (dec %)))
              ">" (update-in state [:active :c] #(min 6 (inc %)))
              "↓" (update-in state [:active :r] #(max 0 (dec %)))
              :else state)]
    (if (collision? st')
      state
      ;;else
      st')))

(defn move-shape
  "Move the active shape one unit in a direction, if it can"
  [state direction]
  (-> state
      (draw-shape 0)
      (update-active-shape direction)
      (draw-shape 1)))

(defn purge-played-tiles
  "Keep the played tile list at a reasonable size"
  [state]
  (if (> (count (:played state)) 30)
    (update state :played rest)
    state))

(defn game-step
  "Increment the game state by one step, i.e. move left/right and down. 
   If nothing has happened then add the next shape."
  [state lr]
  (let [s1 (move-shape state lr)
        s2 (move-shape s1 "↓")]
    (if (= s1 s2)
      (-> s2
          (update :rocks inc)
          purge-played-tiles
          add-new-shape)
      ;;else
      (update s2 :moves inc))))

(defn initial-field
  "Return an empty starting field"
  []
  (m/reshape [] [7 7]))

(defn initial-state
  []
  (-> {:field (initial-field)
       :active nil
       :played []
       :next-shape 0
       :moves 0
       :rocks 0}
      add-new-shape))

(defn play-game
  "Step through the game until the given number of rocks have fallen"
  [limit moves]
  (-> (reduce
       (fn [st mv]
         (if (>= (:rocks st) limit)
           (reduced st)
            ;; else
           (game-step st mv)))
       (initial-state)
       (cycle moves))
      stack-height))

;;------------------------------
(defn part1
  [f]
  (let [moves (read-data f)]
    (play-game 2022 moves)))

(defn part2
  [f]
  (let [moves (read-data f)]
    (play-game 10000 moves)))

(comment
  (assert (= 3068 (part1 testf)))
  (assert (= 1514285714288 (part2 testf))))

;; The End