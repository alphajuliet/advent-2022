(ns aoc22.day11
  (:require [instaparse.core :as insta]
            [clojure.edn :as edn]
            [clojure.core.match :as m]))

(def testf "data/day11-test.txt")
(def inputf "data/day11-input.txt")

(def monkeys
  "Parser for the input data"
  (insta/parser
   "<rules> := (monkey <newline>)+ monkey
    monkey := <header> start-items operation test-div

    header := <'Monkey '> number <':'> <newline>
    start-items := <space> <'Starting items: '> number (<', '> number)* <newline>
    operation := <space> <'Operation: new = '> expression <newline>
    <expression> := <'old '> #'[+*]' <space> (number | 'old')

    test-div := <space> <'Test: divisible by '> number <newline> if-true if-false
    if-true := <space> <'If true: throw to monkey '> number <newline>
    if-false := <space> <'If false: throw to monkey '> number <newline>
    number := #'\\d+'
    <space> := #'\\s+'
    <newline> := '\n'"))

(defn read-data
  [f]
  (->> f
       slurp
       monkeys
       (insta/transform {:number edn/read-string})))

(defn init-state
  "Set up the initial state"
  [rules]
  {:rules (vec rules)
   :inspected (vec (repeat (count rules) 0))
   :worries (mapv (comp rest second) rules)
   :quotient 3
   :modulo 10000})

(defn destination
  "Return the destination monkey and the new worry level"
  [state monkey package]
  (let [op (rest (get-in state [:rules monkey 2]))
        divisor (get-in state [:rules monkey 3 1])
        if-true (get-in state [:rules monkey 3 2 1])
        if-false (get-in state [:rules monkey 3 3 1])
        quotient (:quotient state)
        modulo (:modulo state)
        worry' (-> (m/match [op]
                            [(["*" "old"] :seq)] (* package package)
                            [(["+" arg] :seq)] (+ package arg)
                            [(["*" arg] :seq)] (* package arg))
                   (quot quotient)
                   (mod modulo))]
    {:dest (if (zero? (mod worry' divisor))
             if-true
             if-false)
     :worry' worry'}))

(defn do-monkey
  "One monkey inspects all his packages"
  [st monkey]
  (let [packages (get-in st [:worries monkey])]
    (reduce (fn [s p]
              (let [{:keys [dest worry']} (destination s monkey p)]
                (-> s
                    (update-in [:worries monkey] #(remove #{p} %))
                    (update-in [:worries dest] #(conj % worry'))
                    (update-in [:inspected monkey] inc))))
            st
            packages)))

(defn do-round
  "All monkeys inspect their packages"
  [s0]
  (let [monkeys (count (:rules s0))]
    (reduce do-monkey
            s0
            (range monkeys))))

(defn run-all-rounds
  "Run n rounds with an initial state"
  [n st]
  (reduce (fn [st _] (do-round st))
          st
          (range n)))

(defn update-params
  [st]
  (let [q (map #(get-in st [:rules % 3 1]) (range (count (:rules st))))]
    (-> st
        (assoc :quotient 1)
        (assoc :modulo (apply * q)))))

;;------------------------------
(defn part1
  [f]
  (->> f
       read-data
       init-state
       (run-all-rounds 20)
       :inspected
       (sort >)
       (take 2)
       (apply *)))

(assert (= 10605 (part1 testf)))

(defn part2
  [f]
  (->> f
    read-data
    init-state
    update-params
    (run-all-rounds 10000)
    :inspected
    (sort >)
    (take 2)
    (apply *)))

;; (assert (= 0 (part2 testf)))
;; The End
