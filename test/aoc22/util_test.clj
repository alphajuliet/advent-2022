(ns aoc22.util-test
  (:require [clojure.test :refer [deftest is]]
            [aoc22.util :as u]))

(deftest sanity
  (is (= 5 (+ 2 3))))

(deftest numeric
  (is (= 3 (u/clamp 1 3 5))))

(deftest logic
  (let [x (range 5)]
    (is (= 3 (u/count-if even? x)))
    (is (= [0 2 4] (u/filter-if [true false true false true] x)))))

(deftest binary
  (is (= 4 (u/number-of-bits 15)))
  (is (zero? (u/number-of-bits -5)))
  (is (= [0 1 1 0] (u/ones-complement [1 0 0 1])))
  (is (= 12 (u/binv->dec [1 1 0 0]))))

(deftest strings
  (is (= 123 (u/str->num "123")))
  (is (= -456 (u/str->num "-456")))
  (is (= " 1" (u/left-pad "1" 2)))
  (is (= "001" (u/left-pad "1" 3 "0")))
  (is (= "bca" (u/rotate-string 1 "abc")))
  (is (= "cab" (u/rotate-string -1 "abc"))))

(deftest collections
  (is (= [2 3 1] (u/rotate 1 [1 2 3])))
  (is (= [1 4 3 2] (u/swap-elements [1 2 3 4] 1 3))))

(deftest maps
  (is (= {:a 2 :b 3} (u/map-kv inc {:a 1 :b 2}))))

;; The End
