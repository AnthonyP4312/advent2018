(ns advent-of-code.dec-02
  (:require [clojure.string :as str]
            [advent-of-code.input :refer [day2] :rename {day2 input}]))

;; Part 1
;; Generate a checksum given the input
;; Checksum = 3s * 2s
(defn twice?
  "Returns true if the string repeats a letter twice"
  [s]
  (some #(= 2 %) (vals (frequencies s))))

(defn thrice?
  "Returns true if the string repeats a letter thrice"
  [s]
  (some #(= 3 %) (vals (frequencies s))))

(def result (apply * (reduce (fn [[twos threes] s]
                               [(if (twice? s) (inc twos) twos)
                                (if (thrice? s) (inc threes) threes)])
                             [0 0] input)))

;; Part 2
;; Which letters are common between the correct IDs
;; Correct Ids are identical except for 1 letter
(defn id-parser
  "Given a pair of IDs return a string with disimilar characters replaced with nil"
  [[a b]]
  (map (fn [x y] (if (= x y) x nil)) a b))

(defn one-off?
  "Given a list return true if there exists only one nil"
  [x]
  (= 1 (get (frequencies x) nil)))

(def pairs (for [a input b input] [a b]))
(def result2 (->> pairs
                  (map id-parser)
                  (reduce #(when (one-off? %2) (reduced %2)))
                  (remove nil?)
                  (str/join "")))
