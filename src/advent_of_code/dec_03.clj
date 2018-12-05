(ns advent-of-code.dec-03
  (:require [clojure.string :as str]
            [advent-of-code.input :refer [day3] :rename {day3 input}]))

;; PART 1
;; Part find the number of overlapping squares
(def matrix (vec (for [_ (repeat 1000 0)] (vec (repeat 1000 0)))))

(defn parse-claim
  "Given a claim-string return the parsed values"
  [s]
  (let [[id _ coords size] (str/split s #" ")
        [x y] (map read-string (str/split (str/replace coords #":" "") #","))
        [w h] (map read-string (str/split size #"x"))]
    {:id id
     :x x
     :y y
     :width w
     :height h}))

(defn apply-claim
  "Given a claim, increment the appropriate coordinates in the matrix"
  [m {:keys [id x y width height]}]
  (let [coords (for [a (range x (+ x width)) b (range y (+ y height))] [a b])]
    (reduce (fn [m [x y]] (update-in m [x y] inc)) m coords)))

(def claimed-matrix (reduce apply-claim matrix (map parse-claim input)))
(def result
  "Number of occurences where a cell is > 1"
  (reduce #(if (> %2 1) (inc %1) %1) 0 (flatten claimed-matrix)))

;; PART 2
(defn valid-claim?
  "Given a claimed matrix and a claim:
    Return true if the entire claim is without overlaps.
    An overlap is determined by an value being greater than 1"
  [m {:keys [id x y width height]}]
  (let [coords (for [a (range x (+ x width)) b (range y (+ y height))] [a b])]
    (every? (fn [[x y]] (= 1 (get-in m [x y]))) coords)))

(def result2 (first (filter (partial valid-claim? claimed-matrix) (map parse-claim input))))
