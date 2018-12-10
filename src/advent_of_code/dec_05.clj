(ns advent-of-code.dec-05
  (:require [clojure.zip :as z]
            [clojure.string :as str]
            [advent-of-code.input :refer [day5] :rename {day5 input}]))

;; PART 1
;; Sounds like a merge-sort kindof thing (it wasnt)
(defn ->lower
  [x]
  (try (Character/toLowerCase x)
       (catch Exception e nil)))

(defn upper?
  [x]
  (try (Character/isUpperCase x)
       (catch Exception e nil)))

(defn react
  "Evaluate whether two units react with eachother.

  The two units given are each a seq of characters, Reaction is determined using
  the last element of the first seq and the first element of the second seq.

  If they react, return an empty seq. Else, concat the given seqs

  If either element of the pair is nil then return the seq without the nil"
  [xs ys]
  (let [x (peek xs)
        y (first ys)
        case-diff (= #{true false}
                     (into #{} (map upper? [x y])))
        same-letter (apply = (map ->lower [x y]))]
    (if (and case-diff same-letter)
      (vec (concat (pop xs) (drop 1 ys)))
      (vec (concat xs ys)))))

(def result (count (reduce react (mapv vector input))))
