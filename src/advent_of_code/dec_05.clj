(ns advent-of-code.dec-05
  (:require [clojure.string :as str]
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
  "Evaluate whether two units react with eachother."
  [xs y]
  (let [x (peek xs)
        case-diff (= #{true false}
                     (into #{} (map upper? [x y])))
        same-letter (apply = (map ->lower [x y]))]
    (if (and case-diff same-letter)
      (pop xs)
      (conj xs y))))

(def result (count (reduce react [] input)))
