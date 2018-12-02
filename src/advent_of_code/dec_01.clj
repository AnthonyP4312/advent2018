(ns advent-of-code.dec-01
  (:require [clojure.string :as str]
            [advent-of-code.input :refer [day1]]))

;; PART 1
;; Find the total of a list of numbers and operations
(def num-seq (map read-string (str/split-lines day1)))
(def result (reduce + 0 num-seq))

;; PART 2
;; Find the first repeated total
(defn lazy-cat' [colls]
  (lazy-seq
   (if (seq? colls)
     (lazy-cat (first colls) (lazy-cat' (next colls))))))

(def inf-seq (lazy-cat' (repeat num-seq)))

(def result2
  (reduce (fn [[total set] n]
            (let [freq (+ total n)]
              (if (contains? set freq)
                (reduced freq)
                [freq (conj set freq)])))
          [0 #{0}] inf-seq))
