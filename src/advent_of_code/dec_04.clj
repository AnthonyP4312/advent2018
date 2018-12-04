(ns advent-of-code.dec-04
  (:require [clojure.string :as str]
            [java-time :refer [local-date-time]]
            [advent-of-code.input :refer [day4] :rename {day4 input}]))

(defn parse-input
  [s]
  (let [[time event] (str/split (str/replace s #"\[(.*)\] (.*)" "$1_$2") #"_")
        parsed-time (local-date-time "yyyy-MM-dd HH:mm" time)]
    {:time parsed-time
     :event event}
    ))

(parse-input "[1518-05-18 00:57] wakes up")

(map :event (sort-by :time (map parse-input input)))
