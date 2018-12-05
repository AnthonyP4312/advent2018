(ns advent-of-code.dec-04
  (:require [clojure.string :as str]
            [java-time :as t :refer [local-date-time]]
            [advent-of-code.input :refer [day4] :rename {day4 input}]))

;; Part 1
;; Find the guard who sleeps the most
;; and find the specific minute they are most often asleep
(defn parse-input
  [s]
  (let [[time event] (str/split (str/replace s #"\[(.*)\] (.*)" "$1_$2") #"_")
        parsed-time (local-date-time "yyyy-MM-dd HH:mm" time)]
    {:time parsed-time
     :event event}))

(defn parse-int [number-string]
  (try (Integer/parseInt number-string)
       (catch Exception e nil)))

(defn extract-id
  "Given an event string return the ID of the guard"
  [s]
  (str/replace s #".*#([0-9]+).*" "$1"))

(defn time-slept
  "Given a seq of events return a tuple of guard ID and total minutes slept"
  [[guard & events]]
  (let [pairs (partition 2 (map :time events))
        minutes (fn [[s w]] (t/as (t/duration s w) :minutes))
        sum (reduce + (map minutes pairs))]
    [(extract-id (:event guard)) sum]))

(defn overlap
  "Given a seq of sleeping habits return the minute that guard slept the most"
  [hs]
  (let [minutes (map parse-int (map #(t/format "mm" (:time %)) hs))
        ranges (map #(apply range %) (partition 2 minutes))]
    (when-not (empty? ranges)
            (first (apply max-key val (frequencies (flatten ranges)))))))

(def chunked-result
  "Chronologically sorted events paritioned for each evening"
  (->> input
       (map parse-input)
       (sort-by :time)
       (reduce (fn [acc val]
                 (let [last (peek acc) event (:event val)]
                   (if (re-matches #".*[0-9].*" event)
                     (conj acc [val])
                     (conj (pop acc) (conj last val))))) [])))

(def habits
  "Map from Guard ID to seq of all chronologically sorted events"
  (->> chunked-result
       (group-by #(extract-id (first %)))
       (map (fn [[k v]] [k (mapcat rest v)]))
       (into {})))

(def sleepy-guard
  "The Guard who slept the most and the minutes slept"
  (->> chunked-result
                 (map time-slept)
                 (group-by first)
                 (map (fn [[k v]] [k (reduce + (map second v))]))
                 (sort-by second)
                 (last)))

(def result
  (let [[guard time-slept] sleepy-guard
        minute (overlap (get habits guard))]
    (* (parse-int guard) minute)))

;; PART 2
(defn minute-frequency
  "Given a seq of habits return a map of minute slept and frequencies"
  [hs]
  (let [minutes (map parse-int (map #(t/format "mm" (:time %)) hs))
        ranges (map #(apply range %) (partition 2 minutes))]
    (when-not (empty? ranges)
      (frequencies (flatten ranges)))))

(def guard->minutes
  "A map from guard ID to a map of minutes to frequency of sleep"
  (into {} (map (fn [[k v]] [k (minute-frequency v)]) habits)))

(def result-set
  "A list of tuples of minute, guardID, and frequency where the tuple represents
  the guard most likely to sleep for the given minute"
  (for [minute (range 0 60)]
    (let [guard (last (sort-by #(get-in guard->minutes [% minute]) (keys habits)))]
      [minute guard (get-in guard->minutes [guard minute])])))

(def result2
  (let [[m g f] (last (sort-by #(get % 2) result-set))]
    (* (parse-int g) m)))
