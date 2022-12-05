(ns aoc2022.d3
  (:require [clojure.string :as s]
            [clojure.set :refer [intersection]]
            [util :refer [read-file]]))

(def priority
  (merge (zipmap (range (int \a) (inc (int \z)))
                 (range 1 27))
         (zipmap (range (int \A) (inc (int \Z)))
                 (range 27 53))))

(defn get-read-file []
  (-> "2022/2022.3.input" read-file s/split-lines))

(defn parse-p1 [ss]
  (->> ss
       (map (fn [s]
              (let [len (count s)
                    half (/ len 2)]
                [(take half s) (drop half s)])))
       (map #(map set %))))

(defn parse-p2 [ss]
  (partition 3 ss))

(defn p1 [ss]
  (->> ss
       (map #(apply intersection %))
       (map #(-> % first int priority))))

(defn p2 [ss]
  (->> ss
       (map #(->> %
                  (map set)
                  (apply intersection)))
       (map #(-> % first int priority))))

(comment
  (def sample "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")
  priority

  (->> (s/split-lines sample)
       parse-p1
       p1
       (apply +))
  (->> (get-read-file) parse-p1 p1 (apply +))
  (->> (s/split-lines sample)
       parse-p2
       p2
       (apply +))
  (->> (get-read-file) parse-p2 p2 (apply +))
  :rcf)