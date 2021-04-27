(ns aoc2018.d4
  (:require [clojure.string :as s]
            [java-time :as j]
            [util]))

(def ops ["Guard #([0-9]+) begins shift"
          "falls asleep"
          "wakes up"])

(def parse-pattern
  (->> (str "(" (s/join "|" ops) ")")
       (re-pattern)))

(defn split [line]
  (let [re (re-matches #"\[([0-9-]{10}) ([0-9:]{5})\] (.*)" line)
        [_ date time op] re
        t (j/local-date-time "yyyy-MM-dd HH:mm" (str date " " time))]
    (when (not (nil? re))
      (let [re (re-matches parse-pattern op)
            [_ a b] re
            [op] (s/split a #" ")]
        (cond
          (= op "Guard")
          {:op :shift :t t :id (Integer/parseInt b)}

          (= op "falls")
          {:op :sleep :t t}

          (= op "wakes")
          {:op :wake :t t})))))

(defn attach-id [coll]
  (->> coll
       (reduce (fn [[head & _ :as coll] hm]
                 (conj coll (if (and head (not (:id hm)))
                              (assoc hm :id (:id head))
                              hm)))
               ())
       (reverse)))

(defn parse [f]
  (->> f
       (s/split-lines)
       (map split)))

(defn generate-minute-range [start end]
  (->> (:t start)
       (iterate #(.plusMinutes % 1))
       (take-while #(.isBefore % (:t end)))))

(defn preprocess [coll]
  (->> coll
       (sort-by :t)
       (attach-id)
       (filter #(not= :shift (:op %)))
       (group-by :id)
       (mapcat (fn [[id coll]]
                 (->> coll
                      (partition 2)
                      (mapcat (fn [[start end]] (generate-minute-range start end)))
                      (map (fn [m] [id (.getMinute m)])))))))

(defn find-first-max-count [coll]
  (->> coll
       (map (fn [[id coll]] [id (count coll)]))
       (sort-by second >)
       (first)))

(defn find-frequency [coll f]
  (->> coll
       (map f)
       (frequencies)
       (sort-by second >)
       (first)))

(defn aggregate [f g coll]
  (let [coll (->> coll (group-by f))
        [a _] (find-first-max-count coll)
        [b _] (find-frequency (coll a) g)]
    (* a b)))

(defn aggregate-part1 [coll]
  (->> coll (aggregate first second)))

(defn aggregate-part2 [coll]
  (->> coll (aggregate second first)))

(comment
  "엄격" #"\[([0-9]{4})-([0-9]{2})-([0-9]{2})\] ([0-9]{2}):([0-9]{2})"
  (def input "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up")
  (def input (util/read-file "2018/2018.4.input"))
  (def preprocessed (->> input (parse) (preprocess)))
  (->> preprocessed (aggregate-part1))
  (->> preprocessed (aggregate-part2)))
