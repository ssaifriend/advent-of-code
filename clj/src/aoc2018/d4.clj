(ns aoc2018.d4
  (:require [clojure.string :as s]
            [java-time :as j]
            [util]))

(def ops ["Guard #([0-9]+) begins shift"
          "falls asleep"
          "wakes up"])

(def parse-pattern
  (re-pattern (str "(" (s/join "|" ops) ")")))

(defn parse [f]
  (->> (s/split-lines f)
       (map (fn [line]
              (let [re (re-matches #"\[([0-9-]{10}) ([0-9:]{5})\] (.*)" line)
                    [_ date time op] re
                    [_ a b] (re-matches parse-pattern op)

                    op (first (s/split a #" "))
                    t (j/local-date-time "yyyy-MM-dd HH:mm" (str date " " time))]
                (case op
                  "Guard"
                  {:op :shift :t t :id (Integer/parseInt b)}

                  "falls"
                  {:op :sleep :t t}

                  "wakes"
                  {:op :wake :t t}))))))

(defn attach-id [coll]
  (->> coll
       (reduce (fn [[head & _ :as coll] hm]
                 (conj coll (if (and head (not (:id hm)))
                              (assoc hm :id (:id head))
                              hm)))
               ())
       reverse))

(defn generate-minute-range [start end]
  (->> (:t start)
       (iterate #(.plusMinutes % 1))
       (take-while #(.isBefore % (:t end)))))

(defn preprocess [coll]
  (->> coll
       (sort-by :t)
       attach-id
       (filter #(not= :shift (:op %)))
       (partition 2)
       (mapcat (fn [[start end]]
                 (->> (generate-minute-range start end)
                      (map (fn [m] [(:id start) (.getMinute m)])))))))

(defn find-first-max-count [coll]
  (->> coll
       (map (fn [[id coll]] [id (count coll)]))
       (apply max-key second)))

(defn find-frequency [coll]
  (->> coll
       (map second)
       frequencies
       (apply max-key second)))

(defn aggregate-part-1 [coll]
  (let [coll (->> coll (group-by first))
        [a _] (find-first-max-count coll)
        [b _] (find-frequency (coll a) )]
    (* a b)))

(defn aggregate-part-2 [coll]
  (let [coll (->> coll (group-by identity))
        [a _] (find-first-max-count coll)]
    (apply * a)))

(comment
  "엄격" #"\[([0-9]{4})-([0-9]{2})-([0-9]{2})\] ([0-9]{2}):([0-9]{2})"
  (def input "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up")
  (def input (util/read-file "2018/2018.4.input"))
  (def preprocessed (->> input (parse) (preprocess)))
  (->> preprocessed (aggregate-part-1))
  (->> preprocessed (aggregate-part-2)))
