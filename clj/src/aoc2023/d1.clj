(ns aoc2023.d1
  (:require [clojure.string :as s]
            [util :refer [read-file]]))

(defn read-file-> []
  (-> "2023/2023.1.input" read-file s/split-lines))

(defn string-int? [c]
  (<= (int \0) (int c) (int \9)))

(defn string-int->int [c]
  (- (int c) (int \0)))

(defn filter-string-int [s]
  (filter string-int? (seq s)))

(defn ->make-digit [s]
  (let [a (first s)
        b (last s)]
    (+ (* 10 (string-int->int a))
       (string-int->int b))))

(defn q1 [ss]
  (->> ss
       (map filter-string-int)
       (map ->make-digit)
       (apply +)))

(defn text-int->int [s]
  (let [pattern "(one|two|three|four|five|six|seven|eight|nine|1|2|3|4|5|6|7|8|9)"
        text-int->int-map {"one" \1 "two" \2 "three" \3 "four" \4 "five" \5 "six" \6 "seven" \7 "eight" \8 "nine" \9
                           "1" \1 "2" \2 "3" \3 "4" \4 "5" \5 "6" \6 "7" \7 "8" \8 "9" \9}]
    [(text-int->int-map (first (re-find (re-pattern pattern) s)))
     (text-int->int-map (second (re-find (re-pattern (str ".*(?<=" pattern ")")) s)))]))

(defn q2 [ss]
  (->> ss
       (map text-int->int)
       (map filter-string-int)
       (map ->make-digit)
       (apply +)))

(comment
  (def sample "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

  (string-int? \a)
  (q1 (s/split-lines sample))
  (q1 (read-file->))

  (def sample "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")
  (map text-int->int (s/split-lines sample))
  (q2 (s/split-lines sample))
  (q2 (read-file->))
  :rcf)
