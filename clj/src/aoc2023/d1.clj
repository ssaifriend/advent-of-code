(ns aoc2023.d1
  (:require [clojure.string :as s]
            [util :refer [read-file]]))

(defn read-file-> []
  (-> "2023/2023.1.input" read-file s/split-lines))

(defn string-int? [c]
  (<= (int \0) (int c) (int \9)))

(defn string-int->int [c]
  (- (int c) (int \0)))

(defn q1 [ss]
  (->> ss
       (map (fn [row] (->> (seq row)
                           (filter string-int?))))
       (map (fn [row]
              (let [a (first row)
                    b (last row)]
                (+ (* 10 (string-int->int a))
                   (string-int->int b)))))
       (apply +)))

(comment
  (def sample "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

  (string-int? \a)
  (q1 (s/split-lines sample))
  (q1 (read-file->))
  :rcf)
