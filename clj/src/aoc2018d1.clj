(ns aoc2018d1
  (:require [clojure.string :as s]))

(defn parse->process
  "line 단위로 분리하고 숫자로 변환합니다."
  [ds]
  (->> (s/split ds #"\n")
       (map #(Integer/parseInt %))))

(defn ->aggregate-part-1
  "입력받은 값을 모두 더합니다."
  [ds]
  (apply + ds))

(defn ->find-twice-search
  "더할때 반환값이 두번째로 나오는 값을 반환합니다."
  [ds]
  (loop [sum 0
         sums #{}
         ds (cycle ds)]
    (let [sum (+ sum (first ds))]
      (if (contains? sums sum)
        sum
        (recur sum
               (conj sums sum)
               (rest ds))))))

(comment
  (->>
    (slurp "../input/2018/2018.1.input")
    (parse->process)
    (->aggregate-part-1)
    (println))
  (->>
    (slurp "../input/2018/2018.1.input")
    (parse->process)
    (->find-twice-search)
    (println))

  (let [test #{1 2 3}]
    (contains? test 3))
  (let [test (cycle [1 2 3])]
    (first test))
  (let [test #{}]
    (contains? test 0)))
