(ns aoc2018.d1
  (:require [util :refer [read-file ->split-line->int]]))

(defn pp []
  (-> "2018/2018.1.input" read-file ->split-line->int))

(defn ->aggregate-part-1
  "입력받은 값을 모두 더합니다."
  [ds]
  (apply + ds))

(defn ->find-twice-search
  "더할때 반환값이 두번째로 나오는 값을 반환합니다."
  [ds]
  (->> (cycle ds)
       (reductions +)
       (reduce (fn [s v]
                 (if (contains? s v)
                   (reduced v)
                   (conj s v)))
               #{})))

(comment
  (->> (pp) ->aggregate-part-1 println)
  (->> (pp) ->find-twice-search println)

  (let [test #{1 2 3}]
    (contains? test 3))
  (let [test (cycle [1 2 3])]
    (first test))
  (let [test #{}]
    (contains? test 0)))
