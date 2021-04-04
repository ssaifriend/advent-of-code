(ns aoc2018.d1
  (:require [util]))

(defn pp []
  (->>
    (util/read-file "2018/2018.1.input")
    (util/split-line-to-int)))

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
    (pp)
    (->aggregate-part-1)
    (println))
  (->>
    (pp)
    (->find-twice-search)
    (println))

  (let [test #{1 2 3}]
    (contains? test 3))
  (let [test (cycle [1 2 3])]
    (first test))
  (let [test #{}]
    (contains? test 0)))
