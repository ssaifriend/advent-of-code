(ns aoc2021.d1
  (:require [util :refer [read-file ->split-line->int]]))

(defn get-read-file []
  (-> "2021/2021.1.input" read-file ->split-line->int))

(defn count-increase [inputs]
  (reduce (fn [m x]
            (-> (if-let [recent (peek (:stack m))]
                  (if (< recent x)
                    (update m :acc inc)
                    m)
                  m)
                (update :stack #(conj % x))))
          {:stack []
           :acc 0}
          inputs))

(defn three-measurements [inputs]
  (-> (reduce (fn [m x]
                (println m)
                (let [m (-> (if (= (count (:queue m)) 3)
                              (update m :queue #(rest %))
                              m)
                            (update :queue #(conj (vec %) x)))]
                  (if (= (count (:queue m)) 3)
                    (update m :new-inputs #(conj % (reduce + (:queue m))))
                    m)))
              {:queue []
               :new-inputs []}
              inputs)
      :new-inputs))

(comment
  (count-increase [1 2 3])
  (-> (get-read-file) count-increase :acc) ;; part 1
  (-> (get-read-file) three-measurements count-increase :acc) ;; part2
  )
