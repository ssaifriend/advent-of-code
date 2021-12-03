(ns aoc2021.d2
  (:require [util :refer [read-file]]
            [clojure.string :as s]))

(defn get-read-file []
  (-> "2021/2021.2.input" read-file s/split-lines))

(defn parse [inputs]
  (map (fn [input]
         (let [[action x] (s/split input #" ")]
           {:action (keyword action)
            :x (Integer/parseInt x)}))
       inputs))

(defn move [inputs]
  (reduce (fn [m {:keys [action x]}]
            (case action
              :forward (update m :position #(+ % x))
              :up (update m :depth #(- % x))
              :down (update m :depth #(+ % x))))
          {:depth 0
           :position 0}
          inputs))

(comment
  (let [result (-> (get-read-file) parse move)]
    (* (:depth result) (:position result))))
