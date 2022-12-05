(ns aoc2022.d5
  (:require [clojure.string :as s]
            [util :refer [read-file]]))

;; init
;; [S]                 [T] [Q]
;; [L]             [B] [M] [P]     [T]
;; [F]     [S]     [Z] [N] [S]     [R]
;; [Z] [R] [N]     [R] [D] [F]     [V]
;; [D] [Z] [H] [J] [W] [G] [W]     [G]
;; [B] [M] [C] [F] [H] [Z] [N] [R] [L]
;; [R] [B] [L] [C] [G] [J] [L] [Z] [C]
;; [H] [T] [Z] [S] [P] [V] [G] [M] [M]
;; 1   2   3   4   5   6   7   8   9

(def stack
  (into (sorted-map) {1 [\H \R \B \D \Z \F \L \S]
                      2 [\T \B \M \Z \R]
                      3 [\Z \L \C \H \N \S]
                      4 [\S \C \F \J]
                      5 [\P \G \H \W \R \Z \B]
                      6 [\V \J \Z \G \D \N \M \T]
                      7 [\G \L \N \W \F \S \P \Q]
                      8 [\M \Z \R]
                      9 [\M \C \L \G \V \R \T]}))

(defn get-read-file []
  (-> "2022/2022.5.input" read-file s/split-lines))

(defn parse [ss]
  (map (fn [s]
         (let [re (re-find #"move ([0-9]+) from ([0-9]) to ([0-9])" s)
               [_ cnt from to] re]
           {:cnt (Integer/parseInt cnt)
            :from (Integer/parseInt from)
            :to (Integer/parseInt to)}))
       ss))

(defn process [stack moves]
  (reduce (fn [stack {:keys [cnt from to stack?]}]
            (let [items (take-last cnt (stack from))
                  moves (if stack? (reverse items) items)]
              (-> stack
                  (update from #(drop-last cnt %))
                  (update to #(concat % moves)))))
          stack
          moves))

(defn print-result [stack]
  (->> (vals stack)
       (map last)
       (s/join "")))

(comment
  (def stack
    {1 [\Z \N]
     2 [\M \C \D]
     3 [\P]})
  (def sample "move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

  ;; p1
  (->> (get-read-file)
       parse
       (map #(assoc % :stack? true))
       (process stack)
       print-result)

  ;; p2
  (->> (get-read-file)
       parse
       (map #(assoc % :stack? false))
       (process stack)
       print-result)
  :rcf)