(ns days.two
  (:require [clojure.string :refer [split join]]
            [days.intcode-computer :as ic]))

(def MEM (ic/str->mem (slurp "input/two.txt")))

(defn with-noun-verb [mem n v]
  (-> mem
      (assoc 1 n)
      (assoc 2 v)))

(defn return-val [prog]
  (get-in prog [:mem 0]))

(defn determine-args [mem target-val]
  (->> (for [x (range 100) y (range 100)] [x y])
       (filter (fn [[n v]]
                 (let [init-prog (ic/make-prog (with-noun-verb mem n v))
                       terminal-prog (ic/run-program init-prog)]
                   (= (return-val terminal-prog)
                      target-val))))
       (first)))

(println "a:" (return-val (ic/run-program (ic/make-prog (with-noun-verb MEM 12 2)))))

(def solution-b
  (let [[n v] (determine-args MEM 19690720)]
    (+ (* 100 n)
       v)))

(println solution-b)



