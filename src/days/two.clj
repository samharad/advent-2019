(ns days.two
  (:require [clojure.string :refer [split join]]))

(def input (slurp "input/two.txt"))

(defn str->prog [s]
  (->> (split s #",")
       (map #(Integer. %))
       (vec)))

(defn run-program [program]
  (loop [program program
         i 0]
    (let [subv #(try (subvec %1 %2 %3)
                     (catch IndexOutOfBoundsException _ (subvec %1 %2)))
          new-i (+ i 4)
          [instr a-idx b-idx dest] (subv program i new-i)
          [a b] (map #(get program %) [a-idx b-idx])]
      (case instr
        1 (recur (assoc program dest (+ a b))
                 new-i)
        2 (recur (assoc program dest (* a b))
                 new-i)
        99 (get program 0)))))

(defn with-noun-verb [prog n v]
  (-> prog
      (assoc 1 n)
      (assoc 2 v)))

(defn determine-args [program target-val]
  (->> (for [x (range 100) y (range 100)] [x y])
       (filter (fn [[n v]]
                 (= (run-program (with-noun-verb program n v))
                    target-val)))
       (first)))

(defn solution-a []
  (-> input
      (str->prog)
      (with-noun-verb 12 2)
      (run-program)))

(println (solution-a))

(defn solution-b []
  (let [[n v] (-> input
                  (str->prog)
                  (determine-args 19690720))]
    (+ (* 100 n)
       v)))

(println (solution-b))



