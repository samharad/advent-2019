(ns days.five
  (:require [clojure.string :refer [split join]]))

(defn digits [n]
  (letfn [(digits [n]
            (if (= n 0)
              []
              (conj (digits (quot n 10))
                    (rem n 10))))]
    (if (= n 0)
      [0]
      (digits n))))

(defn digits->num [digs]
  (let [places (map #(int (Math/pow 10 %)) (range))]
    (->> (reverse digs)
         (map vector places)
         (reduce #(+ %1 (apply * %2)) 0))))

(defn pad [xs p]
  (concat xs (repeat p)))

(defn- get-arg [prog param param-mode]
  (if (zero? param-mode)
    (get prog param)
    param))

(defn- get-args-and-return
  [prog i param-modes]
  (->> (conj (vec param-modes) 1)
       (map vector (rest (drop i prog)))
       (map #(apply get-arg (cons prog %)))))

(defn computation
  "Returns [new-program new-i]"
  [program i param-modes compute-fn]
  (let [args (get-args-and-return program i param-modes)
        computed-val (apply compute-fn (butlast args))
        new-prog (assoc program (last args) computed-val)
        new-i (+ i (inc (count args)))]
    [new-prog new-i]))

(defn multiply [program i param-modes]
  (computation program i (take 2 param-modes) *))

(defn add [program i param-modes]
  (computation program i (take 2 param-modes) +))

(defn readln [program i _]
  (computation program i [] #(do
                               (println "Enter a single digit integer:")
                               (Integer. (read-line)))))

(defn putln [program i param-modes]
  (let [[x _] (get-args-and-return program i (take 1 param-modes))]
    (println "PROGRAM OUTPUT:" x)
    [program (+ i 2)]))

(defn jump-if-true [program i param-modes]
  (let [[x y _] (get-args-and-return program i (take 2 param-modes))]
    (if (zero? x)
      [program (+ i 3)]
      [program y])))

(defn jump-if-false [program i param-modes]
  (let [[x y _] (get-args-and-return program i (take 2 param-modes))]
    (if (zero? x)
      [program y]
      [program (+ i 3)])))

(defn less-than [program i param-modes]
  (computation program i (take 2 param-modes) #(if (< %1 %2) 1 0)))

(defn equals [program i param-modes]
  (computation program i (take 2 param-modes) #(if (= %1 %2) 1 0)))

(def opcode-handler {1 add
                     2 multiply
                     3 readln
                     4 putln
                     5 jump-if-true
                     6 jump-if-false
                     7 less-than
                     8 equals})

(defn str->prog [s]
  (->> (split s #",")
       (map #(Integer. %))
       (vec)))

(defn run-program [program]
  (loop [program program
         i 0]
    (let [instr (get program i)
          digs (digits instr)
          opcode (digits->num (take-last 2 digs))
          param-modes (pad (reverse (drop-last 2 digs))
                           0)
          handler (opcode-handler opcode)]
      (if (= opcode 99)
        (get program 0)
        (let [[new-p new-i] (handler program i param-modes)]
          (recur new-p new-i))))))

