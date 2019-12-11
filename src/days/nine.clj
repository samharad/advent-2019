(ns days.nine
  (:require [clojure.string :refer [split join trim]]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :refer [chan go go-loop close! onto-chan <! <!! >! >!!]]
            [days.util :refer [digits digits->num ?assoc pad]]))

(defn str->prog [s]
  (->> (split s #",")
       (map #(BigInteger. %))
       (vec)))

(defn make-prog
  ([mem] (make-prog mem nil nil))
  ([mem in-chan out-chan]
   (let [base-prog {:mem (vec (take 99999 (pad mem 0)))
                    :pc 0
                    :r-base 0}]
     (?assoc base-prog
             :in-chan in-chan
             :out-chan out-chan))))

(def MEM (str->prog (slurp "input/nine.txt")))

(defn opcode [instr]
  (rem instr 100))

(defn param-modes [instr]
  (let [provided-pms (reverse (digits (quot instr 100)))]
    (pad provided-pms 0)))

(defn parse-instr [instr]
  ((juxt opcode param-modes) instr))

(defn- arg [{:keys [mem r-base]} param param-mode]
  (case param-mode
    0 {:reg param :val (get mem param)}
    1 {:val param}
    2 (let [r (+ r-base param)]
        {:reg r :val (get mem r)})
    (throw (ex-info "Invalid param-mode" {:param-mode param-mode}))))

(defn args [{:keys [mem pc] :as prog}]
  (let [p-vals (fn p-vals [[p & more-params] [pm & more-param-modes]]
                 (lazy-seq
                   (cons (arg prog p pm)
                         (p-vals more-params more-param-modes))))
        ps (rest (drop pc mem))
        pms (param-modes (get mem pc))] ; TODO
    (p-vals ps pms)))

(def op-type {1 :add
              2 :multiply
              3 :readln
              4 :putln
              5 :jump-if-true
              6 :jump-if-false
              7 :less-than
              8 :equals
              9 :r-base-offset})

(defmulti exec (fn [{:keys [mem pc]} _]
                 (op-type (opcode (get mem pc)))))

(defmethod exec :r-base-offset [{:keys [pc r-base] :as prog} [a]]
  (let [r-base (+ r-base (:val a))
        pc (+ pc 2)]
    (assoc prog :r-base r-base :pc pc)))

(defmethod exec :add [{:keys [mem pc] :as prog} [a b r]]
  (let [v (apply + (map :val [a b]))
        mem (assoc mem (:reg r) v)
        pc (+ pc 4)]
    (assoc prog :mem mem :pc pc)))

(defmethod exec :multiply [{:keys [mem pc] :as prog} [a b r]]
  (let [v (apply * (map :val [a b]))
        mem (assoc mem (:reg r) v)
        pc (+ pc 4)]
    (assoc prog :mem mem :pc pc)))

(defmethod exec :readln [{:keys [mem pc in-chan] :as prog} [r]]
  (let [v (or (and in-chan (BigInteger. (<!! in-chan)))
              (do
                (print "Enter a number: ")
                (flush)
                (BigInteger. (read-line))))
        mem (assoc mem (:reg r) v)
        pc (+ pc 2)]
    (assoc prog :mem mem :pc pc)))

(defmethod exec :putln [{:keys [mem pc out-chan] :as prog} [a]]
  (let [v (:val a)
        pc (+ pc 2)]
    (if out-chan
      (assert (>!! out-chan v))
      (println "OUTPUT:" v))
    (assoc prog :pc pc)))

(defmethod exec :jump-if-true [{:keys [mem pc] :as prog} [a b]]
  (let [[av bv] (map :val [a b])
        pc (if ((complement zero?) av) bv (+ pc 3))]
    (assoc prog :pc pc)))

(defmethod exec :jump-if-false [{:keys [mem pc] :as prog} [a b]]
  (let [[av bv] (map :val [a b])
        pc (if (zero? av) bv (+ pc 3))]
    (assoc prog :pc pc)))

(defmethod exec :less-than [{:keys [mem pc] :as prog} [a b r]]
  (let [[av bv] (map :val [a b])
        v (if (< av bv) 1 0)
        mem (assoc mem (:reg r) v)
        pc (+ pc 4)]
    (assoc prog :mem mem :pc pc)))

(defmethod exec :equals [{:keys [mem pc] :as prog} [a b r]]
  (let [[av bv] (map :val [a b])
        v (if (= av bv) 1 0)
        mem (assoc mem (:reg r) v)
        pc (+ pc 4)]
    (assoc prog :mem mem :pc pc)))

(defn run-program
  ([prog]
   (loop [{:keys [mem pc] :as prog} prog]
     (let [instr (get mem pc 99)
           oc (opcode instr)
           args (args prog)]
       (if (= oc 99)
         (get mem 0)
         (recur (exec prog args)))))))

(println "Soln A:" (run-program (make-prog MEM)))

