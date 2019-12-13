(ns days.seven
  (:require [clojure.string :refer [split join trim]]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :refer [chan go go-loop close! onto-chan <! <!! >! >!!]]
            [days.util :refer :all]
            [days.intcode-computer :as ic]))

(defn make-progs [mem phase-settings]
  (reduce (fn [progs phase-setting]
            (let [in (or (:out-chan (last progs))
                         (chan 1))
                  out (chan 1)
                  prog (ic/make-prog mem in out)]
              (assert (>!! in phase-setting))
              (go (ic/run-program prog)
                  (close! out))
              (conj progs prog)))
          []
          phase-settings))

(defn optimize [phases return-chan chain-runner]
  (apply max (map (fn [ps]
                    (chain-runner ps)
                    (<!! return-chan))
                  (combo/permutations phases))))

(defn optimize-phase-settings [mem phases]
  (let [r-chan (chan)]
    (optimize
      phases
      r-chan
      (fn [phase-settings]
        (let [progs (make-progs mem phase-settings)
              start-chan (:in-chan (first progs))]
          (go (let [last-out (:out-chan (last progs))
                    v (<! last-out)]
                (close! start-chan)
                (assert (>! r-chan v))))
          (go (assert (>! start-chan 0))))))))

(defn optimize-recursive-phase-settings [prog phases]
  (let [return-chan (chan)]
    (optimize
      phases
      return-chan
      (fn [phase-settings]
        (let [progs (make-progs prog phase-settings)
              start-chan (:in-chan (first progs))]
          (go-loop [last-val nil]
            (let [last-chan (:out-chan (last progs))
                  v (<! last-chan)]
              (if (some? v)
                (do
                  (assert (>! start-chan v))
                  (recur v))
                (assert (>! return-chan last-val)))))
          (go (>! start-chan 0)))))))

(def MEM (ic/str->mem (slurp "input/seven.txt")))
(println "Soln A:" (optimize-phase-settings MEM (range 5)))
(println "Soln B:" (optimize-recursive-phase-settings MEM (range 5 10)))
