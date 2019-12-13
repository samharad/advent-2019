(ns days.five
  (:require [clojure.string :refer [split join]]
            [clojure.core.async :refer [to-chan]]
            [days.intcode-computer :as ic]))

(def MEM (ic/str->mem (slurp "input/five.txt")))
(do (ic/run-program (ic/make-prog MEM (to-chan [1])))
    (println "Soln A ^"))
(do (ic/run-program (ic/make-prog MEM (to-chan [5])))
    (println "Soln B ^"))

