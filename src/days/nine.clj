(ns days.nine
  (:require [clojure.string :refer [split join trim]]
            [clojure.core.async :refer [to-chan chan go go-loop close! onto-chan <! <!! >! >!!]]
            [days.util :refer [digits digits->num ?assoc pad]]
            [days.intcode-computer :as ic]))

(def MEM (ic/str->mem (slurp "input/nine.txt")))

(do (ic/run-program (ic/make-prog MEM (to-chan [1])))
    (println "Soln A ^"))
(do (ic/run-program (ic/make-prog MEM (to-chan [2])))
    (println "Soln B ^"))

