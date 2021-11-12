(ns days.twelve
  (:require [clojure.string :refer [split-lines]]
            [clojure.math.combinatorics :as combo])
  (:import (java.util Comparator)))

(defn add-vecs [& vs]
  (vec (apply map + vs)))

(defn make-moon [coord velocity]
  {:coord coord
   :velocity velocity})

(defn apply-gravity [target influencer]
  (let [[ct ci] (map :coord [target influencer])
        comparator (Comparator/naturalOrder)
        acc (map #(.compare comparator %1 %2)
                 ci
                 ct)]
    (update-in target [:velocity] (partial add-vecs acc))))

(defn move-moon [{:keys [velocity] :as moon}]
  (update-in moon [:coord] (partial add-vecs velocity)))

(defn energy [v]
  (apply + (map #(Math/abs %) v)))

(defn potential-energy [{:keys [coord]}]
  (energy coord))

(defn kinetic-energy [{:keys [velocity]}]
  (energy velocity))

(defn moon-energy [moon]
  (apply * ((juxt potential-energy kinetic-energy) moon)))

(defn total-energy [moons]
  (apply + (map moon-energy moons)))

(defn gravitated [moons]
  (map (fn [target]
         (reduce (fn [t influencer]
                   (apply-gravity t influencer))
                 target
                 moons))
       moons))

(defn step [moons]
  (->> moons
       (gravitated)
       (map move-moon)))

(defn simulation [moons]
  (rest (iterate step moons)))

(def COORDS [[19 -10 7]
             [1 2 -3]
             [14 -4 1]
             [8 7 -6]])

(def TEST [[-8 -10 0]
           [5 5 10]
           [2 -7 3]
           [9 -8 -3]])

(defn coords->moons [coords]
  (map #(make-moon % [0 0 0])
       coords))

(def MOONS (map #(make-moon % [0 0 0]) COORDS))

(def SIMULATION (rest (iterate step MOONS)))

(def TEST-2 [[-1 0 2]
             [2 -10 -7]
             [4 -8 8]
             [3 5 1]])

(def solution-a
  (->> SIMULATION
       (take 1000)
       (last)
       (total-energy)))

;(def solution-b
;  (reduce (fn [states state]
;            (if (states state)
;              (reduced (inc (count states)))
;              (conj states state)))
;          #{}
;          SIMULATION))

(println "a" solution-a)
;(println "b" solution-b)


(def xs (map first TEST-2))
(def ys (map second TEST-2))
(def zs (map last TEST-2))

(def x-moons (map (comp coords->moons vec) xs))

