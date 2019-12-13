(ns days.ten
  (:require [clojure.string :refer [split-lines]]))

(defn dist [a b]
  (let [delts (map - b a)
        sq-delts (map #(Math/pow % 2) delts)]
    (Math/sqrt (apply + sq-delts))))

(defn slope [a b]
  (let [[run rise] (map - b a)]
    (if (zero? run)
      :undefined
      (/ rise run))))

(defn colinear? [& coords]
  (->> (partition 2 1 coords)
       (map #(apply slope %))
       (apply =)))

(defn str->roids [s]
  (let [cs (->> (split-lines s)
                (map seq))
        coords (map-indexed
                 (fn [r chars]
                   (map-indexed
                     (fn [c char]
                       (when (= char \#)
                         [c r]))
                     chars))
                 cs)]
    (->> coords
         (mapcat identity)
         (filter identity))))

(defn between?
  "Whether x is between a and b."
  [x a b]
  (= x (second (sort [x a b]))))

(defn obfuscation
  "If either ra or rb obscures the other from the POV
  of the vantage roid, returns [blocking blocked]."
  [vantage-roid ra rb]
  (and (colinear? vantage-roid rb ra)
       (not (between? vantage-roid rb ra))
       (sort-by #(dist vantage-roid %) [rb ra])))

(defn visible-from
  "Returns the roids visible from the specified roid."
  [roids vantage-roid]
  (let [other-roids (filter #(not= vantage-roid %) roids)]
    (reduce (fn [acc r]
              (let [[blocking blocked] (some #(obfuscation vantage-roid % r) acc)]
                (if (nil? blocking)
                  (conj acc r)
                  (-> acc (disj blocked) (conj blocking)))))
            #{}
            other-roids)))

(defn rank-vantages [roids]
  (->> roids
       (map (fn [r] [r (visible-from roids r)]))
       (sort-by (comp count second))
       (reverse)))

(defn best-vantage [roids]
  (let [[roid visible] (first (rank-vantages roids))]
    (count visible)))

(def ROIDS (str->roids (slurp "input/ten.txt")))

(defn slope-key [s]
  (if (= :undefined s) Integer/MIN_VALUE s))

(defn left-of?
  "Whether a is left of b. True if a is directly below b;
  false if it is directly above."
  [[ax ay] [bx by]]
  (or (< ax bx)
      (and (= ax bx)
           (> ay by))))

(defn vaporize
  "Returns a seq of the roids in order of vaporization"
  [roids shooter-roid]
  (let [target-roids (filter (partial not= shooter-roid) roids)
        by-proximity (sort-by (partial dist shooter-roid) target-roids)
        grouped-by-slope (into [] (group-by #(vector (left-of? % shooter-roid)
                                                     (slope shooter-roid %))
                                            by-proximity))
        sorted-by-slope (sort-by (fn [[[left-of-shooter? slope] & _]]
                                   [left-of-shooter? (slope-key slope)])
                                 grouped-by-slope)
        by-line-of-sight (map second sorted-by-slope)]
    (mapcat (fn [i]
              (map #(get % i) by-line-of-sight))
            (range))))

(defn soln-b [roids]
  (let [[roid _] (first (rank-vantages roids))]
    (last (take 200 (vaporize roids roid)))))

(println "soln a:" (best-vantage ROIDS))
(println "soln b:" (soln-b ROIDS))













