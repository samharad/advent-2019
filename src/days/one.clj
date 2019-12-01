(ns days.one
  (:require [clojure.string :refer [split-lines]]))

(defn read-input-lines [f-name]
  (split-lines (slurp f-name)))

(defn fuel-for-mass
  "Does NOT account for the mass of the fuel itself."
  [mass]
  (-> mass (/ 3) (long) (- 2)))

(defn adjust-fuel-mass
  "Adjusts a 'raw' fuel mass to account for
  the mass of the fuel itself."
  [fuel-mass]
  (if (pos? fuel-mass)
    (+ fuel-mass (adjust-fuel-mass
                   (fuel-for-mass fuel-mass)))
    0))

(defn fuel-for-mass-1
  "Accounts for the mass of the fuel itself."
  [mass]
  (adjust-fuel-mass (fuel-for-mass mass)))

(defn- solution [f-name f]
  (let [lines (read-input-lines f-name)]
    (apply + (map (comp f #(Long/valueOf %))
                  lines))))

(defn solution-a [f-name]
  (solution f-name fuel-for-mass))

(defn solution-b [f-name]
  (solution f-name fuel-for-mass-1))
