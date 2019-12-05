(ns days.four)

(def min-pass 109165)
(def max-pass 576723)

(defn digits
  "Egregious!"
  [n]
  (->> n
       (String/valueOf)
       (seq)
       (map #(Integer/valueOf (str %)))))

(defn repetitions [xs]
  (partition-by identity xs))

(defn has-repetition-satisfying? [xs pred]
  (->> (repetitions xs)
       (some pred)))

(defn has-2-peat [digs]
  (has-repetition-satisfying? digs #(>= (count %) 2)))

(defn has-exact-2-peat [digs]
  (has-repetition-satisfying? digs #(= (count %) 2)))

(defn num-pass-candidates [min max & ps]
  (->> (range min (inc max))
       (map digits)
       (filter #(apply <= %))
       (filter (apply every-pred ps))
       (count)))

(def solution-a
  (num-pass-candidates
    min-pass
    max-pass
    has-2-peat))

(def solution-b
  (num-pass-candidates
    min-pass
    max-pass
    has-exact-2-peat))

(println solution-a)
(println solution-b)

