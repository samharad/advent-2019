(ns days.four)

(def min-pass 109165)
(def max-pass 576723)

(defn digits [n]
  (letfn [(digits [n]
            (if (= n 0)
              []
              (conj (digits (quot n 10))
                    (rem n 10))))]
    (if (= n 0)
      [0]
      (digits n))))

(defn repetitions [xs]
  (partition-by identity xs))

(defn some-repetition? [pred xs]
  (->> (repetitions xs)
       (some pred)))

(defn num-pass-candidates [& ps]
  (->> (range min-pass (inc max-pass))
       (map digits)
       (filter #(apply <= %))
       (filter (apply every-pred ps))
       (count)))

(defn has-2-peat [xs]
  (some-repetition? #(>= (count %) 2) xs))

(defn has-exact-2-peat [xs]
  (some-repetition? #(= (count %) 2) xs))

(def solution-a
  (time (num-pass-candidates has-2-peat)))

(def solution-b
  (time (num-pass-candidates has-exact-2-peat)))

(println solution-a)
(println solution-b)

