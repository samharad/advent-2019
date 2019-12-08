(ns days.eight)

(def input (slurp "input/eight.txt"))
(def width 25)
(def height 6)
(def area (* width height))
(def layers (->> (seq input)
                 (partition area)
                 (map #(partition width %))))

(defn char-count [c layer]
  (count (filter #(= c %)
                 (flatten layer))))

(def soln-a
  (let [l (first (sort-by (partial char-count \0)
                          layers))
        num-ones (char-count \1 l)
        num-twos (char-count \2 l)]
    (* num-ones num-twos)))

(defn merge-layers [fst snd]
  (map #(if (= %1 \2) %2 %1)
       fst
       snd))

(def soln-b
  (->> layers
       (map flatten)
       (reduce merge-layers)
       (map #(if (= % \0) \. \#))
       (partition width)))

(println soln-a)

(doseq [row soln-b]
  (println row))


