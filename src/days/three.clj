(ns days.three
  (:require [clojure.string :refer [split-lines split]]))

(def input (slurp "input/three.txt"))

(defn next-coord [[r c] move-str]
  (let [dir (first move-str)
        mag (Integer. (subs move-str 1))]
    (case dir
      \U [r (- c mag)]
      \D [r (+ c mag)]
      \R [(+ r mag) c]
      \L [(- r mag) c])))

(defn moves->coords
  "Moves of the shape ['U21', 'D23']"
  [moves]
  (reverse (reduce (fn [[coord & _ :as acc] move]
                     (cons (next-coord coord move) acc))
                   '([0 0])
                   moves)))

(defn wire-str->segments [s]
  (->> (split s #",")
       (moves->coords)
       (partition 2 1)
       (map sort)))

(defn str->wires
  "Returns a list of wires, where each wire is a list of segments."
  [s]
  (->> s
       (split-lines)
       (map wire-str->segments)))

(defn vertical? [[[_ a-col] [_ b-col]]]
  (= a-col b-col))

(def vertical-first (complement vertical?))

(defn segment-intersection [a b]
  #_(let [[sega segb] (sort-by vertical? [a b])])
  #_(let [[[a-start a-end] [b-start b-end]] (sort-by vertical? [a b])])
  (let [[[[v1r v1c] [v2r v2c]] [[h1r h1c] [h2r h2c]] :as all] (sort-by vertical-first [a b])]
    (if (and (<= v1r h1r v2r)
             (<= h1c v1c h2c))
      [h1r v1c])))

(defn intersections [wire-a wire-b]
  (->> (map (fn [a-segment]
              (->> wire-b
                   (map #(segment-intersection a-segment %))
                   (filter identity)))
            wire-a)
       (mapcat identity)))

(defn manhattan-dist [[ar ac] [br bc]]
  (+ (Math/abs (- ac bc))
     (Math/abs (- ar br))))

(defn steps-to-coord
  ([wire-a wire-b coord] (+ (steps-to-coord wire-a coord)
                            (steps-to-coord wire-b coord)))
  ([wire coord]
   (loop [[head & more-wire] wire
          steps 0]
     (let [intersect (segment-intersection head [coord coord])]
       (if intersect
         (+ steps (manhattan-dist (first head) intersect))
         (recur more-wire (+ steps (apply manhattan-dist head))))))))

(defn- solution [dist-f]
  (let [[wire-a wire-b] (str->wires input)]
    (->> (intersections wire-a wire-b)
         (map dist-f)
         (sort)
         (first))))

(defn solution-a []
  (solution #(manhattan-dist [0 0] %)))

(defn solution-b []
  (let [[wire-a wire-b] (str->wires input)]
    (solution #(steps-to-coord wire-a wire-b %))))

(println (solution-a))
(println (solution-b))



