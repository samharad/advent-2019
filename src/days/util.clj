(ns days.util)


(defn pad [xs p]
  (concat xs (repeat p)))

(defn digits [n]
  (letfn [(digits [n]
            (if (= n 0)
              []
              (conj (digits (quot n 10))
                    (rem n 10))))]
    (if (= n 0)
      [0]
      (digits n))))

(defn digits->num [digs]
  (let [places (map #(int (Math/pow 10 %)) (range))]
    (->> (reverse digs)
         (map vector places)
         (reduce #(+ %1 (apply * %2)) 0))))

(defn ?assoc
  "Credit to Alex Miller and https://stackoverflow.com/questions/16356888/assoc-if-in-clojure"
  [m & kvs]
  (->> kvs
       (partition 2)
       (filter (comp some? second))
       (map vec)
       (into m)))

