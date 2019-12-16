(ns days.eleven
  (:require [days.intcode-computer :as ic]
            [clojure.core.async :refer [chan go close! >! >!! <! <!!]]
            [quil.core :as q]))

(def init-state {:coord [0 0]
                 :direction :north
                 :hull {}})

(def right
  (let [ds [:north :east :south :west]]
    (into {} (map vec (take 4 (partition 2 1 (cycle ds)))))))

(def left (clojure.set/map-invert right))

(defn turned [direction turn]
  (if (zero? turn) (left direction) (right direction)))

(defn turn [{:keys [direction] :as state} t]
  (assoc state :direction (turned direction t)))

(def move-vec {:north [0 1]
               :east  [1 0]
               :south [0 -1]
               :west  [-1 0]})

(defn move [{:keys [direction coord] :as state}]
  (let [coord' (map + coord (move-vec direction))]
    (assoc state :coord coord')))

(defn turn-and-move [state t]
  (move (turn state t)))

(defn paint [{:keys [hull coord] :as state} color]
  (let [hull' (assoc hull coord color)]
    (assoc state :hull hull')))

(def draw-chan (chan))
(defn draw []
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (let [[[x y] color] (<!! draw-chan)]
      (if (= color 1)
        (q/ellipse (* x 10) (* y 10) 5 5)))))
(defn setup []
  (q/frame-rate 20)
  (q/background 255))

(q/defsketch hull-sketch
  :size [1000 600]
  :setup setup
  :draw draw)

(defn paint-hull [mem start-color]
  (let [in-chan (chan)
        out-chan (chan)
        prog (ic/make-prog mem in-chan out-chan)
        return-chan (go (loop [state init-state]
                          (let [color (<! out-chan)
                                t (<! out-chan)
                                painted (paint state color)
                                {:keys [hull coord] :as state'} (-> painted
                                                                    (turn-and-move t))]
                            (>! draw-chan [(:coord state) color])
                            (println coord)
                            (if (>! in-chan (get hull coord 0))
                              (recur state')
                              state'))))]
    (go (ic/run-program prog))
    (>!! in-chan start-color)
    (<!! return-chan)))


(def MEM (ic/str->mem (slurp "input/eleven.txt")))
(println "soln a" (count (keys (:hull (paint-hull MEM 0)))))
;(paint-hull MEM 1)


