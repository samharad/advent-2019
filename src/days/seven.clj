(ns days.seven
  (:require [clojure.string :refer [split join trim]]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :refer [chan go go-loop close! onto-chan <! <!! >! >!!]]))

(defn digits [n]
  (assert (some? n))
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

(defn pad [xs p]
  (concat xs (repeat p)))

(defn- get-arg [prog param param-mode]
  (if (zero? param-mode)
    (get prog param)
    param))

(defn- get-args-and-return
  [prog i param-modes]
  (->> (conj (vec param-modes) 1)
       (map vector (rest (drop i prog)))
       (map #(apply get-arg (cons prog %)))))

(defn computation
  "Returns [new-program new-i]"
  [program i param-modes compute-fn]
  (let [args (get-args-and-return program i param-modes)
        computed-val (apply compute-fn (butlast args))
        new-prog (assoc program (last args) computed-val)
        new-i (+ i (inc (count args)))]
    [new-prog new-i]))

(defn multiply [program i param-modes]
  (computation program i (take 2 param-modes) *))

(defn add [program i param-modes]
  (computation program i (take 2 param-modes) +))

(defn readln
  ([program i _]
   (let [c (chan)]
     (go (>! c (read-line))
         (close! c))
     (readln c program i _)))
  ([in-chan program i _]
   (computation program i [] #(Integer. (<!! in-chan)))))

(defn putln
  ([program i param-modes]
   (let [c (chan)]
     (go (println (<! c))
         (close! c))
     (putln c program i param-modes)))
  ([out-chan program i param-modes]
   (let [[x _] (get-args-and-return program i (take 1 param-modes))]
     (assert (>!! out-chan x))
     [program (+ i 2)])))

(defn jump-if-true [program i param-modes]
  (let [[x y _] (get-args-and-return program i (take 2 param-modes))]
    (if (zero? x)
      [program (+ i 3)]
      [program y])))

(defn jump-if-false [program i param-modes]
  (let [[x y _] (get-args-and-return program i (take 2 param-modes))]
    (if (zero? x)
      [program y]
      [program (+ i 3)])))

(defn less-than [program i param-modes]
  (computation program i (take 2 param-modes) #(if (< %1 %2) 1 0)))

(defn equals [program i param-modes]
  (computation program i (take 2 param-modes) #(if (= %1 %2) 1 0)))

(def opcode-handler {1 add
                     2 multiply
                     3 readln
                     4 putln
                     5 jump-if-true
                     6 jump-if-false
                     7 less-than
                     8 equals})

(defn str->prog [s]
  (->> (split s #",")
       (map #(Integer. %))
       (vec)))

(def prog (str->prog (slurp "input/seven.txt")))

(defn run-program
  ([program] (run-program program opcode-handler))
  ([program opcode-handler]
   (loop [program program
          i 0]
     (let [instr (get program i)
           digs (digits instr)
           opcode (digits->num (take-last 2 digs))
           param-modes (pad (reverse (drop-last 2 digs))
                            0)
           handler (opcode-handler opcode)]
       (if (= opcode 99)
         (get program 0)
         (let [[new-p new-i] (handler program i param-modes)]
           (recur new-p new-i)))))))

(defn run-prog-with-chans [prog in-chan out-chan]
  (let [handlers (-> opcode-handler
                     (assoc 3 (partial readln in-chan))
                     (assoc 4 (partial putln out-chan)))]
    (run-program prog handlers)))

(defn make-progs [prog phase-settings]
  (reduce (fn [cs phase-setting]
            (let [in (last cs)
                  out (chan 1)]
              (assert (>!! in phase-setting))
              (go (run-prog-with-chans prog in out)
                  (close! out))
              (conj cs out)))
          [(chan 1)]
          phase-settings))

(defn optimize [phases return-chan chain-runner]
  (apply max (map (fn [ps]
                    (chain-runner ps)
                    (<!! return-chan))
                  (combo/permutations phases))))

(defn optimize-phase-settings [prog phases]
  (let [r-chan (chan)]
    (optimize
      phases
      r-chan
      (fn [phase-settings]
        (let [progs (make-progs prog phase-settings)
              start-chan (first progs)]
          (go (let [last-out (last progs)
                    v (<! last-out)]
                (close! start-chan)
                (assert (>! r-chan v))))
          (go (assert (>! start-chan 0))))))))

(defn optimize-recursive-phase-settings [prog phases]
  (let [return-chan (chan)]
    (optimize
      phases
      return-chan
      (fn [phase-settings]
        (let [progs (make-progs prog phase-settings)
              start-chan (first progs)]
          (go-loop [last-val nil]
            (let [last-chan (last progs)
                  v (<! last-chan)]
              (if (some? v)
                (do
                  (assert (>! start-chan v))
                  (recur v))
                (assert (>! return-chan last-val)))))
          (go (>! start-chan 0)))))))

;(println "Soln A:" (optimize-phase-settings prog (range 5)))
;(println "Soln B:" (optimize-recursive-phase-settings prog (range 5 10)))
