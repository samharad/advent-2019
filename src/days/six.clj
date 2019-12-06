(ns days.six
  (:require [clojure.string :refer [split-lines split]]))

(def orbits
  (->> (slurp "input/six.txt")
       (split-lines)
       (map #(split % #"\)"))))

;;;; Tree
;;; Tree is [node child-tree-1 child-tree-2 ...]

(defn leaves [tree]
  (if (= (count tree) 1)
    [(first tree)]
    (flatten (map leaves (rest tree)))))

(defn insert-edge
  "Assumes the parent is already in the tree."
  [[node & children :as tree] [parent child :as edge]]
  (cond
    (= node parent) (->> children
                         (cons [child])
                         (cons node))
    (nil? children) tree
    :else (cons node (map #(insert-edge % edge) children))))

(defn build-tree [edges root]
  (loop [tree [root]
         edges edges]
    (if (empty? edges)
      tree
      (let [ls (leaves tree)
            to-add (filter (fn [[parent child]]
                             (some #{parent} ls))
                           edges)
            un-added (clojure.set/difference (set edges) (set to-add))
            t (reduce #(insert-edge %1 %2) tree to-add)]
        (recur t un-added)))))

(defn tree-has-node [[node & children] target]
  (cond
    (= node target) true
    (nil? children) false
    :else (some identity (map #(tree-has-node % target) children))))

(defn common-ancestor [[node & children :as tree] a b]
  (cond
    (nil? children) false

    (and (tree-has-node tree a)
         (tree-has-node tree b))
    (or (some identity (map #(common-ancestor % a b) children))
        tree)

    :else false))

(defn tag-with-depths
  "Each node becomes [node depth]"
  ([tree] (tag-with-depths tree 0))
  ([[node & children] depth]
   (if (nil? children)
     [[node depth]]
     (cons [node depth] (map #(tag-with-depths % (inc depth)) children)))))

(defn depth-sum [tree]
  (let [ds (->> (tag-with-depths tree)
                (flatten)
                (rest)
                (take-nth 2))]
    (apply + ds)))

(def solution-b
  (let [ls (as-> (build-tree orbits "COM") t
                 (common-ancestor t "YOU" "SAN")
                 (tag-with-depths t)
                 (leaves t))
        pairs (partition 2 ls)
        get-depth #(some (fn [[name depth]]
                           (when (= name %) depth))
                         pairs)
        you-depth (get-depth "YOU")
        san-depth (get-depth "SAN")]
    (println (+ you-depth san-depth -2))))



