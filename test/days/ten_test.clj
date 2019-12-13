(ns days.ten-test
  (:require [clojure.test :refer :all])
  (:require [days.ten :refer [colinear? str->roids visible-from best-vantage rank-vantages]]))

(deftest colinear?-test
  (is (colinear? [1 1] [2 2] [3 3]))
  (is (not (colinear? [1 1] [2 2] [3 1]))))


(def test-roids-35 (str->roids "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."))
(def test-roids-41 (str->roids ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."))

(deftest visible-from-test
  (let [vf (visible-from test-roids-35 [1 2])]
    (println vf)
    (is (= (count test-roids-35) 40))
    (is (and (not (some #{[1 5]} vf))
             (some #{[1 5]} test-roids-35)))
    (is (and (not (some #{[1 2]} vf))
             (some #{[1 2]} test-roids-35)))
    (is (and (not (some #{[1 9]} vf))
             (some #{[1 9]} test-roids-35)))
    (is (and (not (some #{[7 5]} vf))
             (some #{[7 5]} test-roids-35)))
    (is (and (not (some #{[7 6]} vf))
             (some #{[7 6]} test-roids-35)))
    (is (every? (fn [visible-roid] (some #{visible-roid} test-roids-35)) vf))
    ; Every solution roid is indeed a test roid
    (is (= [] (filter #(not (some #{%} test-roids-35))
                      vf)))
    (is (= (count vf) 35))))

(deftest best-vantage-roid-test
  (let [[r35 v35] (rank-vantages test-roids-35)]
    (is (= [1 2] r35))))

