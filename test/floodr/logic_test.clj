(ns floodr.logic-test
  (:require [clojure.test :refer :all]
            [floodr.logic :refer :all]))

(deftest everything-syntactically-correct
  (let [w (gen-rand-world 3 3)]
    (cluster w 1)
    (neighbors w 1)
    (merge-clusters w 1 2)
    (color w 1)
    (adjacent? w 1 2)))

(deftest test-add-node
  (is (= (add-node empty-world 1 2 #{3 4} :red)
         (struct world {1 2} {1 #{3 4}} {1 :red}))))

(def tw (struct world
                {8 7, 7 7, 6 6, 5 5, 4 3, 3 1, 2 2, 1 1, 0 0}
                {8 #{7 5}, 7 #{1 6 5}, 6 #{7 3}, 5 #{4 2 8}, 4 #{7 1 3 5}, 3 #{0 7 1 6 5}, 2 #{1 5}, 1 #{0 7 6 2 5}, 0 #{1 3}}
                {8 :green, 7 :red, 6 :cyan, 5 :green, 4 :cyan, 3 :magenta, 2 :blue, 1 :green, 0 :cyan}))
; clusters: (1 3 4) (7 8) singles ...

(deftest test-cluster
  (is (= (cluster tw 1) (cluster tw 1)))
  (is (= (cluster tw 3) (cluster tw 1)))
  (is (= (cluster tw 4) (cluster tw 1)))
  (is (= (cluster tw 7) (cluster tw 8)))
  (is (= (cluster tw 8) (cluster tw 8))))

(deftest test-neighbors
  (is (contains? (neighbors tw 4) (cluster tw 0)))
  (is (contains? (neighbors tw 4) (cluster tw 2)))
  (is (contains? (neighbors tw 4) (cluster tw 5)))
  (is (contains? (neighbors tw 4) (cluster tw 6)))
  (is (contains? (neighbors tw 4) (cluster tw 7)))
  (is (contains? (neighbors tw 4) (cluster tw 8)))
  (is (contains? (neighbors tw 0) (cluster tw 1))))

