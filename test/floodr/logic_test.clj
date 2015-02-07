(ns floodr.logic-test
  (:require [clojure.test :refer :all]
            [floodr.logic :refer :all]))

(deftest everything-syntactically-correct
  (let [w (gen-rand-world 3 3)]
    (mk-world 1 1)
    (add-node w 5 5 #{} :red)
    (cluster w 1)
    (neighbors w 1)
    (when (not (= (cluster w 0) (cluster w 8)))
               (merge-clusters w 0 8))
    (color w 1)
    (merge-candidates w 1)
    (merge-neighbors w 1)
    (colorize w 1 :red)
    (adjacent? w 1 2)))

(deftest test-add-node
  (is (= (add-node empty-world 1 2 #{3 4} :red)
         (struct world 0 0 {1 2} {1 #{3 4}} {1 :red}))))

(def tw (struct world 0 0
                {8 7, 7 7, 6 6, 5 5, 4 3, 3 1, 2 2, 1 1, 0 0}
                {7 #{1 6 5}, 6 #{7 3}, 5 #{4 2 8}, 2 #{1 5}, 1 #{0 7 6 2 5}, 0 #{1 3}}
                {7 :red, 6 :yellow, 5 :blue, 2 :magenta, 1 :green, 0 :cyan}))
; clusters: (1 3 4) (7 8) singles ...

(deftest test-add-node
  (let [w1 (add-node tw 9 9 #{7 8} :green)
        w2 (add-node w1 -1 0 #{1} :cyan)]
    (is (= (neighbors w2 -1) (neighbors w2 0)))))

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

(deftest test-merge-clusters 
  (let [w (merge-clusters tw 1 8)
        c #(cluster w %)
        n #(neighbors w %)]
    (is (= (c 1) (c 3) (c 4) (c 7) (c 8)))
    (is (= (n 1) (n 3) (n 4) (n 7) (n 8)))))

(defn lazy-cont? [coll key]
  (some #(= key %) coll))

(deftest test-merge-candidates
  (let [w (add-node tw -1 -1 #{0 3} :green)]
    (is (lazy-cont? (merge-candidates w -1) (cluster w 1)))
    (is (lazy-cont? (merge-candidates w -1) (cluster w 3)))
    (is (lazy-cont? (merge-candidates w -1) (cluster w 4)))))

(deftest test-merge-neighbors
  (let [w1 (add-node tw -1 -1 #{0 6} :cyan)
        w2 (merge-neighbors w1 -1)]
    (is (= (cluster w2 -1) (cluster w2 0)))
    (is (= (neighbors w2 -1) (neighbors w2 0)))
    (is (contains? (neighbors w2 0) 6))))
    
(deftest test-colorize
  (let [w1 (colorize tw 0 :green)
        w2 (colorize w1 0 :yellow)]
    (is (= (cluster w2 0) (cluster w2 6) (cluster w2 3)))
    (is (contains? (neighbors w2 0) (cluster w2 7)))
    (is (contains? (neighbors w2 0) (cluster w2 8)))
    (is (contains? (neighbors w2 0) (cluster w2 2)))
    (is (contains? (neighbors w2 0) (cluster w2 5)))))

