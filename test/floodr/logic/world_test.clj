(ns floodr.logic.world-test
  (:require [clojure.test :refer :all]
            [floodr.test-data :as t]
            [floodr.util :refer :all]
            [floodr.logic.world :refer :all]))

(def tnodes t/test-nodes1)
(def tw t/test-world1)

(deftest test-cluster
  (is (= (cluster tw 1) (cluster tw 1)))
  (is (= (cluster tw 3) (cluster tw 1)))
  (is (= (cluster tw 4) (cluster tw 1)))
  (is (= (cluster tw 7) (cluster tw 8)))
  (is (= (cluster tw 8) (cluster tw 8))))

(deftest test-clusters
  (is (= (apply clusters tw tnodes) (:clusters tw))))

(deftest test-color
  (is (= (color tw 7) (color tw 8) :red))
  (is (has-color? tw 7 :red))
  (is (has-color? tw 8 :red))
  (is (has-color? tw 0 :cyan))
  (is (not (has-color? tw 0 :red))))

(deftest test-size
  (is (= 3 (size tw 1)))
  (is (= 4 (size tw 1 0)))
  (is (= 4 (size tw 0 1 3 4)))
  (is (= 2 (size tw 8)))
  (is (= 3 (apply size tw (neighbors tw 0)))))

(deftest test-neighbors
  (is (= 1 (count (neighbors tw 0))))
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
    (is (= (size w 1) 5))
    (is (= (c 1) (c 3) (c 4) (c 7) (c 8)))
    (is (= (n 1) (n 3) (n 4) (n 7) (n 8))))
  (let [w (merge-clusters tw 0 1 2 3 4 5 6 7 8)
        c #(cluster w %)                                    
        n #(neighbors w %)]
    (is (= (size w 1) 9))
    (is (= (c 1) (c 2) (c 5) (c 6) (c 7)))))
