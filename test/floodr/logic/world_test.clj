(ns floodr.logic.world-test
  (:require [clojure.test :refer :all]
            [floodr.test-data :as t]
            [floodr.util :refer :all]
            [floodr.logic.world :refer :all]))

(def tnodes t/test-nodes1)
(def tclusters t/test-clusters1)
(def tw t/test-world1)

(deftest test-cluster
  (is (= (cluster tw [1 0]) (cluster tw [1 0])))
  (is (= (cluster tw [0 1]) (cluster tw [1 0]) [1 0]))
  (is (= (cluster tw [1 1]) (cluster tw [1 0])))
  (is (= (cluster tw [1 2]) (cluster tw [2 2])))
  (is (= (cluster tw [2 2]) (cluster tw [2 2]))))

(deftest test-nodes
  (is (= (nodes tw) tnodes)))

(deftest test-clusters
  (is (= (clusters tw) tclusters)))

(deftest test-nodes->clusters
  (is (= (nodes->clusters tw tnodes) tclusters)))

(deftest test-color
  (is (= (color tw [1 2]) (color tw [2 2]) :red))
  (is (has-color? tw [1 2] :red))
  (is (has-color? tw [2 2] :red))
  (is (has-color? tw [0 0] :cyan))
  (is (not (has-color? tw [0 0] :red))))

(deftest test-size
  (is (= 3 (size tw [1 0])))
  (is (= 4 (size tw [1 0] [0 0])))
  (is (= 4 (size tw [0 0] [1 0] [0 1] [1 1])))
  (is (= 2 (size tw [2 2])))
  (is (= 3 (apply size tw (neighbors tw [0 0])))))

(deftest test-dist
  (is (= 0 (dist tw [1 1])))
  (is (= 0 (dist tw [0 1])))
  (is (= 0 (dist tw [1 0])))
  (is (= 1 (dist tw [2 1])))
  (is (= 1 (dist tw [1 2]))))

(deftest test-neighbors
  (is (= 1 (count (neighbors tw [0 0]))))
  (is (contains? (neighbors tw [1 1]) (cluster tw [0 0])))
  (is (contains? (neighbors tw [1 1]) (cluster tw [2 0])))
  (is (contains? (neighbors tw [1 1]) (cluster tw [2 1])))
  (is (contains? (neighbors tw [1 1]) (cluster tw [0 2])))
  (is (contains? (neighbors tw [1 1]) (cluster tw [1 2])))
  (is (contains? (neighbors tw [1 1]) (cluster tw [2 2])))
  (is (contains? (neighbors tw [0 0]) (cluster tw [1 0]))))

(deftest test-merge-clusters 
  (let [w (merge-clusters tw [1 0] [2 2])
        c #(cluster w %)                                    
        n #(neighbors w %)]
    (is (= (size w [1 0]) 5))
    (is (= (c [1 0]) (c [0 1]) (c [1 1]) (c [1 2]) (c [2 2])))
    (is (= (n [1 0]) (n [0 1]) (n [1 1]) (n [1 2]) (n [2 2]))))
  (let [w (merge-clusters tw [0 0] [1 0] [2 0] [0 1] [1 1] [2 1] [0 2] [1 2] [2 2])
        c #(cluster w %)                                    
        n #(neighbors w %)]
    (is (= (size w [1 0]) 9))
    (is (= (c [1 0]) (c [2 0]) (c [2 1]) (c [0 2]) (c [1 2])))))
