(ns floodr.logic
  (:require [clojure.set :as set])
  (:gen-class))

(defstruct node :x :y :parent)

(defstruct cluster :color :nodes)

(defn adjacent?
  "Checks if two nodes are next to each other"
  [p1 p2]
  (let [p1x (get p1 :x) p1y (get p1 :y)
        p2x (get p2 :x) p2y (get p2 :y)]
    (or (and (= p1x p2x)
             (or (= p1y (+ p2y 1))
                 (= p1y (- p2y 1))))
        (and (= p1y p2y)        
             (or (= p1x (+ p2x 1))
                 (= p1x (- p2x 1)))))))

(defn sets-adjacent?
  "Checks if two sets 'touch'"
  [set1 set2]
  (some (fn [node]
          (some #(adjacent? node %) set1))
        set2))

(defn same-color? [cluster1 cluster2]
  (= (get cluster1 :color)
     (get cluster2 :color)))

(defn mergeable? [cluster1 cluster2]
  (and (same-color? cluster1 cluster2)
       (sets-adjacent? (get cluster1 :nodes)
                       (get cluster2 :nodes))))

(defn merge-all [cs]
  (when (> (count cs) 1)
    (let [c1 (first cs) c2 (second cs)]
    (if (mergeable? c1 c2)
      (recur (cons (struct cluster (get c1 :color)
                           (set/union (get c1 :nodes)
                                      (get c2 :nodes)))
                   (rest (rest cs))))
      (recur (rest cs))))))
