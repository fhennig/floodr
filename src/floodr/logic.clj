(ns floodr.logic
  (:require [clojure.set :as set])
  (:gen-class))

; :parents - a map node to node
; :neighbors - a map node to set of nodes
; :colors - a map node to color
(defstruct world :parents :neighbors :colors)

(def empty-world (struct world {} {} {}))

(defn add-node
  [w node node-parent node-neighbors node-color]
  (struct world
          (assoc-in (get w :parents) [node] node-parent)
          (assoc-in (get w :neighbors) [node] node-neighbors)
          (assoc-in (get w :colors) [node] node-color)))



(defn cluster 
  "returns the cluster that the node is a member of"
  [world node]
  (let [p (get (get world :parents) node)]
    (if (= node p)
      node
      (cluster world p))))

(defn neighbors 
  "returns the neighbor clusters of the cluster"
  [world node]
  (set (map #(cluster world %)
            (get (get world :neighbors) (cluster world node)))))
  
(defn merge-clusters
  "returns a new world where c1 and c2 are merged"
  [w n1 n2]
  (let [c1 (cluster w n1) c2 (cluster w n2)]
    (struct world
            (assoc-in (get w :parents) [c1] c2)
            (assoc-in (get w :neighbors) [c2]
                      (set/difference (set/union (neighbors w c1) (neighbors w c2)) #{c1 c2}))
            (get w :colors))))

(defn color
  "gets the color of a cluster"
  [w node]
  (get (get w :colors) (cluster w node)))

(defn adjacent?
  "determines if two clusters are next to each other"
  [w n1 n2]
  (contains? (neighbors w (cluster w n1)) (cluster w n2)))

(defn gen-neighbors
  [w h node]
  (let [up (- node w) down (+ node w)
        left (- node 1) right (+ node 1)]
    (set/union (when (>= up 0) #{up})
               (when (< down (* w h)) #{down})
               (when (and (= (quot node w) (quot left w))
                          (>= left 0)) #{left})
               (when (and (= (quot node w) (quot right w))
                          (< right (* w h))) #{right}))))

(def colors '(:red :green :blue :yellow :cyan :magenta))

(defn gen-rand-world
  [width height]
  (let [nodes (range (* width height))
        add (fn [world node]
              (add-node world node node
                        (gen-neighbors width height node)
                        (rand-nth colors)))]
    (reduce add empty-world nodes)))
