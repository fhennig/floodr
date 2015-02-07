(ns floodr.logic
  (:require [clojure.set :as set])
  (:gen-class))

; :parents - a map node to node
; :neighbors - a map node to set of nodes
; :colors - a map node to color
(defstruct world :w :h :parents :neighbors :colors)

(def empty-world (struct world 0 0 {} {} {}))

(defn mk-world [w h]
  (struct world w h {} {} {}))

(defn add-node
  [w node node-parent node-neighbors node-color]
  (struct world 
          (get w :w) (get w :h)
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
            (get w :w) (get w :h)
            (assoc (get w :parents) c1 c2) ; c1 merged into c2
            (dissoc (assoc (get w :neighbors) c2
                                 (set/difference (set/union (neighbors w c1)
                                                            (neighbors w c2))
                                                 #{c1 c2}))
                       c1)
            (dissoc (get w :colors) c1))))

(defn color
  "gets the color of a cluster"
  [w node]
  (get (get w :colors) (cluster w node)))

(defn merge-candidates [w cluster]
  (let [c-col (color w cluster)
        c-ns (neighbors w cluster)]
    (filter #(= (color w %) c-col) c-ns)))

(defn merge-neighbors [w c]
    (reduce #(merge-clusters %1 (cluster %1 c) %2)
          w (merge-candidates w c)))

(defn colorize [w clust color]
  (let [c (cluster w clust)
        nw (struct world (get w :w) (get w :h) (get w :parents)
                   (get w :neighbors) (assoc-in (get w :colors) [c] color))]
    (merge-neighbors nw c)))

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
        init-world (mk-world width height)
        add (fn [world node]
              (add-node world node node
                        (gen-neighbors width height node)
                        (rand-nth colors)))
        w1 (reduce add init-world nodes)
        w2 (reduce #(colorize %1 %2 (color %1 %2)) w1 nodes)]
    w2))
