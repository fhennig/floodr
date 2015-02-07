(ns floodr.logic
  (:require [clojure.set :as set])
  (:gen-class))

;;; world struct

(defstruct world
  :w :h ; used within the UI
  :generation ; how many colorize operations were done on this world
  :clusters ; the clusters that exist
  :parents ; maps nodes to their parents; used to get the cluster of a node
  :neighbors ; what neighbors does a cluster have
  :colors) ; what color does a cluster have

(def empty-world (struct world 0 0 0 #{} {} {} {}))

(defn new-world [ow args]
  (let [w (get args :w)
        h (get args :h)
        gen (get args :generation)
        cls (get args :clusters)
        ps (get args :parents)
        ns (get args :neighbors)
        cs (get args :colors)]
    (struct world
            (if w w (get ow :w))
            (if h h (get ow :h))
            (if gen gen (get ow :generation))
            (if cls cls (get ow :clusters))
            (if ps ps (get ow :parents))
            (if ns ns (get ow :neighbors))
            (if cs cs (get ow :colors)))))

(defn generation [world]
  (get world :generation))

(defn clusters [world]
  (get world :clusters))

;;; functions that operate on nodes

(defn cluster 
  "returns the cluster that the node is a member of"
  [world node]
  (let [p (get (get world :parents) node)]
    (if (= node p)
      node
      (cluster world p))))

;;; functions that operate on clusters

(defn color
  "gets the color of a cluster"
  [w node]
  (get (get w :colors) (cluster w node)))

(defn neighbors 
  "returns the neighbor clusters of the cluster"
  [world node]
  (set (map #(cluster world %)
            (get (get world :neighbors) (cluster world node)))))

(defn mergeable-neighbors
  "returns the clusters that are adjacent and of the same color"
  [w cluster]
  (let [c-col (color w cluster)
        c-ns (neighbors w cluster)]
    (filter #(= (color w %) c-col) c-ns)))
  
(defn merge-clusters
  "returns a new world where c1 and c2 are merged"
  [w n1 n2]
  (let [c1 (cluster w n1) c2 (cluster w n2)]
    (new-world w {:clusters (disj (get w :clusters) c1)
                  :parents (assoc (get w :parents) c1 c2)
                  :neighbors (dissoc (assoc (get w :neighbors) c2
                                            (set/union (disj (neighbors w c1) c2)
                                                       (disj (neighbors w c2) c1))) c1)
                  :colors (dissoc (get w :colors) c1)})))

(defn merge-neighbors
  "merges all mergeable neighbors"
  [w c]
  (reduce #(merge-clusters %1 (cluster %1 c) %2)
          w (mergeable-neighbors w c)))

(defn colorize
  "changes the color of the given cluster and merges its neighbors"
  [w clust color]
  (let [c (cluster w clust)
        nw (new-world w {:generation (+ 1 (get w :generation))
                         :colors (assoc (get w :colors) c color)})]
    (merge-neighbors nw c)))

;;; World generation

(defn add-node
  [w node node-neighbors node-color]
  (new-world w {:clusters (conj (get w :clusters) node)
                :parents (assoc (get w :parents) node node)
                :neighbors (assoc (get w :neighbors) node node-neighbors)
                :colors (assoc (get w :colors) node node-color)}))

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
        init-world (new-world empty-world {:w width, :h height
                                           :clusters (set nodes)})
        add (fn [world node]
              (add-node world node
                        (gen-neighbors width height node)
                        (rand-nth colors)))
        w1 (reduce add init-world nodes)
        w2 (reduce #(colorize %1 %2 (color %1 %2)) w1 nodes)
        w3 (new-world w2 {:generation 0})]
    w3))
