(ns floodr.logic
  (:require [clojure.set :as set])
  (:gen-class))

;;; util

(defn change-state
  [f map & to-update]
  (reduce (fn [state [kw & actions]]
            (apply f state [kw] actions))
          map to-update))

(def update-vals #(apply change-state update-in %&))
(def set-vals #(apply change-state assoc-in %&))

;;; world struct

(def empty-world
  {:w 0 :h 0 ; used within the UI
   :generation 0 ; how many colorize operations were done on this world
   :clusters #{} ; the clusters that exist
   :parents {} ; maps nodes to their parents; used to get the cluster of a node
   :neighbors {} ; what neighbors does a cluster have
   :colors {} ; what color does a cluster have
   :sizes {}}) ; size of the clusters

(defn won? [world]
  (= 1 (count (:clusters world))))

;;; functions that operate on nodes

(defn parent
  [w n]
  (get (:parents w) n))

(defn cluster 
  "returns the cluster that the node is a member of"
  [world node]
  (let [p (parent world node)]
    (if (= node p) node
      (recur world p))))

(defn path
  "mainly used for debugging, returns the path that the node takes
  to determine its cluster"
  [w n]
  (let [p (parent w n)]
    (if (= n p) '()
        (cons p (path w p)))))
        
;;; functions that operate on clusters

(defn color
  "gets the color of a cluster"
  [w node]
  (get (:colors w) (cluster w node)))

(defn size
  "gets the size of a cluster"
  [w node]
  (get (:sizes w) (cluster w node)))

(defn neighbors 
  "returns the neighbor clusters of the cluster"
  [world node]
  (set (map #(cluster world %)
            (get (:neighbors world) (cluster world node)))))

(defn mergeable-neighbors
  "returns the clusters that are adjacent and of the same color"
  [w cluster]
  (let [c-col (color w cluster)
        c-ns (neighbors w cluster)]
    (filter #(= (color w %) c-col) c-ns)))
  
(defn merge-clusters
  "returns a new world where c1 and c2 are merged"
  [w n1 n2]
  (let [c1 (cluster w n1) c2 (cluster w n2)
        cs (if (> (size w c1) (size w c2)) c2 c1) ; smaller cluster
        cb (if (> (size w c1) (size w c2)) c1 c2)] ; bigger cluster
    (update-vals w [:clusters disj cs]
                 [:parents assoc cs cb]
                 [:neighbors assoc cb (set/union (disj (neighbors w cs) cb)
                                                 (disj (neighbors w cb) cs))]
                 [:neighbors dissoc cs]
                 [:colors dissoc cs]
                 [:sizes assoc cb (+ (size w cs) (size w cb))]
                 [:sizes dissoc cs])))

(defn merge-neighbors
  "merges all mergeable neighbors"
  [w c]
  (reduce #(merge-clusters %1 (cluster %1 c) %2)
          w (mergeable-neighbors w c)))

(defn colorize
  "changes the color of the given cluster and merges its neighbors"
  [w clust color]
  (let [c (cluster w clust)
        nw (update-vals w [:generation inc]
                        [:colors assoc c color])]
    (merge-neighbors nw c)))

;;; World generation

(defn add-node
  [w node node-ns node-col]
  (update-vals w [:clusters conj node]
               [:parents assoc node node]
               [:neighbors assoc node node-ns]
               [:colors assoc node node-col]
               [:sizes assoc node 1]))

(defn index->coords
  [w i]
  [(rem i w) (quot i w)])

(defn coords->index
  [w [x y]]
  (+ (* y w) x))

(defn in-bounds?
  [w h [x y]]
  (and (<= 0 x) (< x w)
       (<= 0 y) (< y h)))

(defn gen-neighbors 
  [w h i neighborhood-fn]
  (let [[x y] (index->coords w i)]
    (set (map #(coords->index w %)
         (filter #(in-bounds? w h %)
                 (neighborhood-fn x y))))))

(defn neighbors-4
  [x y]
  (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]))

(defn neighbors-8
  [x y]
  (list [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)] [(dec x) y]
        [(inc x) y] [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]))

(def colors '(:red :green :blue :yellow :cyan :magenta))

(defn gen-rand-world
  [width height]
  (let [nodes (range (* width height))
        init-world (set-vals empty-world
                             [:w width]
                             [:h height]
                             [:lusters (set nodes)])
        add (fn [world node]
              (add-node world node
                        (gen-neighbors width height node neighbors-4)
                        (rand-nth colors)))
        w1 (reduce add init-world nodes)
        w2 (reduce #(colorize %1 %2 (color %1 %2)) w1 nodes)
        w3 (set-vals w2 [:generation 0])]
    w3))

;;; utilities

(defn adjacent-nodes 
  "amout of nodes with color adjacent to the given cluster"
  [w cluster col]
  (apply + (map #(size w %) (filter #(= col (color w %)) (neighbors w cluster)))))

(defn adjacent-colors
  "returns the colors of which there are adjacent clusters"
  [w cluster]
  (distinct (map #(color w %) (neighbors w cluster))))
