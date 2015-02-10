(ns floodr.logic.world
  (:require [clojure.set :as set]
            [floodr.util :refer :all]))



(defn- parent
  [w n]
  (get (:parents w) n))

(defn cluster 
  "returns the cluster that the node is a member of"
  [world node]
  (let [p (parent world node)]
    (if (= node p) node
      (recur world p))))

(defn clusters
  [w & nodes]
    (set (map #(cluster w %) nodes)))

(defn- path
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

(defn has-color?
  [w c col]
  (= (color w c) col))

(defn size
  "gets the accumulated size of the given clusters, without duplicates"
  [w & nodes]
  (apply + (map #(get (:sizes w) %) 
                (apply clusters w nodes))))

(defn neighbors 
  "returns the neighbor clusters of the cluster"
  [world node]
  (set (map #(cluster world %)
            (get (:neighbors world) (cluster world node)))))

(defn- merge-clusters-h
  "returns a new world where c1 and c2 are merged"
  [w n1 n2]
  (let [c1 (cluster w n1), c2 (cluster w n2)]
    (if (= c1 c2) w
        (let [[cs cb] (sort-by #(size w %) [c1 c2])]
          (update-vals w [:neighbors assoc cb (set/union (disj (neighbors w cs) cb)
                                                         (disj (neighbors w cb) cs))]
                       [:sizes assoc cb (+ (size w cs) (size w cb))]
                       [:parents assoc cs cb]
                       [:neighbors dissoc cs]
                       [:sizes dissoc cs]
                       [:colors dissoc cs]
                       [:clusters disj cs])))))

(defn merge-clusters
  "merges any number of clusters"
  [w c1 & cr]
  (reduce #(merge-clusters-h %1 c1 %2) w cr))

;;; world definition

(def colors '(:red :green :blue :yellow :cyan :magenta))

(def empty-world
  {:w 0 :h 0 ; used within the UI
   :clusters #{} ; the clusters that exist
   :parents {} ; maps nodes to their parents; used to get the cluster of a node
   :neighbors {} ; what neighbors does a cluster have
   :colors {} ; what color does a cluster have
   :sizes {}}) ; size of the clusters

;;; World generation

(defn- add-node ; TODO make private 
  [w node node-ns node-col]
  (update-vals w [:clusters conj node]
               [:parents assoc node node]
               [:neighbors assoc node node-ns]
               [:colors assoc node node-col]
               [:sizes assoc node 1]))

(defn- gen-neighbors 
  [w h i neighborhood-fn]
  (let [[x y] (index->coords w i)]
    (set (map #(coords->index w %)
         (filter #(in-bounds? w h %)
                 (neighborhood-fn x y))))))

(defn- neighbors-4
  [x y]
  (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]))

(defn- neighbors-8
  [x y]
  (list [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)] [(dec x) y]
        [(inc x) y] [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]))

(defn- colorize
  [w c]
  (let [col (color w c)]
    (apply merge-clusters w c (filter #(has-color? w % col) (neighbors w c)))))

(defn gen-rand-world ;; TODO add parameter what neighbor function to use
  [width height]
  (let [nodes (range (* width height))
        ki (- (* width height) 1)
        init-world (set-vals empty-world
                             [:w width]
                             [:h height])
        add (fn [world node]
              (add-node world node
                        (gen-neighbors width height node neighbors-4)
                        (rand-nth colors)))
        w1 (reduce add init-world nodes)
        w2 (reduce #(colorize %1 %2) w1 nodes)]
    w2))
