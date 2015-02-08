(ns floodr.solvers
  (:require [floodr.logic :as l]))



(defn greedy-solve [w cluster]
  (if (l/won? w) w
      (do (let [color (reduce #(if (> (l/adjacent-nodes w cluster %2)
                                      (l/adjacent-nodes w cluster %1)) %2 %1)
                              (l/adjacent-colors w cluster))]
            (recur (l/colorize w cluster color) cluster)))))

           
