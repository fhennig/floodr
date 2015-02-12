(ns floodr.util)

;;; changing struct maps

(defn change-state
  [f map & to-update]
  (reduce (fn [state [kw & actions]]
            (apply f state [kw] actions))
          map to-update))

(def update-vals #(apply change-state update-in %&))
(def set-vals #(apply change-state assoc-in %&))

;;; working with grid-graphs

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

;;; math

(defn floor [x]
  (int (Math/floor x)))

;;; misc

(defn no-op [& [x]] x)
