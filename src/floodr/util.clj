(ns floodr.util)

;;; changing struct maps

(defn change-state
  [f map & to-update]
  (reduce (fn [state [kws & actions]]
            (apply f state kws actions))
          map to-update))

(def m-update-in #(apply change-state update-in %&))
(def m-assoc-in #(apply change-state assoc-in %&))

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

;;; converting chars and digits

(defn char->digit
  "returns the digit represented by the char or nil"
  [c]
  (- (int c) 48))

(defn digit->char
  "returns the digit as a character"
  [d]
  (char (+ d 48)))

;;; misc

(defn no-op [& [x]] x)
