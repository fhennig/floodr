(ns floodr.core
  (:require [lanterna.screen :as s]
            [floodr.logic :as l])
  (:gen-class))

;;; Constants

(def current-world)
(def user-cluster 0)

(def scr (s/get-screen :unix))

(defn all-black []
  (let [[w h] (s/get-size scr)]
    (doseq [y (range h)]
      (doseq [x (range w)]
        (s/put-string scr x y " ")))))

(defn draw [world]
  (let [[w h] [(get world :w) (get world :h)]
        [off-x off-y] [2 2]
        color (fn [n] (l/color world n))]
    (doseq [i (range (* w h))]
      (s/put-string scr (+ (* 2 (mod i w)) off-x) (+ (quot i w) off-y)
                    "  " {:bg (color i)}))))

(defn redraw [world]
  (all-black)
  (s/put-string scr 0 0 "floodr")
  (draw world)
  (s/redraw scr))



(defn quit []
  (s/stop scr)
  (System/exit 0))

(defn new-world []
;  (let [[w h] [78 10]]
  (let [[w h] (s/get-size scr)]
    (l/gen-rand-world (int (Math/floor (/ (- w 4) 2)))
                      (- h 3))))

(defn handle-input [world]
  (redraw world)
  (let [key (s/get-key-blocking scr)]
    (case key
      \q (recur (quit))
      \s (recur (new-world))
      \r (recur (l/colorize world user-cluster :red))
      \g (recur (l/colorize world user-cluster :green))
      \b (recur (l/colorize world user-cluster :blue))
      \y (recur (l/colorize world user-cluster :yellow))
      \c (recur (l/colorize world user-cluster :cyan))
      \v (recur (l/colorize world user-cluster :magenta)) ; violet
      (recur world))))

(defn -main
  "Nothing to see here."
  [& args]
  (s/start scr)
  (s/get-key scr) ; workaround for tiling managers
  (let [w (new-world)]
    ; handle command line parameters
    (redraw w)
    (handle-input (new-world))))
