(ns floodr.core
  (:require [lanterna.screen :as s]
            [floodr.logic :as l])
  (:gen-class))

;;; Constants

(def current-world)

(def scr (s/get-screen :unix))

(defn put-random-blocks [screen startx starty width height]
  (doseq [y (range height)]
    (doseq [x (range 0 width 2)]
      (s/put-string screen (+ startx x) (+ starty y)
                    "  " {:bg (rand-nth l/colors)}))))

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
      (s/put-string scr (+ (mod i w) off-x) (+ (quot i w) off-y)
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
  (let [[w h] (s/get-size scr)]
    (l/gen-rand-world (- w 4) (- h 4))))

(defn handle-input [world]
  (redraw world)
  (let [key (s/get-key-blocking scr)]
    (case key
      \q (recur (quit))
      \r (recur (new-world))
      (recur world))))

(defn -main
  "Nothing to see here."
  [& args]
  (s/start scr)
  (handle-input (new-world)))
