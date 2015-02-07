(ns floodr.lanterna-window
  (:require [lanterna.screen :as s])
  (:gen-class))

(defn floor [x]
  (int (Math/floor x)))

(defn space [n] (apply str (repeat n " ")))

(defn put-lines [screen width lines]
  (let [height (count lines)
        [w h] (s/get-size screen)
        [b-l b-b b-t b-r] [3 1 1 3]
        box-w (+ b-l b-r width)
        box-h (+ b-b b-t height)
        empty-ln (space width)
        x (floor (- (/ w 2) (/ box-w 2)))
        y (floor (- (/ h 2) (/ box-h 2)))
        put-ln (fn [line string] 
                 (s/put-string screen x line (str (space b-l) string (space b-r))))]
    (doseq [i (range y (+ y b-t))]
      (put-ln i empty-ln))
    (doseq [i (range (+ y b-t) (+ y b-t height))]
      (put-ln i (nth lines (- i y b-t))))
    (doseq [i (range (+ y b-t height) (+ y b-t height b-b))]
      (put-ln i empty-ln))))

(defn pad [s len & [centered]]
  (let [pad-len (- len (count s))
        pad-f (if centered (space (floor (/ pad-len 2))) "")
        pad-b (space (- pad-len (count pad-f)))]
      (str pad-f s pad-b)))
    
(defn show-window
  "displays a window, centered in the screen containing the given lines.
  if 'centered' is non-nil, the text in the window will be centered"
  [screen lines & [centered]]
  (let [line-width (apply max (map count lines))
        padded-lines (map #(pad % line-width centered) lines)]
    (put-lines screen line-width padded-lines)
    (s/redraw screen)))
