(ns floodr.core
  (:require [lanterna.screen :as s]
            [floodr.logic :as l]
            [floodr.lanterna-window :as w])
  (:gen-class))

;;; util

(defn floor [x]
  (int (Math/floor x)))

;;; Constants

(def current-world)
(def user-cluster 0)

;;; screen

(def scr (ref nil))

(defn set-scr [s] (dosync (ref-set scr s)))
(defn get-scr [] @scr)

;;; drawing

(defn all-black []
  (let [[w h] (s/get-size (get-scr))]
    (doseq [y (range h)]
      (doseq [x (range w)]
        (s/put-string (get-scr) x y " ")))))

(defn put-ln [line str]
  (if (or (pos? line) (= 0 line))
    (s/put-string (get-scr) 2 line str)
    (let [[_ h] (s/get-size (get-scr))]
      (s/put-string (get-scr) 2 (+ h line) str))))

(defn draw [world]
  (let [[w h] [(get world :w) (get world :h)]
        [off-x off-y] [2 2]
        color (fn [n] (l/color world n))]
    (doseq [i (range (* w h))]
      (s/put-string (get-scr) (+ (* 2 (mod i w)) off-x) (+ (quot i w) off-y)
                    "  " {:bg (color i)}))))

(defn stats [world]
  (let [title "floodr"
        help "'h' for help"
        gen (str "generation: " (l/generation world))
        cs-left (str "blobs left: " (- (count (l/clusters world)) 1))]
    (put-ln 0 (reduce #(str %1 " - " %2) (list title help gen cs-left)))))

(defn win []
  (w/show-window (get-scr)
                 ["good job!"
                  " "
                  "[q - quit] [s - new game]"] :centered))

(defn redraw [world]
  (all-black)
  (stats world)
  (draw world)
  (when (l/won? world) (win))
  (s/redraw (get-scr)))
    
    

(defn quit []
  (s/stop (get-scr))
  (System/exit 0))

(defn new-world []
  (let [[w h] (s/get-size (get-scr))]
    (l/gen-rand-world (floor (/ (- w 4) 2))
                      (- h 3))))

(defn show-help []
  (w/show-window (get-scr)
                 ["controls:"
                  " "
                  "  q - quit"
                  "  h - show this help"
                  "  s - start new game"
                  " "
                  "  r - red"
                  "  g - green"
                  "  b - blue"
                  "  c - cyan"
                  "  v - violet"
                  "  y - yellow"
                  " " " "
                  "press any key to continue"])
  (s/get-key-blocking (get-scr)))

(defn show-debug [w]
  (w/show-window (get-scr)
                 ["debug window"
                  ""
                  (str "r: " (l/adjacent-nodes w user-cluster :red))
                  (str "g: " (l/adjacent-nodes w user-cluster :green))
                  (str "b: " (l/adjacent-nodes w user-cluster :blue))
                  (str "y: " (l/adjacent-nodes w user-cluster :yellow))
                  (str "c: " (l/adjacent-nodes w user-cluster :cyan))
                  (str "v: " (l/adjacent-nodes w user-cluster :magenta))])
  (s/get-key-blocking (get-scr)))

(defn auto-solve [w]
  (if (l/won? w) w
      (do (redraw w)
;          (show-debug w)
          (Thread/sleep 30)
          (let [color (reduce #(if (> (l/adjacent-nodes w user-cluster %2)
                                      (l/adjacent-nodes w user-cluster %1)) %2 %1)
                              (apply list (set (map #(l/color w %) (l/neighbors w user-cluster)))))]
          (recur (l/colorize w user-cluster color))))))

(defn handle-input [world]
  (redraw world)
  (let [key (s/get-key-blocking (get-scr))]
    (case key
      \q (recur (quit))
      \s (recur (new-world))
      \h (do (show-help) (recur world))
      \? (do (show-help) (recur world))
      \d (do (show-debug world) (recur world))
      \a (recur (auto-solve world))
      \r (recur (l/colorize world user-cluster :red))
      \g (recur (l/colorize world user-cluster :green))
      \b (recur (l/colorize world user-cluster :blue))
      \y (recur (l/colorize world user-cluster :yellow))
      \c (recur (l/colorize world user-cluster :cyan))
      \v (recur (l/colorize world user-cluster :magenta)) ; violet
      (recur world))))


(defn handle-args [args]
  (let [as (set args)]
    (if (contains? as "--swing")
      (set-scr (s/get-screen :swing))
      (set-scr (s/get-screen :unix)))))



(defn -main
  "Nothing to see here."
  [& args]
  (handle-args args)
  (s/start (get-scr))
  (s/get-key (get-scr)) ; workaround for tiling managers
  (redraw (new-world))
  (let [w (new-world)]
    ; handle command line parameters
    (handle-input w)))
