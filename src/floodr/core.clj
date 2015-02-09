(ns floodr.core
  (:require [lanterna.screen :as s]
            [floodr.logic :as l]
            [floodr.solvers :as solver]
            [floodr.lanterna-window :as w])
  (:gen-class))

;;; util

(defn floor [x]
  (int (Math/floor x)))

;;; Constants

(def current-world)
(def user-cluster 0)
(def ki 50)

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
  (let [[w h] [(:w world) (:h world)]
        [off-x off-y] [2 2]
        color (fn [n] (l/color world n))]
    (doseq [i (range (* w h))]
      (s/put-string (get-scr) (+ (* 2 (mod i w)) off-x) (+ (quot i w) off-y)
                    "  " {:bg (color i)}))))

(defn stats [world]
  (let [title "floodr"
        help "'h' for help"
        gen (str "generation: " (:generation world))
        cs-left (str "blobs left: " (- (count (:clusters world)) 1))]
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
  (when (l/finished? world) (win))
  (s/redraw (get-scr)))
    
    

(defn quit []
  (s/stop (get-scr))
  (System/exit 0))

(defn new-world []
  (let [[w h] (s/get-size (get-scr))
        w (l/gen-rand-world (floor (/ (- w 4) 2))
                             (- h 3))]
    (l/add-player (l/add-player w user-cluster) ki)))

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
                  (str "greedy solver: " (:generation (solver/greedy-solve w user-cluster)))
                  ""
                  (str "r: " (solver/potential-gain w user-cluster :red))
                  (str "g: " (solver/potential-gain w user-cluster :green))
                  (str "b: " (solver/potential-gain w user-cluster :blue))
                  (str "y: " (solver/potential-gain w user-cluster :yellow))
                  (str "c: " (solver/potential-gain w user-cluster :cyan))
                  (str "v: " (solver/potential-gain w user-cluster :magenta))])
  (s/get-key-blocking (get-scr)))

(defn handle-input [world]
  (redraw world)
  (let [key (s/get-key-blocking (get-scr))]
    (case key
      \q (recur (quit))
      \s (recur (new-world))
      \h (do (show-help) (recur world))
      \? (do (show-help) (recur world))
      \d (do (show-debug world) (recur world))
      \k (recur (solver/greedy-step world ki))
      \r (recur (l/colorize world user-cluster :red))
      \g (recur (l/colorize world user-cluster :green))
      \b (recur (l/colorize world user-cluster :blue))
      \y (recur (l/colorize world user-cluster :yellow))
      \c (recur (l/colorize world user-cluster :cyan))
      \v (recur (l/colorize world user-cluster :magenta)) ; violet
      (recur world))))



(defn handle-args
  "handles command line parameters"
  [args]
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
    (handle-input w)))
