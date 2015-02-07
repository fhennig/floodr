(ns floodr.core
  (:require [lanterna.screen :as s]
            [floodr.logic :as l])
  (:gen-class))

;;; Constants

(def current-world)
(def user-cluster 0)

;(def scr (s/get-screen :unix))

(def scr (ref nil))

(defn set-scr [s] (dosync (ref-set scr s)))
(defn get-scr [] @scr)

(defn all-black []
  (let [[w h] (s/get-size (get-scr))]
    (doseq [y (range h)]
      (doseq [x (range w)]
        (s/put-string (get-scr) x y " ")))))

(defn draw [world]
  (let [[w h] [(get world :w) (get world :h)]
        [off-x off-y] [2 2]
        color (fn [n] (l/color world n))]
    (doseq [i (range (* w h))]
      (s/put-string (get-scr) (+ (* 2 (mod i w)) off-x) (+ (quot i w) off-y)
                    "  " {:bg (color i)}))))

(defn redraw [world]
  (all-black)
  (s/put-string (get-scr) 2 0 "floodr - 'h' for help")
  (draw world)
  (s/redraw (get-scr)))



(defn quit []
  (s/stop (get-scr))
  (System/exit 0))

(defn new-world []
  (let [[w h] (s/get-size (get-scr))]
    (l/gen-rand-world (int (Math/floor (/ (- w 4) 2)))
                      (- h 3))))

(defn put-ln [line str]
  (if (or (pos? line) (= 0 line))
    (s/put-string (get-scr) 2 line str)
    (let [[_ h] (s/get-size (get-scr))]
      (s/put-string (get-scr) 2 (+ h line) str))))

(defn show-help []
  (all-black)
  (put-ln 0 "floodr")
  (put-ln 2 "controls:")
  (put-ln -1 "press any key to continue")
  (s/redraw (get-scr))
  (s/get-key-blocking (get-scr)))

(defn handle-input [world]
  (redraw world)
  (let [key (s/get-key-blocking (get-scr))]
    (case key
      \q (recur (quit))
      \s (recur (new-world))
      \h (do (show-help) (recur world))
      \? (do (show-help) (recur world))
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
  (let [w (new-world)]
    ; handle command line parameters
    (redraw w)
    (handle-input (new-world))))
