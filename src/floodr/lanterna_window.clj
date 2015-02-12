(ns floodr.lanterna-window
  (:require [lanterna.screen :as s]
            [floodr.util :refer :all]))

;;; padding 

(defn- space [n] (apply str (repeat n " ")))

(defn- pad [s len centered]
  (let [pad-len (- len (count s))
        pad-f (if centered (space (floor (/ pad-len 2))) "")
        pad-b (space (- pad-len (count pad-f)))]
      (str pad-f s pad-b)))

(defn- pad-lines [lines]
  (let [line-width (apply max (map count lines))]
    (map #(pad % line-width false) lines)))

;;; clearing the screen

(defn all-black [screen]
  (let [[w h] (s/get-size screen)]
    (doseq [y (range h)]
      (doseq [x (range w)]
        (s/put-string screen x y " ")))))

;;; drawing windows

(defn- put-lines [screen width lines]
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

(defn- line->string [line key-descs]
  (cond (string? line) line
        (char? line) (str line " - " (:desc (get key-descs line)))))

(defn put-window [screen & [lines key-descs]]
  "displays a window containing the given lines
   a line may be a single character if a key-description is provided"
  (when-not (nil? lines)
    (let [ls (map #(line->string % key-descs) lines)
          ls-padded (pad-lines ls)
          width (count (first ls-padded))]
      (put-lines screen width ls-padded))))

;;; drawing the status line (currently unused)

(defn put-status
  [screen status]
  (let [[w h] (s/get-size screen)
        y (- h 1)]
    (if (empty? status)
      (s/put-string screen 0 y (space (- w 1)))
      (s/put-string screen 2 y status))))

(defn clear-status [screen] (put-status screen nil))

;;; getting user input

(defn get-valid
  "gets key input from the user. retries until a key is
  given that is contained in the given set of valid keys"
  [screen valid-keys]
  (let [key (s/get-key-blocking screen)]
    (if (contains? valid-keys key) key
        (recur screen valid-keys))))
