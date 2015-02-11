(ns floodr.lanterna-window
  (:require [lanterna.screen :as s]
            [floodr.util :refer :all]))

;;; drawing windows

(defn- space [n] (apply str (repeat n " ")))

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

(defn- pad [s len centered]
  (let [pad-len (- len (count s))
        pad-f (if centered (space (floor (/ pad-len 2))) "")
        pad-b (space (- pad-len (count pad-f)))]
      (str pad-f s pad-b)))

(defn put-window
  "displays a window, centered in the screen containing the given lines.
  a map of options can be passed. valid options are:
  :centered bool - determines if the text should be centered"
    [screen lines & [options]]
  (let [line-width (apply max (map count lines))
          padded-lines (map #(pad % line-width (:centered options)) lines)]
    (put-lines screen line-width padded-lines)))

(defn- key->string [key options]
  (if (= key :nl) ""
      (str key " - " (:desc (get options key)))))

(defn put-options-window
  "shows a window with an optional title that displays the given options"
  [screen options order & [title]]
  (let [option-lines (map #(key->string % options) order)
        lines (if (nil? title) option-lines
                  (into [] (concat [title ""] option-lines)))]
    (put-window screen lines)))

;;; drawing the status line

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
  [screen valid-input]
  (let [key (s/get-key-blocking screen)
        pred (if (set? valid-input)
               #(contains? valid-input %) valid-input)]
    (if (pred key) key
        (recur screen valid-input))))

;(defn char->digit [char]
;  (let [i (- (int char) 48)]
;    (if (and (> i -1) (< i 10)) i nil)))
;
;(defn get-digit
;  [screen]
;  (char->digit (get-valid screen
;                          #(not (= nil (char->digit %))))))
