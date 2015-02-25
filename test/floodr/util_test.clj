(ns floodr.util-test
  (:require [clojure.test :refer :all]
            [floodr.util :refer :all]))

;; illustration
;  0  1  2  3  4  5  6  7
;  8  9 10 11 12 13 14 15
; 16 17 18 19 20 21 22 23

(def w 8)

(deftest test-index-coord-conversions
  (is (= [1 2] (index->coords w 17)))
  (is (= 17 (coords->index w [1 2])))
  (is (= 42 (coords->index w (index->coords w 42)))))

(deftest test-in-bounds
  (is (in-bounds? w 3 [0 0]))
  (is (in-bounds? w 3 [0 2]))
  (is (in-bounds? w 3 [7 0]))
  (is (in-bounds? w 3 [7 2]))
  (is (in-bounds? w 3 [5 1])))

(deftest test-distance
  (is (= 2 (distance [0 0] [0 2])))
  (is (= 2 (distance [0 0] [2 0])))
  (is (= 2 (distance [0 2] [0 0])))
  (is (= 2 (distance [2 0] [0 0]))))

(deftest test-drop-up-to
  (is (= '(3 4 5) (drop-up-to 3 '(1 2 3 4 5)))))
