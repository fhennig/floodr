(ns floodr.logic-test
  (:require [clojure.test :refer :all]
            [floodr.logic :refer :all]))

;               (3, 3)
; (2, 1) (2, 2)
; (1, 1) (2, 1)

(def p11 (struct point 1 1))
(def p12 (struct point 1 2))
(def p22 (struct point 2 2))
(def p33 (struct point 3 3))
(def p21 (struct point 2 1))

(def set1 #{p11 p12})
(def set2 #{p21 p22})
(def set12 #{p11 p12 p21 p22})
(def set3 #{p33})
(def empty-set #{})

(def cl1r (struct cluster :red set1))
(def cl1g (struct cluster :green set1))
(def cl2r (struct cluster :red set2))
(def cl2g (struct cluster :green set2))
(def cl12r (struct cluster :red set12))
(def cl3r (struct cluster :red set3))
(def cl3g (struct cluster :green set3))


(deftest test-adjacent?
  (is (adjacent? p11 p12) "up")
  (is (adjacent? p12 p11) "down")
  (is (adjacent? p22 p12) "left")
  (is (adjacent? p12 p22) "right")
  (is (not (adjacent? p11 p22)) "not adjacent"))
      
(deftest test-sets-adjacent?
  (is (sets-adjacent? set1 set2) "should be adjacent")
  (is (not (sets-adjacent? set1 set3)) "should not be adjacent")
  (is (not (sets-adjacent? set1 empty-set)) "empty set should not be adjacent"))

(deftest test-same-color?
  (is (same-color? cl1r cl2r) "both red")
  (is (not (same-color? cl1r cl1g)) "one red, one green"))

(deftest test-mergeable?
  (is (mergeable? cl1r cl2r) "adjacent and same color")
  (is (not (mergeable? cl1r cl2g)) "not same color")
  (is (not (mergeable? cl1r cl3r)) "not adjacent")
  (is (not (mergeable? cl1r cl3g)) "not adjacent and not same color"))

(deftest test-merge-all
  (is (= #{cl12r cl3r} (merge-all #{cl1r cl2r cl3r})) "1 and 2 can be merged, 3 not"))
