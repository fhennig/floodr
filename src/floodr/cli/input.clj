(ns floodr.cli.input
  (:require [lanterna.screen :as s]))

;;; getting user input

(defn get-valid
  "gets key input from the user. retries until a key is
  given that is contained in the given set of valid keys"
  [screen valid-keys]
  (let [key (s/get-key-blocking screen)]
    (if (contains? valid-keys key) key
        (recur screen valid-keys))))
