(ns clojure-dom.common
  )

(def *keywords* (agent 
		 (hash-map)
		 :meta {:doc "Hash-map of {string :string} used for keyword creation optimisation"}))


(defn update-keywords
  "Adds a new keyword to the *keywords* map, based on the value of string s. Returns a keyword based on s"
  [s]
  (let [k (keyword s)]
   (send *keywords* assoc s k)
   k))

(defn to-keyword
  "Optimised version of the keyword function that uses *keywords*. Returns an :s keyword"
  [s]
  (let [kw (get @*keywords* s)]
    (if kw
      kw
      (update-keywords s))))




