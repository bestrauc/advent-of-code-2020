(ns day6
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.set :as s]))

; Utility functions.
; --------------------------------------

(defn sum [coll]
  (reduce + 0 coll))

(def problem-input
  (map str/split-lines
    (-> "input6.txt" 
        io/resource 
        slurp 
        (#(str/split % #"\n\n")))))

(defn answer-count-1 [inp]
  ; Join all the chars in the group and count the size of their set.
  (sum (map #(->> % (str/join "") set count) inp)))

(defn answer-count-2 [inp]
  ; Intersect the chars of all persons in a group and count the size.
  (sum (map count (map #(apply s/intersection (map set %)) inp))))

; --------------------------------------

(defn run [opts]
  (println (answer-count-2 problem-input)))
