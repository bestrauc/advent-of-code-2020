(ns day1
  (:require [clojure.string :as str]))

(def inp1 (mapv #(Integer. %) (str/split (slurp "inputs/input1.txt") #"\n")))

(defn run [opts]
  (println 
    (for 
      [i (range (count inp1)) 
       j (range i) 
       k (range j) 
       :let [x (nth inp1 i) 
             y (nth inp1 j) 
             z (nth inp1 k)] 
       :when (== (+ x y z) 2020)] 
      (* x y z))))
