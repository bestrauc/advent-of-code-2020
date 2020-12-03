(ns day3
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]))

(def problem-input
  (str/split (slurp (io/resource "input3.txt")) #"\n"))

(def input-slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn follow-vector [mat [x y]]
  (let [h (count mat)
        w (count (first mat))]
    (loop [i 0
           j 0
           counter 0]
      (if (>= i h)
        counter
        (let [row (nth mat i)
              c (nth row (mod j w))
              next-i (+ i y)
              next-j (+ j x)]
          (if (= c \#)
            (recur next-i next-j (inc counter))
            (recur next-i next-j counter)))))))



(defn run [opts]
  (println 
    (reduce * 1 (map #(follow-vector problem-input %) input-slopes))))
