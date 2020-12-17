(ns day10
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]))

; Parse the input
; --------------------------------------

(def problem-input
  (->> "input10.txt" 
      io/resource 
      slurp 
      str/split-lines
      (map #(Integer. %))
      sort))

(def extended-input
  ; Construct (0) input (last+3) for the problem.
  (into (vector) (concat [0] problem-input [(+ (last problem-input) 3)])))

(defn count-diffs [xs]
  (for [[v1 v2] (map vector (butlast xs) (rest xs))]
    (- v2 v1)))

(defn get-diff-product [xs]
  (let [device-jolts (+ (last xs) 3)
        freqs (frequencies (count-diffs xs))]
    (* (freqs 1) (freqs 3))))

(def count-combinations
  ; Recursively enumerate and count options. The input is the vector index
  ; at which we start counting. Many redundant subproblems, so we memoize.
  (memoize 
    (fn [i]
      ; For vector index i, find which of the following fields are at most 3 off.
      ; We then recursively count for vectors starting at i+1, i+2 and so on.
      (let [n (nth extended-input i)
            xs (subvec extended-input (inc i))
            candidates (take-while #(>= (+ n 3) %) xs)
            next-is (map #(+ i 1 %) (range (count candidates)))]
        (if (empty? xs)
          1 ; End of recursion, we add one count to the possible combinations.
          (reduce + (map count-combinations next-is))))))) ; Sum the counts of the subproblems.

; Run the program.
; --------------------------------------
(defn run [opts]
  (do 
    (println (format "Problem 1: %s" (get-diff-product extended-input)))
    (println (format "Problem 2: %s" (count-combinations 0)))))
