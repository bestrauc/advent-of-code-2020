(ns day5
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]))

; Utility functions.
; --------------------------------------

(def problem-input
  (str/split-lines (slurp (io/resource "input5.txt"))))

(defn binarize-code [seat-code]
  (map #(Character/digit % 10)
    (reduce #(apply str/replace %1 %2) 
            seat-code [[#"[FL]" "0"] [#"[BR]" "1"]])))

; Cool generator stolen from the internet, to be honest.
(def powers-of-2 (iterate (partial *' 2) 1))

(defn bin-to-num [bits]
  ; Map e.g. (0 1 1 0 1) to 14
  (reduce + 0 (map * (reverse bits) powers-of-2)))

(defn decode-seat [seat-code]
  (let [binary-rep (binarize-code seat-code)
        row (bin-to-num (take 7 binary-rep))
        col (bin-to-num (drop 7 binary-rep))]
    [row col (+ (* row 8) col)]))

; --------------------------------------

(defn run [opts]
  ; Problem 1: (println (apply max (map #(nth % 2) (map decode-seat problem-input)))))
  ; Problem 2: The gap leads to a diff of 2 in the sorted seq, then count from start.
  (println 
    (let [ids (map #(nth % 2) (map decode-seat problem-input))
          sorted-ids (sort ids)
          id-diffs (map - (rest sorted-ids) sorted-ids)]
      (+ 1 (.indexOf id-diffs 2) (first sorted-ids)))))
