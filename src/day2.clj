(ns day2
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]))

(def sample-inputs 
  ["1-3 a: abcde",
   "1-3 b: cdefg",
   "2-9 c: ccccccccc"])

(def problem-inputs
  (str/split (slurp (io/resource "input2.txt")) #"\n"))

(defn parse-line [line]
  ; Parse "1-3 a: abcde" into [[1 3] \a "abcde"]
  (let [[reps c pass] (str/split line #" ")
        [minc maxc] (map #(Integer. %) (str/split reps #"-"))]
    [[minc maxc] (first c) pass]))

(defn valid-password-1? [[minc maxc] c pass]
  ; Password policy from problem 1
  (let [pass-counts (frequencies pass)
        c-count (or (pass-counts c) 0)]
    (<= minc c-count maxc)))

(defn char-equal? [c1 c2]
  (== 0 (compare c1 c2)))

(defn valid-password-2? [[idx1 idx2] c pass]
  ; Password policy from problem 2
  (let [c1 (nth pass (dec idx1))
        c2 (nth pass (dec idx2))]
    (not= (char-equal? c1 c) (char-equal? c2 c))))

(defn run [opts]
  (println 
    (count 
      (filter identity
        (map #(apply valid-password-2? %) 
             (map parse-line problem-inputs))))))
