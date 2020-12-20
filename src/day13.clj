(ns day13
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.pprint :as pp]))

(defn dbg [x] (doto x (println)))

; Parse the input
; --------------------------------------
(def problem-input-lines
  (->> "input13.txt" 
      io/resource 
      slurp 
      str/split-lines))

(def problem-input
  (let [[mins buses] problem-input-lines]
    [(Integer. mins) 
     (->> buses 
          (#(str/split % #",")) 
          (map-indexed #(vector %1 (when (not= %2 "x") (Integer. %2))))
          (filter second))]))

(defn get-next-multiple [n k]
  (if (zero? (mod n k))
    n
    (+ (- n (mod n k)) k)))

(defn bus-series? [t buses]
  ; Note: Didn't end up using this.
  ; Check that (t+i = 0 (mod bus-id)) holds for each [i bus-id]
  (every? zero? (for [[i bus-id] buses] (mod (+ t i) bus-id))))

(defn prob1 [departure buses]
  (map #(vector % (get-next-multiple departure (second %))) buses))

(defn prob2 [buses]
  ; Output the congruences we have to type into a Chinese remainer theorem solver.
  ; For instance: https://www.dcode.fr/chinese-remainder 
  ; Had previously tried with a primitive sieving method, but still took quite long.
  (doseq [[i bus-id] buses]
    (println (- i) bus-id)))

; Run the program.
; --------------------------------------
(defn run [opts]
    (println (apply min-key second (apply prob1 problem-input))))
