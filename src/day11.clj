(ns day11
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.pprint :as pp]))

(defn get-entry [matrix [row col] default]
  (try 
    (-> matrix (nth row) (nth col))
    (catch IndexOutOfBoundsException e default)))

(defn set-entry [matrix [row col] newval]
  (assoc matrix row (-> matrix (nth row) (assoc col newval))))

(defn get-neighbor-set [matrix [row col]]
  ; Used to get the neighborhood for part 1 of the problem.
  (frequencies 
    (let [h (count matrix)
          w (count (first matrix))]
      (for [i (range (max (- row 1) 0) (min (+ row 2) h))
            j (range (max (- col 1) 0) (min (+ col 2) w))
            :when (not= [row col] [i j])]
        (get-entry matrix [i j] nil)))))

(defn get-first-visible [matrix [row col] [di dj]]
  ; Cast a line from [row col] into the direction [di dj].
  (loop [c 1]
    (let [new-row (+ row (* c di))
          new-col (+ col (* c dj))
          next-entry (get-entry matrix [new-row new-col] nil)]
      (cond 
        (nil? next-entry) \.
        (contains? #{\#,\L} next-entry) next-entry
        :else (recur (inc c))))))

(defn get-visible-neighbor-set [matrix coords]
  ; Get the visible neighborhood for part 2.
  ; Just brute force and cast a line in each direction.
  (frequencies 
    (for [[di dj] [[-1 -1] [-1 0] [1 0] [1 1] [0 1] [0 -1] [1 -1] [-1 1]]]
      (get-first-visible matrix coords [di dj]))))

(defn adapt-seat [[write-matrix read-matrix] coord]
  ; Update the given seat to its new state according to its neighborhood.
  ; 
  ; Use a write and read matrix because we want to update all states
  ; at the same time and therefore don't want do modify the original state.
  (let [neighbor-set (get-visible-neighbor-set read-matrix coord)
        seat-state (get-entry read-matrix coord nil)]
    (cond
      (and (= seat-state \L) (not (neighbor-set \#)))
        [(set-entry write-matrix coord \#) read-matrix]
      (and (= seat-state \#) (<= 5 (neighbor-set \# 0)))
        [(set-entry write-matrix coord \L) read-matrix]
      :else [write-matrix read-matrix])))

(defn update-seats [matrix]
  ; Update all seats by reducing over the coordinates.
  (let [h (count matrix)
        w (count (first matrix))
        coords (for [i (range h) j (range w)] [i j])]
    (first (reduce adapt-seat [matrix matrix] coords))))

(defn update-until-stable [matrix]
  ; Simply apply changes until nothing changes.
  (loop [matrix matrix]
    (let [next-matrix (update-seats matrix)]
      (if (= matrix next-matrix)
        matrix
        (recur next-matrix)))))

; Parse the input
; --------------------------------------
(def problem-input
  (->> "input11.txt" 
      io/resource 
      slurp 
      str/split-lines
      (mapv #(mapv char %))))

; Run the program.
; --------------------------------------
(defn run [opts]
    (println ((-> problem-input update-until-stable flatten frequencies) \#)))
