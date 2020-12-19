(ns day12
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.pprint :as pp]))

; Was used to debug some intermediate output.
(defn cl-println [x] (doto x (println)))

; Different start values for problem 1.
(def ship-state
  {:wx 10 :wy 1   ; waypoint coordinates
   :x 0   :y 0})  ; ship coordinates

(defn rotate-ship [ship-state degrees]
  ; Rotate direction vector clockwise by the given degrees.
  (let [{:keys [wx wy]} ship-state
        rads (* (/ degrees 360) (* 2 Math/PI))
        new-wx (Math/round (- (* wx (Math/cos rads)) (* wy (Math/sin rads))))
        new-wy (Math/round (+ (* wx (Math/sin rads)) (* wy (Math/cos rads))))]
    (-> ship-state (assoc :wx new-wx) (assoc :wy new-wy))))

(defn follow-instruction [ship-state [ins value]]
  ; For problem 1, update :x :y instead of :wx :wy for N/S/E/W.
  (cond
    (= ins \N) (update ship-state :wy #(+ % value))
    (= ins \S) (update ship-state :wy #(- % value))
    (= ins \E) (update ship-state :wx #(+ % value))
    (= ins \W) (update ship-state :wx #(- % value))
    (= ins \L) (rotate-ship ship-state value)
    (= ins \R) (follow-instruction ship-state [\L (- 360 value)])
    (= ins \F) (let [xval (* (:wx ship-state) value)
                     yval (* (:wy ship-state) value)]
                 (-> ship-state (update :x + xval) (update :y + yval)))))

(defn follow-navigation [ship-state instructions]
  (reduce follow-instruction ship-state instructions))

; Parse the input
; --------------------------------------
(def problem-input
  (->> "input12.txt" 
      io/resource 
      slurp 
      str/split-lines
      (map #(vector (first %) (Integer. (subs % 1))))))

; Run the program.
; --------------------------------------
(defn run [opts]
    (println (follow-navigation ship-state problem-input)))
