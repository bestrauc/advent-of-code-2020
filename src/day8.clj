(ns day8
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.set :as s]))

; Program state and OPs.
; --------------------------------------

(def initial-state 
  {:acc 0
   :ip 0
   :visited #{}})

(defn acc [state & args]
  (let [[v] args]
    (-> state 
        (update :acc #(+ % v))
        (update :ip inc))))

(defn nop [state & args]
  (-> state 
      (update :ip inc)))

(defn jmp [state & args]
  (let [[v] args]
    (-> state 
        (update :ip #(+ % v)))))

; Parse the input
; --------------------------------------

(defn parse-op [op-str val-str]
  ; Parse an op like '"acc" "+5"' into the function and an integer arg.
  (vector (resolve (symbol op-str)) (Integer. val-str)))

(def problem-input
  ; Split OPs by line and convert them to funcs and args.
  (->> "input8.txt" 
      io/resource 
      slurp 
      str/split-lines
      (map #(str/split % #"\s"))
      (mapv #(apply parse-op %))))

; Run the program.
; --------------------------------------

(defn run-program [start-state program]
  (loop [state start-state]
    (let [{:keys [ip acc visited]} state
          next-op (nth program ip [nil nil])
          [op-fn op-args] next-op]
    (cond 
      (nil? op-fn)
        (println (format "Program ended with acc=%s (IP=%s)" (:acc state) (:ip state)))
      (contains? visited ip)
        (println (format "Loop found with acc=%s (IP=%s)" (:acc state) (:ip state)))
      :else
        ; Mutate the state with the next OP and update the :visited collection.
        (recur (-> state (op-fn op-args) (update :visited #(conj % ip))))))))

(defn fix-program [program]
  ; Successively replace nops<->jmps until the program terminates.
  ; 
  ; Don't know if the `var` call is necessary, but it seems to be
  ; to compare functions robustly, maybe they are objects otherwise.
  (doseq [idx (range (count program)) 
          :let [[op-fn op-args] (nth program idx)]
          :when (contains? #{(var nop), (var jmp)} op-fn)]
    (run-program 
      initial-state 
      (assoc program idx [(if (= op-fn (var nop)) jmp nop) op-args]))))
      
; --------------------------------------

; Output all fix attempts, check manually which succeeded.
(defn run [opts]
  (println (fix-program problem-input)))
