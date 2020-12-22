(ns day14
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.pprint :as pp]))

(defn dbg [x] (doto x (println)))

; Parse the input
; --------------------------------------
(def problem-input
  (->> "input14.txt" 
      io/resource 
      slurp 
      str/split-lines
      ; Regex that can parse both masks and mem[num1] = num2 assignments.
      ; Returns tuples (mask nil nil) or (nil "8" "101"), depending.
      (map #(->> % (re-find #"([X01]{36})|mem\[(\d+)\] = (\d+)") rest))))

(defn update-memory-1 [mem [mask idx v]]
  (if (nil? mask)
    (assoc mem idx (-> (Long. v) (bit-and (:mem1 mem)) (bit-or (:mem0 mem))))
    (-> mem 
        (assoc :mem0 (Long/parseLong (str/replace mask "X" "0") 2))
        (assoc :mem1 (Long/parseLong (str/replace mask "X" "1") 2)))))

(defn get-floating-masks [mask]
  ; Recursively replace X with 0/1 and accumulate all variants.
  ((fn fill-mask [_mask masks]
    (if (str/includes? _mask "X") ; Probably could keep of whether we're done more efficiently.
      (concat 
        (fill-mask (str/replace-first _mask "X" "0") masks)
        (fill-mask (str/replace-first _mask "X" "1") masks))
      ; Already convert filled-in concrete masks to Long.
      (conj masks (Long/parseLong _mask 2)))) 
   mask []))

(defn update-memory-2 [mem [mask idx-str v-str]]
  ; Misunderstood the question a bit, did it the wrong way and then
  ; ended up wrangling bits until it worked. Not very thoughtful approach here ;(
  ;
  ; Problem is I got the floating vals in the masks already and had to compensate
  ; for that by splitting the masks on the X positions again.
  (if (nil? mask)
    (let [idx (Long. idx-str) 
          v   (Long. v-str)
          {:keys [masks xmask]} mem]
      (reduce (fn [_mem _mask]
                ; We map over all floating masks, but have to take care to
                ; AND the non-X mask values and then set all the X-substituted mask values.
                (let [xvals (bit-and xmask _mask)
                      rxvals (bit-and (bit-not xmask) _mask)
                      base-idx (bit-or rxvals idx)]
                  (assoc _mem (bit-or (bit-and (bit-not xmask) base-idx) (bit-and xvals xmask)) v)))
              mem masks))
    (-> mem 
        (assoc :masks (get-floating-masks mask))
        (assoc :xmask (Long/parseLong (str/replace (str/replace mask "1" "0") "X" "1") 2)))))

; Run the program.
; --------------------------------------
(defn run [opts]
  (do
    (println (reduce + (vals (dissoc (reduce update-memory-1 {} problem-input) :mem0 :mem1))))
    (println (reduce + (vals (dissoc (reduce update-memory-2 {} problem-input) :masks :xmask))))))

