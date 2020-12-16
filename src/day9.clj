(ns day9
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]))

; Parse the input
; --------------------------------------

(def problem-input
  (->> "input9.txt" 
      io/resource 
      slurp 
      str/split-lines
      (map #(Long. %))))

(defn sum-of-nums? [n nums]
  ; Check if n=a+b for a,b in nums.
  ;
  ; The implementation checks if (n-a) is in (nums \ a).
  (not (empty?
    (for [summand1 nums
        :let [summands-left (disj nums summand1)
              summand2 (- n summand1)]
        :when (contains? summands-left summand2)]
      1))))

(defn check-code [code window-len]
  ; Shift a window over the code and check if numbers can be summed.
  (loop [priors (into (clojure.lang.PersistentQueue/EMPTY) (take window-len code))
         code (drop window-len code)
         results []]
    (if-not (some? code)  ; The [next-num & rest-code] code destructuring will leave rest-code nil at the end.
      results
      (let [[next-num & rest-code] code             ; [5 6 7] -> 5 & [6 7]
            prior-set (set priors)]                 ; [2 3 4] -> #{2 3 4} 
        (recur (-> priors pop (conj next-num))      ; [2 3 4] 5 6 7 -> 2 [3 4 5] 6 7
               rest-code                            ; [6 7]
               (conj results [next-num (sum-of-nums? next-num prior-set)]))))))

(defn find-number-sum [number code]
  ; Compute extending cumulative sums, shortening the window if they become too large.
  ; Should be O(n), though this lazy implementation is slower because of redundant summation.
  (loop [num-list clojure.lang.PersistentQueue/EMPTY
         code code]
    (let [sum (reduce + 0 num-list)     ; We recompute the sum too often here, not optimized out of laziness.
          [next-num & rest-code] code]
      (cond
        (= sum number)          (+ (apply min num-list) (apply max num-list))
        (not (some? next-num))  (println "Not found")
        (> sum number)          (recur (pop num-list) code)
        :else                   (recur (conj num-list next-num) rest-code)))))


; Run the program.
; --------------------------------------
(defn run [opts]
  (let [[ans1 _] (first (filter #(not (second %)) (check-code problem-input 25)))
        ans2 (find-number-sum ans1 problem-input)]
    (println (format "Number is %s, weakness is %s" ans1 ans2))))
