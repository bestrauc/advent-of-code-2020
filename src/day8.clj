; WARNING - mega spaghetti code ahead.
; Not thinking too carefully, I solved problem 1 quickly with a lazy data structure.
; For problem 2 this turned out very problematic and things got super messy to solve.
;
; If you do it cleanly, this should be nicely solvable with proper graph data structure
; and algorithms, where it is then very easy to walk along the edges and track arbitrary 
; computations, like summing and multiplying along the path.

(ns day8
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.set :as s]))

; Utility functions.
; --------------------------------------

(defn to-keyword [bag-str-with-spaces]
  ; Convert "dark yellow bags" to :dark-yellow-bag
  (-> bag-str-with-spaces (str/replace #"s$" "") (str/replace #" " "-") keyword))

(defn invert-graph [graph]
  ; Invert a graph represented as an adjacency list.
  ; For problem 1 it's helpful to flip the adjacency list around ("contained in" instead of "contains").
  (reduce-kv
    (fn [m k vals] (reduce (fn [m2 v] (assoc m2 v (conj (m2 v []) k))) m vals))
    (hash-map) graph))


; ---------------------------------------

(def problem-input
  (-> "input8.txt"
      io/resource
      slurp
      str/split-lines))

(defn parse-bags [bag-line]
  ; parse "shiny gold bags contain 2 dark red bags" into ["shiny gold bags" ["2" "dark red bags"]]
  (let [[bag-type & bags-contained] (str/split bag-line #"s contain")
        bag-contents (->> (first bags-contained) (re-seq #"(\d) ([a-z ]+)[,.]") (map rest))]
    [bag-type bag-contents]))

(defn to-bag-map [parsed-bag]
  ; turn ["shiny gold bags" ["2" "dark red bags"]] into {:shiny-gold-bag [:dark-red-bag]}
  (let [[bag-type bag-contents] parsed-bag] 
    (hash-map (to-keyword bag-type) (mapv #(-> % second to-keyword) bag-contents))))

(defn to-bag-distance-map [parsed-bag]
  ; turn ["shiny gold bags" ["2" "dark red bags"]] into {(:shiny-gold-bag :dark-red-bag) 2}
  (let [[bag-type bag-contents] parsed-bag]
    (into (hash-map)
          (map #(vector [(-> % second to-keyword) (to-keyword bag-type)]
                        (-> % first Integer.)) bag-contents))))

; Our helper functions parse the input into dicts line by line. 
; We merge the dicts here, both for adjacencies and distances.
(def bag-graph (apply merge (map to-bag-map (map parse-bags problem-input))))
(def distances (apply merge (map to-bag-distance-map (map parse-bags problem-input))))

(defn neighbor-dists [node-with-dist neighbors]
  ; Given something like [:dark-red-bag 2] and {:shiny-gold-bag, :dark-blue-bag},
  ; get the distances between the node and the neighbords and multiply node weight and neighbor dists.
  ; We basically accumulate the distances here as we visit the neighbors (not good to do this here).
  (apply hash-map 
         (mapcat vector neighbors 
                 (map #(* (distances (vector % (first node-with-dist))) (second node-with-dist))
                          neighbors))))

(defn get-reachable-nodes [node-set graph intermediates]
  ; Move one step outwards from the `node-set`, keeping track of their accumulated distances.
  ; we keep track of the intermediary node distances of each step - at the end we can sum those to solve problem 2.
  (let [neighbor-maps (apply merge (map #(hash-map % (->> % first graph (neighbor-dists %))) node-set))
        neighbor-nodes (apply merge-with + (vals neighbor-maps))]
    (if (empty? neighbor-nodes)
      intermediates
      (get-reachable-nodes neighbor-nodes graph (merge-with + intermediates neighbor-nodes)))))

(defn run [args]
  (println 
    (reduce + 0 (vals (get-reachable-nodes {:shiny-gold-bag 1} bag-graph {})))))
