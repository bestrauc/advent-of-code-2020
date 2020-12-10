(ns day8
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.set :as s]))

; Utility functions.
; --------------------------------------

(defn to-keyword [str-with-spaces]
  (-> str-with-spaces (str/replace #"s$" "") (str/replace #" " "-") keyword))

(defn get-reachable-nodes [node-set graph]
  ; Follow the node-set recursively until no new nodes get added.
  (let [extended-set (into node-set (mapcat graph node-set))]
    (if (= extended-set node-set)
      node-set
      (get-reachable-nodes extended-set graph))))

(defn invert-graph [graph]
  ; Invert a graph represented as an adjacency list.
  (reduce-kv
    (fn [m k vals] (reduce (fn [m2 v] (assoc m2 v (conj (m2 v []) k))) m vals))
    (hash-map) graph))


; ---------------------------------------

(def problem-input
  (-> "input8_sample.txt"
      io/resource
      slurp
      str/split-lines))

(defn parse-bags [bag-line]
  (let [[bag-type & bags-contained] (str/split bag-line #"s contain")
        bag-contents (->> (first bags-contained) (re-seq #"(\d) ([a-z ]+)[,.]") (map rest))]
    [bag-type bag-contents]))

(defn to-bag-map [parsed-bag]
  (let [[bag-type bag-contents] parsed-bag]
    (hash-map (to-keyword bag-type) (mapv #(-> % second to-keyword) bag-contents))))

(defn get-bag-graph [inp]
  (apply merge (map to-bag-map (map parse-bags inp))))

(for [a (parse-bags (first problem-input))]
  (println a))

(def bags (get-bag-graph problem-input))
