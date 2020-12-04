(ns day4
  (:require 
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.set :as s]))

; Some useful constants for the problem.
; --------------------------------------

(def mandatory-keys
  #{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"})

(def valid-eye-colors
  #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

; Utility functions.
; --------------------------------------

(def problem-input
  (slurp (io/resource "input4.txt")))

(defn rm-newline [s]
  (str/replace s #"\n" " "))

(defn parse-int [number-str]
  ; With error handling, taken from: https://stackoverflow.com/a/28776575 
  (try
    (Integer. (re-find #"[0-9]*" (.toString number-str)))
    (catch NumberFormatException e nil)))

(defn number-str-in-range? [s low up]
  (let [n (parse-int s)]
    (when (some? n) (<= low n up))))

; --------------------------------------

(defn into-passport-dicts [inp]
  (map #(apply hash-map %)
    (map #(str/split % #"[ :]")
         (map rm-newline
              (str/split inp #"\n\n")))))

(defn validate-key [k]
  ; Return validation function for key k.
  (case k
    "byr" #(number-str-in-range? % 1920 2002)
    "iyr" #(number-str-in-range? % 2010 2020)
    "eyr" #(number-str-in-range? % 2020 2030)
    "hgt" #(if (str/includes? % "in")
             (number-str-in-range? % 59 76)
             (number-str-in-range? % 150 193))
    "hcl" #(re-find #"^#[0-9a-f]{6}$" %)
    "ecl" #(valid-eye-colors %)
    "pid" #(and (parse-int %) (== (count %) 9))))

(defn simple-validation [passport-dict]
  ; Validation function for part 1 of the problem.
  (s/superset? (-> passport-dict keys set) mandatory-keys))

(defn complex-validation [passport-dict]
  ; Validation function for part 2 of the problem.
  ; 
  ; For each of the mandatory keys we get the validation
  ; function and check it on the value of that key. 
  ; In the end, all validations must be truthy. 
  (every? identity 
          (map #(when-some [v (passport-dict %)] ((validate-key %) v)) 
               mandatory-keys)))

(defn count-valid-passports [passport-dicts validation-fn]
  (count (filter validation-fn passport-dicts)))

(defn run [opts]
  (println 
    (-> problem-input 
        into-passport-dicts 
        (count-valid-passports complex-validation))))
