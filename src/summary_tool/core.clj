(ns summary-tool.core)
(require 'clojure.string)
(require 'clojure.set)

(def sentence-scores {})

(defn split-to-sentences 
  [content]
  (clojure.string/split 
    (clojure.string/replace content #"\n" ". ") 
    #"\. "))

(defn split-to-paragraphs
  [content]
  (clojure.string/split content #"\n\n"))

(defn format-sentence
  [sentence]
  (clojure.string/replace sentence #"\W+" ""))

(defn calc-sentence-intersection
  [sen1 sen2]
  (let [s1 (set (clojure.string/split sen1 #" "))
        s2 (set (clojure.string/split sen2 #" "))
        n  (count (list (clojure.set/intersection s1 s2)))]
       (if n 
          ; normalize result
          (/ n (/ (+ (count s1) (count s2)) 2))
          0)))
          
(defn remove-current
  [current collection]
  (remove #{current} collection))

(defn compute-sentence-scores
  [current coll]
  (map #(calc-sentence-intersection current %) (remove-current current coll)))

; compare every sentence to every other sentence
; compute intersections between sentences
; sum these values
; store the sum for each sentence
(defn rank-sentences
  [content]
  (let [s (split-to-sentences content)]
    (doseq [current s]
     (assoc sentence-scores 
              #(format-sentence current)                          ;key
              #(reduce + (compute-sentence-scores current s)))))) ;value




