(ns summary-tool.core)
(require 'clojure.string)
(require 'clojure.set)

(defn split-to-sentences 
  [content]
  (clojure.string/split 
    (clojure.string/replace content #"\n" ". ") 
    #"\. "))

(defn split-to-paragraphs
  [content]
  (clojure.string/split content #"\n\n"))

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
; store the tuple [sentence-score sentence]
(defn rank-sentences
  [content]
  (let [s (split-to-sentences content)
        scores (map #(compute-sentence-scores % s) s)
        ranks (map #(reduce + %) scores)]
    (map vector ranks s)))

(defn sort-ranked-sentences
   [ranked]
   (sort-by first > ranked))

(defn find-best-sentence
  [content]
  (let [paras (split-to-paragraphs content)]
    (doseq [p paras]
      (if (< 2 (count p))
        nil ; ignore short paragraphs 
        (let [[_ best] (first (sort-ranked-sentences (rank-sentences p)))] best )))))

(defn make-summary
   [content]
   (filter #(not= nil %) (find-best-sentence content)))


(def test-content "Habitat for Humanity homes, built for low-income buyers using volunteer labor and donations, are financed with affordable loans. The nonprofit selects homeowners based on their level of need, willingness to become partners in the program and ability to repay their loan. Homeowners invest their own time into building the homes as well.")


