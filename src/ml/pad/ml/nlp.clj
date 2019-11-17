(ns pad.ml.nlp
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn build-vocab
  "Returns {word idx}"
  [tokens]
  (let [words (flatten tokens)
        freq (reduce (fn [a word]
                       (update-in a [word] (fnil inc 0))) {} words)
        freq-sorted (sort-by second > freq)
        words-sorted (map first freq-sorted)]
    {:frequencies freq
     :frequencies-sorted freq-sorted
     :indexes (->>
               (map vector words-sorted (range 0 (count words-sorted)))
               (into {}))}))
